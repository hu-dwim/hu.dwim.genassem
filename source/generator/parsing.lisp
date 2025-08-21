;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem)

(defun json-value (obj key &optional of-type)
  (let ((result (cdr (assoc key obj :test 'equal))))
    (when of-type
      (assert (eq of-type (pop result))))
    result))

(defun json/true-value? (obj key)
  (equal (json-value obj key) 1))

(defun json/bitfield-value (obj key)
  (let ((bits (json-value obj key)))
    (assert (eq :array (pop bits)))
    (json-bitfield-to-integer bits)))

(defun json/def-like-value (obj key)
  (let* ((entry (json-value obj key :object))
         (value (json-value entry "def")))
    (intern value :keyword)))

(defun pseudo-instruction? (obj)
  (or (json/true-value? obj "isPseudo")
      (json/true-value? obj "isCodeGenOnly")))

(defun asm-mnemonic (obj)
  (let ((value (json-value obj "AsmString")))
    (check-type value string)
    (when (plusp (length value))
      value)))

(defun get-encoding (obj)
  (let ((value (json-value obj "Opcode")))
    (when (eq :array (pop value))
      (assert (eql 8 (length value)))
      value)))

;; https://llvm.org/docs/TableGen/BackEnds.html#json-reference
;; "A bits array is ordered from least-significant bit to most-significant."
(defun json-bitfield-to-integer (bit-list)
  (let ((result 0))
    (loop :for i = 0 :then (incf i)
          :for bit :in (reverse bit-list)
          :do (progn
                (setf result (ash result 1))
                (eswitch (bit)
                  (1 (setf result (logior result 1)))
                  (0 (values)))))
    result))

(defun normalize-instruction (obj)
  (assert (eq :object (pop obj)))
  (let ((result ()))
    (macrolet ((set-field (name value)
                 `(setf (getf result ,name) ,value))
               (get-field (name)
                 `(getf result ,name)))
      (set-field :mnemonic (asm-mnemonic obj))
      (set-field :name (json-value obj "!name"))

      (let ((form-entry (json-value obj "Form" :object)))
        (set-field :form (intern (json-value form-entry "def") :keyword)))

      (set-field :opcode (json/bitfield-value obj "Opcode"))

      (let ((preds (json-value obj "Predicates" :array)))
        (loop :with kwpkg = (find-package :keyword)
              :for pred :in preds
              :do (progn
                    (assert (eq :object (pop pred)))
                    (let ((name (json-value pred "def")))
                      (push (intern name kwpkg)
                            (getf result :predicates))))))

      (let ((value (json/def-like-value obj "OpSize")))
        (assert (member value '(:|OpSize16| :|OpSize32| :|OpSizeFixed|)))
        (set-field :op-size value))

      (let ((value (json/def-like-value obj "OpMap")))
        (assert (member value '(:|OB| :|TA| :|TB| :|T8|
                                :|T_MAP4| :|T_MAP5| :|T_MAP6| :|T_MAP7|
                                :|XOPA| :|XOP8| :|XOP9| :|ThreeDNow|)))
        (set-field :op-map value))

      (let ((value (json/def-like-value obj "OpPrefix")))
        (unless (eq value :|NoPrfx|)
          (set-field :op-prefix value)))

      (let ((value (json/def-like-value obj "explicitOpPrefix")))
        (unless (eq value :|NoExplicitOpPrefix|)
          (assert (member value '(:|PD| :|PS| :|SD| :|ExplicitEVEX|
                                  :|ExplicitREX2| :|ExplicitVEX|)
                          :test 'eq))
          ;; PD: 0x66
          ;; PS: 0xF3
          ;; SD: 0xF2
          (set-field :op-prefix/explicit value)))

      ;;;
      ;;; args
      ;;;

      ;; InOperandList/OutOperandList = visible, explicit operands.
      ;; Uses/Defs = implicit operands (hidden side effects: registers, flags, memory, ports).
      (let* ((inputs     (json-value obj "InOperandList" :object))
             (json-args  (json-value inputs "args" :array)))
        (assert (equal "dag" (json-value inputs "kind")))
        (let ((args (loop :for (json-type type name) :in json-args
                          :do (assert (eq :array json-type))
                          :unless (eq :null name)
                            :collect (cons name type))))
          (set-field :args args)))

      result)))

;; (defparameter *supported-encodings* '("EncNormal"))

;; called on the raw json
(defun json/include-instruction? (obj)
  (and (not (ends-with-subseq "_PREFIX" (json-value obj "!name")))
       (asm-mnemonic obj)
       (not (pseudo-instruction?  obj))
       (get-encoding obj)
       (let* ((tsflags (json-value obj "TSFlags" :array))
              (vector-flags (subseq tsflags 40)))
         ;; for now skip all the "fancy" instructions
         (every 'zerop vector-flags))))

;; called on the normalized plist form
(defun include-instruction? (obj)
  (declare (ignore obj))
  t)

(defun process-x86-json (path instruction-emitter)
  (declare (optimize (speed 3)) ; needed to force-enable tail call optimization
           (type function instruction-emitter))
  (with-input-from-file (file path)
    (json-streams:with-open-json-stream
        (stream (json-streams:make-json-input-stream file))
      (let ((instruction-names ())
            (emit-counter 0))
        (declare (type fixnum emit-counter))
        (block parsing
          (labels
              ((next-token ()
                 (let ((token (json-streams:json-read stream)))
                   ;; (print token) ; TODO
                   (when (eq token :eof)
                     (return-from parsing (values)))
                   token))
               (toplevel ()
                 (ecase (next-token)
                   (:begin-object (toplevel-key))))
               (toplevel-key ()
                 (let ((token (next-token)))
                   (unless (eq token :end-object)
                     (assert (stringp token))
                     (let ((obj (json-streams::parse-single stream)))
                       (cond
                         ((string= token "!instanceof")
                          (assert (eq :object (pop obj)))
                          (assert (not instruction-names))
                          (let ((entry (find "X86Inst" obj :test 'equal :key 'first)))
                            (assert (eq :array (second entry)))
                            (setf instruction-names (make-hash-table :test 'equal))
                            (map nil (lambda (name)
                                       (setf (gethash name instruction-names) t))
                                 (nthcdr 3 entry))
                            (format *error-output* "~&; Found ~S instruction descriptors~%"
                                    (hash-table-size instruction-names))))
                         ((and (gethash token instruction-names) ; this toplevel key is one of the listed instructions
                               (json/include-instruction? (rest obj)))
                          (let ((instr (normalize-instruction obj)))
                            (when (include-instruction? instr)
                              (assert (eq :object (pop obj)))
                              (assert (equal "X86" (json-value obj "Namespace")))
                              (incf emit-counter)
                              ;; (format *error-output* "~&; calling emitter for ~S~%" (getf instr :mnemonic))
                              (funcall instruction-emitter instr :raw-json obj))))))))
                 (toplevel-key)))
            (toplevel)))))))

(defun emit-form (form)
  (pprint form)
  ;;(terpri)
  (force-output))

;; (defun to-hex-byte-string (val)
;;   (assert (<= val #xff))
;;   (format nil "~x" val))

(defun intern/asm (str)
  (intern (string-upcase str)))

(define-constant rex.w #x08)
(define-constant rex.r #x04)
(define-constant rex.x #x02)
(define-constant rex.b #x01)

(defun generate-x86-instruction-emitter (instr)
  (destructuring-bind (&key name args mnemonic opcode op-map
                         op-prefix ;op-prefix/explicit
                         form
                       &allow-other-keys)
      instr
    ;;(format *error-output* "~&; emitting ~S / ~S~%" name mnemonic)
    (assert (<= opcode 255))
    (labels
        (
         ;; (skip-instruction ()
         ;;   (format *error-output* "; skipping ~S~%" name)
         ;;   (return-from generate-x86-instruction-emitter))
         )
      (let ((prefix-bytes (make-array 8 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
            (lisp-name (intern/asm (concatenate 'string "_" name)))
            (arg-names (remove nil
                               (mapcar (lambda (arg)
                                         (destructuring-bind (name . type)
                                             arg
                                           (declare (ignore type))
                                           (assert (stringp name))
                                           (intern/asm name)))
                                       args)))
            (opcode-prefix-forms nil))
        (ecase op-prefix
          ((nil)     ())
          (:pd       (vector-push-extend #x66 prefix-bytes))
          ((:sd :xd) (vector-push-extend #xf2 prefix-bytes))
          ((:ps :xs) (vector-push-extend #xf3 prefix-bytes)))
        (case op-map
          (:tb (push `(emit-byte #x0f) opcode-prefix-forms)))

        (let ((prefix-forms (loop :for byte :across prefix-bytes
                                  :collect `(emit-byte ,byte))))
          ;; (when (equal mnemonic "adc{w}	{$src, %ax|ax, $src}")
          ;;   (break))
          ;; (when (equal name "RET64")
          ;;   (break))
          ;; (when (starts-with-subseq "OUT" name)
          ;;   (break))
          (case form
            (:|RawFrm|
             ;; RawFrm specifically means raw form: the instruction has no special ModR/M or opcode map handling—it’s just a fixed sequence of bytes.
             (emit-form
              `(defmacro ,lisp-name ,arg-names
                 `(progn
                    ,@',prefix-forms
                    ,@',opcode-prefix-forms
                    (emit-byte ,',opcode))))
             lisp-name)
            (:|AddRegFrm|
             (emit-form
              `(defmacro ,lisp-name ,arg-names
                 (multiple-value-bind (reg-index reg-mode reg-extra-bit)
                     (register-name->encoding-bits ,(first arg-names))
                   `(progn
                      ,@',prefix-forms
                      (when (eql ,reg-mode 64)
                        ;; emit REX.W + the reg extra bit
                        (emit-byte (logior #x48 ,(if reg-extra-bit rex.b 0))))
                      ,@',opcode-prefix-forms
                      (emit-byte (logior ,',opcode ,reg-index))))))
             lisp-name)
            (:|RawFrmDstSrc|)
            (:|RawFrmImm8|)
            (:|MRMDestMem|)
            (:|MRM_E0|)
            (:|MRM_E1|)
            (:|MRM_C0|)
            (:|MRM_CA|)
            (:|MRM_CF|)
            (:|MRM_FC|)
            (:|MRM_D7|)
            (:|MRM_D9|)
            (:|MRM_DD|)
            (:|MRM_F0|)
            (:|MRM_F6|)
            (:|MRM_FA|)
            (:|MRM_FB|)
            (:|MRM_FF|)
            (:|MRMXr|)
            (:|MRM1m|)
            (:|MRM2m|)
            (:|MRM3m|)
            (:|MRM4m|)
            (:|MRM5m|)
            (:|MRM6m|)
            (:|MRM7m|)
            (:|MRM1r|)
            (:|MRM2r|)
            (:|MRM3r|)
            (:|MRM4r|)
            (:|MRM5r|)
            (:|MRM6r|)
            (:|MRM7r|)
            (:|MRMSrcMem|)
            (:|MRMSrcMem4VOp3|)
            (:|MRMSrcReg|)
            (:|MRMSrcReg4VOp3|)
            (:|MRMDestReg|)
            (:|MRM0m|)
            (:|MRM0r|)))))))

(defun generate-assembler/x86_64 (&key
                                    (print-source? nil)
                                    (package :hu.dwim.genassem/x86))
  (unless (packagep package)
    (setf package (find-package package)))
  (with-output-to-file
      (out-stream (asdf:system-relative-pathname
                   :hu.dwim.genassem
                   "source/assembler/x86-instructions.lisp")
                  :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*standard-output* out-stream)
            (*print-readably* nil)
            (*print-case* :downcase)
            (*package* package))
        (write-string ";;; This file is generated, editing it is unwise.")
        (pprint `(in-package ,(intern (package-name package) :keyword)))
        (let ((predicate-blacklist
                '(:|Mode16|
                  :|Not64BitMode|
                  :|UseSSE1|
                  :|UseSSE2|
                  :|UseSSE3|
                  :|UseSSE41|
                  :|UseSSE42|
                  :|HasSSEPrefetch|
                  :|UseAVX|
                  :|HasAVX|
                  :|HasAVX2|
                  :|HasAVX512|
                  :|HasF16C| ; SSE; half-precision (16-bit) fp -> single-precision (32-bit) fp
                  :|HasRTM|
                  :|HasTSXLDTRK|
                  :|HasWBNOINVD|
                  :|HasAMXTILE|
                  :|HasAMXMOVRS|
                  :|HasAMXTRANSPOSE|
                  :|HasAMXCOMPLEX|
                  :|HasMOVRS|
                  :|HasPCLMUL|
                  :|HasTBM|
                  :|HasCLDEMOTE|
                  :|HasUINTR|
                  ))
              (instr-counter 0)
              (emit-counter 0)
              (symbols-to-export ()))
          (process-x86-json "/home/alendvai/common-lisp/maru/source/assembler/x86.json"
                            (lambda (obj &key raw-json)
                              (incf instr-counter)
                              (let* ((predicates (getf obj :predicates))
                                     (include? (loop :for blacklisted :in predicate-blacklist
                                                     :always (not (member blacklisted predicates :test 'eq)))))
                                (if include?
                                    (progn
                                      (write-char #\. *error-output*)
                                      (incf emit-counter)
                                      (when print-source?
                                        (print obj)
                                        (print raw-json))
                                      (push (generate-x86-instruction-emitter obj)
                                            symbols-to-export))
                                    (write-char #\x *error-output*)))))
          (emit-form `(export '(,@ (remove nil symbols-to-export))))
          (format *error-output* "~&; emitted ~S of ~S instructions~%"
                  emit-counter instr-counter))))))
