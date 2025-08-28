;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem)

(defun json-value (obj key)
  (gethash key obj))

(defun json/true-value? (obj key)
  (eql (json-value obj key) 1))

(defun json/bitfield-value (obj key)
  (let ((bits (json-value obj key)))
    (json-bitfield-to-integer bits)))

(defun json/def-like-value (obj key)
  (let ((entry (json-value obj key)))
    (json/def-like-as-keyword entry)))

(defun json/def-like-as-keyword (entry)
  (let ((value (json-value entry "def")))
    (intern value :keyword)))

(defun pseudo-instruction? (obj)
  (or (json/true-value? obj "isPseudo")
      (json/true-value? obj "isCodeGenOnly")))

(defun asm-string (obj)
  (let ((value (json-value obj "AsmString")))
    (check-type value string)
    (when (plusp (length value))
      value)))

(defun has-encoding? (obj)
  (let ((value (json-value obj "Opcode")))
    (assert (eql 8 (length value)))
    value))

;; https://llvm.org/docs/TableGen/BackEnds.html#json-reference
;; "A bits array is ordered from least-significant bit to most-significant."
(defun json-bitfield-to-integer (bit-vector)
  (let ((result 0))
    (loop :for i :from (1- (length bit-vector)) :downto 0
          :for bit = (aref bit-vector i)
          :do (progn
                (setf result (ash result 1))
                (eswitch (bit)
                  (1 (setf result (logior result 1)))
                  (0 (values)))))
    result))

(defun normalize-instruction (obj)
  (assert (equal "" (json-value obj "TwoOperandAliasConstraint")))
  (let ((result ()))
    (macrolet ((set-field (name value)
                 `(setf (getf result ,name) ,value))
               (get-field (name)
                 `(getf result ,name)))
      (let ((asm-string (asm-string obj)))
        (when asm-string
          (set-field :asm-string asm-string)
          (set-field :mnemonic (aif (position #\Tab asm-string)
                                    (subseq asm-string 0 it)
                                    asm-string))))

      (set-field :name (json-value obj "!name"))

      (let* ((form-entry (json-value obj "Form"))
             (form-value (json-value form-entry "def")))
        (set-field
         :form
         (cond
           ((starts-with-subseq "RawFrm" form-value)
            :raw)
           ((equal form-value "AddRegFrm")
            :add-reg)
           ((starts-with-subseq "MRM" form-value)
            :mrm)
           (t
            (error "Unexpected Form value ~S" form-value)))))

      ;;(set-field :form-bits (json/bitfield-value obj "FormBits"))
      (set-field :opcode (json/bitfield-value obj "Opcode"))
      (set-field :has-position-order (json/true-value? obj "HasPositionOrder"))

      (let ((preds (json-value obj "Predicates")))
        (loop :with kwpkg = (find-package :keyword)
              :for pred :across preds
              :do (let ((name (json-value pred "def")))
                    (push (intern name kwpkg)
                          (getf result :predicates)))))

      (let ((value (json/def-like-value obj "OpSize")))
        (assert (member value '(:|OpSize16| :|OpSize32| :|OpSizeFixed|)))
        (set-field :op-size value))

      (let ((value (json/def-like-value obj "OpMap")))
        (assert (member value '(:|OB| ; One Byte
                                :|TA|
                                :|TB|
                                :|T8|
                                :|T_MAP4| :|T_MAP5| :|T_MAP6| :|T_MAP7|
                                :|XOPA|
                                :|XOP8| :|XOP9|
                                :|ThreeDNow|)))
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
      (labels
          ((get-arg-list (json-key)
             (let ((operands (json-value obj json-key)))
               (assert (equal "dag" (json-value operands "kind")))
               (json-value operands "args")))
           (normalize-arg-list (args)
             (loop :for arg :across args
                   :for type = (elt arg 0)
                   :for name = (elt arg 1)
                   :unless (eq 'null name)
                     :collect (cons name (json/def-like-as-keyword type)))))
        (let ((output-args (normalize-arg-list (get-arg-list "OutOperandList")))
              (input-args  (normalize-arg-list (get-arg-list "InOperandList"))))
          (when (string= (json-value obj "Constraints")
                         "$src = $dst")
            (assert (string= "src" (car (first input-args))))
            (assert (string= "dst" (car (first output-args))))
            (pop output-args))
          (set-field :parameters (append output-args input-args))))

      ;; TODO delme
      (let ((key (json-value obj "FormBits")
                 ;;(get-field :form)
                 #+nil(list
                       ;;(get-field :mnemonic)
                       (get-field :opcode)
                       (get-field :op-map)
                       (get-field :form)
                       )))
        (push result (gethash key *map*)))

      result)))

;; TODO delme
(defparameter *map* (make-hash-table :test 'equal))

;; (maphash (lambda (key value)
;;            (format t "~%*** ~A~%      ~A~%"
;;                    key (mapcar (lambda (instr) (getf instr :asm-string)) value)))
;;          *map*)


;; (defparameter *supported-encodings* '("EncNormal"))

;; called on the raw json
(defun json/include-instruction? (obj)
  (and (not (ends-with-subseq "_PREFIX" (json-value obj "!name")))
       (asm-string obj)
       (not (pseudo-instruction?  obj))
       (has-encoding? obj)
       (let* ((tsflags (json-value obj "TSFlags"))
              (vector-flags (subseq tsflags 40)))
         ;; for now skip all the "fancy" instructions
         (every 'zerop vector-flags))))

;; called on the normalized plist form
(defun include-instruction? (obj)
  (declare (ignore obj))
  t)

(defun process-tablegen-json (stream instruction-emitter)
  (jzon:with-parser (parser stream)
    (let ((depth 0)
          top
          stack
          key-stack
          instr-names)
      (flet ((finish-value (value)
               (typecase stack
                 (null                 (setf top value))
                 ((cons list)          (push value (car stack)))
                 ((cons hash-table)    (let ((key (pop key-stack)))
                                         (if (= depth 1)
                                             (values) ; don't collect toplevel entries
                                             (setf (gethash key (car stack)) value)))))))
        (loop
          (multiple-value-bind (event value)
              (jzon:parse-next parser)
            (ecase event
              ((nil)
               (assert (zerop (hash-table-count top)))
               (return instr-names))

              (:value
               (finish-value value))

              (:begin-array
               (push (list) stack))

              (:end-array
               (finish-value (coerce (the list (nreverse (pop stack))) 'simple-vector)))

              (:begin-object
               ;; (when (< depth 2)
               ;;   (format t ":begin-object depth ~A, key stack ~A~%" depth key-stack))
               (incf depth)
               (push (make-hash-table :test 'equal) stack))

              (:end-object
               (decf depth)
               (let ((obj (pop stack))
                     (key (first key-stack)))
                 (when (eql depth 1)
                   ;; (format t ":end-object depth ~A, key stack ~A, obj ~A~%" depth key-stack obj)
                   (cond
                     ((equal "!instanceof" key)
                      (assert (not instr-names))
                      (check-type obj hash-table)
                      (let ((name-vector (gethash "X86Inst" obj)))
                        (setf instr-names (make-hash-table :test 'equal))
                        (loop :for name :across name-vector
                              :do (setf (gethash name instr-names) t))))
                     ((and (gethash key instr-names)
                           (json/include-instruction? obj))
                      (let ((instr (normalize-instruction obj)))
                        (when (include-instruction? instr)
                          (assert (equal "X86" (json-value obj "Namespace")))
                          ;; (format *error-output* "~&; calling emitter for ~S~%" (getf instr :mnemonic))
                          (funcall instruction-emitter instr))))))
                 (finish-value obj)))

              (:object-key
               (push value key-stack)))))))))

;; (defun to-hex-byte-string (val)
;;   (assert (<= val #xff))
;;   (format nil "~x" val))

(defun intern/asm (str)
  (intern (string-upcase str)))

(defun emit-asm-form (form)
  (pprint form)
  ;;(terpri)
  (force-output))

