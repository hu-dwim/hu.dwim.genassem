;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86-generator)

(define-constant rex.w #x08)
(define-constant rex.r #x04)
(define-constant rex.x #x02)
(define-constant rex.b #x01)

(defun needs-operand-size-prefix? (op-size)
  (eq op-size :|OpSize16|))

(defun emit-forms/imm (value bits &optional (signed? t))
  `(progn
     ,@(unless (or (and signed?
                        (<= (- (1- (expt 2 (1- bits))))
                            value
                            (1- (expt 2 bits))))
                   (and (not signed?)
                        (<= 0
                            value
                            (1- (expt 2 bits)))))
         (invalid-instruction-error "~S bit immediate operand ~A is out of range" bits value)
         (values))
     ,@(loop :for offset = 0 :then (+ offset 8)
             :while (< offset bits)
             :collect `(emit-byte (ldb (byte 8 ,offset) ,value)))))

(defmacro map-params! ((params &key (single-match? nil)) &body body)
  "What the name suggests, except that it also consumes/removes the param from the list when body returns with non-nil."
  (with-unique-names (param)
    (once-only (single-match?)
      `(loop :while (or (null results)
                        (not ,single-match?))
             :for ,param :in ,params
             :for result = (destructuring-bind (-name- . -type-) ,param
                             ,@body)
             :if (and result
                      (or (not ,single-match?)
                          (null results)))
               :collect result :into results
             :else
               :collect ,param :into new-params
             :finally
                (setf ,params new-params)
                (return (if ,single-match?
                            (first results)
                            results))))))

(defun register-type? (type)
  ;; TODO not using an ecase here is fragile...
  (member type
          '(:gr8
            :gr16
            :gr32
            :gr64)
          :test 'eq))

(defun immediate-type? (type)
  ;; TODO not using an ecase here is fragile...
  (member type
          '(:|i64imm|
            :|i32imm|
            :|i16imm|
            :|i8imm|
            :|u8imm|
            :|u4imm|
            :|i32imm_brtarget|
            :|i16imm_brtarget|
            :|i64i32imm_brtarget|
            :|i64i8imm|
            :|i64u8imm|
            :|i32u8imm|
            :|i32i8imm|
            :|i16u8imm|
            :|i16i8imm|
            :|i64i32imm|
            )
          :test 'eq))

(defun form/raw (instr name rex prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key form op-size parameters
                       &allow-other-keys)
      instr
    (let ((prefix-and-rex (append prefix-bytes
                                  (when rex
                                    (list rex))))
          (opcode-part (append opcode-prefix-bytes
                               (list opcode)))
          ;; All raw forms share that register operands are implicit.
          (parameters parameters))
      (ecase form
        ((:|RawFrmDstSrc|
          :|RawFrmDst|
          :|RawFrmSrc|)
         (setf parameters ()))
        ((:|RawFrm|
          :|RawFrmImm8|
          :|RawFrmImm16|
          :|RawFrmImm32|
          :|RawFrmMemOffs|)
         ;; nop; just to error for unexpected values
         ))
      (prog1
          `(define-instruction ,name ,(mapcar 'car parameters)
             ;; TODO add once-only wrapper for the macro args
             `(progn
                (emit-bytes ',',prefix-and-rex)
                ,@,(when (needs-operand-size-prefix? op-size)
                     ''((maybe-emit-operand-size-prefix)))
                (emit-bytes ',',opcode-part)
                ;; emit immediates, if any
                ,@,(append
                    '(list)
                    (map-params! (parameters)
                      (case -type-
                        ((:|i8imm|
                          :|i16i8imm|
                          :|i32i8imm|
                          :|i64i8imm|
                          :|brtarget8|
                          ;; TODO maybe this two should be deleted
                          ;; same for the 16 32 and 64 versions below
                          :|srcidx8|
                          :|dstidx8|
                          )
                         `(emit-forms/imm ,-name- 8))
                        ((:|offset16_8|
                          :|offset16_16|
                          :|offset16_32|
                          :|offset32_8|
                          :|offset32_16|
                          :|offset32_32|
                          :|offset32_64|
                          :|offset64_8|
                          :|offset64_16|
                          :|offset64_32|
                          :|offset64_64|)
                         ;; these are de facto obsolete, let's just skip them...
                         ;; FIXME introduce an API for this
                         (throw :skip-instruction nil))
                        (:|u8imm|
                         `(emit-forms/imm ,-name- 8 nil))
                        ((:|i16imm|
                          :|srcidx16|
                          :|dstidx16|
                          )
                         `(emit-forms/imm ,-name- 16))
                        ((:|i32imm|
                          :|i64i32imm|
                          :|srcidx32|
                          :|dstidx32|
                          :|i64i32imm_brtarget|
                          )
                         `(emit-forms/imm ,-name- 32))
                        ((:|i64imm|
                          :|srcidx64|
                          :|dstidx64|
                          )
                         `(emit-forms/imm ,-name- 64)))))))
        ;; Make sure we have consumed all the immediates
        (assert (null parameters))))))

(defun form/add-reg (instr name rex prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key op-size     ; inputs outputs
                         parameters &allow-other-keys)
      instr
    (let ((dst-param (map-params! (parameters :single-match? t)
                       (when (register-type? -type-)
                         (cons -name- -type-)))))
      (assert dst-param)
      (prog1
          `(define-instruction ,name ,(mapcar 'car (getf instr :parameters))
             (multiple-value-bind (reg-index reg-mode reg-extra-bit)
                 (register-name->encoding-bits
                  ,(car dst-param)
                  :expected-mode ,(ecase (cdr dst-param)
                                    (:gr8 8)
                                    (:gr16 16)
                                    (:gr32 32)
                                    (:gr64 64)
                                    ((:|i8imm|
                                      :|i16imm|
                                      :|i32imm|
                                      :|i64imm|
                                      )
                                     ;; TODO
                                     )))
               `(progn
                  (emit-bytes ',',prefix-bytes)
                  ,(when (eql reg-mode 64)
                     ;; TODO how come rex is nil here for e.g. bswap32r?
                     `(emit-byte ,(logior ,(or rex (logior #x40 rex.w))
                                          (if reg-extra-bit ,rex.b 0))))
                  ,@,(when (needs-operand-size-prefix? op-size)
                       ''((maybe-emit-operand-size-prefix)))
                  (emit-bytes ',',opcode-prefix-bytes)
                  (emit-byte ,(logior ',opcode reg-index))
                  ;; TODO copy-paste from above
                  ,@,(append
                      '(list)
                      (map-params! (parameters)
                        (case -type-
                          ((:|i8imm|
                            :|i16i8imm|
                            :|i32i8imm|
                            :|i64i8imm|
                            :|brtarget8|)
                           `(emit-forms/imm ,-name- 8))
                          ((:|offset16_8|
                            :|offset16_16|
                            :|offset16_32|
                            :|offset32_8|
                            :|offset32_16|
                            :|offset32_32|
                            :|offset32_64|
                            :|offset64_8|
                            :|offset64_16|
                            :|offset64_32|
                            :|offset64_64|)
                           ;; FIXME introduce an API for this
                           (throw :skip-instruction nil))
                          (:|u8imm|
                           `(emit-forms/imm ,-name- 8 nil))
                          (:|i16imm|
                           `(emit-forms/imm ,-name- 16))
                          ((:|i32imm|
                            :|i64i32imm|
                            :|i64i32imm_brtarget|)
                           `(emit-forms/imm ,-name- 32))
                          (:|i64imm|
                           `(emit-forms/imm ,-name- 64))))))))
        (assert (null parameters))))))

(defun generate-x86-instruction-emitter (instr)
  (catch :skip-instruction
    (bind (((&key name parameters form  ;mnemonic
                  opcode op-map has-rex.w
                  op-prefix             ;op-prefix/explicit
                  form-category has-position-order
                  &allow-other-keys) instr)
           (lisp-name (intern/asm (concatenate 'string "_" name)))
           (prefix-bytes ())
           (opcode-prefix-bytes ())
           (rex (when has-rex.w
                  (logior #x40 rex.w))))
      ;;(format *error-output* "~&; emitting ~S / ~S~%" name mnemonic)
      (assert (<= opcode 255))
      (assert has-position-order)
      (emit-comment form " " name " " parameters)
      (progn
        (ecase op-prefix
          ((nil)     ())
          (:pd       (push #x66 prefix-bytes))
          ((:sd :xd) (push #xf2 prefix-bytes))
          ((:ps :xs) (push #xf3 prefix-bytes)))
        (case op-map
          (:tb (push #x0f opcode-prefix-bytes)))

        (let ()
          ;; (when (equal mnemonic "adc{w}	{$src, %ax|ax, $src}")
          ;;   (break))
          ;; (when (equal name "RET64")
          ;;   (break))
          ;; (when (starts-with-subseq "OUT" name)
          ;;   (break))
          (ecase form-category
            (:raw
             ;; RawFrm specifically means raw form: the instruction has no special ModR/M or opcode map handling—it’s just a fixed sequence of bytes.
             (emit-asm-form
              (form/raw instr
                        lisp-name
                        rex
                        (nreverse prefix-bytes)
                        (nreverse opcode-prefix-bytes)
                        opcode))
             lisp-name)
            (:add-reg
             (emit-asm-form
              (form/add-reg instr
                            lisp-name
                            rex
                            (nreverse prefix-bytes)
                            (nreverse opcode-prefix-bytes)
                            opcode))
             lisp-name)
            (:mrm
             ;; TODO
             )))))))

(defun setup-pprint-dispatch ()
  (set-pprint-dispatch
   '(cons (eql define-instruction))
   (pprint-dispatch '(defun x (y)))))

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
            (*print-right-margin* 1160)
            (*package* package)
            (*print-pprint-dispatch* (copy-pprint-dispatch)))
        (setup-pprint-dispatch)
        (write-string ";;; This file is generated; editing it is unwise.")
        (pprint `(in-package ,(intern (package-name package) :keyword)))
        (terpri)
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
          (with-open-file (jstream (asdf:system-relative-pathname
                                    :hu.dwim.genassem
                                    "tablegen/x86-20.1.8.json"))
            (process-tablegen-json
             jstream
             (lambda (obj)
               (incf instr-counter)
               (let* ((predicates (getf obj :predicates))
                      (include? (loop :for blacklisted :in predicate-blacklist
                                      :always (not (member blacklisted predicates :test 'eq)))))
                 (if include?
                     (progn
                       (write-char #\. *error-output*)
                       (incf emit-counter)
                       (when print-source?
                         (print obj))
                       (push (generate-x86-instruction-emitter obj)
                             symbols-to-export))
                     (write-char #\x *error-output*))))))
          (emit-asm-form `(export '(,@ (remove nil symbols-to-export))))
          (format *error-output* "~&; emitted ~S of ~S instructions~%"
                  emit-counter instr-counter))))))
