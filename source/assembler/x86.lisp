;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86)

(define-constant +x86-registers/16+ '(ax cx dx bx sp bp si di) :test 'equal)
(define-constant +x86-registers/32+ '(eax ecx edx ebx esp ebp esi edi) :test 'equal)
(define-constant +x86-registers/64+ '(rax rcx rdx rbx rsp rbp rsi rdi) :test 'equal)

(declaim (inline register-index))
(defun register-index (name registers &optional (default nil default?))
  (declare (type symbol name))
  (or (position name registers :test 'string=)
      (if default?
          default
          (error "Unexpected register name: ~S" name))))

(defun register-name->encoding-bits (name &key expected-mode)
  (declare (optimize (debug 3))
           (type symbol name))
  (flet ((illegal-register ()
           (invalid-instruction-error
            "Trying to use register ~S in ~S bit mode" name expected-mode)))
    (let ((name/s (symbol-name name)))
      (case (elt name/s 0)
        (#\R
         (when (and expected-mode
                    (not (eql expected-mode 64)))
           (illegal-register))
         (aif (register-index name +x86-registers/64+ nil)
              (values it 64 nil)
              (values (let ((length (length name/s))
                            (index (- (char-code (elt name/s 1))
                                      (char-code #\0))))
                        (ecase length
                          (2 (values))
                          (3 (setf index (+ (* index 10)
                                            (- (char-code (elt name/s 2))
                                               (char-code #\0))))))
                        (unless (<= 8 index 15)
                          (invalid-instruction-error
                           "Invalid register name: ~S" name))
                        index)
                      64 1)))
        (#\E
         (when (and expected-mode
                    (not (eql expected-mode 32)))
           (illegal-register))
         (values (register-index name +x86-registers/32+) 32 nil))
        (t
         (when (and expected-mode
                    (not (eql expected-mode 16)))
           (illegal-register))
         (values (register-index name +x86-registers/16+) 16 nil))))))

(define-constant rex.w #x08)
(define-constant rex.r #x04)
(define-constant rex.x #x02)
(define-constant rex.b #x01)

(defun needs-operand-size-prefix? (op-size)
  (or (and (eq op-size :|OpSize16|)
           (not (eql (current-execution-mode) 16)))
      (and (eq op-size :|OpSize32|)
           (not (eql (current-execution-mode) 64)))))

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
                (when (needs-operand-size-prefix? ',',op-size)
                  (emit-byte #x66))
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
                  (when (needs-operand-size-prefix? ',',op-size)
                    (emit-byte #x66))
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
