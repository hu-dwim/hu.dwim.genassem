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

(defun may-have-operand-size-prefix? (op-size)
  (ecase op-size
    ((:|OpSize16|)
     t)
    ((:|OpSize32|
      :|OpSizeFixed|)
     nil)))

;; TODO drop the unused single-match? complexity
(defmacro map-params! ((params &key (single-match? nil)) &body body)
  "What the name suggests, except that it also consumes/removes the param from the list when body returns with non-nil."
  (with-unique-names (param)
    (once-only (single-match?)
      `(loop :while (or (null results)
                        (not ,single-match?))
             :for ,param :in ,params
             :for result = (destructuring-bind (-name- . -type-) ,param
                             ,@body)
             :if result
               :collect result :into results
             :else
               :collect ,param :into new-params
             :finally
                (return
                  (if ,single-match?
                      (progn
                        (when result
                          (setf ,params (remove ,param ,params)))
                        (first results))
                      (progn
                        (setf ,params new-params)
                        results)))))))

(defmacro pop-reg-param! (params &optional (predicate ''register-type?))
  `(let ((match (find-if ,predicate ,params :key 'cdr)))
     (when match
       (setf ,params (remove match ,params)))
     match))

(defun register-type? (type)
  ;; TODO not using an ecase here is fragile...
  (member type
          '(gr8
            gr16
            gr32
            gr64
            :|GR32orGR64|
            :|GR16orGR32orGR64|
            vr64
            vr128 ; :fr32 :fr64
            vr256
            vr512
            st    ; :|RSTi|
            sr    ; :segment_reg
            cr    ; :control_reg
            dr    ; :debug_reg
            )
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

(defun emit-imm (value bits &optional (signed? t))
  (unless (or (and signed?
                   (<= (- (1- (expt 2 (1- bits))))
                       value
                       (1- (expt 2 bits))))
              (and (not signed?)
                   (<= 0
                       value
                       (1- (expt 2 bits)))))
    (invalid-instruction-error "~S bit immediate operand ~A is out of range" bits value))
  (loop :for offset = 0 :then (+ offset 8)
        :while (< offset bits)
        :do (emit-byte (ldb (byte 8 offset) value))))

(defun emit-imm-forms (parameters)
  (prog1
      (map-params! (parameters)
        (case -type-
          ((:|i8imm|
            :|i16i8imm|
            :|i32i8imm|
            :|i64i8imm|
            :|brtarget8|
            )
           `(emit-imm ,-name- 8))
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
           (skip-instruction))
          (:|u8imm|
           `(emit-imm ,-name- 8 nil))
          ((:|i16imm|
            :|i16imm_brtarget|
            )
           `(emit-imm ,-name- 16))
          ((:|i32imm|
            :|i64i32imm|
            :|i32imm_brtarget|
            :|i64i32imm_brtarget|
            )
           `(emit-imm ,-name- 32))
          ((:|i64imm|
             )
           `(emit-imm ,-name- 64))))
    ;; Make sure we have consumed all the paramaters (immediates are
    ;; always the last ones).
    (when parameters
      (format *error-output* "TODO: some operands have remained unprocessed: ~A~%" parameters))))

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
      `(define-instruction ,name ,(mapcar 'car parameters)
         ,@(emit-bytes-form prefix-and-rex)
         ,@(when (may-have-operand-size-prefix? op-size)
             '((maybe-emit-operand-size-prefix)))
         ,@(emit-bytes-form opcode-part)
         ;; emit immediates, if any
         ,@(emit-imm-forms parameters)))))

(defun form/add-reg (instr name rex prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key op-size     ; inputs outputs
                         parameters &allow-other-keys)
      instr
    (let* ((dst-reg-param (pop-reg-param! parameters))
           (dst-reg  (car dst-reg-param))
           (dst-type (cdr dst-reg-param)))
      (assert dst-reg-param)
      `(define-instruction ,name ,(mapcar 'car (getf instr :parameters))
         (multiple-value-bind (dst-reg-index dst-reg-extra-bit)
             (decode-register ,dst-reg ,dst-type)
           ,@(emit-bytes-form prefix-bytes)
           ;; TODO OPTIMIZATION: sometimes this typep is known to
           ;; be false at generation time; add a dispatch to it.
           (when (typep ,dst-reg 'gr64)
             ;; TODO how come rex is nil here for e.g. bswap32r?
             (emit-byte (logior ,(or rex (logior #x40 rex.w))
                                (if dst-reg-extra-bit ,rex.b 0))))
           ,@(when (may-have-operand-size-prefix? op-size)
               '((maybe-emit-operand-size-prefix)))
           ,@(emit-bytes-form opcode-prefix-bytes)
           (emit-byte (logior ',opcode dst-reg-index))
           ,@(emit-imm-forms parameters))))))

(defun form/mrm (instr name prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key form op-size has-rex.w
                         parameters &allow-other-keys)
      instr
    (let* ((modrm 0)
           (form-str (symbol-name form))
           (form-length (length form-str))
           (dst-reg-param nil)
           (src-reg-param nil))
;;; +---+---+---+---+---+---+---+---+
;;; |   mod  |  reg/opcode |  r/m   |
;;; +---+---+---+---+---+---+---+---+
;;;   7   6     5   4   3     2   1 0
;;;
;;; - mod (2 bits): addressing mode (register, memory, disp8, disp32, etc.)
;;; - reg/opcode (3 bits):
;;;   Normally selects a register operand,
;;;   but in some opcodes, it’s treated as an opcode extension.
;;; - r/m (3 bits): register or memory operand (sometimes extended with SIB if r/m=100).

      (cond
;;;
;;; MRM0r–MRM7r
;;;
        ((and (= 5 form-length)
              (eql #\r (elt form-str (1- form-length))))
         (let ((idx (- (char-code (elt form-str 3))
                       (char-code #\0))))
           (assert (<= 0 idx 7))
           (setf (ldb (byte 2 6) modrm) #b11)
           (setf (ldb (byte 3 3) modrm) idx)
           (setf dst-reg-param (pop-reg-param! parameters))
           ;; This may or may not pop a register.
           (setf src-reg-param (pop-reg-param! parameters))))
;;;
;;; MRMSrcReg
;;;
        ((equal form-str "MRMSrcReg")
         ;; source:      mrm.reg, rex.r
         ;; destination: mrm.r/m, rex.b
         (setf (ldb (byte 2 6) modrm) #b11)
         (setf dst-reg-param (pop-reg-param! parameters))
         (setf src-reg-param (pop-reg-param! parameters)))

;;;
;;; MRMDestReg
;;;
        ((equal form-str "MRMDestReg")
         ;; source:      mrm.r/m, rex.b
         ;; destination: mrm.reg, rex.r
         (setf (ldb (byte 2 6) modrm) #b11)
         (setf src-reg-param (pop-reg-param! parameters))
         (setf dst-reg-param (pop-reg-param! parameters)))

        (t
         ;; TODO
         (skip-instruction)
         ;; (error "Unexpected MRM form value: ~S" form)
         ))
      (assert dst-reg-param)
      (let ((dst-reg  (car dst-reg-param))
            (dst-type (cdr dst-reg-param))
            (src-reg  (car src-reg-param))
            (src-type (cdr src-reg-param)))
        `(define-instruction ,name ,(mapcar 'car (getf instr :parameters))
           (multiple-value-bind (dst-reg-index dst-reg-extra-bit)
               (decode-register ,dst-reg ,dst-type)
             (,@(if src-reg
                    `(multiple-value-bind (src-reg-index src-reg-extra-bit)
                         (decode-register ,src-reg ,src-type))
                    '(progn))
              ,@(emit-bytes-form prefix-bytes)
              ;; REX
              (let (,@(when has-rex.w
                        `((rex.w-part (if (typep ,dst-reg 'gr64)
                                          ,rex.w
                                          0)))))
                (when (or ,@(when has-rex.w
                              '((not (zerop rex.w-part))))
                          dst-reg-extra-bit
                          ,@(when src-reg
                              '(src-reg-extra-bit)))
                  (emit-byte (logior #x40 ,@(when has-rex.w
                                              '(rex.w-part))
                                     (if dst-reg-extra-bit ,rex.b 0)
                                     ,@(when src-reg
                                         `((if src-reg-extra-bit ,rex.r 0)))))))
              ,@(when (may-have-operand-size-prefix? op-size)
                  '((maybe-emit-operand-size-prefix)))
              ,@(emit-bytes-form opcode-prefix-bytes)
              (emit-byte ',opcode)
              (emit-byte (logior ',modrm dst-reg-index ; modrm.r/m
                                 ,@(when src-reg
                                     `((ash src-reg-index 3))))) ; modrm.reg
              ,@(emit-imm-forms parameters))))))))

(defun skip-instruction ()
  (throw :skip-instruction nil))

(defun generate-x86-instruction-emitter (instr)
  (catch :skip-instruction
    (bind (((&key name parameters form predicates
                  opcode op-enc op-map has-rex.w
                  op-prefix             ;op-prefix/explicit
                  form-category has-position-order
                  &allow-other-keys) instr)
           (lisp-name (intern/asm (if (find-symbol name :common-lisp)
                                      ;; clases with CL:LOOP
                                      (concatenate 'string "_" name)
                                      name)))
           (prefix-bytes ())
           (opcode-prefix-bytes ())
           (rex (when has-rex.w
                  (logior #x40 rex.w)))
           (*package* (find-package :hu.dwim.genassem/x86/functional))
           ;; normalize the param types to our clos classes
           ;; representing the various register classes
           (parameters
            (mapcar (lambda (param)
                      (bind (((name . type) param))
                        (cons name (case type
                                     (:|RSTi|       'st)
                                     ((:fr32 :fr64) 'vr128)
                                     (:control_reg  'cr)
                                     (:debug_reg    'dr)
                                     (:segment_reg  'sr)
                                     ((:gr8 :gr16 :gr32 :gr64
                                       :vr64 :vr128 :vr512 :vr256)
                                      (aprog1
                                          (find-symbol (symbol-name type) *package*)
                                        (assert it)))
                                     (otherwise type)))))
                    parameters))
           ;; KLUDGE get rid of this...
           (instr (list* :parameters parameters instr)))
      (unless (null (intersection '(:|HasSSE4A| ; irrelevant AMD-only extension
                                    )
                                  predicates))
        (skip-instruction))
      ;;(format *error-output* "~&; emitting ~S~%" name)
      (assert (<= opcode 255))
      (assert has-position-order)
      (emit-comment op-enc "	" form "	" name "	" parameters "	" opcode
                    "	" op-prefix "	" op-map)
      (progn
        (ecase op-enc
          (:|EncEVEX|
           (skip-instruction))
          (:|EncVEX|
           (skip-instruction))
          (:|EncXOP|
           (skip-instruction))
          (:|EncNormal|
           (ecase op-prefix
             ((nil)     ())
             (:pd       (push #x66 prefix-bytes))
             ((:sd :xd) (push #xf2 prefix-bytes))
             ((:ps :xs) (push #xf3 prefix-bytes))
             ;;((:ps :xs) (push #xc4 prefix-bytes))
             )
           (ecase op-map
             (:ob)
             ((:tb) (push #x0f opcode-prefix-bytes))
             ((:t8)
              (push #x0f opcode-prefix-bytes)
              (push #x38 opcode-prefix-bytes))
             ((:ta)
              (push #x0f opcode-prefix-bytes)
              (push #x3a opcode-prefix-bytes))
             ((:xop8)
              (push #x8f opcode-prefix-bytes)
              (push #x08 opcode-prefix-bytes))
             ((:xop9)
              (push #x8f opcode-prefix-bytes)
              (push #x90 opcode-prefix-bytes))
             ((:xopa)
              (push #x8f opcode-prefix-bytes)
              (push #x0a opcode-prefix-bytes))
             ((:|ThreeDNow|)
              ;; TODO ?
              (skip-instruction)))))
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
             (emit-asm-form
              (form/mrm instr
                        lisp-name
                        (nreverse prefix-bytes)
                        (nreverse opcode-prefix-bytes)
                        opcode))
             lisp-name)))))))

(defun setup-pprint-dispatch ()
  (set-pprint-dispatch
   '(cons (eql define-instruction))
   (pprint-dispatch '(defun x (y)))))

(defun generate-assembler/x86_64 (&key (package :hu.dwim.genassem/x86/functional))
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
                  ;; :|UseSSE1|
                  ;; :|UseSSE2|
                  ;; :|UseSSE3|
                  ;; :|UseSSE41|
                  ;; :|UseSSE42|
                  ;; :|HasSSEPrefetch|
                  ;; :|UseAVX|
                  ;; :|HasAVX|
                  ;; :|HasAVX2|
                  ;; :|HasAVX512|
                  ;; :|HasF16C| ; SSE; half-precision (16-bit) fp -> single-precision (32-bit) fp
                  ;; :|HasRTM|
                  ;; :|HasTSXLDTRK|
                  ;; :|HasWBNOINVD|
                  ;; :|HasAMXTILE|
                  ;; :|HasAMXMOVRS|
                  ;; :|HasAMXTRANSPOSE|
                  ;; :|HasAMXCOMPLEX|
                  ;; :|HasMOVRS|
                  ;; :|HasPCLMUL|
                  ;; :|HasTBM|
                  ;; :|HasCLDEMOTE|
                  ;; :|HasUINTR|
                  ))
              (instr-counter 0)
              (emit-counter 0)
              (symbols-to-export ()))
          (with-open-file (jstream (asdf:system-relative-pathname
                                    :hu.dwim.genassem
                                    "tablegen/x86-20.1.8.json"))
            (process-tablegen-json
             jstream
             (lambda (instr)
               (incf instr-counter)
               (let* ((predicates (getf instr :predicates))
                      (include? (loop :for blacklisted :in predicate-blacklist
                                      :always (not (member blacklisted predicates :test 'eq)))))
                 (if include?
                     (progn
                       (write-char #\. *error-output*)
                       (incf emit-counter)
                       (push (generate-x86-instruction-emitter instr)
                             symbols-to-export))
                     (write-char #\x *error-output*))))))
          (emit-asm-form `(export '(,@ (remove nil symbols-to-export))))
          (format *error-output* "~&; emitted ~S of ~S instructions~%"
                  emit-counter instr-counter))))))
