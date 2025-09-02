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
          '(:gr8
            :gr16
            :gr32
            :gr64
            :|GR32orGR64|
            :|GR16orGR32orGR64|
            :vr64
            :vr128
            :vr256
            :vr512
            :|RSTi|
            ;; TODO these are not handled at assembly-time
            :segment_reg
            :control_reg
            :debug_reg
            )
          :test 'eq))

(defun segment-register-type? (type)
  (eq type :segment_reg))

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
                         (skip-instruction))
                        (:|u8imm|
                         `(emit-forms/imm ,-name- 8 nil))
                        ((:|i16imm|
                          )
                         `(emit-forms/imm ,-name- 16))
                        ((:|i32imm|
                          :|i64i32imm|
                          :|i64i32imm_brtarget|
                          )
                         `(emit-forms/imm ,-name- 32))
                        ((:|i64imm|
                          )
                         `(emit-forms/imm ,-name- 64)))))))
        ;; Make sure we have consumed all the immediates
        (assert (null parameters))))))

(defun form/add-reg (instr name rex prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key op-size     ; inputs outputs
                         parameters &allow-other-keys)
      instr
    (let ((dst-param (pop-reg-param! parameters)))
      (assert dst-param)
      (prog1
          `(define-instruction ,name ,(mapcar 'car (getf instr :parameters))
             (multiple-value-bind (reg-index/1 reg-mode/1 reg-extra-bit/1)
                 (decode-register ,(car dst-param) ,(cdr dst-param))
               `(progn
                  (emit-bytes ',',prefix-bytes)
                  ,(when (eql reg-mode/1 64)
                     ;; TODO how come rex is nil here for e.g. bswap32r?
                     `(emit-byte ,(logior ,(or rex (logior #x40 rex.w))
                                          (if reg-extra-bit/1 ,rex.b 0))))
                  ,@,(when (needs-operand-size-prefix? op-size)
                       ''((maybe-emit-operand-size-prefix)))
                  (emit-bytes ',',opcode-prefix-bytes)
                  (emit-byte ,(logior ',opcode reg-index/1))
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
                           (skip-instruction))
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

(defun form/mrm (instr name rex prefix-bytes opcode-prefix-bytes opcode)
  (destructuring-bind (&key form op-size
                         parameters &allow-other-keys)
      instr
    (let* ((modrm 0)
           (form-str (symbol-name form))
           (form-length (length form-str))
           (dst-reg-param nil)
           (src-reg-param nil))
;; +---+---+---+---+---+---+---+---+
;; |   mod  |  reg/opcode |  r/m   |
;; +---+---+---+---+---+---+---+---+
;;   7   6     5   4   3     2   1 0
;;
;; - mod (2 bits): addressing mode (register, memory, disp8, disp32, etc.)
;; - reg/opcode (3 bits):
;;   Normally selects a register operand,
;;   but in some opcodes, it’s treated as an opcode extension.
;; - r/m (3 bits): register or memory operand (sometimes extended with SIB if r/m=100).

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
;;; MRMDestReg
;;;
        ((or (equal form-str "MRMDestReg")
             (equal form-str "MRMSrcReg"))
         (setf (ldb (byte 2 6) modrm) #b11)
         (let ((segment-reg
                 (pop-reg-param! parameters 'segment-register-type?)))
           (when segment-reg
             ;; ...then we need to make sure the segment register is
             ;; in src for te right encoding. this is somwhat kludgey.
             (setf src-reg-param segment-reg))
           (setf dst-reg-param (pop-reg-param! parameters))
           (unless src-reg-param
             (setf src-reg-param (pop-reg-param! parameters)))))

        (t
         ;; TODO
         (skip-instruction)
         ;; (error "Unexpected MRM form value: ~S" form)
         ))
      (assert dst-reg-param)
      (when (intersection (list (cdr dst-reg-param) (cdr src-reg-param))
                          '(:control_reg
                            ;;:segment_reg
                            :debug_reg))
        ;; TODO
        (skip-instruction))
      (prog1
          `(define-instruction ,name ,(mapcar 'car (getf instr :parameters))
             (multiple-value-bind (reg-index/1 reg-mode/1 reg-extra-bit/1)
                 (decode-register ,(car dst-reg-param) ,(cdr dst-reg-param))
               (,@(if (not src-reg-param)
                      '(progn)
                      `(multiple-value-bind (reg-index/2 reg-mode/2 reg-extra-bit/2)
                           (decode-register ,(car src-reg-param) ,(cdr src-reg-param))))
                `(progn
                   (emit-bytes ',',prefix-bytes)
                   ;; TODO when expected-mode is present and it's not
                   ;; 64 then this whole WHEN is unnecessary
                   ,(when (or (eql reg-mode/1 64)
                              ,@(when src-reg-param
                                  `((eql reg-mode/2 64))))
                      `(emit-byte ,(logior ,(or rex (logior #x40 rex.w))
                                           (if reg-extra-bit/1 ,rex.b 0)
                                           ,@(when src-reg-param
                                               `((if reg-extra-bit/2 ,rex.r 0))))))
                   ,@,(when (needs-operand-size-prefix? op-size)
                        ''((maybe-emit-operand-size-prefix)))
                   (emit-bytes ',',opcode-prefix-bytes)
                   (emit-byte ',',opcode)
                   (emit-byte ',(logior ',modrm reg-index/1
                                        ,@(when src-reg-param
                                            `((ash reg-index/2 3)))))
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
                            (skip-instruction))
                           ((:|u8imm|
                             :|i16u8imm|
                             :|i32u8imm|
                             :|i64u8imm|)
                            `(emit-forms/imm ,-name- 8 nil))
                           (:|i16imm|
                            `(emit-forms/imm ,-name- 16))
                           ((:|i32imm|
                             :|i64i32imm|
                             :|i64i32imm_brtarget|)
                            `(emit-forms/imm ,-name- 32))
                           (:|i64imm|
                            `(emit-forms/imm ,-name- 64)))))))))
        ;; TODO (assert (null parameters))
        ))))

(defun skip-instruction ()
  (throw :skip-instruction nil))

(defun generate-x86-instruction-emitter (instr)
  (catch :skip-instruction
    (bind (((&key name parameters form  ;mnemonic
                  opcode op-enc op-map has-rex.w
                  op-prefix             ;op-prefix/explicit
                  form-category has-position-order
                  &allow-other-keys) instr)
           (lisp-name (intern/asm (concatenate 'string "_" name)))
           (prefix-bytes ())
           (opcode-prefix-bytes ())
           (rex (when has-rex.w
                  (logior #x40 rex.w))))
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
                        rex
                        (nreverse prefix-bytes)
                        (nreverse opcode-prefix-bytes)
                        opcode))
             lisp-name)))))))

(defun setup-pprint-dispatch ()
  (set-pprint-dispatch
   '(cons (eql define-instruction))
   (pprint-dispatch '(defun x (y)))))

(defun generate-assembler/x86_64 (&key
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
                       (push (generate-x86-instruction-emitter obj)
                             symbols-to-export))
                     (write-char #\x *error-output*))))))
          (emit-asm-form `(export '(,@ (remove nil symbols-to-export))))
          (format *error-output* "~&; emitted ~S of ~S instructions~%"
                  emit-counter instr-counter))))))
