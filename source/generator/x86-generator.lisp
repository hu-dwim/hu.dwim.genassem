;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem)

(defun generate-x86-instruction-emitter (instr)
  (catch :skip-instruction
    (bind (((&key name parameters form      ;mnemonic
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
                               "x86.json"))
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
