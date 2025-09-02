;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(defsuite* (test :in root-suite))

(defun disasm-output/stdin (cmdline asm-context)
  (uiop:run-program
   cmdline
   :external-format :iso-8859-1
   :input (make-string-input-stream
           (babel:octets-to-string
            (buffer-of asm-context)
            :encoding :iso-8859-1))
   :output :string
   :error-output :string))

(defun disasm-output/tmp-file (cmdline asm-context)
  (check-type cmdline list)
  (uiop:with-temporary-file (:stream stream :pathname path
                             :element-type '(unsigned-byte 8))
    (write-sequence (buffer-of asm-context) stream)
    (finish-output stream)
    (uiop:run-program
     (append cmdline (list (namestring path)))
     :output :string
     :error-output :string)))

(defun ndisasm-output (asm-context)
  (disasm-output/stdin '("ndisasm" "-b" "64" "-p" "intel" "-") asm-context))

(defun xed-output (asm-context)
  (disasm-output/tmp-file '("xed" "-64" "-ir") asm-context))

(defun zydis-output (asm-context)
  (disasm-output/tmp-file '("ZydisDisasm""-64") asm-context))

(defun compare-with-external-assembler/x86 (forms)
  (loop :for entry :in forms
        :for instrs = (butlast entry)
        :for expected = (car (last entry))
        :for session = (eval `(with-asm () ,@instrs))
        :do (is (equal expected (ndisasm-output session)))
            ;; :do (print (xed-output context))
            ;; :do (print (zydis-output context))
        ))
