;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(uiop:define-package :hu.dwim.genassem/asm-common
  (:use :alexandria
        :anaphora
        :common-lisp
        )
  (:export
   #:define-instruction
   #:emit-byte
   #:emit-bytes
   #:emit-bytes-form
   #:with-asm
   #:current-execution-mode
   #:bits
   #:buffer-of

   #:assembler-error
   #:simple-assembler-error
   #:invalid-instruction-error
   #:invalid-operand-error
   #:fully-qualified-symbol-name
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-common
        )
  (:export
   #:index-of
   #:decode-register ; TODO delme
   #:maybe-emit-operand-size-prefix
   #:emit-imm
   ))

(uiop:define-package :hu.dwim.genassem/x86/functional
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-common
        :hu.dwim.genassem/x86
        )
  (:export
   ;; these are only exported so that the output is not filled with pkg:: prefixes
   #:rex.w-part

   #:dst-reg-extra-bit
   #:dst-reg-index
   #:src-reg-extra-bit
   #:src-reg-index
   ))
