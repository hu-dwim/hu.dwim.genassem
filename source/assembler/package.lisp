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
   #:with-asm
   #:current-execution-mode
   #:bits
   #:buffer-of

   #:assembler-error
   #:simple-assembler-error
   #:invalid-instruction-error
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-common
        )
  (:export
   #:register-name->encoding-bits
   #:form/raw
   #:form/add-reg
   ))
