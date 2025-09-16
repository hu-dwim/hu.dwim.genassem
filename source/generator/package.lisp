;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(uiop:define-package :hu.dwim.genassem
  (:use :common-lisp
        :alexandria
        :anaphora
        :hu.dwim.genassem/asm-common
        :hu.dwim.genassem/x86
        :metabang-bind)
  (:local-nicknames
   (#:jzon :com.inuoe.jzon))
  (:export
   #:intern/asm
   #:emit-comment
   #:emit-asm-form
   #:process-tablegen-json
   ))

(uiop:define-package :hu.dwim.genassem/x86-generator
  (:use :common-lisp
        :alexandria
        :anaphora
        :hu.dwim.genassem
        :hu.dwim.genassem/asm-common
        :hu.dwim.genassem/x86
        :hu.dwim.genassem/x86/functional
        :metabang-bind)
  (:export
   #:generate-assembler/x86_64
   )
  (:shadowing-import-from
   :hu.dwim.genassem/x86
   ;; these are used as symbols instead of the tablegen names (see normalize-instruction)
   #:gr8 #:gr16 #:gr32 #:gr64
   #:vr64 #:vr128 #:vr256 #:vr512
   #:st #:sr #:cr #:dr
   ))
