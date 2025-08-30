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
        :metabang-bind)
  (:export
   #:generate-assembler/x86_64
   ))
