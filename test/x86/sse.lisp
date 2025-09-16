;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(defsuite* (sse :in test))

(deftest sse/1 ()
  (compare-with-external-assembler/x86
   '(((comissrr xmm2 xmm3)
      "00000000  0F2FDA            comiss xmm3,xmm2
"))))
