;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(defsuite* (x87 :in test))

(deftest x87/mrm/1 ()
  (compare-with-external-assembler/x86
   '(((mul_fprst0 st3)
      (subr_fprst0 st7)
      (subr_fst0r st1)
      "00000000  DECB              fmulp st3
00000002  DEE7              fsubrp st7
00000004  D8E9              fsubr st1
"))))
