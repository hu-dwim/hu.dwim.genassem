;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl)

(defpackage :hu.dwim.genassem/x86.test
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.stefil
        :hu.dwim.genassem/asm-common
        :hu.dwim.genassem/x86
        )
  (:export
   #:test
   ))
