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
        :hu.dwim.genassem/x86)
  (:export))
