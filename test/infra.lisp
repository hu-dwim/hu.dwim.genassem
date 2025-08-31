;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(in-suite test)

(defsuite* infra)

(defun register-lookup ()
  (mapcar (lambda (el)
            (is (equal (second el)
                       (multiple-value-list
                        (decode-register (first el) :gr8)))))
          '((al   (0 8 ()))
            (bl   (3 8 ()))
            (r13b (5 8 1))
            (r8b  (0 8 1))
            ))
  (mapcar (lambda (el)
            (is (equal (second el)
                       (multiple-value-list
                        (decode-register (first el) :gr16)))))
          '((bx   (3 16 ()))
            (r13w (5 16 1))
            (si   (6 16 ()))
            ))
  (mapcar (lambda (el)
            (is (equal (second el)
                       (multiple-value-list
                        (decode-register (first el) :gr32)))))
          '((eax  (0 32 ()))
            (edx  (2 32 ()))
            (r11d (3 32 1))
            (esi  (6 32 ()))
            ))
  (mapcar (lambda (el)
            (is (equal (second el)
                       (multiple-value-list
                        (decode-register (first el) :gr64)))))
          '((rax  (0 64 ()))
            (r8   (0 64 1))
            (r15  (7 64 1))
            (rsi  (6 64 ()))
            ))
  (values))
