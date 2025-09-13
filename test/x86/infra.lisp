;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(in-suite test)

(defsuite* infra)

(deftest register-lookup ()
  (macrolet ((test-it (register-class elements)
               `(mapcar (lambda (el)
                          (is (equal (second el)
                                     (multiple-value-list
                                      (decode-register (first el) ,register-class)))))
                        ,elements)))
    (test-it gr8
             `((,al   (0 ()))
               (,bl   (3 ()))
               (,r13b (5 1))
               (,r8b  (0 1))
               ))
    (test-it gr16
             `((,bx   (3 ()))
               (,r13w (5 1))
               (,si   (6 ()))
               ))
    (test-it gr32
             `((,eax  (0 ()))
               (,edx  (2 ()))
               (,r11d (3 1))
               (,esi  (6 ()))
               ))
    (test-it gr64
             `((,rax  (0 ()))
               (,r8   (0 1))
               (,r15  (7 1))
               (,rsi  (6 ()))
               ))
    (test-it st
             `((,st0  (0 ()))
               (,st3  (3 ()))
               (,st7  (7 ()))
               ))
    (test-it vr64
             `((,mm0  (0 ()))
               (,mm3  (3 ()))
               (,mm7  (7 ()))
               ))
    (test-it vr128
             `((,xmm0  (0 ()))
               (,xmm3  (3 ()))
               (,xmm7  (7 ()))
               (,xmm15 (7 1))
               ))
    (test-it vr256
             `((,ymm0  (0 ()))
               (,ymm3  (3 ()))
               (,ymm7  (7 ()))
               (,ymm15 (7 1))
               ))
    (test-it vr512
             `((,zmm0  (0 ()))
               (,zmm3  (3 ()))
               (,zmm7  (7 ()))
               (,zmm14 (6 1))
               ))
    (values)))
