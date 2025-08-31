;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

;;; This file contans the support definitions for the x86 assemblers
;;; that are needed at runtime (assembling-time). IOW, these are the
;;; definitions that the generated instruction emitters need at
;;; runtime.

(in-package :hu.dwim.genassem/x86)

(define-constant +x86-registers/8+  '(al  cl  dl  bl  spl bpl sil dil) :test 'equal)
(define-constant +x86-registers/16+ '(ax  cx  dx  bx  sp  bp  si  di)  :test 'equal)
(define-constant +x86-registers/32+ '(eax ecx edx ebx esp ebp esi edi) :test 'equal)
(define-constant +x86-registers/64+ '(rax rcx rdx rbx rsp rbp rsi rdi) :test 'equal)

(declaim (inline register-index))
(defun register-index (name registers &optional (default nil default?))
  (declare (type symbol name))
  (or (position name registers :test 'eq)
      (if default?
          default
          (error "Unexpected register name: ~S" name))))

(defun register-name->encoding-bits (name &key expected-class)
  "Returns (values reg-index reg-mode reg-extra-bit)."
  (declare (optimize (debug 3))
           (type symbol name)
           (type (member nil :gr8 :gr16 :gr32 :gr64
                         :|GR32orGR64|)
                 expected-class))
  (flet ((illegal-register ()
           (invalid-instruction-error
            "Trying to use register ~S while expecting a ~S" name expected-class)))
    (let* ((name/s (symbol-name name))
           (len    (length name/s))
           (last-char (elt name/s (1- len))))
      (case (elt name/s 0)
        (#\R
         ;; 64 bit
         (when (and expected-class
                    (not (member expected-class
                                 '(:gr64 :|GR32orGR64| :|GR16orGR32orGR64|)
                                 :test 'eq)))
           (illegal-register))
         (aif (register-index name +x86-registers/64+ nil)
              (values it 64 nil)
              (let (
                    (index (- (char-code (elt name/s 1))
                              (char-code #\0)))
                    (class 64))
                (case last-char
                  (#\D
                   (setf class 32)
                   (decf len))
                  (#\W
                   (setf class 16)
                   (decf len))
                  (#\B
                   (setf class 8)
                   (decf len)))
                (ecase len
                  (2)
                  (3 (setf index (+ (* index 10)
                                    (- (char-code (elt name/s 2))
                                       (char-code #\0))))))
                (unless (<= 8 index 15)
                  (invalid-instruction-error
                   "Invalid register name: ~S" name))
                (values (logand #b111 index) class 1))))
        (#\E
         ;; 32 bit
         (when (and expected-class
                    (not (member expected-class
                                 '(:gr32 :|GR32orGR64| :|GR16orGR32orGR64|)
                                 :test 'eq)))
           (illegal-register))
         (values (register-index name +x86-registers/32+) 32 nil))
        (t
         (cond
           ((eql last-char #\L)
            ;; 8 bit
            (when (and expected-class
                       (not (eq expected-class :gr8)))
              (illegal-register))
            (values (register-index name +x86-registers/8+) 8 nil))
           (t
            ;; 16 bit
            (when (and expected-class
                       (not (member expected-class
                                    '(:gr16 :|GR16orGR32orGR64|)
                                    :test 'eq)))
              (illegal-register))
            (values (register-index name +x86-registers/16+) 16 nil))))))))

(defun maybe-emit-operand-size-prefix ()
  (when (not (eql (current-execution-mode) 16))
    (emit-byte #x66)))
