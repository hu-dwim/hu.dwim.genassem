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

(defun maybe-emit-operand-size-prefix ()
  (when (not (eql (current-execution-mode) 16))
    (emit-byte #x66)))

;;;
;;; Register lookup/decode
;;;

(defmacro decode-register (name expected-class)
  (ecase expected-class
    (:gr8  `(register-name->encoding/gr8  ,name))
    (:gr16 `(register-name->encoding/gr16 ,name))
    (:gr32 `(register-name->encoding/gr32 ,name))
    (:gr64 `(register-name->encoding/gr64 ,name))
    (:|GR32orGR64| `(multiple-value-bind (index mode extra-bit)
                        (register-name->encoding/gr32 ,name nil)
                      (if index
                          (values index mode extra-bit)
                          (register-name->encoding/gr64 ,name))))
    (:|GR16orGR32orGR64| `(multiple-value-bind (index mode extra-bit)
                              (register-name->encoding/gr16 ,name nil)
                            (cond
                              (index
                               (values index mode extra-bit))
                              ((progn
                                 (multiple-value-setq (index mode extra-bit)
                                   (register-name->encoding/gr32 ,name nil))
                                 index)
                               (values index mode extra-bit))
                              (t (register-name->encoding/gr64 ,name)))))
    (:segment_reg
     `(register-name->encoding/segment ,name))
    (:|RSTi| ; ST(0)-ST(7)
     `(register-name->encoding/2letter ,name ,expected-class #\S #\T))
    (:vr64 ; MM0-MM7
     `(register-name->encoding/2letter ,name ,expected-class #\M #\M))
    (:vr128 ; XMM0-XMM15
     `(register-name->encoding/3letter ,name ,expected-class #\X #\M #\M))
    (:vr256 ; YMM0-YMM15
     `(register-name->encoding/3letter ,name ,expected-class #\Y #\M #\M))
    (:vr512 ; ZMM0-ZMM15
     `(register-name->encoding/3letter ,name ,expected-class #\Z #\M #\M))))

(define-constant +x86-registers/segment+ '(es cs ss ds fs gs) :test 'equal)
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

(defun unexpected-register (name expected-class)
  (invalid-instruction-error
   "Trying to use register ~S while expecting a ~S" name expected-class))

(defun unknown-register (name)
  (invalid-instruction-error "Invalid register name: ~S" name))

(defun register-name->encoding/2letter (name class first second)
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s)))
    (unless (and (= 3 len)
                 (eql first (elt name/s 0))
                 (eql second (elt name/s 1))
                 (digit-char-p (elt name/s 2)))
      (unexpected-register name class))
    (aprog1
        (- (char-code (elt name/s 2))
           (char-code #\0))
      (unless (<= 0 it 7)
        (unknown-register name)))))

(defun register-name->encoding/3letter (name class first second third)
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s)))
    (unless (and (<= 4 len 5)
                 (eql first (elt name/s 0))
                 (eql second (elt name/s 1))
                 (eql third (elt name/s 2))
                 (digit-char-p (elt name/s 3))
                 (or (< len 5)
                     (digit-char-p (elt name/s 4))))
      (unexpected-register name class))
    (let ((index (- (char-code (elt name/s 3))
                    (char-code #\0))))
      (when (< 4 len)
        (setf index (* index 10))
        (incf index (- (char-code (elt name/s 4))
                       (char-code #\0))))
      (unless (<= 0 index 15)
        (unknown-register name))
      index)))

(defun register-name->encoding/segment (name)
  (declare (type symbol name))
  (register-index name +x86-registers/segment+))

(defun register-name->encoding/r-numbered (name expected-class)
  "Returns (values reg-index reg-mode reg-extra-bit)."
  (declare (type symbol name)
           (type (member :gr8 :gr16 :gr32 :gr64
                         :|GR32orGR64| :|GR16orGR32orGR64|)
                 expected-class))
  (let* ((name/s (symbol-name name))
         (len    (length name/s))
         (first-char (elt name/s 0))
         (last-char  (elt name/s (1- len))))
    (when (or (not (eql first-char #\R))
              (register-index name +x86-registers/64+ nil))
      (unexpected-register name expected-class))
    (unless (digit-char-p (elt name/s 1))
      (unknown-register name))
    (let ((index (- (char-code (elt name/s 1))
                    (char-code #\0)))
          (class 64))
      (case last-char
        (#\D
         (unless (member expected-class
                         '(:gr32 :|GR32orGR64| :|GR16orGR32orGR64|)
                         :test 'eq)
           (unexpected-register name expected-class))
         (setf class 32)
         (decf len))
        (#\W
         (unless (eq expected-class :gr16)
           (unexpected-register name expected-class))
         (setf class 16)
         (decf len))
        (#\B
         (unless (eq expected-class :gr8)
           (unexpected-register name expected-class))
         (setf class 8)
         (decf len)))
      (ecase len
        (2)
        (3 (setf index (+ (* index 10)
                          (- (char-code (elt name/s 2))
                             (char-code #\0))))))
      (unless (<= 8 index 15)
        (unknown-register name))
      (values (logand #b111 index) class 1))))

(defun register-name->encoding/gr8 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s))
         (last-char (elt name/s (1- len))))
    (cond
      ((eql last-char #\L)
       (values (register-index name +x86-registers/8+) 8 nil))
      ((and (eql last-char #\B)
            (eql (elt name/s 0) #\R))
       ;; R8B..R15B
       (register-name->encoding/r-numbered name :gr8))
      (t
       (if otherwise?
           otherwise
           (unexpected-register name :gr8))))))

(defun register-name->encoding/gr16 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s))
         (last-char (elt name/s (1- len))))
    (acond
      ((register-index name +x86-registers/16+ nil)
       (values it 16 nil))
      ((and (eql last-char #\W)
            (eql (elt name/s 0) #\R))
       ;; R8W..R15W
       (register-name->encoding/r-numbered name :gr16))
      (t
       (if otherwise?
           otherwise
           (unexpected-register name :gr16))))))

(defun register-name->encoding/gr32 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s))
         (last-char (elt name/s (1- len))))
    (acond
      ((register-index name +x86-registers/32+ nil)
       (values it 32 nil))
      ((and (eql last-char #\D)
            (eql (elt name/s 0) #\R))
       ;; R8D..R15D
       (register-name->encoding/r-numbered name :gr32))
      (t
       (if otherwise?
           otherwise
           (unexpected-register name :gr32))))))

(defun register-name->encoding/gr64 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (let* ((name/s (symbol-name name))
         (len    (length name/s))
         (last-char (elt name/s (1- len))))
    (acond
      ((register-index name +x86-registers/64+ nil)
       (values it 64 ()))
      ((and (digit-char-p last-char)
            (eql (elt name/s 0) #\R))
       ;; R8..R15
       (register-name->encoding/r-numbered name :gr64))
      (t
       (if otherwise?
           otherwise
           (unexpected-register name :gr64))))))
