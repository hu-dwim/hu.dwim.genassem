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
;;; Generic registers
;;;
(define-register-instances gr8  (al  cl  dl  bl  spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b))
(define-register-instances gr16 (ax  cx  dx  bx  sp  bp  si  di  r8w r9w r10w r11w r12w r13w r14w r15w))
(define-register-instances gr32 (eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d))
(define-register-instances gr64 (rax rcx rdx rbx rsp rbp rsi rdi r8  r9  r10  r11  r12  r13  r14  r15 ))

;;;
;;; Float, x87, ST(0)-ST(7) registers
;;;
(define-register-instances/numbered st #:st 8)

;;;
;;; Vector registers
;;;
(define-register-instances/numbered vr64  #:mm  8)
(define-register-instances/numbered vr128 #:xmm 16)
(define-register-instances/numbered vr256 #:ymm 16)
(define-register-instances/numbered vr512 #:zmm 16)

;;;
;;; Segment registers
;;;
(define-register-instances sr (es cs ss ds fs gs))

;;;
;;; Control registers
;;;
(define-register-instances/numbered cr #:cr 8)

;;;
;;; Debug registers
;;;
(define-register-instances/numbered dr #:dr 8)

(defmacro decode-register (name expected-class)
  (ecase expected-class
    (:gr64 `(register-name->encoding/gr64 ,name))
    (:gr32 `(register-name->encoding/gr32 ,name))
    (:|GR32orGR64|
     ;; GR32orGR64 means: this slot could be a 32-bit or a 64-bit
     ;; general register, depending on prefixes. In 32-bit mode, it’s
     ;; fixed to 32-bit. In 64-bit mode, the assembler decides:
     ;; without REX.W, use 32-bit; with REX.W, use 64-bit.
     `(multiple-value-bind (index mode extra-bit)
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
    (:gr16        `(register-name->encoding/gr16 ,name))
    (:gr8         `(register-name->encoding/gr8  ,name))
    ;; FIXME the next 3 also needs to extract the extra-bit
    (:vr512       `(vr512-index (vr512 ,name))) ; ZMM0-ZMM15
    (:vr256       `(vr256-index (vr256 ,name))) ; YMM0-YMM15
    ((:vr128 :fr32 :fr64) `(vr128-index (vr128 ,name))) ; XMM0-XMM15
    (:vr64        `(vr64-index (vr64 ,name))) ; MM0-MM7
    (:control_reg `(cr-index (cr ,name))) ; CR0-CR7
    (:debug_reg   `(dr-index (dr ,name))) ; DR0-DR7
    (:|RSTi|      `(st-index (st ,name))) ; ST0-ST7
    (:segment_reg `(sr-index (sr ,name)))))

(defun unexpected-register (name expected-class)
  (invalid-instruction-error
   "Trying to use register ~A while expecting a ~S"
   (fully-qualified-symbol-name name) expected-class))

(defun unknown-register (name)
  (invalid-instruction-error "Invalid register name: ~A"
                             (fully-qualified-symbol-name name)))

(defun register-name->encoding/gr8 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (acond
    ((gr8 name nil)
     (let ((index (gr8-index it))
           (extra-bit nil))
       (when (< 7 index)
         (setf extra-bit 1)
         (decf index 8))
       (values index 8 extra-bit)))
    (otherwise? otherwise)
    (t (unexpected-register name :gr8))))

(defun register-name->encoding/gr16 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (acond
    ((gr16 name nil)
     (let ((index (gr16-index it))
           (extra-bit nil))
       (when (< 7 index)
         (setf extra-bit 1)
         (decf index 8))
       (values index 16 extra-bit)))
    (otherwise? otherwise)
    (t (unexpected-register name :gr16))))

(defun register-name->encoding/gr32 (name &optional (otherwise nil otherwise?))
  (declare (type symbol name))
  (acond
    ((gr32 name nil)
     (let ((index (gr32-index it))
           (extra-bit nil))
       (when (< 7 index)
         (setf extra-bit 1)
         (decf index 8))
       (values index 32 extra-bit)))
    (otherwise? otherwise)
    (t (unexpected-register name :gr32))))

(defun register-name->encoding/gr64 (name)
  (declare (type symbol name))
  (let* ((reg (gr64 name))
         (index (gr64-index reg))
         (extra-bit nil))
    (when (< 7 index)
      (setf extra-bit 1)
      (decf index 8))
    (values index 64 extra-bit)))
