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

(defmacro decode-register (reg expected-class)
  `(progn
     ,(ecase expected-class
        (:|GR32orGR64|
         ;; TODO ?
         #+nil
         `(typecase ,reg
            (gr32 (when (<= 32 (current-execution-mode))
                    (unexpected-register ,reg 'gr32)))
            (gr64 (when (< (current-execution-mode) 64)
                    (unexpected-register ,reg 'gr64)))
            (t (unexpected-register ,reg ',expected-class))))
        (:|GR16orGR32orGR64|
         ;; TODO ?
         )
        ((gr8 gr16 gr32 gr64
          vr64 vr128 vr512 vr256
          st cr dr sr)
         `(check-type ,reg ,expected-class)))
     (let ((index
             ,(ecase expected-class
                (:|GR32orGR64|
                 ;; GR32orGR64 means: this slot could be a 32-bit or a
                 ;; 64-bit general register, depending on prefixes. In
                 ;; 32-bit mode, it’s fixed to 32-bit. In 64-bit mode,
                 ;; the assembler decides: without REX.W, use 32-bit;
                 ;; with REX.W, use 64-bit.  But then sometimes REX.W
                 ;; is not valid (i.e. hasREX_W=0) then it means that
                 ;; the current execution mode specifies which
                 ;; register class is valid (e.g. PEXTRBrri)
                 `(typecase ,reg
                    ((or gr32 gr64) (index-of ,reg))
                    (t (unexpected-register ,reg ',expected-class))))
                (:|GR16orGR32orGR64|
                 `(typecase ,reg
                    ((or gr16 gr32 gr64) (index-of ,reg))
                    (t (unexpected-register ,reg ',expected-class))))
                ((gr8 gr16 gr32 gr64
                  vr64 vr128 vr512 vr256
                  st cr dr sr)
                 `(index-of ,reg)))))
       (cond
         ((<= 0 index 7)
          (values index nil))
         ,@(when (or (member expected-class '(:|GR32orGR64| :|GR16orGR32orGR64|))
                     (< 7 (max-index-of (closer-mop:class-prototype (find-class expected-class)))))
             `(((<= 8 index 15)
                (unless (<= 64 (current-execution-mode))
                  (invalid-instruction-error "Register 7-15 (~A) can only be encoded in 64 bit mode" ,reg))
                (values (- index 8) 1))))
         (t (error "BUG: register index is ~A?!" index))))))

(defun unexpected-register (reg expected-class)
  (invalid-instruction-error
   "Trying to use register ~A while expecting a ~S"
   name expected-class))

;; TODO delme?
(defun unknown-register (reg)
  (invalid-instruction-error "Invalid register: ~A" reg))
