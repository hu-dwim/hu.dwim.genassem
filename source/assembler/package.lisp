;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(uiop:define-package :hu.dwim.genassem/asm-common
  (:use :alexandria
        :anaphora
        :common-lisp
        )
  (:export
   #:define-instruction
   #:emit-byte
   #:emit-bytes
   #:with-asm
   #:current-execution-mode
   #:bits
   #:buffer-of

   #:assembler-error
   #:simple-assembler-error
   #:invalid-instruction-error
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-common
        )
  (:export
   #:decode-register
   #:register-name->encoding/gr8
   #:register-name->encoding/gr16
   #:register-name->encoding/gr32
   #:register-name->encoding/gr64
   #:maybe-emit-operand-size-prefix

   #:al #:cl #:dl #:bl #:spl #:bpl #:sil #:dil
   #:ax #:cx #:dx #:bx #:sp #:bp #:si #:di
   #:eax #:ecx #:edx #:ebx #:esp #:ebp #:esi #:edi
   #:rax #:rcx #:rdx #:rbx #:rsp #:rbp #:rsi #:rdi

   ;; these are only exported so that the output is not filled with pkg:: prefixes
   #:reg-mode/dst
   #:reg-extra-bit/dst
   #:reg-index/dst

   #:reg-mode/src
   #:reg-extra-bit/src
   #:reg-index/src

   #:emit-forms/imm
   ))
