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
   #:invalid-operand-error
   ))

(uiop:define-package :hu.dwim.genassem/x86
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.genassem/asm-common
        )
  (:export
   #:decode-register
   #:register-name->encoding/segment
   #:register-name->encoding/gr8
   #:register-name->encoding/gr16
   #:register-name->encoding/gr32
   #:register-name->encoding/gr64
   #:maybe-emit-operand-size-prefix

   #:cs #:ds #:es #:fs #:gs #:ss
   #:al #:cl #:dl #:bl #:spl #:bpl #:sil #:dil
   #:ax #:cx #:dx #:bx #:sp #:bp #:si #:di
   #:eax #:ecx #:edx #:ebx #:esp #:ebp #:esi #:edi
   #:rax #:rcx #:rdx #:rbx #:rsp #:rbp #:rsi #:rdi
   #:st0 #:st1 #:st2 #:st3 #:st4 #:st5 #:st6 #:st7

   ;; these are only exported so that the output is not filled with pkg:: prefixes
   #:reg-mode/1
   #:reg-extra-bit/1
   #:reg-index/1

   #:reg-mode/2
   #:reg-extra-bit/2
   #:reg-index/2

   #:emit-forms/imm
   ))
