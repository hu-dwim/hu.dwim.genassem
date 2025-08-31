;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(in-suite test)

(defun disasm-output/stdin (cmdline asm-context)
  (uiop:run-program
   cmdline
   :external-format :iso-8859-1
   :input (make-string-input-stream
           (babel:octets-to-string
            (buffer-of asm-context)
            :encoding :iso-8859-1))
   :output :string
   :error-output :string))

(defun disasm-output/tmp-file (cmdline asm-context)
  (check-type cmdline list)
  (uiop:with-temporary-file (:stream stream :pathname path
                             :element-type '(unsigned-byte 8))
    (write-sequence (buffer-of asm-context) stream)
    (finish-output stream)
    (uiop:run-program
     (append cmdline (list (namestring path)))
     :output :string
     :error-output :string)))

(defun ndisasm-output (asm-context)
  (disasm-output/stdin '("ndisasm" "-b" "64" "-p" "intel" "-") asm-context))

(defun xed-output (asm-context)
  (disasm-output/tmp-file '("xed" "-64" "-ir") asm-context))

(defun zydis-output (asm-context)
  (disasm-output/tmp-file '("ZydisDisasm""-64") asm-context))

(defun test-using-external-assembler (forms)
  (loop :for entry :in forms
        :for instrs = (butlast entry)
        :for expected = (car (last entry))
        :for session = (eval `(with-asm () ,@instrs))
        :do (is (equal expected (ndisasm-output session)))
            ;; :do (print (xed-output context))
            ;; :do (print (zydis-output context))
        ))

(deftest form/raw ()
  (test-using-external-assembler
   '(((bits 64)
      (_ret64)
      (_hlt)
      (_pause)
      (_cmpsb)
      (_cmpsl)
      (_cmpsq)
      (_cmpsw)
      (_noop)
      "00000000  C3                ret
00000001  F4                hlt
00000002  F390              pause
00000004  A6                cmpsb
00000005  A7                cmpsd
00000006  48A7              cmpsq
00000008  66A7              cmpsw
0000000A  90                nop
")
     ((bits 64)
      (_enter #x1122 #xff)
      (_adc64i32 #x11223344)
      "00000000  C82211FF          enter 0x1122,0xff
00000004  481544332211      adc rax,0x11223344
")
     ((bits 64)
      (_cmpsb)
      (_cmpsl)
      (_cmpsq)
      (_cmpsw)
      "00000000  A6                cmpsb
00000001  A7                cmpsd
00000002  48A7              cmpsq
00000004  66A7              cmpsw
")
     )))

(deftest form/immediate ()
  (test-using-external-assembler
   '(((bits 64)
      (_cmp8i8   #x12)
      (_cmp16i16 #x1122)
      (_cmp32i32 #x11223344)
      (_cmp64i32 #x11223344)
      (_xor8i8   #x7f)
      (_xor16i16 #x1122)
      (_xor32i32 #x11223344)
      (_xor64i32 #x11223344)
      (_adc8i8   #x84)
      (_adc8i8   #xff)
      (_adc16i16 #x6677)
      (_adc16i16 #xffff)
      (_adc32i32 #x44556677)
      (_adc32i32 #xffffffff)
      "00000000  3C12              cmp al,0x12
00000002  663D2211          cmp ax,0x1122
00000006  3D44332211        cmp eax,0x11223344
0000000B  483D44332211      cmp rax,0x11223344
00000011  347F              xor al,0x7f
00000013  66352211          xor ax,0x1122
00000017  3544332211        xor eax,0x11223344
0000001C  483544332211      xor rax,0x11223344
00000022  1484              adc al,0x84
00000024  14FF              adc al,0xff
00000026  66157766          adc ax,0x6677
0000002A  6615FFFF          adc ax,0xffff
0000002E  1577665544        adc eax,0x44556677
00000033  15FFFFFFFF        adc eax,0xffffffff
")
     ((bits 64)
      (_call64pcrel32 #x11223344)
      "00000000  E844332211        call 0x11223349
"))))

(deftest form/add-reg ()
  (test-using-external-assembler
   '(((bits 64)
      (_bswap32r edx)
      (_bswap64r rbx)
      (_bswap64r r14)
      (_bswap64r rsp)
      (_mov16ri #x1122 bx)
      "00000000  0FCA              bswap edx
00000002  480FCB            bswap rbx
00000005  490FCE            bswap r14
00000008  480FCC            bswap rsp
0000000B  66BB2211          mov bx,0x1122
")
     ((bits 64)
      (_mov16ri #x1122 bx)
      "00000000  66BB2211          mov bx,0x1122
"))))

(deftest form/mrm ()
  (test-using-external-assembler
   '(((bits 64)
      (_adc16ri #x1122 ax)
      (_adc64ri32 #x11223344 rax)
      (_adc64ri8 #x42 rbx)
      (_adc64ri8 #x42 r15)
      (_adc64ri8 #x42 rsi)
      (_tpause ecx)
      (_tpause edx)
      (_test8i8 #x42)    ; same as next, but special encoding
      (_test8ri #x42 al)
      (_test8ri #x42 bl)
      "00000000  6681D02211        adc ax,0x1122
00000005  4881D044332211    adc rax,0x11223344
0000000C  4883D342          adc rbx,byte +0x42
00000010  4983D742          adc r15,byte +0x42
00000014  4883D642          adc rsi,byte +0x42
00000018  660FAEF1          tpause ecx
0000001C  660FAEF2          tpause edx
00000020  A842              test al,0x42
00000022  F6C042            test al,0x42
00000025  F6C342            test bl,0x42
"))))

(deftest invalids ()
  (map nil (lambda (instr)
             (signals invalid-instruction-error
               (macroexpand instr)))
       '((_bswap32r r14)
         (_bswap64r eax)
         (_adc8i8 #x112)
         (_adc16i16 #x112345678)
         (_adc32i32 #x112345678)
         (_enter #x11234 #x12)
         (_enter #x1234 #x112)
         )))
