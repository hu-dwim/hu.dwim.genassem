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

(defparameter *simple-tests*
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
   (_bswap32r edx)
   (_bswap64r r14)
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
   "00000000  0FCA              bswap edx
00000002  490FCE            bswap r14
00000005  3C12              cmp al,0x12
00000007  663D2211          cmp ax,0x1122
0000000B  3D44332211        cmp eax,0x11223344
00000010  483D44332211      cmp rax,0x11223344
00000016  347F              xor al,0x7f
00000018  66352211          xor ax,0x1122
0000001C  3544332211        xor eax,0x11223344
00000021  483544332211      xor rax,0x11223344
00000027  1484              adc al,0x84
00000029  14FF              adc al,0xff
0000002B  66157766          adc ax,0x6677
0000002F  6615FFFF          adc ax,0xffff
00000033  1577665544        adc eax,0x44556677
00000038  15FFFFFFFF        adc eax,0xffffffff
")))

(deftest simple ()
  (loop :for entry :in *simple-tests*
        :for instrs = (butlast entry)
        :for expected = (car (last entry))
        :for context = (eval `(with-asm () ,@instrs))
        :do (is (equal expected (ndisasm-output context)))
        ;; :do (print (xed-output context))
        ;; :do (print (zydis-output context))
        ))

(defparameter *invalid-instructions*
  '((_bswap32r r14)
    (_bswap64r eax)
    (_adc8i8 #x100)
    (_adc16i16 #x10000)
    (_adc32i32 #x100000000)
    ))

(deftest invalids ()
  (map nil (lambda (instr)
             (signals invalid-instruction-error
               (macroexpand instr)))
       *invalid-instructions*))
