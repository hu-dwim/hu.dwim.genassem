;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(defsuite* (simple :in test))

(deftest form/raw ()
  (compare-with-external-assembler/x86
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

(deftest form/raw/calls ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_call64pcrel32 #x11223344)
      (_call64pcrel32 #x-11223344)
      "00000000  E844332211        call 0x11223349
00000005  E8BCCCDDEE        call 0xffffffffeeddccc6
")
     )))

(deftest form/immediate ()
  (compare-with-external-assembler/x86
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
  (compare-with-external-assembler/x86
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

(deftest form/mrm/special-registers ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_mov64cr rcx cr3)
      (_mov64cr rdx cr7)
      (_mov64cr rax cr0)
      (_mov64rc cr0 r15)
      (_mov64rc cr1 r11)
      "00000000  480F22D9          mov cr3,rcx
00000004  480F22FA          mov cr7,rdx
00000008  480F22C0          mov cr0,rax
0000000C  490F20C7          mov r15,cr0
00000010  490F20CB          mov r11,cr1
"))))

(deftest form/mrm ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_adc16ri #x1122 ax)
      (_adc64ri32 #x11223344 rax)
      (_adc64ri8 #x11 rbx)
      (_adc64ri8 #x11 r15)
      (_adc64ri8 #x11 rsi)
      (_tpause ecx)
      (_tpause edx)
      (_test8i8 #x11)    ; same as next, but special encoding
      (_test8ri #x11 al)
      (_test8ri #x11 bl)
      "00000000  6681D02211        adc ax,0x1122
00000005  4881D044332211    adc rax,0x11223344
0000000C  4883D311          adc rbx,byte +0x11
00000010  4983D711          adc r15,byte +0x11
00000014  4883D611          adc rsi,byte +0x11
00000018  660FAEF1          tpause ecx
0000001C  660FAEF2          tpause edx
00000020  A811              test al,0x11
00000022  F6C011            test al,0x11
00000025  F6C311            test bl,0x11
")
     )))

(deftest form/mrm/srcreg ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_adc8rr     bl dl)
      (_adcx32rr   ebx eax)
      (_adcx64rr   rcx r10)
      (_add16rr    dx ax)
      (_addpdrr    xmm0 xmm1)
      (_addsubpdrr xmm4 xmm7)
      (_adox64rr   r8 rax)
      (_blendpdrri #x11 xmm1 xmm2)
      "00000000  10DA              adc dl,bl
00000002  660F38F6C3        adcx eax,ebx
00000007  664C0F38F6D1      adcx r10,rcx
0000000D  6601D0            add ax,dx
00000010  660F58C8          addpd xmm1,xmm0
00000014  660FD0FC          addsubpd xmm7,xmm4
00000018  F3490F38F6C0      adox rax,r8
0000001E  660F3A0DD111      blendpd xmm2,xmm1,byte 0x11
"))))

(deftest form/mrm/destreg ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_adc16rr bx ax)
      (_mov64rr rax rbx)
      (_mov64rr rax r15)
      (_mov64rr r8 r14)
      (_extractpsrri #x11 xmm7 eax)
      (_extractpsrri #x11 xmm7 r15)
      (_pextrbrri    #x11 xmm0 ebx)
      (_pextrbrri    #x11 xmm7 edx)
      "00000000  6611D8            adc ax,bx
00000003  4889C3            mov rbx,rax
00000006  4989C7            mov r15,rax
00000009  4D89C6            mov r14,r8
0000000C  660F3A17F811      extractps eax,xmm7,byte 0x11
00000012  66490F3A17FF11    extractps r15d,xmm7,byte 0x11
00000019  660F3A14C311      pextrb ebx,xmm0,byte 0x11
0000001F  660F3A14FA11      pextrb edx,xmm7,byte 0x11
")
     )))

(deftest form/mrm/destreg/bug/1 ()
  (with-expected-failures
    (compare-with-external-assembler/x86
     '(((bits 64)
        ;; this is an architectural headache without a trivial fix:
        ;; DECODE-REGISTER happens at macroexpand-time, but with the
        ;; :|GR32orGR64| operand type it would need to dispatch on the
        ;; execution mode, which is only available at runtime in the
        ;; current setup.  this is also related to being able to
        ;; generate a functional interface where there's no
        ;; macroexpand time.
        (_pextrbrri    #x11 xmm0 rbx)
        (_pextrbrri    #x11 xmm15 ebx)
        ;; TODO test whether (_pextrbrri #x11 xmm0 rbx) signals and
        ;; error in 32bit mode.
        "00000000  zork 66480F3A14C311    pextrb rbx,xmm0,byte 0x11
00000007  zork 660F3A14FB11      pextrb ebx,xmm7,byte 0x11
")
       ))))

;; TODO
;; (deftest form/mrm/vex ()
;;   (compare-with-external-assembler/x86
;;    '(((bits 64)
;;       (_blsi32rr edx ebx)
;;       "00000000  C4E260F3DA        blsi ebx,edx
;; "))))

(deftest form/mrm/segment ()
  (compare-with-external-assembler/x86
   '(((bits 64)
      (_mov16rs fs cx)
      (_mov16rs gs bx)
      (_mov16rs fs cx)
      (_mov16rs es dx)
      (_mov16sr cx fs)
      (_mov16sr bx gs)
      (_mov16sr cx fs)
      (_mov16sr dx es)
      "00000000  668CE1            mov cx,fs
00000003  668CEB            mov bx,gs
00000006  668CE1            mov cx,fs
00000009  668CC2            mov dx,es
0000000C  668EE1            mov fs,cx
0000000F  668EEB            mov gs,bx
00000012  668EE1            mov fs,cx
00000015  668EC2            mov es,dx
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
