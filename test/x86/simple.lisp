;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2025 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.genassem/x86.test)

(defsuite* (simple :in test))

(deftest form/raw ()
  (compare-with-external-assembler/x86
   '(((ret64)
      (hlt)
      (pause)
      (cmpsb)
      (cmpsl)
      (cmpsq)
      (cmpsw)
      (noop)
      "00000000  C3                ret
00000001  F4                hlt
00000002  F390              pause
00000004  A6                cmpsb
00000005  A7                cmpsd
00000006  48A7              cmpsq
00000008  66A7              cmpsw
0000000A  90                nop
")
     ((enter #x1122 #xff)
      (adc64i32 #x11223344)
      "00000000  C82211FF          enter 0x1122,0xff
00000004  481544332211      adc rax,0x11223344
")
     ((cmpsb)
      (cmpsl)
      (cmpsq)
      (cmpsw)
      "00000000  A6                cmpsb
00000001  A7                cmpsd
00000002  48A7              cmpsq
00000004  66A7              cmpsw
")
     )))

(deftest form/raw/calls ()
  (compare-with-external-assembler/x86
   '(((call64pcrel32 #x11223344)
      (call64pcrel32 #x-11223344)
      "00000000  E844332211        call 0x11223349
00000005  E8BCCCDDEE        call 0xffffffffeeddccc6
")
     )))

(deftest form/immediate ()
  (compare-with-external-assembler/x86
   '(((adc8i8   #x84)
      (adc8i8   #xff)
      (adc16i16 #x6677)
      (adc16i16 #xffff)
      (adc32i32 #x44556677)
      (adc32i32 #xffffffff)
      "00000000  1484              adc al,0x84
00000002  14FF              adc al,0xff
00000004  66157766          adc ax,0x6677
00000008  6615FFFF          adc ax,0xffff
0000000C  1577665544        adc eax,0x44556677
00000011  15FFFFFFFF        adc eax,0xffffffff
")

     ((xor8i8   #x7f)
      (xor16i16 #x1122)
      (xor32i32 #x11223344)
      (xor64i32 #x11223344)
      "00000000  347F              xor al,0x7f
00000002  66352211          xor ax,0x1122
00000006  3544332211        xor eax,0x11223344
0000000B  483544332211      xor rax,0x11223344
")

     ((cmp8i8   #x12)
      (cmp16i16 #x1122)
      (cmp32i32 #x11223344)
      (cmp64i32 #x11223344)
      "00000000  3C12              cmp al,0x12
00000002  663D2211          cmp ax,0x1122
00000006  3D44332211        cmp eax,0x11223344
0000000B  483D44332211      cmp rax,0x11223344
")
     ((call64pcrel32 #x11223344)
      "00000000  E844332211        call 0x11223349
")
     ((adc32ri   #x11223344 ebx)
      (adc64ri32 #x11223344 rbx)
      "00000000  81D344332211      adc ebx,0x11223344
00000006  4881D344332211    adc rbx,0x11223344
"))))

(deftest form/add-reg ()
  (compare-with-external-assembler/x86
   '(((bswap32r edx)
      (bswap64r rbx)
      (bswap64r r14)
      (bswap64r rsp)
      (mov16ri #x1122 bx)
      "00000000  0FCA              bswap edx
00000002  480FCB            bswap rbx
00000005  490FCE            bswap r14
00000008  480FCC            bswap rsp
0000000B  66BB2211          mov bx,0x1122
")
     ((mov16ri #x1122 bx)
      "00000000  66BB2211          mov bx,0x1122
"))))

(deftest form/mrm/special-registers ()
  (compare-with-external-assembler/x86
   '(((mov64cr rcx cr3)
      (mov64cr rdx cr7)
      (mov64cr rax cr0)
      (mov64rc cr0 r15)
      (mov64rc cr1 r11)
      "00000000  0F22D9            mov cr3,rcx
00000003  0F22FA            mov cr7,rdx
00000006  0F22C0            mov cr0,rax
00000009  410F20C7          mov r15,cr0
0000000D  410F20CB          mov r11,cr1
"))))

(deftest form/mrm ()
  (compare-with-external-assembler/x86
   '(((adc16ri #x1122 ax)
      (adc64ri32 #x11223344 rax)
      (adc64ri8 #x11 rbx)
      (adc64ri8 #x11 r15)
      (adc64ri8 #x11 rsi)
      "00000000  6681D02211        adc ax,0x1122
00000005  4881D044332211    adc rax,0x11223344
0000000C  4883D311          adc rbx,byte +0x11
00000010  4983D711          adc r15,byte +0x11
00000014  4883D611          adc rsi,byte +0x11
")
     ((tpause ecx)
      (tpause edx)
      "00000000  660FAEF1          tpause ecx
00000004  660FAEF2          tpause edx
")
     ((test8i8 #x11)    ; same as next, but special encoding
      (test8ri #x11 al)
      (test8ri #x11 bl)
      "00000000  A811              test al,0x11
00000002  F6C011            test al,0x11
00000005  F6C311            test bl,0x11
")
     )))

(deftest form/mrm/srcreg ()
  (compare-with-external-assembler/x86
   '(((adc8rr     bl dl)
      (adcx32rr   ebx eax)
      (adcx64rr   rcx r10)
      (add16rr    dx ax)
      (addpdrr    xmm0 xmm1)
      (addsubpdrr xmm4 xmm7)
      (adox64rr   r8 rax)
      (blendpdrri #x11 xmm1 xmm2)
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
   '(((adc16rr bx ax)
      (mov64rr rax rbx)
      (mov64rr rax r15)
      (mov64rr r8 r14)
      (extractpsrri #x11 xmm7 eax)
      (extractpsrri #x11 xmm7 r15)
      (pextrbrri    #x11 xmm0 ebx)
      (pextrbrri    #x11 xmm7 edx)
      "00000000  6611D8            adc ax,bx
00000003  4889C3            mov rbx,rax
00000006  4989C7            mov r15,rax
00000009  4D89C6            mov r14,r8
0000000C  660F3A17F811      extractps eax,xmm7,byte 0x11
00000012  66410F3A17FF11    extractps r15d,xmm7,byte 0x11
00000019  660F3A14C311      pextrb ebx,xmm0,byte 0x11
0000001F  660F3A14FA11      pextrb edx,xmm7,byte 0x11
")
     )))

(deftest form/mrm/destreg/bug/1 ()
  (compare-with-external-assembler/x86
   '((;; the PEXTRBrri instruction operates on the low 8 bits of the
      ;; register, but the register encoded just like a GR32 or GR64
      ;; into the machine code. in practice this means that ndisasm,
      ;; xed, and zydis (and probably other disassemblers) disassemble
      ;; it as ebx.
      (bits 64)
      (pextrbrri    #x11 xmm0 rbx)
      (bits 32)
      (pextrbrri    #x11 xmm0 ebx)
      (pextrbrri    #x11 xmm6 ebx)
      (bits 64)
      (pextrbrri    #x11 xmm6 rbx)
      (pextrbrri    #x11 xmm14 rbx)
      "00000000  660F3A14C311      pextrb ebx,xmm0,byte 0x11
00000006  660F3A14C311      pextrb ebx,xmm0,byte 0x11
0000000C  660F3A14F311      pextrb ebx,xmm6,byte 0x11
00000012  660F3A14F311      pextrb ebx,xmm6,byte 0x11
00000018  66440F3A14F311    pextrb ebx,xmm14,byte 0x11
")
     )))

;; TODO
;; (deftest form/mrm/vex ()
;;   (compare-with-external-assembler/x86
;;    '((
;;       (blsi32rr edx ebx)
;;       "00000000  C4E260F3DA        blsi ebx,edx
;; "))))

(deftest form/mrm/segment ()
  (compare-with-external-assembler/x86
   '(((mov16rs fs cx)
      (mov16rs gs bx)
      (mov16rs fs cx)
      (mov16rs es dx)
      (mov16sr cx fs)
      (mov16sr bx gs)
      (mov16sr cx fs)
      (mov16sr dx es)
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
  ;; TODO these should check for more specific error types
  (map nil (lambda (instr)
             (signals serious-condition
               (eval instr)))
       '((bswap32r r14)
         (bswap64r eax)
         (adc8i8 #x112)
         (adc16i16 #x112345678)
         (adc32i32 #x112345678)
         (enter #x11234 #x12)
         (enter #x1234 #x112)
         ))
  (with-expected-failures
    (signals serious-condition
      (emit-assembly/x86
        (bits 64)
        (pextrbrri    #x11 xmm0 ebx)))
    (signals serious-condition
      (emit-assembly/x86
        (bits 32)
        (pextrbrri    #x11 xmm0 rbx)))))
