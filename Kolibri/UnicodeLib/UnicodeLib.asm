;
;   Kolibri OS Unicode Library (UnicodeLib)
;
;   Copyright (c) 2012 The Unified Environment Laboratory
;
;   Distributed under BSD license:
;     http://websvn.tunilab.org/wsvn/TuniLab.UniEnv/license.eng.txt
;     http://websvn.tunilab.org/wsvn/TuniLab.UniEnv/license.rus.txt
;
;   Conditional defines:
;     * UTF32 -- UTF-32 character set support
;

format MS COFF

public Exports

section '.flat' code readable align 16

;include '../../../macros.inc'
include '../../../proc32.inc'

mem.alloc     dd ?
mem.free      dd ?
mem.realloc   dd ?
;dll.load      dd ?

lib_init:
        MOV [mem.alloc], EAX
        MOV [mem.free], EBX
        MOV [mem.realloc], ECX
        RET

@GetMem:
        PUSH EAX
        JMP mem.alloc

@FreeMem:
        PUSH EAX
        JMP mem.free

@ReallocMem:
        PUSH EAX
        MOV EAX, [EAX]
        PUSH EDX
        PUSH EAX
        CALL mem.realloc
        POP EDX
        MOV [EDX], EAX
        RET

@FillChar:
        PUSH EDI
        MOV EDI, EAX
        MOV CH, CL
        MOV EAX, ECX
        SHL EAX, 16
        MOV AX, CX
        MOV ECX, EDX
        SAR ECX, 2
        JS @@exit
        REP STOSD

        MOV ECX,EDX
        AND ECX, 3
        REP STOSB
@@exit:
        POP EDI
        RET

tmp:
   PUSH EBP
   MOV EBP,ESP
   ADD ESP,-60
   PUSH EBX
   PUSH ESI
   PUSH EDI
   MOV DWORD [EBP-16],ECX
   MOV DWORD [EBP-12],EDX
   MOV DWORD [EBP-8],EAX
   MOV EBX,DWORD [EBP+8]
   MOV ESI,DWORD [EBP+20]
; -- Line #1806 --
   TEST ESI,ESI
   JE +188; (0xDC)
; -- Line #1808 --
   TEST BYTE [EBP+16],$01
   JE +64; (0x66)
; -- Line #1810 --
   MOV EAX,DWORD [EBP-12]
   PUSH ESI
   MOV ESI,EAX
   LEA EDI,DWORD [EBP-60]
   MOV ECX,$0000000B
   REPE MOVSD
   POP ESI
; -- Line #1811 --
   MOV AL,0
   PUSH EAX
   MOV EAX,DWORD [EBP+12]
   PUSH EAX
   PUSH EBX
   LEA EAX,DWORD [EBP-60]
   MOV ECX,ESI
   MOV EDX,DWORD [EBP-16]
   ;CALL UTF8ToUTF32
   TEST AL,AL
   JE +19; (0x66)
; -- Line #1813 --
   MOV EAX,DWORD [EBP-12]
   MOV EDI,EAX
   LEA ESI,DWORD [EBP-60]
   MOV ECX,$0000000B
   REPE MOVSD
; -- Line #1814 --
   MOV AL,$01
; -- Line #1815 --
   JMP +120; (0xDE)
; -- Line #1822 --
   DEC ESI
   TEST ESI,ESI
   JB +113; (0xDC)
   INC ESI
   XOR EDI,EDI
; -- Line #1824 --
   MOV EAX,DWORD [EBP-16]
   MOV AL,BYTE [EAX+EDI]
; -- Line #1825 --
   TEST AL,AL
   JE +58; (0xB2)
; -- Line #1827 --
   AND EAX,$000000FF
   MOV EDX,DWORD [EBP-8 ]
   MOVZX EAX,WORD [EDX+2*EAX-256]
   MOV DWORD [EBP-4],EAX
; -- Line #1828 --
   CMP DWORD [EBP-4],0
   JNE +38; (0xB7)
; -- Line #1829 --
   TEST BL ,$01
   JE +15; (0xA5)
; -- Line #1831 --
   MOV DWORD [EBP-4],$0000FFFD
; -- Line #1832 --
   MOV EAX,DWORD [EBP-12]
   INC DWORD [EAX+32]
   JMP +18; (0xB7)
; -- Line #1836 --
   MOV EAX,DWORD [EBP-12]
   MOV EDX,DWORD [EBP-4]
   MOV DWORD [EAX+32],EDX
; -- Line #1837 --
   XOR EAX,EAX
; -- Line #1838 --
   JMP +44; (0xDE)
; -- Line #1842 --
   XOR EAX,EAX
   MOV DWORD [EBP-4],EAX
; -- Line #1852 --
   TEST BL ,$02
   JE +8; (0xC4)
; -- Line #1854 --
   MOV EAX,DWORD [EBP-4]
; -- Line #1855 --
   BSWAP EAX
; -- Line #1856 --
   MOV DWORD [EBP-4],EAX
; -- Line #1859 --
   MOV EAX,DWORD [EBP+12 ]
   MOV EDX,DWORD [EBP-4]
   MOV DWORD [EAX+4*EDI],EDX
; -- Line #1862 --
   MOV EAX,DWORD [EBP-12]
   INC DWORD [EAX]
; -- Line #1863 --
   MOV EAX,DWORD [EBP-12]
   INC DWORD [EAX+40]
; -- Line #1865 --
   INC EDI
; -- Line #1822 --
   DEC ESI
   JNE -110; (0x6E)
; -- Line #1868 --
   MOV AL,$01
; -- Line #1869 --
   POP EDI
   POP ESI
   POP EBX
   MOV ESP,EBP
   POP EBP
   RET 16
   ADD BYTE [EAX],AL
   ADD BYTE [EAX],AL


;include 'FillChar.inc'

include 'CodePages.inc'
include 'CodePageNames.inc'
;include 'CoreStrings.asm'

sLibInit db 'lib_init', 0

Exports:
  dd sLibInit, lib_init
