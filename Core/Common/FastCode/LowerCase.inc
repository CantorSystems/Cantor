(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

var
  AsciiLowerCase : array[Char] of Char;

function LowerCase(const S: string): string;
begin
  if AsciiLowerCase[#$FF] = #0 then
  begin
//    InitAsciiTable(AsciiLowerCase);
    CharLowerBuff(AsciiLowerCase, SizeOf(AsciiLowerCase));
  end;

{$IFDEF MMX}

//function LowerCase_JOH_MMX_2_b(const S: string): string;
const
  B25 : Int64 = $2525252525252525;
  B65 : Int64 = $6565656565656565;
  B20 : Int64 = $2020202020202020;
asm
  xchg    eax, edx
  test    edx, edx              {Test for S = ''}
  jz      system.@LStrSetLength {Return Empty String}
  mov     ecx, edx              {Addr(S)}
  mov     edx, [edx-4]
  push    ebx
  push    edi
  push    esi
  mov     edi, ecx              {Addr(S)}
  mov     esi, edx              {Length}
  mov     ebx, eax              {Addr(Result)}
  call    system.@LStrSetLength {Create Result String}
  mov     ecx, esi              {Length}
  mov     eax, edi              {Addr(S)}
  sub     ecx, 16
  mov     edx, [ebx]            {Result}
  jl      @@Small
  movq    mm4, B25
  movq    mm5, B65
  movq    mm6, B20
  add     eax, ecx
  add     edx, ecx
  neg     ecx
@@LargeLoop:
  movq    mm0, [eax+ecx  ]
  movq    mm1, [eax+ecx+8]
  movq    mm2, mm0
  movq    mm3, mm1
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  paddb   mm0, mm2
  paddb   mm1, mm3
  movq    [edx+ecx  ], mm0
  movq    [edx+ecx+8], mm1
  add     ecx, 16
  jle     @@LargeLoop
  emms
  neg     ecx
  sub     eax, ecx
  sub     edx, ecx
@@Small:
  add     ecx, 16
  lea     edi, AsciiLowerCase
  jz      @@Done
@@SmallLoop:
  sub     ecx, 1
  movzx   esi, [eax+ecx]
  movzx   ebx, [edi+esi]
  mov     [edx+ecx], bl
  jg      @@SmallLoop
@@Done:
  pop     esi
  pop     edi
  pop     ebx
end;

{$ELSE}

//function LowerCase_JOH_IA32_5_d(const S: string): string;
asm {Size = 134 Bytes}
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx-4], ecx
  sub     ebx, 1
  and     ebx, -4
  jmp     @@CheckDone
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Loop:                          {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx], ecx
@@CheckDone:
  sub     ebx, 4
  jnc     @@Loop
  pop     esi
  pop     edi
  pop     ebx
end;

{$ENDIF}

end;
