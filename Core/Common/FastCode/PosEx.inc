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
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

//function PosEx_JOH_IA32_6_a(const SubStr, S: string; Offset: Integer = 1): Integer;
asm {239 Bytes}
  push    ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  sub     ecx, 1           {Offset - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or Offset < 1}
  jl      @@InvalidInput
  push    edi
  push    esi
  push    ebp
  push    edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}
  add     edx, ecx         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     ax, [ebp+edi]    {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  pop     edx
  pop     ebp
  pop     esi
  pop     edi
  pop     ebx
  sub     eax, edx         {Subtract Start Position}
  ret
@@NotFound:
  pop     edx              {Dump Start Position}
  pop     ebp
  pop     esi
  pop     edi
@@InvalidInput:
  pop     ebx
  xor     eax, eax         {No Match Found - Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length Bytes}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  lea     eax, [edx-4]     {Check Valid Match Positions}
  cmp     cl, [eax]
  lea     edx, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+2]
  cmp     cl, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+3]
  cmp     cl, [eax+2]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+4]
  jmp     @@Compare
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end; {PosEx}
