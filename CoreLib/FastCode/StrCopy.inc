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

//function StrCopy_JOH_IA32_5_c(Dest: PChar; const Source: PChar): PChar;
asm {Size = 126 Bytes}
  movzx ecx, [edx]
  mov   [eax], cl
  test  cl, cl
  jz    @@Exit
  movzx ecx, [edx+1]
  mov   [eax+1], cl
  test  cl, cl
  jz    @@Exit
  movzx ecx, [edx+2]
  mov   [eax+2], cl
  test  cl, cl
  jz    @@Exit
  movzx ecx, [edx+3]
  mov   [eax+3], cl
  test  cl, cl
  jz    @@Exit
  push  eax
  mov   ecx, edx             {DWORD Align Reads}
  and   edx, -4
  sub   ecx, edx
  sub   eax, ecx
  push  ebx
@@Loop:
  add   edx, 4               {4 Chars per Loop}
  add   eax, 4
  mov   ecx, [edx]
  lea   ebx, [ecx-$01010101]
  not   ecx
  and   ecx, ebx
  and   ecx, $80808080
  jnz   @@Remainder
  mov   ecx, [edx]
  mov   [eax], ecx
  jmp   @@Loop               {Loop until any #0 Found}
@@Remainder:
  pop   ebx
  movzx ecx, [edx]
  mov   [eax], cl
  test  cl, cl
  jz    @@Done
  movzx ecx, [edx+1]
  mov   [eax+1], cl
  test  cl, cl
  jz    @@Done
  movzx ecx, [edx+2]
  mov   [eax+2], cl
  test  cl, cl
  jz    @@Done
  movzx ecx, [edx+3]
  mov   [eax+3], cl
@@Done:
  pop   eax
@@Exit:
end;
