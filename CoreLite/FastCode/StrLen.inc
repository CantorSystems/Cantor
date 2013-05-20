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

//function StrLen_JOH_IA32_7_d(const Str: PChar): Cardinal;
asm
  TEST EAX, EAX
  JZ @@0        // by Freeman
  cmp   byte ptr [eax], 0
  je    @@0
  cmp   byte ptr [eax+1], 0
  je    @@1
  cmp   byte ptr [eax+2], 0
  je    @@2
  cmp   byte ptr [eax+3], 0
  je    @@3
  push  eax
  and   eax, -4              {DWORD Align Reads}
@@Loop:
  add   eax, 4
  mov   edx, [eax]           {4 Chars per Loop}
  lea   ecx, [edx-$01010101]
  not   edx
  and   edx, ecx
  and   edx, $80808080       {Set Byte to $80 at each #0 Position}
  jz    @@Loop               {Loop until any #0 Found}
@@SetResult:
  pop   ecx
  bsf   edx, edx             {Find First #0 Position}
  shr   edx, 3               {Byte Offset of First #0}
  add   eax, edx             {Address of First #0}
  sub   eax, ecx
  ret
@@0:
  xor   eax, eax
  ret
@@1:
  mov   eax, 1
  ret
@@2:
  mov   eax, 2
  ret
@@3:
  mov   eax, 3
end;
