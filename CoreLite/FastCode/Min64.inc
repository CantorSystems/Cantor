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
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

function Min{_JOH_IA32_2_b}(const A, B : Int64) : Int64;
asm
  xor   ecx, ecx
  mov   eax, [ebp+20]   {high(A)}
  cmp   eax, [ebp+12]   {high(B)}
  jne   @@CompareHigh
  mov   eax, [ebp+16]   {low(A)}
  cmp   eax, [ebp+8]    {low(B)}
  setna cl
  mov   eax, [ebp+ecx*8+8]
  mov   edx, [ebp+ecx*8+12]
  pop   ebp
  ret   16
@@CompareHigh:
  setng cl
  mov   eax, [ebp+ecx*8+8]
  mov   edx, [ebp+ecx*8+12]
end;
