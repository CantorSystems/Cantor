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

//function CompareMem_PLR_IA32_2c(const P1, P2: pointer; const Length: integer): boolean;
asm
  {Less than a qword to compare?}
  sub ecx, 8
  jl @VerySmallCompare
  {save ebx}
  push ebx
  {Compare first dword}
  mov ebx, [eax]
  cmp ebx, [edx]
  je @FirstFourMatches
@InitialMismatch:
  xor eax, eax
  pop ebx
  ret
@FirstFourMatches:
  {Point eax and edx to the last 8 bytes}
  add eax, ecx
  add edx, ecx
  {Compare the second last dword}
  mov ebx, [eax]
  cmp ebx, [edx]
  jne @InitialMismatch
  {Compare the last dword}
  mov ebx, [eax + 4]
  cmp ebx, [edx + 4]
  jne @InitialMismatch
  {12 or less bytes to compare?}
  sub ecx, 4
  jle @InitialMatch
  {Save esi}
  push esi
  {DWord align reads from P1 (P2 reads are not aligned)}
  neg ecx
  add ecx, eax
  and ecx, -4
  sub ecx, eax
  {Compare chunks of 8 bytes at a time}
@CompareLoop:
  mov ebx, [eax + ecx]
  mov esi, [eax + ecx + 4]
  xor ebx, [edx + ecx]
  xor esi, [edx + ecx + 4]
  or ebx, esi
  jnz @LargeMismatch
  add ecx, 8
  jl @CompareLoop
  pop esi
@InitialMatch:
  pop ebx
@MatchSmall:
  mov al, True
  ret
@VerySmallCompare:
  add ecx, 8
  jle @MatchSmall
@SmallLoop:
  mov ch, [eax]
  xor ch, [edx]
  jnz @MismatchSmall
  add eax, 1
  add edx, 1
  sub cl, 1
  jnz @SmallLoop
  jmp @MatchSmall
@LargeMismatch:
  pop esi
  pop ebx
@MismatchSmall:
  xor eax, eax
end;
