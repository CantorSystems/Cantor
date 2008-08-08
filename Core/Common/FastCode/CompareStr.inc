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

//function CompareStr_PLR_IA32_14b(const S1, S2: string): Integer;
asm
  {On entry:
    eax = S1,
    edx = S2}
  cmp eax, edx
  je @SameString
  {Is either of the strings perhaps nil?}
  test eax, edx
  jz @PossibleNilString
  {Compare the first four characters (there has to be a trailing #0). In random
   string compares this can save a lot of CPU time.}
@BothNonNil:
  {Compare the first character}
  mov ecx, [edx]
  cmp cl, [eax]
  je @FirstCharacterSame
  {First character differs}
  movzx eax, byte ptr [eax]
  movzx ecx, cl
  sub eax, ecx
  ret
@FirstCharacterSame:
  {save ebx}
  push ebx
  {Get first four characters}
  mov ebx, [eax]
  cmp ebx, ecx
  je @FirstFourSame
  {Get the string lengths in eax and edx}
  mov eax, [eax - 4]
  mov edx, [edx - 4]
  {Is the second character the same?}
  cmp ch, bh
  je @FirstTwoCharactersMatch
  {Second character differs: Are any of the strings non-nil but zero length?}
  test eax, eax
  jz @ReturnLengthDifference
  test edx, edx
  jz @ReturnLengthDifference
  movzx eax, bh
  movzx edx, ch
@ReturnLengthDifference:
  sub eax, edx
  pop ebx
  ret
@FirstTwoCharactersMatch:
  cmp eax, 2
  jna @ReturnLengthDifference
  cmp edx, 2
  jna @ReturnLengthDifference
  {Swap the bytes into the correct order}
  mov eax, ebx
  bswap eax
  bswap ecx
  sub eax, ecx
  pop ebx
  ret
  {It is the same string}
@SameString:
  xor eax, eax
  ret
  {Good possibility that at least one of the strings are nil}
@PossibleNilString:
  test eax, eax
  jz @FirstStringNil
  test edx, edx
  jnz @BothNonNil
  {Return first string length: second string is nil}
  mov eax, [eax - 4]
  ret
@FirstStringNil:
  {Return 0 - length(S2): first string is nil}
  sub eax, [edx - 4]
  ret
  {The first four characters are identical}
@FirstFourSame:
  {set ebx = length(S1)}
  mov ebx, [eax - 4]
  xor ecx, ecx
  {set ebx = length(S1) - length(S2)}
  sub ebx, [edx - 4]
  {Save the length difference on the stack}
  push ebx
  {set esi = 0 if length(S1) < length(S2), $ffffffff otherwise}
  adc ecx, -1
  {set esi = - min(length(s1), length(s2))}
  and ecx, ebx
  sub ecx, [eax - 4]
  {Adjust the pointers to be negative based}
  sub eax, ecx
  sub edx, ecx
@CompareLoop:
  add ecx, 4
  jns @MatchUpToLength
  mov ebx, [eax + ecx]
  xor ebx, [edx + ecx]
  jz @CompareLoop
@Mismatch:
  bsf ebx, ebx
  shr ebx, 3
  add ecx, ebx
  jns @MatchUpToLength
  movzx eax, byte ptr [eax + ecx]
  movzx edx, byte ptr [edx + ecx]
  sub eax, edx
  pop ebx
  pop ebx
  ret
  {All characters match - return the difference in length}
@MatchUpToLength:
  pop eax
  pop ebx
end;
