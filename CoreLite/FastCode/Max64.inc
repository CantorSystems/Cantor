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
 * Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)

function Max{ShaInt64_2_b}(const a, b: int64): int64;
asm
  mov eax,[ebp+$08]
  xor ecx,ecx
  mov edx,[ebp+$0C]
  sub eax,[ebp+$10]
  sbb edx,[ebp+$14]
  setl cl
  mov eax,[ebp+8*ecx+$08]
  mov edx,[ebp+8*ecx+$0C]
end;
