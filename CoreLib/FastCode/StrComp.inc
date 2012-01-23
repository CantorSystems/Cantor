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

//function StrComp_Sha_IA32_7_a(const Str1, Str2: PChar): Integer;
asm
   sub   eax, edx
   jz    @ret
@loop:
   movzx ecx, [eax+edx]
   cmp   cl, [edx]
   jne   @stop
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+1]
   cmp   cl, [edx+1]
   jne   @stop1
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+2]
   cmp   cl, [edx+2]
   jne   @stop2
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+3]
   cmp   cl, [edx+3]
   jne   @stop3
   add   edx, 4
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx]
   cmp   cl, [edx]
   jne   @stop
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+1]
   cmp   cl, [edx+1]
   jne   @stop1
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+2]
   cmp   cl, [edx+2]
   jne   @stop2
   test  cl, cl
   jz    @eq
   movzx ecx, [eax+edx+3]
   cmp   cl, [edx+3]
   jne   @stop3
   add   edx, 4
   test  cl, cl
   jnz   @loop
@eq:
   xor   eax, eax
@ret:
   ret
@stop3:
   add   edx, 1
@stop2:
   add   edx, 1
@stop1:
   add   edx, 1
@stop:
   mov   eax, ecx
   movzx edx, [edx]
   sub   eax, edx
end;
