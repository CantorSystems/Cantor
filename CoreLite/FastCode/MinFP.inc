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
 * Dennis Kjaer Christensen
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)

function Min{JOH}(const A, B : Single) : Single; overload;
asm
  fld     A
  fld     B
  fcomi   st(0), st(1)
  fcmovnb st(0), st(1)
  ffree   st(1)
end;

//Author:            Dennis Kjaer Christensen
//Date:              6/2 2004
//Optimized for:     Blended
//Instructionset(s): IA32, IA32 Extensions

function Min{DKC_Asm2}(const A, B : Double) : Double; overload;
asm
 fld   A
 fld   B
 fcomi st(0),st(1)
 jnb   @Else
 ffree st(1)
 pop   ebp
 ret   16
@Else :
 fxch
 ffree st(1)
end;

//Author:            Dennis Kjaer Christensen
//Date:              6/2 2004
//Optimized for:     Blended
//Instructionset(s): IA32, IA32 Extensions

function Min{DKC_Asm2}(const A, B : Extended) : Extended; overload;
asm
 fld   A
 fld   B
 fcomi st(0), st(1)
 jnb   @Else
 ffree st(1)
 pop   ebp
 ret   $18
@Else :
 fxch
 ffree st(1)
end;
