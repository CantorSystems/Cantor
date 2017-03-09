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

function Round32{ExtendedDKC1}(const X : Extended) : Integer;
asm
 sub   esp,8
 fld   X
 fistp dword ptr [esp]
 wait
 mov   eax, [esp]
 add   esp,8
end;

{$IFNDEF Lite}
function Round32{DoubleDKC1}(const X : Double) : Integer;
asm
 sub   esp,8
 fld   X
 fistp dword ptr [esp]
 wait
 mov   eax, [esp]
 add   esp,8
end;
{$ENDIF}

function Round32{SingleJOH_1}(const X : Single) : Integer;
asm
 sub   esp, 4
 fld   X
 fistp dword ptr [esp]
 wait
 pop   eax
end;
