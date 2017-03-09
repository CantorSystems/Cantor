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
 * Lars Blok Gravengaard
 *
 * ***** END LICENSE BLOCK ***** *)

function Max{_LBG_IA32_1_a}(const A, B : Integer) : Integer;
asm
                    // A in EAX
                    // B in EDX
   CMP EDX,EAX      // Is edx > eax  ?
   CMOVG EAX,EDX    // Conditional move if greater
                    // Does not affect the destination operand if the condition is false.
end;
