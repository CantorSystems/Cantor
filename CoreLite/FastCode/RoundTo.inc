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
 *
 * ***** END LICENSE BLOCK ***** *)

//Author:            Dennis Kjaer Christensen
//Date:              29/12 2004
//Instructionset(s): IA32

function RoundTo{EX_DKC_IA32_1}(const Value: Extended; Digit: TRoundToRange): Extended;
var
 LFactor, LFactorReciprocal : Extended;
 CW8087 : Word;
const
 CW8087X : Word = 4978;
 LFactorArray : array[TRoundToRange] of Extended =
  (1E-20, 1E-19, 1E-18, 1E-17, 1E-16, 1E-15, 1E-14, 1E-13, 1E-12, 1E-11, 1E-10,
   1E-9, 1E-8, 1E-7, 1E-6, 1E-5, 1E-4, 1E-3, 1E-2, 1E-1, 1, 1E1, 1E2, 1E3, 1E4,
   1E5, 1E6, 1E7, 1E8, 1E9, 1E10, 1E11, 1E12, 1E13, 1E14, 1E15, 1E16, 1E17, 1E18,
   1E19, 1E20);
 LFactorReciprocalArray : array[TRoundToRange] of Extended =
  (1E20, 1E19, 1E18, 1E17, 1E16, 1E15, 1E14, 1E13, 1E12, 1E11, 1E10, 1E9, 1E8,
   1E7, 1E6, 1E5, 1E4, 1E3, 1E2, 1E1, 1, 1E-1, 1E-2, 1E-3, 1E-4, 1E-5, 1E-6,
   1E-7, 1E-8, 1E-9, 1E-10, 1E-11, 1E-12, 1E-13, 1E-14, 1E-15, 1E-16, 1E-17,
   1E-18, 1E-19, 1E-20);

asm
   sub     esp,$40
{   cmp     al,-20      // Freeman: not needed, let FPU raise own exception
   jl      @Exception
   cmp     al,20
   jle     @Continue
 @Exception :
   call    RaiseException
 @Continue :}
   //CW8087 := Get8087CW;
   fstcw   word ptr [esp+6]
   //Set8087CW(4978);//pmExtended, rmNearest etc.
   fldcw   word ptr [CW8087X]
   //if ADigit = 0 then
   test    al,al
   jnz     @Else
   //Result := Round(AValue)
   fld     tbyte ptr [ebp+$08]
   frndint
   jmp     @IfEnd
 @Else :
   //LFactor := LFactorArray[ADigit];
   movsx   eax,al
   lea     eax,[eax+eax*4]
   fld     tbyte ptr [eax*2+LFactorArray + $C8]
   //LFactorReciprocal := LFactorReciprocalArray[ADigit];
   fld     tbyte ptr [eax*2+LFactorReciprocalArray + $C8]
   //Result := Round(AValue * LFactorReciprocal) * LFactor;
   fld     tbyte ptr [ebp+$08]//AValue
   fmulp
   frndint
   fmulp
 @IfEnd :
   fldcw   word ptr [esp+6]
   wait
   mov     esp,ebp
end;
