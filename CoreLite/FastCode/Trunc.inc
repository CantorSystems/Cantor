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
 * Pierre le Riche
 *
 * ***** END LICENSE BLOCK ***** *)

function Trunc{Extended_PLR_IA32_1}(const X: Extended): Int64;
asm
  {On entry: X = [esp + 8]}
  {Extended variable layout: S1E15M64}
  {Get the mantissa into edx: eax}
  mov eax, [esp + 8]
  mov edx, [esp + 12]
  {Get the exponent and sign in ecx}
  movsx ecx, word ptr [esp + 16]
  {Save the sign in ebp}
  mov ebp, ecx
  {Mask out the sign bit of ecx}
  and ecx, $7fff
  {Get the number of positions to shift}
  neg ecx
  add ecx, 16383 + 63
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
@RaiseInvalidOperationException:
  {Force numeric overflow}
  mov al, reInvalidOp
  jmp System.Error
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns @RaiseInvalidOperationException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js @RaiseInvalidOperationException
  xor eax, eax
@Done:
end;

function Trunc{Double_PLR_IA32_1}(const X: Double): Int64;
asm
  {On entry: X = [esp + 8]}
  {Double variable layout: S1E11M52}
  {Get the mantissa into edx:eax}
  mov eax, [esp + 8]
  mov ecx, [esp + 12]
  mov edx, ecx
  {Save the sign in ebp}
  mov ebp, ecx
  {Get the exponent in ecx}
  and ecx, $7fffffff
  shr ecx, 20
  {Get the number of positions to shift the mantissa}
  neg ecx
  add ecx, 1023 + 63
  {Shift the mantissa all the way to the left}
  shld edx, eax, 11
  shl eax, 11
  {Add the implied 1 bit to the mantissa}
  or edx, $80000000
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
@RaiseInvalidOperationException:
  {Force numeric overflow}
  mov al, reInvalidOp
  jmp System.Error
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns @RaiseInvalidOperationException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js @RaiseInvalidOperationException
  xor eax, eax
@Done:
end;

function Trunc{Single_PLR_IA32_1}(const X: Single): Int64;
asm
  {On entry: X = [esp + 8]}
  {Double variable layout: S1E8M23}
  {Get the mantissa into edx}
  mov ecx, [esp + 8]
  mov edx, ecx
  {Save the sign in ebp}
  mov ebp, ecx
  {Get the exponent in ecx}
  and ecx, $7fffffff
  shr ecx, 23
  {Get the number of positions to shift the mantissa}
  neg ecx
  add ecx, 127 + 63
  {Shift the mantissa all the way to the left}
  shl edx, 8
  {Add the implied 1 bit to the mantissa}
  or edx, $80000000
  xor eax, eax
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
@RaiseInvalidOperationException:
  {Force numeric overflow}
  mov al, reInvalidOp
  jmp System.Error
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns @RaiseInvalidOperationException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js @RaiseInvalidOperationException
  xor eax, eax
@Done:
end;
