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

// MoveEx calls are replaced to Move because our System.Move is already fast
// (x) Freeman

var
  AnsiUpcase : packed array[Char] of AnsiChar;

{Non Case Sensitive version of PosEx}
function PosExIgnoreCase; //(const SubStr, S: string; Offset: Cardinal = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi              {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx              {Save Max Start Position}
  lea     edi, AnsiUpcase  {Uppercase Lookup Table}
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]   {Convert to Uppercase}
@@Loop:                    {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]       {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within String Data}
@@NotChar2:
  add     edx, 2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, AnsiUpcase
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
@@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check Match is within String Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1           {OK - Adjust Result}
@@SetResult:               {Set Result Position}
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end; {PosExIgnoreCase}

{Replace all occurance of Old (Ignoring Case) with New in Non-Null String S}
procedure CharReplaceIC; //(var S: AnsiString; const Old, New: Char);
asm
  push  ebx
  push  edi
  push  esi
  mov   eax, [eax]         {@S}
  mov   ebx, ecx           {bl = New}
  lea   edi, AnsiUpcase
  and   edx, $FF           {edx = Old}
  mov   ecx, [eax-4]       {Length(S)}
  movzx edx, [edx+edi]     {edx = Uppercase(Old)}
  lea   esi, [eax+ecx]
  neg   ecx
@@Loop:
  movzx eax, [esi+ecx]     {Next Char}
  movzx eax, [eax+edi]     {Convert to Uppercase}
  cmp   eax, edx           {Compare Char}
  jne   @@Next
  mov   [esi+ecx], bl      {Replace Char}
@@Next:
  add   ecx, 1
  jnz   @@Loop
  pop   esi
  pop   edi
  pop   ebx
end;

{Replace all occurance of Old (Case Sensitive) with New in Non-Null String S}
procedure CharReplaceCS; //(var S: AnsiString; const Old, New: Char);
asm
  push  ebx
  mov   eax, [eax]    {@S}
  mov   ebx, ecx      {bl = New, dl = Old}
  mov   ecx, [eax-4]  {Length(S)}
  add   eax, ecx
  neg   ecx
@@Loop:
  cmp   dl, [eax+ecx] {Compare Next Char}
  jne   @@Next
  mov   [eax+ecx], bl {Replace Char}
@@Next:
  add   ecx, 1
  jnz   @@Loop
  pop   ebx
end;

function StringReplace; //(const S, OldPattern, NewPattern: AnsiString;
                        //          const Flags: TReplaceFlags): AnsiString;

//function StringReplace_JOH_IA32_4(const S, OldPattern, NewPattern: AnsiString;
//                                 Flags: TReplaceFlags): AnsiString;
type
  TPosEx   = function(const SubStr, S: AnsiString; Offset: Cardinal = 1): Integer;
  TCharRep = procedure(var S : AnsiString; const Old, New : AnsiChar);
const
  StaticBufferSize = 16;
  PosExFunction : array[Boolean] of TPosEx   = (PosEx, PosExIgnoreCase);
  CharReplace   : array[Boolean] of TCharRep = (CharReplaceCS, CharReplaceIC);
var
  SrcLen, OldLen, NewLen, Found, Count, Start, Match, BufSize, BufMax : Integer;
  StaticBuffer : array[0..StaticBufferSize-1] of Integer;
  Buffer       : PIntegerArray;
  PSrc, PRes   : PAnsiChar;
  IgnoreCase   : Boolean;
begin
{$IFDEF AllowLengthShortcut}
  SrcLen := 0;
  if (S <> '') then      
    SrcLen := PCardinal(Cardinal(S)-4)^;
  OldLen := 0;
  if (OldPattern <> '') then
    OldLen := PCardinal(Cardinal(OldPattern)-4)^;
  NewLen := 0;
  if (NewPattern <> '') then
    NewLen := PCardinal(Cardinal(NewPattern)-4)^;
{$ELSE}
  SrcLen := Length(S);
  OldLen := Length(OldPattern);
  NewLen := Length(NewPattern);
{$ENDIF}
  if (OldLen = 0) or (SrcLen < OldLen) then
    begin
      if SrcLen = 0 then
        Result := '' {Needed for Non-Nil Zero Length Strings}
      else
        Result := S
    end
  else
    begin
      IgnoreCase := rfIgnoreCase in Flags;
      if rfReplaceAll in Flags then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            begin
              SetLength(Result, SrcLen);
              Move(Pointer(S)^, Pointer(Result)^, SrcLen);
              CharReplace[IgnoreCase](Result, OldPattern[1], NewPattern[1]);
              Exit;
            end;
          Found := PosExFunction[IgnoreCase](OldPattern, S, 1);
          if Found <> 0 then
            begin
              Buffer    := @StaticBuffer;
              BufMax    := StaticBufferSize;
              BufSize   := 1;
              Buffer[0] := Found;
              repeat
                Inc(Found, OldLen);
                Found := PosExFunction[IgnoreCase](OldPattern, S, Found);
                if Found > 0 then
                  begin
                    if BufSize = BufMax then
                      begin {Create or Expand Dynamic Buffer}
                        BufMax := BufMax + (BufMax shr 1); {Grow by 50%}
                        if Buffer = @StaticBuffer then
                          begin {Create Dynamic Buffer}
                            GetMem(Buffer, BufMax * SizeOf(Integer));
                            Move(StaticBuffer, Buffer^, SizeOf(StaticBuffer));
                          end
                        else {Expand Dynamic Buffer}
                          ReallocMem(Buffer, BufMax * SizeOf(Integer));
                      end;
                    Buffer[BufSize] := Found;
                    Inc(BufSize);
                  end
              until Found = 0;
              SetLength(Result, SrcLen + (BufSize * (NewLen - OldLen)));
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              Start := 1;
              Match := 0;
              repeat
                Found := Buffer[Match];
                Count := Found - Start;
                Start := Found + OldLen;
                if Count > 0 then
                  begin
                    Move(PSrc^, PRes^, Count);
                    Inc(PRes, Count);
                  end;
                Inc(PSrc, Count + OldLen);
                Move(Pointer(NewPattern)^, PRes^, NewLen);
                Inc(PRes, NewLen);
                Inc(Match);
              until Match = BufSize;
              Dec(SrcLen, Start);
              if SrcLen >= 0 then
                Move(PSrc^, PRes^, SrcLen + 1);
              if BufMax <> StaticBufferSize then
                FreeMem(Buffer); {Free Dynamic Buffwe if Created}
            end
          else {No Matches Found}                
            Result := S
        end
      else
        begin {Replace First Occurance Only}
          Found := PosExFunction[IgnoreCase](OldPattern, S, 1);
          if Found <> 0 then
            begin {Match Found}
              SetLength(Result, SrcLen - OldLen + NewLen);
              Dec(Found);
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              if NewLen = OldLen then
                begin
                  Move(PSrc^, PRes^, SrcLen);
                  Inc(PRes, Found);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                end
              else
                begin
                  Move(PSrc^, PRes^, Found);
                  Inc(PRes, Found);
                  Inc(PSrc, Found + OldLen);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                  Inc(PRes, NewLen);
                  Move(PSrc^, PRes^, SrcLen - Found - OldLen);
                end;
            end
          else {No Matches Found}
            Result := S
        end;
    end;
end;
