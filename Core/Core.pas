(*
    The Unified Environment, legacy Win32 core

    General core unit

    Copyright (c) 2007-2009 The Unified Environment Laboratory
*)

unit Core;

interface

type
//  TCoreObject = TInterfacedObject;
//  TCoreClass = class of TCoreObject;

//  ICoreInterface = IInterface;

//  Float = Double;

{$IFDEF Unicode}
  CoreChar    = WideChar;
  PCoreChar   = PWideChar;
  PPCoreChar  = PPWideChar;
  CoreString  = WideString;
  PCoreString = PWideString;
{$ELSE}
  CoreChar    = AnsiChar;
  PCoreChar   = PAnsiChar;
  PPCoreChar  = PPAnsiChar;
  CoreString  = AnsiString;
  PCoreString = PAnsiString;
{$ENDIF}

  QuadChar    = UCS4Char;
  QuadString  = UCS4String;
  PQuadChar   = PUCS4Char;
  PPQuadChar  = ^PQuadChar;
  PQuadString = ^QuadString;

var
  PlatformIsWindowsXP: Boolean;

function IsPlatformUnicode(MinVersion: Word = $500): Boolean;
function IsPlatformUnicodeEx(MinVersion: Word = $500): Boolean;

{ Core services }

function AllocMem(Count: Integer): Pointer;
procedure FreeMemAndNil(var P);
procedure FreeAndNil(var Obj);

procedure Exchange(var P1, P2: Pointer); overload;
{$IFNDEF Multithread}
procedure Exchange(var P1, P2: Int64); overload;
{$ENDIF}

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;

implementation

uses
  Windows, SysUtils;

{ Core services }

function AllocMem(Count: Integer): Pointer;
begin
  GetMem(Result, Count);
  FillChar(Result^, Count, 0);
end;

procedure FreeMemAndNil(var P);
{$IFDEF Multithread}
var
  T: Pointer;
begin
  T := InterlockedExchange(LongInt(P), 0);
  FreeMem(T);
end;
{$ELSE}
begin
  FreeMem(Pointer(P));
  Pointer(P) := nil;
end;
{$ENDIF}

procedure FreeAndNil(var Obj);
{$IFDEF Multithread}
var
  T: TObject;
begin
  InterlockedExchange(LongInt(T), 0);
  T.Free;
end;
{$ELSE}
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;
{$ENDIF}

procedure Exchange(var P1, P2: Pointer); overload;
{$IFDEF Multithread}
begin
  InterlockedExchange(LongInt(P1), LongInt(P2));
end;
{$ELSE}
asm
        MOV   ECX, [EAX]
        XCHG  ECX, [EDX]
        MOV   [EAX], ECX
end;
{$ENDIF}

{$IFNDEF Multithread} // InterlockedExchange64 requires Vista :(
procedure Exchange(var P1, P2: Int64); overload;
asm
        MOV   ECX, [EAX]
        XCHG  ECX, [EDX]
        MOV   [EAX], ECX
        MOV   ECX, [EAX+4]
        XCHG  ECX, [EDX+4]
        MOV   [EAX+4], ECX
end;
{$ENDIF}

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;
asm
        MUL EDX
        DIV ECX
end;

function IsPlatformUnicode(MinVersion: Word): Boolean;
var
  Version: Cardinal;
begin
  Version := GetVersion;
  if Version and $80000000 = 0 then
    Result := Swap(LongRec(Version).Lo) >= MinVersion
  else
    Result := False;
end;

function IsPlatformUnicodeEx(MinVersion: Word): Boolean;
var
  Version: Cardinal;
begin
  Version := GetVersion;
  if Version and $80000000 = 0 then
    with LongRec(Version) do
    begin
      Lo := Swap(Lo);
      PlatformIsWindowsXP := Lo >= $501;
      Result := Lo >= MinVersion;
    end
  else
    Result := False;
end;

end.

