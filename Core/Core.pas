(*
    The Unified Environment Core Library

    General core unit

    Copyright (c) 2007-2010 The Unified Environment Laboratory
*)

unit Core;

{$WARN SYMBOL_PLATFORM OFF}

interface

type
  WordRec = packed record
    case Integer of
      0: (Lo, Hi: Byte);
      1: (Bytes: array [0..1] of Byte);
  end;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  Int64Rec = packed record
    case Integer of
      0: (Lo, Hi: LongWord);
      1: (LongWords: array [0..1] of LongWord);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxInt div SizeOf(Word) - 1] of Word;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..MaxInt div SizeOf(LongWord) - 1] of LongWord;

  LegacyChar    = AnsiChar;
  PLegacyChar   = PAnsiChar;
  PPLegacyChar  = PPAnsiChar;
  LegacyString  = AnsiString;
  PLegacyString = PAnsiString;

  QuadChar      = UCS4Char;       // type LongWord
  PQuadChar     = PLongWordArray; // instead of ^UCS4Char;
  PPQuadChar    = ^PQuadChar;

  QuadString    = UCS4String;
  PQuadString   = ^QuadString;

{$IFDEF Unicode}
  CoreChar      = WideChar;
  PCoreChar     = PWideChar;
  PPCoreChar    = PPWideChar;
  CoreString    = WideString;
  PCoreString   = PWideString;
{$ELSE}
  CoreChar      = LegacyChar;
  PCoreChar     = PLegacyChar;
  PPCoreChar    = PPLegacyChar;
  CoreString    = LegacyString;
  PCoreString   = PLegacyString;
{$ENDIF}

{$IFDEF Localization}
  TMessageText  = Cardinal;
{$ELSE}
  TMessageText  = PLegacyChar;
{$ENDIF}

{$IFNDEF Tricks}
  CRLF: array[0..Length(sLineBreak) - 1] of LegacyChar = sLineBreak;
  HexDigits: array [$0..$F] of LegacyChar = '0123456789ABCDEF';
var
  MainWindow: THandle;
{$ENDIF}

const
  PathDelimiter = CoreChar('\');

{ Platform support }

var
  PlatformIsWindowsXP: Boolean;

function IsPlatformUnicode(MinVersion: Word = $500): Boolean;
function IsPlatformUnicodeEx(MinVersion: Word = $500): Boolean;
function IsPlatformWindows(MinVersion: Word = $500): Boolean;

{$IFNDEF Tricks}
procedure ErrorMessage(Msg: PLegacyChar; Len: Cardinal);
function MMX_Supported: Boolean;
{$ENDIF}

{ Memory service }

function AllocMem(Count: Integer): Pointer;
procedure FreeMemAndNil(var P);
procedure FreeAndNil(var Obj);
{function ReleaseAndNil(var Obj): Integer;
function ReleaseAndRef(var Obj; NewObj: TSharedObject): Integer;}

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;

implementation

uses
  Windows;

{ Memory service }

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
  Integer(T) := InterlockedExchange(Integer(P), 0);
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
  InterlockedExchange(Integer(T), 0);
  T.Free;
end;
{$ELSE}
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;
{$ENDIF}

{function ReleaseAndNil(var Obj): Integer;
begin
  Result := TSharedObject(Obj).Release(rmRelease);
  if Result = 0 then
    FreeAndNil(Obj);
end;

function ReleaseAndRef(var Obj; NewObj: TSharedObject): Integer;
begin
  ReleaseAndNil(Obj);
  Result := NewObj.Ref;
  TSharedObject(Obj) := NewObj;
end;}

procedure Exchange(var P1, P2: Pointer); overload;
{$IFDEF Multithread}
begin
  InterlockedExchange(Integer(P1), Integer(P2));
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

{ Platform support }

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

function IsPlatformWindows(MinVersion: Word = $500): Boolean;
begin
  Result := Swap(LongRec(GetVersion and not $80000000).Lo) >= MinVersion;
end;

{$IFNDEF Tricks}
procedure ErrMsgBox(Msg: PLegacyChar; Len: Cardinal);
var
  P: PLegacyChar;
  Flags: Cardinal;
begin
  if Len <> 0 then
  begin
    P := Msg + Len - 1;
    if not (P^ in ['.', '!', '?']) then
      P^ := '.';
    Inc(P);
    P^ := #0;
  end
  else
    Msg^ := #0;
  Flags := MB_ICONERROR;
  if MainWindow = 0 then
    Flags := Flags or MB_TASKMODAL;
  MessageBox(MainWindow, Msg, nil, Flags);
end;

procedure ErrorMessageBox(Msg: PLegacyChar; Len: Cardinal);
asm
        MOV ECX, EDX // MsgLen
        ADD ECX, 3
        AND ECX, $FFFFFFFC

        PUSH EDI
        MOV EDI, ESP
        SUB ESP, ECX
        MOV ECX, EDX
        MOV EDX, ESP
        PUSH ECX
        CALL Move
        POP EDX
        MOV EAX, ESP
        CALL ErrMsgBox
        MOV ESP, EDI
        POP EDI
end;

procedure ErrorMessage(Msg: PLegacyChar; Len: Cardinal);
var
  hError: THandle;
  Dummy: Cardinal;
begin
  if IsConsole then
  begin
    Flush(Output);
    hError := GetStdHandle(STD_ERROR_HANDLE);
    WriteFile(hError, Msg^, Len, Dummy, nil);
    WriteFile(hError, CRLF, Length(CRLF), Dummy, nil);
  end
  else
    ErrorMessageBox(Msg, Len);
end;

function MMX_Supported: Boolean; // by Freeman
asm
        PUSHFD
        POP EAX
        MOV EDX, EAX
        XOR EAX, $200000
        PUSH EAX
        POPFD
        PUSHFD
        POP EAX
        XOR EAX, EDX
        JZ @@exit

        PUSH EBX
        MOV EAX, 1
        CPUID
        TEST EDX, $800000
        SETNZ AL
        POP EBX
@@exit:
end;
{$ENDIF}

end.



