(*
    The Unified Environment Core Library

    Typecast and platform-based non-OOP utilites

    Copyright (c) 2007-2012 The Unified Environment Laboratory
*)

unit CoreUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows;

type
{$IFDEF CPUX64}
  Integer   = NativeInt;
  Cardinal  = NativeUInt;
{$ENDIF}

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

  QuadChar      = UCS4Char;       // type LongWord
  PQuadChar     = PLongWordArray; // instead of ^UCS4Char;
  PPQuadChar    = ^PQuadChar;

  LegacyChar    = AnsiChar;
  PLegacyChar   = PAnsiChar;
  PPLegacyChar  = PPAnsiChar;

  CoreChar      = WideChar;  // new API, Unicode only
  PCoreChar     = PWideChar;
  PPCoreChar    = PPWideChar;

{$I Unicode.inc}

const
  PathDelimiter = WideChar('\'); // platform;
  WideCRLF: array[0..1] of WideChar = (WideChar(13), WideChar(10));
{$IFNDEF Tricks}
  CRLF: array[0..Length(sLineBreak) - 1] of LegacyChar = sLineBreak;
  HexDigits: array [$0..$F] of LegacyChar = '0123456789ABCDEF';
var
  MainWindow: THandle;
{$ENDIF}

{ Platform support }

var
  PlatformIsWindowsXP: Boolean;

function IsPlatformUnicode(MinVersion: Word = $500): Boolean;
function IsPlatformUnicodeEx(MinVersion: Word = $500): Boolean;

{$IFNDEF Tricks}
procedure ErrorMessage(Msg: PLegacyChar; Len: Cardinal);
function MMX_Supported: Boolean;
{$ENDIF}

{ Memory service }

function AllocMem(Count: Cardinal): Pointer;
procedure FreeMemAndNil(var P);
procedure FreeAndNil(var Obj);
{function ReleaseAndNil(var Obj): Integer;
function ReleaseAndRef(var Obj; NewObj: TSharedObject): Integer;}

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;

{ String service }

const
  DecimalLongInt  = 11;
  DecimalQuadInt  = 22;
  DecimalInt      = DecimalLongInt; // TODO: x64

  HexLongInt      = 8;
  HexQuadInt      = 16;

  HexInt          = HexLongInt; // TODO: x64
  HexPointer      = HexInt;     // both x86 and x64

  DecimalFloat    = 18;
  DecimalExtended = 22;
  DecimalCurrency = DecimalExtended;

function EstimateArgs(const Args: array of const): Cardinal;

function StrAlloc(Length: Cardinal): PLegacyChar;
function WideStrAlloc(Length: Cardinal): PWideChar;
function QuadStrAlloc(Length: Cardinal): PQuadChar;

procedure StrCopy(Dest, Source: PLegacyChar); overload;
procedure StrCopy(Dest, Source: PLegacyChar; Length: Cardinal); overload;
procedure WideStrCopy(Dest, Source: PWideChar); overload;
procedure WideStrCopy(Dest, Source: PWideChar; Length: Cardinal); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar; Length: Cardinal); overload;

function StrLen(Str: PLegacyChar): Cardinal; overload;
function StrLen(Str: PLegacyChar; MaxLength: Cardinal): Cardinal; overload;
function WideStrLen(Str: PWideChar): Cardinal; overload;
function WideStrLen(Str: PWideChar; MaxLength: Cardinal): Cardinal; overload;
function QuadStrLen(Str: PQuadChar): Cardinal; overload;
function QuadStrLen(Str: PQuadChar; MaxLength: Cardinal): Cardinal; overload;

function StrNew(Str: PLegacyChar): PLegacyChar; overload;
function StrNew(Str: PLegacyChar; Length: Cardinal): PLegacyChar; overload;
function WideStrNew(Str: PWideChar): PWideChar; overload;
function WideStrNew(Str: PWideChar; Length: Cardinal): PWideChar; overload;
function QuadStrNew(Str: PQuadChar): PQuadChar; overload;
function QuadStrNew(Str: PQuadChar; Length: Cardinal): PQuadChar; overload;

function StrScan(Where: PLegacyChar; What: LegacyChar; Count: Cardinal): PLegacyChar;
function WideStrScan(Where: PWideChar; What: WideChar; Count: Cardinal): PWideChar;
function QuadStrScan(Where: PQuadChar; What: QuadChar; Count: Cardinal): PQuadChar;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbUnknown): TCharBlock;

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): PWideChar;

{ Legacy Windows service }

function FormatBuf(Fmt: PLegacyChar; const Args: array of const;
  Buf: PLegacyChar): Cardinal;
function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Cardinal;

{ FreeMem finalization required }

function DecodeLegacy(Source: PLegacyChar; CodePage: Word): PWideChar; overload;
function DecodeLegacy(Source: PLegacyChar; Count: Cardinal; CodePage: Word): PWideChar; overload;

function EncodeLegacy(Source: PWideChar; CodePage: Word): PLegacyChar; overload;
function EncodeLegacy(Source: PWideChar; Count: Cardinal; CodePage: Word): PLegacyChar; overload;

function Format(Fmt: PLegacyChar; const Args: array of const): PLegacyChar;
function WideFormat(Fmt: PWideChar; const Args: array of const): PWideChar;

//function LatinFormat(Fmt: PLegacyChar; const Args: array of const): PWideChar;
function LegacyFormat(Fmt: PLegacyChar; CodePage: Word; const Args: array of const): PWideChar;

implementation

{ Memory service }

function AllocMem(Count: Cardinal): Pointer;
begin
  GetMem(Result, Count);
  FillChar(Result^, Count, 0);
end;

procedure FreeMemAndNil(var P);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP System.@FreeMem
end;

procedure FreeAndNil(var Obj);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TObject.Free
end;

procedure Exchange(var P1, P2: Pointer); overload;
asm
        MOV  ECX, [EAX]
        XCHG ECX, [EDX]  // XCHG enforces LOCK
        XCHG ECX, [EAX]
end;

procedure Exchange(var P1, P2: Int64); overload;
asm
        MOV ECX, EDX
        MOV EDX, [EAX + 2]
        MOV EAX, [EAX]
   LOCK CMPXCHG8B [ECX]
end;

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;
asm
        MUL EDX
        DIV ECX
end;

{ Platform support }

function IsPlatformUnicode(MinVersion: Word): Boolean;
var
  Version: LongWord;
begin
  Version := GetVersion;
  if Version and $80000000 = 0 then
    Result := Swap(LongRec(Version).Lo) >= MinVersion
  else
    Result := False;
end;

function IsPlatformUnicodeEx(MinVersion: Word): Boolean;
var
  Version: LongWord;
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
    Inc(P);   // ad-hoc, see ErrorMessageBox below
    P^ := #0;
  end
  else
    Msg := nil;
  Flags := MB_ICONERROR;
  if MainWindow = 0 then
    Flags := Flags or MB_TASKMODAL;
  MessageBoxA(MainWindow, Msg, nil, Flags);
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
        CALL Move  // ad-hoc for ending '.'
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

{ String service }

function EstimateArgs(const Args: array of const): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Args) to High(Args) do
    case TVarRec(Args[I]).VType of
      vtInteger:
        Inc(Result, DecimalInt);
      vtPChar:
        Inc(Result, StrLen(TVarRec(Args[I]).VPChar));
      vtPWideChar{, vtWideString}:
        Inc(Result, WideStrLen(TVarRec(Args[I]).VPWideChar));
      vtPointer:
        Inc(Result, SizeOf(Pointer) * 2); // 2 hex digit per byte
      vtChar, vtWideChar:
        Inc(Result);
      vtExtended, vtCurrency{, vtInt64}:
        Inc(Result, DecimalExtended);
      {vtString:
        Inc(Result, Length(TVarRec(Args[I]).VString^));
      vtAnsiString:
        Inc(Result, Length(PLegacyChar(TVarRec(Args[I]).VAnsiString)));}
    end;
end;

function StrAlloc(Length: Cardinal): PLegacyChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, Length + 1);
    Result[0] := #0;
  end
  else
    Result := nil;
end;

function WideStrAlloc(Length: Cardinal): PWideChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(WideChar));
    Result[0] := WideChar(0);
  end
  else
    Result := nil;
end;

function QuadStrAlloc(Length: Cardinal): PQuadChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(QuadChar));
    Result[0] := QuadChar(0);
  end
  else
    Result := nil;
end;

procedure StrCopy(Dest, Source: PLegacyChar);
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
begin
end;
{$ENDIF}
{$I FastCode\StrCopy.inc}

procedure StrCopy(Dest, Source: PLegacyChar; Length: Cardinal);
begin
  Move(Source^, Dest^, Length);
  Dest[Length] := #0;
end;

procedure WideStrCopy(Dest, Source: PWideChar);
begin
  WideStrCopy(Dest, Source, WideStrLen(Source));
end;

procedure WideStrCopy(Dest, Source: PWideChar; Length: Cardinal);
begin
  Move(Source^, Dest^, Length * SizeOf(WideChar));
  Dest[Length] := WideChar(0);
end;

procedure QuadStrCopy(Dest, Source: PQuadChar);
begin
  QuadStrCopy(Dest, Source, QuadStrLen(Source));
end;

procedure QuadStrCopy(Dest, Source: PQuadChar; Length: Cardinal);
begin
  Move(Source^, Dest^, Length * SizeOf(QuadChar));
  Dest[Length] := QuadChar(0);
end;

function StrLen(Str: PLegacyChar): Cardinal;
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
begin
end;
{$ENDIF}
{$I FastCode\StrLen.inc}

function StrLen(Str: PLegacyChar; MaxLength: Cardinal): Cardinal;
asm
        TEST EAX, EAX
        JZ @@exit
        TEST EDX, EDX
        JZ @@exit

        MOV ECX, MaxLength
        PUSH ECX
        MOV EDX, EDI
        MOV EDI, EAX
        XOR AL, AL
        REPNE SCASB
        POP EAX
        SUB EAX, ECX
        DEC EAX
        MOV EDI, EDX
@@exit:
end;

function WideStrLen(Str: PWideChar): Cardinal;
asm
        TEST EAX, EAX
        JZ @@exit

        MOV EDX, EDI
        MOV EDI, EAX
        MOV ECX, $FFFFFFFF
        XOR AX, AX
        REPNE SCASW
        MOV EAX, $FFFFFFFE
        SUB EAX, ECX
        MOV EDI, EDX
@@exit:
end;

function WideStrLen(Str: PWideChar; MaxLength: Cardinal): Cardinal;
asm
        TEST EAX, EAX
        JZ @@exit
        TEST EDX, EDX
        JZ @@exit

        MOV ECX, MaxLength
        PUSH ECX
        MOV EDX, EDI
        MOV EDI, EAX
        XOR AX, AX
        REPNE SCASW
        POP EAX
        SUB EAX, ECX
        DEC EAX
        MOV EDI, EDX
@@exit:
end;

function QuadStrLen(Str: PQuadChar): Cardinal;
asm
        TEST EAX, EAX
        JZ @@exit

        MOV EDX, EDI
        MOV EDI, EAX
        MOV ECX, $FFFFFFFF
        XOR EAX, EAX
        REPNE SCASD
        MOV EAX, $FFFFFFFE
        SUB EAX, ECX
        MOV EDI, EDX
@@exit:
end;

function QuadStrLen(Str: PQuadChar; MaxLength: Cardinal): Cardinal;
asm
        TEST EAX, EAX
        JZ @@exit
        TEST EDX, EDX
        JZ @@exit

        MOV ECX, MaxLength
        PUSH ECX
        MOV EDX, EDI
        MOV EDI, EAX
        XOR EAX, EAX
        REPNE SCASD
        POP EAX
        SUB EAX, ECX
        DEC EAX
        MOV EDI, EDX
@@exit:
end;

function StrNew(Str: PLegacyChar): PLegacyChar;
var
  L: Cardinal;
begin
  L := StrLen(Str);
  if L <> 0 then
  begin
    GetMem(Result, L + 1);
    StrCopy(Result, Str, L);
  end
  else
    Result := nil;
end;

function StrNew(Str: PLegacyChar; Length: Cardinal): PLegacyChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, Length + 1);
    StrCopy(Result, Str, Length);
  end
  else
    Result := nil;
end;

function WideStrNew(Str: PWideChar): PWideChar;
var
  L: Cardinal;
begin
  L := WideStrLen(Str);
  if L <> 0 then
  begin
    GetMem(Result, (L + 1) * SizeOf(WideChar));
    WideStrCopy(Result, Str, L);
  end
  else
    Result := nil;
end;

function WideStrNew(Str: PWideChar; Length: Cardinal): PWideChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(WideChar));
    WideStrCopy(Result, Str, Length);
  end
  else
    Result := nil;
end;

function QuadStrNew(Str: PQuadChar): PQuadChar;
var
  L: Cardinal;
begin
  L := QuadStrLen(Str);
  if L <> 0 then
  begin
    GetMem(Result, (L + 1) * SizeOf(QuadChar));
    QuadStrCopy(Result, Str, L);
  end
  else
    Result := nil;
end;

function QuadStrNew(Str: PQuadChar; Length: Cardinal): PQuadChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(QuadChar));
    QuadStrCopy(Result, Str, Length);
  end
  else
    Result := nil;
end;

function StrScan(Where: PLegacyChar; What: LegacyChar; Count: Cardinal): PLegacyChar;
asm
        XCHG EAX, EDX
        XCHG EDI, EDX
        REPNE SCASB
        JNE @@notfound
        MOV EAX, EDI
        DEC EAX
        MOV EDI, EDX
        RET
@@notfound:
        XOR EAX, EAX
        MOV EDI, EDX
end;

function WideStrScan(Where: PWideChar; What: WideChar; Count: Cardinal): PWideChar;
asm
        XCHG EAX, EDX
        XCHG EDI, EDX
        REPNE SCASW
        JNE @@notfound
        MOV EAX, EDI
        DEC EAX
        DEC EAX
        MOV EDI, EDX
        RET
@@notfound:
        XOR EAX, EAX
        MOV EDI, EDX
end;

function QuadStrScan(Where: PQuadChar; What: QuadChar; Count: Cardinal): PQuadChar;
asm
        XCHG EAX, EDX
        XCHG EDI, EDX
        REPNE SCASD
        JNE @@notfound
        MOV EAX, EDI
        SUB EAX, SizeOf(QuadChar);
        MOV EDI, EDX
        RET
@@notfound:
        XOR EAX, EAX
        MOV EDI, EDX
end;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock): TCharBlock;
var
  Min, Max: TUnicodeBlock;
begin
  if PrevBlock in [Low(TUnicodeBlock)..High(TUnicodeBlock)] then
  begin
    if Source < UnicodeBlockRanges[PrevBlock].Min then
    begin
      Min := Low(TUnicodeBlock);
      Max := Pred(PrevBlock);
    end
    else if Source > UnicodeBlockRanges[PrevBlock].Max then
    begin
      Min := Succ(PrevBlock);
      Max := High(TUnicodeBlock);
    end
    else
    begin
      Result := PrevBlock;
      Exit;
    end;
  end
  else
  begin
    Min := Low(TUnicodeBlock);
    Max := High(TUnicodeBlock);
  end;
  while Min <= Max do
  begin
    Result := TCharBlock((Ord(Min) + Ord(Max)) div 2);
    if Source < UnicodeBlockRanges[Result].Min then
      Max := Pred(Result)
    else if Source > UnicodeBlockRanges[Result].Max then
      Min := Succ(Result)
    else
      Exit;
  end;
  Result := cbNonUnicode;
end;

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): PWideChar;
var
  L: Cardinal;
begin
  L := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    nil, ErrorCode, 0, @Result, 0, nil);
  while (L <> 0) and
    ((Result[L] >= WideChar(0)) and (Result[L] <= WideChar(32)) or
     (Result[L] = WideChar('.')))
  do
    Dec(L);
  if L <> 0 then
    Result[L + 1] := WideChar(0);
end;

{ Legacy Windows service }

const
  VarArgSize = SizeOf(TVarRec);

function FormatBuf(Fmt: PLegacyChar; const Args: array of const;
  Buf: PLegacyChar): Cardinal;
asm
        PUSH EDI
        PUSH EBX
        MOV EBX, ESP

        INC ECX
        JZ @@call
@@arg:
        MOV EDI, [EDX + ECX * VarArgSize - VarArgSize]
        PUSH EDI
        LOOP @@arg
@@call:
        PUSH ESP
        PUSH EAX
        MOV EAX, Buf
        PUSH EAX
        CALL wvsprintfA

        MOV ESP, EBX
        POP EBX
        POP EDI
end;

function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Cardinal;
asm
        PUSH EDI
        PUSH EBX
        MOV  EBX, ESP

        INC ECX
        JZ  @@call
@@arg:
        MOV EDI, [EDX + ECX * VarArgSize - VarArgSize]
        PUSH EDI
        LOOP @@arg
@@call:
        PUSH ESP
        PUSH EAX
        MOV EAX, Buf
        PUSH EAX
        CALL wvsprintfW

        MOV ESP, EBX
        POP EBX
        POP EDI
end;

{ FreeMem finalization required }

function DecodeLegacy(Source: PLegacyChar; CodePage: Word): PWideChar;
begin
  Result := DecodeLegacy(Source, StrLen(Source), CodePage);
end;

function DecodeLegacy(Source: PLegacyChar; Count: Cardinal; CodePage: Word): PWideChar;
var
  L: Cardinal;
begin
  L := {$IFDEF Tricks} System. {$ENDIF}
    MultiByteToWideChar(CodePage, 0, Source, Count, nil, 0);
  if L <> 0 then
  begin
    GetMem(Result, (L + 1) * SizeOf(WideChar));
  {$IFDEF Tricks} System. {$ENDIF}
    MultiByteToWideChar(CodePage, 0, Source, Count, Result, L);
    Result[L] := WideChar(0);
  end
  else
    Result := nil;
end;

function EncodeLegacy(Source: PWideChar; CodePage: Word): PLegacyChar;
begin
  Result := EncodeLegacy(Source, WideStrLen(Source), CodePage);
end;

function EncodeLegacy(Source: PWideChar; Count: Cardinal; CodePage: Word): PLegacyChar;
var
  L: Cardinal;
begin
  L := {$IFDEF Tricks} System. {$ENDIF}
    WideCharToMultiByte(CodePage, 0, Source, Count, nil, 0, nil, nil);
  if L <> 0 then
  begin
    GetMem(Result, L + 1);
  {$IFDEF Tricks} System. {$ENDIF}
    WideCharToMultiByte(CodePage, 0, Source, Count, Result, L, nil, nil);
    Result[L] := #0;
  end
  else
    Result := nil;
end;

function Format(Fmt: PLegacyChar; const Args: array of const): PLegacyChar;
begin
  GetMem(Result, StrLen(Fmt) + EstimateArgs(Args) + 1);
  ReallocMem(Result, FormatBuf(Fmt, Args, Result) + 1);
end;

function WideFormat(Fmt: PWideChar; const Args: array of const): PWideChar;
begin
  GetMem(Result, (WideStrLen(Fmt) + EstimateArgs(Args) + 1) * SizeOf(WideChar));
  ReallocMem(Result, (WideFormatBuf(Fmt, Args, Result) + 1) * SizeOf(WideChar));
end;

{function LatinFormat(Fmt: PLegacyChar; const Args: array of const): PWideChar;
var
  I, L: Cardinal;
  T: LongWord;
  W, Z: PWideChar;
begin
  L := StrLen(Fmt);
  Inc(L);
  if L <> 0 then
  begin
    GetMem(W, L * SizeOf(WideChar));
    try
      Z := W;
      for I := 0 to L div 4 - 1 do // FastCore
      begin
        T := PLongWordArray(Fmt)[I];
        PLongWord(Z)^ := (T and $FF) or (((T and $FF00) shr 8) shl 16);
        Inc(Z, 2);
        T := T shr 16;
        PLongWord(Z)^ := (T and $FF) or (((T and $FF00) shr 8) shl 16);
        Inc(Z, 2);
      end;
      for I := L and not 3 to L - 1 do
      begin
        Z^ := WideChar(Fmt[I]);
        Inc(Z);
      end;
      Result := WideFormat(W, Args);
    finally
      FreeMem(W);
    end;
  end
  else
    Result := nil;
end;}

function LegacyFormat(Fmt: PLegacyChar; CodePage: Word; const Args: array of const): PWideChar;
var
  W: PWideChar;
begin
  W := DecodeLegacy(Fmt, CodePage);
  try
    Result := WideFormat(W, Args);
  finally
    FreeMem(W);
  end;
end;

end.



