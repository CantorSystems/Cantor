(*
    Lite Core Library (CoreLite)

    Typecast and platform-based non-OOP utilites

    Copyright (c) 2007-2013 Vladislav Javadov (Freeman)

    Conditional defines:
      * Compat -- allow ShortString, AnsiString and WideString at EstimateArgs
*)

unit CoreUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows;

type
{$IFDEF CPUX64}
  CoreInt   = NativeInt;
  PCoreInt  = PNativeInt;

  CoreWord  = NativeUInt;
  PCoreWord = PNativeUInt;

  QuadWord  = NativeUInt;
  PQuadWord = PNativeUInt;
{$ELSE}
  CoreInt   = LongInt;
  PCoreInt  = PLongInt;

  CoreWord  = LongWord;
  PCoreWord = PLongWord;

  QuadWord  = {type} Int64;
  PQuadWord = {type} PInt64;
{$ENDIF}

  WordRec = packed record
    case Byte of
      0: (Lo, Hi: Byte);
      1: (Bytes: array [0..1] of Byte);
  end;

  LongRec = packed record
    case Byte of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

  QuadRec = packed record
    case Byte of
      0: (Lo, Hi: LongWord);
      1: (LongWords: array [0..1] of LongWord);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
  end;

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt - 1] of Byte;

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

  CoreChar      = WideChar;  // TODO: non-Unicode
  PCoreChar     = PWideChar;
  PPCoreChar    = PPWideChar;

  TLegacyStringRec = record
    Value: PLegacyChar;
    Length: Integer;
  end;

  TWideStringRec = record
    Value: PWideChar;
    Length: Integer;
  end;

  TCoreStringRec = TWideStringRec; // TODO: non-Unicode

  TLegacyParamRec = record
    NextParam, Param: PLegacyChar;
    Length: Integer;
    Quoted: Boolean;
  end;

  TWideParamRec = record
    NextParam, Param: PWideChar;
    Length: Integer;
    Quoted: Boolean;
  end;

  TCoreParamRec = TWideParamRec; // TODO: non-Unicode

var
  Ellipsis: array[0..2] of LegacyChar = '...'; // non-Unicode, not only for European languages
  WideEllipsis: WideChar = WideChar(8230);

const
  PathDelimiter = WideChar('\'); // platform;

  CRLF: array[0..1] of LegacyChar = #13#10;
  WideCRLF: array[0..1] of WideChar = (WideChar(13), WideChar(10));

  WideLF: WideChar = WideChar(10);
{$IFNDEF Tricks}
  LF: LegacyChar = #10;
  HexDigits: array [$0..$F] of LegacyChar = '0123456789ABCDEF';
var
  MainWindow: THandle;
{$ENDIF}

{ Platform support }

var
  PlatformIsWindowsXP: Boolean;

function IsPlatformUnicode: Boolean;
function IsPlatformUnicodeEx(MinVersion: Word = $500): Boolean;

{$IFNDEF Tricks}
procedure ErrorMessage(Msg: PLegacyChar; Len: Integer);
function MMX_Supported: Boolean;
{$ENDIF}

{ Memory service }

function AllocMem(Count: CoreInt): Pointer;
procedure FreeMemAndNil(var P);
procedure FreeAndNil(var Obj);

procedure Exchange(var P1, P2: Pointer); overload;
procedure Exchange(var P1, P2: Int64); overload;

function SwapBytes(Source: LongWord): LongWord;
function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;

{ String service }

const
  DecimalLongInt  = 11;
  DecimalQuadInt  = 21;
  DecimalInt      = DecimalLongInt; // TODO: x64

  HexLongInt      = 8;
  HexQuadInt      = 16;

  HexInt          = HexLongInt; // TODO: x64
  HexPointer      = HexInt;     // both x86 and x64

  DecimalFloat    = 18;
  DecimalExtended = 22;
  DecimalCurrency = DecimalExtended;

function EstimateArgs(const Args: array of const): Integer;

function StrAlloc(Length: Integer): PLegacyChar;
function WideStrAlloc(Length: Integer): PWideChar;
function QuadStrAlloc(Length: Integer): PQuadChar;

procedure StrCopy(Dest, Source: PLegacyChar); overload;
procedure StrCopy(Dest, Source: PLegacyChar; Length: Integer); overload;
procedure WideStrCopy(Dest, Source: PWideChar); overload;
procedure WideStrCopy(Dest, Source: PWideChar; Length: Integer); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar; Length: Integer); overload;

function StrLen(Str: PLegacyChar): Integer; overload;
function StrLen(Str: PLegacyChar; MaxLength: Integer): Integer; overload;
function WideStrLen(Str: PWideChar): Integer; overload;
function WideStrLen(Str: PWideChar; MaxLength: Integer): Integer; overload;
function QuadStrLen(Str: PQuadChar): Integer; overload;
function QuadStrLen(Str: PQuadChar; MaxLength: Integer): Integer; overload;

function StrNew(Str: PLegacyChar): PLegacyChar; overload;
function StrNew(Str: PLegacyChar; Length: Integer): PLegacyChar; overload;
function WideStrNew(Str: PWideChar): PWideChar; overload;
function WideStrNew(Str: PWideChar; Length: Integer): PWideChar; overload;
function QuadStrNew(Str: PQuadChar): PQuadChar; overload;
function QuadStrNew(Str: PQuadChar; Length: Integer): PQuadChar; overload;

function StrScan(Where: PLegacyChar; What: LegacyChar; Count: Integer): PLegacyChar;
function WideStrScan(Where: PWideChar; What: WideChar; Count: Integer): PWideChar;
function QuadStrScan(Where: PQuadChar; What: QuadChar; Count: Integer): PQuadChar;

function StrRScan(Where: PLegacyChar; What: LegacyChar; Count: Integer): PLegacyChar;
function WideStrRScan(Where: PWideChar; What: WideChar; Count: Integer): PWideChar;
function QuadStrRScan(Where: PQuadChar; What: QuadChar; Count: Integer): PQuadChar;

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Integer; Dest: PQuadChar);
procedure SwapWideCharBytes(Source: PWideChar; Count: Integer; Dest: PWideChar);

function StrComp(Str1: PLegacyChar; Count1: Integer; Str2: PLegacyChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;
function WideStrComp(Str1: PWideChar; Count1: Integer; Str2: PWideChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;

{ Command line parameters }

function ParamStr(CommandLine: PLegacyChar): TLegacyParamRec;
function WideParamStr(CommandLine: PWideChar): TWideParamRec;

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): PCoreChar;

{ Legacy Windows service }

const
  CSTR_LESS_THAN    = 1;
  CSTR_EQUAL        = 2;
  CSTR_GREATER_THAN = 3;

function FormatBuf(Fmt: PLegacyChar; const Args: array of const;
  Buf: PLegacyChar): Integer;
function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Integer;

{ FreeMem finalization required }

function DecodeLegacy(Source: PLegacyChar; CodePage: Word): TWideStringRec; overload;
function DecodeLegacy(Source: PLegacyChar; Count: Integer; CodePage: Word): TWideStringRec; overload;

function EncodeLegacy(Source: PWideChar; CodePage: Word{;
  UseDefaultChar: Boolean = True}): TLegacyStringRec; overload;
function EncodeLegacy(Source: PWideChar; Count: Integer; CodePage: Word{;
  UseDefaultChar: Boolean = True}): TLegacyStringRec; overload;

function Format(Fmt: PLegacyChar; const Args: array of const): PLegacyChar;
function WideFormat(Fmt: PWideChar; const Args: array of const): PWideChar;

//function LatinFormat(Fmt: PLegacyChar; const Args: array of const): PWideChar;
function LegacyFormat(Fmt: PLegacyChar; CodePage: Word; const Args: array of const): PWideChar;

{ User-friendly class names }

type
  // only the first 255 characters are significant (c) Delphi Help
  TClassName = array [0..256] of LegacyChar; // including null terminator

function FriendlyClassName(var Dest: TClassName; Source: TClass): Byte; overload;
function FriendlyClassName(var Dest: TClassName; Source: TObject): Byte; overload;

implementation

{ Memory service }

function AllocMem(Count: CoreInt): Pointer;
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

function SwapBytes(Source: LongWord): LongWord;
asm
        BSWAP EAX
end;

function MulDiv(Multiplicand, Multiplier, Divisor: LongWord): LongWord;
asm
        MUL EDX
        DIV ECX
end;

{ Platform support }

function IsPlatformUnicode: Boolean;
asm
        CALL GetVersion
        MOV EDX, EAX
        XOR EAX, EAX
        TEST EDX, $80000000
        SETZ AL
end;

function IsPlatformUnicodeEx(MinVersion: Word): Boolean;
asm
        PUSH EAX
        CALL IsPlatformUnicode
        POP ECX
        XCHG DL, DH
        CMP DX, CX
        SETNB AL
        CMP DX, $501
        SETNB PlatformIsWindowsXP
end;

{$IFNDEF Tricks}
procedure ErrMsgBox(Msg: PLegacyChar; Len: Integer);
var
  P: PLegacyChar;
  Flags: LongWord;
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

procedure ErrorMessageBox(Msg: PLegacyChar; Len: Integer);
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

procedure ErrorMessage(Msg: PLegacyChar; Len: Integer);
var
  hError: THandle;
  Dummy: LongWord;
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

function EstimateArgs(const Args: array of const): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Args) to High(Args) do
    case TVarRec(Args[I]).VType of
      vtInteger:
        Inc(Result, DecimalInt);
      vtInt64:
        Inc(Result, DecimalQuadInt);
      vtPChar:
        Inc(Result, StrLen(TVarRec(Args[I]).VPChar));
      vtPWideChar:
        Inc(Result, WideStrLen(TVarRec(Args[I]).VPWideChar));
      vtPointer:
        Inc(Result, SizeOf(Pointer) * 2); // 2 hex digit per byte
      vtChar, vtWideChar:
        Inc(Result);
      vtExtended, vtCurrency:
        Inc(Result, DecimalExtended);
    {$IFDEF Compat}
      vtString:
        Inc(Result, PByte(TVarRec(Args[I]).VString)^);
      vtAnsiString:
        Inc(Result, PInteger(PLegacyChar(TVarRec(Args[I]).VAnsiString) - SizeOf(Integer))^);
      vtWideString:
        Inc(Result, PLongInt(PLegacyChar(TVarRec(Args[I]).VWideString) - SizeOf(LongInt))^ div SizeOf(WideChar));
    {$ENDIF}
    end;
end;

function StrAlloc(Length: Integer): PLegacyChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, Length + 1);
    Result[0] := #0;
  end
  else
    Result := nil;
end;

function WideStrAlloc(Length: Integer): PWideChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(WideChar));
    Result[0] := WideChar(0);
  end
  else
    Result := nil;
end;

function QuadStrAlloc(Length: Integer): PQuadChar;
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

procedure StrCopy(Dest, Source: PLegacyChar; Length: Integer);
begin
  Move(Source^, Dest^, Length);
  Dest[Length] := #0;
end;

procedure WideStrCopy(Dest, Source: PWideChar);
begin
  WideStrCopy(Dest, Source, WideStrLen(Source));
end;

procedure WideStrCopy(Dest, Source: PWideChar; Length: Integer);
begin
  Move(Source^, Dest^, Length * SizeOf(WideChar));
  Dest[Length] := WideChar(0);
end;

procedure QuadStrCopy(Dest, Source: PQuadChar);
begin
  QuadStrCopy(Dest, Source, QuadStrLen(Source));
end;

procedure QuadStrCopy(Dest, Source: PQuadChar; Length: Integer);
begin
  Move(Source^, Dest^, Length * SizeOf(QuadChar));
  Dest[Length] := QuadChar(0);
end;

function StrLen(Str: PLegacyChar): Integer;
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
begin
end;
{$ENDIF}
{$I FastCode\StrLen.inc}

function StrLen(Str: PLegacyChar; MaxLength: Integer): Integer;
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
        JCXZ @@max
        SUB EAX, ECX
        DEC EAX
@@max:
        MOV EDI, EDX
@@exit:
end;

function WideStrLen(Str: PWideChar): Integer;
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

function WideStrLen(Str: PWideChar; MaxLength: Integer): Integer;
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
        JCXZ @@max
        SUB EAX, ECX
        DEC EAX
@@max:
        MOV EDI, EDX
@@exit:
end;

function QuadStrLen(Str: PQuadChar): Integer;
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

function QuadStrLen(Str: PQuadChar; MaxLength: Integer): Integer;
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
        JCXZ @@max
        SUB EAX, ECX
        DEC EAX
@@max:
        MOV EDI, EDX
@@exit:
end;

function StrNew(Str: PLegacyChar): PLegacyChar;
var
  L: Integer;
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

function StrNew(Str: PLegacyChar; Length: Integer): PLegacyChar;
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
  L: Integer;
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

function WideStrNew(Str: PWideChar; Length: Integer): PWideChar;
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
  L: Integer;
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

function QuadStrNew(Str: PQuadChar; Length: Integer): PQuadChar;
begin
  if Length <> 0 then
  begin
    GetMem(Result, (Length + 1) * SizeOf(QuadChar));
    QuadStrCopy(Result, Str, Length);
  end
  else
    Result := nil;
end;

function StrScan(Where: PLegacyChar; What: LegacyChar; Count: Integer): PLegacyChar;
asm
        TEST ECX, ECX
        JZ @@null
        XCHG EAX, EDX
        XCHG EDI, EDX
        REPNE SCASB
        JNE @@notfound
        MOV EAX, EDI
        DEC EAX
        MOV EDI, EDX
        RET
@@notfound:
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

function WideStrScan(Where: PWideChar; What: WideChar; Count: Integer): PWideChar;
asm
        TEST ECX, ECX
        JZ @@null
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
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

function QuadStrScan(Where: PQuadChar; What: QuadChar; Count: Integer): PQuadChar;
asm
        TEST ECX, ECX
        JZ @@null
        XCHG EAX, EDX
        XCHG EDI, EDX
        REPNE SCASD
        JNE @@notfound
        MOV EAX, EDI
        SUB EAX, SizeOf(QuadChar)
        MOV EDI, EDX
        RET
@@notfound:
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

function StrRScan(Where: PLegacyChar; What: LegacyChar; Count: Integer): PLegacyChar;
asm
        TEST ECX, ECX
        JZ @@null
        ADD EAX, ECX

        XCHG EAX, EDX
        XCHG EDI, EDX
        STD
        REPNE SCASB
        CLD
        JNE @@notfound
        MOV EAX, EDI
        INC EAX
        MOV EDI, EDX
        RET
@@notfound:
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

function WideStrRScan(Where: PWideChar; What: WideChar; Count: Integer): PWideChar;
asm
        TEST ECX, ECX
        JZ @@null
        PUSH ECX
        SHL ECX, 1
        ADD EAX, ECX
        POP ECX

        XCHG EAX, EDX
        XCHG EDI, EDX
        STD
        REPNE SCASW
        CLD
        JNE @@notfound
        MOV EAX, EDI
        INC EAX
        INC EAX
        MOV EDI, EDX
        RET
@@notfound:
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

function QuadStrRScan(Where: PQuadChar; What: QuadChar; Count: Integer): PQuadChar;
asm
        TEST ECX, ECX
        JZ @@null
        PUSH ECX
        SHL ECX, 2
        ADD EAX, ECX
        POP ECX

        XCHG EAX, EDX
        XCHG EDI, EDX
        STD
        REPNE SCASD
        CLD
        JNE @@notfound
        MOV EAX, EDI
        ADD EAX, SizeOf(QuadChar)
        MOV EDI, EDX
        RET
@@notfound:
        MOV EDI, EDX
@@null:
        XOR EAX, EAX
end;

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Integer; Dest: PQuadChar);
asm
        TEST EDX, EDX
        JZ @@exit

        XCHG ECX, EDX
        PUSH EBX

@@repeat:
        MOV EBX, [EAX]
        BSWAP EBX
        MOV [EDX], EBX
        ADD EAX, SizeOf(QuadChar)
        ADD EDX, SizeOf(QuadChar)
        LOOP @@repeat

        POP EBX
@@exit:
end;

procedure SwapWideCharBytes(Source: PWideChar; Count: Integer; Dest: PWideChar);
asm
        TEST EDX, EDX
        JZ @@exit

        XCHG ECX, EDX
        PUSH EBX

        PUSH ECX
        SHR ECX, 1
@@repeat2:
        MOV EBX, [EAX]
        XCHG BL, BH
        ROL BX, 16
        XCHG BL, BH
        ROL BX, 16
        MOV [EDX], EBX
        ADD EAX, SizeOf(LongWord)
        ADD EDX, SizeOf(LongWord)
        LOOP @@repeat2

        POP ECX
        AND ECX, 1
        JZ @@complete
        MOV BX, [EAX]
        XCHG BL, BH
        MOV [EDX], BX
@@complete:
        POP EBX
@@exit:
end;

function StrComp(Str1: PLegacyChar; Count1: Integer; Str2: PLegacyChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;
begin
  Result := CompareStringA(Locale, IgnoreFlags, Str1, Count1, Str2, Count2) - CSTR_EQUAL;
end;

function WideStrComp(Str1: PWideChar; Count1: Integer; Str2: PWideChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;
begin
  Result := CompareStringW(Locale, IgnoreFlags, Str1, Count1, Str2, Count2) - CSTR_EQUAL;
end;

{ Command line parameters }

function ParamStr(CommandLine: PLegacyChar): TLegacyParamRec;
var
  P: PLegacyChar;
  L: Integer;
begin
  if CommandLine <> nil then
  begin
    while (CommandLine^ in [#9, #32]) do
      Inc(CommandLine);
    if CommandLine^ = '"' then
    begin
      Inc(CommandLine);
      L := StrLen(CommandLine);
      P := StrScan(CommandLine, '"', L); // TODO: MBCS
      if P <> nil then
        L := P - CommandLine;
      Result.Quoted := True;
    end
    else
    begin
      P := CommandLine;
      while not (P^ in [#32, #9, #0]) do
        Inc(P);
      L := P - CommandLine;
      Result.Quoted := False;
    end;
    with Result do
    begin
      NextParam := CommandLine + L + Byte(Quoted);
      if NextParam^ <> #0 then
        Inc(NextParam);
      Param := CommandLine;
      Length := L;
    end;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function WideParamStr(CommandLine: PWideChar): TWideParamRec;
var
  P: PWideChar;
  L: Integer;
begin
  if CommandLine <> nil then
  begin
    while (CommandLine^ = WideChar(32)) or (CommandLine^ = WideChar(9)) do
      Inc(CommandLine);
    if CommandLine^ = WideChar('"') then
    begin
      Inc(CommandLine);
      L := WideStrLen(CommandLine);
      P := WideStrScan(CommandLine, CoreChar('"'), L);
      if P <> nil then
        L := P - CommandLine;
      Result.Quoted := True;
    end
    else
    begin
      P := CommandLine;
      while (P^ <> WideChar(32)) and (P^ <> WideChar(9)) and (P^ <> WideChar(0)) do
        Inc(P);
      L := P - CommandLine;
      Result.Quoted := False;
    end;
    with Result do
    begin
      NextParam := CommandLine + L + Byte(Quoted);
      if NextParam^ <> WideChar(0) then
        Inc(NextParam);
      Param := CommandLine;
      Length := L;
    end;
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): PCoreChar;
var
  L: Integer;
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
  Buf: PLegacyChar): Integer;
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
  Buf: PWideChar): Integer;
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

function DecodeLegacy(Source: PLegacyChar; CodePage: Word): TWideStringRec;
begin
  Result := DecodeLegacy(Source, StrLen(Source), CodePage);
end;

function DecodeLegacy(Source: PLegacyChar; Count: Integer; CodePage: Word): TWideStringRec;
begin
  with Result do
  begin
    Length := {$IFDEF Tricks} System. {$ENDIF}
      MultiByteToWideChar(CodePage, 0, Source, Count, nil, 0);
    if Length <> 0 then
    begin
      GetMem(Value, (Length + 1) * SizeOf(WideChar));
    {$IFDEF Tricks} System. {$ENDIF}
      MultiByteToWideChar(CodePage, 0, Source, Count, Value, Length);
      Value[Length] := WideChar(0);
    end
    else
      Value := nil;
  end;
end;

function EncodeLegacy(Source: PWideChar; CodePage: Word{; UseDefaultChar: Boolean}): TLegacyStringRec;
begin
  Result := EncodeLegacy(Source, WideStrLen(Source), CodePage{, UseDefaultChar});
end;

function EncodeLegacy(Source: PWideChar; Count: Integer; CodePage: Word{;
  UseDefaultChar: Boolean}): TLegacyStringRec;
var
  DefaultCharUsed: Bool;
begin
  with Result do
  begin
    Length := {$IFDEF Tricks} System. {$ENDIF}
      WideCharToMultiByte(CodePage, 0, Source, Count, nil, 0, nil, @DefaultCharUsed);
    if (Length <> 0) and ({not UseDefaultChar or} not DefaultCharUsed) then
    begin
      GetMem(Value, Length + 1);
    {$IFDEF Tricks} System. {$ENDIF}
      WideCharToMultiByte(CodePage, 0, Source, Count, Value, Length, nil, nil);
      Value[Length] := #0;
    end
    else
      Value := nil;
  end;
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

function LegacyFormat(Fmt: PLegacyChar; CodePage: Word; const Args: array of const): PWideChar;
var
  W: PWideChar;
begin
  W := DecodeLegacy(Fmt, CodePage).Value;
  try
    Result := WideFormat(W, Args);
  finally
    FreeMem(W);
  end;
end;

function FriendlyClassName(var Dest: TClassName; Source: TClass): Byte;
var
  P: PLegacyChar;
begin
  P := PPointer(PLegacyChar(Source) + vmtClassName)^;
  Result := PByte(P)^; // Length(P^);
  Inc(P);
  if (Result > 1) and (P^ in ['T', 't']) then
  begin
    Inc(P);
    Dec(Result);
  end;
  Move(P^, Dest, Result);
  Dest[Result] := #0;
end;

function FriendlyClassName(var Dest: TClassName; Source: TObject): Byte;
begin
  if Source <> nil then
    Result := FriendlyClassName(Dest, TClass(PPointer(Source)^))
  else
  begin // Fast core
    PLongWord(@Dest)^ := $6C6C756E; // 'null'
    Dest[4] := #0;
    Result := SizeOf(LongWord);
  end;
end;

end.
