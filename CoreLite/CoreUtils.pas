(*
    Lite Core Library (CoreLite mini)

    Typecast and platform-based non-OOP utilites

    Copyright (c) 2007-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * CoreLiteVCL -- the same as Debug, but also partial use of SysUtils
      * Debug -- allow ShortString, AnsiString, WideString and UnicodeString
                 at EstimateArgs
*)

unit CoreUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, CoreConsts;

const
  UnicodeRTL = RTLVersion >= 20.0;

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

  QuadWord  = {$IF RTLVersion >= 15} {type} UInt64 {$ELSE} Int64 {$IFEND};
  PQuadWord = {$IF RTLVersion >= 15} {type} ^UInt64 {$ELSE} PInt64 {$IFEND};
{$ENDIF}

  QuadInt   = Int64;

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

  CoreChar      = WideChar;     // TODO: non-Unicode
  PCoreChar     = PWideChar;
  PPCoreChar    = PPWideChar;

  CharCode      = Word;         // TODO: non-Unicode
  PCharCode     = PWord;

  PAddress      = PLegacyChar;  // for address arithmetic

  TLegacyStringRec = record
    case Byte of
      0: (Value: PLegacyChar;
          Length: Integer);
      1: (Handle: THandle);
  end;

  TWideStringRec = record
    case Byte of
      0: (Value: PWideChar;
          Length: Integer);
      1: (Handle: THandle);
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
  LegacyReplacementChar = #127;

  CRLF: array[0..1] of LegacyChar = #13#10;
  WideCRLF: array[0..1] of WideChar = (WideChar(13), WideChar(10));

  LF: array[0..1] of LegacyChar = #10#0;
  WideLF: array[0..1] of WideChar = (#10, #0);
  WhitespaceOrLineBreak: array[Boolean] of LegacyChar = (#32, #10);
{$IFNDEF Tricks}
  HexDigits: array [$0..$F] of LegacyChar = '0123456789ABCDEF';
var
  MainWindow: THandle;
{$IF not UnicodeRTL}
  DefaultSystemCodePage: Word = CP_LOCALIZATION;
{$IFEND}  
{$ENDIF}

{$IF RTLVersion < 15}
const
  CP_THREAD_ACP = 3;
{$IFEND}

{$IF defined(CoreLiteVCL) and not UnicodeRTL}
var
  DefaultUnicodeCodePage: Word {$IFNDEF Lite} = CP_THREAD_ACP {$ENDIF};
{$IFEND}

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

function AllocMem(Count: CoreInt; FillingByte: Byte = 0): Pointer;
procedure FreeMemAndNil(var P);
// procedure FreeAndNil(var Obj); --> moved to CoreClasses

procedure Exchange(var P1, P2: Pointer); overload;
procedure Exchange(var P1, P2: QuadWord); overload;

function CompareMem(P1, P2: Pointer; Count: Integer): Boolean;
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

  DecimalSingle   = 10;
  DecimalDouble   = 18;
  DecimalExtended = 22;
  DecimalCurrency = DecimalExtended;

function EstimateArgs(const Args: array of const): Integer;

{$IFNDEF CoreLiteVCL}
function StrAlloc(Length: Integer): PLegacyChar;
function WideStrAlloc(Length: Integer): PWideChar;
{$ENDIF}
function QuadStrAlloc(Length: Integer): PQuadChar;

{$IFNDEF CoreLiteVCL}
procedure StrCopy(Dest, Source: PLegacyChar); overload;
{$ENDIF}
procedure StrCopy(Dest, Source: PLegacyChar; Length: Integer); overload;
procedure WideStrCopy(Dest, Source: PWideChar); overload;
procedure WideStrCopy(Dest, Source: PWideChar; Length: Integer); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar); overload;
procedure QuadStrCopy(Dest, Source: PQuadChar; Length: Integer); overload;

function StrLen(Str: PLegacyChar): Integer; overload; {$IFDEF UnicodeRTL} inline; {$ENDIF}
function StrLen(Str: PLegacyChar; MaxLength: Integer): Integer; overload;
function WideStrLen(Str: PWideChar): Integer; overload;
function WideStrLen(Str: PWideChar; MaxLength: Integer): Integer; overload;
function QuadStrLen(Str: PQuadChar): Integer; overload;
function QuadStrLen(Str: PQuadChar; MaxLength: Integer): Integer; overload;

{$IFNDEF CoreLiteVCL}
function StrNew(Str: PLegacyChar): PLegacyChar; overload;
{$ENDIF}
function StrNew(Str: PLegacyChar; Length: Integer): PLegacyChar; overload;
function WideStrNew(Str: PWideChar): PWideChar; overload;
function WideStrNew(Str: PWideChar; Length: Integer): PWideChar; overload;
function QuadStrNew(Str: PQuadChar): PQuadChar; overload;
function QuadStrNew(Str: PQuadChar; Length: Integer): PQuadChar; overload;

function StrScan(Where: PLegacyChar; Count: Integer; What: LegacyChar): PLegacyChar;
function WideStrScan(Where: PWideChar; Count: Integer; What: WideChar): PWideChar;
function QuadStrScan(Where: PQuadChar; Count: Integer; What: QuadChar): PQuadChar;

function StrRScan(Where: PLegacyChar; Count: Integer; What: LegacyChar): PLegacyChar;
function WideStrRScan(Where: PWideChar; Count: Integer; What: WideChar): PWideChar;
function QuadStrRScan(Where: PQuadChar; Count: Integer; What: QuadChar): PQuadChar;

{function StrPos(Where: PLegacyChar; WhereCount: Integer; What: PLegacyChar;
  WhatCount: Integer): PLegacyChar;
function StrRPos(Where: PLegacyChar; WhereCount: Integer; What: PLegacyChar;
  WhatCount: Integer): PLegacyChar;}

procedure SwapWideCharBytes(Source, Dest: PWideChar; Count: Integer);
procedure SwapQuadCharBytes(Source, Dest: PQuadChar; Count: Integer);

function StrComp(Str1: PLegacyChar; Count1: Integer; Str2: PLegacyChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;
function WideStrComp(Str1: PWideChar; Count1: Integer; Str2: PWideChar; Count2: Integer;
  IgnoreFlags: LongWord = NORM_IGNORECASE; Locale: LongWord = LOCALE_USER_DEFAULT): Integer;

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): TCoreStringRec;

{ Legacy Windows service }

const
  CP_GB18030 = 54936;

{$IF not UnicodeRTL}
  CSTR_LESS_THAN    = 1;
  CSTR_EQUAL        = 2;
  CSTR_GREATER_THAN = 3;

  MB_ERR_INVALID_CHARS = 8;
  WC_NO_BEST_FIT_CHARS = $400;

type
  TCPInfoEx = packed record
    MaxCharSize: LongWord;
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of LegacyChar;
    LeadByte: array[0..MAX_LEADBYTES - 1] of Byte;
    UnicodeDefaultChar: WideChar;
    CodePage: LongWord;
    CodePageName: array[0..MAX_PATH - 1] of CoreChar;
  end;

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
{$IFEND}

function FormatBuf(Fmt: PLegacyChar; const Args: array of const;
  Buf: PLegacyChar): Integer;
function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Integer;

{ MaxCharBytes(5022x, 52936) = 0, e. g. estimate on each string }
function MaxCharBytes(CodePage: Word; SurrogatePairs: Boolean = True): Byte;
function TranslateCodePage(Source: Word): Word;

type
  TModuleFileName = record
    Value: array[0..$7FF] of CoreChar;
    Length, FileNameIndex: Integer;
  end;

function ModuleFileName(Handle: THandle = 0): TModuleFileName;

{ FreeMem finalization required }

function DecodeUTF16(Source: PWideChar; SurrogatePairs: Boolean; CodePage: Word; 
  ReplacementChar: LegacyChar = LegacyReplacementChar): TLegacyStringRec; overload;
function DecodeUTF16(Source: PWideChar; Count: Integer; SurrogatePairs: Boolean;
  CodePage: Word; ReplacementChar: LegacyChar = LegacyReplacementChar): TLegacyStringRec; overload;

function EncodeUTF16(Source: PLegacyChar; CodePage: Word;
  ReplaceInvalidChars: Boolean = True): TWideStringRec; overload;
function EncodeUTF16(Source: PLegacyChar; Count: Integer; CodePage: Word;
  ReplaceInvalidChars: Boolean = True): TWideStringRec; overload;

function Format(Fmt: PLegacyChar; FixedWidth: Integer;
  const Args: array of const): TLegacyStringRec; overload;
function Format(Fmt: PLegacyChar; CodePage: Word; FixedWidth: Integer;
  const Args: array of const): TWideStringRec; overload;

function WideFormat(Fmt: PWideChar; FixedWidth: Integer;
  const Args: array of const): TWideStringRec;

{ User-friendly class names }

type
  // only the first 255 characters are significant (c) Delphi Help
  TClassName = array [0..256] of LegacyChar; // including null terminator

function FriendlyClassName(var Dest: TClassName; Source: TClass): Byte; overload;
function FriendlyClassName(var Dest: TClassName; Source: TObject): Byte; overload;

{ Math functions }

function Ceil(const X: Extended): CoreInt;
function Floor(const X: Extended): CoreInt;

function Log10(const X: Extended): Extended;
function Log2(const X: Extended): Extended;
function LogN(const Base, X: Extended): Extended;

implementation

{$IFDEF CoreLiteVCL}
uses SysUtils;
{$ENDIF}

{ Memory service }

function AllocMem(Count: CoreInt; FillingByte: Byte): Pointer;
begin
  GetMem(Result, Count);
  FillChar(Result^, Count, FillingByte);
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

procedure Exchange(var P1, P2: QuadWord); overload;
asm
        MOV ECX, EDX
        MOV EDX, [EAX + 2]
        MOV EAX, [EAX]
   LOCK CMPXCHG8B [ECX]
end;

function CompareMem(P1, P2: Pointer; Count: Integer): Boolean;
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
asm
end;
{$ENDIF}
{$I FastCode\CompareMem.inc}

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
        Inc(Result, CoreUtils.StrLen(TVarRec(Args[I]).VPChar));
      vtPWideChar:
        Inc(Result, WideStrLen(TVarRec(Args[I]).VPWideChar));
      vtPointer:
        Inc(Result, SizeOf(Pointer) * 2); // 2 hex digit per byte
      vtChar, vtWideChar:
        Inc(Result);
      vtExtended, vtCurrency:
        Inc(Result, DecimalExtended);
    {$IF defined(Debug) or defined(CoreLiteVCL)}
      vtString:
        Inc(Result, PByte(TVarRec(Args[I]).VString)^);
      {$IFDEF UnicodeRTL} vtUnicodeString, {$ENDIF}
      vtAnsiString:
        Inc(Result, PInteger(PAddress(TVarRec(Args[I]).VAnsiString) - SizeOf(Integer))^);
      vtWideString:
        Inc(Result, PLongInt(PAddress(TVarRec(Args[I]).VWideString) - SizeOf(LongInt))^ div SizeOf(WideChar));
    {$IFEND}
    end;
end;

{$IFNDEF CoreLiteVCL}
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
{$ENDIF}

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

{$IFNDEF CoreLiteVCL}
procedure StrCopy(Dest, Source: PLegacyChar);
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
asm
end;
{$ENDIF}
{$I FastCode\StrCopy.inc}
{$ENDIF}

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
{$IF defined(CoreLiteVCL) and UnicodeRTL}
begin
  Result := SysUtils.StrLen(Str);
end;
{$ELSE}
{$I FastCode\StrLen.inc}
{$IFEND}

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

{$IFNDEF CoreLiteVCL}
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
{$ENDIF}

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

function StrScan(Where: PLegacyChar; Count: Integer; What: LegacyChar): PLegacyChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
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

function WideStrScan(Where: PWideChar; Count: Integer; What: WideChar): PWideChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
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

function QuadStrScan(Where: PQuadChar; Count: Integer; What: QuadChar): PQuadChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
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

function StrRScan(Where: PLegacyChar; Count: Integer; What: LegacyChar): PLegacyChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
        ADD EAX, EDX

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

function WideStrRScan(Where: PWideChar; Count: Integer; What: WideChar): PWideChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
        SHL ECX, 1
        ADD EAX, ECX
        SHR ECX, 1

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

function QuadStrRScan(Where: PQuadChar; Count: Integer; What: QuadChar): PQuadChar;
asm
        TEST EDX, EDX
        JZ @@null
        XCHG ECX, EDX
        SHL ECX, 2
        ADD EAX, ECX
        SHR ECX, 2

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

{function StrPos(Where: PLegacyChar; WhereCount: Integer; What: PLegacyChar;
  WhatCount: Integer): PLegacyChar;
asm
        TEST EDX, EDX
        JZ @@nowhere

        PUSH EDI
        MOV EDI, EAX

        MOV EAX, WhatCount
        TEST EAX, EAX
        JZ @@nowhat

        ADD EDI, EDX
        PUSH ESI
        MOV ESI, ECX

        SHR EAX, 2
        JZ @@tail

        MOV ECX, EDX
        SHR ECX, 2
        JZ @@tail

@@next:
        MOV EAX, [ESI]
        STD
        REPNE SCASD
        CLD
        JNE @@notfound

        ADD EDI, SizeOf(LongWord)
        MOV EDX, EDI

        MOV ECX, WhatCount
        SHR ECX, 2
        MOV EAX, ESI
        REPE CMPSD
        MOV ESI, EAX
        JNE @@notfound

        MOVZX ECX, byte ptr WhatCount
        AND CL, $03
        REPE CMPSB
        JE @@found

        MOV EDI, EDX
        JMP @@next

@@found:
        MOV EAX, EDX
        JMP @@final

@@tail:
        STD
        REPNE SCASB
        CLD
        JE @@found

@@notfound:
        XOR EAX, EAX
@@final:
        POP ESI
@@nowhat:
        POP EDI
        JMP @@end
@@nowhere:
        XOR EAX, EAX
@@end:
end;

function StrRPos(Where: PLegacyChar; WhereCount: Integer; What: PLegacyChar;
  WhatCount: Integer): PLegacyChar;
asm
        TEST EDX, EDX
        JZ @@nowhere

        PUSH EDI
        MOV EDI, EAX

        MOV EAX, WhatCount
        TEST EAX, EAX
        JZ @@nowhat

        ADD EDI, EDX
        PUSH ESI
        MOV ESI, ECX

        SHR EAX, 2
        JZ @@tail

        MOV ECX, EDX
        SHR ECX, 2
        JZ @@tail

@@next:
        MOV EAX, [ESI]
        STD
        REPNE SCASD
        CLD
        JNE @@notfound

        ADD EDI, SizeOf(LongWord)
        MOV EDX, EDI

        MOV ECX, WhatCount
        SHR ECX, 2
        MOV EAX, ESI
        REPE CMPSD
        MOV ESI, EAX
        JNE @@notfound

        MOVZX ECX, byte ptr WhatCount
        AND CL, $03
        REPE CMPSB
        JE @@found

        MOV EDI, EDX
        JMP @@next

@@found:
        MOV EAX, EDX
        JMP @@final

@@tail:
        STD
        REPNE SCASB
        CLD
        JE @@found

@@notfound:
        XOR EAX, EAX
@@final:
        POP ESI
@@nowhat:
        POP EDI
        JMP @@end
@@nowhere:
        XOR EAX, EAX
@@end:
end;}

procedure SwapWideCharBytes(Source, Dest: PWideChar; Count: Integer);
asm
        TEST ECX, ECX
        JZ @@exit

        PUSH EBX

        PUSH ECX
        SHR ECX, 1
@@repeat2:
        MOV EBX, [EAX]
        BSWAP EBX
        ROL EBX, 16
        MOV [EDX], EBX
        LEA EAX, [EAX+4]
        LEA EDX, [EDX+4]
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

procedure SwapQuadCharBytes(Source, Dest: PQuadChar; Count: Integer);
asm
        TEST ECX, ECX
        JZ @@exit

        PUSH EBX

@@repeat:
        MOV EBX, [EAX]
        BSWAP EBX
        MOV [EDX], EBX
        LEA EAX, [EAX+4]
        LEA EDX, [EDX+4]
        LOOP @@repeat

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

{ LocalFree finalization required }

function SysErrorMessage(ErrorCode: LongWord): TCoreStringRec;
begin
  with Result do
  begin
    Length := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER,
      nil, ErrorCode, 0, Pointer(@Result.Value), 0, nil);
    while (Length <> 0) and
      ((Value[Length] >= CoreChar(0)) and (Value[Length] <= CoreChar(32)) or
       (Value[Length] = CoreChar('.')))
    do
      Dec(Length);
    if Length <> 0 then
      Value[Length + 1] := CoreChar(0);
  end;
end;

{ Legacy Windows service }

{$IF not UnicodeRTL}
function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';
{$IFEND}

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

function MaxCharBytes(CodePage: Word; SurrogatePairs: Boolean): Byte;
begin
  CodePage := TranslateCodePage(CodePage);
  if
    (CodePage - 900 in [0..99]) or      // ANSI/OEM CJK
    (CodePage - 1300 in [0..99]) or     // Johab
    (CodePage - 10000 in [1..5, 8]) or  // X-Mac
    (CodePage - 20000 in [1..9]) or     // X-CP, CNS, Eten
    (CodePage - 20900 in [0..99]) or    // EUC-JP, GB2312, Wansung
    (CodePage - 50900 in [30..50])      // EBCDIC, EUC
  then
    Result := 2
  else if CodePage = CP_GB18030 then
    Result := 2 * Byte(SurrogatePairs)
  else if CodePage = CP_UTF8 then
    Result := 3 + Byte(SurrogatePairs)
  else if CodePage = CP_UTF7 then
    Result := 5 + Byte(SurrogatePairs) * 2
  else if
    (CodePage - 50220 in [0..9]) or     // ISO-2022
    (CodePage = 52936)                  // HZ-GB2312
  then
    Result := 0
  else
    Result := 1;
end;

function TranslateCodePage(Source: Word): Word;
{$IFNDEF Lite}
var Info: TCPInfoEx;
{$ENDIF}
begin
  case Source of
    CP_ACP:
      begin
        Result := GetACP;
        Exit;
      end;
    CP_OEMCP:
      begin
        Result := GetOEMCP;
        Exit;
      end;
  {$IFNDEF Lite}
    CP_THREAD_ACP, CP_MACCP:
      if GetCPInfoEx(Source, 0, Info) then
      begin
        Result := Info.CodePage;
        Exit;
      end;
  {$ENDIF}
  end;
  Result := Source;
end;

function ModuleFileName(Handle: THandle): TModuleFileName;
var
  W: PWideChar;
begin
  with Result do
  begin
    Length := GetModuleFileNameW(Handle, Value, System.Length(Value));
    if Length <> 0 then
    begin
      W := WideStrRScan(Value, Length, PathDelimiter);
      if W <> nil then
        FileNameIndex := W - Value + 1
      else
        FileNameIndex := 0;
    end;
  end;
end;

{ FreeMem finalization required }

function DecodeUTF16(Source: PWideChar; SurrogatePairs: Boolean; CodePage: Word; 
  ReplacementChar: LegacyChar): TLegacyStringRec;
begin
  Result := DecodeUTF16(Source, WideStrLen(Source), SurrogatePairs, CodePage, ReplacementChar);
end;

function DecodeUTF16(Source: PWideChar; Count: Integer; SurrogatePairs: Boolean;
  CodePage: Word; ReplacementChar: LegacyChar): TLegacyStringRec;
var
  Flags: LongWord;
  Replacement: PLegacyChar;
begin
  if CodePage - CP_UTF7 in [0..1] then
  begin
    Flags := 0;
    Replacement := nil;
  end
  else
  begin
    Flags := WC_NO_BEST_FIT_CHARS;
    Replacement := @ReplacementChar;
  end;

  with Result do
  begin
    Length := MaxCharBytes(CodePage, SurrogatePairs);
    if Length <> 0 then
      Length := Count * Length
    else
    begin
      Length := {$IFDEF Tricks} System. {$ENDIF}
        WideCharToMultiByte(CodePage, WC_NO_BEST_FIT_CHARS, Source, Count, nil, 0, nil, nil);
      Replacement := nil;
    end;
    GetMem(Value, Length + 1);
    Length := {$IFDEF Tricks} System. {$ENDIF}
      WideCharToMultiByte(CodePage, Flags, Source, Count, Value, Length, Replacement, nil);
    Value[Length] := #0;
  end;
end;

function EncodeUTF16(Source: PLegacyChar; CodePage: Word;
  ReplaceInvalidChars: Boolean): TWideStringRec;
begin
  Result := EncodeUTF16(Source, CoreUtils.StrLen(Source), CodePage);
end;

function EncodeUTF16(Source: PLegacyChar; Count: Integer; CodePage: Word;
  ReplaceInvalidChars: Boolean): TWideStringRec;
begin
  with Result do
  begin
    Length := Count; // because it real for all code pages
    GetMem(Value, (Length + 1) * SizeOf(WideChar));
    {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(CodePage,
      MB_ERR_INVALID_CHARS and (Integer(ReplaceInvalidChars) - 1), Source, Count, Value, Length);
    Value[Length] := #0;
  end;
end;

function Format(Fmt: PLegacyChar; FixedWidth: Integer;
  const Args: array of const): TLegacyStringRec;
begin
  with Result do
  begin
    GetMem(Value, CoreUtils.StrLen(Fmt) + FixedWidth + EstimateArgs(Args) + 1);
    Length := FormatBuf(Fmt, Args, Value);
  end;
end;

function Format(Fmt: PLegacyChar; CodePage: Word; FixedWidth: Integer;
  const Args: array of const): TWideStringRec;
var
  W: PWideChar;
begin
  W := EncodeUTF16(Fmt, CodePage).Value;
  try
    Result := WideFormat(W, FixedWidth, Args);
  finally
    FreeMem(W);
  end;
end;

function WideFormat(Fmt: PWideChar; FixedWidth: Integer;
  const Args: array of const): TWideStringRec;
begin
  with Result do
  begin
    GetMem(Value, (WideStrLen(Fmt) + FixedWidth + EstimateArgs(Args) + 1) * SizeOf(WideChar));
    Length := WideFormatBuf(Fmt, Args, Value);
  end;
end;

function FriendlyClassName(var Dest: TClassName; Source: TClass): Byte;
var
  P: PLegacyChar;
begin
  try
    P := PPointer(PAddress(Source) + vmtClassName)^;
    Result := PByte(P)^; // Length(P^);
    Inc(P);
    if (Result > 1) and (P^ in ['T', 't']) then
    begin
      Inc(P);
      Dec(Result);
    end;
  except
    P := sInlineObject;
    Result := CoreUtils.StrLen(P);
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
    Dest[SizeOf(LongWord)] := #0;
    Result := SizeOf(LongWord);
  end;
end;

{ Math functions }

function Ceil(const X: Extended): CoreInt;
begin
  Result := Trunc(X);
  if Frac(X) > 0 then
    Inc(Result);
end;

function Floor(const X: Extended): CoreInt;
begin
  Result := Trunc(X);
  if Frac(X) < 0 then
    Dec(Result);
end;

function Log10(const X: Extended): Extended;
// Log.10(X) := Log.2(X) * Log.10(2)
asm
        FLDLG2
        FLD X
        FYL2X
        FWAIT
end;

function Log2(const X: Extended): Extended;
asm
        FLD1
        FLD X
        FYL2X
        FWAIT
end;

function LogN(const Base, X: Extended): Extended;
// Log.N(X) := Log.2(X) / Log.2(N)
asm
        FLD1
        FLD X
        FYL2X
        FLD1
        FLD Base
        FYL2X
        FDIV
        FWAIT
end;

initialization
{$IF defined(Tricks) or UnicodeRTL}
  DefaultSystemCodePage := CP_LOCALIZATION;
{$IFEND}

end.

