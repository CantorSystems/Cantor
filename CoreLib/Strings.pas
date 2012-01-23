(*
    The Unified Environment Core Library

    Core string, character set, code page, and MIME encoding implementation

    Copyright (c) 2007-2009 The Unified Environment Laboratory

    TODO:
      * Unicode:
        * Complete UTF-7 support
        * Composition/descomposition
        * Use of control and glyph characters
        * SCSU and BOCU-1 encodings
      * Non-Unicode:
        * ISO-2022
        * GB18030
        * TRON?

    NOTE:  CTRL_SHIFT_UP_CTRL_SHIFT_DOWN is not regular define,
           just workaround for the named IDE option can work properly
*)

unit Strings;

interface

uses
  Windows, Core, Containers;

{$I CodePage.inc}
{$I Unicode.inc}

const
  DecimalLongInt  = 11;
  DecimalQuadInt  = 22;
  DecimalInt      = DecimalLongInt; // TODO: x64

  HexLongInt      = 8;
  HexQuadInt      = 16;

  HexInt          = HexLongInt; // TODO: x64

  HexPointer      = HexInt; // both x86 and x64

  DecimalFloat    = 18;
  DecimalExtended = 22;
  DecimalCurrency = DecimalExtended;

type
  PUTF16Surrogates = ^UTF16Surrogates;
  UTF16Surrogates = packed record
    Hi, Lo: WideChar; // fixed word order, always big-endian
  end;

  PUTF16Char = ^UTF16Char;
  UTF16Char = packed record
    case Word of
      0: (BasicChar: WideChar);
      1: (Surrogates: UTF16Surrogates);
  end;

const
  ASCII_MaxCharBytes = SizeOf(LegacyChar);
  UTF7_MaxCharBytes  = 7;
  UTF8_MaxCharBytes  = 4;
  UTF16_MaxCharBytes = SizeOf(UTF16Char);
  UTF32_MaxCharBytes = SizeOf(QuadChar);

type
  // CESU-8: encode surrogates as is
  // Modified UTF-8: encode #0 as ($C0, $80)
  TUTF8Compliance = (cpRegular, cpCESU8, cpModified);

  TCharInfo = record
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount, SingleByteCount, MultiByteCount: Cardinal);
      2: (Reserved, Count, SurrogateCount, NonSpacingCount, NonUnicodeCount: Cardinal;
          UTF7DirectChars: set of #$00..#$7F;
          UTF8Bytes: Cardinal;
          UTF8Compliance: TUTF8Compliance;
          Blocks: TCharBlocks);
  end;

  PUnicodeStrInfo = ^TUnicodeStrInfo;
  TUnicodeStrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount: Cardinal);
  end;

  PUTF32StrInfo = ^TUTF32StrInfo;
  TUTF32StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount, NonUnicodeCount: Cardinal);
  end;

  PUTF16StrInfo = ^TUTF16StrInfo;
  TUTF16StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount, SurrogateCount: Cardinal);
  end;

  PUTF7StrInfo = ^TUTF7StrInfo;
  TUTF7StrInfo = record
    ByteCount: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount, SurrogateCount, UnicodeCount: Cardinal;
          DirectChars: set of #$00..#$7F);
  end;

  TUTF8Sequences = array[1..6] of Cardinal;

  PUTF8StrInfo = ^TUTF8StrInfo;
  TUTF8StrInfo = record
    ByteCount: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer; SurrogateCount: Cardinal);
      1: (InvalidCount, NonUnicodeCount, UnicodeCount: Cardinal;
          Compliance: TUTF8Compliance;
          Sequences: TUTF8Sequences);
  end;

  PNonUnicodeStrInfo = ^TNonUnicodeStrInfo;
  TNonUnicodeStrInfo = record
    Count, MultiByteCount: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount: Cardinal);
  end;

  // TODO: MaxCharBytes service

  TStringInfoOption = (siBigEndian, siDecomposed, //uoGlyphChars,
    siForceInvalid, siRangeBlocks, siRangeDirectChars);

  TCharInfoOption = siBigEndian..siRangeBlocks;

  TUTF7Options = set of siDecomposed..siRangeDirectChars;
  TUTF8Options = set of siDecomposed..siRangeBlocks;
  TEndianUTFOptions = set of siBigEndian..siRangeBlocks;
  TUTF16Options = TEndianUTFOptions;
  TUTF32Options = TEndianUTFOptions;
  TNonUnicodeOptions = set of siForceInvalid..siRangeBlocks; // TODO: Remove
  TUTFOptions = type TUTF8Options;                           // TODO: Remove

  TCharDecoder = class(TSharedObject)
  private
    //FSource: Pointer;
    //FCount: Cardinal;
    //FOptions: TEndianUTFOptions;
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar;
      var Info: TCharInfo): Boolean; virtual; abstract;
  public
    class function ByteCount(Length: Cardinal): Cardinal; overload; virtual;
    class function ByteCount(Str: Pointer): Cardinal; overload;

    function CharInfo: TCharInfo; overload;
    function CharInfo(var Info: TCharInfo): Cardinal; overload;

    function EncodeASCII(Dest: PLegacyChar): Cardinal; virtual;
    function EncodeASCIIEx(Dest: PLegacyChar): TCharInfo; overload;
    function EncodeASCIIEx(var Info: TCharInfo; Dest: PLegacyChar): Cardinal; overload; virtual;

{    function EncodeUTF7(Dest: PLegacyChar): Cardinal;
    function EncodeUTF7Ex(Dest: PLegacyChar): TCharInfo; overload;
    function EncodeUTF7Ex(var Info: TCharInfo; Dest: PLegacyChar): Cardinal; overload;}

    function EncodeUTF8(Dest: PLegacyChar; Compliance: TUTF8Compliance = cpRegular): Cardinal; virtual;
    function EncodeUTF8Ex(Dest: PLegacyChar; Compliance: TUTF8Compliance = cpRegular): TCharInfo; overload;
    function EncodeUTF8Ex(var Info: TCharInfo; Dest: PLegacyChar;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; overload; virtual;

    function EncodeUTF16(Dest: PWideChar; BigEndian: Boolean = False): Cardinal; virtual;
    function EncodeUTF16Ex(Dest: PWideChar; BigEndian: Boolean = False): TCharInfo; overload;
    function EncodeUTF16Ex(var Info: TCharInfo; Dest: PWideChar;
      BigEndian: Boolean = False): Cardinal; overload; virtual;

    function EncodeUTF32(Dest: PQuadChar; BigEndian: Boolean = False): Cardinal; virtual;
    function EncodeUTF32Ex(Dest: PQuadChar; BigEndian: Boolean = False): TCharInfo; overload;
    function EncodeUTF32Ex(var Info: TCharInfo; Dest: PQuadChar;
      BigEndian: Boolean = False): Cardinal; overload; virtual;

    class function Length(Str: Pointer): Cardinal; virtual;

    procedure SetData(Source: Pointer); overload;
    procedure SetData(Source: Pointer; Count: Cardinal); overload;
  end;

  TUnicodeDecoder = class(TCharDecoder)
  end;

  TUTFDecoder = class(TUnicodeDecoder)
  private
    FSource: PLegacyChar;
    FCount: Cardinal;
    FOptions: TUTFOptions;
  public
  // properties
    property Count: Cardinal read FCount;
    property Options: TUTFOptions read FOptions write FOptions;
    property Source: PLegacyChar read FSource;
  end;

{  TUTF7Decoder = class(TUTFDecoder)
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar): Boolean; override;
  end;}

  TUTF8Decoder = class(TUTFDecoder)
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar;
      var Info: TCharInfo): Boolean; override;
  public
    function EncodeUTF16(Dest: PWideChar; BigEndian: Boolean = False): Cardinal; override;
  end;

  TUTF16Decoder = class(TUnicodeDecoder)
  private
    FSource: PWideChar;
    FCount: Cardinal;
    FOptions: TEndianUTFOptions;
    FBigEndian: Boolean;
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar;
      var Info: TCharInfo): Boolean; override;
  public
    class function ByteCount(Length: Cardinal): Cardinal; override;

    function EncodeUTF8(Dest: PLegacyChar; Compliance: TUTF8Compliance = cpRegular): Cardinal; override;

    class function Length(Str: Pointer): Cardinal; override;
  // properties
    property BigEndian: Boolean read FBigEndian write FBigEndian;
    property Count: Cardinal read FCount;
    property Options: TEndianUTFOptions read FOptions write FOptions;
    property Source: PWideChar read FSource;
  end;

  TUTF32Decoder = class(TUnicodeDecoder)
  private
    FSource: PQuadChar;
    FCount: Cardinal;
    FOptions: TEndianUTFOptions;
    FBigEndian: Boolean;
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar;
      var Info: TCharInfo): Boolean; override;
  public
    class function ByteCount(Length: Cardinal): Cardinal; override;
    class function Length(Str: Pointer): Cardinal; override;
  // properties
    property BigEndian: Boolean read FBigEndian write FBigEndian;
    property Count: Cardinal read FCount;
    property Options: TEndianUTFOptions read FOptions write FOptions;
    property Source: PQuadChar read FSource;
  end;

  TNonUnicodeDecoder = class(TCharDecoder)
  private
    FSource: PLegacyChar;
    FCount: Cardinal;
    FOptions: TNonUnicodeOptions;
  public
  // properties
    property Count: Cardinal read FCount;
    property Options: TNonUnicodeOptions read FOptions write FOptions;
    property Source: PLegacyChar read FSource;
  end;

  TASCIIDecoder = class(TNonUnicodeDecoder)
  protected
    function NextChar(var Index: Cardinal; var Char: QuadChar;
      var Info: TCharInfo): Boolean; override;
  end;

{ Common service }

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

{ Unicode service }

{function ComposeUTF16(Source: QuadChar): Cardinal;
function ComposeUTF16LE(Source: QuadChar; Count: Cardinal; Dest: PWideChar): Cardinal;
function ComposeUTF16BE(Source: QuadChar; Count: Cardinal; Dest: PWideChar): Cardinal;

function ComposeUTF32(Source: QuadChar): Cardinal; overload;
function ComposeUTF32(Source: QuadChar; Count: Cardinal; Dest: PWideChar): Cardinal; overload;

function Decompose(Source: PWideChar; Count: Cardinal): Cardinal; overload;
function Decompose(Source: PWideChar; Count: Cardinal; Dest: PWideChar): Cardinal; overload;
function Decompose(Source: PQuadChar; Count: Cardinal): Cardinal; overload;
function Decompose(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar): Cardinal; overload;}

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbUnknown): TCharBlock;

{ Legacy Windows service }

function FormatBuf(Fmt: PLegacyChar; const Args: array of const;
  Buf: PLegacyChar): Cardinal;
function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Cardinal;

{ Missing in Windows.pas }

const
  MB_PRECOMPOSED        = $00000001;  // use precomposed chars
  MB_COMPOSITE          = $00000002;  // use composite chars
  MB_USEGLYPHCHARS      = $00000004;  // use glyph chars, not ctrl chars
  MB_ERR_INVALID_CHARS  = $00000008;  // error for invalid chars

  WC_COMPOSITECHECK     = $00000200;  // convert composite to precomposed
  WC_DISCARDNS          = $00000010;  // discard non-spacing chars
  WC_SEPCHARS           = $00000020;  // generate separate chars
  WC_DEFAULTCHAR        = $00000040;  // replace with default char

type
  TCHAR   = CoreChar;
  LPCTSTR = PCoreChar;
  LPTSTR  = PCoreChar;

  TCPInfoEx = packed record
    MaxCharSize: UINT;
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of CHAR;
    LeadByte: array[0..MAX_LEADBYTES - 1] of BYTE;
    UnicodeDefaultChar: WCHAR;
    CodePage: UINT;
    CodePageName: array[0..MAX_PATH - 1] of TCHAR;
  end;

function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;

implementation

uses
  Exceptions;

{ Common service }

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
{$I Common\FastCode\StrCopy.inc}

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
{$I Common\FastCode\StrLen.inc}

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

{ Unicode service }

{function Compose(Source: PWideChar; Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

function Compose(Source: PWideChar; Count: Cardinal;
  Dest: PWideChar): Cardinal;
begin
  if Source <> Dest then
    Move(Source^, Dest^, Count * SizeOf(WideChar));
  Result := Count;
end;

function Compose(Source: PQuadChar; Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

function Compose(Source: PQuadChar; Count: Cardinal;
  Dest: PQuadChar): Cardinal;
begin
  if Source <> Dest then
    Move(Source^, Dest^, Count * SizeOf(QuadChar));
  Result := Count;
end;

function Decompose(Source: PWideChar; Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

function Decompose(Source: PWideChar; Count: Cardinal; Dest: PWideChar): Cardinal;
begin
  if Source <> Dest then
    Move(Source^, Dest^, Count * SizeOf(WideChar));
  Result := Count;
end;

function Decompose(Source: PQuadChar; Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

function Decompose(Source: PQuadChar; Count: Cardinal;
  Dest: PQuadChar): Cardinal;
begin
  if Source <> Dest then
    Move(Source^, Dest^, Count * SizeOf(QuadChar));
  Result := Count;
end;}

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

{ Missing in Windows.pas }

function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name {$IFDEF UNICODE} 'GetCPInfoExW' {$ELSE} 'GetCPInfoExA' {$ENDIF} ;

{ TCharDecoder }

class function TCharDecoder.ByteCount(Length: Cardinal): Cardinal;
begin
  Result := Length;
end;

class function TCharDecoder.ByteCount(Str: Pointer): Cardinal;
begin
  Result := ByteCount(Length(Str));
end;

function TCharDecoder.CharInfo: TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  CharInfo(Result);
end;

function TCharDecoder.CharInfo(var Info: TCharInfo): Cardinal;
var
  Block: TCharBlock;
  Q: QuadChar;
begin
  Block := cbUnknown;
  Result := 0;
  while Result < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Result, Q, Info) then
      Inc(Info.Count)
    else
      if siForceInvalid in TUTFDecoder(Self).Options then
      begin
        with Info do
        begin
          Inc(InvalidCount);
          Inc(UTF8Bytes, Unknown_UTF8_Bytes);
          Include(Blocks, cbUnknown);
        end;
        Continue;
      end
      else
      begin
        Info.InvalidChar := TUTFDecoder(Self).FSource + ByteCount(Result);
        Exit;
      end;

    case Q of
      $80..$7FF:
        Inc(Info.UTF8Bytes);
      $800..$FFFF:
        Inc(Info.UTF8Bytes, 2);
      $10000..$1FFFFF:
        Inc(Info.UTF8Bytes, 3);
      $200000..$3FFFFFF:
        Inc(Info.UTF8Bytes, 4);
      $4000000..$FFFFFFFF:
        Inc(Info.UTF8Bytes, 5);
    end;

    if Q > High(TUnicodeBMP) then
    begin
      Inc(Info.SurrogateCount);
      if Q >= Low(TNonUnicode) then
      begin
        with Info do
        begin
          Inc(NonUnicodeCount);
          Include(Blocks, cbNonUnicode);
        end;
        Continue;
      end;
    end;

    if siRangeBlocks in TUTFDecoder(Self).Options then
    begin
      Block := FindCharBlock(Q, Block);
      Include(Info.Blocks, Block);
    end;
  end;
end;

function TCharDecoder.EncodeASCII(Dest: PLegacyChar): Cardinal;
var
  Q: QuadChar;
  Idx: Cardinal;
  Info: TCharInfo;
begin
  Result := 0;
  Idx := 0;
  while Idx < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Idx, Q, Info) then
      case Q of
        $00..$7F:
          begin
            Dest[Result] := Char(Q);
            Inc(Result);
            Continue;
          end;
      end;
    if siForceInvalid in TUTFDecoder(Self).Options then
    begin
      Dest[Result] := Unknown_Latin;
      Inc(Result);
    end
    else
      raise Exception.Create; // TODO: cannot convert into ASCII
  end;
end;

function TCharDecoder.EncodeASCIIEx(Dest: PLegacyChar): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EncodeASCIIEx(Result, Dest);
end;

function TCharDecoder.EncodeASCIIEx(var Info: TCharInfo; Dest: PLegacyChar): Cardinal;
var
  Q: QuadChar;
  Inf: TCharInfo;
begin
  Result := 0;
  while Result < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Result, Q, Inf) then
      case Q of
        $00..$7F:
          with Info do
          begin
            Dest[Count + InvalidCount] := Char(Q);
            Inc(Count);
            Include(Blocks, cbBasicLatin);
            Continue;
          end;
      end;
    if siForceInvalid in TUTFDecoder(Self).Options then
      with Info do
      begin
        Dest[Count + InvalidCount] := Unknown_Latin;
        Inc(InvalidCount);
        Include(Blocks, cbUnknown);
      end
    else
    begin
      with Info do
      begin
        InvalidChar := TUTFDecoder(Self).FSource + ByteCount(Result);
        Include(Blocks, cbUnknown);
      end;
      Exit;
    end;
  end;
end;

{function TCharDecoder.EncodeUTF7(Dest: PLegacyChar): Cardinal;
begin
  Result := 0; // TODO
end;

function TCharDecoder.EncodeUTF7Ex(Dest: PLegacyChar): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF7Ex(Result, Dest);
end;

function TCharDecoder.EncodeUTF7Ex(var Info: TCharInfo; Dest: PLegacyChar): Cardinal;
begin
  Result := 0; // TODO
end;}

function TCharDecoder.EncodeUTF8(Dest: PLegacyChar; Compliance: TUTF8Compliance): Cardinal;
var
  Idx: Cardinal;
  Q, T: QuadChar;
  Inf: TCharInfo;
  P: PLegacyChar;
  H, L: Word;
begin
  Result := 0;
  Idx := 0;
  while Idx < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Idx, Q, Inf) then
    begin
      case Q of
        $0:
          if Compliance = cpModified then
          begin
            PWord(Dest + Result)^ := $80C0;
            Inc(Result, SizeOf(Word));
            Continue;
          end
          else
          begin
            Dest[Result] := #0;
            Inc(Result);
            Continue;
          end;
        $01..$7F:
          begin
            Dest[Result] := Char(Q);
            Inc(Result);
            Continue;
          end;
        $80..$7FF:
          begin
            P := Dest + Result;
            Inc(Result, SizeOf(Word));
            PWord(P)^ :=
              ($C0 or (Q shr 6)) or
              (($80 or (Q and $3F)) shl 8);
            Continue;
          end;
        $800..$FFFF:
          begin
            P := Dest + Result;
            Inc(Result, SizeOf(Word) + SizeOf(Byte));
            PWord(P)^ :=
              ($E0 or (Q shr 12)) or
              (($80 or ((Q shr 6) and $3F)) shl 8);
            Inc(P, SizeOf(Word));
            PByte(P)^ := $80 or (Q and $3F);
            Continue;
          end;
      else
        if Compliance = cpRegular then
          case Q of
            $10000..$1FFFFF:
              begin
                P := Dest + Result;
                Inc(Result, SizeOf(LongWord));
                PLongWord(P)^ :=
                  ($F0 or (Q shr 18)) or
                  (($80 or ((Q shr 12) and $3F)) shl 8) or
                  (($80 or ((Q shr 6) and $3F)) shl 16) or
                  (($80 or (Q and $3F)) shl 24);
                Continue;
              end;
            $200000..$3FFFFFF:
              begin
                P := Dest + Result;
                Inc(Result, SizeOf(LongWord) + SizeOf(Byte));
                PLongWord(P)^ :=
                  ($F8 or (Q shr 24)) or
                  (($80 or ((Q shr 18) and $3F)) shl 8) or
                  (($80 or ((Q shr 12) and $3F)) shl 16) or
                  (($80 or ((Q shr 6) and $3F)) shl 24);
                Inc(P, SizeOf(LongWord));
                PByte(P)^ := $80 or (Q and $3F);
                Continue;
              end;
            $4000000..$7FFFFFFF:
              begin
                P := Dest + Result;
                Inc(Result, SizeOf(LongWord) + SizeOf(Word));
                PLongWord(P)^ :=
                  ($FC or (Q shr 30)) or
                  (($80 or ((Q shr 24) and $3F)) shl 8) or
                  (($80 or ((Q shr 18) and $3F)) shl 16) or
                  (($80 or ((Q shr 12) and $3F)) shl 24);
                Inc(P, SizeOf(LongWord));
                PWord(P)^ :=
                  ($80 or (Q shr 6)) or
                  (($80 or (Q and $3F)) shl 8);
                Continue;
              end;
          end
        else
          case Q of
            Low(TUnicodeSMP)..High(TUnicodePUA):
              begin
                P := Dest + Result;
                Inc(Result, SizeOf(LongWord) + SizeOf(Word));
                T := Q - Low(TUnicodeSMP);
                H := Low(THighSurrogates) + T div $400;
                L := Low(TLowSurrogates) + T mod $400;
                PLongWord(P)^ :=
                  ($E0 or (H shr 12)) or
                  (($80 or ((H shr 6) and $3F)) shl 8) or
                  (($80 or (H and $3F)) shl 16) or
                  (($E0 or (L shr 12)) shl 24);
                Inc(P, SizeOf(LongWord));
                PWord(P)^ :=
                  ($80 or ((L shr 6) and $3F)) or
                  (($80 or (L and $3F)) shl 8);
                Continue;
              end;
          end;
      end;
    end;
    if siForceInvalid in TUTFDecoder(Self).Options then
    begin
      P := Dest + Result;
      Inc(Result, SizeOf(UnknownUTF8));
      PWord(P)^ := Unknown_UTF8 and High(Word);
      Inc(P, SizeOf(Word));
      PByte(P)^ := Unknown_UTF8 shr 16;
    end
    else
      raise Exception.Create; // TODO: cannot convert into UTF-8
  end;
end;

function TCharDecoder.EncodeUTF8Ex(Dest: PLegacyChar; Compliance: TUTF8Compliance): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF8Ex(Result, Dest, Compliance);
end;

function TCharDecoder.EncodeUTF8Ex(var Info: TCharInfo; Dest: PLegacyChar;
  Compliance: TUTF8Compliance): Cardinal;
var
  Block: TCharBlock;
  Q, T: QuadChar;
  Inf: TCharInfo;
  P: PLegacyChar;
  H, L: Word;
begin
  Block := cbUnknown;
  Result := 0;
  while Result < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Result, Q, Inf) then
    begin
      case Q of
        $0:
          if Compliance = cpModified then
            with Info do
            begin
              PWord(Dest + Count + InvalidCount + UTF8Bytes)^ := $80C0;
              Inc(Count);
              Inc(UTF8Bytes);
              UTF8Compliance := cpModified;
              Include(Blocks, cbBasicLatin);
              Continue;
            end
          else
            with Info do
            begin
              Dest[Count + InvalidCount + UTF8Bytes] := #0;
              Inc(Count);
              Include(Blocks, cbBasicLatin);
              Continue;
            end;
        $01..$7F:
          with Info do
          begin
            Dest[Count + InvalidCount + UTF8Bytes] := Char(Q);
            Inc(Count);
            Include(Blocks, cbLatin1Supplement);
            Continue;
          end;
        $80..$7FF:
          begin
            with Info do
            begin
              P := Dest + Count + InvalidCount + UTF8Bytes;
              Inc(Count);
              Inc(UTF8Bytes);
              if siRangeBlocks in TUTFDecoder(Self).Options then
              begin
                Block := FindCharBlock(Q, Block);
                Include(Blocks, Block);
              end;
            end;
            PWord(P)^ :=
              ($C0 or (Q shr 6)) or
              (($80 or (Q and $3F)) shl 8);
            Continue;
          end;
        $800..$FFFF:
          begin
            with Info do
            begin
              P := Dest + Count + InvalidCount + UTF8Bytes;
              Inc(Count);
              Inc(UTF8Bytes, 2);
              if siRangeBlocks in TUTFDecoder(Self).Options then
              begin
                Block := FindCharBlock(Q, Block);
                Include(Blocks, Block);
              end;
            end;
            PWord(P)^ :=
              ($E0 or (Q shr 12)) or
              (($80 or ((Q shr 6) and $3F)) shl 8);
            Inc(P, SizeOf(Word));
            PByte(P)^ := $80 or (Q and $3F);
            Continue;
          end;
      else
        if Compliance = cpRegular then
          case Q of
            $10000..$1FFFFF:
              begin
                with Info do
                begin
                  P := Dest + Count + InvalidCount + UTF8Bytes;
                  Inc(Count);
                  Inc(SurrogateCount);
                  Inc(UTF8Bytes, 3);
                  if siRangeBlocks in TUTFDecoder(Self).Options then
                  begin
                    Block := FindCharBlock(Q, Block);
                    Include(Blocks, Block);
                  end;
                end;
                PLongWord(P)^ :=
                  ($F0 or (Q shr 18)) or
                  (($80 or ((Q shr 12) and $3F)) shl 8) or
                  (($80 or ((Q shr 6) and $3F)) shl 16) or
                  (($80 or (Q and $3F)) shl 24);
                Continue;
              end;
            $200000..$3FFFFFF:
              begin
                with Info do
                begin
                  P := Dest + Count + InvalidCount + UTF8Bytes;
                  Inc(Count);
                  Inc(NonUnicodeCount);
                  Inc(UTF8Bytes, 4);
                  Include(Blocks, cbNonUnicode);
                end;
                PLongWord(P)^ :=
                  ($F8 or (Q shr 24)) or
                  (($80 or ((Q shr 18) and $3F)) shl 8) or
                  (($80 or ((Q shr 12) and $3F)) shl 16) or
                  (($80 or ((Q shr 6) and $3F)) shl 24);
                Inc(P, SizeOf(LongWord));
                PByte(P)^ := $80 or (Q and $3F);
                Continue;
              end;
            $4000000..$7FFFFFFF:
              begin
                with Info do
                begin
                  P := Dest + Count + InvalidCount + UTF8Bytes;
                  Inc(Count);
                  Inc(NonUnicodeCount);
                  Inc(UTF8Bytes, 5);
                  Include(Blocks, cbNonUnicode);
                end;
                PLongWord(P)^ :=
                  ($FC or (Q shr 30)) or
                  (($80 or ((Q shr 24) and $3F)) shl 8) or
                  (($80 or ((Q shr 18) and $3F)) shl 16) or
                  (($80 or ((Q shr 12) and $3F)) shl 24);
                Inc(P, SizeOf(LongWord));
                PWord(P)^ :=
                  ($80 or (Q shr 6)) or
                  (($80 or (Q and $3F)) shl 8);
                Continue;
              end;
          end
        else
          case Q of
            Low(TUnicodeSMP)..High(TUnicodePUA):
              begin
                with Info do
                begin
                  P := Dest + Count + InvalidCount + UTF8Bytes;
                  Inc(Count);
                  Inc(SurrogateCount);
                  Inc(UTF8Bytes, 5);
                  if UTF8Compliance = cpRegular then
                    UTF8Compliance := cpCESU8;
                  if siRangeBlocks in TUTFDecoder(Self).Options then
                  begin
                    Block := FindCharBlock(Q, Block);
                    Include(Blocks, Block);
                  end;
                end;
                T := Q - Low(TUnicodeSMP);
                H := Low(THighSurrogates) + T div $400;
                L := Low(TLowSurrogates) + T mod $400;
                PLongWord(P)^ :=
                  ($E0 or (H shr 12)) or
                  (($80 or ((H shr 6) and $3F)) shl 8) or
                  (($80 or (H and $3F)) shl 16) or
                  (($E0 or (L shr 12)) shl 24);
                Inc(P, SizeOf(LongWord));
                PWord(P)^ :=
                  ($80 or ((L shr 6) and $3F)) or
                  (($80 or (L and $3F)) shl 8);
                Continue;
              end;
          end;
      end;
    end;
    if siForceInvalid in TUTFDecoder(Self).Options then
    begin
      with Info do
      begin
        P := Dest + Count + InvalidCount + UTF8Bytes;
        Inc(InvalidCount);
        Inc(UTF8Bytes, Unknown_UTF8_Bytes - 1);
        if siRangeBlocks in TUTFDecoder(Self).Options then
        begin
          Block := FindCharBlock(Q, Block);
          Include(Blocks, Block);
        end;
      end;
      PWord(P)^ := Unknown_UTF8 and High(Word);
      Inc(P, SizeOf(Word));
      PByte(P)^ := Unknown_UTF8 shr 16;
    end
    else
    begin
      with Info do
      begin
        InvalidChar := TUTFDecoder(Self).FSource + ByteCount(Result);
        Include(Blocks, FindCharBlock(Q, Block));
      end;
      Exit;
    end;
  end;
end;

function TCharDecoder.EncodeUTF16(Dest: PWideChar; BigEndian: Boolean): Cardinal;
var
  Idx: Cardinal;
  Q: QuadChar;
  Inf: TCharInfo;
begin
  Result := 0;
  Idx := 0;
  while Idx < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Idx, Q, Inf) then
    begin
      case Q of
        Low(TUnicodeBMP)..High(TUnicodeBMP):
          begin
            if BigEndian then
              Dest[Result] := WideChar(Swap(Q))
            else
              Dest[Result] := WideChar(Q);
            Inc(Result);
            Continue;
          end;
        Low(TUnicodeSMP)..High(TUnicodePUA):
          begin
            Dec(Q, Low(TUnicodeSMP));
            if BigEndian then
              PLongWord(Dest + Result)^ :=
                Swap(Q div $400 + Low(THighSurrogates)) or
                (Swap(Q mod $400 + Low(TLowSurrogates)) shl 16)
            else
              PLongWord(Dest + Result)^ :=
                (Q div $400 + Low(THighSurrogates)) or
                ((Q mod $400 + Low(TLowSurrogates)) shl 16);
            Inc(Result, 2);
            Continue;
          end;
      end;
    end;
    if siForceInvalid in TUTFDecoder(Self).Options then
    begin
      (Dest + Result)^ := UnknownUTF16[BigEndian];
      Inc(Result);
    end
    else
      raise Exception.Create; // TODO: cannot encode UTF-16
  end;
end;

function TCharDecoder.EncodeUTF16Ex(Dest: PWideChar; BigEndian: Boolean): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF16Ex(Result, Dest);
end;

function TCharDecoder.EncodeUTF16Ex(var Info: TCharInfo; Dest: PWideChar;
  BigEndian: Boolean): Cardinal;
var
  Block: TCharBlock;
  Q, T: QuadChar;
  Inf: TCharInfo;
  W: Word;
begin
  Block := cbUnknown;
  Result := 0;
  while Result < TUTFDecoder(Self).FCount do
  begin
    if NextChar(Result, Q, Inf) then
    begin
      case Q of
        Low(TUnicodeBMP)..High(TUnicodeBMP):
          begin
            W := Q;
            if BigEndian then
              W := Swap(W);
            with Info do
            begin
              Dest[Count + InvalidCount + SurrogateCount] := WideChar(W);
              Inc(Count);
              if siRangeBlocks in TUTFDecoder(Self).Options then
              begin
                Block := FindCharBlock(Q, Block);
                Include(Blocks, Block);
              end;
            end;
            Continue;
          end;
        Low(TUnicodeSMP)..High(TUnicodePUA):
          begin
            if BigEndian then
              T :=
                Swap((Q - Low(TUnicodeSMP)) div $400 + Low(THighSurrogates)) or
                (Swap(Q mod $400 + Low(TLowSurrogates)) shl 16)
            else
              T :=
                ((Q - Low(TUnicodeSMP)) div $400 + Low(THighSurrogates)) or
                ((Q mod $400 + Low(TLowSurrogates)) shl 16);
            with Info do
            begin
              PLongWord(Dest + Count + InvalidCount + SurrogateCount)^ := T;
              Inc(Count);
              Inc(SurrogateCount);
              if siRangeBlocks in TUTFDecoder(Self).Options then
              begin
                Block := FindCharBlock(Q, Block);
                Include(Blocks, Block);
              end;
            end;
            Continue;
          end;
      end;
    end;
    if siForceInvalid in TUTFDecoder(Self).Options then
      with Info do
      begin
        (Dest + Count + InvalidCount + SurrogateCount)^ := UnknownUTF16[BigEndian];
        Inc(InvalidCount);
        case Q of
          Low(TNonUnicode)..High(TNonUnicode):
            begin
              Inc(NonUnicodeCount);
              Include(Blocks, cbNonUnicode);
            end;
        else
          if siRangeBlocks in TUTFDecoder(Self).Options then
          begin
            Block := FindCharBlock(Q, Block);
            Include(Blocks, Block);
          end;
        end;
      end
    else
    begin
      with Info do
      begin
        InvalidChar := TUTFDecoder(Self).FSource + ByteCount(Result);
        Include(Blocks, FindCharBlock(Q, Block));
      end;
      Exit;
    end;
  end;
end;

function TCharDecoder.EncodeUTF32(Dest: PQuadChar; BigEndian: Boolean): Cardinal;
begin
  Result := 0; // TODO
end;

function TCharDecoder.EncodeUTF32Ex(Dest: PQuadChar; BigEndian: Boolean): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF32Ex(Result, Dest);
end;

function TCharDecoder.EncodeUTF32Ex(var Info: TCharInfo; Dest: PQuadChar;
  BigEndian: Boolean): Cardinal;
begin
  Result := 0; // TODO
end;

class function TCharDecoder.Length(Str: Pointer): Cardinal;
begin
  Result := StrLen(Str);
end;

procedure TCharDecoder.SetData(Source: Pointer);
begin
  SetData(Source, Length(Source));
end;

procedure TCharDecoder.SetData(Source: Pointer; Count: Cardinal);
begin
  TUTFDecoder(Self).FSource := Source;
  TUTFDecoder(Self).FCount := Count;
end;

{ TUTF8Decoder }

// 0x00000080 — 0x000007FF      110xxxxx 10xxxxxx                    // C0
// 0x00000800 — 0x0000FFFF      1110xxxx 10xxxxxx 10xxxxxx           // E0
// 0x00010000 - 0x001FFFFF      11110xxx 10xxxxxx 10xxxxxx 10xxxxxx  // F0

// 0x00200000 — 0x03FFFFFF      111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx           // F8
// 0x04000000 — 0x7FFFFFFF      1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx  // FC

function TUTF8Decoder.EncodeUTF16(Dest: PWideChar; BigEndian: Boolean): Cardinal;
var
  Index: Cardinal;
  Q, T: QuadChar;
  B, Bytes: Byte;
begin
  Result := 0;
  Index := 0;
  T := 0;
  while Index < FCount do
  begin
    B := Byte(FSource[Index]);
    Inc(Index);
    if B and $80 = 0 then
    begin
      if BigEndian then
        Dest[Result] := WideChar(B shl 8)
      else
        Dest[Result] := WideChar(B);
      Inc(Result);
      Continue;
    end
    else
      if B and $40 <> 0 then
      begin
        if B and $20 = 0 then
        begin
          Q := B and $1F;
          Bytes := 1;
        end
        else if B and $10 = 0 then
        begin
          Q := B and $0F;
          Bytes := 2;
        end
        else if B and $08 = 0 then
        begin
          Q := B and $07;
          Bytes := 3;
        end
        else if B and $04 = 0 then
        begin
          Q := B and $03;
          Bytes := 4;
        end
        else if B and $02 = 0 then
        begin
          Q := B and $01;
          Bytes := 5;
        end
        else
        begin
          Bytes := 0;
          Q := High(QuadChar);
        end;

        while Bytes <> 0 do
        begin
          B := Byte(Source[Index]);
          if B and $80 = 0 then
          begin
            Q := High(QuadChar);
            Break;
          end;
          Q := (Q shl 6) or (B and $3F);
          Inc(Index);
          Dec(Bytes);
        end;

        if Q <= High(TUnicodeBMP) then
          case Q of
            Low(THighSurrogates)..High(THighSurrogates):
              if T = 0 then
              begin
                T := Q;
                Continue;
              end;
            Low(TLowSurrogates)..High(TLowSurrogates):
              if T <> 0 then
              begin
                if BigEndian then
                  PLongWord(Dest + Result)^ := Swap(T) or (Swap(Q) shl 16)
                else
                  PLongWord(Dest + Result)^ := T or (Q shl 16);
                Inc(Result, 2);
                T := 0;
                Continue;
              end;
          else
            if BigEndian then
              Dest[Result] := WideChar(Swap(Q))
            else
              Dest[Result] := WideChar(Q);
            Inc(Result);
            Continue;
          end
        else if (T = 0) and (Q <= High(TUnicodePUA)) then
        begin
          Dec(Q, Low(TUnicodeSMP));
          if BigEndian then
            PLongWord(Dest + Result)^ :=
              Swap(Q div $400 + Low(THighSurrogates)) or
              (Swap(Q mod $400 + Low(TLowSurrogates)) shl 16)
          else
            PLongWord(Dest + Result)^ :=
              (Q div $400 + Low(THighSurrogates)) or
              ((Q mod $400 + Low(TLowSurrogates)) shl 16);
          Inc(Result, 2);
          Continue;
        end;
      end;
      
    if siForceInvalid in Options then
    begin
      (Dest + Result)^ := UnknownUTF16[BigEndian];
      Inc(Result);
      T := 0;
    end
    else
      raise Exception.Create; // TODO: cannot encode UTF-16
  end;
end;

function TUTF8Decoder.NextChar(var Index: Cardinal; var Char: QuadChar;
  var Info: TCharInfo): Boolean;

function NextUTF8Char(var Index: Cardinal; var Char: QuadChar): Boolean;
var
  B, Bytes: Byte;
begin
  B := Byte(FSource[Index]);
  Inc(Index);
  if B and $80 = 0 then
  begin
    Char := B;
    Result := True;
    Exit;
  end
  else
    if B and $40 <> 0 then
    begin
      if B and $20 = 0 then
      begin
        Char := B and $1F;
        Bytes := 1;
      end
      else if B and $10 = 0 then
      begin
        Char := B and $0F;
        Bytes := 2;
      end
      else if B and $08 = 0 then
      begin
        Char := B and $07;
        Bytes := 3;
      end
      else if B and $04 = 0 then
      begin
        Char := B and $03;
        Bytes := 4;
      end
      else if B and $02 = 0 then
      begin
        Char := B and $01;
        Bytes := 5;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  while Bytes <> 0 do
  begin
    B := Byte(Source[Index]);
    if B and $80 = 0 then
    begin
      Result := False;
      Exit;
    end;
    Char := (Char shl 6) or (B and $3F);
    Inc(Index);
    Dec(Bytes);
  end;
  if Char = 0 then
    Info.UTF8Compliance := cpModified;
  Result := True;
end;

var
  T: QuadChar;
  Idx: Cardinal;
begin
  if NextUTF8Char(Index, Char) then
    case Char of
      Low(THighSurrogates)..High(THighSurrogates):
        begin
          Idx := Index;
          if NextUTF8Char(Idx, T) then
            case T of
              Low(TLowSurrogates)..High(TLowSurrogates):
                begin
                  Index := Idx;
                  Char := Low(TUnicodeSMP) + (Char - Low(THighSurrogates)) * $400 +
                          T - Low(TLowSurrogates);
                  with Info do
                    if UTF8Compliance = cpRegular then
                      UTF8Compliance := cpCESU8;
                  Result := True;
                  Exit;
                end;
            end;
          //Result := False;
        end;
      Low(TLowSurrogates)..High(TLowSurrogates):
        //Result := False;
    else
      Result := True;
      Exit;
    end;
  Result := False;
end;

{ TUTF16Decoder }

class function TUTF16Decoder.ByteCount(Length: Cardinal): Cardinal;
begin
  Result := Length * SizeOf(WideChar);
end;

function TUTF16Decoder.EncodeUTF8(Dest: PLegacyChar; Compliance: TUTF8Compliance): Cardinal;
var
  Index: Cardinal;
  H, L: Word;
  Q: QuadChar;
  P: PLegacyChar;
begin
  Index := 0;
  Result := 0;
  while Index < FCount do
  begin
    H := PWord(FSource + Index)^;
    if siBigEndian in FOptions then
      H := Swap(H);
    Inc(Index);
    case H of
      $0:
        if Compliance = cpModified then
        begin
          PWord(Dest + Result)^ := $80C0;
          Inc(Result, SizeOf(Word));
          Continue;
        end
        else
        begin
          Dest[Result] := #0;
          Inc(Result);
          Continue;
        end;
      $01..$7F:
        begin
          Dest[Result] := Char(H);
          Inc(Result);
          Continue;
        end;
      $80..$7FF:
        begin
          P := Dest + Result;
          Inc(Result, SizeOf(Word));
          PWord(P)^ :=
            ($C0 or (H shr 6)) or
            (($80 or (H and $3F)) shl 8);
          Continue;
        end;
      Low(THighSurrogates)..High(THighSurrogates):
        begin
          if Index < FCount then
          begin
            L := PWord(FSource + Index)^;
            if siBigEndian in FOptions then
              L := Swap(L);
            case L of
              Low(TLowSurrogates)..High(TLowSurrogates):
                if Compliance = cpRegular then
                begin
                  Q := Low(TUnicodeSMP) + (H - Low(THighSurrogates)) * $400 +
                       L - Low(TLowSurrogates);
                  Inc(Index);
                  PLongWord(Dest + Result)^ :=
                    ($F0 or (Q shr 18)) or
                    (($80 or ((Q shr 12) and $3F)) shl 8) or
                    (($80 or ((Q shr 6) and $3F)) shl 16) or
                    (($80 or (Q and $3F)) shl 24);
                  Inc(Result, SizeOf(LongWord));
                end
                else
                begin
                  P := Dest + Result;
                  Inc(Result, SizeOf(LongWord) + SizeOf(Word));
                  PLongWord(P)^ :=
                    ($E0 or (H shr 12)) or
                    (($80 or ((H shr 6) and $3F)) shl 8) or
                    (($80 or (H and $3F)) shl 16) or
                    (($E0 or (L shr 12)) shl 24);
                  Inc(P, SizeOf(LongWord));
                  PWord(P)^ :=
                    ($80 or ((L shr 6) and $3F)) or
                    (($80 or (L and $3F)) shl 8);
                  Continue;
                end;
            end;
          end;
          //Result := False;
        end;
      Low(TLowSurrogates)..High(TLowSurrogates):
        //Result := False;
    else
      P := Dest + Result;
      Inc(Result, SizeOf(Word) + SizeOf(Byte));
      PWord(P)^ :=
        ($E0 or (H shr 12)) or
        (($80 or ((H shr 6) and $3F)) shl 8);
      Inc(P, SizeOf(Word));
      PByte(P)^ := $80 or (H and $3F);
      Continue;
    end;
    if siForceInvalid in Options then
    begin
      P := Dest + Result;
      Inc(Result, SizeOf(UnknownUTF8));
      PWord(P)^ := Unknown_UTF8 and High(Word);
      Inc(P, SizeOf(Word));
      PByte(P)^ := Unknown_UTF8 shr 16;
    end
    else
      raise Exception.Create; // TODO: cannot convert into UTF-8
  end;
end;

class function TUTF16Decoder.Length(Str: Pointer): Cardinal;
begin
  Result := WideStrLen(Str);
end;

function TUTF16Decoder.NextChar(var Index: Cardinal; var Char: QuadChar;
  var Info: TCharInfo): Boolean;
var
  H, L: Word;
begin
  H := PWord(FSource + Index)^;
  if siBigEndian in FOptions then
    H := Swap(H);
  Inc(Index);
  case H of
    Low(THighSurrogates)..High(THighSurrogates):
      begin
        if Index < FCount then
        begin
          L := PWord(FSource + Index)^;
          if siBigEndian in FOptions then
            L := Swap(L);
          case L of
            Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                Char := Low(TUnicodeSMP) + (H - Low(THighSurrogates)) * $400 +
                        L - Low(TLowSurrogates);
                Inc(Index);
                Result := True;
                Exit;
              end;
          end;
        end;
        //Result := False;
      end;
    Low(TLowSurrogates)..High(TLowSurrogates):
      //Result := False;
  else
    Char := H;
    Result := True;
    Exit;
  end;
  Char := H;
  Result := False;
end;

{ TUTF32Decoder }

class function TUTF32Decoder.ByteCount(Length: Cardinal): Cardinal;
begin
  Result := Length * SizeOf(QuadChar);
end;

class function TUTF32Decoder.Length(Str: Pointer): Cardinal;
begin
  Result := QuadStrLen(Str);
end;

function TUTF32Decoder.NextChar(var Index: Cardinal; var Char: QuadChar;
  var Info: TCharInfo): Boolean;
var
  T: QuadChar;
begin
  Char := PQuadChar(FSource)[Index];
  if siBigEndian in FOptions then
  asm
    MOV EDX, Char
    BSWAP EDX
    MOV Char, EDX
  end;
  Inc(Index);
  case Char of
    Low(THighSurrogates)..High(THighSurrogates):
      begin
        if siForceInvalid in Options then
        begin
          if Index < Count then
          begin
            T := PQuadChar(FSource)[Index];
            if siBigEndian in Options then
            asm
              MOV EDX, T
              BSWAP EDX
              MOV T, EDX
            end;
            case T of
              Low(TLowSurrogates)..High(TLowSurrogates):
                begin
                  Char := Low(TUnicodeSMP) + (Char - Low(THighSurrogates)) * $400 +
                          T - Low(TLowSurrogates);
                  Inc(Index);
                  Result := True;
                  Exit;
                end;
            end;
          end;
        end;
        //Result := False;
      end;
    Low(TLowSurrogates)..High(TLowSurrogates):
      //Result := False;
  else
    Result := True;
    Exit;
  end;
  Result := False;
end;

{ TASCIIDecoder }

function TASCIIDecoder.NextChar(var Index: Cardinal; var Char: QuadChar;
  var Info: TCharInfo): Boolean;
var
  B: Byte;
begin
  B := PByte(FSource + Index)^;
  Inc(Index);
  if B and $80 = 0 then
  begin
    Char := B;
    Result := True;
  end
  else
  begin
    Char := Byte(Unknown_Latin);
    Result := False;
  end;
end;

end.

