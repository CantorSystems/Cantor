(*
    The Unified Environment Core Library

    Core string, character set, code page, and MIME encoding implementation

    Copyright (c) 2007-2009 The Unified Environment Laboratory

    TODO: ISO 2022 and GB18030 support

    NOTE:  CTRL_SHIFT_UP_CTRL_SHIFT_DOWN is not regular define,
           just workaround for the named IDE option can work properly
*)

unit Strings;

interface

uses
  Windows, Core, Storage;

{$I CodePage.inc}
{$I Unicode.inc}

const
  CP_ACP        = Windows.CP_ACP;
  CP_OEMCP      = Windows.CP_OEMCP;
  CP_UTF7       = Windows.CP_UTF7;
  CP_UTF8       = Windows.CP_UTF8;

  CP_UTF16_LE   = CP_UCS2_LE;
  CP_UTF16_BE   = CP_UCS2_BE;

  CP_UTF16      = CP_UTF16_LE;  // plaform-dependent

  CP_UTF32_LE   = CP_UCS4_LE;
  CP_UTF32_BE   = CP_UCS4_BE;

  CP_UTF32      = CP_UTF32_LE;  // plaform-dependent

  BOM_UTF7      = $762F2B;
  BOM_UTF7_LastBytes = [$38, $39, $2B, $2F];

  BOM_UTF8      = $BFBBEF;

  BOM_UTF16_BE  = $FFFE;
  BOM_UTF16_LE  = $FEFF;
  BOM_UTF32_BE  = $FFFE0000;
  BOM_UTF32_LE  = $0000FEFF;

// UTF-16 to UTF-32: $10000 + (H - $D800) * $400 + (L - $DC00)

  UNKNOWN_ASCII     = Char($1A);

  UNKNOWN_UTF16_LE  = WideChar($FFFD);
  UNKNOWN_UTF16_BE  = WideChar($FDFF);

  UNKNOWN_UTF16     = UNKNOWN_UTF16_LE;  // plaform-dependent

  UNKNOWN_UTF32_LE  = QuadChar($0000FFFD);
  UNKNOWN_UTF32_BE  = QuadChar($FDFF0000);

  UNKNOWN_UTF32     = UNKNOWN_UTF32_LE;  // plaform-dependent

type
  TCharacterSetType = (cpASCII, cpSingleByte, cpMultiByte, cpWideChar, cpQuadChar);

  DoubleChar = packed record
    case Byte of
      0: (SingleByte: Char);
      1: (LeadByte, TrailByte: Char);
  end;

  UTF16Surrogates = packed record
    Hi, Lo: WideChar; // fixed word order, always big-endian
  end;

  UTF16Char = packed record
    case Word of
      0: (BasicChar: WideChar);
      1: (Surrogates: UTF16Surrogates);
  end;

  TCharInfo = record
    Count, SurrogateCount, NonSpacingCount: Cardinal;
    UnicodeBlocks: TUnicodeBlocks;
  //  FullwidthCount, HalfwidthCount: Cardinal; // TODO?
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount: Cardinal);
  end;

  TUnicodeOptions = set of (uoBigEndian, uoForceInvalid, uoRangeBlocks,
    uoDecomposed{, uoGlyphChars});

{ Common service }

function Length(Str: PChar): Cardinal; overload;
function Length(Str: PChar; MaxLength: Cardinal): Cardinal; overload;
function Length(Str: PWideChar): Cardinal; overload;
function Length(Str: PWideChar; MaxLength: Cardinal): Cardinal; overload;
function Length(Str: PQuadChar): Cardinal; overload;
function Length(Str: PQuadChar; MaxLength: Cardinal): Cardinal; overload;

function Scan(What: Char; Where: PChar; Count: Cardinal): PChar; overload;
function Scan(What: WideChar; Where: PWideChar; Count: Cardinal): PWideChar; overload;
function Scan(What: QuadChar; Where: PQuadChar; Count: Cardinal): PQuadChar; overload;

{ Unicode service }

function Compose(Source: PWideChar; Count: Cardinal): Cardinal; overload;
function Compose(Source: PWideChar; Count: Cardinal; Dest: PWideChar): Cardinal; overload;
function Compose(Source: PQuadChar; Count: Cardinal): Cardinal; overload;
function Compose(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar): Cardinal; overload;

function Decompose(Source: PWideChar; Count: Cardinal): Cardinal; overload;
function Decompose(Source: PWideChar; Count: Cardinal; Dest: PWideChar): Cardinal; overload;
function Decompose(Source: PQuadChar; Count: Cardinal): Cardinal; overload;
function Decompose(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar): Cardinal; overload;

function FindUnicodeBlock(Source: QuadChar): TUnicodeBlock;

function ASCIICharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
function UTF7CharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
function UTF8CharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
function WideCharInfo(Str: PWideChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
function QuadCharInfo(Str: PQuadChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;

{ Decoding }

function DecodeASCII(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeASCII(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeASCII(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function DecodeUTF7(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF7(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF7(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function DecodeUTF8(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF8(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF8(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function DecodeUTF16(Source: PWideChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF16(Source: PWideChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function DecodeUTF32(Source: PQuadChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function DecodeUTF32(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function DecodeASCIIEx(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeASCIIEx(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeASCIIEx(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function DecodeUTF7Ex(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF7Ex(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF7Ex(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function DecodeUTF8Ex(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF8Ex(Source: PChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF8Ex(Source: PChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function DecodeUTF16Ex(Source: PWideChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF16Ex(Source: PWideChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function DecodeUTF32Ex(Source: PQuadChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function DecodeUTF32Ex(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

{ Encoding }

function EncodeASCII(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeASCII(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeASCII(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function EncodeUTF7(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF7(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF7(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function EncodeUTF8(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF8(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF8(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function EncodeUTF16(Source: PWideChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF16(Source: PQuadChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function EncodeUTF32(Source: PWideChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;
function EncodeUTF32(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): Cardinal; overload;

function EncodeASCIIEx(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeASCIIEx(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeASCIIEx(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function EncodeUTF7Ex(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF7Ex(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF7Ex(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function EncodeUTF8Ex(Source: PChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF8Ex(Source: PWideChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF8Ex(Source: PQuadChar; Count: Cardinal; Dest: PChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function EncodeUTF16Ex(Source: PWideChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF16Ex(Source: PQuadChar; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

function EncodeUTF32Ex(Source: PWideChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;
function EncodeUTF32Ex(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions = []): TCharInfo; overload;

type
  TCharSet = class
  protected
    function GetCodePage: Cardinal; virtual; abstract;
  // properties
    property CodePage: Cardinal read GetCodePage;
  public
    class function ByteLength(Source: Cardinal): Cardinal; overload; virtual;
    class function ByteLength(Str: Pointer): Cardinal; overload;
    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; overload; virtual; abstract;
    function CharInfo(Str: Pointer; const Options: TUnicodeOptions = []): TCharInfo; overload;

    function Decode(Source: Pointer; Count: Cardinal; Dest: PWideChar;
      const Options: TUnicodeOptions = []): Cardinal; overload;
    function Decode(Source: Pointer; Count: Cardinal; Dest: PQuadChar;
      const Options: TUnicodeOptions = []): Cardinal; overload;

    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PWideChar;
      const Options: TUnicodeOptions = []): TCharInfo; overload; virtual; abstract;
    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PQuadChar;
      const Options: TUnicodeOptions = []): TCharInfo; overload; virtual; abstract;

    function Encode(Source: PWideChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): Cardinal; overload;
    function Encode(Source: PQuadChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): Cardinal; overload;

    function EncodeEx(Source: PWideChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; overload; virtual; abstract;
    function EncodeEx(Source: PQuadChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; overload; virtual; abstract;

    class function Length(Str: Pointer): Cardinal; virtual;
    class function MaxCharBytes: Byte; virtual; abstract;

    function NextChar(Str: Pointer; Count: Cardinal): Pointer; virtual; abstract;
    function PrevChar(Start, Current: Pointer): Pointer; virtual; abstract;
  end;

  TUnicodeCharSet = class(TCharSet)
  public
    class function MaxCharBytes: Byte; override;
  // properties
    property CodePage;
  end;

  TUTF7CharSet = class(TUnicodeCharSet)
  protected
    function GetCodePage: Cardinal; override;
  public
    class function MaxCharBytes: Byte; override;
  end;

  TUTF8CharSet = class(TUnicodeCharSet)
  protected
    function GetCodePage: Cardinal; override;
  end;

  TEndianCharSet = class(TUnicodeCharSet)
  private
    FBigEndian: Boolean;
  public
    constructor Create(BigEndian: Boolean);
  // properties
    property BigEndian: Boolean read FBigEndian;
  end;

  TUTF16CharSet = class(TEndianCharSet)
  protected
    function GetCodePage: Cardinal; override;
  public
    class function ByteLength(Source: Cardinal): Cardinal; override;
    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    class function Length(Str: Pointer): Cardinal; override;
  end;

  TUTF32CharSet = class(TEndianCharSet)
  protected
    function GetCodePage: Cardinal; override;
  public
    class function ByteLength(Source: Cardinal): Cardinal; override;
    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    class function Length(Str: Pointer): Cardinal; override;
  end;

  TNonUnicodeCharSet = class(TCharSet)
  public
    function Decode(Source: PChar; Count: Cardinal; Dest: PChar;
      CharSet: TNonUnicodeCharSet): Cardinal; overload;
    function DecodeEx(Source: PChar; Count: Cardinal; Dest: PChar;
      CharSet: TNonUnicodeCharSet): TCharInfo; overload; virtual; abstract;

    function Encode(Source: PChar; Count: Cardinal;
      CharSet: TNonUnicodeCharSet; Dest: PChar): Cardinal; overload;
    function EncodeEx(Source: PChar; Count: Cardinal; CharSet: TNonUnicodeCharSet;
      Dest: PChar): TCharInfo; overload; virtual; abstract;

    class function MaxCharBytes: Byte; override;
    function NextChar(Str: Pointer; Count: Cardinal): Pointer; override;
    function PrevChar(Start, Current: Pointer): Pointer; override;
  end;

  TASCIICharSet = class(TNonUnicodeCharSet)
  public
    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; override;

    function DecodeEx(Source: PChar; Count: Cardinal; Dest: PChar;
      CharSet: TNonUnicodeCharSet): TCharInfo; override;
    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PWideChar;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PQuadChar;
      const Options: TUnicodeOptions = []): TCharInfo; override;

    function EncodeEx(Source: PChar; Count: Cardinal;
      CharSet: TNonUnicodeCharSet; Dest: PChar): TCharInfo; override;
    function EncodeEx(Source: PWideChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    function EncodeEx(Source: PQuadChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; override;
  end;

  TCodePageCharSet = class(TNonUnicodeCharSet)
  private
    FCodePage: Cardinal;
  protected
    function GetCodePage: Cardinal; override;
  public
  // properties
    property CodePage: Cardinal read FCodePage;
  end;

  // mapping from 0 for EBCDIC compliance
  TSingleByteMap = array[Low(Char)..High(Char)] of WideChar;

  TSingleByteCharSet = class(TCodePageCharSet)
  private
    FMap: TSingleByteMap;
    FWideMap: PChar;
    FWideMapMin, FWideMapMax: WideChar;
    function GetWideMap(Index: WideChar): Char;
    function GetWideMapCount: Word;
  public
    constructor Create(CodePage: Cardinal); overload;
    constructor Create(CodePage: Cardinal; DefaultUnicodeChar: WideChar); overload;
    destructor Destroy; override;

    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; override;

    function DecodeEx(Source: PChar; Count: Cardinal; Dest: PChar;
      CharSet: TNonUnicodeCharSet): TCharInfo; override;
    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PWideChar;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    function DecodeEx(Source: Pointer; Count: Cardinal; Dest: PQuadChar;
      const Options: TUnicodeOptions = []): TCharInfo; override;

    function EncodeEx(Source: PChar; Count: Cardinal;
      CharSet: TNonUnicodeCharSet; Dest: PChar): TCharInfo; override;
    function EncodeEx(Source: PWideChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; override;
    function EncodeEx(Source: PQuadChar; Count: Cardinal; Dest: Pointer;
      const Options: TUnicodeOptions = []): TCharInfo; override;
  // properties
    property Map: TSingleByteMap read FMap;
    property WideMap[Index: WideChar]: Char read GetWideMap;
    property WideMapChar: PChar read FWideMap;
    property WideMapCount: Word read GetWideMapCount;
    property WideMapMin: WideChar read FWideMapMin;
    property WideMapMax: WideChar read FWideMapMax;
  end;

  TLeadByteRange = packed record
    Min, Max: Char;
  end;

const
  MaxLeadBytes = MAX_LEADBYTES div SizeOf(TLeadByteRange);

type
  PLeadByteRanges = ^TLeadByteRanges;
  TLeadByteRanges = array[0..MaxLeadBytes - 1] of TLeadByteRange;

  TLeadByteMap = record
    LeadByte: Char;
    TrailByteMap: TSingleByteMap;
  end;

  PDoubleByteMap = ^TDoubleByteMap;
  TDoubleByteMap = array[0..MaxLeadBytes - 1] of TLeadByteMap;

  PWideMap = ^TWideMap;
  TWideMap = array[0..MaxInt div SizeOf(DoubleChar) - 1] of DoubleChar;

  TMultiByteCharSet = class(TCodePageCharSet)
  private
    FLeadByteCount: Cardinal;
    FMap: PDoubleByteMap;
    FWideMap: PWideMap;
    FWideMapMin, FWideMapMax: WideChar;
    function GetMap(const Source: DoubleChar): WideChar;
    function GetWideMap(Source: WideChar): DoubleChar;
  public
    constructor Create(CodePage: Cardinal; LeadByteRanges: PLeadByteRanges;
      LeadByteRangeCount: Cardinal); overload;
    destructor Destroy; override;
    function IndexOf(LeadByte: Char): Integer;
    function CharInfo(Str: Pointer; Count: Cardinal;
      const Options: TUnicodeOptions = []): TCharInfo; override;
  // properties
    property DoubleByteMap: PDoubleByteMap read FMap;
    property LeadByteCount: Cardinal read FLeadByteCount;
    property Map[const Source: DoubleChar]: WideChar read GetMap;
    property WideMap[Source: WideChar]: DoubleChar read GetWideMap;
    property WideMapChar: PWideMap read FWideMap;
    property WideMapMin: WideChar read FWideMapMin;
    property WideMapMax: WideChar read FWideMapMax;
  end;

  TCodePageInfo = record
    MaxCharBytes: Integer;
    CodePageType: TCharacterSetType;
  end;

function CodePageInfo(CodePage: Cardinal): TCodePageInfo;
function DestCodePage(Source1, Source2: Cardinal): Cardinal;
function IsQuadChar(CodePage: Cardinal): Boolean;
function IsWideChar(CodePage: Cardinal): Boolean;
function TranslateCodePage(CodePage: Cardinal): Cardinal;

function QuadCharToWideChar(Source: PQuadChar; Count: Cardinal;
  Dest: PWideChar; StopOnInvalidChar: Boolean): Cardinal;
function WideCharToQuadChar(Source: PWideChar; Count: Cardinal;
  Dest: PQuadChar): Cardinal;

procedure SwapQuadCharBytes(Source, Dest: PQuadChar; Count: Cardinal); overload;
procedure SwapWideCharBytes(Source, Dest: PWideChar; Count: Cardinal); overload;

function QuickFind(S: PChar; Count: Cardinal; SortedSet: PChar;
  SortedCount: Cardinal): PChar; overload;
function QuickFind(S: PQuadChar; Count: Cardinal; SortedSet: PQuadChar;
  SortedCount: Cardinal): PQuadChar; overload;
function QuickFind(S: PWideChar; Count: Cardinal; SortedSet: PWideChar;
  SortedCount: Cardinal): PWideChar; overload;

procedure QuickSort(S: PChar; Count: Cardinal); overload;
procedure QuickSort(S: PQuadChar; Count: Cardinal); overload;
procedure QuickSort(S: PWideChar; Count: Cardinal); overload;

function FormatBuf(Fmt: PChar; const Args: array of const;
  Buf: PChar): Cardinal;
function WideFormatBuf(Fmt: PWideChar; const Args: array of const;
  Buf: PWideChar): Cardinal;

procedure WriteBOM(S: TWriteableStream; CodePage: Cardinal);

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
  TCPInfoEx = packed record
    MaxCharSize: UINT;
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of Char;
    LeadByte: array[0..MAX_LEADBYTES - 1] of Byte;
    UnicodeDefaultChar: WideChar;
    CodePage: UINT;
    CodePageName: array[0..MAX_PATH - 1] of CoreChar;
  end;

function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;

{ Error codes }

const
  E_ACCESS_TO_NULL_STRING       = 0;
  E_INDEX_OUT_OF_BOUNDS         = 1;
  E_CANNOT_FORMAT_BE            = 2;
  E_CANNOT_FORMAT_UTF32         = 3;
  E_CANNOT_CONVERT_TO_INT       = 4;
  E_CANNOT_CONVERT_TO_INT64     = 5;
  E_CANNOT_CONVERT_TO_FLOAT     = 6;
  E_CANNOT_CONVERT_TO_MULTIBYTE = 7;
  E_CANNOT_CONVERT_TO_UTF16     = 8;
  E_CANNOT_CONVERT_TO_CODEPAGE  = 9;

type
  PStringData = ^TStringData;
  TStringData = object
    CodePage: Cardinal;
    RefCount: LongInt;
    Length: Integer;
  end;

  PStringConst = ^TStringConst;
  TStringConst = object(TStringData)
    Buffer: Pointer;
  end;

  TStringBuffer = record
    case Integer of
    //  -1: (CoreChars: array[0..0] of CoreChar);
       1: (Chars: array[0..0] of Char);
       2: (WideChars: array[0..0] of WideChar);
       4: (QuadChars: array[0..0] of QuadChar);
  end;

  PStringVar = ^TStringVar;
  TStringVar = object(TStringData)
    Buffer: TStringBuffer;
  end;

  TAutoDetectCodePage = set of (cpBOM, cpUTF7, cpUTF8);
  TTrimOptions = set of (toLeft, toRight);

//  TSubString = class;
  TString = class;

  TSubString = TString;
  TUniString = TString;

  TString = class(TObject)
  private
    FData: PStringData;
    FIndex, FLength: Integer;
    procedure AssignData(S: Pointer; Count: Integer; CodePage: Cardinal;
      Index: Integer; AttachToBuffer: Boolean); overload;
    function AssignData(S: TString): Boolean; overload;
    procedure CheckCharIndex(Value: Integer; ForUpdate: Boolean);
    procedure CheckQuadCharIndex(Value: Integer; ForUpdate: Boolean);
    procedure CheckWideCharIndex(Value: Integer; ForUpdate: Boolean);
    function GetAsChar(CodePage: Cardinal): PChar;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: LongInt;
    function GetAsString(CodePage: Cardinal): LegacyString;
    function GetAsWideChar(BigEndian: Boolean): PWideChar;
    function GetAsWideString(BigEndian: Boolean): WideString;
    function GetByteLength: Integer;
    function GetChar(Index: Integer): Char;
    function GetCharInfo: Integer;
    function GetCodePage(Translate: Boolean): Cardinal;
    function GetIsQuadChar: Boolean;
    function GetIsWideChar: Boolean;
    function GetQuadChar(Index: Integer): QuadChar;
    function GetWideChar(Index: Integer): WideChar;
    procedure InsertAt(Index: Integer; Ch: Char; CodePage: Cardinal;
      Count: Integer); overload;
    procedure InsertAt(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
      Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: PChar; ByteCount: Integer;
      CodePage: Cardinal; Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: TString; Count: Integer); overload;
    function NewData(S: Pointer; Count: Integer; CodePage: Cardinal;
      Index: Integer; AttachToBuffer, KeepData: Boolean): PStringData;
    procedure SetAsChar(CodePage: Cardinal; Value: PChar);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsInteger(Value: LongInt);
    procedure SetAsString(CodePage: Cardinal; const Value: LegacyString);
    procedure SetAsWideChar(BigEndian: Boolean; Value: PWideChar);
    procedure SetAsWideString(BigEndian: Boolean; const Value: WideString);
    procedure SetByteLength(Value: Integer);
    procedure SetChar(Index: Integer; Value: Char);
    procedure SetCharInfo(Value: Integer);
    procedure SetCodePage(Translate: Boolean; Value: Cardinal);
    procedure SetIsQuadChar(Value: Boolean);
    procedure SetIsWideChar(Value: Boolean);
    procedure SetLength(Value: Integer);
    procedure SetQuadChar(Index: Integer; Value: QuadChar);
    procedure SetWideChar(Index: Integer; Value: WideChar);
  protected
    procedure Error(Code: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(Ch: Char; CodePage: Cardinal; Count: Integer = 1); overload;
    procedure Append(Ch: QuadChar; BigEndian: Boolean; Count: Integer = 1); overload;
    procedure Append(S: PChar; ByteCount: Integer; CodePage: Cardinal;
      Count: Integer = 1); overload;
    procedure Append(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Count: Integer = 1); overload;
    procedure Append(S: TString; Count: Integer = 1); overload;
    procedure Assign(S: TString);
    procedure CheckIndex(Index: Integer);
    procedure Clear;
    function CodePageInfo: TCodePageInfo;
    function CompareWith(S: TString; CaseSensitive: Boolean): Integer;
    function Data: Pointer;
    procedure Format(S: PChar; ByteCount: Integer; CodePage: Cardinal;
      const Args: array of const); overload;
    procedure Format(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      const Args: array of const); overload;
    procedure Format(S: TString; const Args: array of const); overload;
    function GetData(S: PChar; ByteCount: Integer;
      CodePage: Cardinal): Integer; overload;
    function GetData(S: PChar; ByteCount: Integer; CodePage: Cardinal;
      var BytesWritten: Integer): Boolean; overload;
    function GetData(S: PWideChar; CharCount: Integer;
      BigEndian: Boolean): Integer; overload;
    function GetData(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      var CharsWritten: Integer): Boolean; overload;
    function HasIndex(Index: Integer): Boolean;
    function IndexOf(Ch: Char; CodePage: Cardinal; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOf(Ch: QuadChar; BigEndian: Boolean; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOf(S: PChar; ByteCount: Integer; CodePage: Cardinal;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOf(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOf(S: TString; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    procedure IndexOf(Source: TString; Ch: Char; CodePage: Cardinal;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; Ch: QuadChar; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; S: PChar; ByteCount: Integer;
      CodePage: Cardinal; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source, S: TString; Index: Integer = 0;
      Count: Integer = 1); overload;
    function IndexOfChar(S: PChar; ByteCount: Integer; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOfChar(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOfChar(S: TString; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    procedure IndexOfChar(Source: TString; S: PChar; ByteCount: Integer;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOfChar(Source: TString; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOfChar(Source, S: TString; Index: Integer = 0;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; Ch: Char; CodePage: Cardinal;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: PChar; ByteCount: Integer;
      CodePage: Cardinal; Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: TString; Count: Integer = 1); overload;
    procedure Load(S: TReadableStream; DefaulTCharacterSet: Cardinal;
      AutoDetect: TAutoDetectCodePage);
    procedure LowerCase; overload;
    procedure LowerCase(S: TString); overload;
    function NextChar(Index: Integer): Integer;
    function PrevChar(Index: Integer): Integer;
    function Save(S: TWriteableStream; WriteBOM: Boolean): Boolean;
    procedure SetData(S: PChar; ByteCount: Integer; CodePage: Cardinal;
      AttachToBuffer: Boolean = False); overload;
    procedure SetData(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      AttachToBuffer: Boolean = False); overload;
    procedure SubString(S: TString; Index: Integer); overload;
    procedure SubString(S: TString; Index, Length: Integer); overload;
//    procedure Trim(Options: TTrimOptions; What
    function TryCodePage(CodePage: Cardinal; Translate: Boolean): Boolean;
    function TryAsFloat(var Value: Double): Boolean;
    function TryAsInt64(var Value: Int64): Boolean;
    function TryAsInteger(var Value: LongInt): Boolean;
    procedure UpperCase; overload;
    procedure UpperCase(S: TString); overload;
  // properties
    property Chars[Index: Integer]: Char read GetChar
      write SetChar;
    property AsChar[CodePage: Cardinal]: PChar read GetAsChar
      write SetAsChar;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsString[CodePage: Cardinal]: LegacyString read GetAsString
      write SetAsString;
    property AsWideChar[BigEndian: Boolean]: PWideChar read GetAsWideChar
      write SetAsWideChar;
    property AsWideString[BigEndian: Boolean]: WideString read GetAsWideString
      write SetAsWideString;
    property ByteLength: Integer read GetByteLength write SetByteLength;
    property CodePage[Translate: Boolean]: Cardinal read GetCodePage
      write SetCodePage;
    property CharInfo: Integer read GetCharInfo write SetCharInfo;
    property IsQuadChar: Boolean read GetIsQuadChar write SetIsQuadChar;
    property IsWideChar: Boolean read GetIsWideChar write SetIsWideChar;
    property Length: Integer read FLength write SetLength;
    property QuadChars[Index: Integer]: QuadChar read GetQuadChar write SetQuadChar;
    property WideChars[Index: Integer]: WideChar read GetWideChar write SetWideChar;
  end;

  TCoreString = class(TUniString)
  protected
    procedure Error(Code: Integer); override;
  public
    procedure SysErrorMessage(ErrorCode: Integer);
  // properties
  {$IFNDEF Unicode}
    property Chars; default;
  {$ELSE}
    property WideChars; default;
  {$ENDIF}
  end;

{ MIME support }

function MIME_CodePage(Charset: PCoreChar): Cardinal;
function MIME_Charset(CodePage: Cardinal; Charset: TString): Boolean; overload;
function MIME_Charset(BigEndian, QuadChar: Boolean;
  Charset: TString): Boolean; overload;

implementation

uses
  Exceptions;

{ Missing in Windows.pas }

function GetCPInfoEx(CodePage: UINT; dwFlags: DWORD; var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name {$IFDEF UNICODE} 'GetCPInfoExW' {$ELSE} 'GetCPInfoExA' {$ENDIF} ;

{ Utilites }

function CodePageInfo(CodePage: Cardinal): TCodePageInfo;
var
  Info: TCPInfo;
begin
  if IsWideChar(CodePage) then
    with Result do
    begin
      MaxCharBytes := SizeOf(WideChar);
      CodePageType := cpWideChar;
    end
  else if IsQuadChar(CodePage) then
    with Result do
    begin
      MaxCharBytes := SizeOf(QuadChar);
      CodePageType := cpQuadChar;
    end
  else
  begin
    if GetCPInfo(CodePage, Info) then
      Result.MaxCharBytes := Info.MaxCharSize
    else
      Result.MaxCharBytes := 0;
    Result.CodePageType := cpMultiByte;
  end;
end;

function DestCodePage(Source1, Source2: Cardinal): Cardinal;
begin
  case Source1 of
    CP_UTF32_LE, CP_UTF32_BE:
      begin
        Result := Source1;
        Exit;
      end;
    CP_UTF7:
      Result := CP_UTF16;
  else
    Result := Source1;
  end;
  case Source2 of
    CP_UTF32_LE, CP_UTF32_BE:
      Result := Source2;
    CP_UTF7, CP_UTF16_LE:
      if Result <> CP_UTF16_BE then
        Result := CP_UTF16;
    CP_UTF16_BE:
      if Result <> CP_UTF16_BE then
        Result := Source2;
  else
    if TranslateCodePage(Result) <> TranslateCodePage(Source2) then
      Result := CP_UTF16;
  end;
end;

function IsQuadChar(CodePage: Cardinal): Boolean;
begin
  Result := CodePage - CP_UTF32_LE in [0, 1];
end;

function IsWideChar(CodePage: Cardinal): Boolean;
begin
  Result := CodePage - CP_UTF16_LE in [0, 1];
end;

function TranslateCodePage(CodePage: Cardinal): Cardinal;
begin
  case CodePage of
    CP_ACP:
      Result := GetACP;
    CP_OEMCP:
      Result := GetOEMCP;
  else
    Result := CodePage;
  end;
end;

function Length(Str: PChar): Cardinal;
{$IFDEF CTRL_SHIFT_UP_CTRL_SHIFT_DOWN}
begin
end;
{$ENDIF}
{$I Common\FastCode\StrLen.inc}

function Length(Str: PChar; MaxLength: Cardinal): Cardinal;
asm
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
end;

function Length(Str: PWideChar): Cardinal;
asm
         MOV EDX, EDI
         MOV EDI, EAX
         MOV ECX, $FFFFFFFF
         XOR AX, AX
         REPNE SCASW
         MOV EAX, $FFFFFFFE
         SUB EAX, ECX
         MOV EDI, EDX
end;

function Length(Str: PWideChar; MaxLength: Cardinal): Cardinal;
asm
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
end;

function Length(Str: PQuadChar): Cardinal;
asm
         MOV EDX, EDI
         MOV EDI, EAX
         MOV ECX, $FFFFFFFF
         XOR EAX, EAX
         REPNE SCASD
         MOV EAX, $FFFFFFFE
         SUB EAX, ECX
         MOV EDI, EDX
end;

function Length(Str: PQuadChar; MaxLength: Cardinal): Cardinal;
asm
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
end;

function Scan(What: Char; Where: PChar; Count: Cardinal): PChar;
asm
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

function Scan(What: WideChar; Where: PWideChar; Count: Cardinal): PWideChar;
asm
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

function Scan(What: QuadChar; Where: PQuadChar; Count: Cardinal): PQuadChar;
asm
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

function Compose(Source: PWideChar; Count: Cardinal): Cardinal;
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

function Decompose(Source: PWideChar; Count: Cardinal;
  Dest: PWideChar): Cardinal;
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
end;

function FindUnicodeBlock(Source: QuadChar): TUnicodeBlock;
var
  Min, Max: TUnicodeBlock;
begin
  Min := Low(Result);
  Max := High(Result);
  while Min <= Max do
  begin
    Result := TUnicodeBlock((Ord(Min) + Ord(Max)) div 2);
    if Source < UnicodeBlockRanges[Result].Min then
      Max := Pred(Result)
    else if Source > UnicodeBlockRanges[Result].Max then
      Min := Succ(Result)
    else
      Exit;
  end;
  Result := ubUnknown;
end;

function ASCIICharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
asm
        PUSH ECX
        PUSH EDX
        PUSH EAX
        MOV EAX, Result
        MOV EDX, LongWord(SizeOf(TCharInfo))
        MOV CL, 0
        CALL System.@FillChar
        POP EAX
        POP EDX
        POP ECX
        TEST ECX, ECX // Count
        JZ @@exit
@@start:
        PUSH ESI

        MOV ESI, Str
        //MOV ECX, Count
        PUSH ECX

        XOR EDX, EDX  // InvalidCount
        XOR EAX, EAX
@@loop:
        LODSB
        TEST AL, $80
        SETNZ AL
        ADD EDX, EAX
        LOOP @@loop
@@result:
        POP EAX
        SUB EAX, ECX
        SUB EAX, EDX
        MOV ECX, Result
        MOV [ECX].TCharInfo.Count, EAX
        MOV [ECX].TCharInfo.InvalidCount, EDX
        TEST EDX, EDX
        JZ @@latin
        XOR EDX, EDX
        BTS EDX, ubUnknown
@@latin:
        BTS EDX, ubBasicLatin
        MOV dword ptr [ECX].TCharInfo.UnicodeBlocks, EDX

        POP ESI
@@exit:
end;

function UTF7CharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function UTF8CharInfo(Str: PChar; Count: Cardinal;
  const Options: TUnicodeOptions = []): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function UTF16CharInfo(Str: PWideChar; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
var
  H, L: Word;
begin
  FillChar(Result, SizeOf(Result), 0);
  while Count <> 0 do
  begin
    H := PWord(Str)^;
    if uoBigEndian in Options then
      H := Swap(H);
    case H of
      Low(THighSurrogates)..High(THighSurrogates):
        begin
          if Count > 1 then
          begin
            L := PWordArray(Str)[1];
            if uoBigEndian in Options then
              L := Swap(L);
            case L of
              Low(TLowSurrogates)..High(TLowSurrogates):
                begin
                  with Result do
                  begin
                    Inc(Count);
                    Inc(SurrogateCount);
                    if uoRangeBlocks in Options then
                      Include(UnicodeBlocks, FindUnicodeBlock(QuadChar(
                        $10000 + (H - Low(THighSurrogates)) * $400 +
                        L - Low(TLowSurrogates)
                      )));
                  end;
                  Inc(PWideChar(Str), 2);
                  Dec(Count, 2);
                  Continue;
                end;
            end;
          end;
          Inc(Result.InvalidCount);
          if uoForceInvalid in Options then
          begin
            Inc(Result.SurrogateCount);
            if uoRangeBlocks in Options then
            begin
              Include(Result.UnicodeBlocks, FindUnicodeBlock(QuadChar(
                $10000 + (H - Low(THighSurrogates)) * $400
              )));
              Inc(PWideChar(Str));
              Dec(Count);
              Continue;
            end;
          end;
        end;
    Low(TLowSurrogates)..High(TLowSurrogates):
      begin
        Inc(Result.InvalidCount);
        if uoForceInvalid in Options then
        begin
          Inc(Result.SurrogateCount);
          if uoRangeBlocks in Options then
          begin
            Include(Result.UnicodeBlocks, FindUnicodeBlock(
              QuadChar($10000 + H{as L} - Low(TLowSurrogates))
            ));
            Inc(PWideChar(Str));
            Dec(Count);
            Continue;
          end;
        end;
      end;
    else
      Inc(Result.Count);
    end;
    if uoRangeBlocks in Options then
      Include(Result.UnicodeBlocks, FindUnicodeBlock(QuadChar(H)));
    Inc(PWideChar(Str));
    Dec(Count);
  end;
end;

function UTF32CharInfo(Str: PQuadChar; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
var
  Q, T: QuadChar;
begin
  FillChar(Result, SizeOf(Result), 0);
  while Count <> 0 do
  begin
    Q := PQuadChar(Str)^;
    if uoBigEndian in Options then
    asm
      MOV EDX, Q
      BSWAP EDX
      MOV Q, EDX
    end;
    case Q of
      Low(THighSurrogates)..High(THighSurrogates):
        begin
          Inc(Result.InvalidCount);
          if uoForceInvalid in Options then
          begin
            Inc(Result.SurrogateCount);
            if Count > 1 then
            begin
              T := PLongWordArray(Str)[1];
              if uoBigEndian in Options then
              asm
                MOV EDX, T
                BSWAP EDX
                MOV T, EDX
              end;
              case T of
                Low(TLowSurrogates)..High(TLowSurrogates):
                  begin
                    with Result do
                    begin
                      Inc(InvalidCount);
                      if uoRangeBlocks in Options then
                        Include(UnicodeBlocks, FindUnicodeBlock(QuadChar(
                          $10000 + (Q - Low(THighSurrogates)) * $400 +
                          T - Low(TLowSurrogates)
                        )));
                    end;
                    Inc(PQuadChar(Str), 2);
                    Dec(Count, 2);
                    Continue;
                  end;
              end;
            end;
            if uoRangeBlocks in Options then
            begin
              Include(Result.UnicodeBlocks, FindUnicodeBlock(QuadChar(
                $10000 + (Q - Low(THighSurrogates)) * $400
              )));
              Inc(PQuadChar(Str));
              Dec(Count);
              Continue;
            end;
          end;
        end;
      Low(TLowSurrogates)..High(TLowSurrogates):
        begin
          Inc(Result.InvalidCount);
          if uoForceInvalid in Options then
          begin
            Inc(Result.SurrogateCount);
            if uoRangeBlocks in Options then
            begin
              Include(Result.UnicodeBlocks, FindUnicodeBlock(QuadChar(
                $10000 + Q{as L} - Low(TLowSurrogates)
              )));
              Inc(PQuadChar(Str));
              Dec(Count);
              Continue;
            end;
          end;
        end;
      High(TUnicodePUA)..High(QuadChar):
        Inc(Result.InvalidCount);
    else
      Inc(Result.Count);
      if Q > High(TUnicodeBMP) then
        Inc(Result.SurrogateCount);
    end;
    if uoRangeBlocks in Options then
      Include(Result.UnicodeBlocks, FindUnicodeBlock(Q));
    Inc(PQuadChar(Str));
    Dec(Count);
  end;
end;

function QuadCharToWideChar(Source: PQuadChar; Count: Cardinal;
  Dest: PWideChar; StopOnInvalidChar: Boolean): Cardinal;
var
  P: PQuadChar;
begin
  Result := 0;
  P := Source;
  while Count <> 0 do
  begin
    case LongWord(P^) of
      $D800..$DBFF, $DC00..$DFFF:
        if StopOnInvalidChar then
        begin
          Result := (FlatAddress(P) - FlatAddress(Source)) div SizeOf(QuadChar);
          Exit;
        end;
      $10000..$10FFFF:
      begin
        if Dest <> nil then
        begin
          // y = ((x ? 0001 0000) / 400) + D800
          // z = ((x ? 0001 0000) % 400) + DC00
          Dest^ := WideChar((LongWord(P^) - $10000) mod $400 + $DC00);
          Inc(Dest);
          Dest^ := WideChar((LongWord(P^) - $10000) div $400 + $D800);
          Inc(Dest);
        end;
        Inc(Result, 2);
      end;
      $110000..$FFFFFFFF: ;
        // no mapping defined
    else
      if Dest <> nil then
      begin
        Dest^ := WideChar(P^);
        Inc(Dest);
      end;
      Inc(Result);
    end;
    Inc(P);
    Dec(Count);
  end;
end;

function WideCharToQuadChar(Source: PWideChar; Count: Cardinal;
  Dest: PQuadChar): Cardinal;
var
  HasSurrogate: Boolean;
begin
  Result := 0;
  while Count <> 0 do
  begin
    case Word(Source^) of
      // QuadChar($10000 + (Source[0] - $DC00) + (Source[1] - $D800) * $400;
      $DC00..$DFFF: // low surrogates
        begin
          if Dest <> nil then
          begin
            Dest^ := QuadChar($10000 + Word(Source^) - $DC00);
            Inc(Dest);
          end;
          Inc(Result);
        end;
      $D800..$DBFF: // high surrogates
        begin
          if Result <> 0 then
          begin
            Dec(Source);
            case Word(Source^) of
              $DC00..$DFFF:
                HasSurrogate := True;
            else
              HasSurrogate := False;
            end;
            Inc(Source);
          end
          else
            HasSurrogate := False;
          if Dest <> nil then
          begin
            if HasSurrogate then
            begin
              Dec(Dest);
              Inc(Dest^, (Word(Source^) - $D800) * $400);
              Inc(Dest);
            end
            else
            begin
              Dest^ := $10000 + (Word(Source^) - $D800) * $400;
              Inc(Dest);
              Inc(Result);
            end;
          end
          else if not HasSurrogate then
            Inc(Result);
        end;
    else
      if Dest <> nil then
      begin
        Dest^ := QuadChar(Source^);
        Inc(Dest);
      end;
      Inc(Result);
    end;
    Inc(Source);
    Dec(Count);
  end;
end;

procedure SwapQuadCharBytes(Source, Dest: PQuadChar; Count: Cardinal);
asm
         TEST ECX, ECX
         JZ @@2

         PUSH ESI
         PUSH EDI

         MOV ESI, EAX
         MOV EDI, EDX
@@1:
         LODSD
         BSWAP EAX
         STOSD
         DEC ECX
         JNZ @@1

         POP EDI
         POP ESI
@@2:
end;

procedure SwapWideCharBytes(Source, Dest: PWideChar; Count: Cardinal);
asm
         TEST ECX, ECX
         JZ @@2

         PUSH ESI
         PUSH EDI

         MOV ESI, EAX
         MOV EDI, EDX
@@1:
         LODSW
         XCHG AL, AH
         STOSW
         DEC ECX
         JNZ @@1

         POP EDI
         POP ESI
@@2:
end;

function QuickFind(S: PChar; Count: Cardinal; SortedSet: PChar;
  SortedCount: Cardinal): PChar;
begin
  Result := nil; // TODO
end;

function QuickFind(S: PQuadChar; Count: Cardinal; SortedSet: PQuadChar;
  SortedCount: Cardinal): PQuadChar;
begin
  Result := nil; // TODO
end;

function QuickFind(S: PWideChar; Count: Cardinal; SortedSet: PWideChar;
  SortedCount: Cardinal): PWideChar;
begin
  Result := nil; // TODO
end;

procedure QuickSort(S: PChar; Count: Cardinal);
begin
  // TODO
end;

procedure QuickSort(S: PQuadChar; Count: Cardinal);
begin
  // TODO
end;

procedure QuickSort(S: PWideChar; Count: Cardinal);
begin
  // TODO
end;

function FormatBuf(Fmt: PChar; const Args: array of const;
  Buf: PChar): Cardinal;
asm
        PUSH EDI
        PUSH EBX
        MOV EBX, ESP

        INC ECX
        JZ @@2
@@1:
        MOV EDI, [EDX + ECX*8 - 8]
        PUSH EDI
        LOOP @@1
@@2:
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
        JZ  @@2
@@1:
        MOV EDI, [EDX + ECX*8 - 8]
        PUSH EDI
        LOOP @@1
@@2:
        PUSH ESP
        PUSH EAX
        MOV EAX, Buf
        PUSH EAX
        CALL wvsprintfW

        MOV ESP, EBX
        POP EBX
        POP EDI
end;

procedure WriteBOM(S: TWriteableStream; CodePage: Cardinal);
var
  BOM: LongWord;
begin
  case CodePage of
    CP_UTF7:
      begin
        BOM := BOM_UTF7;
        S.WriteBuffer(BOM, 3);
      end;
    CP_UTF8:
      begin
        BOM := BOM_UTF8;
        S.WriteBuffer(BOM, 3);
      end;
    CP_UTF16_LE:
      begin
        BOM := BOM_UTF16_LE;
        S.WriteBuffer(BOM, SizeOf(Word));
      end;
    CP_UTF16_BE:
      begin
        BOM := BOM_UTF16_BE;
        S.WriteBuffer(BOM, SizeOf(Word));
      end;
    CP_UTF32_LE:
      begin
        BOM := BOM_UTF32_LE;
        S.WriteBuffer(BOM, SizeOf(LongWord));
      end;
    CP_UTF32_BE:
      begin
        BOM := BOM_UTF32_BE;
        S.WriteBuffer(BOM, SizeOf(LongWord));
      end;
  end;
end;

{ TCharSet }

class function TCharSet.ByteLength(Source: Cardinal): Cardinal;
begin
  Result := Source;
end;

class function TCharSet.ByteLength(Str: Pointer): Cardinal;
begin
  Result := ByteLength(Length(Str));
end;

function TCharSet.CharInfo(Str: Pointer; const Options: TUnicodeOptions): TCharInfo;
begin
  Result := CharInfo(Str, Length(Str), Options);
end;

function TCharSet.Decode(Source: Pointer; Count: Cardinal; Dest: PWideChar;
  const Options: TUnicodeOptions): Cardinal;
begin
  with DecodeEx(Source, Count, Dest, Options) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: cannot convert into UTF-16
    Result := Count;
  end;
end;

function TCharSet.Decode(Source: Pointer; Count: Cardinal; Dest: PQuadChar;
  const Options: TUnicodeOptions): Cardinal;
begin
  with DecodeEx(Source, Count, Dest, Options) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: cannot convert into UTF-32
    Result := Count;
  end;
end;

function TCharSet.Encode(Source: PWideChar; Count: Cardinal; Dest: Pointer;
  const Options: TUnicodeOptions): Cardinal;
begin
  with EncodeEx(Source, Count, Dest, Options) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: cannot convert into <native>
    Result := Count;
  end;
end;

function TCharSet.Encode(Source: PQuadChar; Count: Cardinal; Dest: Pointer;
  const Options: TUnicodeOptions): Cardinal;
begin
  with EncodeEx(Source, Count, Dest, Options) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: cannot convert into <native>
    Result := Count;
  end;
end;

class function TCharSet.Length(Str: Pointer): Cardinal;
begin
  Result := Strings.Length(PChar(Str));
end;

{ TUnicodeCharSet }

class function TUnicodeCharSet.MaxCharBytes: Byte;
begin
  Result := 4;
end;

{ TUTF7CharSet }

function TUTF7CharSet.GetCodePage: Cardinal;
begin
  Result := CP_UTF7;
end;

class function TUTF7CharSet.MaxCharBytes: Byte;
begin
  Result := 7;
end;

{ TUTF8CharSet }

function TUTF8CharSet.GetCodePage: Cardinal;
begin
  Result := CP_UTF8;
end;

{ TEndianCharSet }

constructor TEndianCharSet.Create(BigEndian: Boolean);
begin
  FBigEndian := BigEndian;
end;

{ TUTF16CharSet }

class function TUTF16CharSet.ByteLength(Source: Cardinal): Cardinal;
begin
  Result := Source * SizeOf(WideChar);
end;

function TUTF16CharSet.CharInfo(Str: Pointer; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
begin
  Result := Strings.CharInfo(PWideChar(Str), Count, Options);
end;

function TUTF16CharSet.GetCodePage: Cardinal;
begin
  Result := CP_UTF16 + Byte(FBigEndian);
end;

class function TUTF16CharSet.Length(Str: Pointer): Cardinal;
begin
  Result := Length(Str);
end;

{ TUTF32CharSet }

class function TUTF32CharSet.ByteLength(Source: Cardinal): Cardinal;
begin
  Result := Source * SizeOf(QuadChar);
end;

function TUTF32CharSet.CharInfo(Str: Pointer; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
begin
  Result := Strings.CharInfo(PQuadChar(Str), Count, Options);
end;

function TUTF32CharSet.GetCodePage: Cardinal;
begin
  Result := CP_UTF32 + Byte(FBigEndian);
end;

class function TUTF32CharSet.Length(Str: Pointer): Cardinal;
begin
  Result := Length(Str);
end;

{ TNonUnicodeCharSet }

function TNonUnicodeCharSet.Decode(Source: PChar; Count: Cardinal;
  Dest: PChar; CharSet: TNonUnicodeCharSet): Cardinal;
begin
  with DecodeEx(Source, Count, Dest, CharSet) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: code page decode failed
    Result := Count;
  end;
end;

function TNonUnicodeCharSet.Encode(Source: PChar; Count: Cardinal;
  CharSet: TNonUnicodeCharSet; Dest: PChar): Cardinal;
begin
  with EncodeEx(Source, Count, CharSet, Dest) do
  begin
    if InvalidChar <> nil then
      raise Exception.Create; // TODO: code page encode failed
    Result := Count;
  end;
end;

class function TNonUnicodeCharSet.MaxCharBytes: Byte;
begin
  Result := 1;
end;

function TNonUnicodeCharSet.NextChar(Str: Pointer; Count: Cardinal): Pointer;
begin
  if Count <> 0 then
    Result := PChar(Str) + 1
  else
    Result := nil;
end;

function TNonUnicodeCharSet.PrevChar(Start, Current: Pointer): Pointer;
begin
  if Start <> Current then
    Result := PChar(Current) - 1
  else
    Result := nil;
end;

{ TASCIICharSet }

function TASCIICharSet.CharInfo(Str: Pointer; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
begin
  Result := Strings.CharInfo(PChar(Str), Count, Options);
end;

function TASCIICharSet.DecodeEx(Source: PChar; Count: Cardinal;
  Dest: PChar; CharSet: TNonUnicodeCharSet): TCharInfo;
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI

        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX

        XOR EDX, EDX
        MOV [EAX].TCharInfo.InvalidChar, EDX
@@loop:
        LODSB
        OR DL, AL
        STOSB // TODO: EBCDIC
        LOOP @@loop
        MOV EAX, Result
        MOV [EAX].TCharInfo.Range, EDX

        POP EDI
        POP ESI
@@exit:
end;

function TASCIICharSet.DecodeEx(Source: Pointer; Count: Cardinal;
  Dest: PWideChar; const Options: TUnicodeOptions): TCharInfo;
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI

        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        PUSH ECX

        XOR EDX, EDX
        BT dword ptr Options, uoBigEndian
        JC @@BE

        XOR AX, AX
@@LE:
        LODSB
        TEST AL, $80
        JNZ @@invalid
        OR DL, AL
        STOSW
        LOOP @@LE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@BE:
        LODSB
        TEST AL, $80
        JNZ @@invalid
        OR DL, AL
        MOV AH, AL
        XOR AL, AL
        STOSW
        LOOP @@BE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@invalid:
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ESI
@@result:
        POP EAX
        SUB EAX, ECX
        MOV [EDI].TCharInfo.Length, EAX
        MOV [EDI].TCharInfo.Range, EDX

        POP EDI
        POP ESI
@@exit:
end;

function TASCIICharSet.DecodeEx(Source: Pointer; Count: Cardinal;
  Dest: PQuadChar; const Options: TUnicodeOptions): TCharInfo;
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI

        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        PUSH ECX

        BT dword ptr Options, uoBigEndian
        JC @@BE

        XOR EAX, EAX
@@LE:
        LODSB
        TEST AL, $80
        JNZ @@invalid
        OR DL, AL
        STOSD
        LOOP @@LE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@BE:
        XOR EAX, EAX
        LODSB
        TEST AL, $80
        JNZ @@invalid
        OR DL, AL
        BSWAP EAX
        STOSD
        LOOP @@BE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@invalid:
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ESI
@@result:
        POP EAX
        SUB EAX, ECX
        MOV [EDI].TCharInfo.Length, EAX
        MOV [EDI].TCharInfo.Range, EDX

        POP EDI
        POP ESI
@@exit:
end;

function TASCIICharSet.EncodeEx(Source: PChar; Count: Cardinal;
  CharSet: TNonUnicodeCharSet; Dest: PChar): TCharInfo;
{var
  Ch: Char;
  Next: PChar;
begin
  FillChar(Result, SizeOf(Result), 0);
  while Count <> 0 do
  begin
    Ch := Source^;
    if Byte(Ch) and $80 <> 0 then
    begin
      Result.InvalidChar := Source;
      Break;
    end;
    Dest^ := Ch;
    Inc(Dest);
    with Result do
    begin
      Inc(Count);
      Range := Range or Byte(Ch);
    end;
    Next := NextChar(Source, Count);
    if Next = nil then
      Break;
    Dec(Count, Next - Source);
    Source := Next;
  end;
end;}
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI
        PUSH EBX

        MOV EBX, CharSet
        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        PUSH ECX
        XOR EDX, EDX

@@loop:
        MOV DL, [ESI] // TODO: EBCDIC
        TEST AL, $80
        JNZ @@invalid
        OR DL, AL
        STOSB
        PUSH EDX
        MOV EAX, EBX
        MOV EDX, ESI
        //MOV ECX, ECX
        CALL TCharSet.NextChar
        POP EDX
        TEST EAX, EAX
        JZ @@result
        MOV ESI, EAX
        LOOP @@loop
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@invalid:
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ESI
@@result:
        POP EAX
        SUB EAX, ECX
        MOV [EDI].TCharInfo.Length, EAX
        MOV [EDI].TCharInfo.Range, EDX

        POP EBX
        POP EDI
        POP ESI
@@exit:
end;


function TASCIICharSet.EncodeEx(Source: PWideChar; Count: Cardinal;
  Dest: Pointer; const Options: TUnicodeOptions): TCharInfo;
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI

        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        PUSH ECX
        XOR EDX, EDX

        BT dword ptr Options, uoBigEndian
        JC @@BE

        XOR EAX, EAX
@@LE:
        LODSW
        TEST AX, $FF80
        JNZ @@invalid
        OR DX, AX
        STOSB
        LOOP @@LE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@BE:
        LODSW
        XCHG AH, AL
        TEST AX, $FF80
        JNZ @@invalid
        OR DX, AX
        STOSB
        LOOP @@BE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@invalid:
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ESI
@@result:
        POP EAX
        SUB EAX, ECX
        MOV [EDI].TCharInfo.Length, EAX
        MOV [EDI].TCharInfo.Range, EDX

        POP EDI
        POP ESI
@@exit:
end;

function TASCIICharSet.EncodeEx(Source: PQuadChar; Count: Cardinal;
  Dest: Pointer; const Options: TUnicodeOptions): TCharInfo;
asm
        TEST ECX, ECX // Count
        JNZ @@start
        MOV EAX, Result
        MOV [EAX].TCharInfo.Length, ECX
        MOV [EAX].TCharInfo.Range, ECX
        MOV [EAX].TCharInfo.InvalidChar, ECX
        JMP @@exit
@@start:
        PUSH ESI
        PUSH EDI

        MOV ESI, Source
        MOV EDI, Dest
        //MOV ECX, Count
        PUSH ECX
        XOR EDX, EDX

        BT dword ptr Options, uoBigEndian
        JC @@BE

        XOR EAX, EAX
@@LE:
        LODSD
        TEST EAX, $FFFFFF80
        JNZ @@invalid
        OR EDX, EAX
        STOSB
        LOOP @@LE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@BE:
        LODSW
        BSWAP EAX
        TEST EAX, $FFFFFF80
        JNZ @@invalid
        OR EDX, EAX
        STOSB
        LOOP @@BE
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ECX
        JMP @@result
@@invalid:
        MOV EDI, Result
        MOV [EDI].TCharInfo.InvalidChar, ESI
@@result:
        POP EAX
        SUB EAX, ECX
        MOV [EDI].TCharInfo.Length, EAX
        MOV [EDI].TCharInfo.Range, EDX

        POP EDI
        POP ESI
@@exit:
end;

{ TCodePageCharSet }

function TCodePageCharSet.GetCodePage: Cardinal;
begin
  Result := FCodePage;
end;

{ TSingleByteCharSet }

constructor TSingleByteCharSet.Create(CodePage: Cardinal);
var
  Info: TCPInfoEx;
begin
  if GetCPInfoEx(CodePage, 0, Info) then
    Create(CodePage, Info.UnicodeDefaultChar)
  else
    RaiseLastPlatformError;
end;

constructor TSingleByteCharSet.Create(CodePage: Cardinal; DefaultUnicodeChar: WideChar);
var
  SourceMap: array[Low(TSingleByteMap)..High(TSingleByteMap)] of Char;
  I: Low(TSingleByteMap)..High(TSingleByteMap);
  W: WideChar;
  J: Cardinal;
  L: LongWord;
  P, F: PWideChar;
begin
  FCodePage := CodePage;
  L := $03020100;
  for J := 0 to SizeOf(SourceMap) div SizeOf(LongWord) - 1 do
  begin
    PLongWordArray(@SourceMap)[J] := L;
    Inc(L, $04040404);
  end;
  if MultiByteToWideChar(CodePage, 0, @SourceMap, SizeOf(SourceMap), @FMap,
    System.Length(FMap)) = 0
  then
    RaiseLastPlatformError;

  P := @FMap;
  J := System.Length(FMap);
  repeat
    F := Scan(DefaultUnicodeChar, P, J);
    if F = nil then
      Break;
    if (F - @FMap) <> Word(DefaultUnicodeChar) then
    begin
      F^ := UNKNOWN_UTF16;
      Dec(J, F - P + 1);
    end;
    P := F + 1;
  until False;

  FWideMapMin := High(FWideMapMin);
  for I := Low(I) to High(I) do
  begin
    W := FMap[I];
    if W > FWideMapMax then
      FWideMapMax := W
    else if W < FWideMapMin then
      FWideMapMin := W;
  end;
  FWideMap := AllocMem(Word(FWideMapMax) - Word(FWideMapMin) + 1);
  for I := Low(I) to High(I) do
    FWideMap[Word(FMap[I]) - Word(FWideMapMin)] := Char(I);
end;

destructor TSingleByteCharSet.Destroy;
begin
  FreeMem(FWideMap);
  inherited;
end;

function TSingleByteCharSet.CharInfo(Str: Pointer; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TSingleByteCharSet.DecodeEx(Source: PChar; Count: Cardinal;
  Dest: PChar; CharSet: TNonUnicodeCharSet): TCharInfo;
begin

end;

function TSingleByteCharSet.DecodeEx(Source: Pointer; Count: Cardinal;
  Dest: PWideChar; const Options: TUnicodeOptions): TCharInfo;
begin

end;

function TSingleByteCharSet.DecodeEx(Source: Pointer; Count: Cardinal;
  Dest: PQuadChar; const Options: TUnicodeOptions): TCharInfo;
begin

end;

function TSingleByteCharSet.EncodeEx(Source: PChar; Count: Cardinal;
  CharSet: TNonUnicodeCharSet; Dest: PChar): TCharInfo;
begin

end;

function TSingleByteCharSet.EncodeEx(Source: PWideChar; Count: Cardinal;
  Dest: Pointer; const Options: TUnicodeOptions): TCharInfo;
begin

end;

function TSingleByteCharSet.EncodeEx(Source: PQuadChar; Count: Cardinal;
  Dest: Pointer; const Options: TUnicodeOptions): TCharInfo;
begin

end;

function TSingleByteCharSet.GetWideMap(Index: WideChar): Char;
begin
  if (Index >= FWideMapMin) and (Index <= FWideMapMax) then
    Result := FWideMap[Word(Index) - Word(FWideMapMin)]
  else
    Result := #0;
end;

function TSingleByteCharSet.GetWideMapCount: Word;
begin
  Result := Word(FWideMapMax) - Word(FWideMapMin) + 1;
end;

{ TMultiByteCharSet }

constructor TMultiByteCharSet.Create(CodePage: Cardinal;
  LeadByteRanges: PLeadByteRanges; LeadByteRangeCount: Cardinal);
var
  I: Integer;
begin
  FCodePage := CodePage;
  for I := 0 to LeadByteRangeCount - 1 do
    with LeadByteRanges[I] do
      Inc(FLeadByteCount, Byte(Max) - Byte(Min) + 1);
  GetMem(FMap, FLeadByteCount * SizeOf(TDoubleByteMap)); ;
end;

destructor TMultiByteCharSet.Destroy;
begin
//  FreeMem(FLeadBytes);
  inherited;
end;

function TMultiByteCharSet.CharInfo(Str: Pointer; Count: Cardinal;
  const Options: TUnicodeOptions): TCharInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
{  while (S <> nil) and (PChar(S)^ <> #0) do
  begin
    S := NextChar(S, MaxLength);
    Inc(Result);
  end;}
end;

function TMultiByteCharSet.GetMap(const Source: DoubleChar): WideChar;
var
  Index: Integer;
begin
  if Byte(Source.SingleByte) and $80 <> 0 then
  begin
    Index := IndexOf(Source.LeadByte);
    if Index >= 0 then
      Result := FMap[Index].TrailByteMap[Source.TrailByte]
    else
      Result := UNKNOWN_UTF16;
  end
  else
    Result := WideChar(Source.SingleByte);
end;

function TMultiByteCharSet.GetWideMap(Source: WideChar): DoubleChar;
begin

end;

function TMultiByteCharSet.IndexOf(LeadByte: Char): Integer;
begin

end;

{ MIME support }

//const
//  MIME

function MIME_CodePage(Charset: PCoreChar): Cardinal;
begin
  Result := Cardinal(-1);
end;

function MIME_Charset(CodePage: Cardinal; Charset: TString): Boolean;
begin
  Result := False;
end;

function MIME_Charset(BigEndian, QuadChar: Boolean; Charset: TString): Boolean;
const
  CodePage: array[Boolean] of Cardinal = (CP_UTF16, CP_UTF32);
begin
  Result := MIME_Charset(CodePage[QuadChar] + Byte(BigEndian), Charset);
end;

{ Helper functions }

function DataBuffer(Data: PStringData): Pointer;
begin
  if Data.RefCount < 0 then
    Result := PStringConst(Data).Buffer
  else
    Result := @PStringVar(Data).Buffer;
end;

{ TString }

constructor TString.Create;
begin
  FIndex := -1;
end;

destructor TString.Destroy;
begin
  Clear;
  inherited;
end;

procedure TString.Append(Ch: Char; CodePage: Cardinal; Count: Integer);
begin
  InsertAt(FLength, Ch, CodePage, Count);
end;

procedure TString.Append(Ch: QuadChar; BigEndian: Boolean; Count: Integer);
begin
  InsertAt(FLength, Ch, BigEndian, Count);
end;

procedure TString.Append(S: PChar; ByteCount: Integer;
  CodePage: Cardinal; Count: Integer);
begin
  InsertAt(FLength, S, ByteCount, CodePage, Count);
end;

procedure TString.Append(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Count: Integer);
begin
  InsertAt(FLength, S, CharCount, BigEndian, Count);
end;

procedure TString.Append(S: TString; Count: Integer);
begin
  InsertAt(FLength, S, Count);
end;

procedure TString.Assign(S: TString);
begin
  if (S <> nil) and (S.FData <> nil)
    {$IFDEF Multithread} and (S.FData.RefCount <> 0) {$ENDIF} then
  begin
    if AssignData(S) then
    begin
      FIndex := 0;
      FLength := S.FLength;
    end;
  end
  else
    Clear;
end;

procedure TString.AssignData(S: Pointer; Count: Integer; CodePage: Cardinal;
  Index: Integer; AttachToBuffer: Boolean);
var
  Data: PStringData;
begin
  Data := NewData(S, Count, CodePage, Index, AttachToBuffer, False);
  if Data <> nil then
  begin
    if AttachToBuffer then
      FIndex := Index
    else
      FIndex := 0;
    FLength := Data.Length;
    FData := Data;  // thread-safe assignment order
  end
  else
  begin
    FreeMemAndNil(FData);  // thread-safe free order
    FLength := 0;
    FIndex := -1;
  end;
end;

function TString.AssignData(S: TString): Boolean;
begin
  if (S <> Self) and (S.FData.RefCount <> 0) then
  begin
    if S.FData.RefCount > 0 then
    {$IFDEF Multithread}
      InterlockedIncrement(S.FData.RefCount);
    {$ELSE}
      Inc(S.FData.RefCount);
    {$ENDIF}
    FData := S.FData;
    Result := True;
  end
  else
    Result := False;
end;

procedure TString.CheckCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    case FData.CodePage of
      CP_UTF16_LE, CP_UCS2_BE, CP_UTF32_LE, CP_UTF32_BE:
        begin
          CodePage[True] := CP_ACP;
          CheckIndex(Value);
          Exit;
        end;
    end;
    CheckIndex(Value);
    if ForUpdate then
      SetData(nil, FLength, FData.CodePage);
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.CheckIndex(Index: Integer);
begin
  if not HasIndex(Index) then
    Error(E_INDEX_OUT_OF_BOUNDS);
end;

procedure TString.CheckQuadCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    if not Strings.IsQuadChar(FData.CodePage) then
    begin
      IsQuadChar := True;
      CheckIndex(Value);
    end
    else
    begin
      CheckIndex(Value);
      if ForUpdate then
        SetData(nil, FLength, FData.CodePage);
    end;
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.CheckWideCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    if not Strings.IsWideChar(FData.CodePage) then
    begin
      IsQuadChar := True;
      CheckIndex(Value);
    end
    else
    begin
      CheckIndex(Value);
      if ForUpdate then
        SetData(nil, FLength, FData.CodePage);
    end;
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.Clear;
begin
  AssignData(nil, 0, 0, 0, False);
end;

function TString.CodePageInfo: TCodePageInfo;
begin
  Result := Strings.CodePageInfo(CodePage[False]);
end;

function TString.CompareWith(S: TString; CaseSensitive: Boolean): Integer;
var
  CodePage: Cardinal;
begin
  if S <> nil then
    if FData <> nil then
      if S.FData <> nil then
      begin
        CodePage := DestCodePage(FData.CodePage, S.FData.CodePage);
        case CodePage of
          CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
            Result := CompareStringW(LOCALE_USER_DEFAULT, Integer(not CaseSensitive),
              AsWideChar[False], Length, S.AsWideChar[False], S.Length);
        else
          Result := CompareString(LOCALE_USER_DEFAULT, Integer(not CaseSensitive),
            AsChar[CodePage], Length, S.AsChar[CodePage], S.Length);
        end;
      end
      else
        Result := 1
    else
      Result := S.Length
  else
    Result := Length;
end;

function TString.Data: Pointer;
begin
  if FData <> nil then
  begin
    Result := DataBuffer(FData);
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        Result := PWideChar(Result) + FIndex;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := @QuadString(Result)[FIndex] // Delphi 6 workaround!
    else
      Result := PChar(Result) + FIndex;
    end;
  end
  else
    Result := nil;
end;

procedure TString.Error(Code: Integer);
begin
  RunError(219); // invalid typecast
end;

procedure TString.Format(S: TString; const Args: array of const);
var
  P: PChar;
begin
  if S <> nil then
  begin
    P := S.Data;
    if P <> nil then
    begin
      Format(P, S.FLength, S.FData.CodePage, Args); // hack :)
      Exit;
    end;
  end;
  Length := 0;
end;

procedure TString.Format(S: PChar; ByteCount: Integer; CodePage: Cardinal;
  const Args: array of const);
var
  Buf: array[0..2047] of Char;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Format(Pointer(S), ByteCount div SizeOf(WideChar), CodePage = CP_UTF16_BE, Args);
    CP_UTF32_LE, CP_UTF32_BE:
      Error(E_CANNOT_FORMAT_UTF32);
  else
    SetData(Buf, FormatBuf(S, Args, Buf), CodePage);
  end;
end;

procedure TString.Format(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; const Args: array of const);
var
  Buf: array[0..2047] of WideChar;
begin
  if BigEndian then
    Error(E_CANNOT_FORMAT_BE)
  else
    SetData(Buf, WideFormatBuf(S, Args, Buf), False);
end;

function TString.GetAsChar(CodePage: Cardinal): PChar;
begin
  AssignData(nil, FLength, CodePage, 0, False);
  Result := nil;
//  Result := @FData.Chars[FIndex];
end;

function TString.GetAsFloat: Double;
begin
  if not TryAsFloat(Result) then
    Error(E_CANNOT_CONVERT_TO_FLOAT);
end;

function TString.GetAsInt64: Int64;
begin
  if not TryAsInt64(Result) then
    Error(E_CANNOT_CONVERT_TO_INT64);
end;

function TString.GetAsInteger: LongInt;
begin
  if not TryAsInteger(Result) then
    Error(E_CANNOT_CONVERT_TO_INT);
end;

function TString.GetAsString(CodePage: Cardinal): LegacyString;
begin
  System.SetLength(Result, GetData(nil, 0, CodePage));
  GetData(Pointer(Result), System.Length(Result), CodePage);
end;

function TString.GetAsWideChar(BigEndian: Boolean): PWideChar;
begin
  AssignData(nil, FLength, CP_UTF16 + Byte(BigEndian), 0, False);
  Result := nil;
//  Result := @FData.WideChars[FIndex];
end;

function TString.GetAsWideString(BigEndian: Boolean): WideString;
begin
  System.SetLength(Result, GetData(nil, 0, BigEndian));
  GetData(Pointer(Result), System.Length(Result), BigEndian);
end;

function TString.GetByteLength: Integer;
begin
  Result := FLength shl Integer(CodePageInfo.CodePageType);
end;

function TString.GetChar(Index: Integer): Char;
begin
  CheckCharIndex(Index, False);
  Result := PChar(DataBuffer(FData))[FIndex + Index];
end;

function TString.GetCharInfo: Integer;
begin
  Result := GetData(nil, 0, CP_UTF32) div SizeOf(QuadChar);
end;

function TString.GetCodePage(Translate: Boolean): Cardinal;
begin
  if FData <> nil then
    if Translate then
      Result := TranslateCodePage(FData.CodePage)
    else
      Result := FData.CodePage
  else
    Result := Cardinal(-1);
end;

function TString.GetData(S: PChar; ByteCount: Integer;
  CodePage: Cardinal): Integer;
begin
  if not GetData(S, ByteCount, CodePage, Result) then
  begin
    Error(E_CANNOT_CONVERT_TO_MULTIBYTE);
    Result := 0; // FData is null
  end;
end;

function TString.GetData(S: PChar; ByteCount: Integer;
  CodePage: Cardinal; var BytesWritten: Integer): Boolean;
var
  P: PWideChar;
  CharCount: Integer;
  Buffer: PChar;
begin
  if FData <> nil then
  begin
    if Strings.IsWideChar(FData.CodePage) then
    begin
      Result := GetData(Pointer(S), ByteCount div SizeOf(WideChar),
        CodePage = CP_UTF16_BE, CharCount);
      BytesWritten := CharCount * SizeOf(WideChar);
      Exit;
    end;

    if ByteCount > FLength then
      BytesWritten := FLength
    else
      BytesWritten := ByteCount;

    if TranslateCodePage(FData.CodePage) <> TranslateCodePage(CodePage) then
    begin
      Buffer := PChar(DataBuffer(FData)) + FIndex;
      CharCount := {$IFDEF Tricks} System. {$ENDIF}
        MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
          Buffer, BytesWritten, nil, 0);
      GetMem(P, CharCount * SizeOf(WideChar));
      try
        CharCount := {$IFDEF Tricks} System. {$ENDIF}
          MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
            Buffer, BytesWritten, P, CharCount);
        BytesWritten := {$IFDEF Tricks} System. {$ENDIF}
          WideCharToMultiByte(CodePage, 0, P, CharCount, S, BytesWritten,
            nil, @Result);
        Result := not Result; // no default chars
      finally
        FreeMem(P);
      end;
    end
    else
    begin
      Move(PChar(DataBuffer(FData))[FIndex], S[0], BytesWritten);
      Result := True;
    end;
  end
  else
    Result := False;
end;

function TString.GetData(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean): Integer;
begin
  if not GetData(S, CharCount, BigEndian, Result) then
  begin
    Error(E_CANNOT_CONVERT_TO_UTF16);
    Result := 0; // FData is null
  end;
end;

function TString.GetData(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; var CharsWritten: Integer): Boolean;
begin
  if FData <> nil then
  begin
    if Strings.IsWideChar(FData.CodePage) then
    begin
      if CharCount > FLength then
        CharsWritten := FLength
      else
        CharsWritten := CharCount;

      if FData.CodePage = CP_UTF16 + Byte(BigEndian) then
        Move(PWideChar(DataBuffer(FData))[FIndex], S[0], CharsWritten * SizeOf(WideChar))
      else
        SwapWideCharBytes(PWideChar(DataBuffer(FData)) + FIndex, S, CharsWritten);

      Result := True;
    end
    else
    begin
      CharsWritten := {$IFDEF Tricks} System. {$ENDIF}
        MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
          PChar(DataBuffer(FData)) + FIndex, FLength, S, CharCount);
      if BigEndian then
        SwapWideCharBytes(S, S, CharsWritten);
      Result := CharsWritten <> 0;
    end;
  end
  else
    Result := False;
end;

function TString.GetIsQuadChar: Boolean;
begin
  Result := (FData <> nil) and Strings.IsQuadChar(FData.CodePage);
end;

function TString.GetIsWideChar: Boolean;
begin
  Result := (FData <> nil) and Strings.IsWideChar(FData.CodePage);
end;

function TString.GetQuadChar(Index: Integer): QuadChar;
begin
  CheckQuadCharIndex(Index, False);
  // Sic! Delphi 6 could not compile it with PQuadChar
  Result := QuadString(DataBuffer(FData))[FIndex + Index];
end;

function TString.GetWideChar(Index: Integer): WideChar;
begin
  CheckWideCharIndex(Index, False);
  Result := PWideChar(DataBuffer(FData))[FIndex + Index];
end;

function TString.HasIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FLength);
end;

function TString.IndexOf(Ch: Char; CodePage: Cardinal; Index,
  Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(Ch: QuadChar; BigEndian: Boolean; Index,
  Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: PChar; ByteCount: Integer;
  CodePage: Cardinal; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: TString; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

procedure TString.IndexOf(Source: TString; Ch: Char;
  CodePage: Cardinal; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(Ch, CodePage, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; Ch: QuadChar;
  BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(Ch, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; S: PChar;
  ByteCount: Integer; CodePage: Cardinal; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, ByteCount, CodePage, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, CharCount, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source, S: TString; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

function TString.IndexOfChar(S: TString; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOfChar(S: PChar; ByteCount: Integer;
  Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOfChar(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

procedure TString.IndexOfChar(Source: TString; S: PChar;
  ByteCount: Integer; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, ByteCount, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOfChar(Source: TString; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, CharCount, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOfChar(Source, S: TString; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.Insert(Index: Integer; Ch: Char; CodePage: Cardinal;
  Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, Ch, CodePage, Count);
end;

procedure TString.Insert(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
  Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, Ch, BigEndian, Count);
end;

procedure TString.Insert(Index: Integer; S: PChar; ByteCount: Integer;
  CodePage: Cardinal; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, ByteCount, CodePage, Count);
end;

procedure TString.Insert(Index: Integer; S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, CharCount, BigEndian, Count);
end;

procedure TString.Insert(Index: Integer; S: TString; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, Count);
end;

procedure TString.InsertAt(Index: Integer; Ch: Char;
  CodePage: Cardinal; Count: Integer);
var
  CP: Cardinal;
  Q: QuadChar;
begin
  if FData <> nil then
    CP := DestCodePage(FData.CodePage, CodePage)
  else
    CP := CodePage;
  AssignData(nil, FLength + Count * SizeOf(Ch), CP, Index, False);
  if TranslateCodePage(CP) <> TranslateCodePage(CodePage) then
    case CP of
      CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
        if {$IFDEF Tricks} System. {$ENDIF}
          MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS,
            @Ch, SizeOf(Ch), @Q, SizeOf(Ch)) <> 0 then
        begin
           case CP of
             CP_UTF16_BE:
               LongRec(Q).Lo := Swap(LongRec(Q).Lo);
             CP_UTF32_BE:
               asm
                 MOV EDX, Q
                 BSWAP EDX
                 XOR DX, DX
                 MOV Q, EDX
               end;
           end;
           case CP of
             CP_UTF16_LE, CP_UTF16_BE:
               begin
               //  FillChar(FData.WideChars[FIndex + Index], Count, LongRec(Q).Lo);
                 Exit;
               end;
             CP_UTF32_LE, CP_UTF32_BE:
               begin
               //  FillChar(FData.QuadChars[FIndex + Index], Count, Q);
                 Exit;
               end;
           end;
        end
        else
          Error(E_CANNOT_CONVERT_TO_UTF16);
    end;
//  FillChar(FData.Chars[FIndex + Index], Count, Ch);
end;

procedure TString.InsertAt(Index: Integer; Ch: QuadChar;
  BigEndian: Boolean; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: PChar;
  ByteCount: Integer; CodePage: Cardinal; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: TString;
  Count: Integer);
begin

end;

procedure TString.Load(S: TReadableStream; DefaulTCharacterSet: Cardinal;
  AutoDetect: TAutoDetectCodePage);

{var
  BOM: LongRec;
  ByteCount: Cardinal;
  T: MultiByteString;
  BOMLen: Byte;

procedure LoadUTF(UTF: LongWord; CP: Cardinal);
begin
  if BOM.Lo = LongRec(UTF).Lo then
  begin
    ByteCount := S.Read(BOM.Hi, 1);
    if ByteCount <> 0 then
    begin
      if BOM.Bytes[2] = LongRec(UTF).Bytes[2] then
      begin
        if CodePage <> nil then
          CodePage^ := CP;
        Result := LoadMultibyteText(S, CP);
        Exit;
      end;
      BOMLen := 3;
    end
    else
    begin
      SetString(Result, PChar(@BOM), SizeOf(BOM.Lo));
      Exit;
    end;
  end
  else
    BOMLen := 2;
end;}

procedure LoadUTF16(BigEndian: Boolean);
begin
  AssignData(nil, (S.Size - S.Position) div SizeOf(WideChar),
    CP_UTF16 + Byte(BigEndian), 0, False);
//  FLength := S.Read(FData.WideChars, FLength);
end;

begin
{  if cpBOM in AutoDetect then
  ByteCount := S.Read(BOM.Lo, SizeOf(BOM.Lo));
  if ByteCount = SizeOf(BOM.Lo) then
    case BOM.Lo of
      BOM_UTF16_LE:
        LoadUTF16(False);
      BOM_BE16:
        LoadUTF16(True);
    else
      if Word(BOM) = 0 then
        // TODO: LoadUTF32(True) // 0 = no BOM, means big-endian by ISO/IEC convensions
      else if BOM.Lo = 0 then
        // TODO: BOM.Hi + LoadUTF16(True) // see the comment above :)
      else
      LoadUTF(BOM_UTF8, CP_UTF8);
      if Result <> '' then
        Exit;
      LoadUTF(BOM_UTF7, CP_UTF7);
      if Result <> '' then
        Exit;
      SetLength(T, S.GetSize - S.GetPosition + BOMLen);
      Move(BOM, T[1], BOMLen);
      SetLength(T, S.Read(T[BOMLen + 1], Length(T) - BOMLen) + BOMLen);
      if AutoDetect then
      begin
        if IsTextUTF8(T) then
        begin
          if CodePage <> nil then
            CodePage^ := CP_UTF8;
          Result := DecodeString(T, CP_UTF8);
          Exit;
        end;
        if IsTextUTF7(T) then
        begin
          if CodePage <> nil then
            CodePage^ := CP_UTF7;
          Result := DecodeString(T, CP_UTF7);
          Exit;
        end;
      end;
      Result := T;
    end
  else
    AssignData(@BOM, ByteCount, DefaulTCharacterSet, 0, False);}
end;

procedure TString.LowerCase;
begin
  if FData <> nil then
  begin
    CodePage[True] := CP_UTF16;
//    CharLowerBuffW(@FData.Buffer, FLength);
  end;
end;

procedure TString.LowerCase(S: TString);
begin
  if (S <> nil) and (S.FData <> nil) then
  begin
    SetLength(S.Length);
//    FLength := S.GetData(@FData.Buffer, Length, False);
//    CharLowerBuffW(@FData.Buffer, FLength);
  end
  else
    Clear;
end;

function TString.NewData(S: Pointer; Count: Integer; CodePage: Cardinal;
  Index: Integer; AttachToBuffer, KeepData: Boolean): PStringData;
var
  Bytes, CharBytes, NewSize: Integer;
  OldData: PStringData;
begin
  OldData := nil;

  if (FData <> nil) and (FData.RefCount > 0) then // 0 means data being in other transaction
  begin
  {$IFDEF Multithread}
    if InterlockedDecrement(FData.RefCount) <> 0 then
  {$ELSE}
    Dec(FData.RefCount);
    if FData.RefCount <> 0 then
  {$ENDIF}
      OldData := FData;
  end;

  if Count <> 0 then
  begin
    case CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        begin
          CharBytes := SizeOf(WideChar);
          Bytes := Count * SizeOf(WideChar);
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        begin
          CharBytes := SizeOf(QuadChar);
          Bytes := Count * SizeOf(WideChar);
        end;
    else
      CharBytes := SizeOf(Char);
      Bytes := Count;
    end;

    if AttachToBuffer then
      NewSize := SizeOf(TStringData)
    else                                                                 // #0
      NewSize := SizeOf(TStringData) - SizeOf(Pointer) + Index + Bytes + CharBytes;

    if (OldData <> nil) and (not KeepData or
      (TranslateCodePage(OldData.CodePage) = TranslateCodePage(CodePage))) then
    begin
      Result := FData;
      ReallocMem(Result, NewSize);
    end
    else
    begin
      GetMem(Result, NewSize);

      if AttachToBuffer then
//        Result.Buffer := S
      else
        if S <> nil then
//          Move(S^, PChar(@Result.Buffer)[Index * CharBytes], Bytes)
        else
//          if not GetData(PChar(@Result.Buffer) + Index * CharBytes, Bytes,
//            CodePage, NewSize) and KeepData then
          begin
            FreeMemAndNil(Result);
            if OldData = nil then
              Inc(FData.RefCount); // =0 meaning data renewal
            Exit;
          end;
    end;

    if AttachToBuffer then
      Result.RefCount := -1
    else
    begin
      {case CodePage of
        CP_UTF16_LE, CP_UTF16_BE:
          PWideChar(@Result.Buffer)[Count] := WideChar(0);
        CP_UTF32_LE, CP_UTF32_BE:
          QuadString(@Result.Buffer)[Count] := QuadChar(0);
      else
        PChar(@Result.Buffer)[Count] := #0;
      end;}
      Result.RefCount := 1;
    end;

    Result.CodePage := CodePage;
    Result.Length := Count;

  {$IFDEF Multithread}
    FreeMemAndNil(OldData);
  {$ELSE}
    FreeMem(OldData);
  {$ENDIF}
  end
  else
    Result := nil;
end;

function TString.NextChar(Index: Integer): Integer;
var
  P: Pointer;
begin
  if HasIndex(Index + 1) then
    case FData.CodePage of
      CP_UTF16_LE:
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          if (WordRec(P^).Hi in [$DC..$DF]) and
            (WordRec(PWordArray(P)[1]).Hi in [$D8..$DB]) and (Index + 2 < FLength)
          then
            Result := Index + 2
          else
            Result := Index + 1;
        end;
      CP_UTF16_BE:
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          if (WordRec(P^).Lo in [$D8..$DB]) and
            (WordRec(PWordArray(P)[1]).Lo in [$DC..$DF]) and (Index + 2 < FLength)
          then
            Result := Index + 2
          else
            Result := Index + 1;
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := Index + 1;
    else
      P := PChar(DataBuffer(FData)) + FIndex + Index;
      // TODO: UTF7/8 (not supported by CharNextExA)
      Result := CharNextExA(FData.CodePage, P, 0) - PChar(P) + Index;
    end
  else
    Result := -1;
end;

function TString.PrevChar(Index: Integer): Integer;
var
  P: Pointer;
  W: PWideChar;
begin
  if HasIndex(Index - 1) then
    case FData.CodePage of
      CP_UTF16_LE: // CharPrevW does not support UTF-16 surrogates
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          W := P;
          if WordRec(W^).Hi in [$D8..$DB] then
            Dec(W);
          if WordRec(W^).Hi in [$DC..$DF] then
          begin
            Dec(W);
            if WordRec(W^).Hi in [$D8..$DB] then
              Dec(W);
          end
          else
            Dec(W);
          Result := Index - (PWideChar(P) - W);
        end;
      CP_UTF16_BE: // Windows has no UTF-16 BE support
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          W := P;
          if WordRec(W^).Lo in [$DC..$DF] then
            Dec(W);
          if WordRec(W^).Lo in [$D8..$DB] then
          begin
            Dec(W);
            if WordRec(W^).Lo in [$DC..$DF] then
              Dec(W);
          end
          else
            Dec(W);
          Result := Index - (PWideChar(P) - W);
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := Index - 1;
    else
      P := PChar(DataBuffer(FData)) + FIndex;
      // TODO: UTF7/8 (isn't supported by CharPrevExA)
      Result := PChar(P) + Index - CharPrevExA(FData.CodePage, P, PChar(P) + Index, 0);
    end
  else
    Result := -1;
end;

function TString.Save(S: TWriteableStream; WriteBOM: Boolean): Boolean;
var
  P: Pointer;
begin
  P := Data;
  if P <> nil then
  begin
    if WriteBOM then
      Strings.WriteBOM(S, FData.CodePage);
    S.WriteBuffer(P^, ByteLength);
    Result := True;
  end
  else
    Result := False;
end;

procedure TString.SetAsChar(CodePage: Cardinal; Value: PChar);
var
  Count: Integer;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Count := Strings.Length(PWideChar(Value));
    CP_UTF32_LE, CP_UTF32_BE:
      Count := Strings.Length(PQuadChar(Value));
  else
    Count := Strings.Length(PChar(Value));
  end;
  AssignData(Value, Count, CodePage, 0, False);
end;

procedure TString.SetAsFloat(const Value: Double);
var
  S: string[24];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsInt64(Value: Int64);
var
  S: string[24];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsInteger(Value: Integer);
var
  S: string[12];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsString(CodePage: Cardinal; const Value: LegacyString);
begin
  SetData(Pointer(Value), System.Length(Value), CodePage,
    PLongInt(PChar(Pointer(Value)) - SizeOf(Integer) - SizeOf(LongInt))^  < 0); // Borland magic
end;

procedure TString.SetAsWideChar(BigEndian: Boolean; Value: PWideChar);
begin
  AssignData(Value, Strings.Length(Value), CP_UTF16 + Byte(BigEndian), 0, False);
end;

procedure TString.SetAsWideString(BigEndian: Boolean; const Value: WideString);
begin
  AssignData(Pointer(Value), System.Length(Value), CP_UTF16 + Byte(BigEndian), 0, False);
end;

procedure TString.SetByteLength(Value: Integer);
begin
  if FData <> nil then
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        Value := Value div SizeOf(WideChar);
      CP_UTF32_LE, CP_UTF32_BE:
        Value := Value div SizeOf(QuadChar);
    end;
{$IFDEF Unicode}
  Length := Value div SizeOf(WideChar);
{$ELSE}
  Length := Value;
{$ENDIF}
end;

procedure TString.SetChar(Index: Integer; Value: Char);
begin
  CheckCharIndex(Index, True);
//  FData.Chars[FIndex + Index] := Value;
end;

procedure TString.SetCharInfo(Value: Integer);
begin
  if FData <> nil then
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
        { Value := Value } ; // :)
    else
      Value := Value * CodePageInfo.MaxCharBytes;
    end;
  Length := Value;
end;

procedure TString.SetCodePage(Translate: Boolean; Value: Cardinal);
begin
  if not TryCodePage(Value, Translate) then
    Error(E_CANNOT_CONVERT_TO_CODEPAGE);
end;

procedure TString.SetData(S: PChar; ByteCount: Integer;
  CodePage: Cardinal; AttachToBuffer: Boolean);
var
  Count: Integer;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Count := ByteCount div SizeOf(WideChar);
    CP_UTF32_LE, CP_UTF32_BE:
      Count := ByteCount div SizeOf(QuadChar);
  else
    Count := ByteCount;
  end;
  AssignData(S, Count, CodePage, 0, AttachToBuffer);
end;

procedure TString.SetData(S: PWideChar; CharCount: Integer; BigEndian,
  AttachToBuffer: Boolean);
begin
  AssignData(S, CharCount, CP_UTF16 + Byte(BigEndian), 0, AttachToBuffer);
end;

procedure TString.SetIsQuadChar(Value: Boolean);
begin
  if not IsQuadChar then
    CodePage[True] := CP_UTF32;
end;

procedure TString.SetIsWideChar(Value: Boolean);
begin
  if not IsWideChar then
    CodePage[True] := CP_UTF16;
end;

procedure TString.SetLength(Value: Integer);
var
  CodePage: Cardinal;
begin
  if FData <> nil then
    CodePage := FData.CodePage
  else
  {$IFDEF Unicode}
    CodePage := CP_UTF16;
  {$ELSE}
    CodePage := CP_ACP;
  {$ENDIF}
  AssignData(nil, Value, CodePage, 0, False);
end;

procedure TString.SetQuadChar(Index: Integer; Value: QuadChar);
begin
  CheckQuadCharIndex(Index, True);
//  FData.QuadChars[FIndex + Index] := Value;
end;

procedure TString.SetWideChar(Index: Integer; Value: WideChar);
begin
  CheckWideCharIndex(Index, True);
//  FData.WideChars[FIndex + Index] := Value;
end;

procedure TString.SubString(S: TString; Index: Integer);
begin
  SubString(S, Index, S.Length - Index);
end;

procedure TString.SubString(S: TString; Index, Length: Integer);
begin
  if (S <> nil) and (S.FData <> nil)
    {$IFDEF Multithread} and (S.FData.RefCount <> 0) {$ENDIF} then
  begin
    S.CheckIndex(Index);
    S.CheckIndex(Length - 1);
    if AssignData(S) then
    begin
      FIndex := Index;
      FLength := Length;
    end
    else
    begin
      Inc(FIndex, Index);
      FLength := Length;
    end;
  end
  else
    Clear;
end;

function TString.TryAsFloat(var Value: Double): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryAsInt64(var Value: Int64): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryAsInteger(var Value: Integer): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryCodePage(CodePage: Cardinal; Translate: Boolean): Boolean;
var
  CP: Integer;
  AttachToBuffer: Boolean;
  Data: PStringData;
begin
  if FData <> nil then
  begin
    if Translate then
    begin
      CP := CodePage;
      AttachToBuffer := False;
    end
    else
    begin
      CP := FData.CodePage;
      AttachToBuffer := FData.RefCount < 0;
    end;

    Data := NewData(nil, FLength, CP, 0, AttachToBuffer, True);
    if Data <> nil then
    begin
      FIndex := 0;
      FLength := FData.Length;
      Data.CodePage := CodePage;
      FData := Data;
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    GetMem(FData, SizeOf(TStringData) - SizeOf(Pointer));
    FData.RefCount := 1;
    FData.CodePage := CodePage;
    FData.Length := 0;

    FIndex := 0;
    FLength := 0;

    Result := True;
  end;
end;

procedure TString.UpperCase;
begin

end;

procedure TString.UpperCase(S: TString);
begin

end;

{ TCoreString }

procedure TCoreString.Error(Code: Integer);
begin
  // TODO: exception
end;

procedure TCoreString.SysErrorMessage(ErrorCode: Integer);
var
  Msg: array[0..$FF] of CoreChar;
begin
  SetData(Msg, Exceptions.SysErrorMessage(ErrorCode, Msg, SizeOf(Msg)),
    {$IFDEF Unicode} False {$ELSE} CP_ACP {$ENDIF} );
end;

{function NextUtf8Char(var Index: Cardinal; var Char: QuadChar): Boolean;
var
  B, Bytes: Byte;
begin
  B := Byte(FSource[Index]);
  Inc(Index);
  if B and $80 <> 0 then
  begin
    Bytes := 0;
    while B and (1 shl (6 - Bytes)) <> 0 do
      Inc(Bytes);
    if Bytes <> 0 then
    begin
      Char := B and (1 shl (7 - Bytes) - 1);
      while Index < Count do
      begin
        B := Byte(FSource[Index]);
        if B and $C0 = $80 then
        begin
          Char := (Char shl 6) or (B and $3F);
          Dec(Bytes);
          if Bytes = 0 then
          begin
            if Char = 0 then
              Compliance := cpModified;
            Result := True;
            Exit;
          end;
        end
        else
          Break;
        Inc(Index);
      end;
    end;
    Result := False;
    Exit;
  end;
  Char := B;
  Result := True;
end;}


end.


