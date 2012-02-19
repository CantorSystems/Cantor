(*
    The Unified Environment Core Library

    Core string and character set implementation 

    Copyright (c) 2012 The Unified Environment Laboratory

    Conditional defines:
      * Lite -- strip support of:
        * TString.Language
        * TString.Length(Source, MaxLength)
      * Latin -- Latin charset support (ISO-8859-1)
      * UTF32 -- UTF-32 charset support
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreClasses, Exceptions, CoreConsts;

{$I Unicode.inc}

type
  TStringOption = (soAttachBuffer, soBigEndian, soDetectCharSet);
  TStringOptions = set of TStringOption;

const
  soLatin1 = soBigEndian;

type
  TLegacyOptions = set of soAttachBuffer..soAttachBuffer;
  TLatinOptions = set of soAttachBuffer..soLatin1;
  TEndianOptions = set of soAttachBuffer..soBigEndian;

  TLegacySource = set of soDetectCharSet..soDetectCharSet;
  TLatinSource = set of soLatin1..soDetectCharSet;
  TEndianSource = set of soBigEndian..soBigEndian;

  TConvertOption = (coCase, coKana, coPunctuation, coNonSpace, coWidth, coHanzi,
    coTurkic, coDiacritics, coComposition, coForceInvalid, coRangeBlocks,
    coCompatibility, coSurrogates, coBigEndian);

const
  coLatin1 = coCompatibility;
  coAllowNonUnicode = coSurrogates;
  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;
  coModifiedUTF8 = [coCESU8, coEncodedZero];

  coNFC = [];
  coNFD = [coComposition];
  coNFKC = [coCompatibility];
  coNFKD = [coComposition, coCompatibility];

type
  TEncodeLegacy = set of coNonSpace..coRangeBlocks;
  TEncodeLatin = set of coNonSpace..coLatin1;
  TEncodeEndian = set of coComposition..coBigEndian;
  TEncodeUTF8 = set of coComposition..coEncodedZero;
  TUTF8Compliance = set of coCESU8..coEncodedZero;

  PLatinStrInfo = ^TLatinStrInfo;
  TLatinStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, Latin1Count, DiacriticCount: Cardinal);
  end;

  PSingleByteStrInfo = ^TSingleByteStrInfo;
  TSingleByteStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount, NonLatinCount, DiacriticCount: Cardinal);
  end;

  PDoubleByteStrInfo = ^TDoubleByteStrInfo;
  TDoubleByteStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, DoubleByteCount, DiacriticCount, NonWidthCount: Cardinal);
  end;

  PUTF8StrInfo = ^TUTF8StrInfo;
  TUTF8StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount,
          NonUnicodeCount, UnicodeCount, ZeroCount: Cardinal;
          Compliance: TUTF8Compliance);
  end;

  PUTF16StrInfo = ^TUTF16StrInfo;
  TUTF16StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: PWideChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount: Cardinal);
  end;

  PUTF32StrInfo = ^TUTF32StrInfo;
  TUTF32StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: PQuadChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount, NonUnicodeCount: Cardinal);
  end;

{ Legacy Windows service }

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

{ Code page support }

type
  TCodePage = class
  private
    FNumber: Word;
    FName: PWideChar;
  {$IFNDEF Lite}
    FBlocks: TUnicodeBlocks;
  {$ENDIF}
  public
    class function MaxCharBytes: Byte; virtual; abstract;

  {$IFDEF Latin}
    function DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeLegacy = []): Cardinal; overload;

    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      Options: TEncodeLegacy = []): Cardinal; overload;
  {$ENDIF}

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PQuadChar; Options: TEncodeLegacy = []): Cardinal; overload;

    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      Options: TEncodeEndian = []): Cardinal; overload;
  {$ENDIF}

    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PWideChar; Options: TEncodeLegacy = []): Cardinal; overload;

    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeEndian= []): Cardinal; overload;

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeLegacy = []): Cardinal; overload;

    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeUTF8 = []): Cardinal; overload;

  // properties
    property Number: Word read FNumber;
    property Name: PWideChar read FName;
  {$IFNDEF Lite}
    property Blocks: TUnicodeBlocks read FBlocks;
  {$ENDIF}
  end;

  TWideString = class;

  // mapping from 0 for EBCDIC compliance
  TSingleByteMap = array[LegacyChar] of WideChar;

  TMemoryCodePage = class(TCodePage)
  private
    FSingleByteMap: TSingleByteMap;
    FWideMapLo, FWideMapHi: WideChar;
    function GetWideMapCount: Word;
  protected
    property SingleByteMap: TSingleByteMap read FSingleByteMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;

  // properties
    property WideMapCount: Word read GetWideMapCount;
    property WideMapLo: WideChar read FWideMapLo;
    property WideMapHi: WideChar read FWideMapHi;
  end;

  TSingleByteMemCodePage = class(TMemoryCodePage)
  private
    FWideMap: PLegacyChar;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; override;
    function IsEBCDIC: Boolean;

  {$IFDEF Latin}
    function DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;
  {$ENDIF}

    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; override;

  // properties
  {$WARNINGS OFF}
    property Map: TSingleByteMap read FSingleByteMap;
  {$WARNINGS ON}
    property WideMap: PLegacyChar read FWideMap;
  end;

  TLeadByte = #$80..#$FF;
  PTrailByteMap = ^TSingleByteMap;
  TDoubleByteMap = array[TLeadByte] of PTrailByteMap;

  PDoubleByteChar = ^DoubleByteChar;
  DoubleByteChar = packed record
    case Byte of
      0: (SingleByte: LegacyChar);
      1: (LeadByte, TrailByte: LegacyChar);
  end;

  PWideMap = ^TWideMap;
  TWideMap = array[Word] of DoubleByteChar;

  TDoubleByteMemCodePage = class(TMemoryCodePage)
  private
    FDoubleByteMap: TDoubleByteMap;
    FWideMap: PWideMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; override;

  {$IFDEF Latin}
    function DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;
  {$ENDIF}

    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; override;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;

  // properties
    property SingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideMap: PWideMap read FWideMap;
  end;

{ Strings }

  TSubString = class(TSharedObject)
  private
    FCount: Cardinal;
  {$IFNDEF Lite}
  //  { placeholder }  FLanguage: Word;
  {$ENDIF}
  //  { placeholder }  FData: Pointer;
  //  { placeholder }  FOptions: TStringOptions;
  public
    function ByteCount: Cardinal; overload; virtual; abstract;
  // properties
    property Count: Cardinal read FCount;
  end;

  TString = class(TSubString)
  private
    procedure SetCount(Value: Cardinal);
  public
    constructor Create(Source: Pointer; Count: Cardinal; Options: TStringOptions = []); overload;
    procedure Assign(Value: Pointer; Count: Cardinal; Options: TStringOptions = []); virtual; abstract;

  // properties
    property Count write SetCount;
  end;

  TMemoryString = class(TString)
  private
  {$IFNDEF Lite}
    FLanguage: Word;
  {$ENDIF}
  public
    constructor Create(Source: Pointer; Options: TStringOptions = []); overload;

    procedure Assign(Value: Pointer; Count: Cardinal; Options: TStringOptions = []); override;

    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    function ByteCount: Cardinal; overload; override;

    class function Length(Source: Pointer): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; virtual; abstract;
    property Language: Word read FLanguage write FLanguage;
  {$ENDIF}
  end;

  TLegacyString = class(TMemoryString)
  private
    FData: PLegacyChar;
    procedure SetData(Value: PLegacyChar);
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PLegacyChar read FData write SetData;
  end;

  TLatinString = class(TLegacyString)
  private
    FOptions: TLatinOptions;
  public
  // properties
    property Options: TLatinOptions read FOptions;
  end;

  TCodePageString = class(TLegacyString)
  private
    FOptions: TLegacyOptions;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;

  TUTF8String = class(TLegacyString)
  private
    FOptions: TLegacyOptions;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;

  TWideString = class(TMemoryString)
  private
    FData: PWideChar;
    FOptions: TEndianOptions;
    procedure SetData(Value: PWideChar);
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PWideChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadString = class(TMemoryString)
  private
    FData: PQuadChar;
    FOptions: TEndianOptions;
    procedure SetData(Value: PQuadChar);
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PQuadChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TStringConvert = (scDecode, scEncode);
  TConvertError = (ceLatin1, ceBadUTF8);

  EString = class(Exception)
  private
    FString: TString;
  public
    constructor Create(Source: TString; NonUnicode: Boolean); overload;
    constructor Create(Source: TString; IsEncode, Latin1: Boolean); overload;
    constructor Create(Source: TString; IsEncode: Boolean; CodePage: TCodePage); overload;
  end;

{ Core services }

function TryLatinFromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryLatinFromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF32FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF32FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF16FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF16FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF8FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF8FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLegacySource = []): Boolean; overload;


function LatinFromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TLatinSource = []): Cardinal; overload;
function LatinFromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF32FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF32FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF16FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF16FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF8FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLatinSource = []): Cardinal; overload;
function UTF8FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;


function TryLatinFromUTF32(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF16(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF8(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF32FromLatin(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF32FromUTF16(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF8(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF16FromLatin(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF16FromUTF32(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF8(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF8FromLatin(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF8FromUTF32(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF16(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Boolean; overload;


function TryLatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): TLatinStrInfo; overload;
function TryLatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): TLatinStrInfo; overload;
function TryLatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): TLatinStrInfo; overload;

function TryUTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF32StrInfo; overload;
function TryUTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): TUTF32StrInfo; overload;
function TryUTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF32StrInfo; overload;

function TryUTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF16StrInfo; overload;
function TryUTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): TUTF16StrInfo; overload;
function TryUTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF16StrInfo; overload;

function TryUTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): TUTF8StrInfo; overload;
function TryUTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): TUTF8StrInfo; overload;
function TryUTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): TUTF8StrInfo; overload;


function LatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;


function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbUnknown): TCharBlock;
function TranslateCodePage(Source: Word): Word;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

function TryLatinFromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TLatinSource): Boolean; 
begin
  // TODO
end;

function TryLatinFromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean; 
begin
  // TODO
end;

function TryUTF32FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean; 
begin
  // TODO
end;

function TryUTF16FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TLegacySource): Boolean; 
begin
  // TODO
end;

function LatinFromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TLatinSource): Cardinal;
begin
  if not TryLatinFromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryLatinFromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal; 
begin
  if not TryLatinFromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryLatinFromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF32FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF32FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF32FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF32FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF16FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF16FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF16FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF16FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TLatinSource): Cardinal;
begin
  if not TryUTF8FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF8FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF8FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF8FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function TryLatinFromUTF32(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF16(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF8(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromLatin(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF16(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF8(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromLatin(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF32(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF8(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromLatin(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF32(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF16(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function LatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function LatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function LatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> nil) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock): TCharBlock;
var
  Min, Max: TUnicodeBlock;
begin
  if PrevBlock in [Low(TUnicodeBlock)..High(TUnicodeBlock)] then
  begin
    if Source < UnicodeBlockRanges[PrevBlock].Lo then
    begin
      Min := Low(TUnicodeBlock);
      Max := Pred(PrevBlock);
    end
    else if Source > UnicodeBlockRanges[PrevBlock].Hi then
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
    if Source < UnicodeBlockRanges[Result].Lo then
      Max := Pred(Result)
    else if Source > UnicodeBlockRanges[Result].Hi then
      Min := Succ(Result)
    else
      Exit;
  end;
  Result := cbNonUnicode;
end;

function TranslateCodePage(Source: Word): Word;
begin
  case Source of
    CP_ACP:
      Result := GetACP;
    CP_OEMCP:
      Result := GetOEMCP;
  else
    Result := Source;
  end;
end;

{ EString }

constructor EString.Create(Source: TString; NonUnicode: Boolean);
const
  Messages: array[Boolean] of PLegacyChar = (sNoCodePage, sNonUnicode);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Source);
  Create(Messages[NonUnicode], [@ClassName]);
end;

const
  DecodeEncode: array[Boolean] of PLegacyChar = ('decode', 'encode');

constructor EString.Create(Source: TString; IsEncode, Latin1: Boolean);
const
  CharSets: array[Boolean] of PLegacyChar = (sLatin, sLatin1);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Source);
  Create(sInvalidLatin, [DecodeEncode[IsEncode], @ClassName, CharSets[Latin1]]);
end;

constructor EString.Create(Source: TString; IsEncode: Boolean; CodePage: TCodePage);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Source);
  Create(sInvalidCodePage, CP_LEGACY, [DecodeEncode[IsEncode], @ClassName, CodePage.Name]);
end;

{ TCodePage }

{$IFDEF Latin}
function TCodePage.DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeLatin(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := EncodeLatin(Dest, DestCount, Source, StrLen(Source), Options);
end;
{$ENDIF}

{$IFDEF UTF32}
function TCodePage.DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeUTF32(Dest, DestCount, Source, QuadStrLen(Source), Options);
end;

function TCodePage.EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeEndian): Cardinal;
begin
  Result := EncodeUTF32(Dest, DestCount, Source, StrLen(Source), Options);
end;
{$ENDIF}

function TCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeUTF16(Dest, DestCount, Source, WideStrLen(Source), Options);
end;

function TCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeEndian): Cardinal;
begin
  Result := EncodeUTF16(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeUTF8(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeUTF8): Cardinal;
begin
  Result := EncodeUTF8(Dest, DestCount, Source, StrLen(Source), Options);
end;

{ TMemoryCodePage }

constructor TMemoryCodePage.Create(const Info: TCPInfoEx);
var
  SourceMap: array[Byte] of LegacyChar;
  B, L: Byte;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
  C: LegacyChar;
  I: Cardinal;
  T: LongWord;
  W: WideChar;
begin
  FNumber := Info.CodePage;
  FName := WideStrNew(Info.CodePageName);

  // Fast core
  T := $03020100;
  for I := 0 to SizeOf(SourceMap) div SizeOf(T) - 1 do
  begin
    PLongWordArray(@SourceMap)[I] := T;
    Inc(T, $04040404);
  end;

  B := 0;
  while (Info.LeadByte[B] <> 0) and (B < MAX_LEADBYTES) do
  begin
    L := Info.LeadByte[B];
    FillChar(SourceMap[L], Info.LeadByte[B + 1] - L + 1, 0);
    Inc(B, 2);
  end;

  if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
    (FNumber, 0, SourceMap, Length(SourceMap), FSingleByteMap, Length(FSingleByteMap)) = 0
  then
    RaiseLastPlatformError;

  FWideMapLo := WideChar($100);
{$IFNDEF Lite}
  Block := cbUnknown;
{$ENDIF}
  for C := Low(C) to High(C) do
  begin
    W := FSingleByteMap[C];
    if (W <> WideChar(0)) and (W <> WideChar(C)) then
      if W = Info.UnicodeDefaultChar then
        FSingleByteMap[C] := WideChar(0)
      else
      begin
        if W > FWideMapHi then
          FWideMapHi := W
        else if W < FWideMapLo then
          FWideMapLo := W;
      {$IFNDEF Lite}
        Block := FindCharBlock(QuadChar(W), Block);
        Include(FBlocks, Block);
      {$ENDIF}
      end;
  end;
{$IFNDEF Lite}
  if FWideMapLo >= WideChar($80) then
    Include(FBlocks, cbBasicLatin);
{$ENDIF}
end;

destructor TMemoryCodePage.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

function TMemoryCodePage.GetWideMapCount: Word;
begin
  if FWideMapHi <> WideChar(0) then
    Result := Word(FWideMapHi) - Word(FWideMapLo) + 1
  else
    Result := 0;
end;

{ TSingleByteMemCodePage }

constructor TSingleByteMemCodePage.Create(const Info: TCPInfoEx);
var
  C: LegacyChar;
  W: WideChar;
begin
  inherited;
  if FWideMapLo <= FWideMapHi then
  begin
    FWideMap := AllocMem(Word(FWideMapHi) - Word(FWideMapLo) + 1);
    for C := Low(C) to High(C) do
    begin
      W := FSingleByteMap[C];
      if W <> WideChar(0) then
        FWideMap[Word(W) - Word(FWideMapLo)] := C;
    end;
  end
  else
  begin
    FWideMapLo := WideChar(0);
    FWideMapHi := WideChar(0);
  end;
end;

destructor TSingleByteMemCodePage.Destroy;
begin
  FreeMem(FWideMap);
  inherited;
end;

class function TSingleByteMemCodePage.MaxCharBytes: Byte;
begin
  Result := 1;
end;

function TSingleByteMemCodePage.IsEBCDIC: Boolean;
begin
  Result := FWideMapLo < WideChar($80);
end;

{$IFDEF Latin}
function TSingleByteMemCodePage.DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
var
  I: Integer;
  C: LegacyChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    C := Source[I];
    if ((C in [#0..#$7F]) or ((C in [#$A0..#$FF]) and (coLatin1 in Options))) then
    begin
      if WideChar(C) < FWideMapLo then
      begin
        Dest[I] := C;
        Continue;
      end;

      if WideChar(C) <= FWideMapHi then
      begin
        C := FWideMap[Word(C) - Word(FWideMapLo)];
        if C <> #0 then
        begin
          Dest[I] := C;
          Continue;
        end;
      end;
    end;

    if coForceInvalid in Options then
      Dest[I] := Unknown_Latin
    else
    begin
      Result := 0;
      Exit;
    end;
  end;

  Result := DestCount;
end;

function TSingleByteMemCodePage.EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    C := Source[I];
    W := FSingleByteMap[C];
    if (W = WideChar(0)) and (C = #0) then
      Dest[I] := #0
    else if (W <= WideChar($7F)) or
      ((W >= WideChar($A0)) and (W <= WideChar($FF)) and (coLatin1 in Options))
    then
      Dest[I] := LegacyChar(W)
    else if coForceInvalid in Options then
      Dest[I] := Unknown_Latin
    else
    begin
      Result := 0;
      Exit;
    end;
  end;

  Result := DestCount;
end;
{$ENDIF}

{$IFDEF UTF32}
function TSingleByteMemCodePage.DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    Q := Source[I];
    if coBigEndian in Options then
      Q := SwapBytes(Q);

    if (Q < QuadChar(FWideMapLo)) then
    begin
      Dest[I] := LegacyChar(Q);
      Continue;
    end;

    if Q <= QuadChar(FWideMapHi) then
    begin
      C := FWideMap[Q - QuadChar(FWideMapLo)];
      if C <> #0 then
      begin
        Dest[I] := C;
        Continue;
      end;
    end;

    // TODO: coNonSpace, coWidth, coHanzi, coTurkic, coDiacritics, coComposition
    if coForceInvalid in Options then
      Dest[I] := Unknown_Latin
    else
    begin
      Result := 0;
      Exit;
    end;
  end;

  Result := DestCount;
end;

function TSingleByteMemCodePage.EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    C := Source[I];
    Q := QuadChar(FSingleByteMap[C]);

    if Q = 0 then
    begin
      if C = #0 then
      begin
        Dest[I] := 0;
        Continue;
      end;

      if coForceInvalid in Options then
        Q := Unknown_UTF32
      else
      begin
        Result := 0;
        Exit;
      end
    end;

    if coBigEndian in Options then
      Q := SwapBytes(Q);
    Dest[I] := Q;
  end;

  Result := DestCount;
end;
{$ENDIF}

function TSingleByteMemCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    W := Source[I];
    if coBigEndian in Options then
      W := WideChar(Swap(Word(W)));

    if (W < FWideMapLo) then
    begin
      Dest[I] := LegacyChar(W);
      Continue;
    end;

    if W <= FWideMapHi then
    begin
      C := FWideMap[Word(W) - Word(FWideMapLo)];
      if C <> #0 then
      begin
        Dest[I] := C;
        Continue;
      end;
    end;

    // TODO: coNonSpace, coWidth, coHanzi, coTurkic, coDiacritics, coComposition
    if coForceInvalid in Options then
      Dest[I] := Unknown_Latin
    else
    begin
      Result := 0;
      Exit;
    end;
  end;

  Result := DestCount;
end;

function TSingleByteMemCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if (Source = nil) or (SourceCount = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    C := Source[I];
    W := FSingleByteMap[C];

    if W = WideChar(0) then
    begin
      if C = #0 then
      begin
        Dest[I] := WideChar(0);
        Continue;
      end;

      if coForceInvalid in Options then
        W := Unknown_UTF16
      else
      begin
        Result := 0;
        Exit;
      end
    end;

    if coBigEndian in Options then
      W := WideChar(Swap(Word(W)));
    Dest[I] := W;
  end;

  Result := DestCount;
end;

function TSingleByteMemCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TSingleByteMemCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeUTF8): Cardinal;
begin

end;

{ TDoubleByteMemCodePage }

constructor TDoubleByteMemCodePage.Create(const Info: TCPInfoEx);
var
  SourceMap: array[Byte] of DoubleByteChar;
  B: Byte;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
  C, L: Char;
  I: Word;
  P: PTrailByteMap;
  T: LongWord;
  W: WideChar;
begin
  inherited;

{$IFNDEF Lite}
  Block := cbUnknown;
{$ENDIF}
  B := 0;
  while (B < MAX_LEADBYTES) and (Info.LeadByte[B] <> 0) do
  begin
    for L := LegacyChar(Info.LeadByte[B]) to LegacyChar(Info.LeadByte[B + 1]) do
    begin
      // Fast core
      T := $02000100 or (Byte(L) shl 16) or Byte(L);
      for I := 0 to SizeOf(SourceMap) div SizeOf(T) - 1 do
      begin
        PLongWordArray(@SourceMap)[I] := T;
        Inc(T, $02000200);
      end;
      GetMem(P, SizeOf(TSingleByteMap));
      FDoubleByteMap[L] := P;
      if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
        (FNumber, 0, @SourceMap, Length(SourceMap) * SizeOf(DoubleByteChar),
          PWideChar(P), SizeOf(TSingleByteMap) div SizeOf(WideChar)) = 0
      then
        RaiseLastPlatformError;

      for C := Low(C) to High(C) do
      begin
        W := P[C];
        if W <> WideChar(0) then
          if W = Info.UnicodeDefaultChar then
            P[C] := WideChar(0)
          else
          begin
            if W > FWideMapHi then
              FWideMapHi := W
            else if W < FWideMapLo then
              FWideMapLo := W;
          {$IFNDEF Lite}
            Block := FindCharBlock(QuadChar(W), Block);
            Include(FBlocks, Block);
          {$ENDIF}
          end;
      end;
    end;
    Inc(B, 2);
  end;

  FWideMap := AllocMem((Word(FWideMapHi) - Word(FWideMapLo) + 1) * SizeOf(DoubleByteChar));

  for L := Low(L) to Pred(Low(TLeadByte)) do
  begin
    W := FSingleByteMap[L];
    if W >= FWideMapLo then
      FWideMap[Word(W) - Word(FWideMapLo)].SingleByte := L;
  end;

  for L := Low(TLeadByte) to High(TLeadByte) do
  begin
    P := FDoubleByteMap[L];
    if P <> nil then
    begin
      for C := Low(C) to High(C) do
      begin
        W := P[C];
        if W >= FWideMapLo then
          with FWideMap[Word(W) - Word(FWideMapLo)] do
          begin
            LeadByte := L;
            TrailByte := C;
          end;
      end;
    end
    else
    begin
      W := FSingleByteMap[L];
      if W >= FWideMapLo then
        FWideMap[Word(W) - Word(FWideMapLo)].SingleByte := L;
    end;
  end
end;

destructor TDoubleByteMemCodePage.Destroy;
var
  L: TLeadByte;
begin
  FreeMem(FWideMap);
  for L := Low(L) to High(L) do
    FreeMem(FDoubleByteMap[L]);
  inherited;
end;

class function TDoubleByteMemCodePage.MaxCharBytes: Byte;
begin
  Result := SizeOf(DoubleByteChar);
end;

{$IFDEF Latin}
function TDoubleByteMemCodePage.DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;
{$ENDIF}

{$IFDEF UTF32}
function TDoubleByteMemCodePage.DecodeUTF32(Dest: PLegacyChar;
  DestCount: Cardinal; Source: PQuadChar; SourceCount: Cardinal;
  Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
begin

end;
{$ENDIF}

function TDoubleByteMemCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
begin

end;

function TDoubleByteMemCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeUTF8): Cardinal;
begin

end;

{ TString }

constructor TString.Create(Source: Pointer; Count: Cardinal; Options: TStringOptions);
begin
  Assign(Source, Count, Options);
end;

procedure TString.SetCount(Value: Cardinal);
begin
  Assign(nil, Value);
end;

{ TMemoryString }

constructor TMemoryString.Create(Source: Pointer; Options: TStringOptions);
begin
  Assign(Source, Length(Source), Options);
end;

function TMemoryString.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

procedure TMemoryString.Assign(Value: Pointer; Count: Cardinal;
  Options: TStringOptions);
var
  Bytes, Zero: Cardinal;
begin
  Lock;
  try
    if soAttachBuffer in Options then
    begin
      if not (soAttachBuffer in TLatinString(Self).FOptions) then
        FreeMem(TLegacyString(Self).FData);
      TLegacyString(Self).FData := Value;
    end
    else
    begin
      Bytes := ByteCount(Count);
      Zero := ByteCount(1);
      ReallocMem(TLegacyString(Self).FData, Bytes + Zero);
      if Value <> nil then
        Move(Value^, TLegacyString(Self).FData^, Bytes);
      FillChar(TLegacyString(Self).FData[Bytes], Zero, 0);
    end;
    FCount := Count;
    TLatinString(Self).FOptions := Options;
  finally
    Unlock;
  end;
end;

{ TLegacyString }

class function TLegacyString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

class function TLegacyString.Length(Source: Pointer): Cardinal;
begin
  Result := StrLen(Source);
end;

{$IFNDEF Lite}
class function TLegacyString.Length(Source: Pointer; MaxLength: Cardinal): Cardinal;
begin
  Result := StrLen(Source, MaxLength);
end;
{$ENDIF}

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value));
end;

{ TWideString }

class function TWideString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count * SizeOf(WideChar);
end;

class function TWideString.Length(Source: Pointer): Cardinal;
begin
  Result := WideStrLen(Source);
end;

{$IFNDEF Lite}
class function TWideString.Length(Source: Pointer; MaxLength: Cardinal): Cardinal;
begin
  Result := WideStrLen(Source, MaxLength);
end;
{$ENDIF}

procedure TWideString.SetData(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value));
end;

{ TQuadString }

class function TQuadString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count * SizeOf(QuadChar);
end;

class function TQuadString.Length(Source: Pointer): Cardinal;
begin
  Result := QuadStrLen(Source);
end;

{$IFNDEF Lite}
class function TQuadString.Length(Source: Pointer; MaxLength: Cardinal): Cardinal;
begin
  Result := QuadStrLen(Source, MaxLength);
end;
{$ENDIF}

procedure TQuadString.SetData(Value: PQuadChar);
begin
  Assign(Value, QuadStrLen(Value));
end;

end.

