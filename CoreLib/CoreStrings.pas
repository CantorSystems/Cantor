(*
    The Unified Environment Core Library

    Core string and character set implementation 

    Copyright (c) 2012 The Unified Environment Laboratory

    Conditional defines:
      * Lite -- strip support of:
        * Character blocks
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
  TEndianSource = set of soBigEndian..soDetectCharSet;

  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coForceInvalid, coRangeBlocks,
    coCompatibility, coBigEndian, coSurrogates);

const
  coLatin1 = coCompatibility;
  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;
  coModifiedUTF8 = [coCESU8, coEncodedZero];

  coNFC = [];
  coNFD = [coComposition];
  coNFKC = [coCompatibility];
  coNFKD = [coComposition, coCompatibility];

type
  TEncodeLegacy = set of coCase..coRangeBlocks;
  TEncodeLatin = set of coCase..coLatin1;
  TEncodeUTF32 = set of coComposition..coBigEndian;
  TEncodeUTF16 = set of coComposition..coSurrogates;
  TEncodeUTF8 = set of coComposition..coCESU8;
  TUTF8Compliance = set of coEncodedZero..coCESU8;

  PLegacyStrInfo = ^TLegacyStrInfo;
  TLegacyStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount: Cardinal);
  end;

  PUnicodeStrInfo = ^TUnicodeStrInfo;
  TUnicodeStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, SurrogateCount: Cardinal);
  end;

  TUTF8StrInfo = type TUnicodeStrInfo;
  TUTF16StrInfo = type TUnicodeStrInfo;
  TUTF32StrInfo = type TUnicodeStrInfo;

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
    function FromLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; overload; virtual; abstract;
    function FromLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLatin = []): Boolean; overload; virtual; abstract;
    function ToLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLatin = []): Cardinal; overload;
  {$ENDIF}

  {$IFDEF UTF32}
    function FromUTF32(var Info: TLegacyStrInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; overload; virtual; abstract;
    function FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Boolean; overload; virtual; abstract;
    function ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; overload; virtual; abstract;
    function FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; overload; virtual; abstract;
    function FromUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8 = []): Boolean; overload; virtual; abstract;
    function ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;

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
    function FromLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLatin = []): Boolean; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function FromUTF32(var Info: TLegacyStrInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Boolean; override;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; override;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8 = []): Boolean; override;

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
    function FromLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLatin = []): Boolean; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function FromUTF32(var Info: TLegacyStrInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Boolean; override;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; override;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8 = []): Boolean; override;

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

  TCharSet = (csLatin, csLatin1, csUTF8, csUTF16, csUTF32);

  TConvertSiteType = (stCharSet, stCodePage);
  TConvertSite = record
    case SiteType: TConvertSiteType of
      stCharSet: (CharSet: TCharSet);
      stCodePage: (CodePage: TCodePage);
  end;

  EString = class(Exception)
  end;

  EConvert = class(EString)
  private
    FDestSite, FSourceSite: TConvertSite;
    FSource: Pointer;
    FInfo: TUnicodeStrInfo;
  public
    constructor Create(Source: Pointer; SourceSite: TCharSet;
      const Info; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCharSet;
      const Info: TLegacyStrInfo; DestSite: TCodePage); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage; 
      const Info; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage;
      const Info: TLegacyStrInfo; DestSite: TCodePage); overload;
  // properties
    property DestSite: TConvertSite read FDestSite;
    property Info: TUnicodeStrInfo read FInfo;
    property SourceSite: TConvertSite read FSourceSite;
    property Source: Pointer read FSource;
  end;

  ENoCodePage = class(EString)
  private
    FString: TCodePageString;
  public
    constructor Create(Str: TCodePageString);
  // properties
    property Str: TCodePageString read FString;
  end;

{ Core services }

function LatinToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
  DestOptions: TEncodeLatin = []): Boolean; overload;
function UTF32ToLatin(var Info: TLegacyStrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeLatin = []): Boolean; overload;
function UTF16ToLatin(var Info: TLegacyStrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeLatin = []): Boolean; overload;
function UTF8ToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
  DestOptions: TEncodeLatin = []): Boolean; overload;

function LatinToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF32ToUTF32(var Info: TUTF32StrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF16ToUTF32(var Info: TUTF32StrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF8ToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;

function LatinToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF32ToUTF16(var Info: TUTF16StrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF16ToUTF16(var Info: TUTF16StrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF8ToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;

function LatinToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF32ToUTF8(var Info: TUTF8StrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF16ToUTF8(var Info: TUTF8StrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF8ToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;

function LatinToLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin = []): Cardinal; overload;
function UTF32ToLatin(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin = []): Cardinal; overload;
function UTF16ToLatin(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin = []): Cardinal; overload;
function UTF8ToLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin = []): Cardinal; overload;

function LatinToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;

function LatinToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

function LatinToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;

function EstimateUTF8(const Info: TUTF8StrInfo; Options: TEncodeUTF8 = []): Cardinal; overload;
function EstimateUTF8(const Info: TUTF16StrInfo; Options: TEncodeUTF8 = []): Cardinal; overload;
function EstimateUTF8(const Info: TUTF32StrInfo; Options: TEncodeUTF8 = []): Cardinal; overload;

function EstimateUTF16(const Info: TUTF8StrInfo; Options: TEncodeUTF16 = []): Cardinal; overload;
function EstimateUTF16(const Info: TUTF16StrInfo; Options: TEncodeUTF16 = []): Cardinal; overload;
function EstimateUTF16(const Info: TUTF32StrInfo; Options: TEncodeUTF16 = []): Cardinal; overload;

function EstimateUTF32(const Info: TUTF8StrInfo; Options: TEncodeUTF32 = []): Cardinal; overload;
function EstimateUTF32(const Info: TUTF16StrInfo; Options: TEncodeUTF32 = []): Cardinal; overload;
function EstimateUTF32(const Info: TUTF32StrInfo; Options: TEncodeUTF32 = []): Cardinal; overload;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbUnknown): TCharBlock;
function TranslateCodePage(Source: Word): Word;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

function LatinToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
begin
  // TODO
end;

function UTF32ToLatin(var Info: TLegacyStrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
begin
  // TODO
end;

function UTF16ToLatin(var Info: TLegacyStrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
begin
  // TODO
end;

function UTF8ToLatin(var Info: TLegacyStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
begin
  // TODO
end;

function LatinToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF32ToUTF32(var Info: TUTF32StrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF16ToUTF32(var Info: TUTF32StrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF8ToUTF32(var Info: TUTF32StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function LatinToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF32ToUTF16(var Info: TUTF16StrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF16ToUTF16(var Info: TUTF16StrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF8ToUTF16(var Info: TUTF16StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function LatinToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF32ToUTF8(var Info: TUTF8StrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF16ToUTF8(var Info: TUTF8StrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF8ToUTF8(var Info: TUTF8StrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function LatinToLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions),
      Info, TCharSet(coLatin1 in DestOptions));
end;

function UTF32ToLatin(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, TCharSet(coLatin1 in DestOptions));
end;

function UTF16ToLatin(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, TCharSet(coLatin1 in DestOptions));
end;

function UTF8ToLatin(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, TCharSet(coLatin1 in DestOptions));
end;

function LatinToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF32);
end;

function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF32);
end;

function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF32);
end;

function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, csUTF32);
end;

function LatinToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF16);
end;

function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF16);
end;

function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF16);
end;

function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, csUTF16);
end;

function LatinToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF8);
end;

function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF8);
end;

function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF8);
end;

function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, csUTF8);
end;

function EstimateUTF8(const Info: TUTF8StrInfo; Options: TEncodeUTF8): Cardinal;
begin
  // TODO
end;

function EstimateUTF8(const Info: TUTF16StrInfo; Options: TEncodeUTF8): Cardinal;
begin
  // TODO
end;

function EstimateUTF8(const Info: TUTF32StrInfo; Options: TEncodeUTF8): Cardinal;
begin
  // TODO
end;

function EstimateUTF16(const Info: TUTF8StrInfo; Options: TEncodeUTF16): Cardinal;
begin
  // TODO
end;

function EstimateUTF16(const Info: TUTF16StrInfo; Options: TEncodeUTF16): Cardinal;
begin
  // TODO
end;

function EstimateUTF16(const Info: TUTF32StrInfo; Options: TEncodeUTF16): Cardinal;
begin
  // TODO
end;

function EstimateUTF32(const Info: TUTF8StrInfo; Options: TEncodeUTF32): Cardinal;
begin
  // TODO
end;

function EstimateUTF32(const Info: TUTF16StrInfo; Options: TEncodeUTF32): Cardinal;
begin
  // TODO
end;

function EstimateUTF32(const Info: TUTF32StrInfo; Options: TEncodeUTF32): Cardinal;
begin
  // TODO
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

{ EConvert }

const
  CharSets: array [TCharSet] of PLegacyChar = (sLatin, sLatin1, sUTF8, sUTF16, sUTF32);

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet;
  const Info; DestSite: TCharSet);
begin
  if DestSite >= csUTF8 then
  begin
    if TUnicodeStrInfo(Info).SurrogateCount <> 0 then
      Create(sSurrogates, [CharSets[SourceSite]])
    else 
      Create(sNonUnicode, [CharSets[SourceSite]]);
    FInfo := TUnicodeStrInfo(Info);
  end
  else
    Move(Info, FInfo, SizeOf(TLegacyStrInfo));

  if Message = nil then
  {$IFDEF Lite}
    Create(sInvalidCharSet2, [CharSets[SourceSite], CharSets[DestSite]]);
  {$ELSE}
    Create(sInvalidCharSet, [CharBlockNames[FindCharBlock(TLegacyStrInfo(Info).InvalidChar)],
      CharSets[DestSite]]);
  {$ENDIF}

  FDestSite.CharSet := DestSite;     // SiteType filled with 0 = stCharSet
  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCodePage;
  const Info; DestSite: TCharSet);
begin
{$IFDEF Lite}
  Create(sInvalidCharSet, CP_LEGACY, [SourceSite.Name, CharSets[DestSite]]);
{$ELSE}
  Create(sInvalidCharSet, [CharBlockNames[FindCharBlock(TLegacyStrInfo(Info).InvalidChar)],
    CharSets[DestSite]]);
{$ENDIF}

  FDestSite.CharSet := DestSite; // SiteType filled with 0 = stCharSet
  if DestSite < csUTF8 then
    Move(Info, FInfo, SizeOf(TLegacyStrInfo))
  else
    FInfo := TUnicodeStrInfo(Info);

  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet; 
  const Info: TLegacyStrInfo; DestSite: TCodePage);
begin
{$IFDEF Lite}
  Create(sInvalidCodePage2, CP_LEGACY, [CharSets[SourceSite], DestSite.Name]);
{$ELSE}
  Create(sInvalidCodePage, CP_LEGACY,
    [CharBlockNames[FindCharBlock(Info.InvalidChar)], DestSite.Name]);
{$ENDIF}

  with FDestSite do
  begin
    SiteType := stCodePage;
    CodePage := DestSite;
  end;
  Move(Info, FInfo, SizeOf(Info));

  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCodePage; 
  const Info: TLegacyStrInfo; DestSite: TCodePage);
begin
{$IFDEF Lite}
  Create(sInvalidCodePage2, CP_LEGACY, [SourceSite.Name, DestSite.Name]);
{$ELSE}
  Create(sInvalidCodePage, CP_LEGACY,
    [CharBlockNames[FindCharBlock(Info.InvalidChar)], DestSite.Name]);
{$ENDIF}

  with FDestSite do
  begin
    SiteType := stCodePage;
    CodePage := DestSite;
  end;
  Move(Info, FInfo, SizeOf(Info));

  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
end;

{ ECodePage }

constructor ENoCodePage.Create(Str: TCodePageString);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Str);
  inherited Create(sNoCodePage, [@ClassName]);
end;

{ TCodePage }

{$IFDEF Latin}
function TCodePage.FromLatin(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if FromLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, Self);
end;

function TCodePage.ToLatin(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLatin): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, Self, Info, TCharSet(coLatin1 in DestOptions));
end;
{$ENDIF}

{$IFDEF UTF32}
function TCodePage.FromUTF32(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if FromUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, Self);
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, Self, Info, csUTF32);
end;
{$ENDIF}

function TCodePage.FromUTF16(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if FromUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, Self);
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, Self, Info, csUTF16);
end;

function TCodePage.FromUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TLegacyStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if FromUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, Self);
end;

function TCodePage.ToUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions) then
    Result := Info.Count
  else
    raise EConvert.Create(Source, Self, Info, csUTF8);
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
function TSingleByteMemCodePage.FromLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Integer;
  C: LegacyChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      if ((C in [#0..#$7F]) or ((C in [#$A0..#$FF]) and (soLatin1 in SourceOptions))) then
      begin
        if WideChar(C) < FWideMapLo then
        begin
          Dest[I] := C;
          Inc(Info.Count);
          Continue;
        end;

        if WideChar(C) <= FWideMapHi then
        begin
          C := FWideMap[Word(C) - Word(FWideMapLo)];
          if C <> #0 then
          begin
            Dest[I] := C;
            Inc(Info.Count);
            Continue;
          end;
        end;
      end;

      if coForceInvalid in DestOptions then
      begin
        Dest[I] := Unknown_Latin;
        with Info do
        begin
          Inc(InvalidCount);
          Inc(Count);
        end;
      end
      else
      begin
        Info.InvalidChar := QuadChar(C);
        Result := False;
        Exit;
      end;
    end;
  end;
  
  Result := True;
end;

function TSingleByteMemCodePage.ToLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      W := FSingleByteMap[C];
      if (W = WideChar(0)) and (C = #0) then
      begin
        Dest[I] := #0;
        Inc(Info.Count);
      end
      else if (W <= WideChar($7F)) or
        ((W >= WideChar($A0)) and (W <= WideChar($FF)) and (soLatin1 in SourceOptions))
      then begin
        Dest[I] := LegacyChar(W);
        Inc(Info.Count);
      end
      else if coForceInvalid in DestOptions then
      begin
        Dest[I] := Unknown_Latin;
        with Info do
        begin
          Inc(InvalidCount);
          Inc(Count);
        end;
      end
      else
      begin
        Info.InvalidChar := QuadChar(W);
        Result := False;
        Exit;
      end;
    end;
  end;

  Result := True;
end;
{$ENDIF}

{$IFDEF UTF32}
function TSingleByteMemCodePage.FromUTF32(var Info: TLegacyStrInfo;
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      Q := Source[I];
      if soBigEndian in SourceOptions then
        Q := SwapBytes(Q);

      if Q = Unknown_UTF32 then
      begin
        Dest[I] := Unknown_Latin;
        Inc(Info.Count);
        Continue;
      end;

      if (Q < QuadChar(FWideMapLo)) then
      begin
        Dest[I] := LegacyChar(Q);
        Inc(Info.Count);
        Continue;
      end;

      if Q <= QuadChar(FWideMapHi) then
      begin
        C := FWideMap[Q - QuadChar(FWideMapLo)];
        if C <> #0 then
        begin
          Dest[I] := C;
          Inc(Info.Count);
          Continue;
        end;
      end;

      // TODO: coNonSpace, coWidth, coHanzi, coTurkic, coDiacritics, coComposition
      if coForceInvalid in DestOptions then
      begin
        Dest[I] := Unknown_Latin;
        with Info do
        begin
          Inc(InvalidCount);
          Inc(Count);
        end;
      end
      else
      begin
        Info.InvalidChar := Q;
        Result := False;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.ToUTF32(var Info: TUTF32StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      Q := QuadChar(FSingleByteMap[C]);

      if Q = 0 then
      begin
        if C = #0 then
        begin
          Dest[I] := 0;
          Inc(Info.Count);
          Continue;
        end;

        if coForceInvalid in DestOptions then
        begin
          Q := Unknown_UTF32;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := Q;
          Result := False;
          Exit;
        end
      end;

      if coBigEndian in DestOptions then
        Q := SwapBytes(Q);
      Dest[I] := Q;
      Inc(Info.Count);
    end;
  end;

  Result := True;
end;
{$ENDIF}

function TSingleByteMemCodePage.FromUTF16(var Info: TLegacyStrInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      W := Source[I];
      if soBigEndian in SourceOptions then
        W := WideChar(Swap(Word(W)));

      if W = Unknown_UTF16 then
      begin
        Dest[I] := Unknown_Latin;
        Inc(Info.Count);
        Continue;
      end;

      if (W < FWideMapLo) then
      begin
        Dest[I] := LegacyChar(W);
        Inc(Info.Count);
        Continue;
      end;

      if W <= FWideMapHi then
      begin
        C := FWideMap[Word(W) - Word(FWideMapLo)];
        if C <> #0 then
        begin
          Dest[I] := C;
          Inc(Info.Count);
          Continue;
        end;
      end;

      // TODO: coNonSpace, coWidth, coHanzi, coTurkic, coDiacritics, coComposition
      if coForceInvalid in DestOptions then
      begin
        Dest[I] := Unknown_Latin;
        with Info do
        begin
          Inc(InvalidCount);
          Inc(Count);
        end;
      end
      else
      begin
        Info.InvalidChar := QuadChar(W);
        Result := False;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.ToUTF16(var Info: TUTF16StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
begin
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      W := FSingleByteMap[C];

      if W = WideChar(0) then
      begin
        if C = #0 then
        begin
          Dest[I] := WideChar(0);
          Inc(Info.Count);
          Continue;
        end;

        if coForceInvalid in DestOptions then
        begin
          W := Unknown_UTF16;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := QuadChar(W);
          Result := False;
          Exit;
        end
      end;

      if coBigEndian in DestOptions then
        W := WideChar(Swap(Word(W)));
      Dest[I] := W;
      Inc(Info.Count);
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.FromUTF8(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin

end;

function TSingleByteMemCodePage.ToUTF8(var Info: TUTF8StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
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
function TDoubleByteMemCodePage.FromLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.ToLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
begin
  // TODO
end;
{$ENDIF}

{$IFDEF UTF32}
function TDoubleByteMemCodePage.FromUTF32(var Info: TLegacyStrInfo;
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.ToUTF32(var Info: TUTF32StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;
{$ENDIF}

function TDoubleByteMemCodePage.FromUTF16(var Info: TLegacyStrInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.ToUTF16(var Info: TUTF16StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.FromUTF8(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.ToUTF8(var Info: TUTF8StrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
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

