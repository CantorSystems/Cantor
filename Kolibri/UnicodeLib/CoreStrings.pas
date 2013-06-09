(*
    The Unified Environment Core Library

    Core string and character set implementation 

    Copyright © 2012 The Unified Environment Laboratory

    Conditional defines:
      * Lite -- strip support of:
        * Character blocks
        * TString.Language
        * TString.Length(Source, MaxLength)
      * Kolibri -- cross-compile as Kolibri UnicodeLib  
      * Latin -- Latin character set support (ISO-8859-1)
      * UTF32 -- UTF-32 character set support
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreClasses, Exceptions, CoreConsts;

{$I Unicode.inc}

type
  PLegacyStrInfo = ^TLegacyStrInfo;
  TLegacyStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, DoubleByteCount: Cardinal);
  end;

  PUnicodeStrInfo = ^TUnicodeStrInfo;
  TUnicodeStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, SurrogateCount, CharCount: Cardinal);
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

{$IFDEF Kolibri}
{$I ..\Kolibri\UnicodeLib\CoreStrings.pas.inc}
{$I ..\Kolibri\UnicodeLib\CodePageNames.pas.inc}
const
  soLatin1 = soBigEndian;
  coLatin1 = coBigEndian;

type
  TLatinOptions = set of soLatin1..soAttachBuffer;
  TLatinSource = set of soDetectCharSet..soLatin1;
  TEncodeLatin = set of coForceInvalid..coLatin1;
{$ELSE}
type
{
  soDetectCharSet:
    * Latin source: try to decode source as UTF-8, continue as latin on fail
    * Code page source: try to decode source as UTF-8, continue as code page on fail
    * UTF-8 source: try to decode source as code page, continue as UTF-8 on fail
    * UTF-16 and UTF-32: try to detect byte order
}
  TStringOption = (soDetectCharSet, soBigEndian, soAttachBuffer);

const
  soLatin1 = soBigEndian;

type
  TLegacyOptions = set of soAttachBuffer..soAttachBuffer;
  TLatinOptions = set of soLatin1..soAttachBuffer;
  TEndianOptions = set of soBigEndian..soAttachBuffer;

  TLegacySource = set of soDetectCharSet..soDetectCharSet;
  TLatinSource = set of soDetectCharSet..soLatin1;
  TEndianSource = set of soDetectCharSet..soBigEndian;

{
  coForceInvalid -- replace invalid characters with:
    * U+001A for code page (recommended by Unicode for single-byte character sets)
    * U+FFFD for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coForceInvalid, coRangeBlocks,
    coCompatibility, coBigEndian, coSurrogates);

const
  coLatin1 = coCompatibility;
  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;
  coModifiedUTF8 = [coCESU8, coEncodedZero];  // UTF-8 compliance

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
{$ENDIF Kolibri}

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

    function ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
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

    function ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; overload; virtual; abstract;
    function FromUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
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

{$IFNDEF Kolibri}
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
    function ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Boolean; override;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; override;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8 = []): Boolean; override;

  // properties
  {$WARNINGS OFF}
    property Map: TSingleByteMap read FSingleByteMap;
  {$WARNINGS ON}
    property WideMap: PLegacyChar read FWideMap;
  end;
{$ENDIF Kolibri}

type
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
    function ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Boolean; override;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Boolean; override;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Boolean; override;
    function ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8 = []): Boolean; override;

  // properties
    property SingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideMap: PWideMap read FWideMap;
  end;

{ Strings }

type
  TStringOptions = set of TStringOption;

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
    function CatchInvalidChar(const Info: TLegacyStrInfo; DestSite: TCharSet): Boolean; overload;
    function CatchInvalidChar(const Info: TLegacyStrInfo; DestSite: TCodePage): Boolean; overload;
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

const
  LeadBytes = [Low(TLeadByte)..High(TLeadByte)];

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

function LatinToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF32ToUTF32(var Info: TUnicodeStrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF16ToUTF32(var Info: TUnicodeStrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;
function UTF8ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []): Boolean; overload;

function LatinToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF32ToUTF16(var Info: TUnicodeStrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF16ToUTF16(var Info: TUnicodeStrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;
function UTF8ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []): Boolean; overload;

function LatinToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
  Count: Cardinal; SourceOptions: TLatinSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF32ToUTF8(var Info: TUnicodeStrInfo; Source: PQuadChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF16ToUTF8(var Info: TUnicodeStrInfo; Source: PWideChar;
  Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
  DestOptions: TEncodeUTF8 = []): Boolean; overload;
function UTF8ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
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

procedure UTF32SwapByteOrder(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar);
procedure UTF16SwapByteOrder(Source: PWideChar; Count: Cardinal; Dest: PQuadChar);

{$IFNDEF Kolibri}
function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource;
function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource;
{$ENDIF}

function EstimateUTF8(const Info: TUnicodeStrInfo; Options: TEncodeUTF8 = []): Cardinal; overload;
function EstimateUTF16(const Info: TUnicodeStrInfo; Options: TEncodeUTF16 = []): Cardinal; overload;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

procedure UTF32SwapByteOrder(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar);
asm
end;

procedure UTF16SwapByteOrder(Source: PWideChar; Count: Cardinal; Dest: PQuadChar);
asm
end;

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

function LatinToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF32ToUTF32(var Info: TUnicodeStrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF16ToUTF32(var Info: TUnicodeStrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function UTF8ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
begin
  // TODO
end;

function LatinToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF32ToUTF16(var Info: TUnicodeStrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF16ToUTF16(var Info: TUnicodeStrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function UTF8ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
begin
  // TODO
end;

function LatinToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLatinSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF32ToUTF8(var Info: TUnicodeStrInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF16ToUTF8(var Info: TUnicodeStrInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Boolean;
begin
  // TODO
end;

function UTF8ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar; Count: Cardinal;
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
  if LatinToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if UTF32ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if UTF16ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if UTF8ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, TCharSet(coLatin1 in DestOptions));
end;

function LatinToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUnicodeStrInfo;
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
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF32);
end;

function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF32);
end;

function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUnicodeStrInfo;
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
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF16);
end;

function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF16);
end;

function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF16);
end;

function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, csUTF16);
end;

function LatinToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLatinSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if LatinToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF8);
end;

function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF32ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, csUTF8);
end;

function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF16ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, csUTF8);
end;

function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if UTF8ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, csUTF8);
end;

{$IFDEF Kolibri}
function FromUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

{$IFDEF UTF32}
function FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;
{$ENDIF}


function ToUTF8(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function ToUTF16(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

{$IFDEF UTF32}
function ToUTF32(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;
{$ENDIF}

function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

{$IFDEF UTF32}
function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;

function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal;
begin
  // TODO
end;
{$ENDIF}

function EstimateUTF8(Count, SurrogateCount: Cardinal; Options: TEncodeUTF8): Cardinal;
begin
  if coCESU8 in Options then
    Result := (Count + SurrogateCount) * 3
  else
    Result := Count * 3 + SurrogateCount;
end;

function EstimateUTF16(Count, SurrogateCount: Cardinal; Options: TEncodeUTF16): Cardinal;
begin
  Result := Count;
  if coSurrogates in Options then
    Inc(Result, SurrogateCount);
end;

function EnumCodePages(Callback: TEnumCodePages): Word;
begin
  Result := 0;
end;

function GetCharSetDisplayName(CodePage: Word; Language: Byte): PLegacyChar;
begin
  Result := nil; // TODO
end;

function GetCharSetMIME(CodePage: Word; Language: Byte): PLegacyChar;
begin
  Result := nil; // TODO
end;
{$ENDIF Kolibri}

function EstimateUTF8(const Info: TUnicodeStrInfo; Options: TEncodeUTF8): Cardinal;
begin
  if coCESU8 in Options then
    Result := (Info.CharCount + Info.SurrogateCount) * 3
  else
    Result := Info.CharCount * 3 + Info.SurrogateCount;
end;

function EstimateUTF16(const Info: TUnicodeStrInfo; Options: TEncodeUTF16): Cardinal;
begin
  Result := Info.CharCount;
  if coSurrogates in Options then
    Inc(Result, Info.SurrogateCount);
end;

function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource;
begin
  case Source of
    BOM_UTF32_BE:
      Result := Options + [soBigEndian];
    BOM_UTF32_LE:
      Result := Options - [soBigEndian];
  else
    if ((Source and $FFFF) < $11) and ((Source shr 16) > $10) then
      Result := Options + [soBigEndian]
    else
      Result := Options - [soBigEndian];
  end;
end;

function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource;
var
  W, X: Word;
begin
  if Source <> nil then
  begin
    W := Word(Source[0]);
    case Swap(W) of
      BOM_UTF16:
        begin
          Result := Options + [soBigEndian];  // because of Swap below
          Exit;
        end;
      Low(THighSurrogates)..High(THighSurrogates):
        case Swap(Word(Source[1])) of
          Low(TLowSurrogates)..High(TLowSurrogates):
          begin
            Result := Options + [soBigEndian];
            Exit;
          end;
        end;
    else
      case W of
        BOM_UTF16:
          begin
            Result := Options - [soBigEndian];
            Exit;
          end;
        Low(THighSurrogates)..High(THighSurrogates):
          case Word(Source[1]) of
            Low(TLowSurrogates)..High(TLowSurrogates):
            begin
              Result := Options - [soBigEndian];
              Exit;
            end;
          end;
      end;
    end;

    X := Word(Source[1]);
    if ((W and $FF) < 32) and ((W shr 8) > 31) and
      ((X and $FF) < 32) and ((X shr 8) > 31) then
    begin
      Result := Options + [soBigEndian];
      Exit;
    end
    else if ((W and $FF) > 31) and ((W shr 8) < 32) and
      ((X and $FF) > 31) and ((X shr 8) < 32) then
    begin
      Result := Options - [soBigEndian];
      Exit;
    end;
  end;                                 

  Result := Options;
end;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock): TCharBlock;
var
  Min, Max: TUnicodeBlock;
begin
  if PrevBlock in UnicodeBlocks then
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

function EConvert.CatchInvalidChar(const Info: TLegacyStrInfo; DestSite: TCharSet): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(TLegacyStrInfo(Info).InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCharSetChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(TLegacyStrInfo(Info).InvalidChar), CharSets[DestSite]]);
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidChar(const Info: TLegacyStrInfo; DestSite: TCodePage): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(TLegacyStrInfo(Info).InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCodePageChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(TLegacyStrInfo(Info).InvalidChar), DestSite.Name]);
    Result := True;
  end
  else
    Result := False;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet;
  const Info; DestSite: TCharSet);
begin
  if (DestSite >= csUTF8) and (TUnicodeStrInfo(Info).SurrogateCount <> 0) then
  begin
    Create(sSurrogates, [CharSets[SourceSite]]);
    FInfo := TUnicodeStrInfo(Info);
  end
  else
  begin
  {$IFDEF Lite}
    Create(sInvalidCharSet2, [CharSets[SourceSite], CharSets[DestSite]]);
  {$ELSE}
    if not CatchInvalidChar(TLegacyStrInfo(Info), DestSite) then
      Create(sNonUnicode, [CharSets[SourceSite]]);
  {$ENDIF}
    Move(Info, FInfo, SizeOf(TLegacyStrInfo));
  end;

  FDestSite.CharSet := DestSite;     // SiteType filled with 0 = stCharSet
  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCodePage;
  const Info; DestSite: TCharSet);
begin
{$IFNDEF Lite}
  if not CatchInvalidChar(TLegacyStrInfo(Info), DestSite) then
{$ENDIF}
    Create(sInvalidCharSet, CP_LEGACY, [SourceSite.Name, CharSets[DestSite]]);

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
{$IFNDEF Lite}
  if not CatchInvalidChar(Info, DestSite) then
{$ENDIF}
    Create(sInvalidCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Name]);

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
{$IFNDEF Lite}
  if not CatchInvalidChar(Info, DestSite) then
{$ENDIF}
    Create(sInvalidCodePage2, CP_LEGACY, [SourceSite.Name, DestSite.Name]);

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
  if FromLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if ToLatin(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if FromUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF32, Info, Self);
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if FromUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF16, Info, Self);
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  if FromUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
    Result := Info.Count
  else
    raise EConvert.Create(Source, csUTF8, Info, Self);
end;

function TCodePage.ToUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TUnicodeStrInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF})
  then
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
  with Info do
  begin
    FNumber := CodePage;
    FName := WideStrNew(CodePageName);
  end;

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
  Block := cbNonUnicode;
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

{$IFDEF Kolibri}
constructor TRawCodePage.Create(const RawMap: TRawMap);
begin
end;
{$ELSE}
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
{$ENDIF}

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
  I: Cardinal;
  C, D: LegacyChar;
  Uni: TLegacyStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if FromUTF8(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      C := Source[I];

      D := #0;
      if (C in [#0..#$7F]) or (soLatin1 in SourceOptions) then
        if WideChar(C) < FWideMapLo then
          D := C
        else if WideChar(C) <= FWideMapHi then
          D := FWideMap[Word(C) - Word(FWideMapLo)];

      if (D = #0) and (C <> #0) then
        if coForceInvalid in DestOptions then
        begin
          D := Unknown_Latin;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := QuadChar(C);
          Result := False;
          Exit;
        end;

      Dest[I] := D;
      Inc(Info.Count);
    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(C), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.ToLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TLegacyStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToLatin(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      C := Source[I];

      if C <> #0 then
      begin
        W := FSingleByteMap[C];
        case W of
          WideChar(1)..WideChar($7F):
            C := LegacyChar(W);
          WideChar($A0)..WideChar($FF):
            if coLatin1 in DestOptions then
              C := LegacyChar(W);
        else
          if coForceInvalid in DestOptions then
          begin
            C := Unknown_Latin;
            Inc(Info.InvalidCount);
          end
          else
          begin
            Info.InvalidChar := QuadChar(W);
            Result := False;
            Exit;
          end;
        end;
      end;

      Dest[I] := C;
      Inc(Info.Count);
    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(C), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
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
  I: Cardinal;
  C: LegacyChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
      SourceOptions := DetectUTF32(Source[0], SourceOptions);

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      Q := Source[I];
      if soBigEndian in SourceOptions then
      asm
        MOV EAX, Q
        BSWAP EAX
        MOV Q, EAX
      end;

      if Q = Unknown_UTF32 then
        C := Unknown_Latin
      else if Q < QuadChar(FWideMapLo) then
        C := LegacyChar(Q)
      else if Q <= QuadChar(FWideMapHi) then
      begin
        C := FWideMap[Q - QuadChar(FWideMapLo)];
        if C = #0 then
          if coForceInvalid in DestOptions then
          begin
            C := Unknown_Latin;                        
            Inc(Info.InvalidCount);
          end
          else
          begin
            Info.InvalidChar := Q;
            Result := False;
            Exit;
          end;
      end;

      Dest[I] := C;
      Inc(Info.Count);
    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(C), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.ToUTF32(var Info: TUnicodeStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  Q: QuadChar;
  Uni: TUnicodeStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      if C <> #0 then
      begin
        Q := QuadChar(FSingleByteMap[C]);
        if Q = 0 then
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
          end;
      end
      else
        Q := 0;

    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(Q, Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}

      if coBigEndian in DestOptions then
      asm
        MOV EAX, Q
        BSWAP EAX
        MOV Q, EAX
      end;

      Dest[I] := Q;
      with Info do
      begin
        Inc(Count);
        Inc(CharCount);
      end;
    end;
  end;

  Result := True;
end;
{$ENDIF}

function TSingleByteMemCodePage.FromUTF16(var Info: TLegacyStrInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  W, L: WideChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if soDetectCharSet in SourceOptions then
    SourceOptions := DetectUTF16(Source, SourceOptions);

{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}
  I := 0;
  while I <> Count do
  begin
    W := Source[I];
    if soBigEndian in SourceOptions then
      W := WideChar(Swap(Word(W)));

    if W = Unknown_UTF16 then
      C := Unknown_Latin
    else if W < FWideMapLo then
      C := LegacyChar(W)
    else if W <= FWideMapHi then
    begin
      C := FWideMap[Word(W) - Word(FWideMapLo)];
      if C = #0 then
      begin
        Q := QuadChar(W);
        case Word(W) of
          Low(THighSurrogates)..High(THighSurrogates):
          begin
            L := Source[I + 1];
            case Word(L) of
              Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                Inc(I);
                Q := $10000 + (Word(W) - Low(THighSurrogates)) * $400 +
                     Word(L) - Low(TLowSurrogates);
              end;
            end;
          end;
        end;

        if coForceInvalid in DestOptions then
        begin
          C := Unknown_Latin;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := Q;
          Result := False;
          Exit;
        end;
      end;
    end;

  {$IFNDEF Lite}
    if coRangeBlocks in DestOptions then
    begin
      Block := FindCharBlock(QuadChar(W), Block);
      Include(Info.Blocks, Block);
    end;
  {$ENDIF}
    Dest^ := C;
    Inc(Dest);
    Inc(Info.Count);
    Inc(I);
  end;

  Result := True;
end;

function TSingleByteMemCodePage.ToUTF16(var Info: TUnicodeStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TUnicodeStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      C := Source[I];
      if C <> #0 then
      begin
        W := FSingleByteMap[C];
        if W = WideChar(0) then
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
          end;
      end
      else
        W := WideChar(0);

    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(W), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
      if coBigEndian in DestOptions then
        W := WideChar(Swap(Word(W)));
      Dest[I] := W;
      with Info do
      begin
        Inc(Count);
        Inc(CharCount);
      end;
    end;
  end;

  Result := True;
end;

function TSingleByteMemCodePage.FromUTF8(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin

end;

function TSingleByteMemCodePage.ToUTF8(var Info: TUnicodeStrInfo;
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
  C, L: LegacyChar;
  I: Word;
  P: PTrailByteMap;
  T: LongWord;
  W: WideChar;
begin
  inherited;

{$IFNDEF Lite}
  Block := cbNonUnicode;
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
          Word(FWideMap[Word(W) - Word(FWideMapLo)]) := Byte(L) or (Byte(C) shl 8);
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
var
  I: Cardinal;
  C: LegacyChar;
  D: DoubleByteChar;
  Uni: TLegacyStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if FromUTF8(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      C := Source[I];

      Word(D) := 0;
      if (C in [#0..#$7F]) or (soLatin1 in SourceOptions) then
        if WideChar(C) < FWideMapLo then
          Word(D) := Word(C)
        else if WideChar(C) >= FWideMapLo then
          D := FWideMap[Word(C) - Word(FWideMapLo)];

      if (Word(D) = 0) and (C <> #0) then
        if coForceInvalid in DestOptions then
        begin
          Word(D) := Word(Unknown_Latin);
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := QuadChar(C);
          Result := False;
          Exit;
        end;

    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(C), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}

      if D.TrailByte <> #0 then
      begin
        PWord(Dest)^ := Word(D);
        Inc(Dest, 2);
        with Info do
        begin
          Inc(Count, 2);
          Inc(DoubleByteCount);
        end;
      end
      else
      begin
        Dest^ := D.SingleByte;
        Inc(Dest);
        Inc(Info.Count);
      end;
    end;
  end;

  Result := True;
end;

function TDoubleByteMemCodePage.ToLatin(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLatin): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TLegacyStrInfo;
  T: PTrailByteMap;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToLatin(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    I := 0;
    while I <> Count do
    begin
      C := Source[I];
      Inc(I);

      W := WideChar(0);
      if C in LeadBytes then
      begin
        T := FDoubleByteMap[C];
        if T <> nil then
        begin
          C := Source[I];
          Inc(I);
          W := T[C];
        end;
      end;

      if W = WideChar(0) then
        W := FSingleByteMap[C];

      case W of
        WideChar(1)..WideChar($7F):
          C := LegacyChar(W);
        WideChar($80)..WideChar($FF):
          if coLatin1 in DestOptions then
            C := LegacyChar(W);
      else
        if coForceInvalid in DestOptions then
        begin
          C := Unknown_Latin;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := QuadChar(C);
          Result := False;
          Exit;
        end;
      end;

      Dest^ := C;
      Inc(Dest);
      Inc(Info.Count);
    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(C), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
    end;
  end;

  Result := True;
end;
{$ENDIF}

{$IFDEF UTF32}
function TDoubleByteMemCodePage.FromUTF32(var Info: TLegacyStrInfo;
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Cardinal;
  D: DoubleByteChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
      SourceOptions := DetectUTF32(Source[0], SourceOptions);

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    for I := 0 to Count - 1 do
    begin
      Q := Source[I];
      if soBigEndian in SourceOptions then
      asm
        MOV EAX, Q
        BSWAP EAX
        MOV Q, EAX
      end;

      if Q = Unknown_UTF32 then
        Word(D) := Word(Unknown_Latin)
      else if Q < QuadChar(FWideMapLo) then
        Word(D) := Word(Q)
      else if Q <= QuadChar(FWideMapHi) then
      begin
        D := FWideMap[Q - QuadChar(FWideMapLo)];
        if Word(D) = 0 then
          if coForceInvalid in DestOptions then
          begin
            Word(D) := Word(Unknown_Latin);
            Inc(Info.InvalidCount);
          end
          else
          begin
            Info.InvalidChar := Q;
            Result := False;
            Exit;
          end;
      end;

      if D.TrailByte <> #0 then
      begin
        PWord(Dest)^ := Word(D);
        Inc(Dest, 2);
        with Info do
        begin
          Inc(Count, 2);
          Inc(DoubleByteCount);
        end;
      end
      else
      begin
        Dest^ := D.SingleByte;
        Inc(Dest);
        Inc(Info.Count);
      end;
    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(Q, Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
    end;
  end;

  Result := True;
end;

function TDoubleByteMemCodePage.ToUTF32(var Info: TUnicodeStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  T: PTrailByteMap;
  Q: QuadChar;
  Uni: TUnicodeStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    I := 0;
    while I <> Count do
    begin
      C := Source[I];
      Inc(I);

      Q := 0;
      if C in LeadBytes then
      begin
        T := FDoubleByteMap[C];
        if T <> nil then
        begin
          C := Source[I];
          Inc(I);
          Q := QuadChar(T[C]);
        end;
      end;

      if Q = 0 then
        Q := QuadChar(FSingleByteMap[C]);

      if Q = 0 then
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
        end;

    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(Q, Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}

      if coBigEndian in DestOptions then
      asm
        MOV EAX, Q
        BSWAP EAX
        MOV Q, EAX
      end;

      Dest[0] := Q;
      Inc(PLegacyChar(Dest), SizeOf(QuadChar));
      with Info do
      begin
        Inc(Count);
        Inc(CharCount);
      end;
    end;
  end;

  Result := True;
end;
{$ENDIF}

function TDoubleByteMemCodePage.FromUTF16(var Info: TLegacyStrInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  W, L: WideChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if soDetectCharSet in SourceOptions then
    SourceOptions := DetectUTF16(Source, SourceOptions);

{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}
  I := 0;
  while I <> Count do
  begin
    W := Source[I];
    if soBigEndian in SourceOptions then
      W := WideChar(Swap(Word(W)));

    if W = Unknown_UTF16 then
      C := Unknown_Latin
    else if W < FWideMapLo then
      C := LegacyChar(W)
    else if W <= FWideMapHi then
    begin
//      C := FWideMap[Word(W) - Word(FWideMapLo)];
      if C = #0 then
      begin
        Q := QuadChar(W);
        case Word(W) of
          Low(THighSurrogates)..High(THighSurrogates):
          begin
            L := Source[I + 1];
            case Word(L) of
              Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                Inc(I);
                Q := $10000 + (Word(W) - Low(THighSurrogates)) * $400 +
                     Word(L) - Low(TLowSurrogates);
              end;
            end;
          end;
        end;

        if coForceInvalid in DestOptions then
        begin
          C := Unknown_Latin;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := Q;
          Result := False;
          Exit;
        end;
      end;
    end;

  {$IFNDEF Lite}
    if coRangeBlocks in DestOptions then
    begin
      Block := FindCharBlock(QuadChar(W), Block);
      Include(Info.Blocks, Block);
    end;
  {$ENDIF}
    Dest^ := C;
    Inc(Dest);
    Inc(Info.Count);
    Inc(I);
  end;

  Result := True;
end;

function TDoubleByteMemCodePage.ToUTF16(var Info: TUnicodeStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Boolean;
var
  I: Cardinal;
  C: LegacyChar;
  T: PTrailByteMap;
  W: WideChar;
  Uni: TUnicodeStrInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      if UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions) then
      begin
        Info := Uni;
        Result := True;
        Exit;
      end;
    end;

  {$IFNDEF Lite}
    Block := cbNonUnicode;
  {$ENDIF}
    I := 0;
    while I <> Count do
    begin
      C := Source[I];
      Inc(I);

      W := WideChar(0);
      if C in LeadBytes then
      begin
        T := FDoubleByteMap[C];
        if T <> nil then
        begin
          C := Source[I];
          Inc(I);
          W := T[C];
        end;
      end;

      if W = WideChar(0) then
        W := FSingleByteMap[C];

      if W = WideChar(0) then
        if coForceInvalid in DestOptions then
        begin
          W := Unknown_UTF16;
          Inc(Info.InvalidCount);
        end
        else
        begin
          Info.InvalidChar := QuadChar(C);
          Result := False;
          Exit;
        end;

    {$IFNDEF Lite}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(W), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
      if coBigEndian in DestOptions then
        W := WideChar(Swap(Word(W)));
      Dest^ := W;
      Inc(Dest);
      with Info do
      begin
        Inc(Count);
        Inc(CharCount);
      end;
    end;
  end;

  Result := True;
end;

function TDoubleByteMemCodePage.FromUTF8(var Info: TLegacyStrInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Boolean;
begin
  // TODO
end;

function TDoubleByteMemCodePage.ToUTF8(var Info: TUnicodeStrInfo;
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

