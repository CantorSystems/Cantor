(*
    The Unified Environment Core Library

    Core string and character set implementation

    Copyright (c) 2012 The Unified Environment Laboratory

    TODO (first is more important):
      * Unicode NFC/NFD/NFKC/NFKD, UTF-7, SCSU, (BOCU is not patent-free)
      * Non-Unicode GB18030, ISO-2022, (also TRON?)

    Conditional defines:
      * Lite -- removes support of:
        * Character blocks
        * TString.Language
        * TString.Length(Source, MaxLength)
      * UTF32 -- UTF-32 character set support
*)

unit CoreStrings;

interface

uses
  Windows, Exceptions, CoreUtils, CoreClasses, CoreConsts;

{$I Unicode.inc}

type
  PStringInfo = ^TStringInfo;
  TStringInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (Latin1Count: Cardinal;
          InvalidChar: QuadChar);
      1: (DoubleByteCount: Cardinal);
      2: (SurrogateCount, InvalidCount, CharCount: Cardinal);
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

type
{
  soDetectCharSet:
    * Latin source: try to decode source as UTF-8, continue as latin if fails
    * Code page source: try to decode source as UTF-8, continue as code page if fails
    * UTF-8 source: try to decode source as code page, continue as UTF-8 if fails
    * UTF-16 and UTF-32: try to detect byte order
}
  TStringOption = (soDetectCharSet, soBigEndian, soAttachBuffer);

const
  soLatin1 = soBigEndian;

type
  TLegacyOptions = set of soLatin1..soAttachBuffer;
  TEndianOptions = set of soBigEndian..soAttachBuffer;

  TLegacySource = set of soDetectCharSet..soLatin1;
  TEndianSource = set of {$IFDEF Lite} soBigEndian {$ELSE} soDetectCharSet {$ENDIF}..soBigEndian;

{
  coForceInvalid -- replace invalid characters with:
    * U+001A for code page (recommended by Unicode for single-byte character sets)
    * U+FFFD for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coCompatibility,
    coForceInvalid, coRangeBlocks, coBigEndian, coSurrogates);

const
  coLatin1 = coBigEndian; // only without coCESU8
//  coCodePage = coCompatibility; //mutually exclusive with coEncodedZero

  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodedZero];  // UTF-8 compliance

  coNFC = [];
  coNFD = [coComposition];
  coNFKC = [coCompatibility];
  coNFKD = [coComposition, coCompatibility];

type
  TEncodeOptions = set of coForceInvalid..coSurrogates;
  TEncodeLegacy = TEncodeOptions;
  TEncodeUTF32 = set of coForceInvalid..coBigEndian;
  TEncodeUTF16 = set of coForceInvalid..coSurrogates;

{ Code page support }

type
  TCodePage = class
  private
    FNumber: Word;
    FName: PCoreChar;
  {$IFNDEF Lite}
    FBlocks: TUnicodeBlocks;
  {$ENDIF}
  public
    class function MaxCharBytes: Byte; virtual; abstract;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromLegacy(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PLegacyChar; CodePage: TCodePage = nil;
      DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function ToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; CodePage: TCodePage = nil; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Cardinal; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Cardinal; overload; virtual; abstract;
    function ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
  {$ENDIF}

  // properties
    property Number: Word read FNumber;
    property Name: PCoreChar read FName;
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
    constructor Create(const Info: TCPInfoEx); overload;
    destructor Destroy; override;

    class function Create(CodePage: Word): TMemoryCodePage; overload;

  // properties
    property WideMapCount: Word read GetWideMapCount;
    property WideMapLo: WideChar read FWideMapLo;
    property WideMapHi: WideChar read FWideMapHi;
  end;

  TSingleByteCodePage = class(TMemoryCodePage)
  private
    FWideMap: PLegacyChar;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;
    function IsEBCDIC: Boolean;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PLegacyChar; CodePage: TCodePage = nil;
      DestOptions: TEncodeLegacy = []): Cardinal; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Cardinal; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Cardinal; override;
  {$ENDIF}

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

  TDoubleByteCodePage = class(TMemoryCodePage)
  private
    FDoubleByteMap: TDoubleByteMap;
    FWideMap: PWideMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; override;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PLegacyChar; CodePage: TCodePage = nil;
      DestOptions: TEncodeLegacy = []): Cardinal; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16 = []): Cardinal; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32 = []): Cardinal; override;
  {$ENDIF}

  // properties
    property SingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideMap: PWideMap read FWideMap;
  end;

{ Subtrings }

  TStringOptions = set of TStringOption;

  TSubstring = class(TIndexed)
  private
  {$IFNDEF Lite}
    FLanguage: Word;
  {$ENDIF}
  //  { placeholder }  FData: Pointer;
  //  { placeholder }  FOptions: TStringOptions;
  public
    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    function ByteCount: Cardinal; overload; virtual;

    class function Length(Source: Pointer): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; virtual; abstract;
    property Language: Word read FLanguage write FLanguage;
  {$ENDIF}
  end;

  TLegacySubstring = class(TSubstring)
  private
    FData: PLegacyChar;
  {$IFDEF Latin}
    FOptions: TLegacyOptions;
  {$ENDIF}
  public
  // properties
    property Data: PLegacyChar read FData;
  {$IFDEF Latin}
    property Options: TLegacyOptions read FOptions;
  {$ENDIF}
  end;

{$IFDEF Latin}
  TLatinSubstring = TLegacySubstring;
  TUTF8Substring = TLegacySubstring;
{$ELSE}
  TLatinSubstring = class(TLegacySubstring)
  private
    FOptions: TLegacyOptions;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;

  TUTF8Substring = class(TLegacySubstring)
  private
    FOptions: TLegacyOptions;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;
{$ENDIF}

  TCodePageSubstring = class(TLegacySubstring)
  private
  {$IFNDEF Latin}
    FOptions: TLegacyOptions;
  {$ENDIF}
    FCodePage: TCodePage;
  public
  // properties
    property CodePage: TCodePage read FCodePage;
  {$IFNDEF Latin}
    property Options: TLegacyOptions read FOptions;
  {$ENDIF}
  end;

  TWideSubstring = class(TSubstring)
  private
    FData: PWideChar;
    FOptions: TEndianOptions;
  public
  // properties
    property Data: PWideChar read FData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadSubstring = class(TSubstring)
  private
    FData: PQuadChar;
    FOptions: TEndianOptions;
  public
    property Data: PQuadChar read FData;
    property Options: TEndianOptions read FOptions;
  end;

{ Strings }

  TString = class(TSubstring)
  private
    procedure SetCount(Value: Cardinal);
  protected
    procedure DoAssign(Value: Pointer; Count: Cardinal; Options: TStringOptions); overload;

    function DoAssign(var Info; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; overload;

    function DoAssign(var Info; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload;

  {$IFDEF UTF32}
    function DoAssign(var Info; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload;
  {$ENDIF}

    function DoAssign(var Info; Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions): Cardinal; overload;
    function DoAssign(Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions): Cardinal; overload;

    function DoAssign(var Info; Source: TSubstring; Index: Integer;  // negative indexes like in Oracle
      DestOptions: TEncodeOptions): Cardinal; overload;
    function DoAssign(var Info; Source: TSubstring; Index: Integer;
      DestOptions: TEncodeOptions): Cardinal; overload;

  public
    constructor Create(Source: Pointer; Count: Cardinal; Options: TStringOptions = []); overload;
    constructor Create(Source: Pointer; Options: TStringOptions = []); overload;

    constructor Create(var Info; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions = []); overload;

    constructor Create(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions = []); overload;

  {$IFDEF UTF32}
    constructor Create(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions = []); overload;
  {$ENDIF}

    constructor Create(var Info; Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions = []); overload;

    constructor Create(var Info; Source: TSubstring; Index: Integer = 0;
      DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: TSubstring; Index: Integer = 0;
      DestOptions: TEncodeOptions = []); overload;

    procedure Assign(Value: Pointer; Count: Cardinal; Options: TStringOptions = []); overload;
    procedure Assign(Value: Pointer; Options: TStringOptions = []); overload;

    function Assign(var Info; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function Assign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; overload;

    function Assign(var Info; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function Assign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload;

  {$IFDEF UTF32}
    function Assign(var Info; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function Assign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      DestOptions: TEncodeOptions): Cardinal; overload;
  {$ENDIF}

    function Assign(var Info; Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(Source: TSubstring; Index: Integer; Count: Cardinal;
      DestOptions: TEncodeOptions): Cardinal; overload;

    function Assign(var Info; Source: TSubstring; Index: Integer = 0;  // negative indexes like in Oracle
      DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(var Info; Source: TSubstring; Index: Integer = 0;
      DestOptions: TEncodeOptions = []): Cardinal; overload;

  // properties
    property Count write SetCount;
  end;

  TLegacyString = class(TString)
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
    FOptions: TLegacyOptions;
  protected
  {$IFDEF Latin}
    function DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  {$IFDEF UTF32}
    function DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
    function DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
    function DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;

  TCodePageString = class(TLegacyString)
  private
    FOptions: TLegacyOptions;
    FCodePage: TCodePage;
  protected
  {$IFDEF Latin}
    function DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  {$IFDEF UTF32}
    function DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
    function DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
    function DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  public
    constructor Create(Value: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      Options: TStringOptions = []); overload;
    procedure Assign(Value: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      Options: TStringOptions = []); overload;
  // properties
    property CodePage: TCodePage read FCodePage;
    property Options: TLegacyOptions read FOptions;
  end;

  TUTF8String = class(TLegacyString)
  private
    FOptions: TLegacyOptions;
  protected
  {$IFDEF Latin}
    function DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  {$IFDEF UTF32}
    function DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
    function DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
    function DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  public
  // properties
    property Options: TLegacyOptions read FOptions;
  end;

{$IFNDEF Lite}
  TEndianString = class(TString)
  protected
    procedure DoSwapByteOrder; virtual; abstract;
  public
    procedure SwapByteOrder;
  end;
{$ELSE}
  TEndianString = TString;
{$ENDIF}

  TWideString = class(TEndianString)
  private
    FData: PWideChar;
    FOptions: TEndianOptions;
    procedure SetData(Value: PWideChar);
  protected
  {$IFDEF Latin}
    function DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  {$IFDEF UTF32}
    function DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
    function DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
    function DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$IFNDEF Lite}
    procedure DoSwapByteOrder; override;
  {$ENDIF}
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

  TQuadString = class(TEndianString)
  private
    FData: PQuadChar;
    FOptions: TEndianOptions;
    procedure SetData(Value: PQuadChar);
  protected
  {$IFDEF Latin}
    function DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  {$IFDEF UTF32}
    function DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
    function DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal; override;
    function DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal; override;
  {$IFNDEF Lite}
    procedure DoSwapByteOrder; override;
  {$ENDIF}
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

  TCoreString = TWideString; // TODO: non-Unicode

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
    FInfo: TStringInfo;
    function CatchInvalidChar(const Info: TStringInfo; DestSite: TCharSet): Boolean; overload;
    function CatchInvalidChar(const Info: TStringInfo; DestSite: TCodePage): Boolean; overload;
  public
    constructor Create(Source: Pointer; SourceSite: TCharSet;
      const Info; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCharSet;
      const Info: TStringInfo; DestSite: TCodePage); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage;
      const Info; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage;
      const Info: TStringInfo; DestSite: TCodePage); overload;
  // properties
    property DestSite: TConvertSite read FDestSite;
    property Info: TStringInfo read FInfo;
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

function LatinToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF32ToLegacy(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF16ToLegacy(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF8ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

function LatinToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF32ToUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF16ToUTF32(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF8ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;

function LatinToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF32ToUTF16(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF16ToUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF8ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

function LatinToUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF32ToUTF8(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF16ToUTF8(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF8ToUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;

function LatinToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF32ToLegacy(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF16ToLegacy(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;
function UTF8ToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

function LatinToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;

function LatinToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;
function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

function LatinToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;
function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8 = []): Cardinal; overload;

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar);
procedure SwapWideCharBytes(Source: PWideChar; Count: Cardinal; Dest: PWideChar);

function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource;
function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource;

function EstimateUTF8(const Info: TStringInfo; Options: TEncodeUTF8 = []): Cardinal; overload;
function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16 = []): Cardinal; overload;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar);
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

procedure SwapWideCharBytes(Source: PWideChar; Count: Cardinal; Dest: PWideChar);
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

function LatinToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF32ToLegacy(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF16ToLegacy(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF8ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function LatinToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF32ToUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF16ToUTF32(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF8ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
begin
  Result := 0; // TODO
end;

function LatinToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF32ToUTF16(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF16ToUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF8ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
begin
  Result := 0; // TODO
end;

function LatinToUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF32ToUTF8(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF16ToUTF8(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
end;

function UTF8ToUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
end;

function LatinToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (LatinToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions),
      Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count
end;

function UTF32ToLegacy(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF32ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF32, Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function UTF16ToLegacy(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF16ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF16, Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function UTF8ToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF8ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF8, Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function LatinToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (LatinToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF32);
  Result := Info.Count;
end;

function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF32ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF32, Info, csUTF32);
  Result := Info.Count;
end;

function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF16ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF16, Info, csUTF32);
  Result := Info.Count;
end;

function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF8ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF8, Info, csUTF32);
  Result := Info.Count;
end;

function LatinToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (LatinToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF16);
  Result := Info.Count;
end;

function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF32ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF32, Info, csUTF16);
  Result := Info.Count;
end;

function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF16ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF16, Info, csUTF16);
  Result := Info.Count;
end;

function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF8ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF8, Info, csUTF16);
  Result := Info.Count;
end;

function LatinToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (LatinToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, csUTF8);
  Result := Info.Count;
end;

function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF32ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF32, Info, csUTF8);
  Result := Info.Count;
end;

function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF16ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF16, Info, csUTF8);
  Result := Info.Count;
end;

function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (UTF8ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF8, Info, csUTF8);
  Result := Info.Count;
end;

function EstimateUTF8(const Info: TStringInfo; Options: TEncodeUTF8): Cardinal;
begin
  if coCESU8 in Options then
    Result := (Info.CharCount + Info.SurrogateCount) * 3
  else
    Result := Info.CharCount * 3 + Info.SurrogateCount;
end;

function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16): Cardinal;
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

function EConvert.CatchInvalidChar(const Info: TStringInfo; DestSite: TCharSet): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(TStringInfo(Info).InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCharSetChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(TStringInfo(Info).InvalidChar), CharSets[DestSite]]);
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidChar(const Info: TStringInfo; DestSite: TCodePage): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(TStringInfo(Info).InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCodePageChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(TStringInfo(Info).InvalidChar), DestSite.Number, DestSite.Name]);
    Result := True;
  end
  else
    Result := False;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet;
  const Info; DestSite: TCharSet);
begin
  if (DestSite >= csUTF8) and (TStringInfo(Info).SurrogateCount <> 0) then
  begin
    Create(sSurrogates, [CharSets[SourceSite]]);
    FInfo := TStringInfo(Info);
  end
  else
  begin
  {$IFDEF Lite}
    Create(sInvalidCharSet2, [CharSets[SourceSite], CharSets[DestSite]]);
  {$ELSE}
    if not CatchInvalidChar(TStringInfo(Info), DestSite) then
      Create(sNonUnicode, [CharSets[SourceSite]]);
  {$ENDIF}
    Move(Info, FInfo, SizeOf(TStringInfo));
  end;

  FDestSite.CharSet := DestSite;     // SiteType filled with 0 = stCharSet
  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCodePage;
  const Info; DestSite: TCharSet);
begin
{$IFNDEF Lite}
  if not CatchInvalidChar(TStringInfo(Info), DestSite) then
{$ENDIF}
    Create(sInvalidCharSet, CP_LEGACY, [SourceSite.Number, SourceSite.Name, CharSets[DestSite]]);

  FDestSite.CharSet := DestSite; // SiteType filled with 0 = stCharSet
  if DestSite < csUTF8 then
    Move(Info, FInfo, SizeOf(TStringInfo))
  else
    FInfo := TStringInfo(Info);

  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet;
  const Info: TStringInfo; DestSite: TCodePage);
begin
{$IFNDEF Lite}
  if not CatchInvalidChar(Info, DestSite) then
{$ENDIF}
    Create(sInvalidCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Number, DestSite.Name]);

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
  const Info: TStringInfo; DestSite: TCodePage);
begin
{$IFNDEF Lite}
  if not CatchInvalidChar(Info, DestSite) then
{$ENDIF}
    Create(sInvalidCodePage2, CP_LEGACY, [SourceSite.Number, SourceSite.Name, DestSite.Number, DestSite.Name]);

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
function TCodePage.FromLegacy(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, Self);
  Result := Info.Count;
end;

function TCodePage.ToLegacy(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, Self, Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;
{$ENDIF}

{$IFDEF UTF32}
function TCodePage.FromUTF32(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF32, Info, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, Self, Info, csUTF32);
  Result := Info.Count;
end;
{$ENDIF}

function TCodePage.FromUTF16(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF16, Info, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, Self, Info, csUTF16);
  Result := Info.Count;
end;

function TCodePage.FromUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, csUTF8, Info, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF8(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, Self, Info, csUTF8);
  Result := Info.Count;
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
  I, Tail: Integer;
  T: LongWord;
  W: WideChar;
  Y: PWideChar;
begin
  with Info do
  begin
    FNumber := CodePage;

    for I := Low(CodePageName) to High(CodePageName) do
      case CodePageName[I] of
        WideChar(0)..WideChar(32), WideChar('0')..WideChar('9'):;
        WideChar('('):
          begin
            Y := @CodePageName[I];
            Tail := WideStrLen(Y, Length(CodePageName) - I);
            if CodePageName[Tail + I - 1] = WideChar(')') then
            begin
              Inc(Y);
              Dec(Tail, 2);
            end;
            FName := WideStrNew(Y, Tail);
            Break;
          end;
      else
        Y := @CodePageName[I];
        FName := WideStrNew(Y, WideStrLen(Y, Length(CodePageName) - I));
        Break;
      end;
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
function TSingleByteMemCodePage.FromLegacy(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  I: Cardinal;
  C, D: LegacyChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := FromUTF8(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
          Result := 0;
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

//  Result := ; TODO
end;

function TSingleByteMemCodePage.ToLegacy(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToLegacy(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
            Result := 0;
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

//  Result := True; TODO
end;
{$ENDIF}

{$IFDEF UTF32}
function TSingleByteMemCodePage.FromUTF32(var Info: TStringInfo;
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
            Result := 0;
            Exit;
          end;
      end;

      Dest[I] := C;  // TODO: warning
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

//  Result := True; TODO
end;

function TSingleByteMemCodePage.ToUTF32(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  Q: QuadChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
            Result := 0;
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

//  Result := True; TODO
end;
{$ENDIF}

function TSingleByteMemCodePage.FromUTF16(var Info: TStringInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
          Result := 0;
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
    Dest^ := C;  // TODO: warning
    Inc(Dest);
    Inc(Info.Count);
    Inc(I);
  end;

//  Result := True; TODO
end;

function TSingleByteMemCodePage.ToUTF16(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
            Result := 0;
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

//  Result := True; TODO
end;

function TSingleByteMemCodePage.FromUTF8(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function TSingleByteMemCodePage.ToUTF8(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
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
function TDoubleByteMemCodePage.FromLegacy(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  D: DoubleByteChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := FromUTF8(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
          Result := 0;
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

//  Result := True; TODO
end;

function TDoubleByteMemCodePage.ToLegacy(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  W: WideChar;
  Uni: TStringInfo;
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
      Result := UTF8ToLegacy(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
          Result := 0;
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

//  Result := True; TODO
end;
{$ENDIF}

{$IFDEF UTF32}
function TDoubleByteMemCodePage.FromUTF32(var Info: TStringInfo;
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
            Result := 0;
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

//  Result := True; TODO
end;

function TDoubleByteMemCodePage.ToUTF32(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  T: PTrailByteMap;
  Q: QuadChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
          Result := 0;
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

//  Result := True; TODO
end;
{$ENDIF}

function TDoubleByteMemCodePage.FromUTF16(var Info: TStringInfo;
  Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
      if C = #0 then    // TODO: warning
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
          Result := 0;
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

//  Result := True; TODO
end;

function TDoubleByteMemCodePage.ToUTF16(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  I: Cardinal;
  C: LegacyChar;
  T: PTrailByteMap;
  W: WideChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if Count <> 0 then
  begin
    if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
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
          Result := 0;
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

//  Result := True; TODO
end;

function TDoubleByteMemCodePage.FromUTF8(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
begin
  Result := 0; // TODO
end;

function TDoubleByteMemCodePage.ToUTF8(var Info: TStringInfo;
  Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8): Cardinal;
begin
  Result := 0; // TODO
end;

{ TSubstring }

function TSubstring.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

{ TString }

constructor TString.Create(Source: Pointer; Count: Cardinal; Options: TStringOptions);
begin
  DoAssign(Source, Count, Options);
end;

constructor TString.Create(Source: Pointer; Options: TStringOptions);
begin
  DoAssign(Source, Length(Source), Options); // see also TCodePageString.Create
end;

{$IFDEF Latin}
constructor TString.CreateFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions);
begin
  DoFromLegacy(Info, Source, Count, SourceOptions, DestOptions);
end;

constructor TString.CreateFromLegacy(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions);
begin
  DoFromLegacy(Source, Count, SourceOptions, DestOptions);
end;

constructor TString.Create(var Info; Source: TLatinSubstring; Options: TEncodeOptions);
begin
  DoFromLegacy(Info, Source, Options);
end;

constructor TString.Create(Source: TLatinSubstring; Options: TEncodeOptions);
begin
  DoFromLegacy(Source, Options);
end;
{$ENDIF}

{$IFDEF UTF32}
constructor TString.CreateFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions);
begin
  DoFromUTF32(Info, Source, Count, SourceOptions, DestOptions);
end;

constructor TString.CreateFromUTF32(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions);
begin
  DoFromUTF32(Source, Count, SourceOptions, DestOptions);
end;

constructor TString.Create(var Info; Source: TQuadSubstring; Options: TEncodeOptions);
begin
  DoFromUTF32(Info, Source, Options);
end;

constructor TString.Create(Source: TQuadSubstring; Options: TEncodeOptions);
begin
  DoFromUTF32(Source, Options);
end;
{$ENDIF}

constructor TString.CreateFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions);
begin
  DoFromUTF16(Info, Source, Count, SourceOptions, DestOptions);
end;

constructor TString.CreateFromUTF16(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions);
begin
  DoFromUTF16(Source, Count, SourceOptions, DestOptions);
end;

constructor TString.Create(var Info; Source: TWideSubstring; Options: TEncodeOptions);
begin
  DoFromUTF16(Info, Source, Options);
end;

constructor TString.Create(Source: TWideSubstring; Options: TEncodeOptions);
begin
  DoFromUTF16(Source, Options);
end;

constructor TString.CreateFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions);
begin
  DoFromUTF8(Info, Source, Count, SourceOptions, DestOptions);
end;

constructor TString.CreateFromUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions);
begin
  DoFromUTF8(Source, Count, SourceOptions, DestOptions);
end;

constructor TString.Create(var Info; Source: TUTF8Substring; Options: TEncodeOptions);
begin
  DoFromUTF8(Info, Source, Options);
end;

constructor TString.Create(Source: TUTF8Substring; Options: TEncodeOptions);
begin
  DoFromUTF8(Source, Options);
end;

procedure TString.DoAssign(Value: Pointer; Count: Cardinal; Options: TStringOptions);
var
  Bytes, Zero: Cardinal;
begin
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
end;

procedure TString.Assign(Value: Pointer; Count: Cardinal; Options: TStringOptions);
begin
  Lock;
  try
    DoAssign(Value, Count, Options);
  finally
    Unlock;
  end;
end;

procedure TString.Assign(Value: Pointer; Options: TStringOptions);
begin
  Assign(Value, Length(Value), Options);
end;

procedure TString.SetCount(Value: Cardinal);
begin
  if FCount <> Value then
    Assign(nil, Value);
end;

{$IFDEF Latin}
function TString.DoFromLegacy(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
var
  Info: TStringInfoCast;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (DoFromLegacy(Info, Source, Count, SourceOptions, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions),
      Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function TString.DoFromLegacy(var Info; Source: TLatinSubstring; Options: TEncodeOptions): Cardinal;
begin
  Source.BeginConsistentRead;
  try
    Result := DoFromLegacy(Info, Source.Data, Source.Count, Source.Options, Options);
  finally
    Source.EndConsistentRead;
  end;
end;

function TString.DoFromLegacy(Source: TLatinSubstring; Options: TEncodeOptions): Cardinal;
begin
  Source.BeginConsistentRead;
  try
    Result := DoFromLegacy(Source.Data, Source.Count, Source.Options, Options);
  finally
    Source.EndConsistentRead;
  end;
end;
{$ENDIF}

{$IFDEF UTF32}
function TString.DoFromUTF32(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
var
  Info: TStringInfoCast;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (DoFromUTF32(Info, Source, Count, SourceOptions, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions),
      Info, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function TString.DoFromUTF32(var Info; Source: TQuadSubstring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF32(Source: TQuadSubstring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{$ENDIF}

function TString.DoFromUTF16(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF16(var Info; Source: TWideSubstring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF16(Source: TWideSubstring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF8(var Info; Source: TUTF8Substring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.DoFromUTF8(Source: TUTF8Substring; Options: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{$IFDEF Latin}
function TString.FromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions = []): Cardinal;
begin
  Lock;
  try
    Result := DoFromLegacy(Info, Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromLegacy(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromLegacy(Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromLegacy(var Info; Source: TLatinSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromLegacy(Info, Source, Options);
  finally
    Unlock;
  end;
end;

function TString.FromLegacy(Source: TLatinSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromLegacy(Source, Options);
  finally
    Unlock;
  end;
end;
{$ENDIF}

{$IFDEF UTF32}
function TString.FromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF32(Info, Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF32(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF32(Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF32(var Info; Source: TQuadSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF32(Info, Source, Options);
  finally
    Unlock;
  end;
end;

function TString.FromUTF32(Source: TQuadSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF32(Source, Options);
  finally
    Unlock;
  end;
end;
{$ENDIF}

function TString.FromUTF16(var Info; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF16(Info, Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF16(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF16(Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF16(var Info; Source: TWideSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF16(Info, Source, Options);
  finally
    Unlock;
  end;
end;

function TString.FromUTF16(Source: TWideSubstring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF16(Source, Options);
  finally
    Unlock;
  end;
end;

function TString.FromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF8(Info, Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF8(Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF8(Source, Count, SourceOptions, DestOptions);
  finally
    Unlock;
  end;
end;

function TString.FromUTF8(var Info; Source: TUTF8Substring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF8(Info, Source, Options);
  finally
    Unlock;
  end;
end;

function TString.FromUTF8(Source: TUTF8Substring; Options: TEncodeOptions): Cardinal;
begin
  Lock;
  try
    Result := DoFromUTF8(Source, Options);
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
  if FData <> Value then
    Assign(Value, StrLen(Value));
end;

{ TLatinString }

function TLatinString.DoFromLegacy(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
{  if Count <> 0 then
  begin
    DoAssign(nil, Count, []);
    Result := LatinToLegacy(TStringInfo(Info), Source, Count,
      TLegacySource(SourceOptions), FData, TEncodeLegacy(DestOptions));
  end
  else
    Result := True;} //TODO
end;

function TLatinString.DoFromUTF16(var Info; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TLatinString.DoFromUTF32(var Info; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TLatinString.DoFromUTF8(var Info; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{ TCodePageString }

constructor TCodePageString.Create(Value: PLegacyChar; Count: Cardinal;
  CodePage: TCodePage; Options: TStringOptions);
begin
  DoAssign(Value, Count, Options); // fast core
  FCodePage := CodePage;
end;

procedure TCodePageString.Assign(Value: PLegacyChar; Count: Cardinal;
  CodePage: TCodePage; Options: TStringOptions);
begin
  Lock;
  try
    DoAssign(Value, Count, Options);
    FCodePage := CodePage;
  finally
    Unlock;
  end;
end;

{ TEndianString }

{$IFNDEF Lite}
procedure TEndianString.SwapByteOrder;
begin
  Lock;
  try
    DoSwapByteOrder;
  finally
    Unlock;
  end;
end;
{$ENDIF}

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

procedure TWideString.DoSwapByteOrder;
begin
  UTF16SwapByteOrder(FData, FCount, FData);
end;
{$ENDIF}

procedure TWideString.SetData(Value: PWideChar);
begin
  if FData <> Value then
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

procedure TQuadString.DoSwapByteOrder;
begin
  UTF32SwapByteOrder(FData, FCount, FData);
end;
{$ENDIF}

procedure TQuadString.SetData(Value: PQuadChar);
begin
  if FData <> Value then
    Assign(Value, QuadStrLen(Value));
end;

end.

