(*
    Lite Core Library (CoreLite)

    Core string and character set implementation

    Copyright © 2012-2013 Vladislav Javadov (Freeman)

    TODO (first is more important):
      * Unicode NFC/NFD/NFKC/NFKD, UTF-7, SCSU, (BOCU is not patent-free)
      * Non-Unicode GB18030, ISO-2022 (also TRON?)

    Conditional defines:
      * Lite -- removes support of:
        * Character blocks
        * TString.Language
        * TString.Length(Source, MaxLength)
      * LiteStrings -- no code page support for strings
      * UTF32 -- UTF-32 character set support
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreConsts;

{$I Unicode.inc}

type
{
  soDetectCharSet:
    * Legacy source: try to decode source as UTF-8, continue as code page or Latin1 if code page is null
    * UTF-16 and UTF-32: try to detect byte order
}
  TStringsOption = (soDetectCharSet, soBigEndian, soAttachBuffer);

const
  soLatin1 = soBigEndian;

type
  TLegacyOptions = set of soDetectCharSet..soAttachBuffer;
  TEndianOptions = set of soBigEndian..soAttachBuffer;

  TLegacySource = TLegacyOptions;
  TEndianSource = TEndianOptions;

  TStringSource = TLegacySource;

{
  coForceInvalid -- replace invalid characters with:
    * U+001A for code page (recommended by Unicode for single-byte character sets)
    * U+FFFD for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coCompatibility,
    coContinuous, coForceInvalid, {$IFNDEF Lite} coRangeBlocks, {$ENDIF}
    coBigEndian, coSurrogates);

const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodedZero];  // UTF-8 compliance

  coNFC = [];
  coNFD = [coComposition];
  coNFKC = [coCompatibility];
  coNFKD = [coComposition, coCompatibility];

type
  TEncodeOptions = set of coContinuous..coSurrogates;
  TEncodeLegacy = TEncodeOptions;
  TEncodeUTF16 = set of coContinuous..coSurrogates;
  TEncodeUTF32 = set of coContinuous..coBigEndian;

{ Code page support }

const
  InvalidUTFMask = $80000000;

type
  TInvalidUTF = packed record // platform
    case Byte of
      0: (StartingByte: Byte);               // Bad UTF-8 sequence starting with byte $%02X
      1: (BrokenSequence, BrokenByte: Byte); // Broken %u-byte UTF-8 sequence at %s byte
      2: (Surrogate: WideChar;
          UTF: (ivNone, ivUTF8, ivUTF16));
  end;

  TCodePage = class;

  TStringInfo = record
    Count, CharCount: Integer;
    CodePage: TCodePage;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidCount, Latin1Count: Integer);
      1: (InvalidChar: QuadChar;
          DoubleByteCount: Integer);
      2: (InvalidUTF: TInvalidUTF;
          SequenceCount, SurrogatePairCount: Integer);
  end;

  TCodePage = class
  private
    FNumber: Word;
    FName: PCoreChar;
  {$IFNDEF Lite}
    FBlocks: TUnicodeBlocks;
  {$ENDIF}
  public
    class function MaxCharBytes: Byte; virtual; abstract;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload; virtual; abstract;
    function FromLegacy(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload; virtual; abstract;
    function ToLegacy(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PLegacyChar; CodePage: TCodePage = nil; DestOptions: TEncodeLegacy = []): Integer; overload;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload; virtual; abstract;
    function FromUTF16(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload;

    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer; overload;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload; virtual; abstract;
    function FromUTF32(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; overload;

    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Integer; overload; virtual; abstract;
    function ToUTF32(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Integer; overload;
  {$ENDIF}

  {$IFNDEF Lite}
    property Blocks: TUnicodeBlocks read FBlocks;
  {$ENDIF}
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
  end;

{ Subtrings }

  TStringOptions = TLegacyOptions;

  TLineBreak = (lbNone, lbLF, lbCR, lbLFCR, lbCRLF); // ordered
const
  LineBreakChars: array[Boolean] of LegacyChar = #10#13;

type
  TTextSplit = record
    Count: Integer;
    LineBreak: TLineBreak;
  end;

  TSubstring = class(TIndexed)
  private
    FChildCount: Integer;
  { placeholder } // FData: Pointer;
  { placeholder } // FOptions: TStringOptions;
  public
    {class} function ByteCount(Count: Integer): Integer; overload; virtual; abstract;
    function ByteCount: Integer; overload;
    {class} function Length(Source: Pointer): Integer; overload; virtual; abstract;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; overload; virtual; abstract;
  {$ENDIF}
    property ChildCount: Integer read FChildCount;
  end;

  TLegacySubstring = class(TSubstring)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TLegacyOptions;
  {$IFNDEF LiteStrings}
    FCodePage: TCodePage;
  public
    property CodePage: TCodePage read FCodePage;
  {$ENDIF}
  public
    property Data: PLegacyChar read FData;
    property Options: TLegacyOptions read FOptions;
  end;

  TWideSubstring = class(TSubstring)
  private
  { hold } FData: PWideChar;
  { hold } FOptions: TEndianOptions;
  public
    property Data: PWideChar read FData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadSubstring = class(TSubstring)
  private
  { hold } FData: PQuadChar;
  { hold } FOptions: TEndianOptions;
  public
    property Data: PQuadChar read FData;
    property Options: TEndianOptions read FOptions;
  end;

{ Strings }

{$IFDEF LiteStrings}
  {$I LiteStrings.inc}
{$ELSE}
  TString = class(TSubstring)
  private
  { placeholder } // FData: Pointer;
  { placeholder } // FOptions: TStringOptions;
    procedure SetCount(Value: Integer);
  protected
    procedure AcceptRange(Index, Count: Integer);
    procedure CheckIndex(Index: Integer); override;
    procedure DoInsert(Source: Pointer; Count: Integer; SourceOptions: TStringOptions; DestIndex: Integer);
  public
    procedure Clear; override;
  {$IFNDEF Lite}
    //procedure Format(const Args: array of const); overload; virtual; abstract;
  {$ENDIF}

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Insert(Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;

    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Insert(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;

  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Insert(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$ENDIF}

    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; overload; virtual; abstract;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; overload;

    property Count write SetCount;
  end;

  TLegacyString = class(TString)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TLegacyOptions;
    FCodePage: TCodePage;
    procedure SetData(Value: PLegacyChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
  {$ENDIF}
    procedure Format(Source: PLegacyChar; const Args: array of const); overload;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$ENDIF}

    function IsUTF8(ThresholdBytes: Integer = 4): Boolean;
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; override;

    property Data: PLegacyChar read FData write SetData;
    property Options: TLegacyOptions read FOptions;
  end;

{$IFNDEF Lite}
  TEndianString = class(TString)
  public
    procedure SwapByteOrder; virtual; abstract;
  end;
{$ELSE}
  TEndianString = TString;
{$ENDIF}

  TWideString = class(TEndianString)
  private
  { hold } FData: PWideChar;
  { hold } FOptions: TEndianOptions;
    procedure SetData(Value: PWideChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
    procedure SwapByteOrder; override;
  {$ENDIF}
    procedure Format(Source: PWideChar; const Args: array of const); overload;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$ENDIF}

    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; override;

    property Data: PWideChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadString = class(TEndianString)
  private
  { hold } FData: PQuadChar;
  { hold } FOptions: TEndianOptions;
    procedure SetData(Value: PQuadChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
    procedure SwapByteOrder; override;
  {$ENDIF}

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFNDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
    function Insert(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$ENDIF}

    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; override;

    property Data: PQuadChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TSharedString = class(TString)
  private
  { hold } FData: Pointer;
  { hold } FOptions: TStringOptions;
  { hold to TLegacyString } FCodePage: TCodePage;
    FParent: TString; // use soAttachBuffer to share other shared string
    function GetAsLegacyChar: PLegacyChar;
    function GetAsWideChar: PWideChar;
    procedure SetAsLegacyChar(Value: PLegacyChar);
    procedure SetAsWideChar(Value: PWideChar);
  {$IFDEF UTF32}
    function GetAsQuadChar: PQuadChar;
    procedure SetAsQuadChar(Value: PQuadChar);
  {$ENDIF}
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; overload; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; overload; override;
  {$ENDIF}
    procedure Clear; override;
    function IsUnique: Boolean;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$ENDIF}

    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; override;

    property AsCoreChar: PWideChar read GetAsWideChar write SetAsWideChar; // TODO: non-Unicode
    property AsLegacyChar: PLegacyChar read GetAsLegacyChar write SetAsLegacyChar;
    property AsWideChar: PWideChar read GetAsWideChar write SetAsWideChar;
  {$IFDEF UTF32}
    property AsQuadChar: PQuadChar read GetAsQuadChar write SetAsQuadChar;
  {$ENDIF}
    property CodePage: TCodePage read FCodePage;
    property Data: Pointer read FData;
    property Options: TStringOptions read FOptions;
    property Parent: TString read FParent;
  end;
{$ENDIF LiteStrings}

  TCoreString = TWideString; // TODO: non-Unicode

  TStrings = class(TObjects)
  public
    function Load(Source: TSubstring; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload; virtual;
  {$IFNDEF LiteStrings}
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload; virtual;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload;
  {$ENDIF}
    function TextLength: Integer;
  end;

const
  UTF8BOMChars: array[0..2] of LegacyChar = #$BF#$BB#$EF; // BOM_UTF8 const from Unicode.inc is Integer

type
  PLegacyStringArray = ^TLegacyStringArray;
  TLegacyStringArray = array[0..MaxInt div SizeOf(TLegacyString) - 1] of TLegacyString;

  TLegacyStrings = class(TStrings)
  private
  { hold } FItems: PLegacyStringArray;
  { hold } FOwnsStrings: Boolean;
    procedure Append(Source: PLegacyChar; Count: Integer; const Options); reintroduce; overload; // otherwise "hides"
  public
    function Load(Source: TSubstring; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; override;
  {$IFDEF LiteStrings}
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload;
  {$ENDIF}

    procedure Save(Dest: TWritableStream; LineBreak: LegacyChar = #10{;
      WriteBOM: Boolean = False}); overload;
    procedure Save(FileName: PCoreChar; LineBreak: LegacyChar = #10{;
      WriteBOM: Boolean = False}); overload;

    property Items: PLegacyStringArray read FItems;
    property OwnsStrings: Boolean read FOwnsStrings;
  end;

  TStringListItem = TListItem;

  TStringList = class(TList)
  public
    function Load(Source: TSubstring; SourceOptions: TStringOptions = []): TTextSplit; overload; virtual; abstract;
  {$IFNDEF LiteStrings}
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = []): TTextSplit; overload; virtual;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = []): TTextSplit; overload;
  {$ENDIF}
    function TextLength: Integer;
  end;

  TLegacyStringList = class;

  TLegacyStringListItem = class(TStringListItem)
  private
  { hold } FOwner: TLegacyStringList;
  { hold } FPrior, FNext: TLegacyStringListItem;
    FValue: TLegacyString;
  public
    destructor Destroy; override;

    property Next: TLegacyStringListItem read FNext;
    property Owner: TLegacyStringList read FOwner;
    property Prior: TLegacyStringListItem read FPrior;
    property Value: TLegacyString read FValue;
  end;

  TLegacyStringList = class(TStringList)
  private
  { hold } FFirst, FLast: TLegacyStringListItem;
    procedure Append(Source: PLegacyChar; Count: Integer; const Options); overload;
  public
    function Load(Source: TSubstring; SourceOptions: TStringOptions = []): TTextSplit; override;
  {$IFDEF LiteStrings}
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = []): TTextSplit; overload;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = []): TTextSplit; overload;
  {$ENDIF}
    procedure Save(Dest: TWritableStream; LineBreak: LegacyChar = #10{;
      WriteBOM: Boolean = False}); overload;
    procedure Save(FileName: PCoreChar; LineBreak: LegacyChar = #10{;
      WriteBOM: Boolean = False}); overload;

    property First: TLegacyStringListItem read FFirst;
    property Last: TLegacyStringListItem read FLast;
  end;

{ Legacy Windows service }

const
  CSTR_LESS_THAN    = 1;
  CSTR_EQUAL        = 2;
  CSTR_GREATER_THAN = 3;

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

{ Platform code page }

type
  // mapping from 0 for EBCDIC compliance
  TSingleByteMap = array[LegacyChar] of WideChar;

  TPlatformCodePage = class(TCodePage)
  private
    FSingleByteMap: TSingleByteMap;
    FWideMapLo, FWideMapHi: WideChar;
    function GetWideMapCount: Word;
  protected
    property SingleByteMap: TSingleByteMap read FSingleByteMap;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    property WideMapCount: Word read GetWideMapCount;
    property WideMapLo: WideChar read FWideMapLo;
    property WideMapHi: WideChar read FWideMapHi;
  end;

  TSingleByteCodePage = class(TPlatformCodePage)
  private
    FWideMap: PLegacyChar;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;
    function IsEBCDIC: Boolean;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Integer; override;
  {$ENDIF}

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

  TDoubleByteCodePage = class(TPlatformCodePage)
  private
    FDoubleByteMap: TDoubleByteMap;
    FWideMap: PWideMap;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Integer; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
      SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Integer; override;
  {$ENDIF}

    property SingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideMap: PWideMap read FWideMap;
  end;

  TNewCodePageFunc = function(CodePage: Word = CP_ACP): TCodePage;

  TCodePages = class;

  TCodePagesItem = class(TRedBlackTreeItem)
  private
  { hold } FOwner: TCodePages;
  { hold } FLeft, FRight, FParent: TCodePagesItem;
  { hold } FRed: Boolean;
    FCodePage: TCodePage;
  public
    destructor Destroy; override;
    function Compare(Item: TBalancedTreeItem): Integer; override;

    property CodePage: TCodePage read FCodePage;
    property Left: TCodePagesItem read FLeft;
    property Parent: TCodePagesItem read FParent;
    property Owner: TCodePages read FOwner;
    property Red: Boolean read FRed;
    property Right: TCodePagesItem read FRight;
  end;

  TCodePages = class(TRedBlackTree)
  private
  { hold } FRoot: TCodePagesItem;
    FNewCodePageFunc: TNewCodePageFunc;
    function GetItem(CodePage: Word): TCodePage;
  public
    constructor Create(NewCodePageFunc: TNewCodePageFunc);

    property Items[CodePage: Word]: TCodePage read GetItem; default;
    property Root: TCodePagesItem read FRoot;
  end;

{ Exceptions }

  ECodePage = class(Exception)
  private
    FName: PCoreChar;
    FMaxCharBytes: Byte;
  public
    constructor Create(var Info: TCPInfoEx); overload;
    constructor Create(var Info: TCPInfoEx; CodePage: TObject); overload;

    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
  end;

  TCharSet = (csUTF8, csLatin1, csUTF16, csUTF32); // UTF-8 by default

  TConvertSiteType = (stCharSet, stCodePage);
  TConvertSite = record
    case TConvertSiteType of
      stCharSet: (CharSet: TCharSet);
      stCodePage: (CodePage: TCodePage);
  end;

  EString = class(Exception)
  end;

  EChar = class(EString)
  private
    FInvalidChar: QuadChar;
  {$IFNDEF Lite}
    FBlock: TCharBlock;
  {$ENDIF}
  public
    property InvalidChar: QuadChar read FInvalidChar;
  {$IFNDEF Lite}
    property Block: TCharBlock read FBlock;
  {$ENDIF}
  end;

  EConvert = class(EChar)
  private
    FSourceSite, FDestSite: TConvertSite;
  {$IFNDEF Lite}
    function CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCharSet): Boolean; overload;
    function CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCodePage): Boolean; overload;
  {$ENDIF}
  public
    constructor Create(InvalidChar: QuadChar; SourceSite, DestSite: TCharSet); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCodePage; DestSite: TCharSet); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCharSet; DestSite: TCodePage); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCodePage; DestSite: TCodePage); overload;

    property DestSite: TConvertSite read FDestSite;
    property SourceSite: TConvertSite read FSourceSite;
  end;

  EUTF = class(EString)
  private
    FInfo: TInvalidUTF;
  public
    constructor Create(const Info: TInvalidUTF);
    property Info: TInvalidUTF read FInfo;
  end;

const
  LeadBytes = [Low(TLeadByte)..High(TLeadByte)];

{ Core services }

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy = []): Integer;
function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16 = []): Integer;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;

function PlatformCodePage(CodePage: Word = CP_ACP): TPlatformCodePage;

type
  TFromUTF8 = record
    Count, SuccessBytes: Integer;
    NextChar: PLegacyChar;
  end;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []; ThresholdBytes: Integer = MaxInt): TFromUTF8;
function IsUTF8(Source: PLegacyChar; SourceCount: Integer; DestOptions: TEncodeUTF16 = [];
  ThresholdBytes: Integer = 4): Boolean;

type
  TLegacyTextEvent = procedure(Source: PLegacyChar; Count: Integer; const Options) of object;
  TWideTextEvent = procedure(Source: PWideChar; Count: Integer; const Options) of object;
  TQuadTextEvent = procedure(Source: PQuadChar; Count: Integer; const Options) of object;

function SplitText(Source: PLegacyChar; Count: Integer; DestEvent: TLegacyTextEvent;
  const DestOptions): TTextSplit; overload;
function SplitText(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  DestEvent: TWideTextEvent; const DestOptions): TTextSplit; overload;
function SplitText(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  DestEvent: TQuadTextEvent; const DestOptions): TTextSplit; overload;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy): Integer;
begin
  if coCESU8 in Options then
    Result := (Info.CharCount + Info.SurrogatePairCount) * 3
  else
    Result := Info.CharCount * 3 + Info.SurrogatePairCount;
end;

function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16): Integer;
begin
  Result := Info.CharCount;
  if coSurrogates in Options then
    Inc(Result, Info.SurrogatePairCount);
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

type
  TCodePageName = record
    Len: Integer;
    Str: PCoreChar;
  end;

function ExtractCodePageName(var Info: TCPInfoEx): TCodePageName;
var
  I: Integer;
  P: PCoreChar;
begin
  with Info do
  begin
    for I := Low(CodePageName) to High(CodePageName) do
      case CodePageName[I] of
        CoreChar(0)..CoreChar(32), CoreChar('0')..CoreChar('9'):
          ; 
        CoreChar('('):
          begin
            with Result do
            begin
              Str := @CodePageName[I];
              Len := WideStrLen(Str, Length(CodePageName) - I);
              P := Str + Len - 1;
              if P^ = CoreChar(')') then
              begin
                Inc(Str);
                Dec(Len);
                P^ := CoreChar(0);
              end;
            end;
            Exit;
          end;
      else
        Break;
      end;
    with Result do
    begin
      Str := @CodePageName[I];
      Len := WideStrLen(Str, Length(CodePageName) - I);
    end;
  end;
end;

function PlatformCodePage(CodePage: Word): TPlatformCodePage;
var
  Info: TCPInfoEx;
begin
  if not GetCPInfoEx(CodePage, 0, Info) then
    RaiseLastPlatformError;

  case Info.MaxCharSize of
    1:
      begin
        Result := TSingleByteCodePage.Create(Info);
        Exit;
      end;
{    2:
      begin
        Result := TDoubleByteCodePage.Create(Info);
        Exit;
      end;
  else
    if Info.CodePage = 54936 then // TODO: GB18030
    begin
      if not GetCPInfoEx(936, 0, Info) then
        RaiseLastPlatformError;

      Result := TDoubleByteCodePage.Create(Info);
      Exit;
    end;}
  end;

  with Info do
    raise ECodePage.Create(Info);
end;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PWideChar; DestOptions: TEncodeUTF16; ThresholdBytes: Integer): TFromUTF8;

var
  Limit: PLegacyChar;

function GetChar: QuadChar; // Fast core: closure
var
  FirstByte, Bytes, B, C: Byte;
begin
  FirstByte := Byte(Source^);
  Inc(Source);

  if FirstByte and $80 <> 0 then
  begin
    if FirstByte and $40 <> 0 then
    begin
      if FirstByte and $20 = 0 then
      begin
        Result := FirstByte and $1F;
        Bytes := 1;
      end
      else if FirstByte and $10 = 0 then
      begin
        Result := FirstByte and $0F;
        Bytes := 2;
      end
      else if FirstByte and $08 = 0 then
      begin
        Result := FirstByte and $07;
        Bytes := 3;
      end
      else
      begin
        Result := 0;
        Bytes := 0;
      end;

      B := Bytes;

      if Result <> 0 then
        while (B <> 0) and (Source < Limit) do
        begin
          C := Byte(Source^);
          if C and $C0 = $80 then
          begin
            Result := (Result shl 6) or (C and $3F);
            Inc(Source);
            Dec(B);
          end
          else
            Break; // broken sequence
        end;

      if (B <> 0) or (Result = 0) then // broken sequence or unexpected end of string
        if (Source = Limit) and (coContinuous in DestOptions) then
        begin
          Dec(Source, Bytes - B + 1);
          Result := InvalidUTFMask;
        end
        else
          Result := (Bytes + 1) or (Byte(Bytes - B + 2) shl 8) or InvalidUTFMask; // Fast core

      Exit;
    end;

    Result := FirstByte or InvalidUTFMask; // Fast core
  end
  else
    Result := QuadChar(FirstByte);
end;

var
  Q, T: QuadChar;
  W: Word;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  with Result do
  begin
    Count := 0;
    SuccessBytes := 0;
  end;
  Limit := Source + Count;
  T := 0;
{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}

  while (Source < Limit) and (Result.SuccessBytes < ThresholdBytes) do
  begin
    if T <> 0 then
      Q := T
    else
      Q := GetChar;

    if Q and InvalidUTFMask = 0 then
      case Q of
        Low(THighSurrogates)..High(THighSurrogates):
          if coSurrogates in DestOptions then
          begin
            T := GetChar;
            if T and InvalidUTFMask = 0 then
              case T of
                Low(TLowSurrogates)..High(TLowSurrogates): // CESU-8
                  begin
                    if coBigEndian in DestOptions then // Fast core
                      PLongWord(Dest)^ := Swap(Q) or (Swap(T) shl 16)
                    else
                      PLongWord(Dest)^ := Q or (T shl 16);

                    Inc(Dest, 2);

                    with Result do
                    begin
                      Inc(Count, 2);
                      Inc(SuccessBytes, 6); // two 3-byte sequences
                    end;

                    with Info do
                    begin
                      Inc(CharCount);
                      Inc(SequenceCount);
                      Inc(SurrogatePairCount);
                    {$IFNDEF Lite}
                      if coRangeBlocks in DestOptions then
                      begin
                        Block := FindCharBlock(
                          (Q - Low(THighSurrogates)) shl 10 + Low(TUnicodeSMP) +
                          T - Low(TLowSurrogates), Block);
                        Include(Blocks, Block);
                      end;
                    {$ENDIF}
                    end;

                    T := 0; // surrogate pair flushed
                    Continue;
                  end;
              else
                Q := Q or (Byte(ivUTF8) shl 16) or InvalidUTFMask; // Fast core
              end
            else if T and not InvalidUTFMask = 0 then // coContinuous
              Break;
          end;

        Low(TLowSurrogates)..High(TLowSurrogates):
          Q := Q or (Byte(ivUTF8) shl 16) or InvalidUTFMask; // Fast core

        Low(TUnicodeSMP)..High(TUnicodePUA):
          if coSurrogates in DestOptions then
          begin
            W := Q - Low(TUnicodeSMP);

            if coBigEndian in DestOptions then // Fast core
              PLongWord(Dest)^ :=
                Word(Swap(Low(THighSurrogates) + W shr 10)) or
                Word(Swap((Low(TLowSurrogates) + W and $3FF) shl 16))
            else
              PLongWord(Dest)^ :=
                Word(Low(THighSurrogates) + W shr 10) or
                Word((Low(TLowSurrogates) + W and $3FF) shl 16);

            Inc(Dest, 2);
            with Result do
            begin
              Inc(Count, 2);
              Dec(SuccessBytes, 4); // one 4-byte sequence
            end;

            with Info do
            begin
              Inc(CharCount);
              Inc(SequenceCount);
              Inc(SurrogatePairCount);
            end;

          {$IFNDEF Lite}
            if coRangeBlocks in DestOptions then
            begin
              Block := FindCharBlock(Q, Block);
              Include(Info.Blocks, Block);
            end;
          {$ENDIF}

            Continue;
          end;
      else
      {$IFNDEF Lite}
        if coRangeBlocks in DestOptions then
        begin
          Block := FindCharBlock(Q, Block);
          Include(Info.Blocks, Block);
        end;
      {$ENDIF}
      end
    else if Q and not InvalidUTFMask = 0 then // coContinuous
      Break;

    if Q > High(TUnicodeBMP) then // both InvalidUTFMask and higher Unicode
      if coForceInvalid in DestOptions then
      begin
        Q := QuadChar(Unknown_UTF16);
        Inc(Info.InvalidCount);
      {$IFNDEF Lite}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbSpecials); // Fast core
      {$ENDIF}
      end
      else
      begin
        with Info do
        begin
          InvalidChar := Q;
          Inc(Count, Result.Count);
        end;
        with Result do
        begin
          Count := 0;
          NextChar := Source;
        end;
        Exit;
      end;

    if coBigEndian in DestOptions then
      Dest^ := WideChar(Swap(Q))
    else
      Dest^ := WideChar(Q);

    Inc(Dest);

    with Result do
    begin
      Inc(Count);
      case Q of
        $80..$7FF:
          Inc(SuccessBytes, 2);
        $800..$FFFF:
          Inc(SuccessBytes, 3);
      end;
    end;

    with Info do
    begin
      Inc(CharCount);
      if Q > $7F then
        Inc(SequenceCount);
    end;
  end;

  Inc(Info.Count, Result.Count);
  Result.NextChar := Source;
{  if not (coContinuous in DestOptions) then
    Info.CodePage := nil;}
end;

function IsUTF8(Source: PLegacyChar; SourceCount: Integer; DestOptions: TEncodeUTF16;
  ThresholdBytes: Integer): Boolean;
var
  Info: TStringInfo;
  Buf: array[0..$3FF] of WideChar;
  Cnt: Integer;
  Opt: TEncodeUTF16;
begin
  FillChar(Info, SizeOf(Info), 0);

  if coSurrogates in DestOptions then
    Cnt := Length(Buf) div 2
  else
    Cnt := Length(Buf);

  Opt := DestOptions + [coContinuous];

  repeat
    if SourceCount < Cnt then
    begin
      Cnt := SourceCount;
      Opt := DestOptions;
    end;

    with FromUTF8(Info, Source, Cnt, Buf, DestOptions, ThresholdBytes) do
    begin
      if (Count = 0) or (Info.InvalidChar <> 0) then
      begin
        Result := False;
        Exit;
      end;
      if SuccessBytes >= ThresholdBytes then
      begin
        Result := True;
        Exit;
      end;
      Source := NextChar;
      Dec(SourceCount, Count);
    end;
  until SourceCount = 0;

  Result := Info.SequenceCount >= ThresholdBytes div 2; // per 2-byte sequences
end;

function SplitText(Source: PLegacyChar; Count: Integer; DestEvent: TLegacyTextEvent;
  const DestOptions): TTextSplit;
var
  P, Limit, Next: PLegacyChar;
  B: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Source <> nil then
  begin
    Limit := Source + Count;
    P := Source;
    while (P < Limit) and not (P^ in [#13, #10]) do
      Inc(P);
    Next := P;

    for B := Low(LineBreakChars) to High(LineBreakChars) do
      if Next^ = LineBreakChars[B] then
      begin
        Result.LineBreak := TLineBreak(Byte(B) + 1);
        Inc(Next);
        if Next^ = LineBreakChars[not B] then
        begin
          Inc(Next);
          Inc(Result.LineBreak, 2); // Fast core
        end;

        repeat
          DestEvent(Source, P - Source, DestOptions);
          Inc(Result.Count);

          P := StrScan(Next, LineBreakChars[B], Limit - Next);
          if P = nil then
          begin
            DestEvent(Next, Limit - Next, DestOptions);
            Inc(Result.Count);
            Exit;
          end;

          Source := Next;
          Next := P + 1;
          if Next^ = LineBreakChars[not B] then
            Inc(Next);
        until Source >= Limit;

        Exit;
      end;

    DestEvent(Source, Count, DestOptions);
    Inc(Result.Count);
  end;
end;

function SplitText(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  DestEvent: TWideTextEvent; const DestOptions): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function SplitText(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  DestEvent: TQuadTextEvent; const DestOptions): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{ ECodePage }

constructor ECodePage.Create(var Info: TCPInfoEx);
begin
  Create(sUnsupportedCodePage, CP_LEGACY, [Info.CodePage, ExtractCodePageName(Info).Str]);
end;

constructor ECodePage.Create(var Info: TCPInfoEx; CodePage: TObject);
const
  HasNo: array[Boolean] of PLegacyChar = (nil, sWhitespaceNo);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, CodePage);
  Create(sInvalidCodePageClass, CP_LEGACY, [Info.CodePage, ExtractCodePageName(Info).Str,
    HasNo[Info.LeadByte[0] = 0], ClassName]);
end;

{ EConvert }

const
  CharSets: array [TCharSet] of PLegacyChar = (sUTF8, sLatin1, sUTF16, sUTF32); // UTF-8 by default

{$IFNDEF Lite}
function EConvert.CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCharSet): Boolean;
begin
  FBlock := FindCharBlock(InvalidChar);
  if FBlock <> cbNonUnicode then
  begin
    Create(sInvalidCharSetChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(InvalidChar), InvalidChar, CharSets[DestSite]]); // TODO: surrogates
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCodePage): Boolean;
begin
  FBlock := FindCharBlock(InvalidChar);
  if FBlock <> cbNonUnicode then
  begin
    Create(sInvalidCodePageChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(InvalidChar), InvalidChar, DestSite.Number, DestSite.Name]);  // TODO: surrogates
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

constructor EConvert.Create(InvalidChar: QuadChar; SourceSite, DestSite: TCharSet);
begin
{$IFDEF Lite}
  Create(sInvalidCharSetToCharSet, [CharSets[SourceSite], CharSets[DestSite]]);
{$ELSE}
  if not CatchInvalidChar(InvalidChar, DestSite) then
    Create(sNonUnicodeCharSet, [CharSets[SourceSite]]);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CharSet := SourceSite;
  FDestSite.CharSet := DestSite;
end;

constructor EConvert.Create(InvalidChar: QuadChar; SourceSite: TCodePage; DestSite: TCharSet);
begin
{$IFDEF Lite}
  Create(sInvalidCodePageToCharSet, CP_LEGACY, [SourceSite.Number, SourceSite.Name, CharSets[DestSite]]);
{$ELSE}
  if not CatchInvalidChar(InvalidChar, DestSite) then
    Create(sNonUnicodeCodePage, [SourceSite.Number, SourceSite.Name]);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CodePage := SourceSite;
  FDestSite.CharSet := DestSite;
end;

constructor EConvert.Create(InvalidChar: QuadChar; SourceSite: TCharSet; DestSite: TCodePage);
begin
{$IFDEF Lite}
  Create(sInvalidCharSetToCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Number, DestSite.Name]);
{$ELSE}
  if not CatchInvalidChar(InvalidChar, DestSite) then
    Create(sNonUnicodeCharSet, [CharSets[SourceSite]]);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CharSet := SourceSite;
  FDestSite.CodePage := DestSite;
end;

constructor EConvert.Create(InvalidChar: QuadChar; SourceSite, DestSite: TCodePage);
begin
{$IFDEF Lite}
  Create(sInvalidCodePageToCodePage, CP_LEGACY, [SourceSite.Number, SourceSite.Name, DestSite.Number, DestSite.Name]);
{$ELSE}
  if not CatchInvalidChar(InvalidChar, DestSite) then
    Create(sNonUnicodeCodePage, [SourceSite.Number, SourceSite.Name]);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CodePage := SourceSite;
  FDestSite.CodePage := DestSite;
end;

{ EUTF }

constructor EUTF.Create(const Info: TInvalidUTF);
const
  Bytes: array[2..5] of PLegacyChar = (sSecond, sThird, sFourth, sFifth);
  UTFs: array[ivUTF8..ivUTF16] of PLegacyChar = (sCESU8, sUTF16);
begin
  if Info.UTF = ivNone then
    if Info.BrokenByte <> 0 then
      inherited Create(sBrokenUTF8, [Info.BrokenSequence, Bytes[Info.BrokenByte]])
    else
      inherited Create(sBadUTF8, [Info.StartingByte])
  else
    case Info.Surrogate of
      WideChar(Low(THighSurrogates))..WideChar(High(THighSurrogates)):
        inherited Create(sBrokenSurrogate, [UTFs[Info.UTF], Info.Surrogate]);
    else
      inherited Create(sBadSurrogate, [UTFs[Info.UTF], Info.Surrogate]);
    end;
  FInfo := Info;
end;

{ TCodePage }

function TCodePage.FromLegacy(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromLegacy(Info, Source, Count, CodePage, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, TCharSet(soLatin1 in SourceOptions), Self);
  Result := Info.Count;
end;

function TCodePage.ToLegacy(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PLegacyChar; CodePage: TCodePage; DestOptions: TEncodeLegacy): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, Self, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function TCodePage.FromUTF16(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF16, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, Self, csUTF16);
  Result := Info.Count;
end;

{$IFDEF UTF32}
function TCodePage.FromUTF32(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF32, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else
      raise EConvert.Create(Info.InvalidChar, Self, csUTF32);
  Result := Info.Count;
end;
{$ENDIF}

{ TPlatformCodePage }

constructor TPlatformCodePage.Create(var Info: TCPInfoEx);
var
  SourceMap: array[Byte] of LegacyChar;
  B, L: Byte;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
  C: LegacyChar;
  I: Integer;
  T: LongWord;
  W: WideChar;
begin
  FNumber := Info.CodePage;
  with ExtractCodePageName(Info) do
    FName := WideStrNew(Str, Len);

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

destructor TPlatformCodePage.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

function TPlatformCodePage.GetWideMapCount: Word;
begin
  if FWideMapHi <> WideChar(0) then
    Result := Word(FWideMapHi) - Word(FWideMapLo) + 1
  else
    Result := 0;
end;

{ TSubstring }

function TSubstring.ByteCount: Integer;
begin
  Result := ByteCount(FCount);
end;

{ TString }

procedure TString.AcceptRange(Index, Count: Integer);
var
  NewCount: Integer;
begin
  CheckIndex(Index);
  NewCount := Index + Count;
  if NewCount > FCount then
    SetCount(NewCount);
end;

procedure TString.CheckIndex(Index: Integer);
begin
  if (TLegacyString(Self).FData = nil) and (Index <> 0) then
    raise EIndex.Create(Self, Index)
  else
    inherited;
end;

procedure TString.Clear;
begin
  with TLegacyString(Self) do
  begin
    if not (soAttachBuffer in FOptions) then
      FreeMem(FData);
    FData := nil;
  end;
  FCount := 0;
end;

{$IFDEF LiteStrings}
procedure TString.Insert(Source: Pointer; Count: Integer;
  SourceOptions: TStringOptions; DestIndex: Integer);
{$ENDIF}
{$IFDEF CTRL_SHIFT_UP}
begin
end;
{$ENDIF}

{$IFNDEF LiteStrings}
procedure TString.DoInsert(Source: Pointer; Count: Integer;
  SourceOptions: TStringOptions; DestIndex: Integer);
{$ENDIF}
var
  Cnt: Integer;
begin
  if (soAttachBuffer in SourceOptions) and (DestIndex = 0) then
  begin
    if not (soAttachBuffer in TLegacyString(Self).FOptions) then
      FreeMem(TLegacyString(Self).FData);
    TLegacyString(Self).FData := Source;
    FCount := Count;
    TLegacyString(Self).FOptions := SourceOptions;
  end
  else
  begin
    Cnt := DestIndex + Count;
    if Cnt > FCount then
      SetCount(Cnt);
    Move(Source^, TLegacyString(Self).FData[ByteCount(DestIndex)], ByteCount(Count));
    TLegacyString(Self).FOptions := SourceOptions - [soAttachBuffer];
  end;
end;

{$IFNDEF LiteStrings}
function TString.Insert(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  if (TLegacyString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(LegacyChar)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    TLegacyString(Self).FCodePage := CodePage;
    Result := Count;
  end
  else
  begin
    FillChar(Info, SizeOf(Info), 0);
    Result := Insert(Info, Source, Count, CodePage, SourceOptions, DestIndex, DestOptions
      {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF} );
    if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
      if Info.InvalidChar and InvalidUTFMask <> 0 then
        raise EUTF.Create(Info.InvalidUTF)
      else if CodePage <> nil then
        if Info.CodePage <> nil then
          raise EConvert.Create(Info.InvalidChar, CodePage, Info.CodePage)
        else
          raise EConvert.Create(Info.InvalidChar, CodePage, TCharSet(coLatin1 in DestOptions))
      else
        if Info.CodePage <> nil then
          raise EConvert.Create(Info.InvalidChar, TCharSet(soLatin1 in SourceOptions), Info.CodePage)
        else
          raise EConvert.Create(Info.InvalidChar, TCharSet(soLatin1 in SourceOptions), TCharSet(coLatin1 in DestOptions));
  end;
end;

function TString.Insert(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  if (TWideString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(WideChar)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    Result := Count;
  end
  else
  begin
    FillChar(Info, SizeOf(Info), 0);
    Result := Insert(Info, Source, Count, SourceOptions, DestIndex, DestOptions
      {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF} );
    if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
      if Info.InvalidChar and InvalidUTFMask <> 0 then
        raise EUTF.Create(Info.InvalidUTF)
      else if Info.CodePage <> nil then
        raise EConvert.Create(Info.InvalidChar, csUTF16, Info.CodePage)
      else
        raise EConvert.Create(Info.InvalidChar, csUTF16, TCharSet(coLatin1 in DestOptions));
  end;
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Insert(
  Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  if (TQuadString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(QuadChar)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    Result := Count;
  end
  else
  begin
    FillChar(Info, SizeOf(Info), 0);
    Result := Insert(Info, Source, Count, SourceOptions, DestIndex, DestOptions
      {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF} );
    if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
      if Info.InvalidChar and InvalidUTFMask <> 0 then
        raise EUTF.Create(Info.InvalidUTF)
      else if Info.CodePage <> nil then
        raise EConvert.Create(Info.InvalidChar, csUTF32, Info.CodePage)
      else
        raise EConvert.Create(Info.InvalidChar, csUTF32, TCharSet(coLatin1 in DestOptions));
  end;
end;

{$ELSE LiteStrings}

function TString.Load(Source: TReadableStream; SourceOptions: TStringOptions; DestIndex: Integer): Integer;
var
  L: Integer;
begin
  with Source do
    L := Size - Position;
  Result := L div ByteCount(1);
  AcceptRange(DestIndex, Result);
  Source.ReadBuffer(TLegacyString(Self).FData[ByteCount(DestIndex)], L);
end;
{$ENDIF}

function TString.Load(FileName: PCoreChar; SourceOptions: TStringOptions; DestIndex: Integer): Integer;
var
  F: TReadableStream;
begin
  F := TFileStream.Create(FileName, faSequentialRead);
  try
    Result := Load(F, SourceOptions, DestIndex);
  finally
    F.Free;
  end;
end;

procedure TString.SetCount(Value: Integer);
var
  Buf: PLegacyChar;
  Cnt: Integer;
begin
  if soAttachBuffer in TLegacyString(Self).FOptions then
  begin
    if Value < FCount then
      Cnt := Value
    else
      Cnt := FCount;
    GetMem(Buf, ByteCount(Value + 1));
    Move(TLegacyString(Self).FData^, Buf^, ByteCount(Cnt));
    TLegacyString(Self).FData := Buf;
    Exclude(TLegacyString(Self).FOptions, soAttachBuffer);
  end
  else
    ReallocMem(TLegacyString(Self).FData, ByteCount(Value + 1));

//  FillChar(TLegacyString(Self).FData[Value], ByteCount(1), 0);
  FCount := Value;
end;


{ TLegacyString }

{class} function TLegacyString.ByteCount(Count: Integer): Integer;
begin
  Result := Count;
end;

{class} function TLegacyString.Length(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;

{$IFNDEF Lite}
{class} function TLegacyString.Length(Source: Pointer; MaxLength: Integer): Integer;
begin
  Result := StrLen(Source, MaxLength);
end;

{procedure TLegacyString.Format(const Args: array of const);
begin
  SetCount(StrLen(Source) + EstimateArgs(Args));
  SetCount(FormatBuf(Source, Args, FData));
end;}
{$ENDIF}

procedure TLegacyString.Format(Source: PLegacyChar; const Args: array of const);
begin
  SetCount(StrLen(Source) + EstimateArgs(Args));
  SetCount(FormatBuf(Source, Args, FData));
end;

{$IFNDEF LiteStrings}
function TLegacyString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
{  if FData <> nil then
  begin
    CheckIndex(DestIndex);
  end
  else
  begin
    if DestIndex <> 0 then
      raise EIndex.Create(Self, DestIndex);


  end;


   and (soDetectCharSet in FOptions) then
  begin
    if IsUTF8(FData, FCount, DestOptions) then
    begin
      FCodePage := nil;
      Exclude(FOptions, soLatin1);
    end;
    Exclude(FOptions, soDetectCharSet);
  end;

  if (Source <> nil) and (soDetectCharSet in SourceOptions) then
  begin
    if IsUTF8(Source, Count, DestOptions) then
    begin
      CodePage := nil;
      Exclude(SourceOptions, soLatin1);
    end;
  end;}
end;

function TLegacyString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TLegacyString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF UTF32}

function TLegacyString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := CoreStrings.IsUTF8(FData, FCount, [], ThresholdBytes);
end;

function TLegacyString.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  DestIndex: Integer): Integer;
var
  S: TSharedString;
begin
  S := TSharedString.Create;
  try
    Result := S.Load(Source, SourceOptions);
    if (FData <> nil) or (S.ByteCount(1) <> SizeOf(LegacyChar)) then
      Result := Insert(S.FData, S.Count, FCodePage, SourceOptions, DestIndex)
    else
    begin
      FData := S.FData;
      FCount := S.FCount;
      FOptions := SourceOptions - [soAttachBuffer];
      TLegacyString(S.FParent).FData := nil;
      Result := FCount;
    end;
  finally
    S.Free;
  end;
end;
{$ENDIF LiteStrings}

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Insert(Value, StrLen(Value) {$IFNDEF LiteStrings}, FCodePage {$ENDIF});
end;

{ TWideString }

{class} function TWideString.ByteCount(Count: Integer): Integer;
begin
  Result := Count * SizeOf(WideChar);
end;

{class} function TWideString.Length(Source: Pointer): Integer;
begin
  Result := WideStrLen(Source);
end;

{$IFNDEF Lite}
{class} function TWideString.Length(Source: Pointer; MaxLength: Integer): Integer;
begin
  Result := WideStrLen(Source, MaxLength);
end;

procedure TWideString.SwapByteOrder;
begin
  SwapWideCharBytes(FData, FCount, FData);
  Byte(FOptions) := Byte(FOptions) xor Byte(soBigEndian);
end;
{$ENDIF}

procedure TWideString.Format(Source: PWideChar; const Args: array of const);
begin
  SetCount(WideStrLen(Source) + EstimateArgs(Args));
  SetCount(WideFormatBuf(Source, Args, FData));
end;

{$IFNDEF LiteStrings}
function TWideString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TWideString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TWideString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF UTF32}

function TWideString.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  DestIndex: Integer): Integer;
var
  S: TSharedString;
begin
  S := TSharedString.Create;
  try
    Result := S.Load(Source, SourceOptions);
    if (FData <> nil) or (S.ByteCount(1) <> SizeOf(WideChar)) then
      Result := Insert(PWideChar(S.FData), S.Count, SourceOptions, DestIndex)
    else
    begin
      FData := S.FData;
      FCount := S.FCount;
      FOptions := SourceOptions - [soAttachBuffer];
      TWideString(S.FParent).FData := nil;
      Result := FCount;
    end;
  finally
    S.Free;
  end;
end;
{$ENDIF LiteStrings}

procedure TWideString.SetData(Value: PWideChar);
begin
  Insert(Value, WideStrLen(Value));
end;

{ TQuadString }

{class} function TQuadString.ByteCount(Count: Integer): Integer;
begin
  Result := Count * SizeOf(QuadChar);
end;

{class} function TQuadString.Length(Source: Pointer): Integer;
begin
  Result := QuadStrLen(Source);
end;

{$IFNDEF Lite}
{class} function TQuadString.Length(Source: Pointer; MaxLength: Integer): Integer;
begin
  Result := QuadStrLen(Source, MaxLength);
end;

procedure TQuadString.SwapByteOrder;
begin
  SwapQuadCharBytes(FData, FCount, FData);
  Byte(FOptions) := Byte(FOptions) xor Byte(soBigEndian);
end;

{procedure TQuadString.Format(const Args: array of const);
begin
  raise EAbstract.Create(sNotImplemented);
end;}
{$ENDIF}

{$IFNDEF LiteStrings}
function TQuadString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFNDEF UTF32}
function TQuadString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TQuadString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF UTF32}

function TQuadString.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  DestIndex: Integer): Integer;
var
  S: TSharedString;
begin
  S := TSharedString.Create;
  try
    Result := S.Load(Source, SourceOptions);
    if (FData <> nil) or (S.ByteCount(1) <> SizeOf(QuadChar)) then
      Result := Insert(PQuadChar(S.FData), S.Count, SourceOptions, DestIndex)
    else
    begin
      FData := S.FData;
      FCount := S.FCount;
      FOptions := SourceOptions - [soAttachBuffer];
      TQuadString(S.FParent).FData := nil;
      Result := FCount;
    end;
  finally
    S.Free;
  end;
end;
{$ENDIF LiteStrings}

procedure TQuadString.SetData(Value: PQuadChar);
begin
  Insert(Value, QuadStrLen(Value));
end;

 { TSharedString }

{class} function TSharedString.ByteCount(Count: Integer): Integer;
begin
  if FParent <> nil then
    Result := FParent.ByteCount(Count)
  else
    Result := 0;
end;

{class} function TSharedString.Length(Source: Pointer): Integer;
begin
  if FParent <> nil then
    Result := FParent.Length(Source)
  else
    Result := 0;
end;

{$IFNDEF Lite}
{class} function TSharedString.Length(Source: Pointer; MaxLength: Integer): Integer;
begin
  if FParent <> nil then
    Result := FParent.Length(Source, MaxLength)
  else
    Result := 0;
end;
{$ENDIF}

procedure TSharedString.Clear;
begin
//  ReleaseAndNil(FParent); // TODO
  FData := nil;
  FCount := 0;
end;

function TSharedString.IsUnique: Boolean;
begin
  Result := (FParent = nil)
end;

function TSharedString.GetAsLegacyChar: PLegacyChar;
begin
  Result := FData; // TODO
end;

function TSharedString.GetAsWideChar: PWideChar;
begin
  Result := FData; // TODO
end;

procedure TSharedString.SetAsLegacyChar(Value: PLegacyChar);
begin
  // TODO
end;

procedure TSharedString.SetAsWideChar(Value: PWideChar);
begin
  // TODO
end;

{$IFDEF UTF32}
function TSharedString.GetAsQuadChar: PQuadChar;
begin
  Result := FData; // TODO
end;

procedure TSharedString.SetAsQuadChar(Value: PQuadChar);
begin
  // TODO
end;
{$ENDIF}

function TSharedString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TSharedString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TSharedString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF}

{$IFNDEF LiteStrings}
function TSharedString.Load(Source: TReadableStream; SourceOptions: TStringOptions; DestIndex: Integer): Integer;

procedure LoadWideString;
var
  ByteLen: Integer;
begin
  FParent := TWideString.Create;
  ByteLen := Source.Size - SizeOf(WideChar); // BOM
  FParent.SetCount(ByteLen div SizeOf(WideChar));
  Source.ReadBuffer(TLegacyString(FParent).Data^, ByteLen);
  FData := TLegacyString(FParent).Data;
  FCount := FParent.Count;
end;

var
  BOM: WideChar;
begin
  if DestIndex = 0 then
  begin
    Clear;
    Source.ReadBuffer(BOM, SizeOf(BOM));
    case BOM of
      BOM_UTF16_LE:
        LoadWideString;
      BOM_UTF16_BE:
        begin
          LoadWideString;

        end;

    end;
  end
  else
    Result := 0; 
end;
{$ENDIF}

{ TStrings }

function TStrings.Load(Source: TSubstring; SourceOptions: TStringOptions;
  AverageStringLength: Integer): TTextSplit;
var
  AvgCapacity: Integer;
begin
  if Source <> nil then
  begin
    AvgCapacity := Source.Count div AverageStringLength;
    if Count <> 0 then
      Capacity := Capacity + AvgCapacity
    else if AvgCapacity > Capacity then
      Capacity := AvgCapacity;
  end;
{$IFNDEF Lite}
  FillChar(Result, SizeOf(Result), 0);
{$ENDIF}
end;

{$IFNDEF LiteStrings}
function TStrings.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  AverageStringLength: Integer): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO: Load via TSharedString
end;
{$ENDIF LiteStrings}

function {$IFDEF LiteStrings} TLegacyStrings {$ELSE} TStrings {$ENDIF} .Load
  (FileName: PCoreChar; SourceOptions: TStringOptions; AverageStringLength: Integer): TTextSplit;
var
  F: TReadableStream;
begin
  F := TFileStream.Create(FileName, faSequentialRead);
  try
    Result := Load(F, SourceOptions);
  finally
    F.Free;
  end;
end;

function TStrings.TextLength: Integer;
var
  I: Integer;
  S: TLegacyString;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    S := TLegacyStrings(Self).FItems[I];
    if S <> nil then
      Inc(Result, S.Count);
  end;
end;

{ TLegacyStrings }

type
  TLegacyStringAppend = record
    CodePage: TCodePage;
    SourceOptions: TStringOptions;
  end;

procedure TLegacyStrings.Append(Source: PLegacyChar; Count: Integer; const Options);
var
  S: TLegacyString;
begin
  S := TLegacyString.Create;
  with TLegacyStringAppend(Options) do
    S.Insert(Source, Count, {$IFNDEF LiteStrings} CodePage, {$ENDIF} SourceOptions);
  Append(S);
end;

function TLegacyStrings.Load(Source: TSubstring; SourceOptions: TStringOptions;
  AverageStringLength: Integer): TTextSplit;
var
  Opt: TLegacyStringAppend;
begin
  inherited Load(Source, SourceOptions, AverageStringLength);
  with TLegacySubstring(Source) do
  begin
  {$IFNDEF LiteStrings}
    Opt.CodePage := CodePage;
  {$ENDIF}
    Opt.SourceOptions := SourceOptions;
    Result := SplitText(Data, Count, Append, Opt)
  end;
end;

{$IFDEF LiteStrings}
function TLegacyStrings.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  AverageStringLength: Integer): TTextSplit;
var
  S: TLegacyString;
begin
  S := TLegacyString.Create;
  try
    S.Load(Source, SourceOptions);
    Result := Load(S, SourceOptions - [soAttachBuffer], AverageStringLength);
  finally
    S.Free;
  end;
end;
{$ENDIF}

procedure TLegacyStrings.Save(Dest: TWritableStream; LineBreak: LegacyChar{; WriteBOM: Boolean});
var
  I: Integer;
  S: TLegacyString;
begin
//  if WriteBOM and
  Dest.Size := TextLength + FCount * SizeOf(LineBreak); // prevent file fragmentation
  for I := 0 to FCount - 1 do
  begin
    S := FItems[I];
    if S <> nil then
      Dest.WriteBuffer(S.Data^, S.Count);
    Dest.WriteBuffer(LineBreak, SizeOf(LineBreak));
  end;
end;

procedure TLegacyStrings.Save(FileName: PCoreChar; LineBreak: LegacyChar{; WriteBOM: Boolean});
var
  F: TWritableStream;
begin
  F := TFileStream.Create(FileName, faSequentialRewrite);
  try
    Save(F, LineBreak{, WriteBOM});
  finally
    F.Free;
  end;
end;

{ TStringList }

{$IFNDEF LiteStrings}
function TStringList.Load(Source: TReadableStream; SourceOptions: TStringOptions): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO: Load via TSharedString
end;
{$ENDIF LiteStrings}

function {$IFDEF LiteStrings} TLegacyStringList {$ELSE} TStringList {$ENDIF} .Load
  (FileName: PCoreChar; SourceOptions: TStringOptions): TTextSplit;
var
  F: TReadableStream;
begin
  F := TFileStream.Create(FileName, faSequentialRead);
  try
    Result := Load(F, SourceOptions);
  finally
    F.Free;
  end;
end;

function TStringList.TextLength: Integer;
var
  Item: TLegacyStringListItem;
begin
  Result := 0;
  Item := TLegacyStringList(Self).FFirst;
  while Item <> nil do
  begin
    Inc(Result);
    Item := Item.FNext;
  end;
end;

{ TLegacyStringListItem }

destructor TLegacyStringListItem.Destroy;
begin
  FValue.Free;
  inherited;
end;

{ TLegacyStringList }

procedure TLegacyStringList.Append(Source: PLegacyChar; Count: Integer; const Options);
begin
  Append(TLegacyStringListItem.Create);
  with TLegacyStringAppend(Options) do
  begin
    FLast.FValue := TLegacyString.Create;
    FLast.FValue.Insert(Source, Count, {$IFNDEF LiteStrings} nil, {$ENDIF} SourceOptions);
  end;
end;

function TLegacyStringList.Load(Source: TSubstring; SourceOptions: TStringOptions): TTextSplit;
var
  Opt: TLegacyStringAppend;
begin
  with TLegacySubstring(Source) do
  begin
  {$IFNDEF LiteStrings}
    Opt.CodePage := CodePage;
  {$ENDIF}
    Opt.SourceOptions := SourceOptions;
    Result := SplitText(Data, Count, Append, Opt)
  end;
end;

{$IFDEF LiteStrings}
function TLegacyStringList.Load(Source: TReadableStream; SourceOptions: TStringOptions): TTextSplit;
var
  S: TLegacyString;
begin
  S := TLegacyString.Create;
  try
    S.Load(Source, SourceOptions);
    Result := Load(S, SourceOptions - [soAttachBuffer]);
  finally
    S.Free;
  end;
end;
{$ENDIF}

procedure TLegacyStringList.Save(Dest: TWritableStream; LineBreak: LegacyChar{;
  WriteBOM: Boolean});
var
  Item: TLegacyStringListItem;
begin
  //if WriteBOM // TODO
  Dest.Size := TextLength + FCount * SizeOf(LineBreak); // prevent file fragmentation
  Item := FFirst;
  while Item <> nil do
  begin
    if Item.FValue <> nil then
      with Item.FValue do
        Dest.WriteBuffer(Data^, Count);
    Dest.WriteBuffer(LineBreak, SizeOf(LineBreak));
    Item := Item.Next;
  end;
end;

procedure TLegacyStringList.Save(FileName: PCoreChar; LineBreak: LegacyChar{;
  WriteBOM: Boolean});
var
  F: TWritableStream;
begin
  F := TFileStream.Create(FileName, faSequentialRewrite);
  try
    Save(F, LineBreak{, WriteBOM});
  finally
    F.Free;
  end;
end;

{ TSingleByteCodePage }

constructor TSingleByteCodePage.Create(var Info: TCPInfoEx);
var
  C: LegacyChar;
  W: WideChar;
begin
{$IFNDEF Lite}
  if Info.LeadByte[0] <> 0 then
    raise ECodePage.Create(Info, Self);
{$ENDIF}

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

destructor TSingleByteCodePage.Destroy;
begin
  FreeMem(FWideMap);
  inherited;
end;

class function TSingleByteCodePage.MaxCharBytes: Byte;
begin
  Result := 1;
end;

function TSingleByteCodePage.IsEBCDIC: Boolean;
begin
  Result := FWideMapLo < WideChar($80);
end;

type
  TWideCharBuf = array[0..{20}47] of WideChar;
const
  BufLen = High(TWideCharBuf) - Low(TWideCharBuf) + 1;

function TSingleByteCodePage.FromLegacy(var Info: TStringInfo; Source: PLegacyChar;
  Count: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TSingleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TSingleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  Idx: Integer;
  C: LegacyChar;
  W, L: WideChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}
  Idx := 0;
  Result := 0;

  while Idx < Count do
  begin
    W := Source[Idx];
    Inc(Idx);

    if soBigEndian in SourceOptions then
      W := WideChar(Swap(Word(W)));

    if W = Unknown_UTF16 then
      C := Unknown_Latin
    else if W < FWideMapLo then
      C := LegacyChar(W)
    else if W <= FWideMapHi then
      C := FWideMap[Word(W) - Word(FWideMapLo)]
    else
      C := #0;

    if C = #0 then
    begin
      Q := QuadChar(W);

      case Word(W) of
        Low(THighSurrogates)..High(THighSurrogates):
          begin
            L := Source[Idx];

            if soBigEndian in SourceOptions then
              L := WideChar(Swap(Word(L)));

            case Word(L) of
              Low(TLowSurrogates)..High(TLowSurrogates):
                begin
                  Inc(Idx);
                  Q := (Word(W) - Low(THighSurrogates)) shl 10 + Low(TUnicodeSMP) +
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

  {$IFNDEF Lite}
    if coRangeBlocks in DestOptions then
    begin
      Block := FindCharBlock(QuadChar(W), Block);
      Include(Info.Blocks, Block);
    end;
  {$ENDIF}

    Dest[Result] := C;
    Inc(Result);
  end;

  with Info do
  begin
    Inc(Count, Result);
    Inc(CharCount, Result);
  end;
end;

function TSingleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
  Inf: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  if soDetectCharSet in SourceOptions then
  begin
    Inf := Info;
    Result := FromUTF8(Inf, Source, Count, Dest, DestOptions).Count;
    if Result <> 0 then
    begin
      Info := Inf;
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
      Dest[I] := WideChar(Swap(Word(W)))
    else
      Dest[I] := W;
  end;

  Result := Count;

  with Info do
  begin
    Inc(Count, Result);
    Inc(CharCount, Result);
  end;
end;

{$IFDEF UTF32}
function TSingleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TSingleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF}

{ TDoubleByteCodePage }

constructor TDoubleByteCodePage.Create(var Info: TCPInfoEx);
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
{$IFNDEF Lite}
  if Info.LeadByte[0] = 0 then
    raise ECodePage.Create(Info, Self);
{$ENDIF}

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

destructor TDoubleByteCodePage.Destroy;
var
  L: TLeadByte;
begin
  FreeMem(FWideMap);
  for L := Low(L) to High(L) do
    FreeMem(FDoubleByteMap[L]);
  inherited;
end;

class function TDoubleByteCodePage.MaxCharBytes: Byte;
begin
  Result := SizeOf(DoubleByteChar);
end;

function TDoubleByteCodePage.FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TDoubleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF}

{ TCodePagesItem }

destructor TCodePagesItem.Destroy;
begin
  FCodePage.Free;
  inherited;
end;

function TCodePagesItem.Compare(Item: TBalancedTreeItem): Integer;
begin
  Result := 0;
  if TCodePagesItem(Item).CodePage.Number < FCodePage.Number then
    Dec(Result)
  else if TCodePagesItem(Item).CodePage.Number > FCodePage.Number then
    Inc(Result);
end;

{ TCodePages }

constructor TCodePages.Create(NewCodePageFunc: TNewCodePageFunc);
begin
  FNewCodePageFunc := NewCodePageFunc;
end;

function TCodePages.GetItem(CodePage: Word): TCodePage;
var
  Item: TCodePagesItem;
begin
  CodePage := TranslateCodePage(CodePage);
  Item := Root;
  while Item <> nil do
  begin
    if CodePage < Item.CodePage.Number then
      Item := Item.Left
    else if CodePage > Item.CodePage.Number then
      Item := Item.Right
    else
    begin
      Result := Item.CodePage;
      Exit;
    end;
  end;

  Item := TCodePagesItem.Create;
  Item.FCodePage := FNewCodePageFunc(CodePage);
  Insert(Item);
  Result := Item.CodePage;
end;

end.

