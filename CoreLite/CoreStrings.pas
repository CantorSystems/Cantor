(*
    Lite Core Library (CoreLite)

    Core strings and character sets implementation

    Copyright (c) 2012-2014 Vladislav Javadov (aka Freeman)

    TODO (first is more important):
      * Unicode NFC/NFD/NFKC/NFKD, UTF-7, SCSU, (BOCU is not patent-free)
      * Non-Unicode GB18030, ISO-2022 (also TRON?)

    Conditional defines:
      * Debug -- some debugging features and diagnostic exceptions
      * Lite -- remove support of:
        * TString.Length(Source, MaxLength)
        * TString.Format coming soon...
      * NoCodePages -- eliminate code page support for strings
      * UTF32 -- UTF-32 character set and Unicode character blocks support
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreConsts;

{$I Unicode.inc}

type
{
  soDetectCharSet:
    * try to decode source as UTF-8, continue as code page or Latin1 if code page is null
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
    * U+007F -- for code page (used instead of U+001A to avoid compatibility issues)
    * U+FFFD -- for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coCompatibility,
    coContinuous, coForceInvalid, {$IFDEF UTF32} coRangeBlocks, {$ENDIF}
    coBigEndian, coSurrogates);

const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogates;
  coEncodeZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodeZero];  // UTF-8 compliance

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

type
  TInvalidChar = record // platform
    case Byte of
      0: (Value: QuadChar);
      1: (BrokenSequence,       // Broken %u-byte UTF-8 sequence starting with byte $%02X
          StartingByte: Byte;   // Bad UTF-8 sequence starting with byte $%02X
          UTF8Mask: Word);
  end;

  TCodePage = class;

  TStringInfo = record
    Count, CharCount: Integer;
    CodePage: TCodePage;
  {$IFDEF UTF32}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (Latin1Count: Integer);
      1: (DoubleByteCount: Integer);
      2: (SequenceCount, SurrogatePairCount, InvalidCount: Integer);
  end;

  TNextLegacyChar = record
    SourceCount, DestCount, SuccessBytes: Integer; // SuccessBytes < 0 for Latin1
    InvalidChar: TInvalidChar;
  end;

  TNextEndianChar = record
    SourceCount, DestCount: Integer;
    InvalidChar: TInvalidChar;
  end;

  TNextWideChar = TNextEndianChar;
  TNextQuadChar = TNextEndianChar;

  TCodePage = class
  private
    FNumber: Word;
    FName: PCoreChar;
  {$IFDEF UTF32}
    FBlocks: TUnicodeBlocks;
  {$ENDIF}
  protected
    function Accept(const Info: TStringInfo): Boolean;
  public
    class function MaxCharBytes: Byte; virtual; abstract;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload; virtual; abstract;
    function ToLegacy(Source: PLegacyChar; Count: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; overload; virtual; abstract;
    function FromUTF16(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; overload;

    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; overload;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; overload; virtual; abstract;
    function FromUTF32(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; overload;

    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; overload; virtual; abstract;
    function ToUTF32(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; overload;

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
    function ByteCount(Count: Integer): Integer; overload; virtual; abstract;
    function ByteCount: Integer; overload;
    function Length(Source: Pointer): Integer; overload; virtual; abstract;
  {$IFNDEF Lite}
    function Length(Source: Pointer; MaxLength: Integer): Integer; overload; virtual; abstract;
  {$ENDIF}
    property ChildCount: Integer read FChildCount;
  end;

  TLegacySubstring = class(TSubstring)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TLegacyOptions;
  {$IFNDEF NoCodePages}
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

{$IFDEF NoCodePages}
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

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil; SourceOptions: TLegacySource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; overload; virtual; abstract;
    function Insert(Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil; SourceOptions: TLegacySource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; overload;

    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; overload; virtual; abstract;
    function Insert(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; overload;

  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; overload; virtual; abstract;
    function Insert(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; overload; virtual; 
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
    function ByteCount(Count: Integer): Integer; override;
    function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
  {$ENDIF}
    procedure Format(Source: PLegacyChar; const Args: array of const); overload;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil; SourceOptions: TLegacySource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; override;
  {$ENDIF}

    function IsUTF8(ThresholdBytes: Integer = 4): Boolean;
    function Load(Source: TReadableStream; SourceOptions: TStringOptions = [];
      DestIndex: Integer = 0): Integer; override;

    property CodePage: TCodePage read FCodePage write FCodePage;  
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
    function ByteCount(Count: Integer): Integer; override;
    function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
    procedure SwapByteOrder; override;
  {$ENDIF}
    procedure Format(Source: PWideChar; const Args: array of const); overload;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; override;
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
    function ByteCount(Count: Integer): Integer; override;
    function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    //procedure Format(const Args: array of const); override;
    procedure SwapByteOrder; override;
  {$ENDIF}

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; override;
  {$IFNDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; overload;
    function Insert(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; overload;
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
    function ByteCount(Count: Integer): Integer; override;
    function Length(Source: Pointer): Integer; overload; override;
  {$IFNDEF Lite}
    function Length(Source: Pointer; MaxLength: Integer): Integer; overload; override;
  {$ENDIF}
    procedure Clear; override;
    function IsUnique: Boolean;

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextLegacyChar; override;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextWideChar; override;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): TNextQuadChar; override;
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
{$ENDIF NoCodePages}

  TCoreString = TWideString; // TODO: non-Unicode

  TStrings = class(TObjects)
  public
    function Load(Source: TSubstring; SourceOptions: TStringOptions = [];
      AverageStringLength: Integer = 32): TTextSplit; overload; virtual;
  {$IFNDEF NoCodePages}
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
  {$IFDEF NoCodePages}
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
  {$IFNDEF NoCodePages}
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
  {$IFDEF NoCodePages}
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
  TLegacyCharMap = array[LegacyChar] of WideChar;

  TPlatformCodePage = class(TCodePage)
  private
    FWideCharMapLo, FWideCharMapHi: WideChar;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;
    function WideCharMapCount: Word;

    property WideCharMapLo: WideChar read FWideCharMapLo;
    property WideCharMapHi: WideChar read FWideCharMapHi;
  end;

  TSingleByteCodePage = class(TPlatformCodePage)
  private
    FLegacyCharMap: TLegacyCharMap;
    FWideCharMap: PLegacyChar;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;
    function IsEBCDIC: Boolean;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; override;
  {$ENDIF}

    property LegacyCharMap: TLegacyCharMap read FLegacyCharMap;
    property WideCharMap: PLegacyChar read FWideCharMap;
  end;

  TSingleByteMap = TLegacyCharMap;

  TLeadByte = #$80..#$FF;
  PTrailByteMap = ^TLegacyCharMap;
  TDoubleByteMap = array[TLeadByte] of PTrailByteMap;

  PDoubleByteChar = ^DoubleByteChar;
  DoubleByteChar = packed record
    case Byte of
      0: (SingleByte: LegacyChar);
      1: (LeadByte, TrailByte: LegacyChar);
  end;

  PWideCharMap = ^TWideCharMap;
  TWideCharMap = array[Word] of DoubleByteChar;

  TDoubleByteCodePage = class(TPlatformCodePage)
  private
    FSingleByteMap: TSingleByteMap;
    FDoubleByteMap: TDoubleByteMap;
    FWideCharMap: PWideCharMap;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; override;
  {$ENDIF}

    property SingleByteMap: TSingleByteMap read FSingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideCharMap: PWideCharMap read FWideCharMap;
  end;

  TNewCodePageFunc = function(CodePage: Word = CP_ACP): TCodePage;

  TCodePages = class;

  TCodePagesNode = class(TRedBlackTreeNode)
  private
  { hold } FOwner: TCodePages;
  { hold } FLeft, FRight, FParent: TCodePagesNode;
  { hold } FRed: Boolean;
    FCodePage: TCodePage;
  public
    destructor Destroy; override;
    function Compare(Item: TBalancedTreeNode): Integer; override;

    property CodePage: TCodePage read FCodePage;
    property Left: TCodePagesNode read FLeft;
    property Parent: TCodePagesNode read FParent;
    property Owner: TCodePages read FOwner;
    property Red: Boolean read FRed;
    property Right: TCodePagesNode read FRight;
  end;

  TCodePages = class(TRedBlackTree)
  private
  { hold } FRoot: TCodePagesNode;
    FNewCodePageFunc: TNewCodePageFunc;
    function GetItem(CodePage: Word): TCodePage;
  public
    constructor Create(NewCodePageFunc: TNewCodePageFunc);

    property Items[CodePage: Word]: TCodePage read GetItem; default;
    property Root: TCodePagesNode read FRoot;
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

  EString = class(Exception);

  EConvert = class(EString)
  private
    FInvalidChar: TInvalidChar;
    FSourceSite, FDestSite: TConvertSite;
  {$IFDEF Debug}
    FBlock: TCharBlock;
  {$ENDIF}
    function CatchInvalidDest(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean; overload;
    function CatchInvalidDest(const InvalidChar: TInvalidChar; Site: TCodePage): Boolean; overload;
    function CatchInvalidSequence(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
    function CatchInvalidSurrogate(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
    function CatchInvalidSource(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean; overload;
    function CatchInvalidSource(const InvalidChar: TInvalidChar; Site: TCodePage): Boolean; overload;
    procedure InvalidSource(Fmt: PLegacyChar; Site: TCharSet; InvalidChar: QuadChar = 0); overload;
    procedure InvalidSource(Fmt: PLegacyChar; Site: TCodePage; InvalidChar: QuadChar = 0); overload;
  public
    constructor Create(const InvalidChar: TInvalidChar; SourceSite, DestSite: TCharSet); overload;
    constructor Create(const InvalidChar: TInvalidChar; SourceSite: TCodePage; DestSite: TCharSet); overload;
    constructor Create(const InvalidChar: TInvalidChar; SourceSite: TCharSet; DestSite: TCodePage); overload;
    constructor Create(const InvalidChar: TInvalidChar; SourceSite: TCodePage; DestSite: TCodePage); overload;
  {$IFDEF Debug}
    property Block: TCharBlock read FBlock;
  {$ENDIF}
    property DestSite: TConvertSite read FDestSite;
    property InvalidChar: TInvalidChar read FInvalidChar;
    property SourceSite: TConvertSite read FSourceSite;
  end;

const
  LeadBytes = [Low(TLeadByte)..High(TLeadByte)];

{ Character set convertions }

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy = []): Integer;
function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16 = []): Integer;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;
function PlatformCodePage(CodePage: Word = CP_ACP): TPlatformCodePage;

function FromLatin(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; overload;
function FromLatin(Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; overload;
function FromLatin(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; overload;
function FromLatin(Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextLegacyChar; overload;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []; ThresholdBytes: Integer = MaxInt): TNextLegacyChar; overload;
function FromUTF8(Source: PLegacyChar; Count: Integer; Dest: PWideChar;
  DestOptions: TEncodeUTF16 = []; ThresholdBytes: Integer = MaxInt): TNextLegacyChar; overload;
function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []; ThresholdBytes: Integer = MaxInt): TNextLegacyChar; overload;
function FromUTF8(Source: PLegacyChar; Count: Integer; Dest: PQuadChar;
  DestOptions: TEncodeUTF32 = []; ThresholdBytes: Integer = MaxInt): TNextLegacyChar; overload;

function IsUTF8(Source: PLegacyChar; Count: Integer; DestOptions: TEncodeUTF16 = [];
  ThresholdBytes: Integer = 4): Boolean;

function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload;
function ToLegacy(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload;
function ToLegacy(var Info: TStringInfo; Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; overload;
function ToLegacy(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; overload;
function ToLegacy(var Info: TStringInfo; Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; overload;
function ToLegacy(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextQuadChar; overload;

function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextQuadChar; overload;
function FromUTF16(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): TNextQuadChar; overload;

function ToUTF16(var Info: TStringInfo; Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextWideChar; overload;
function ToUTF16(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16): TNextWideChar; overload;

{ Text splitting and merging }

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

{ Character set convertions }

const
  InvalidUTF8Mask   = $80000000;

type
  TLegacyCharBuf = array[0..$3FF] of LegacyChar;
  TWideCharBuf = array[0..$3FF] of WideChar;

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

function FromLatin(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Latin1: Boolean; Dest: PWideChar; DestOptions: TEncodeUTF16): TNextLegacyChar;
var
  C: LegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0);

  while Count > 0 do
  begin
    C := Source[Result.SourceCount];
    Inc(Result.SourceCount);

    if (C in [#$00..#$7F]) or (Latin1 and (C in [#$A0..#$FF])) then
    begin
      PWord(Dest + Result.DestCount)^ := Byte(C) shl (Byte(coBigEndian in DestOptions) * 8);

      if C in [#$A0..#$FF] then
      begin
        Dec(Result.SuccessBytes); // negative for Latin1
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbLatin1Supplement); // Fast core
      {$ENDIF}
      end
      else
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbBasicLatin); // Fast core
      {$ENDIF}
    end
    else if coForceInvalid in DestOptions then
    begin
      PWord(Dest + Result.DestCount)^ := Word(Unknown_UTF16) shl (Byte(coBigEndian in DestOptions) * 8);
      Inc(Info.InvalidCount);
    {$IFDEF UTF32}
      if coRangeBlocks in DestOptions then
        Include(Info.Blocks, cbSpecials); // Fast core
    {$ENDIF}
    end
    else
    begin
      Result.InvalidChar.Value := Byte(C);
      Break;
    end;

    Inc(Result.DestCount);
    Dec(Count);
  end;

  with Info, Result do
  begin
    Inc(Count, DestCount);
    Inc(CharCount, DestCount);
  end;
end;

function FromLatin(Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromLatin(Info, Source, Count, Latin1, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csLatin1, csUTF16);
end;

function FromLatin(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Latin1: Boolean; Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function FromLatin(Source: PLegacyChar; Count: Integer; Latin1: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromLatin(Info, Source, Count, Latin1, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csLatin1, csUTF32);
end;

function ExtractUTF8Char(var Source: PLegacyChar; Limit: PLegacyChar;
  DestOptions: TEncodeOptions): QuadChar; // low-level UTF-8 decoding
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
          Result := InvalidUTF8Mask;
        end
        else
          Result := (Bytes + 1) or (Byte(Bytes - B + 2) shl 8) or InvalidUTF8Mask; // Fast core

      Exit;
    end;

    Result := FirstByte or InvalidUTF8Mask; // Fast core
  end
  else
    Result := FirstByte;
end;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PWideChar; DestOptions: TEncodeUTF16; ThresholdBytes: Integer): TNextLegacyChar;
var
  Limit: PLegacyChar;
  Q, T: QuadChar;
  W: Word;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
  Limit := Source + Count;
  T := 0;
{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}

  while (Source < Limit) and (Result.SuccessBytes < ThresholdBytes) do
  begin
    if T <> 0 then
      Q := T
    else
      Q := ExtractUTF8Char(Source, Limit, DestOptions);

    if Q and InvalidUTF8Mask = 0 then
      case Q of
        Low(THighSurrogates)..High(THighSurrogates):
          if coSurrogates in DestOptions then
          begin
            T := ExtractUTF8Char(Source, Limit, DestOptions);
            if T and InvalidUTF8Mask = 0 then
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
                      Inc(DestCount, 2);
                      Inc(SuccessBytes, 6); // two 3-byte sequences
                    end;

                    with Info do
                    begin
                      Inc(CharCount);
                      Inc(SequenceCount);
                      Inc(SurrogatePairCount);
                    {$IFDEF UTF32}
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
                Q := Q or InvalidUTF8Mask;
              end
            else if T and not InvalidUTF8Mask = 0 then // coContinuous
              Break;
          end;

        Low(TLowSurrogates)..High(TLowSurrogates), Word(BOM_UTF16_BE): // but not Word(BOM_UTF16_LE)
          Q := Q or InvalidUTF8Mask;

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
              Inc(DestCount, 2);
              Inc(SuccessBytes, 4); // one 4-byte sequence
            end;

            with Info do
            begin
              Inc(CharCount);
              Inc(SequenceCount);
              Inc(SurrogatePairCount);
            {$IFDEF UTF32}
              if coRangeBlocks in DestOptions then
              begin
                Block := FindCharBlock(Q, Block);
                Include(Blocks, Block);
              end;
            {$ENDIF}
            end;

            Continue;
          end;
      else
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
        begin
          Block := FindCharBlock(Q, Block);
          Include(Info.Blocks, Block);
        end;
      {$ENDIF}
      end
    else if Q and not InvalidUTF8Mask = 0 then // coContinuous
      Break;

    if Q > High(TUnicodeBMP) then // both InvalidUTF8Mask and higher Unicode
      if coForceInvalid in DestOptions then
      begin
        Q := QuadChar(UnknownUTF16);
        Inc(Info.InvalidCount);
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbSpecials); // Fast core
      {$ENDIF}
      end
      else
      begin
        if Q and InvalidUTF8Mask <> 0 then
          case Q and not InvalidUTF8Mask of
            Low(THighSurrogates)..High(THighSurrogates),
            Low(TLowSurrogates)..High(TLowSurrogates):
              Q := Word(Q);
          end;
        Result.InvalidChar.Value := Q;
        Inc(Info.Count, Result.DestCount);
        Result.SourceCount := Count - (Limit - Source);
        Exit;
      end;

    if coBigEndian in DestOptions then
      Dest^ := WideChar(Swap(Q))
    else
      Dest^ := WideChar(Q);

    Inc(Dest);

    with Result do
    begin
      Inc(DestCount);
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

  Inc(Info.Count, Result.DestCount);
  Result.SourceCount := Count - (Limit - Source);
end;

function FromUTF8(Source: PLegacyChar; Count: Integer; Dest: PWideChar;
  DestOptions: TEncodeUTF16; ThresholdBytes: Integer): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromUTF8(Info, Source, Count, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF}, ThresholdBytes);
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF8, csUTF16);
end;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; ThresholdBytes: Integer): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function FromUTF8(Source: PLegacyChar; Count: Integer; Dest: PQuadChar;
  DestOptions: TEncodeUTF32; ThresholdBytes: Integer): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromUTF8(Info, Source, Count, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF}, ThresholdBytes);
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF8, csUTF32);
end;

function IsUTF8(Source: PLegacyChar; Count: Integer; DestOptions: TEncodeUTF16;
  ThresholdBytes: Integer): Boolean;
var
  Info: TStringInfo;
  UTF8: TNextLegacyChar;
  Chunk: TWideCharBuf;
  Cnt: Integer;
  Opt: TEncodeUTF16;
begin
{$IFDEF Debug}
  FillChar(Info, SizeOf(Info), 0);
{$ENDIF}

  Cnt := Length(Chunk) shr Byte(coSurrogates in DestOptions);
  Opt := DestOptions + [coContinuous];

  repeat
    if Count < Cnt then
    begin
      Cnt := Count;
      Opt := DestOptions;
    end;

    UTF8 := FromUTF8(Info, Source, Cnt, Chunk, Opt, ThresholdBytes);
    if UTF8.InvalidChar.Value <> 0 then
    begin
      Result := False;
      Exit;
    end;
    if UTF8.SuccessBytes >= ThresholdBytes then
    begin
      Result := True;
      Exit;
    end;
    with UTF8 do
    begin
      Inc(Source, SourceCount);
      Dec(Count, SourceCount);
    end;
  until Count = 0;

  Result := Info.SequenceCount >= ThresholdBytes div 2; // per 2-byte sequences
end;

function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
var
  SourceInfo, ChunkInfo: TStringInfo;
  Chunk: TWideCharBuf;
  UTF8: TNextLegacyChar;
  Legacy: TNextWideChar;
  Cnt: Integer;
  Opt: TEncodeUTF16;
  C: LegacyChar;
begin
  if Info.CodePage <> nil then
  begin
    Result := Info.CodePage.ToLegacy(Info, Source, Count, nil, SourceOptions, Dest, DestOptions);
    Exit;
  end;

  FillChar(Result, SizeOf(Result), 0);

  if soDetectCharSet in SourceOptions then
  begin
    FillChar(Result, SizeOf(Result), 0);
    SourceInfo := Info;
  {$IFDEF Debug}
    FillChar(ChunkInfo, SizeOf(ChunkInfo), 0);
  {$ENDIF}
    Cnt := Length(Chunk);
    Opt := DestOptions + [coContinuous];

    repeat
      if Count - Result.SourceCount < Cnt then
      begin
        Cnt := Count - Result.SourceCount;
        Opt := DestOptions;
      end;

      UTF8 := FromUTF8(ChunkInfo, Source + Result.SourceCount, Cnt, Chunk,
        Opt - [coForceInvalid, coBigEndian]);
      if UTF8.InvalidChar.Value <> 0 then
        Break;

      Legacy := ToLegacy(Info, Chunk, UTF8.DestCount, False, Dest + Result.DestCount,
        DestOptions - [coForceInvalid]);
      with Result do
      begin
        Inc(SourceCount, UTF8.SourceCount);
        Inc(SuccessBytes, UTF8.SuccessBytes);
        Inc(DestCount, Legacy.DestCount);
      end;

      if Legacy.InvalidChar.Value <> 0 then
      begin
        if soLatin1 in SourceOptions then
        begin
          Info := SourceInfo;
          Break;
        end;
        Exit; // not a 7-bit ASÑII
      end;
    until Count - Result.SourceCount < Cnt;
  end;

  FillChar(Result, SizeOf(Result), 0);
  Cnt := 0;

  while Count > 0 do
  begin
    C := Source[Cnt];
    if C in [#$00..#$7F, #$A0..#$FF] then
    begin
      Dest[Cnt] := C;
      if C in [#$A0..#$FF] then
      begin
        Dec(Result.SuccessBytes); // negative for Latin1
        Inc(Info.Latin1Count);
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbLatin1Supplement); // Fast core
      {$ENDIF}
      end
      else
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbBasicLatin); // Fast core
      {$ENDIF}
    end
    else if coForceInvalid in DestOptions then
    begin
      Dest[Cnt] := Unknown_Latin;
    {$IFDEF UTF32}
      if coRangeBlocks in DestOptions then
        Include(Info.Blocks, cbBasicLatin); // Fast core
    {$ENDIF}
    end
    else
    begin
      Result.InvalidChar.Value := QuadChar(C);
      Break;
    end;
  end;

  with Result do
  begin
    SourceCount := Cnt;
    DestCount := Cnt;
  end;
  with Info do
  begin
    Inc(Count, Cnt);
    Inc(CharCount, Cnt);
  end;
end;

function ToLegacy(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, TCharSet(soLatin1 in SourceOptions), TCharSet(coLatin1 in DestOptions));
end;

function ToLegacy(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  BigEndian: Boolean; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;

function ExtractChar(var Index: Integer): Word;
begin
  Result := Word(Source[Index]);
  if BigEndian then
    Result := Swap(Result);
  Inc(Index);
end;

var
  Q: QuadChar;
  T, W: Word;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}

  while Result.SourceCount < Count do
  begin
    Q := ExtractChar(Result.SourceCount);

    case Word(Q) of
      $00..$7F:
        begin
          if (Q = 0) and (DestOptions * [coCESU8, coEncodeZero] = [coCESU8, coEncodeZero]) then
          begin
            PWord(Dest)^ := EncodedZero_UTF8; // Fast core
            Inc(Dest, SizeOf(Word));
            Inc(Result.DestCount, SizeOf(Word));
          end
          else
          begin
            Dest^ := LegacyChar(Q);
            Inc(Dest);
            Inc(Result.DestCount);
          end;

          with Info do
          begin
            Inc(CharCount);
          {$IFDEF UTF32}
            if coRangeBlocks in DestOptions then
              Include(Info.Blocks, cbBasicLatin); // Fast core
          {$ENDIF}
          end;

          Continue;
        end;

      Low(THighSurrogates)..High(THighSurrogates):
        begin
          T := ExtractChar(Result.SourceCount);

          case T of
            Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                Q := (Q - Low(THighSurrogates)) shl 10 + Low(TUnicodeSMP) +
                     T - Low(TLowSurrogates);
                if coCESU8 in DestOptions then
                begin
                  W := Q;
                  PLongWord(Dest)^ := // Fast core
                    ($E0 or Byte(W shr 12)) or
                    (($80 or (Byte(W shr 6) and $3F)) shl 8) or
                    (($80 or Byte(W and $3F)) shl 16) or
                    (($E0 or Byte(T shr 12)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  PWord(Dest)^ :=
                    (($80 or (Byte(T shr 6) and $3F)) shl 8) or
                    (($80 or Byte(T and $3F)) shl 16);
                  Inc(Dest, SizeOf(Word));
                  Inc(Result.DestCount, 6);
                  Inc(Info.SurrogatePairCount);
                end
                else if not (coLatin1 in DestOptions) then
                begin
                  PLongWord(Dest)^ := // Fast core
                    ($F0 or Byte(Q shr 18)) or
                    (($80 or Byte(Q shr 12)) shl 8) or
                    (($80 or (Byte(Q shr 6) and $3F)) shl 16) or
                    (($80 or Byte(Q and $3F)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  Inc(Result.DestCount, SizeOf(LongWord));
                end
                else if coForceInvalid in DestOptions then
                begin
                  Dest^ := Unknown_Latin;
                  Inc(Dest);
                  Inc(Result.DestCount);

                  with Info do
                  begin
                    Inc(CharCount);
                    Inc(InvalidCount);
                  {$IFDEF UTF32}
                    if coRangeBlocks in DestOptions then
                      Include(Blocks, cbBasicLatin); // Fast core
                  {$ENDIF}
                  end;

                  Continue;
                end
                else
                begin
                  Result.InvalidChar.Value := Q;
                  Break;
                end;
              end;
          end;
        end;

      Low(TLowSurrogates)..High(TLowSurrogates), Word(BOM_UTF16_BE): // but not Word(BOM_UTF16_LE)
        if coForceInvalid in DestOptions then
        begin
          if DestOptions * [coCESU8, coLatin1] = [coLatin1] then
          begin
            Dest^ := Unknown_Latin;
            Inc(Dest);
            Inc(Result.DestCount);
          {$IFDEF UTF32}
            if coRangeBlocks in DestOptions then
              Include(Info.Blocks, cbBasicLatin); // Fast core
          {$ENDIF}
          end
          else
          begin
            PLongWord(Dest)^ := Unknown_UTF8; // Fast core
            Inc(Dest, 3);
            Inc(Result.DestCount, 3);
          {$IFDEF UTF32}
            if coRangeBlocks in DestOptions then
              Include(Info.Blocks, cbSpecials); // Fast core
          {$ENDIF}
          end;

          with Info do
          begin
            Inc(CharCount);
            Inc(InvalidCount);
          end;

          Continue;
        end
        else
        begin
          Result.InvalidChar.Value := Q;
          Break;
        end;
    else
      if DestOptions * [coCESU8, coLatin1] = [coLatin1] then
      begin
        case Word(Q) of
          $80..$FF:
            begin
              Dest^ := LegacyChar(Q);
            {$IFDEF UTF32}
              if coRangeBlocks in DestOptions then
                Include(Info.Blocks, cbLatin1Supplement); // Fast core
            {$ENDIF}
            end;
        else
          if coForceInvalid in DestOptions then
          begin
            Dest^ := Unknown_Latin;
            Inc(Info.InvalidCount);
          {$IFDEF UTF32}
            if coRangeBlocks in DestOptions then
              Include(Info.Blocks, cbBasicLatin); // Fast core
          {$ENDIF}
          end
          else
          begin
            Result.InvalidChar.Value := Q;
            Break;
          end;
        end;

        Inc(Dest);
        Inc(Result.DestCount);
        Inc(Info.CharCount);

        Continue;
      end
      else
      begin
        W := Q;
        case W of
          $80..$7FF:
            begin
              PWord(Dest)^ :=
                ($C0 or (W shr 6)) or
                (($80 or (W and $3F)) shl 8);
              Inc(Dest, SizeOf(Word));
              Inc(Result.DestCount, SizeOf(Word));
            end;
        else
          PLongWord(Dest)^ := // Fast core
            ($E0 or Byte(W shr 12)) or
            (($80 or (Byte(W shr 6) and $3F)) shl 8) or
            (($80 or Byte(W and $3F)) shl 16);
          Inc(Dest, 3);
          Inc(Result.DestCount, 3);
        end;
      end;
    end;

    Inc(Info.CharCount);
  {$IFDEF UTF32}
    if coRangeBlocks in DestOptions then
    begin
      Block := FindCharBlock(Q, Block);
      Include(Info.Blocks, Block);
    end;
  {$ENDIF}
  end;

  Inc(Info.Count, Result.DestCount);
end;

function ToLegacy(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToLegacy(Info, Source, Count, BigEndian, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF16, TCharSet(coLatin1 in DestOptions));
end;

function ToLegacy(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  BigEndian: Boolean; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function ToLegacy(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextQuadChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToLegacy(Info, Source, Count, BigEndian, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF32, TCharSet(coLatin1 in DestOptions));
end;

function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  BigEndian: Boolean; Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function FromUTF16(Source: PWideChar; Count: Integer; BigEndian: Boolean;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextWideChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromUTF16(Info, Source, Count, BigEndian, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF16, csUTF32);
end;

function ToUTF16(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  BigEndian: Boolean; Dest: PWideChar; DestOptions: TEncodeUTF16): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function ToUTF16(Source: PQuadChar; Count: Integer; BigEndian: Boolean;
  Dest: PWideChar; DestOptions: TEncodeUTF16): TNextQuadChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToUTF16(Info, Source, Count, BigEndian, Dest, DestOptions
    {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF32, csUTF16);
end;

{ Text splitting and merging }

function SplitText(Source: PLegacyChar; Count: Integer;
  DestEvent: TLegacyTextEvent; const DestOptions): TTextSplit;
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

          P := StrScan(Next, Limit - Next, LineBreakChars[B]);
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

function EConvert.CatchInvalidDest(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
begin
  FBlock := FindCharBlock(InvalidChar.Value);
  if FBlock <> cbNonUnicode then
  begin
    Create(sInvalidCharSetChar, CP_LEGACY, [UnicodeBlockNames[Block],
      InvalidChar.Value, CharSets[Site]]);
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidDest(const InvalidChar: TInvalidChar; Site: TCodePage): Boolean;
begin
  FBlock := FindCharBlock(InvalidChar.Value);
  if FBlock <> cbNonUnicode then
  begin
    Create(sInvalidCodePageChar, CP_LEGACY, [UnicodeBlockNames[Block],
      InvalidChar.Value, Site.Number, Site.Name]);
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidSequence(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
begin
  if InvalidChar.Value and InvalidUTF8Mask <> 0 then
    if InvalidChar.BrokenSequence <> 0 then
      Create(sBrokenUTF8, [InvalidChar.BrokenSequence, InvalidChar.StartingByte{and $FF}])
    else
      Create(sBadUTF8, [InvalidChar.StartingByte{and $FF}])
  else
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function EConvert.CatchInvalidSurrogate(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
const
  CharSets: array[Boolean] of PLegacyChar = (sCESU8, sUTF16);
begin
  if Site <> csUTF32 then
  begin
    case InvalidChar.Value of
      Low(THighSurrogates)..High(THighSurrogates):
        begin
          Create(sBrokenSurrogate, [CharSets[Site = csUTF16], InvalidChar.Value]);
          Result := True;
          Exit;
        end;
      Low(TLowSurrogates)..High(TLowSurrogates):
        begin
          Create(sBadSurrogate, [CharSets[Site = csUTF16], InvalidChar.Value]);
          Result := True;
          Exit;
        end;
    end;
  end;
  Result := False;
end;

function EConvert.CatchInvalidSource(const InvalidChar: TInvalidChar; Site: TCharSet): Boolean;
begin
  if not CatchInvalidSequence(InvalidChar, Site) then
    if InvalidChar.Value >= High(TNonUnicode) then
      InvalidSource(sNonUnicode, Site)
    else if InvalidChar.Value > High(TUnicodeBMP) then
      InvalidSource(sDisallowedSurrogates, Site)
    else
    begin
      Result := CatchInvalidSurrogate(InvalidChar, Site);
      Exit;
    end;
  Result := True;
end;

function EConvert.CatchInvalidSource(const InvalidChar: TInvalidChar; Site: TCodePage): Boolean;
begin
  if not CatchInvalidSequence(InvalidChar, csUTF8) then
    if InvalidChar.Value >= High(TNonUnicode) then
      InvalidSource(sNonUnicode, Site)
    else if InvalidChar.Value > High(TUnicodeBMP) then
      InvalidSource(sDisallowedSurrogates, Site)
    else
    begin
      Result := CatchInvalidSurrogate(InvalidChar, csUTF8);
      Exit;
    end;
  Result := True;
end;

procedure EConvert.InvalidSource(Fmt: PLegacyChar; Site: TCharSet; InvalidChar: QuadChar);
var
  Buf: TLegacyCharBuf;
begin
  FormatBuf(sCharSetSource, [CharSets[Site]], Buf);
  Create(Fmt, [Buf, InvalidChar]);
end;

procedure EConvert.InvalidSource(Fmt: PLegacyChar; Site: TCodePage; InvalidChar: QuadChar);
var
  Buf: TWideCharBuf;
begin
  with Site do
    WideFormatBuf(sCodePageSource, [Number, Name], Buf);
  Create(Fmt, CP_LEGACY, [Buf, InvalidChar]);
end;

constructor EConvert.Create(const InvalidChar: TInvalidChar; SourceSite, DestSite: TCharSet);
begin
{$IFDEF Debug}
  if InvalidChar.Value = 0 then
    Create(sCannotMixCharSetAndCharSet, [CharSets[SourceSite], CharSets[DestSite]])
  else
{$ENDIF}
  if not CatchInvalidSource(InvalidChar, SourceSite) then
  {$IFDEF UTF32}
    if not CatchInvalidDest(InvalidChar, DestSite) then
      InvalidSource(sNonUnicode, SourceSite);
  {$ELSE}
    if SourceSite <> DestSite then
      Create(sInvalidCharSetToCharSet, [CharSets[SourceSite], CharSets[DestSite]])
    else
      InvalidSource(sInvalidChar, SourceSite, InvalidChar.Value);
  {$ENDIF}
{$IFDEF Debug}
  FBlock := FindCharBlock(InvalidChar.Value);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CharSet := SourceSite;
  FDestSite.CharSet := DestSite;
end;

constructor EConvert.Create(const InvalidChar: TInvalidChar; SourceSite: TCodePage; DestSite: TCharSet);
begin
{$IFDEF Debug}
  if InvalidChar.Value = 0 then
    Create(sCannotMixCodePageAndCharSet, CP_LEGACY, [SourceSite.Number, SourceSite.Name, CharSets[DestSite]])
  else
{$ENDIF}
  if not CatchInvalidSource(InvalidChar, SourceSite) then
  {$IFDEF UTF32}
    if not CatchInvalidDest(InvalidChar, DestSite) then
      InvalidSource(sNonUnicode, SourceSite);
  {$ELSE}
    Create(sInvalidCodePageToCharSet, CP_LEGACY, [SourceSite.Number, SourceSite.Name, CharSets[DestSite]]);
  {$ENDIF}
{$IFDEF Debug}
  FBlock := FindCharBlock(InvalidChar.Value);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CodePage := SourceSite;
  FDestSite.CharSet := DestSite;
end;

constructor EConvert.Create(const InvalidChar: TInvalidChar; SourceSite: TCharSet; DestSite: TCodePage);
begin
{$IFDEF Debug}
  if InvalidChar.Value = 0 then
    Create(sCannotMixCharSetAndCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Number, DestSite.Name])
  else
{$ENDIF}
  if not CatchInvalidSource(InvalidChar, SourceSite) then
  {$IFDEF UTF32}
    if not CatchInvalidDest(InvalidChar, DestSite) then
      InvalidSource(sNonUnicode, SourceSite);
  {$ELSE}
    Create(sInvalidCharSetToCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Number, DestSite.Name]);
  {$ENDIF}
{$IFDEF Debug}
  FBlock := FindCharBlock(InvalidChar.Value);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CharSet := SourceSite;
  FDestSite.CodePage := DestSite;
end;

constructor EConvert.Create(const InvalidChar: TInvalidChar; SourceSite, DestSite: TCodePage);
begin
{$IFDEF Debug}
  if InvalidChar.Value = 0 then
    Create(sCannotMixCodePageAndCodePage, CP_LEGACY, [SourceSite.Number, SourceSite.Name, DestSite.Number, DestSite.Name])
  else
{$ENDIF}
  if not CatchInvalidSource(InvalidChar, SourceSite) then
  {$IFDEF UTF32}
    if not CatchInvalidDest(InvalidChar, DestSite) then
      InvalidSource(sNonUnicode, SourceSite);
  {$ELSE}
    if SourceSite.Number <> DestSite.Number then
      Create(sInvalidCodePageToCodePage, CP_LEGACY, [SourceSite.Number, SourceSite.Name, DestSite.Number, DestSite.Name])
    else
      InvalidSource(sInvalidChar, SourceSite, InvalidChar.Value);
  {$ENDIF}
{$IFDEF Debug}
  FBlock := FindCharBlock(InvalidChar.Value);
{$ENDIF}
  FInvalidChar := InvalidChar;
  FSourceSite.CodePage := SourceSite;
  FDestSite.CodePage := DestSite;
end;

{ TCodePage }

function TCodePage.Accept(const Info: TStringInfo): Boolean;
begin
  with Info do
    Result := (Count = 0) or ((CodePage <> nil) and (CodePage.FNumber = FNumber));
end;

function TCodePage.ToLegacy(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.CodePage := Self;
  Result := ToLegacy(Info, Source, Count, CodePage, SourceOptions, Dest,
    DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    if CodePage <> nil then
      raise EConvert.Create(Result.InvalidChar, CodePage, Self)
    else
      raise EConvert.Create(Result.InvalidChar, TCharSet(soLatin1 in SourceOptions), Self);
end;

function TCodePage.FromUTF16(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromUTF16(Info, Source, Count, SourceOptions, Dest,
    DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF16, Self);
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToUTF16(Info, Source, Count, SourceOptions, Dest,
    DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, Self, csUTF16);
end;

{$IFDEF UTF32}
function TCodePage.FromUTF32(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextQuadChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := FromUTF32(Info, Source, Count, SourceOptions, Dest,
    DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, csUTF32, Self);
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := ToUTF32(Info, Source, Count, SourceOptions, Dest,
    DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
  if Result.InvalidChar.Value <> 0 then
    raise EConvert.Create(Result.InvalidChar, Self, csUTF32);
end;
{$ENDIF}

{ TPlatformCodePage }

constructor TPlatformCodePage.Create(var Info: TCPInfoEx);
var
  SourceMap: array[Byte] of LegacyChar;
  B, L: Byte;
{$IFDEF UTF32}
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
    (FNumber, 0, SourceMap, Length(SourceMap), TSingleByteCodePage(Self).FLegacyCharMap,
      Length(TSingleByteCodePage(Self).FLegacyCharMap)) = 0
  then
    RaiseLastPlatformError;

  FWideCharMapLo := WideChar($100);
{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}
  for C := Low(C) to High(C) do
  begin
    W := TSingleByteCodePage(Self).FLegacyCharMap[C];
    if (W <> WideChar(0)) and (W <> WideChar(C)) then
      if W = Info.UnicodeDefaultChar then
        TSingleByteCodePage(Self).FLegacyCharMap[C] := WideChar(0)
      else
      begin
        if W > FWideCharMapHi then
          FWideCharMapHi := W
        else if W < FWideCharMapLo then
          FWideCharMapLo := W;
      {$IFDEF UTF32}
        Block := FindCharBlock(QuadChar(W), Block);
        Include(FBlocks, Block);
      {$ENDIF}
      end;
  end;
{$IFDEF UTF32}
  if FWideCharMapLo >= WideChar($80) then
    Include(FBlocks, cbBasicLatin);
{$ENDIF}
end;

destructor TPlatformCodePage.Destroy;
begin
  FreeMem(FName);
  inherited;
end;

function TPlatformCodePage.WideCharMapCount: Word;
begin
  if FWideCharMapHi <> WideChar(0) then
    Result := Word(FWideCharMapHi) - Word(FWideCharMapLo) + 1
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

{$IFDEF NoCodePages}
procedure TString.Insert(Source: Pointer; Count: Integer;
  SourceOptions: TStringOptions; DestIndex: Integer);
{$ENDIF}
{$IFDEF CTRL_SHIFT_UP}
begin
end;
{$ENDIF}

{$IFNDEF NoCodePages}
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

{$IFNDEF NoCodePages}
function TString.Insert(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextLegacyChar;
var
  Info: TStringInfo;
begin
  if (TLegacyString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(LegacyChar)) and
    not ((soDetectCharSet in SourceOptions) and (coContinuous in DestOptions)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    TLegacyString(Self).FCodePage := CodePage;
    with Result do
    begin
      SourceCount := Count;
      DestCount := Count;
    end;
  end
  else
  begin
  {$IFDEF Debug}
    FillChar(Info, SizeOf(Info), 0);
  {$ELSE}
    Info.CodePage := nil;
  {$ENDIF}
    Result := Insert(Info, Source, Count, CodePage, SourceOptions, DestIndex,
      DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
    if Result.InvalidChar.Value <> 0 then
      if CodePage <> nil then
        if Info.CodePage <> nil then
          raise EConvert.Create(Result.InvalidChar, CodePage, Info.CodePage)
        else
          raise EConvert.Create(Result.InvalidChar, CodePage, TCharSet(coLatin1 in DestOptions))
      else
        if Info.CodePage <> nil then
          raise EConvert.Create(Result.InvalidChar, TCharSet(soLatin1 in SourceOptions), Info.CodePage)
        else
          raise EConvert.Create(Result.InvalidChar, TCharSet(soLatin1 in SourceOptions), TCharSet(coLatin1 in DestOptions));
  end;
end;

function TString.Insert(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): TNextWideChar;
var
  Info: TStringInfo;
begin
  if (TWideString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(WideChar)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    with Result do
    begin
      SourceCount := Count;
      DestCount := Count;
    end;
  end
  else
  begin
  {$IFDEF Debug}
    FillChar(Info, SizeOf(Info), 0);
  {$ELSE}
    Info.CodePage := nil;
  {$ENDIF}
    Result := Insert(Info, Source, Count, SourceOptions, DestIndex,
      DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
    if Result.InvalidChar.Value <> 0 then
      if Info.CodePage <> nil then
        raise EConvert.Create(Result.InvalidChar, csUTF16, Info.CodePage)
      else
        raise EConvert.Create(Result.InvalidChar, csUTF16, TCharSet(coLatin1 in DestOptions));
  end;
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Insert(
  Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  DestIndex: Integer; DestOptions: TEncodeOptions): TNextQuadChar;
var
  Info: TStringInfo;
begin
  if (TQuadString(Self).FData = nil) and not (coForceInvalid in DestOptions) and
    (ByteCount(1) = SizeOf(QuadChar)) then
  begin
    DoInsert(Source, Count, SourceOptions, DestIndex);
    with Result do
    begin
      SourceCount := Count;
      DestCount := Count;
    end;
  end
  else
  begin
  {$IFDEF Debug}
    FillChar(Info, SizeOf(Info), 0);
  {$ELSE}
    Info.CodePage := nil;
  {$ENDIF}
    Result := Insert(Info, Source, Count, SourceOptions, DestIndex,
      DestOptions {$IFDEF UTF32} - [coRangeBlocks] {$ENDIF});
    if Result.InvalidChar.Value <> 0 then
      if Info.CodePage <> nil then
        raise EConvert.Create(Result.InvalidChar, csUTF32, Info.CodePage)
      else
        raise EConvert.Create(Result.InvalidChar, csUTF32, TCharSet(coLatin1 in DestOptions));
  end;
end;

{$ELSE NoCodePages}

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

function TLegacyString.ByteCount(Count: Integer): Integer;
begin
  Result := Count;
end;

function TLegacyString.Length(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;

{$IFNDEF Lite}
function TLegacyString.Length(Source: Pointer; MaxLength: Integer): Integer;
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

{$IFNDEF NoCodePages}
function TLegacyString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextLegacyChar;
begin
  AcceptRange(DestIndex, Count);
  if FCodePage <> nil then
//    Result := FCodePage.FromLegacy(Info, Source, Count, CodePage, SourceOptions, FData + DestIndex, DestOptions)
  else
    FillChar(Result, SizeOf(Result), 0);

{   and (soDetectCharSet in FOptions) then
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
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{$IFDEF UTF32}
function TLegacyString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
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
      Result := Insert(S.FData, S.Count, FCodePage, SourceOptions, DestIndex).DestCount
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
{$ENDIF NoCodePages}

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Insert(Value, StrLen(Value) {$IFNDEF NoCodePages}, FCodePage {$ENDIF});
end;

{ TWideString }

function TWideString.ByteCount(Count: Integer): Integer;
begin
  Result := Count * SizeOf(WideChar);
end;

function TWideString.Length(Source: Pointer): Integer;
begin
  Result := WideStrLen(Source);
end;

{$IFNDEF Lite}
function TWideString.Length(Source: Pointer; MaxLength: Integer): Integer;
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

{$IFNDEF NoCodePages}
function TWideString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TWideString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{$IFDEF UTF32}
function TWideString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
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
      Result := Insert(PWideChar(S.FData), S.Count, SourceOptions, DestIndex).DestCount
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
{$ENDIF NoCodePages}

procedure TWideString.SetData(Value: PWideChar);
begin
  Insert(Value, WideStrLen(Value));
end;

{ TQuadString }

function TQuadString.ByteCount(Count: Integer): Integer;
begin
  Result := Count * SizeOf(QuadChar);
end;

function TQuadString.Length(Source: Pointer): Integer;
begin
  Result := QuadStrLen(Source);
end;

{$IFNDEF Lite}
function TQuadString.Length(Source: Pointer; MaxLength: Integer): Integer;
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

{$IFNDEF NoCodePages}
function TQuadString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TQuadString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{$IFNDEF UTF32}
function TQuadString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
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
      Result := Insert(PQuadChar(S.FData), S.Count, SourceOptions, DestIndex).DestCount
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
{$ENDIF NoCodePages}

procedure TQuadString.SetData(Value: PQuadChar);
begin
  Insert(Value, QuadStrLen(Value));
end;

 { TSharedString }

function TSharedString.ByteCount(Count: Integer): Integer;
begin
  if FParent <> nil then
    Result := FParent.ByteCount(Count)
  else
    Result := 0;
end;

function TSharedString.Length(Source: Pointer): Integer;
begin
  if FParent <> nil then
    Result := FParent.Length(Source)
  else
    Result := 0;
end;

{$IFNDEF Lite}
function TSharedString.Length(Source: Pointer; MaxLength: Integer): Integer;
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

{$IFNDEF NoCodePages} // TODO
function TSharedString.IsUnique: Boolean;
begin
  Result := (FParent = nil);
end;
{$ENDIF}

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

function TSharedString.Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  {$IFNDEF NoCodePages} CodePage: TCodePage; {$ENDIF}
  SourceOptions: TLegacySource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TSharedString.Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{$IFDEF UTF32}
function TSharedString.Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer; DestOptions: TEncodeOptions): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;
{$ENDIF}

{$IFNDEF NoCodePages}
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
        begin
          LoadWideString;
//          Exit;
        end;
      BOM_UTF16_BE:
        begin
          LoadWideString;
//          Exit;
        end;
    end;
  end;
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

{$IFNDEF NoCodePages}
function TStrings.Load(Source: TReadableStream; SourceOptions: TStringOptions;
  AverageStringLength: Integer): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO: Load via TSharedString
end;
{$ENDIF NoCodePages}

function {$IFDEF NoCodePages} TLegacyStrings {$ELSE} TStrings {$ENDIF} .Load
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
    S.Insert(Source, Count, {$IFNDEF NoCodePages} CodePage, {$ENDIF} SourceOptions);
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
  {$IFNDEF NoCodePages}
    Opt.CodePage := CodePage;
  {$ENDIF}
    Opt.SourceOptions := SourceOptions;
    Result := SplitText(Data, Count, Append, Opt)
  end;
end;

{$IFDEF NoCodePages}
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

{$IFNDEF NoCodePages}
function TStringList.Load(Source: TReadableStream; SourceOptions: TStringOptions): TTextSplit;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO: Load via TSharedString
end;
{$ENDIF NoCodePages}

function {$IFDEF NoCodePages} TLegacyStringList {$ELSE} TStringList {$ENDIF} .Load
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
    FLast.FValue.Insert(Source, Count, {$IFNDEF NoCodePages} nil, {$ENDIF} SourceOptions);
  end;
end;

function TLegacyStringList.Load(Source: TSubstring; SourceOptions: TStringOptions): TTextSplit;
var
  Opt: TLegacyStringAppend;
begin
  with TLegacySubstring(Source) do
  begin
  {$IFNDEF NoCodePages}
    Opt.CodePage := CodePage;
  {$ENDIF}
    Opt.SourceOptions := SourceOptions;
    Result := SplitText(Data, Count, Append, Opt)
  end;
end;

{$IFDEF NoCodePages}
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

  if FWideCharMapLo <= FWideCharMapHi then
  begin
    FWideCharMap := AllocMem(Word(FWideCharMapHi) - Word(FWideCharMapLo) + 1);
    for C := Low(C) to High(C) do
    begin
      W := FLegacyCharMap[C];
      if W <> WideChar(0) then
        FWideCharMap[Word(W) - Word(FWideCharMapLo)] := C;
    end;
  end
  else
  begin
    FWideCharMapLo := WideChar(0);
    FWideCharMapHi := WideChar(0);
  end;
end;

destructor TSingleByteCodePage.Destroy;
begin
  FreeMem(FWideCharMap);
  inherited;
end;

class function TSingleByteCodePage.MaxCharBytes: Byte;
begin
  Result := 1;
end;

function TSingleByteCodePage.IsEBCDIC: Boolean;
begin
  Result := FWideCharMapLo < WideChar($80);
end;

function TSingleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar;
  Count: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
var
  Chunk: TWideCharBuf;
  ChunkInfo, DestInfo: TStringInfo;
  Legacy: TNextLegacyChar;
  UTF16: TNextWideChar;
  Cnt, I: Integer;
  Opt: TEncodeUTF16;
  C, D: LegacyChar;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
begin
  if Info.CodePage <> nil then
  begin
    if Info.CodePage.Number <> FNumber then
    begin
      Result := Info.CodePage.ToLegacy(Info, Source, Count, CodePage, SourceOptions,
        Dest, DestOptions);
      Exit;
    end;
  end;

  if soDetectCharSet in SourceOptions then
  begin
    FillChar(Result, SizeOf(Result), 0);
    DestInfo := Info;
  {$IFDEF Debug}
    FillChar(ChunkInfo, SizeOf(ChunkInfo), 0);
  {$ENDIF}
    Cnt := Length(Chunk);
    Opt := DestOptions + [coContinuous];

    repeat
      if Count < Cnt then
      begin
        Cnt := Count;
        Opt := DestOptions;
      end;

      Legacy := FromUTF8(ChunkInfo, Source + Result.SourceCount, Cnt, Chunk,
        Opt - [coForceInvalid, coBigEndian]);
      if Legacy.InvalidChar.Value <> 0 then
        Break;

      UTF16 := FromUTF16(DestInfo, Chunk, Legacy.DestCount, [], Dest + Result.DestCount, DestOptions);
      with Result do
      begin
        Inc(SourceCount, Legacy.SourceCount);
        Inc(SuccessBytes, Legacy.SuccessBytes);
        Inc(DestCount, UTF16.DestCount);
      end;
      if UTF16.InvalidChar.Value <> 0 then
      begin
        Result.InvalidChar := UTF16.InvalidChar;
        Break;
      end;

      Dec(Count, Legacy.SourceCount);
    until Count = 0;

    if Legacy.InvalidChar.Value = 0 then
    begin
      Info := DestInfo;
      Exit;
    end;
  end;

{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if CodePage <> nil then
  begin
    DestInfo := Info;
  {$IFDEF Debug}
    FillChar(ChunkInfo, SizeOf(ChunkInfo), 0);
  {$ENDIF}
    Cnt := Length(Chunk);
    Opt := DestOptions + [coContinuous];

    repeat
      if Count < Cnt then
      begin
        Cnt := Count;
        Opt := DestOptions;
      end;

      Legacy := CodePage.ToUTF16(ChunkInfo, Source + Result.SourceCount, Cnt, [], Chunk, Opt);
    {$IFDEF UTF32}
      // TODO: can be failed here due to surrogates, but SBCS having surrogates is very strange
    {$ENDIF}
      UTF16 := FromUTF16(DestInfo, Chunk, UTF16.DestCount, [], Dest + Result.DestCount, DestOptions);
      with Result do
      begin
        Inc(SourceCount, Legacy.SourceCount);
        Inc(DestCount, UTF16.DestCount);
      end;
      if UTF16.InvalidChar.Value <> 0 then
      begin
        Result.InvalidChar := UTF16.InvalidChar;
        Break;
      end;

      Dec(Count, Legacy.SourceCount);
    until Count = 0;

    Info := DestInfo;
    Exit;
  end;

  for I := 0 to Count - 1 do
  begin
    C := Source[I];
    if ((C in [#$00..#$7F]) or ((soLatin1 in SourceOptions) and (C in [#$A0..#$FF]))) and
      (Word(C) >= Word(FWideCharMapLo)) and (Word(C) <= Word(FWideCharMapHi))
    then
      D := FWideCharMap[Word(C) - Word(FWideCharMapLo)]
    else
      D := #0;

    if (D <> #0) or (C = #0) then
    begin
      Dest[I] := D;
      if D > #$7F then
        Inc(Info.Latin1Count);
    {$IFDEF UTF32}
      if coRangeBlocks in DestOptions then
      begin
        Block := FindCharBlock(QuadChar(D), Block);
        Include(Info.Blocks, Block);
      end;
    {$ENDIF}
    end
    else
      if coForceInvalid in DestOptions then
      begin
        Dest[I] := Unknown_Latin;
        Inc(Info.InvalidCount);
      {$IFDEF UTF32}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbBasicLatin); // Fast core
      {$ENDIF}
      end
      else
      begin
        Result.InvalidChar.Value := QuadChar(C);
        Exit;
      end;

    Inc(Result.DestCount);
  end;

  with Info, Result do
  begin
    Inc(Count, DestCount);
    Inc(CharCount, DestCount);
  end;
end;

function TSingleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;
var
  Idx: Integer;
  C: LegacyChar;
  W, L: WideChar;
  Q: QuadChar;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
begin
{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);
  Idx := 0;

  while Idx < Count do
  begin
    W := Source[Idx];  
    Inc(Idx);

    if soBigEndian in SourceOptions then
      W := WideChar(Swap(Word(W)));

    if W = Unknown_UTF16 then
      C := Unknown_Latin
    else if W < FWideCharMapLo then
      C := LegacyChar(W)
    else if W <= FWideCharMapHi then
      C := FWideCharMap[Word(W) - Word(FWideCharMapLo)]
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
//        Info.InvalidChar := Q;
        Exit;
      end;
    end;

  {$IFDEF UTF32}
    if coRangeBlocks in DestOptions then
    begin
      Block := FindCharBlock(QuadChar(W), Block);
      Include(Info.Blocks, Block);
    end;
  {$ENDIF}

    with Result do
    begin
      Dest[DestCount] := C;
      Inc(DestCount);
    end;
  end;

  with Info, Result do
  begin
    Inc(Count, DestCount);
    Inc(CharCount, DestCount);
  end;
end;

function TSingleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): TNextLegacyChar;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
  DestInfo: TStringInfo;
  UTF8: TNextLegacyChar;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
begin
  if soDetectCharSet in SourceOptions then
  begin
    DestInfo := Info;
    UTF8 := FromUTF8(DestInfo, Source, Count, Dest, DestOptions - [coForceInvalid]);
//    if UTF8.SuccessBytes
//    if Result <> 0 then
    begin
      Info := DestInfo;
      Exit;
    end;
  end;

{$IFDEF UTF32}
  Block := cbNonUnicode;
{$ENDIF}
  for I := 0 to Count - 1 do
  begin
    C := Source[I];
    if C <> #0 then
    begin
      W := FLegacyCharMap[C];
      if W = WideChar(0) then
        if coForceInvalid in DestOptions then
        begin
          W := Unknown_UTF16;
          Inc(Info.InvalidCount);
        end
        else
        begin
//          Info.InvalidChar := QuadChar(W);
//          Result := 0;
          Exit;
        end;
    end
    else
      W := WideChar(0);

  {$IFDEF UTF32}
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

  Result.DestCount := Count;

  with Info, Result do
  begin
    Inc(Count, DestCount);
    Inc(CharCount, DestCount);
  end;
end;

{$IFDEF UTF32}
function TSingleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TSingleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;
{$ENDIF}

{ TDoubleByteCodePage }

constructor TDoubleByteCodePage.Create(var Info: TCPInfoEx);
var
  SourceMap: array[Byte] of DoubleByteChar;
  B: Byte;
{$IFDEF UTF32}
  Block: TCharBlock;
{$ENDIF}
  C, L: LegacyChar;
  I: Word;
  P: PTrailByteMap;
  T: LongWord;
  W: WideChar;
begin
{$IFDEF UTF32}
  if Info.LeadByte[0] = 0 then
    raise ECodePage.Create(Info, Self);
{$ENDIF}

  inherited;

{$IFDEF UTF32}
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
      GetMem(P, SizeOf(TLegacyCharMap));
      FDoubleByteMap[L] := P;
      if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
        (FNumber, 0, @SourceMap, Length(SourceMap) * SizeOf(DoubleByteChar),
          PWideChar(P), SizeOf(TLegacyCharMap) div SizeOf(WideChar)) = 0
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
            if W > FWideCharMapHi then
              FWideCharMapHi := W
            else if W < FWideCharMapLo then
              FWideCharMapLo := W;
          {$IFDEF UTF32}
            Block := FindCharBlock(QuadChar(W), Block);
            Include(FBlocks, Block);
          {$ENDIF}
          end;
      end;
    end;
    Inc(B, 2);
  end;

  FWideCharMap := AllocMem((Word(FWideCharMapHi) - Word(FWideCharMapLo) + 1) * SizeOf(DoubleByteChar));

  for L := Low(L) to Pred(Low(TLeadByte)) do
  begin
    W := FSingleByteMap[L];
    if W >= FWideCharMapLo then
      FWideCharMap[Word(W) - Word(FWideCharMapLo)].SingleByte := L;
  end;

  for L := Low(TLeadByte) to High(TLeadByte) do
  begin
    P := FDoubleByteMap[L];
    if P <> nil then
    begin
      for C := Low(C) to High(C) do
      begin
        W := P[C];
        if W >= FWideCharMapLo then
          Word(FWideCharMap[Word(W) - Word(FWideCharMapLo)]) := Byte(L) or (Byte(C) shl 8);
      end;
    end
    else
    begin
      W := FSingleByteMap[L];
      if W >= FWideCharMapLo then
        FWideCharMap[Word(W) - Word(FWideCharMapLo)].SingleByte := L;
    end;
  end
end;

destructor TDoubleByteCodePage.Destroy;
var
  L: TLeadByte;
begin
  FreeMem(FWideCharMap);
  for L := Low(L) to High(L) do
    FreeMem(FDoubleByteMap[L]);
  inherited;
end;

class function TDoubleByteCodePage.MaxCharBytes: Byte;
begin
  Result := SizeOf(DoubleByteChar);
end;

function TDoubleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TDoubleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TDoubleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

{$IFDEF UTF32}
function TDoubleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextQuadChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;

function TDoubleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): TNextLegacyChar;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
end;
{$ENDIF}

{ TCodePagesNode }

destructor TCodePagesNode.Destroy;
begin
  FCodePage.Free;
  inherited;
end;

function TCodePagesNode.Compare(Item: TBalancedTreeNode): Integer;
begin
  Result := 0;
  if TCodePagesNode(Item).CodePage.Number < FCodePage.Number then
    Dec(Result)
  else if TCodePagesNode(Item).CodePage.Number > FCodePage.Number then
    Inc(Result);
end;

{ TCodePages }

constructor TCodePages.Create(NewCodePageFunc: TNewCodePageFunc);
begin
  FNewCodePageFunc := NewCodePageFunc;
end;

function TCodePages.GetItem(CodePage: Word): TCodePage;
var
  Item: TCodePagesNode;
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

  Item := TCodePagesNode.Create;
  Item.FCodePage := FNewCodePageFunc(CodePage);
  Insert(Item);
  Result := Item.CodePage;
end;

end.

