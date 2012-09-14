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
{
  soDetectCharSet:
    * Legacy source: try to decode source as UTF-8, continue as code page or Latin1 if code page is null
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

  TStringSource = TLegacySource;

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

//  coUTF8 = []; // that's by default
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
  TEncodeUTF16 = set of coForceInvalid..coSurrogates;
  TEncodeUTF32 = set of coForceInvalid..coBigEndian;

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
  TCodePage = class;

  TInvalidUTF8 = packed record
  case Integer of
    0: (StartingByte: Byte);         // Bad UTF-8 sequence starting with byte $%02X
    1: (ByteCount, ByteIndex: Byte); // Broken %u-byte UTF-8 sequence at %s byte
  end;

  PStringInfo = ^TStringInfo;
  TStringInfo = record
    Count, CharCount: Cardinal;
    CodePage: TCodePage;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (Latin1Count: Cardinal;
          InvalidChar: QuadChar);
      1: (DoubleByteCount: Cardinal; InvalidUTF8: TInvalidUTF8);
      2: (SurrogateCount, InvalidCount: Cardinal);
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

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromLegacy(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function ToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PLegacyChar; CodePage: TCodePage = nil; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload; virtual; abstract;
    function ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; overload;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; overload;

    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload; virtual; abstract;
    function ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
      Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; overload;
  {$ENDIF}
  // properties
  {$IFNDEF Lite}
    property Blocks: TUnicodeBlocks read FBlocks;
  {$ENDIF}
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
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
    constructor Create(var Info: TCPInfoEx); overload;
    destructor Destroy; override;
  // properties
    property WideMapCount: Word read GetWideMapCount;
    property WideMapLo: WideChar read FWideMapLo;
    property WideMapHi: WideChar read FWideMapHi;
  end;

  TSingleByteCodePage = class(TMemoryCodePage)
  private
    FWideMap: PLegacyChar;
  public
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;
    function IsEBCDIC: Boolean;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; override;
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
    constructor Create(var Info: TCPInfoEx);
    destructor Destroy; override;

    class function MaxCharBytes: Byte; override;

    function FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;

    function FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Cardinal; override;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): Cardinal; override;
    function ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
      SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32 = []): Cardinal; override;
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
  protected
    procedure CheckRange(Index: Integer; Count: Cardinal);
  public
    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    function ByteCount: Cardinal; overload; 

    class function Length(Source: Pointer): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; virtual; abstract;
  // properties
    property Language: Word read FLanguage write FLanguage;
  {$ENDIF}
  end;

  TLegacySubstring = class(TSubstring)
  private
    FData: PLegacyChar;
    FOptions: TLegacyOptions;
    FCodePage: TCodePage;
  public
  // properties
    property CodePage: TCodePage read FCodePage;
    property Data: PLegacyChar read FData;
    property Options: TLegacyOptions read FOptions;
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
  //  { placeholder }  FData: Pointer;
  //  { placeholder }  FOptions: TStringOptions;
    procedure SetCount(Value: Cardinal);
  protected
    function AcceptRange(var Index: Integer; Count: Cardinal): Boolean;

    procedure DoAssign(Value: Pointer; Count: Cardinal; Index: Integer; Options: TStringOptions); overload;

    function DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload;

    function DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload;

  {$IFDEF UTF32}
    function DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload;
  {$ENDIF}

    procedure DoClear; override;
    procedure DoSetCount(Value: Cardinal);
  public
    constructor Create(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;

    constructor Create(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;

  {$IFDEF UTF32}
    constructor Create(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;
  {$ENDIF}

    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;

    function Assign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;

  {$IFDEF UTF32}
    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
  {$ENDIF}
  // properties
    property Count write SetCount;
  end;

  TLegacyString = class(TString)
  private
    FData: PLegacyChar;
    FOptions: TLegacyOptions;
    FCodePage: TCodePage;
    procedure SetData(Value: PLegacyChar);
  protected
    function DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
    function DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
  {$IFDEF UTF32}
    function DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PLegacyChar read FData write SetData;    
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
    function DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
    function DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
  {$IFDEF UTF32}
    function DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
  {$ENDIF}
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
    function DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;
    function DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; override;

    function DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
  {$IFDEF UTF32} override; {$ELSE} overload;
    function DoAssign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload;
  {$ENDIF}

  {$IFNDEF Lite}
    procedure DoSwapByteOrder; override;
  {$ENDIF}
  public
  {$IFNDEF UTF32}
    constructor Create(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
      SourceOptions: TEndianSource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;
    constructor Create(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []); overload;

    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
    function Assign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
  {$ENDIF}

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

{ Exceptions }

  ECodePage = class(Exception)
  private
    FName: PCoreChar;
    FMaxCharBytes: Byte;
  public
    constructor Create(var Info: TCPInfoEx); overload;
    constructor Create(var Info: TCPInfoEx; CodePage: TObject); overload;
  // properties
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
  // properties
    property InvalidChar: QuadChar read FInvalidChar;
  {$IFNDEF Lite}
    property Block: TCharBlock read FBlock;
  {$ENDIF}
  end;

  EConvert = class(EChar)
  private
    FSourceSite, FDestSite: TConvertSite;
    function CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCharSet): Boolean; overload;
    function CatchInvalidChar(InvalidChar: QuadChar; DestSite: TCodePage): Boolean; overload;
  public
    constructor Create(InvalidChar: QuadChar; SourceSite, DestSite: TCharSet); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCodePage; DestSite: TCharSet); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCharSet; DestSite: TCodePage); overload;
    constructor Create(InvalidChar: QuadChar; SourceSite: TCodePage; DestSite: TCodePage); overload;
  // properties
    property DestSite: TConvertSite read FDestSite;
    property SourceSite: TConvertSite read FSourceSite;
  end;

  ESurrogates = class(EChar)
  public
    constructor Create(InvalidChar: QuadChar);
  end;

  EUTF8 = class(EString);

  EBadUTF8 = class(EUTF8)
  private
    FInvalidByte: Byte;
  public
    constructor Create(InvalidByte: Byte);
  // properties
    property InvalidByte: Byte read FInvalidByte;
  end;

  EBrokenUTF8 = class(EUTF8)
  private
    FSequenceBytes, FBrokenByte: Byte;
  public
    constructor Create(SequenceBytes, BrokenByte: Byte);
  // properties
    property BrokenByte: Byte read FBrokenByte;
    property SequenceBytes: Byte read FSequenceBytes;
  end;

const
  LeadBytes = [Low(TLeadByte)..High(TLeadByte)];

{ Core services }

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Cardinal; Dest: PQuadChar);
procedure SwapWideCharBytes(Source: PWideChar; Count: Cardinal; Dest: PWideChar);

function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource;
function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource;

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy = []): Cardinal;
function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16 = []): Cardinal;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;

function PlatformCodePage(CodePage: Word = CP_ACP): TCodePage;

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

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy): Cardinal;
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
    Len: Cardinal;
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
        CoreChar(0)..CoreChar(32), CoreChar('0')..CoreChar('9'):;
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

function PlatformCodePage(CodePage: Word): TCodePage;
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
    2:
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
    end;
  end;

  with Info do
    raise ECodePage.Create(Info);
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

{ ESurrogates }

constructor ESurrogates.Create(InvalidChar: QuadChar);
begin
  // TODO
end;

{ EBadUTF8 }

constructor EBadUTF8.Create(InvalidByte: Byte);
begin
  inherited Create(sBadUTF8, [InvalidByte]);
  FInvalidByte := InvalidByte;
end;

{ EBrokenUTF8 }

constructor EBrokenUTF8.Create(SequenceBytes, BrokenByte: Byte);
const
  Bytes: array[2..5] of PLegacyChar = (sSecond, sThird, sFourth, sFifth);
begin
  inherited Create(sBrokenUTF8, [SequenceBytes, Bytes[BrokenByte]]);
  FSequenceBytes := SequenceBytes;
  FBrokenByte := BrokenByte;
end;

{ TCodePage }

function TCodePage.FromLegacy(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromLegacy(Info, Source, Count, CodePage, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, TCharSet(soLatin1 in SourceOptions), Self);
  Result := Info.Count;
end;

function TCodePage.ToLegacy(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; CodePage: TCodePage; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToLegacy(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, Self, TCharSet(coLatin1 in DestOptions));
  Result := Info.Count;
end;

function TCodePage.FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, csUTF16, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF16(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, Self, csUTF16);
  Result := Info.Count;
end;

{$IFDEF UTF32}
function TCodePage.FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (FromUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, csUTF32, Self);
  Result := Info.Count;
end;

function TCodePage.ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  if (ToUTF32(Info, Source, Count, SourceOptions, Dest, DestOptions
    {$IFNDEF Lite} - [coRangeBlocks] {$ENDIF}) = 0) and (Info.InvalidChar <> 0)
  then
    raise EConvert.Create(Info.InvalidChar, Self, csUTF32);
  Result := Info.Count;
end;
{$ENDIF}

{ TMemoryCodePage }

constructor TMemoryCodePage.Create(var Info: TCPInfoEx);
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

function TSingleByteCodePage.FromLegacy(var Info: TStringInfo; Source: PLegacyChar;
  Count: Cardinal; CodePage: TCodePage; SourceOptions: TLegacySource;
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
  begin  // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := FromUTF8(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

function TSingleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
  begin // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToLegacy(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

function TSingleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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

function TSingleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
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
  begin  // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

{$IFDEF UTF32}
function TSingleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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

function TSingleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
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
  begin // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

function TDoubleByteCodePage.FromLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  CodePage: TCodePage; SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
  begin  // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := FromUTF8(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

function TDoubleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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
  begin // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToLegacy(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

function TDoubleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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

function TDoubleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Cardinal;
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
  begin // TODO
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF16(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

{$IFDEF UTF32}
function TDoubleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Cardinal;
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

function TDoubleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Cardinal;
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
    {if soDetectCharSet in SourceOptions then
    begin
      Uni := Info;
      Result := UTF8ToUTF32(Uni, Source, Count, [], Dest, DestOptions);
      if Result <> 0 then
      begin
        Info := Uni;
        Exit;
      end;
    end;}

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

{ TSubstring }

procedure TSubstring.CheckRange(Index: Integer; Count: Cardinal);
var
  Offset: Cardinal;
begin
  if Index < 0 then
    Offset := FCount - Cardinal(-Index) - 1
  else
    Offset := Index;

  if (Offset >= FCount) or (Offset + Count >= FCount) then
    raise ERange.Create(Self, Index, Count, 0, FCount - 1);
end;

function TSubstring.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

{ TString }

constructor TString.Create(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  CodePage: TCodePage; SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Info, Source, Count, CodePage, SourceOptions, Index, DestOptions);
end;

constructor TString.Create(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Source, Count, CodePage, SourceOptions, Index, DestOptions);
end;

constructor TString.Create(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
end;

constructor TString.Create(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Source, Count, SourceOptions, Index, DestOptions);
end;

constructor {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Create(
  var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
end;

constructor {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Create(
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions);
begin
  AcceptRange(Index, Count);
  DoAssign(Source, Count, SourceOptions, Index, DestOptions);
end;

function TString.AcceptRange(var Index: Integer; Count: Cardinal): Boolean;
begin
  if Index < 0 then
    Cardinal(Index) := FCount - Cardinal(-Index) - 1;
  Inc(Count, Cardinal(Index));
  if Count > FCount then
    DoSetCount(Count);
end;

procedure TString.DoClear;
begin
  if not (soAttachBuffer in TLegacyString(Self).FOptions) then
    FreeMem(TLegacyString(Self).FData);
  TLegacyString(Self).FData := nil;
  FCount := 0;
end;

procedure TString.DoSetCount(Value: Cardinal);
var
  Buf: PLegacyChar;
  Cnt: Cardinal;
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

  FCount := Value;
end;

procedure TString.DoAssign(Value: Pointer; Count: Cardinal; Index: Integer;
  Options: TStringOptions);
var
  Cnt, Idx: Cardinal;
begin
  if (soAttachBuffer in Options) and (Index = 0) then
  begin
    if not (soAttachBuffer in TLegacyString(Self).FOptions) then
      FreeMem(TLegacyString(Self).FData);
    TLegacyString(Self).FData := Value;
    FCount := Count;
    TLegacyString(Self).FOptions := Options;
  end
  else
  begin
    if AcceptRange(Index, Count) then
      FillChar(TLegacyString(Self).FData[ByteCount(Cardinal(Index) + Count)], ByteCount(1), 0);
    Move(Value^, TLegacyString(Self).FData[ByteCount(Index)], ByteCount(Count));
    TLegacyString(Self).FOptions := Options - [soAttachBuffer];
  end;
end;

function TString.DoAssign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := DoAssign(Info, Source, Count, CodePage, SourceOptions, Index, DestOptions);
  if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
    if CodePage <> nil then
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

function TString.DoAssign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
  if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
    if Info.CodePage <> nil then
      raise EConvert.Create(Info.InvalidChar, csUTF16, Info.CodePage)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF16, TCharSet(coLatin1 in DestOptions));
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .DoAssign(
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
  if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
    if Info.CodePage <> nil then
      raise EConvert.Create(Info.InvalidChar, csUTF32, Info.CodePage)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF32, TCharSet(coLatin1 in DestOptions));
end;

function TString.Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Info, Source, Count, CodePage, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

function TString.Assign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Source, Count, CodePage, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

function TString.Assign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

function TString.Assign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Source, Count, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Assign(
  var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Info, Source, Count, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Assign(
  Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  BeginUpdate;
  try
    AcceptRange(Index, Count);
    Result := DoAssign(Source, Count, SourceOptions, Index, DestOptions);
  finally
    EndUpdate;
  end;
end;

procedure TString.SetCount(Value: Cardinal);
begin
  BeginUpdate;
  try
    DoSetCount(Value);
  finally
    EndUpdate;
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

function TLegacyString.DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TLegacyString.DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TLegacyString.DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;
{$ENDIF}

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), FCodePage);
end;

{ TEndianString }

{$IFNDEF Lite}
procedure TEndianString.SwapByteOrder;
begin
  BeginUpdate;
  try
    DoSwapByteOrder;
    with TWideString(Self) do
      Byte(FOptions) := Byte(FOptions) xor Byte(soBigEndian);
  finally
    EndUpdate;
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
  SwapWideCharBytes(FData, FCount, FData);
end;
{$ENDIF}

function TWideString.DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TWideString.DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TWideString.DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
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

procedure TQuadString.DoSwapByteOrder;
begin
  SwapQuadCharBytes(FData, FCount, FData);
end;
{$ENDIF}

function TQuadString.DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TQuadString.DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TQuadString.DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

procedure TQuadString.SetData(Value: PQuadChar);
begin
  Assign(Value, QuadStrLen(Value));
end;

end.

