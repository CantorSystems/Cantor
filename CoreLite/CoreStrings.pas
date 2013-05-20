(*
    Lite Core Library (CoreLite)

    Core string and character set implementation

    Copyright (c) 2012-2013 Vladislav Javadov (Freeman)

    TODO (first is more important):
      * Unicode NFC/NFD/NFKC/NFKD, UTF-7, SCSU, (BOCU is not patent-free)
      * Non-Unicode GB18030, ISO-2022 (also TRON?)

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
  Windows, CoreUtils, CoreExceptions, CoreClasses, CoreConsts;

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
    coContinuous, coForceInvalid, coRangeBlocks, coBigEndian, coSurrogates);

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

  PStringInfo = ^TStringInfo;
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
          SurrogateCount: Integer);
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

  TDoubleByteCodePage = class(TMemoryCodePage)
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
    procedure CheckRange(Index, Count: Integer);
  public
    {class} function ByteCount(Count: Integer): Integer; overload; virtual; abstract;
    function ByteCount: Integer; overload;

    {class} function Length(Source: Pointer): Integer; overload; virtual; abstract;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; overload; virtual; abstract;
    property Language: Word read FLanguage write FLanguage;
  {$ENDIF}
  end;

  TLegacySubstring = class(TSubstring)
  private
    FData: PLegacyChar;
    FOptions: TLegacyOptions;
    FCodePage: TCodePage;
  public
    property CodePage: TCodePage read FCodePage;
    property Data: PLegacyChar read FData;
    property Options: TLegacyOptions read FOptions;
  end;

  TWideSubstring = class(TSubstring)
  private
    FData: PWideChar;
    FOptions: TEndianOptions;
  public
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
    procedure SetCount(Value: Integer);
  public
    procedure AcceptRange(var Index, Count: Integer);
    procedure Clear; override;

    procedure Assign(Value: Pointer; Count, Index: Integer; Options: TStringOptions = []); overload;

    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Assign(Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;

    function Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Assign(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;

  {$IFDEF UTF32}
    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload; virtual; abstract;
    function Assign(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$ENDIF}

    property Count write SetCount;
  end;

  TLegacyString = class(TString)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TLegacyOptions;
    FCodePage: TCodePage;
    procedure SetData(Value: PLegacyChar);
  public
    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFDEF UTF32}
    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$ENDIF}
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
  {$ENDIF}
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
    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFDEF UTF32}
    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$ENDIF}
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    procedure SwapByteOrder; override;
  {$ENDIF}
    property Data: PWideChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadString = class(TEndianString)
  private
  { hold } FData: PQuadChar;
  { hold } FOptions: TEndianOptions;
    procedure SetData(Value: PQuadChar);
  public
    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
    function Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; override;
  {$IFNDEF UTF32}
    function Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
    function Assign(Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$ENDIF}
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    procedure SwapByteOrder; override;
  {$ENDIF}
    property Data: PQuadChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TCoreString = TWideString; // TODO: non-Unicode

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
    procedure Clear; override;
    {class} function Length(Source: Pointer): Integer; overload; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; overload; override;
  {$ENDIF}

    property AsCoreChar: PWideChar read GetAsWideChar write SetAsWideChar; // TODO: non-Unicode
    property AsLegacyChar: PLegacyChar read GetAsLegacyChar write SetAsLegacyChar;
    property AsWideChar: PWideChar read GetAsWideChar write SetAsWideChar;
  {$IFDEF UTF32}
    property AsQuadChar: PQuadChar read GetAsQuadChar write SetAsQuadChar;
  {$ENDIF}
    property Data: Pointer read FData;
    property Options: TStringOptions read FOptions;
    property Parent: TString read FParent;
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

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Integer; Dest: PQuadChar);
procedure SwapWideCharBytes(Source: PWideChar; Count: Integer; Dest: PWideChar);

function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource;
function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource;

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy = []): Integer;
function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16 = []): Integer;

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbNonUnicode): TCharBlock;
function TranslateCodePage(Source: Word): Word;

function PlatformCodePage(CodePage: Word = CP_ACP): TCodePage;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

procedure SwapQuadCharBytes(Source: PQuadChar; Count: Integer; Dest: PQuadChar);
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

procedure SwapWideCharBytes(Source: PWideChar; Count: Integer; Dest: PWideChar);
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

function EstimateLegacy(const Info: TStringInfo; Options: TEncodeLegacy): Integer;
begin
  if coCESU8 in Options then
    Result := (Info.CharCount + Info.SurrogateCount) * 3
  else
    Result := Info.CharCount * 3 + Info.SurrogateCount;
end;

function EstimateUTF16(const Info: TStringInfo; Options: TEncodeUTF16): Integer;
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

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;

var
  Index: Integer;

function GetChar: QuadChar; // Fast core: closure
var
  FirstByte, Bytes, B, C: Byte;
begin
  FirstByte := Byte(Source[Index]);
  Inc(Index);

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
        Result := 0; // to avoid warning
        Bytes := 0;
      end;

      B := Bytes;

      while (B <> 0) and (Index < Count) do
      begin
        C := Byte(Source[Index]);
        if C and $C0 = $80 then
        begin
          Result := (Result shl 6) or (C and $3F);
          Inc(Index);
          Dec(B);
        end
        else
          Break; // broken sequence
      end;

      if B <> 0 then // broken sequence or unexpected end of string
        if (Index = Count) and (coContinuous in DestOptions) then
          Result := InvalidUTFMask
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
  Result := 0;
  Index := 0;
  T := 0;
{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}

  while Index < Count do
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
                    Inc(Result, 2);

                    with Info do
                    begin
                      Inc(CharCount);
                      Inc(SurrogateCount);
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
            Inc(Result, 2);
            Inc(Info.CharCount);
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
          Inc(Count, Result);
        end;

        Result := 0;
        Exit;
      end;

    if coBigEndian in DestOptions then
      Dest^ := WideChar(Swap(Q))
    else
      Dest^ := WideChar(Q);

    Inc(Dest);
    Inc(Result);
    Inc(Info.CharCount);
  end;

  Inc(Info.Count, Result);
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

type
  TWideCharBuf = array[0..{20}47] of WideChar;
const
  BufLen = High(TWideCharBuf) - Low(TWideCharBuf) + 1;

function TSingleByteCodePage.FromLegacy(var Info: TStringInfo; Source: PLegacyChar;
  Count: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  Cnt, I, Idx, W: Integer;
  C, D: LegacyChar;
  Inf: TStringInfo;
  Buf: TWideCharBuf;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TSingleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
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
  if soDetectCharSet in SourceOptions then
    SourceOptions := DetectUTF16(Source, SourceOptions);

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
    Result := FromUTF8(Inf, Source, Count, Dest, DestOptions);
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
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TSingleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Integer;
var
  I: Integer;
  C: LegacyChar;
  Q: QuadChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
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
var
  I: Integer;
  C: LegacyChar;
  D: DoubleByteChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToLegacy(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  I: Integer;
  C: LegacyChar;
  W: WideChar;
  Uni: TStringInfo;
  T: PTrailByteMap;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.FromUTF16(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  I: Integer;
  C: LegacyChar;
  W, L: WideChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToUTF16(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;
var
  I: Integer;
  C: LegacyChar;
  T: PTrailByteMap;
  W: WideChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TDoubleByteCodePage.FromUTF32(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): Integer;
var
  I: Integer;
  D: DoubleByteChar;
  Q: QuadChar;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;

function TDoubleByteCodePage.ToUTF32(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PQuadChar; DestOptions: TEncodeUTF32): Integer;
var
  I: Integer;
  C: LegacyChar;
  T: PTrailByteMap;
  Q: QuadChar;
  Uni: TStringInfo;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0; // TODO
end;
{$ENDIF}

{ TSubstring }

function TSubstring.ByteCount: Integer;
begin
  Result := ByteCount(FCount);
end;

procedure TSubstring.CheckRange(Index, Count: Integer);
var
  Offset: Integer;
begin
  if Index < 0 then
    Offset := FCount + Index - 1
  else
    Offset := Index;

  if (Offset >= FCount) or (Offset + Count >= FCount) then
    raise ERange.Create(Self, Index, Count, 0, FCount - 1);
end;

{ TString }

procedure TString.AcceptRange(var Index, Count: Integer);
begin
  if Index < 0 then
    Index := FCount + Index - 1;
  Inc(Count, Index);
  if Count > FCount then
    SetCount(Count);
end;

procedure TString.Assign(Value: Pointer; Count: Integer; Index: Integer;
  Options: TStringOptions);
var
  Cnt, Idx: Integer;
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
    AcceptRange(Index, Count);
    Move(Value^, TLegacyString(Self).FData[ByteCount(Index)], ByteCount(Count));
    TLegacyString(Self).FOptions := Options - [soAttachBuffer];
  end;
end;

function TString.Assign(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := Assign(Info, Source, Count, CodePage, SourceOptions, Index, DestOptions);
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

function TString.Assign(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := Assign(Info, Source, Count, SourceOptions, Index, DestOptions);
  if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else if Info.CodePage <> nil then
      raise EConvert.Create(Info.InvalidChar, csUTF16, Info.CodePage)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF16, TCharSet(coLatin1 in DestOptions));
end;

function {$IFDEF UTF32} TString {$ELSE} TQuadString {$ENDIF} .Assign(
  Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Integer;
var
  Info: TStringInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := Assign(Info, Source, Count, SourceOptions, Index, DestOptions);
  if (Result = 0) and not (coForceInvalid in DestOptions) and (Info.InvalidChar <> 0) then
    if Info.InvalidChar and InvalidUTFMask <> 0 then
      raise EUTF.Create(Info.InvalidUTF)
    else if Info.CodePage <> nil then
      raise EConvert.Create(Info.InvalidChar, csUTF32, Info.CodePage)
    else
      raise EConvert.Create(Info.InvalidChar, csUTF32, TCharSet(coLatin1 in DestOptions));
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
{$ENDIF}

function TLegacyString.Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TLegacyString.Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TLegacyString.Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF}

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), FCodePage);
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

function TWideString.Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TWideString.Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TWideString.DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;
{$ENDIF}

procedure TWideString.SetData(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value));
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
{$ENDIF}

function TQuadString.Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TQuadString.Assign(var Info: TStringInfo; Source: PWideChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

function TQuadString.Assign(var Info: TStringInfo; Source: PQuadChar; Count: Integer;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Integer;
begin
  Result := 0; // TODO
end;

procedure TQuadString.SetData(Value: PQuadChar);
begin
  Assign(Value, QuadStrLen(Value));
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

end.

