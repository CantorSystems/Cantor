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
  TEndianSource = set of soBigEndian..soDetectCharSet;

  TConvertOption = (coPunctuation, coKana, coCase, coTurkic, coNonSpace, coWidth,
    coHanzi, coDiacritics, coComposition, coForceInvalid, coRangeBlocks,
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
  TEncodeLegacy = set of coCase..coRangeBlocks;
  TEncodeLatin = set of coCase..coLatin1;
  TEncodeEndian = set of coComposition..coBigEndian;
  TEncodeUTF8 = set of coComposition..coEncodedZero;
  TUTF8Compliance = set of coCESU8..coEncodedZero;

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
    FInvalidChar: QuadChar;
  public
    constructor Create(DestSite, SourceSite: TCharSet; Source: Pointer;
      InvalidChar: QuadChar); overload;
    constructor Create(DestSite: TCodePage; SourceSite: TCharSet; Source: Pointer;
      InvalidChar: QuadChar); overload;
    constructor Create(DestSite: TCharSet; SourceSite: TCodePage; Source: Pointer;
      InvalidChar: QuadChar); overload;
    constructor Create(DestSite, SourceSite: TCodePage; Source: Pointer;
      InvalidChar: QuadChar); overload;
  // properties
    property DestSite: TConvertSite read FDestSite;
    property SourceSite: TConvertSite read FSourceSite;
    property Source: Pointer read FSource;
    property InvalidChar: QuadChar read FInvalidChar;
  end;

  ECodePage = class(EString)
  private
    FString: TCodePageString;
  public
    constructor Create(Str: TCodePageString); // has no code page
  // properties
    property Str: TCodePageString read FString;
  end;

{ Core services }

function FindCharBlock(Source: QuadChar; PrevBlock: TCharBlock = cbUnknown): TCharBlock;
function TranslateCodePage(Source: Word): Word;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Core services }

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

constructor EConvert.Create(DestSite, SourceSite: TCharSet; Source: Pointer;
  InvalidChar: QuadChar);
begin
{$IFDEF Lite}
  Create(sInvalidCharSet2, [CharSets[SourceSite], CharSets[DestSite]]);
{$ELSE}
  Create(sInvalidCharSet, [CharBlockNames[FindCharBlock(InvalidChar)], CharSets[DestSite]]);
{$ENDIF}
  FDestSite.CharSet := DestSite;     // SiteType filled with 0 = stCharSet
  FSourceSite.CharSet := SourceSite; // -- " --
  FSource := Source;
  FInvalidChar := InvalidChar;
end;

constructor EConvert.Create(DestSite: TCharSet; SourceSite: TCodePage; Source: Pointer;
  InvalidChar: QuadChar);
begin
{$IFDEF Lite}
  Create(sInvalidCharSet, CP_LEGACY, [SourceSite.Name, CharSets[DestSite]]);
{$ELSE}
  Create(sInvalidCodePage, [CharBlockNames[FindCharBlock(InvalidChar)], CharSets[DestSite]]);
{$ENDIF}
  FDestSite.CharSet := DestSite; // SiteType filled with 0 = stCharSet
  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
  FInvalidChar := InvalidChar;
end;

constructor EConvert.Create(DestSite: TCodePage; SourceSite: TCharSet; Source: Pointer;
  InvalidChar: QuadChar);
begin
{$IFDEF Lite}
  Create(sInvalidCodePage, CP_LEGACY, [CharSets[SourceSite], DestSite.Name]);
{$ELSE}
  Create(sInvalidCodePage, CP_LEGACY, [CharBlockNames[FindCharBlock(InvalidChar)], DestSite.Name]);
{$ENDIF}
  with FDestSite do
  begin
    SiteType := stCodePage;
    CodePage := DestSite;
  end;
  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
  FInvalidChar := InvalidChar;
end;

constructor EConvert.Create(DestSite, SourceSite: TCodePage; Source: Pointer;
  InvalidChar: QuadChar);
begin
{$IFDEF Lite}
  Create(sInvalidCodePage2, CP_LEGACY, [SourceSite.Name, DestSite.Name]);
{$ELSE}
  Create(sInvalidCodePage, CP_LEGACY, [CharBlockNames[FindCharBlock(InvalidChar)], DestSite.Name]);
{$ENDIF}
  with FDestSite do
  begin
    SiteType := stCodePage;
    CodePage := DestSite;
  end;
  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
  FInvalidChar := InvalidChar;
end;

{ ECodePage }

constructor ECodePage.Create(Str: TCodePageString);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Str);
  inherited Create(sNoCodePage, [@ClassName]);
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

