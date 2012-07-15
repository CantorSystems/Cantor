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
  
  PStringInfo = ^TStringInfo;
  TStringInfo = record
    Count: Cardinal;
    CodePage: TCodePage;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (Latin1Count: Cardinal;
          InvalidChar: QuadChar);
      1: (DoubleByteCount: Cardinal);
      2: (SurrogateCount, InvalidCount, CharCount: Cardinal);
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
  public
    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    function ByteCount: Cardinal; overload; virtual;

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
    procedure SetCount(Value: Cardinal);
  protected
    procedure AcceptRange(Index: Integer; Count: Cardinal);
    procedure CheckRange(Index: Integer; Count: Cardinal);

    procedure DoAssign(Value: Pointer; Count: Cardinal; Index: Integer;
      Options: TStringOptions); overload;

    function DoAssign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
      SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
    function DoAssign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
  {$IFDEF UTF32}
    function DoAssign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
      Index: Integer; DestOptions: TEncodeOptions): Cardinal; overload; virtual; abstract;
  {$ENDIF}
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

    function Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource = [];
      Index: Integer = 0; DestOptions: TEncodeOptions = []): Cardinal; overload;
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

{ Exceptions }

  ECodePage = class(Exception)
  private
    FName: PCoreChar;
    FMaxCharBytes: Byte;
  public
    constructor Create(var Info: TCPInfoEx); overload;
    constructor Create(CodePage: TObject; var Info: TCPInfoEx); overload;
  // properties
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
  end;

  TCharSet = (csLatin, csLatin1, csUTF8, csUTF16, csUTF32); // TODO

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
    constructor Create(const Info: TStringInfo; Source: Pointer;
      Dest: TSubstring; DestOptions: TEncodeOptions); overload; // yeah, true!
    constructor Create(Source: Pointer; SourceSite: TCharSet; const Info: TStringInfo; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCharSet; const Info: TStringInfo; DestSite: TCodePage); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage; const Info: TStringInfo; DestSite: TCharSet); overload;
    constructor Create(Source: Pointer; SourceSite: TCodePage; const Info: TStringInfo; DestSite: TCodePage); overload;
  // properties
    property DestSite: TConvertSite read FDestSite;
    property Info: TStringInfo read FInfo;
    property SourceSite: TConvertSite read FSourceSite;
    property Source: Pointer read FSource;
  end;

  TCodePageString = TObject; // TODO

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

function ExtractCodePageName(var Info: TCPInfoEx): PCoreChar;
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
            Result := @CodePageName[I];
            P := Result + WideStrLen(Result, Length(CodePageName) - I) - 1;
            if P^ = CoreChar(')') then
            begin
              Inc(Result);
              P^ := CoreChar(0);
            end;
            Exit;
          end;
      else
        Break;
      end;
    Result := @CodePageName[I];
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
  with Info do
    inherited Create(sUnsupportedCodePage, CP_LEGACY, [CodePage, ExtractCodePageName(Info)]);
end;

constructor ECodePage.Create(CodePage: TObject; var Info: TCPInfoEx);
const
  HasNo: array[Boolean] of PLegacyChar = (nil, sWhitespaceNo);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, CodePage);
  with Info do
    inherited Create(sInvalidCodePageClass, CP_LEGACY, [CodePage, ExtractCodePageName(Info),
      HasNo[LeadByte[0] = 0], ClassName]);
end;

{ EConvert }

const
  CharSets: array [TCharSet] of PLegacyChar = (sLatin, sLatin1, sUTF8, sUTF16, sUTF32);

function EConvert.CatchInvalidChar(const Info: TStringInfo; DestSite: TCharSet): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(Info.InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCharSetChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(Info.InvalidChar), CharSets[DestSite]]);
    Result := True;
  end
  else
    Result := False;
end;

function EConvert.CatchInvalidChar(const Info: TStringInfo; DestSite: TCodePage): Boolean;
var
  Block: TCharBlock;
begin
  Block := FindCharBlock(Info.InvalidChar);
  if Block <> cbNonUnicode then
  begin
    Create(sInvalidCodePageChar, CP_LEGACY, [UnicodeBlockNames[Block],
      WideChar(Info.InvalidChar), DestSite.Number, DestSite.Name]);
    Result := True;
  end
  else
    Result := False;
end;

constructor EConvert.Create(const Info: TStringInfo; Source: Pointer;
  Dest: TSubstring; DestOptions: TEncodeOptions);
begin
  // TODO
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCharSet;
  const Info: TStringInfo; DestSite: TCharSet);
begin
  if (DestSite >= csUTF8) and (Info.SurrogateCount <> 0) then
    Create(sSurrogates, [CharSets[SourceSite]])
  else
  begin
  {$IFDEF Lite}
    Create(sInvalidCharSet2, [CharSets[SourceSite], CharSets[DestSite]]);
  {$ELSE}
    if not CatchInvalidChar(Info, DestSite) then
      Create(sNonUnicode, [CharSets[SourceSite]]);
  {$ENDIF}
  end;

  FInfo := Info;
  FDestSite.CharSet := DestSite;     // SiteType filled with 0 = stCharSet
  FSourceSite.CharSet := SourceSite; // SiteType filled with 0 = stCharSet
  FSource := Source;
end;

constructor EConvert.Create(Source: Pointer; SourceSite: TCodePage;
  const Info: TStringInfo; DestSite: TCharSet);
begin
{$IFNDEF Lite}
  if not CatchInvalidChar(Info, DestSite) then
{$ENDIF}
    Create(sInvalidCharSet, CP_LEGACY, [SourceSite.Number, SourceSite.Name, CharSets[DestSite]]);

  FDestSite.CharSet := DestSite; // SiteType filled with 0 = stCharSet
  FInfo := Info;

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
  FInfo := Info;

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
  FInfo := Info;

  with FSourceSite do
  begin
    SiteType := stCodePage;
    CodePage := SourceSite;
  end;
  FSource := Source;
end;

{ ENoCodePage }

constructor ENoCodePage.Create(Str: TCodePageString);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Str);
  inherited Create(sNoCodePage, [@ClassName]);
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
    raise EConvert.Create(Source, TCharSet(soLatin1 in SourceOptions), Info, Self);
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
    raise EConvert.Create(Source, Self, Info, TCharSet(coLatin1 in DestOptions));
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
    raise EConvert.Create(Source, csUTF16, Info, Self);
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
    raise EConvert.Create(Source, Self, Info, csUTF16);
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
    raise EConvert.Create(Source, csUTF32, Info, Self);
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
    raise EConvert.Create(Source, Self, Info, csUTF32);
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
  FName := WideStrNew(ExtractCodePageName(Info), Length(Info.CodePageName));

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
    raise ECodePage.Create(Self, Info);
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
    raise ECodePage.Create(Self, Info);
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

function TSubstring.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

{ TString }

constructor TString.Create(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  CodePage: TCodePage; SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;

constructor TString.Create(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;

constructor TString.Create(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;

constructor TString.Create(Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;

{$IFDEF UTF32}
constructor TString.Create(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;

constructor TString.Create(Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions);
begin
  // TODO
end;
{$ENDIF}

procedure TString.AcceptRange(Index: Integer; Count: Cardinal);
begin
  // TODO
end;

procedure TString.CheckRange(Index: Integer; Count: Cardinal);
begin
  // TODO
end;

procedure TString.DoAssign(Value: Pointer; Count: Cardinal; Index: Integer;
  Options: TStringOptions);
begin
  // TODO
end;

function TString.Assign(var Info: TStringInfo; Source: PLegacyChar; Count: Cardinal;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.Assign(Source: PLegacyChar; Count: Cardinal; CodePage: TCodePage;
  SourceOptions: TLegacySource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.Assign(var Info: TStringInfo; Source: PWideChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.Assign(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

{$IFDEF UTF32}
function TString.Assign(var Info: TStringInfo; Source: PQuadChar; Count: Cardinal;
  SourceOptions: TEndianSource; Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;

function TString.Assign(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Index: Integer; DestOptions: TEncodeOptions): Cardinal;
begin
  Result := 0; // TODO
end;
{$ENDIF}

procedure TString.SetCount(Value: Cardinal);
begin
  // TODO
end;

end.

