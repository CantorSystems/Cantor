(*
    Lite Core Library (CoreLite mini)

    Core strings and character sets implementation

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- some debugging features and diagnostic exceptions
      * Lite -- remove support of:
        * TString.Length(Source, MaxLength)
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreConsts;

type
{
  soDetectCharSet:
    * try to decode source as UTF-8, continue as code page or Latin1 if code page is null
}
  TStringOption = (soDetectCharSet, soBigEndian, soAttachBuffer);

const
  soLatin1 = soBigEndian;

type
  TLegacyOption = soDetectCharSet..soAttachBuffer;
  TEndianOption = soBigEndian..soAttachBuffer;

  TLegacySource = set of TLegacyOption;
  TEndianSource = set of TEndianOption;

{
  coForceInvalid -- replace invalid characters with:
    * U+007F -- for code page (used instead of U+001A to avoid compatibility issues)
    * U+FFFD -- for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coContinuous, coForceInvalid, coBigEndian, coSurrogates);

const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogates;
  coEncodeZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodeZero];  // UTF-8 compliance

type
  TEncodeOptions = set of coContinuous..coSurrogates;
  TEncodeLegacy = TEncodeOptions;
  TEncodeUTF16 = TEncodeOptions;
//  TEncodeUTF32 = set of coContinuous..coBigEndian;

type
  TLeadByte = #$80..#$FF;
  TTrailByte = #$20..#$FF;

  TSurrogatePair = packed record
    HighSurrogate, LowSurrogate: WideChar; // always big-endian
  end;

  TLeadBytes = set of TLeadByte;
  TTrailBytes = set of TTrailByte;

  TInvalidChar = record // platform
    case Byte of
      0: (Value: QuadChar);
      1: (BrokenSequence,       // Broken %u-byte UTF-8 sequence starting with byte $%02X
          StartingByte: Byte;   // Bad UTF-8 sequence starting with byte $%02X
          UTF8Mask: Word);
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

  PCodePage = ^TCodePage;
  TCodePage = object
  private
    FLeadBytes: TLeadBytes;
    FName: PCoreChar;
    FNumber: Word;
    FMaxCharBytes: Byte;
  public
    constructor Create(CodePage: Word = CP_ACP);
    destructor Destroy;

    function Decode(Source: PLegacyChar; Count: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload;
    function Decode(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestOptions: TEncodeLegacy = []): TNextWideChar; overload;

    function Encode(Source: PLegacyChar; Count: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestOptions: TEncodeUTF16 = []): TNextLegacyChar; overload;

    property LeadBytes: TLeadBytes read FLeadBytes;
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
  end;

{ Substrings }

  TStringData = record
    case Byte of
      0: (LegacyString: PLegacyChar;
          LegacyStringOptions: TLegacySource;
          CodePage: PCodePage);
      1: (WideString: PWideChar;
          WideStringOptions: TEndianSource);
  end;

  PSubstring = ^TSubstring;
  TSubstring = object(TEnumerable)
  protected // prevent stupid warnings
    FData: TStringData;
  end;

  PRawByteSubstring = ^TRawByteSubstring;
  TRawByteSubstring = object(TSubstring)
  public
    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number

    property Data: PLegacyChar read FData.LegacyString;
    property Options: TLegacySource read FData.LegacyStringOptions;
  end;

  PLegacySubstring = ^TLegacySubstring;
  TLegacySubstring = object(TRawByteSubstring)
  public
    property CodePage: PCodePage read FData.CodePage;
  end;

  PWideSubstring = ^TWideSubstring;
  TWideSubstring = object(TSubstring)
  public
    property Data: PWideChar read FData.WideString;
    property Options: TEndianSource read FData.WideStringOptions;
  end;

{ Strings }

  PString = ^TString;
  TString = object(TExpandable)
  protected // prevent stupid warnings
    FData: TStringData;
    procedure SetCount(Value: Integer); virtual;
  public
    procedure Clear; virtual;

    function InsertString(Source: PLegacyChar; Length: Integer; CodePage: PCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0;
      DestOptions: TEncodeLegacy = []): TNextLegacyChar; virtual; abstract;
    function InsertWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeUTF16 = []): TNextWideChar; virtual; abstract;
  end;

  PRawByteString = ^TRawByteString;
  TRawByteString = object(TString)
  private
    procedure SetData(Value: PLegacyChar);
  public
    function InsertString(Source: PLegacyChar; Length: Integer; CodePage: PCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0;
      DestOptions: TEncodeLegacy = []): TNextLegacyChar; virtual;
    function InsertWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeUTF16 = []): TNextWideChar; virtual;

    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number

    property Data: PLegacyChar read FData.LegacyString write SetData;
    property Options: TLegacySource read FData.LegacyStringOptions;
  end;

  PLegacyString = ^TLegacyString;
  TLegacyString = object(TRawByteString)
  public
    property CodePage: PCodePage read FData.CodePage;
  end;

  PEndianString = ^TEndianString;
  TEndianString = object(TString)
  public
    procedure SwapByteOrder(Index, Length: Integer); overload;
    procedure SwapByteOrder; overload;
  end;

  PWideString = ^TWideString;
  TWideString = object(TEndianString)
  private
    procedure SetData(Value: PWideChar);
  public
    function InsertString(Source: PLegacyChar; Length: Integer; CodePage: PCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0;
      DestOptions: TEncodeLegacy = []): TNextLegacyChar; virtual;
    function InsertWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeUTF16 = []): TNextWideChar; virtual;

    property Data: PWideChar read FData.WideString write SetData;
    property Options: TEndianSource read FData.WideStringOptions;
  end;

{ Helper functions }

type
  THighSurrogates = $D800..$DBFF;
  TLowSurrogates  = $DC00..$DFFF;

  TUnicodeBMP = $000000..$00FFFF;  // Basic Multilingual Plane
  TUnicodeSMP = $010000..$01FFFF;  // Supplementary Multilingual Plane
  TUnicodeSIP = $020000..$02FFFF;  // Supplementary Ideographic Plane
  TUnicodeSSP = $0E0000..$0EFFFF;  // Supplementary Special-purpose Plane
  TUnicodePUA = $0F0000..$10FFFF;  // Private Use Area

  TNonUnicode = $110000..$FFFFFFFF;

  TSurrogatePairOptions = set of (poBigEndian, poEncodeBigEndian);

function DecodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;
function EncodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;

function TranslateCodePage(Source: Word): Word;

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

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ Helper functions }

const
  InvalidUTF8Mask   = $80000000;

type
  TLegacyCharBuf = array[0..$3FF] of LegacyChar;
  TWideCharBuf = array[0..$3FF] of WideChar;

function DecodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions): QuadChar;
begin
  if poBigEndian in Options then
    Result := (Swap(Source) - Low(THighSurrogates)) * $400 +
      Low(TUnicodeSMP) + Swap(Source shr 16) - Low(TLowSurrogates)
  else
    Result := (Word(Source) - Low(THighSurrogates)) * $400 +
      Low(TUnicodeSMP) + Word(Source shr 16) - Low(TLowSurrogates);

  if poEncodeBigEndian in Options then
  asm
      MOV EAX, Result
      BSWAP EAX
      MOV Result, EAX
  end;
end;

function EncodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions): QuadChar;
begin
  if poBigEndian in Options then
  asm
      MOV EAX, Source
      BSWAP EAX
      MOV Source, EAX
  end;

  if poEncodeBigEndian in Options then
    Result := (Swap((Low(TLowSurrogates) + Source mod $400)) shl 16) or
      Swap(Low(THighSurrogates) + Source div $400)
  else
    Result := (Word((Low(TLowSurrogates) + Source mod $400)) shl 16) or
      Word(Low(THighSurrogates) + Source div $400);
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
    Str: PCoreChar;
    Len: Integer;
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

{ TCodePage }

constructor TCodePage.Create(CodePage: Word);
var
  Info: TCPInfoEx;
  P: PLegacyChar;
begin
  if not GetCPInfoEx(CodePage, 0, Info) then
    RaiseLastPlatformError {$IFDEF Debug} (sCodePage, CodePage) {$ENDIF} ;

  with Info do
  begin
    FNumber := CodePage;
    FMaxCharBytes := MaxCharSize;

    P := Pointer(@LeadByte);
    while P < P + SizeOf(LeadByte) do
      if (P[0] <> #0) and (P[1] <> #0) then
      begin
        FLeadBytes := FLeadBytes + [P[0]..P[1]];
        Inc(P, 2);
      end
      else
        Break;
  end;

  with ExtractCodePageName(Info) do
    FName := WideStrNew(Str, Len);
end;

destructor TCodePage.Destroy;
begin
  FreeMem(FName);
end;

function TCodePage.Decode(Source: PLegacyChar; Count: Integer; CodePage: TCodePage;
  SourceOptions: TLegacySource; Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextLegacyChar;
begin

end;

function TCodePage.Decode(Source: PWideChar; Count: Integer; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeLegacy): TNextWideChar;
begin

end;

function TCodePage.Encode(Source: PLegacyChar; Count: Integer;
  SourceOptions: TLegacySource; Dest: PWideChar;
  DestOptions: TEncodeUTF16): TNextLegacyChar;
begin

end;

{ TRawByteSubstring }

function TRawByteSubstring.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

{ TString }

procedure TString.Clear;
begin

end;

procedure TString.SetCount(Value: Integer);
begin

end;

{ TRawByteString }

function TRawByteString.InsertString(Source: PLegacyChar; Length: Integer;
  CodePage: PCodePage; SourceOptions: TLegacySource; DestIndex: Integer;
  DestOptions: TEncodeLegacy): TNextLegacyChar;
begin

end;

function TRawByteString.InsertWideString(Source: PWideChar; Length: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer;
  DestOptions: TEncodeUTF16): TNextWideChar;
begin

end;

function TRawByteString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

procedure TRawByteString.SetData(Value: PLegacyChar);
begin

end;

{ TEndianString }

procedure TEndianString.SwapByteOrder(Index, Length: Integer);
begin
  CheckRange(@Self, Index, Index + Length - 1);
  // TODO
end;

procedure TEndianString.SwapByteOrder;
begin
  SwapByteOrder(0, FCount);
end;

{ TWideString }

function TWideString.InsertString(Source: PLegacyChar; Length: Integer;
  CodePage: PCodePage; SourceOptions: TLegacySource; DestIndex: Integer;
  DestOptions: TEncodeLegacy): TNextLegacyChar;
begin

end;

function TWideString.InsertWideString(Source: PWideChar; Length: Integer;
  SourceOptions: TEndianSource; DestIndex: Integer;
  DestOptions: TEncodeUTF16): TNextWideChar;
begin

end;

procedure TWideString.SetData(Value: PWideChar);
begin

end;

end.

