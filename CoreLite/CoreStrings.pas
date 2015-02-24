(*
    Lite Core Library (CoreLite mini)

    Core strings and character sets implementation

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- some debugging features and diagnostic exceptions
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
  TRawByteOptions = set of soDetectCharSet..soLatin1;
  TEndianOptions = set of soBigEndian..soBigEndian;

  TStringSource = set of TStringOption;
  TRawByteSource = set of soDetectCharSet..soAttachBuffer;
  TEndianSource = set of soBigEndian..soAttachBuffer;
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

  TLeadBytes = set of TLeadByte;
  TTrailBytes = set of TTrailByte;

  TSurrogatePair = packed record
    HighSurrogate, LowSurrogate: WideChar; // always big-endian
  end;

  TInvalidChar = record // platform
    case Byte of
      0: (Value: QuadChar);
      1: (BrokenSequence,       // Broken %u-byte UTF-8 sequence starting with byte $%02X
          StartingByte: Byte;   // Bad UTF-8 sequence starting with byte $%02X
          UTF8Mask: Word);
  end;

{  TNextLegacyChar = record
    SourceCount, DestCount, SuccessBytes: Integer; // SuccessBytes < 0 for Latin1
    InvalidChar: TInvalidChar;
  end;

  TNextEndianChar = record
    SourceCount, DestCount: Integer;
    InvalidChar: TInvalidChar;
  end;

  TNextWideChar = TNextEndianChar;
  TNextQuadChar = TNextEndianChar;}

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

{    function Decode(Source: PLegacyChar; SourceCount: Integer; CodePage: TCodePage; SourceOptions: TLegacySource;
      Dest: PLegacyChar; DestCount: Integer; DestOptions: TEncodeLegacy = []): TNextLegacyChar; overload;
    function Decode(Source: PWideChar; SourceCount: Integer; SourceOptions: TEndianSource;
      Dest: PLegacyChar; DestCount: Integer; DestOptions: TEncodeLegacy = []): TNextWideChar; overload;

    function Encode(Source: PLegacyChar; SourceCount: Integer; SourceOptions: TLegacySource;
      Dest: PWideChar; DestCount: Integer; DestOptions: TEncodeUTF16 = []): TNextLegacyChar;}

    property LeadBytes: TLeadBytes read FLeadBytes;
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
  end;

  TRawOptions = TRawByteOptions;
  TStringData = record
    case Byte of
      0: (RawString: Pointer; { hold to TCollection! }
          RawOptions: TRawByteOptions);
      1: (LegacyString: PLegacyChar;
          LegacyStringOptions: TRawByteOptions;
          CodePage: PCodePage);
      2: (WideString: PWideChar;
          WideStringOptions: TEndianOptions);
  end;

  PString = ^TString;
  TString = object(TCollection)
  protected // prevent stupid warnings
    FData: TStringData;
  public
    procedure Assign(Source: Pointer; Options: TStringSource = [];
      CP: PCodePage = nil); overload;
    procedure Assign(Source: Pointer; Length: Integer; Options: TStringSource = [];
      CP: PCodePage = nil); overload;
    class function LengthOf(Source: Pointer): Integer; virtual; abstract;
    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number

    property RawOptions: TRawByteOptions read FData.RawOptions;
    property RawString: Pointer read FData.RawString;
  end;

  PLegacyString = ^TLegacyString;
  TLegacyString = object(TString)
  private
    function GetString: PLegacyChar;
    procedure SetString(Value: PLegacyChar);
  public
    constructor Create; overload;
    class function LengthOf(Source: Pointer): Integer; virtual;

    property AsString: PLegacyChar read GetString write SetString;
    property CodePage: PCodePage read FData.CodePage;
    property Data: PLegacyChar read FData.LegacyString write SetString;
    property Options: TRawByteOptions read FData.LegacyStringOptions;
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
    function GetString: PWideChar;
    procedure SetString(Value: PWideChar);
  public
    constructor Create; overload;
    class function LengthOf(Source: Pointer): Integer; virtual;

    property AsString: PWideChar read GetString write SetString;
    property Data: PWideChar read FData.WideString write SetString;
    property Options: TEndianOptions read FData.WideStringOptions;
  end;

  PCoreString = PWideString;
  TCoreString = TWideString;

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

{ TString }

procedure TString.Assign(Source: Pointer; Options: TStringSource; CP: PCodePage);
begin
  Assign(Source, LengthOf(Source), Options, CP);
end;

procedure TString.Assign(Source: Pointer; Length: Integer; Options: TStringSource;
  CP: PCodePage);
begin
  if Source <> nil then
    if soAttachBuffer in Options then
      inherited Assign(Source, Length, Length + 1, True)
    else
    begin
      inherited Assign(Source, Length, Length + SizeOf(WideChar), False);
      PWideChar(FData.LegacyString + Length * ItemSize)^ := #0;
    end
  else
    Clear;
  FData.RawOptions := Options;
  FData.CodePage := CP;
end;

function TString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

{ TLegacyString }

constructor TLegacyString.Create;
begin
  inherited Create(sLegacyString, SizeOf(LegacyChar));
end;

function TLegacyString.GetString: PLegacyChar;
begin
  Capacity := Capacity; // detach buffer
  FData.LegacyString[Count] := #0;
  Result := FData.LegacyString;
end;

class function TLegacyString.LengthOf(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;

procedure TLegacyString.SetString(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), [soDetectCharSet]);
end;

{ TEndianString }

procedure TEndianString.SwapByteOrder(Index, Length: Integer);
var
  W: PWideChar;
begin
  CheckRange(@Self, Index, Index + Length - 1);
  W := FData.WideString + Index;
  SwapWideCharBytes(W, Length, W);
  with FData do
    Byte(WideStringOptions) := Byte(WideStringOptions) xor (1 shl Byte(soBigEndian));
end;

procedure TEndianString.SwapByteOrder;
begin
  SwapByteOrder(0, Count);
end;

{ TWideString }

constructor TWideString.Create;
begin
  inherited Create(sWideString, SizeOf(WideChar));
end;

function TWideString.GetString: PWideChar;
begin
  Capacity := Capacity; // detach buffer
  FData.WideString[Count] := #0;
  Result := FData.WideString;
end;

class function TWideString.LengthOf(Source: Pointer): Integer;
begin
  Result := WideStrLen(Source);
end;

procedure TWideString.SetString(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value));
end;

end.

