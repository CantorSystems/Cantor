(*
    Lite Core Library (CoreLite mini)

    Core strings and character sets implementation

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * CoreLiteVCL -- TCoreString and UnicodeString types for VCL applications
      * Debug -- diagnostic exceptions
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses;

type
{
  soDetectUTF8:
    * try to decode source as UTF-8, continue as code page or Latin1 if code page is null
}
  TStringOption = (soDetectUTF8, soBigEndian, soAttachBuffer, soNullTerminated);
const
  soLatin1 = soBigEndian;
  soAttach = [soAttachBuffer];
  soBuiltIn = [soAttachBuffer, soNullTerminated];
  soFromTheWild = [soDetectUTF8];

type
  TRawByteOptions = set of soDetectUTF8..soLatin1;
  TEndianOptions = set of soBigEndian..soBigEndian;

  TStringSource = set of TStringOption;
  TRawByteSource = set of soDetectUTF8..soNullTerminated;
  TEndianSource = set of soBigEndian..soNullTerminated;
{
  coReplaceInvalid -- replace invalid characters with:
    * U+007F -- for code page and Latin1 (used instead of U+001A to avoid compatibility issues)
    * U+FFFD -- for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coSurrogatePairs, coBigEndian, coReplaceInvalid, coSysReplacementChar);
const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogatePairs;
  coEncodeZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodeZero];
  coUTF16 = [coSurrogatePairs];

type
//  TConvertRawBytes = coLatin1..coReplaceInvalid; as sub-mode to UTF-8
  TConvertCodePage = coReplaceInvalid..coSysReplacementChar;
  TConvertUTF8 = coSurrogatePairs..coReplaceInvalid;
  TConvertUTF16 = coSurrogatePairs..coReplaceInvalid;
//  TConvertUTF32 = set of coBigEndian..coReplaceInvalid;

  TEncodeCodePage = set of TConvertCodePage;
  TEncodeRawBytes = set of TConvertUTF8;
  TEncodeUTF16 = set of TConvertUTF16;
//  TEncodeUTF32 = TConvertUTF32;

  TEncodeOptions = TEncodeRawBytes;

const
  FirstLeadByte = LegacyChar(#$80); // for Unicode RTL
  LastLeadByte = LegacyChar(#$FF);
type
  TLeadByte = FirstLeadByte..LastLeadByte;
  TLeadBytes = set of TLeadByte;

  TSurrogatePair = packed record
    HighSurrogate, LowSurrogate: WideChar; // always big-endian
  end;

  TInvalidUTF8 = record
    case Byte of
      0: (StartingByte,           // Bad UTF-8 sequence starting with byte $%02X
          BrokenSequence: Byte);  // Broken %u-byte UTF-8 sequence starting with byte $%02X
      1: (RawData: Word);
  end;

  TErrorInfo = record
    case Byte of
      0: (InvalidChar: QuadChar;
          InvalidUTF8: TInvalidUTF8);
      1: (RawData: QuadWord);
  end;

  TConvertResult = record
    SourceCount, DestCount, SuccessBytes, // SuccessBytes < 0 for Latin1
    SurrogatePairs: Integer;
    ErrorInfo: TErrorInfo;
  end;

{$IF defined(CoreLiteVCL) and not UnicodeRTL}
  RawByteString = AnsiString;
  UnicodeString = WideString;
{$IFEND}

  PLegacyString = ^TLegacyString;
  PWideString = ^TWideString;

  TCPInfoEx = packed record
    MaxCharSize: LongWord;
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of LegacyChar;
    LeadByte: array[0..MAX_LEADBYTES - 1] of Byte;
    UnicodeDefaultChar: WideChar;
    CodePage: LongWord;
    CodePageName: array[0..MAX_PATH - 1] of CoreChar;
  end;

  PCodePage = ^TCodePage;
  TCodePage = object
  private
    FName: PCoreChar;
    FNumber: Word;
    FMaxCharBytes: Byte;
    FReplacementChar, FSysReplacementChar: LegacyChar;
    FLeadBytes: TLeadBytes;
    FInfo: TCPInfoEx;
  public
    constructor Create(CodePage: Word = CP_ACP; DefaultReplacementChar: LegacyChar = LegacyReplacementChar);
    function DecodeUTF16(Source: PWideString; Dest: PLegacyString; DestIndex: Integer;
      EncodeOptions: TEncodeCodePage): Integer;
    function EncodeUTF16(Source: PLegacyString; Dest: PWideString; DestIndex: Integer;
      EncodeOptions: TEncodeUTF16): Integer;

    property LeadBytes: TLeadBytes read FLeadBytes;
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
    property ReplacementChar: LegacyChar read FReplacementChar write FReplacementChar;
    property SysRelpacementChar: LegacyChar read FSysReplacementChar;
  end;

  THexadecimal = (hexNone, hexUpperCase, hexLowerCase);

  PString = ^TString;
  TString = object(TCollection)
  private
  { placeholder } // FOptions: TStringOptions;
  protected
    procedure Assign(Source: Pointer; Length: Integer; Options: TStringSource); overload;
    function AssignArray(Index: Integer; const Values: array of const): Integer;
    function AssignInteger(Index: Integer; Value: Int64; MinWidth: Integer = 0;
      Hexadecimal: THexadecimal = hexNone; FillChar: WideChar = #32): Integer;
  {$IFNDEF Lite}
    function AssignString(Index: Integer; Source: PLegacyString;
      EncodeOptions: TEncodeUTF16 = coUTF16): TConvertResult; virtual; abstract;
    function AssignWideString(Index: Integer; Source: PWideString;
      EncodeOptions: TEncodeRawBytes = []): TConvertResult; virtual; abstract;
  {$ENDIF}
    function CharSetName(EncodeOptions: TEncodeOptions): PLegacyChar;
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual; abstract;
    procedure SetRawByteString(Value: RawByteString); virtual; abstract;
    function GetUnicodeString: UnicodeString; virtual; abstract;
    procedure SetUnicodeString(Value: UnicodeString); virtual; abstract;
  {$ENDIF}
  public
    procedure AsArray(const Values: array of const);
    procedure AsInteger(Value: Int64; MinWidth: Integer = 0; Hexadecimal: THexadecimal = hexNone;
      FillChar: WideChar = #32); overload;
    function AsInteger(var Value: Int64; Hexadecimal: Boolean = False): Boolean; overload;
    function AsInteger(Hexadecimal: Boolean = False): Int64; overload;
  {$IFNDEF Lite}
    procedure Detach; virtual; abstract;
    class function LengthOf(Source: Pointer): Integer; virtual; abstract;
  {$ENDIF}
    function Estimate(const Values: array of const): Integer;
  {$IFDEF CoreLiteVCL}
    property AsRawByteString: RawByteString read GetRawByteString write SetRawByteString;
    property AsUnicodeString: UnicodeString read GetUnicodeString write SetUnicodeString;
  {$ENDIF}
  end;

  TNumberFormat = (nfFillChar, nfThousandsSeparator, nfDecimalSeparator);
  TIntegerFormat = nfFillChar..nfThousandsSeparator;

  TLegacyCharNumberFormat = array[TNumberFormat] of LegacyChar;
  TLegacyCharIntegerFormat = array[TIntegerFormat] of LegacyChar;

  TLegacyString = object(TString)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TRawByteOptions;
    FCodePage: PCodePage;
    function GetData: PLegacyChar;
    procedure SetCodePage(Value: PCodePage);
    procedure SetData(Value: PLegacyChar);
  protected
    function AssignString(Index: Integer; Source: PLegacyString;
      EncodeOptions: TEncodeUTF16 = coUTF16): TConvertResult; {$IFNDEF Lite} virtual; {$ENDIF}
    function AssignWideString(Index: Integer; Source: PWideString;
      EncodeOptions: TEncodeRawBytes = []): TConvertResult; {$IFNDEF Lite} virtual; {$ENDIF}
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual;
    procedure SetRawByteString(Value: RawByteString); virtual;
    function GetUnicodeString: UnicodeString; virtual;
    procedure SetUnicodeString(Value: UnicodeString); virtual;
  {$ENDIF}
  public
    constructor Create;

    function AsRange(Index: Integer): TLegacyString; overload;
    function AsRange(Index, MaxCount: Integer): TLegacyString; overload;

    procedure AsString(Source: PLegacyChar; Length: Integer;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsWideString(Source: PWideString; EncodeOptions: TEncodeRawBytes = []);

    function Compare(Value: PLegacyChar; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;
    function Compare(Value: PLegacyString; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;

    procedure Detach; {$IFNDEF Lite} virtual;
    class function LengthOf(Source: Pointer): Integer; virtual; {$ENDIF}
    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number

    function NextIndex(Index: Integer): Integer; overload;
    function NextIndex(Value: LegacyChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Index: Integer): Integer; overload;
    function PrevIndex(Value: LegacyChar): Integer; overload;
    function PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer; overload;

    procedure Load(Source: PReadableStream); overload;
    procedure Load(Source: PReadableStream; AllowBOM: Boolean;
      SourceOptions: TRawByteOptions = soFromTheWild); overload;
    procedure Save(Dest: PWritableStream); overload;
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean); overload;

    property CodePage: PCodePage read FCodePage write SetCodePage;
    property Data: PLegacyChar read GetData write SetData;
    property Options: TRawByteOptions read FOptions;
    property RawData: PLegacyChar read FData write SetData;
  end;

  TByteOrder = (boFromBOM, boLittleEndian, boBigEndian);

  PEndianString = ^TEndianString;
  TEndianString = object(TString)
  public
  //  procedure SwapByteOrder(Index, Length: Integer); overload; virtual; abstract;
  //  procedure SwapByteOrder; overload;
  end;

  TWideCharNumberFormat = array[TNumberFormat] of WideChar;
  TWideCharIntegerFormat = array[TIntegerFormat] of WideChar;

  TWideString = object(TEndianString)
  private
  { hold } FData: PWideChar;
  { hold } FOptions: TEndianOptions;
    FDataSource: PLegacyString;
    function GetData: PWideChar;
    procedure SetData(Value: PWideChar);
  protected
    function AssignString(Index: Integer; Source: PLegacyString;
      EncodeOptions: TEncodeUTF16 = coUTF16): TConvertResult; {$IFNDEF Lite} virtual; {$ENDIF}
    function AssignWideString(Index: Integer; Source: PWideString;
      EncodeOptions: TEncodeRawBytes = []): TConvertResult; {$IFNDEF Lite} virtual; {$ENDIF}
    procedure SwapByteOrder(Index, Length: Integer); overload;
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual;
    procedure SetRawByteString(Value: RawByteString); virtual;
    function GetUnicodeString: UnicodeString; virtual;
    procedure SetUnicodeString(Value: UnicodeString); virtual;
  {$ENDIF}
  public
    constructor Create;

    function AsRange(Index: Integer): TWideString; overload;
    function AsRange(Index, MaxCount: Integer): TWideString; overload;

    procedure AsString(Source: PLegacyString; EncodeOptions: TEncodeUTF16 = coUTF16);
    procedure AsWideString(Source: PWideChar; Length: Integer;
      SourceOptions: TEndianSource = []); overload;

    function Compare(Value: PWideChar; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;
    function Compare(Value: PWideString; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;

    procedure Detach; {$IFNDEF Lite} virtual;
    class function LengthOf(Source: Pointer): Integer; virtual; {$ENDIF}

    function NextIndex(Value: WideChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Value: WideChar): Integer; overload;
    function PrevIndex(Value: WideChar; StartIndex: Integer): Integer; overload;

    procedure Load(Source: PReadableStream); overload;
    procedure Load(Source: PReadableStream; ByteOrder: TByteOrder;
      FallbackCP: Word = CP_ACP); overload;
    procedure Save(Dest: PWritableStream); overload;
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean); overload;

    procedure SwapByteOrder; overload;

    property Data: PWideChar read GetData write SetData;
    property Options: TEndianOptions read FOptions;
    property RawData: PWideChar read FData write SetData;
  end;

  PCoreString = PWideString;
  TCoreString = TWideString;

  PStrings = ^TStrings;
  TStrings = object(TCollections)
  public
    property TextLength: Integer read TotalCount;
  end;

  PLegacyStrings = ^TLegacyStrings;
  TLegacyStrings = object(TStrings)
    // TODO: append in amCapture mode
  end;

  PWideStrings = ^TWideStrings;
  TWideStrings = object(TStrings)
    // TODO: append in amCapture mode
  end;

  PLegacyCommandLineParam = ^TLegacyCommandLineParam;
  TLegacyCommandLineParam = object(TLegacyString)
  private
    FQuoted: Boolean;
  public
    function AsNextParam(CommandLine: PLegacyString): TLegacyString;
    property Quoted: Boolean read FQuoted;
  end;

  PWideCommandLineParam = ^TWideCommandLineParam;
  TWideCommandLineParam = object(TWideString)
  private
    FQuoted: Boolean;
  public
    function AsNextParam(CommandLine: PWideString): TWideString;
    property Quoted: Boolean read FQuoted;
  end;

  PCommandLineParam = PWideCommandLineParam;
  TCommandLineParam = TWideCommandLineParam;

{ Exceptions }

  TErrorSource = record
    case Byte of
      1: (AsString: PLegacyString);
      2: (AsWideString: PWideString);
  end;

  EString = class(Exception)
  private
    FSource: TErrorSource;
  public
    property Source: TErrorSource read FSource;
  end;

  EIntegerString = class(EString)
  private
    FHexadecimal: Boolean;
  public
    constructor Create(Source: PString; Hexadecimal: Boolean);
    property Hexadecimal: Boolean read FHexadecimal;
  end;

  TConvertMode = (cmDecodeUTF16, cmEncodeUTF16);

  EConvert = class(EString)
  private
    FMode: TConvertMode;
  public
    property Mode: TConvertMode read FMode;
  end;

  ECodePage = class(EConvert)
  private
    FErrorCode: LongWord;
    FCodePage: PCodePage;
  public
    constructor Create(Source: PString; CodePage: PCodePage;
      Mode: TConvertMode);
    property ErrorCode: LongWord read FErrorCode;
    property CodePage: PCodePage read FCodePage;
  end;

  TCharSet = (usLatin1, usUTF8, usCESU8, usUTF16);

  EUnicode = class(EConvert)
  private
    FErrorInfo: TErrorInfo;
    FEncodeOptions: TEncodeOptions;
  public
    constructor Create(Source: PString; EncodeOptions: TEncodeOptions; const Info: TErrorInfo);
    property EncodeOptions: TEncodeOptions read FEncodeOptions;
    property ErrorInfo: TErrorInfo read FErrorInfo;
  end;

  EUTF32 = class(EString)
  public
    constructor Create(Source: PString);
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

{$IF not UnicodeRTL}
var
  DefaultUnicodeCodePage: Integer;
{$IFEND}

function DecodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;
function EncodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;

implementation

uses
  CoreConsts;

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

function ExtractCodePageName(const Info: TCPInfoEx): TCoreStringRec;
var
  P, Limit: PCoreChar;
begin
  Result.Value := Info.CodePageName;
  Limit := Info.CodePageName + Length(Info.CodePageName);

  while Result.Value < Limit do
  begin
    case Result.Value^ of
      CoreChar(0)..CoreChar(32), CoreChar('0')..CoreChar('9'):
        begin
          Inc(Result.Value);
          Continue;
        end;
      CoreChar('('):
        begin
          Result.Length := WideStrLen(Result.Value, Limit - Result.Value);
          P := Result.Value + Result.Length - 1;
          if P^ = CoreChar(')') then
          begin
            Inc(Result.Value);
            Dec(Result.Length, 2);
          end;
          Exit;
        end;
    end;
    Break;
  end;
  Result.Length := Limit - Result.Value;
end;

procedure FillWideChar(var Buf; Count: Integer; Value: WideChar);
var
  P: PWideChar;
begin
  P := @Buf;
  while Count <> 0 do
  begin
    P^ := Value;
    Inc(P);
    Dec(Count);
  end;
end;

procedure MoveZeroExpand(const Source; var Dest; Count: Integer);
var
  S: PByte;
  D: PWord;
begin
  S := @Source;
  D := @Dest;
  while Count <> 0 do
  begin
    D^ := S^;
    Inc(S);
    Inc(D);
    Dec(Count);
  end;
end;

procedure PutHexDigits(const Source; var Dest; Count: Integer; WideChar: Boolean); // don't used here
var
  S: PByte;
  W: PWord;
  L: PLongWord;
begin
  S := @Source;
  if WideChar then
  begin
    L := @Dest;
    Inc(L, Count - 1); // platform
    while Count <> 0 do
    begin
      L^ := Byte(HexDigits[S^ and $F]) or (Byte(HexDigits[S^ shr 4]) shl 16);
      Inc(S);
      Dec(L);
      Dec(Count);
    end;
  end
  else
  begin
    W := @Dest;
    Inc(W, Count - 1); // platform
    while Count <> 0 do
    begin
      W^ := Byte(HexDigits[S^ and $F]) or (Byte(HexDigits[S^ shr 4]) shl 8);
      Inc(S);
      Dec(W);
      Dec(Count);
    end;
  end;
end;

{ EIntegerString }

constructor EIntegerString.Create(Source: PString; Hexadecimal: Boolean);
const
  ValueType: array[Boolean] of PLegacyChar = (sInteger, sHexadecimal);
var
  W: TWideString;
begin
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    W.Create;
    try
      if Source.ItemSize = SizeOf(WideChar) then
        W.AsRange(Source, 0)
      else
        W.AsString(PLegacyString(Source));
      inherited Create(sInvalidInteger, CP_LEGACY, [W.RawData, ValueType[Hexadecimal]]);
    finally
      W.Destroy;
    end;
  end
  else
    inherited Create(sNullInteger, [ValueType[Hexadecimal]]);
  FSource.AsString := Pointer(Source);
  FHexadecimal := Hexadecimal;
end;

{ ECodePage }

constructor ECodePage.Create(Source: PString; CodePage: PCodePage; Mode: TConvertMode);
const
  ModeNames: array[TConvertMode] of PLegacyChar = (sSystemUTF16toCP, sSystemCPtoUTF16);
var
  ErrorCode: LongWord;
  Msg: TCoreStringRec;
  CharSet: PLegacyChar;
begin
  ErrorCode := GetLastError;
  if ErrorCode <> 0 then
  begin
    Msg := SysErrorMessage(ErrorCode);
    try
      inherited Create(ModeNames[Mode], CP_LEGACY, [Msg.Value, CodePage.Number, CodePage.Name]);
    finally
      LocalFree(Msg.Handle);
    end;
    CharSet := nil;
  end
  else if PWideString(Source).FDataSource.IsType(TypeOf(TLegacyString)) then
    if PWideString(Source).FDataSource.CodePage <> nil then
    begin
      with PWideString(Source).FDataSource.CodePage^ do
        inherited Create(sCPtoCP, CP_LEGACY, [Number, Name, CodePage.Number, CodePage.Name]);
      CharSet := nil;
    end
    else
      with PWideString(Source).FDataSource^ do
        CharSet := CharSetName(TEncodeRawBytes(FOptions * [soLatin1]))
  else
    CharSet := sUTF16;

  if CharSet <> nil then
    inherited Create(sUnicodetoCP, CP_LEGACY, [CharSet, CodePage.Number, CodePage.Name]);

  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FErrorCode := ErrorCode;
  FCodePage := CodePage;
end;

{ EUnicode }

constructor EUnicode.Create(Source: PString; EncodeOptions: TEncodeOptions; const Info: TErrorInfo);
var
  Buf: array[0..Length(sNonBMP)+8] of LegacyChar;
  CharSet, Msg: PLegacyChar;
begin
  if Info.InvalidUTF8.RawData <> 0 then
    with Info.InvalidUTF8 do
      if BrokenSequence <> 0 then
        inherited Create(sBrokenUTF8, [BrokenSequence, StartingByte])
      else
        inherited Create(sBadUTF8, [StartingByte])
  else
  begin
    CharSet := Source.CharSetName(EncodeOptions);
    case Info.InvalidChar of
      Low(THighSurrogates)..High(THighSurrogates):
        inherited Create(sBrokenSurrogatePair, [CharSet, Info.InvalidChar]);
      Low(TLowSurrogates)..High(TLowSurrogates):
        inherited Create(sBadSurrogatePair, [CharSet, Info.InvalidChar]);
    else
      case Info.InvalidChar of
        $80..$9F:
          Msg := sInvalidChar;
        Low(TUnicodeSMP)..High(TUnicodePUA):
          Msg := sNonBMP;
      else
        Msg := sNonUnicode;
      end;
      FormatBuf(Msg, [Info.InvalidChar, WhitespaceOrLineBreak[IsConsole]], Buf);
      inherited Create(sInvalidString, [CharSet, @Buf]);
    end;
  end;
  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FEncodeOptions := EncodeOptions;
  FErrorInfo := Info;
end;

{ EUTF32 }

constructor EUTF32.Create(Source: PString);
begin
  inherited Create(sUTF32notSupported);
  FSource.AsString := Pointer(Source);
end;

{ TCodePage }

constructor TCodePage.Create(CodePage: Word; DefaultReplacementChar: LegacyChar);
var
  P, Limit: PLegacyChar;
begin
  FillChar(Self, SizeOf(Self), 0);
  FReplacementChar := DefaultReplacementChar;

  if not GetCPInfoEx(CodePage, 0, FInfo) then
    RaiseLastPlatformError(sCodePage, CodePage);

  FNumber := FInfo.CodePage;
  FMaxCharBytes := FInfo.MaxCharSize;
  FSysReplacementChar := LegacyChar(FInfo.DefaultChar[0]);

  with ExtractCodePageName(FInfo) do
  begin
    FName := Value;
    FName[Length] := #0;
  end;

  P := Pointer(@FInfo.LeadByte);
  Limit := P + SizeOf(FInfo.LeadByte);
  while P < Limit do
    if (P[0] <> #0) and (P[1] <> #0) then
    begin
      FLeadBytes := FLeadBytes + [P[0]..P[1]];
      Inc(P, 2);
    end
    else
      Break;
end;

function TCodePage.DecodeUTF16(Source: PWideString; Dest: PLegacyString;
  DestIndex: Integer; EncodeOptions: TEncodeCodePage): Integer;
var
  Replacement: PLegacyChar;
  ReplacementUsed: LongBool;
  ReplaceResult: ^LongBool;
  IsUnicode: Boolean;
begin
{$IFDEF Debug}
  if soBigEndian in Source.Options then
    raise Exception.Create(sUnsupportedUTF16BE);
{$ENDIF}

  IsUnicode := FNumber - CP_UTF7 in [0..1];
  if IsUnicode then
  begin
    Replacement := nil;
    ReplaceResult := nil;
    ReplacementUsed := False;
  end
  else
  begin
    if coSysReplacementChar in EncodeOptions then
      Replacement := @FSysReplacementChar
    else
      Replacement := @FReplacementChar;
    ReplaceResult := @ReplacementUsed;
  end;

  Result := {$IFDEF Tricks} System. {$ENDIF} WideCharToMultiByte(
    FNumber, WC_NO_BEST_FIT_CHARS and (Integer(IsUnicode) - 1), Source.RawData, Source.Count,
    Dest.RawData + DestIndex, Dest.Capacity - DestIndex, Replacement, ReplaceResult
  );
  if ReplacementUsed then
    Result := -Result;
end;

function TCodePage.EncodeUTF16(Source: PLegacyString; Dest: PWideString;
  DestIndex: Integer; EncodeOptions: TEncodeUTF16): Integer;
begin
  Result := {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(
    FNumber, MB_ERR_INVALID_CHARS and (Integer((coReplaceInvalid in EncodeOptions) or (FNumber = CP_GB18030)) - 1),
    Source.RawData, Source.Count, Dest.RawData + DestIndex, Dest.Capacity - DestIndex
  );
  if coBigEndian in EncodeOptions then
    SwapWideCharBytes(Dest.RawData + DestIndex, Dest.RawData + DestIndex, Result);
end;

{ TString }

procedure TString.Assign(Source: Pointer; Length: Integer; Options: TStringSource);
begin
  if Source <> nil then
    if soAttachBuffer in Options then
      inherited Assign(Source, Length, Length + Integer(soNullTerminated in Options), True)
    else
    begin
      inherited Assign(Source, Length, Length + SizeOf(WideChar), False);
      PWideChar(PLegacyString(@Self).RawData + Length * ItemSize)^ := #0;
    end
  else
    Clear;
  PLegacyString(@Self).FOptions := Options - soBuiltIn;
end;

function TString.AssignArray(Index: Integer; const Values: array of const): Integer;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  Result := 0; // TODO
end;

const
  MaxDigits = 20;

function TString.AssignInteger(Index: Integer; Value: Int64; MinWidth: Integer;
  Hexadecimal: THexadecimal; FillChar: WideChar): Integer;
var
  Buf: array[1..MaxDigits] of LegacyChar;
  Digits: PLegacyChar;
  Minus: Boolean;
  LowerCaseMask: Word;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  if Hexadecimal <> hexNone then
  begin
    Digits := @Buf[High(Buf) - SizeOf(Word)];
    Result := 0;
    LowerCaseMask := (Byte(Hexadecimal) - 1) * $20;
    Inc(LowerCaseMask, LowerCaseMask shl 8);
    repeat
      PWord(Digits)^ := Byte(HexDigits[Byte(Value) shr 4]) or
        (Byte(HexDigits[Byte(Value) and $F]) shl 8) or LowerCaseMask; // Fast core
      Inc(Result);
      Value := Value shr 8;
      if Value = 0 then
        Break;
      Dec(Digits, SizeOf(Word));
    until False;
    Result := Result * SizeOf(Word);
    if Digits^ = '0' then
    begin
      Inc(Digits);
      Dec(Result);
    end;
  end
  else
  begin
    if Value < 0 then
    begin
      Minus := True;
      Value := Abs(Value);
    end
    else
      Minus := False;

    Digits := @Buf[High(Buf)];
    Result := 0;
    repeat
      Digits^ := LegacyChar(Value mod 10 + Byte('0'));
      Inc(Result);
      Value := Value div 10;
      if Value = 0 then
        Break;
      Dec(Digits);
    until False;

    if Minus then
    begin
      Dec(Digits);
      Digits^ := '-';
      Inc(Result);
    end;
  end;

  if ItemSize = SizeOf(LegacyChar) then
    with PLegacyString(@Self)^ do
      if Result < Abs(MinWidth) then
      begin
        if MinWidth < 0 then
        begin
          System.FillChar(FData[Index], Abs(MinWidth) - Result, FillChar);
          Move(Digits^, FData[Index + Abs(MinWidth) - Result], Result);
        end
        else
        begin
          Move(Digits^, FData[Index], Result);
          System.FillChar(FData[Index + Result], MinWidth - Result, FillChar);
        end;
        Result := Abs(MinWidth);
      end
      else
        Move(Digits^, FData[Index], Result)
  else
    with PWideString(@Self)^ do
      if Result < Abs(MinWidth) then
      begin
        if MinWidth < 0 then
        begin
          FillWideChar(FData[Index], Abs(MinWidth) - Result, FillChar);
          MoveZeroExpand(Digits^, FData[Index + Abs(MinWidth) - Result], Result);
        end
        else
        begin
          MoveZeroExpand(Digits^, FData[Index], Result);
          FillWideChar(FData[Index + Result], MinWidth - Result, FillChar);
        end;
        Result := Abs(MinWidth);
      end
      else
        MoveZeroExpand(Digits^, FData[Index], Result);
end;

procedure TString.AsArray(const Values: array of const);
var
  Length: Integer;
begin
  Clear;
  Capacity := Estimate(Values) + SizeOf(WideChar);
  if Capacity <> 0 then
  begin
    Length := AssignArray(0, Values);
    Append(Length);
    PWideChar(PLegacyString(@Self).FData + Length * ItemSize)^ := #0;
  end
end;

procedure TString.AsInteger(Value: Int64; MinWidth: Integer; Hexadecimal: THexadecimal;
  FillChar: WideChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > MaxDigits then
    Length := Abs(MinWidth)
  else
    Length := MaxDigits;
  Capacity := Length + SizeOf(WideChar);
  Length := AssignInteger(0, Value, MinWidth, Hexadecimal, FillChar);
  Append(Length);
  PWideChar(PLegacyString(@Self).FData + Length * ItemSize)^ := #0;
end;

function TString.AsInteger(var Value: Int64; Hexadecimal: Boolean): Boolean;
const
  UpperCaseMask = not $20;
var
  Digit, Limit: PLegacyChar;
  Minus: Boolean;
begin
  if Count <> 0 then
  begin
    Digit := PLegacyString(@Self).FData;
    Limit := Digit + Count * ItemSize;
    Value := 0;
    if Hexadecimal then
      while Digit < Limit do
      begin
        if (ItemSize = 1) or (Digit[1] = #0) then
          case Digit^ of
            '0'..'9':
              begin
                Value := Value shl 4 + PByte(Digit)^ - Byte('0');
                Inc(Digit, ItemSize);
                Continue;
              end;
            'A'..'F', 'a'..'f':
              begin
                Value := Value shl 4 + PByte(Digit)^ and UpperCaseMask - Byte('A') + $A;
                Inc(Digit, ItemSize);
                Continue;
              end;
          end;
        Result := False;
        Exit;
      end
    else
    begin
      Minus := Digit^ = '-';
      if (Digit^ in ['+', '-']) and ((ItemSize = 1) or (Digit[1] = #0)) then // Fast core
        Inc(Digit, ItemSize);
      while Digit < Limit do
        if (Digit^ in ['0'..'9']) and ((ItemSize = 1) or (Digit[1] = #0)) then // Fast core
        begin
          Value := Value * 10 + PByte(Digit)^ - Byte('0');
          Inc(Digit, ItemSize);
        end
        else
        begin
          Result := False;
          Exit;
        end;
      if Minus then
        Value := -Value;
    end;
  end;
  Result := True;
end;

function TString.AsInteger(Hexadecimal: Boolean): Int64;
begin
  if not AsInteger(Result, Hexadecimal) then
    raise EIntegerString.Create(@Self, Hexadecimal);
end;

function TString.CharSetName(EncodeOptions: TEncodeOptions): PLegacyChar;
begin
  if ItemSize = SizeOf(LegacyChar) then
    if coSurrogatePairs in EncodeOptions then
      Result := sCESU8
    else if coLatin1 in EncodeOptions then
      Result := sLatin1
    else
      Result := sUTF8
  else
    Result := sUTF16;
end;

function TString.Estimate(const Values: array of const): Integer;
begin
  Result := 0; // TODO
end;

{ TLegacyString }

constructor TLegacyString.Create;
begin
  inherited Create(sLegacyString, SizeOf(LegacyChar));
end;

function TLegacyString.AsRange(Index: Integer): TLegacyString;
begin
  Result.Create;
  Result.AsRange(@Self, Index);
end;

function TLegacyString.AsRange(Index, MaxCount: Integer): TLegacyString;
begin
  Result.Create;
  Result.AsRange(@Self, Index, MaxCount);
end;

procedure TLegacyString.AsString(Source: PLegacyChar; Length: Integer;
  SourceOptions: TRawByteSource);
begin
  Assign(Source, Length, SourceOptions);
end;

function TLegacyString.AssignString(Index: Integer; Source: PLegacyString;
  EncodeOptions: TEncodeUTF16): TConvertResult;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);
  // TODO
end;

function TLegacyString.AssignWideString(Index: Integer; Source: PWideString;
  EncodeOptions: TEncodeRawBytes): TConvertResult;
var
  CodePoint: QuadChar;
  Paired, W: Word;
  Dest, Limit: PLegacyChar;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if FCodePage <> nil then
  begin
    Result.DestCount := FCodePage.DecodeUTF16(Source, @Self, Index, EncodeOptions);
    if Result.DestCount > 0 then
      Result.SourceCount := Source.Count;
    Exit;
  end;

  Dest := FData + Index;
  Limit := FData + Capacity;

  while (Dest < Limit) and (Result.SourceCount < Source.Count) do
  begin
    CodePoint := Word(Source.FData[Result.SourceCount]);
    if soBigEndian in Source.FOptions then
      CodePoint := Swap(CodePoint);
    Inc(Result.SourceCount);

    case Word(CodePoint) of
      $00..$7F:
        begin
          if (CodePoint = 0) and (EncodeOptions * coModifiedUTF8 = coModifiedUTF8) then
          begin
            PWord(Dest)^ := $80C0; // Fast core
            Inc(Dest, SizeOf(Word));
            Inc(Result.SuccessBytes, SizeOf(Word));
          end
          else
          begin
            Dest^ := LegacyChar(CodePoint);
            Inc(Dest);
          end;
          Continue;
        end;

      Low(THighSurrogates)..High(THighSurrogates):
        begin
          Paired := Word(Source.FData[Result.SourceCount]);
          if soBigEndian in Source.FOptions then
            Paired := Swap(Paired);
          Inc(Result.SourceCount);

          case Paired of
            Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                CodePoint := (CodePoint - Low(THighSurrogates)) shl 10 +
                  Low(TUnicodeSMP) + Paired - Low(TLowSurrogates);
                if coCESU8 in EncodeOptions then
                begin
                  W := CodePoint;
                  PLongWord(Dest)^ := // Fast core
                    ($E0 or Byte(W shr 12)) or
                    (($80 or (Byte(W shr 6) and $3F)) shl 8) or
                    (($80 or Byte(W and $3F)) shl 16) or
                    (($E0 or Byte(Paired shr 12)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  PWord(Dest)^ :=
                    (($80 or (Byte(Paired shr 6) and $3F)) shl 8) or
                    (($80 or Byte(Paired and $3F)) shl 16);
                  Inc(Dest, SizeOf(Word));
                  with Result do
                  begin
                    Inc(SuccessBytes, SizeOf(LongWord) + SizeOf(Word));
                    Inc(SurrogatePairs);
                  end;
                end
                else if not (coLatin1 in EncodeOptions) then
                begin
                  PLongWord(Dest)^ := // Fast core
                    ($F0 or Byte(CodePoint shr 18)) or
                    (($80 or Byte(CodePoint shr 12)) shl 8) or
                    (($80 or (Byte(CodePoint shr 6) and $3F)) shl 16) or
                    (($80 or Byte(CodePoint and $3F)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  with Result do
                  begin
                    Inc(SuccessBytes, SizeOf(LongWord));
                    Inc(SurrogatePairs);
                  end;
                end
                else if coReplaceInvalid in EncodeOptions then
                begin
                  Dest^ := LegacyReplacementChar;
                  Inc(Dest);
                  Continue;
                end
                else
                begin
                  Result.ErrorInfo.InvalidChar := CodePoint;
                  Break;
                end;
              end;
          end;
        end;

      Low(TLowSurrogates)..High(TLowSurrogates), $FFFE, $FEFF: // UTF-16 BOM
        if coReplaceInvalid in EncodeOptions then
        begin
          if EncodeOptions * coModifiedUTF8 = [coLatin1] then
          begin
            Dest^ := LegacyReplacementChar;
            Inc(Dest);
          end
          else
          begin
            PLongWord(Dest)^ := $BDBFEF; // U+FFFD in UTF-8
            Inc(Dest, 3);
            Inc(Result.SuccessBytes, 3);
          end;
          Continue;
        end
        else
        begin
          Result.ErrorInfo.InvalidChar := CodePoint;
          Break;
        end;
    else
      if EncodeOptions * coModifiedUTF8 = [coLatin1] then
      begin
        case Word(CodePoint) of
          $A0..$FF:
            begin
              Dest^ := LegacyChar(CodePoint);
              Dec(Result.SuccessBytes);
            end;
        else
          if coReplaceInvalid in EncodeOptions then
            Dest^ := LegacyReplacementChar
          else
          begin
            Result.ErrorInfo.InvalidChar := CodePoint;
            Break;
          end;
        end;
        Inc(Dest);
        Continue;
      end
      else
      begin
        W := CodePoint;
        case W of
          $80..$7FF:
            begin
              PWord(Dest)^ := // Fast core
                ($C0 or (W shr 6)) or
                (($80 or (W and $3F)) shl 8);
              Inc(Dest, SizeOf(Word));
              Inc(Result.SuccessBytes, SizeOf(Word));
            end;
        else
          PLongWord(Dest)^ := // Fast core
            ($E0 or Byte(W shr 12)) or
            (($80 or (Byte(W shr 6) and $3F)) shl 8) or
            (($80 or Byte(W and $3F)) shl 16);
          Inc(Dest, 3);
          Inc(Result.SuccessBytes, 3);
        end;
      end;
    end;
  end;

  Result.DestCount := Dest - FData + Index;
end;

procedure TLegacyString.AsWideString(Source: PWideString; EncodeOptions: TEncodeRawBytes);
var
  CharBytes: Integer;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    if FCodePage <> nil then
      CharBytes := MaxCharBytes(FCodePage.Number)
    else
      CharBytes := 4 + Byte(coCESU8 in EncodeOptions) * 2;

    Capacity := (Source.Count + 1) * CharBytes;
    with AssignWideString(0, Source, EncodeOptions) do
    begin
      if (DestCount <= 0) and (FCodePage <> nil) then
        raise ECodePage.Create(Source, FCodePage, cmDecodeUTF16);
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);

      if FCodePage <> nil then
        Exclude(FOptions, soDetectUTF8);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

function TLegacyString.Compare(Value: PLegacyChar; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
    Value, Count, FData, Count) - CSTR_EQUAL;
end;

function TLegacyString.Compare(Value: PLegacyString; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  if Value <> nil then
    Result := CompareStringA(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
      Value.RawData, Value.Count, FData, Count) - CSTR_EQUAL
  else
    Result := 1;
end;

procedure TLegacyString.Detach;
begin
  if (Capacity <> 0) and (FData[Count] <> #0) then
  begin
    if AttachBuffer then
      Capacity := Count + 1;
    FData[Count] := #0;
  end;
end;

function TLegacyString.GetData: PLegacyChar;
begin
  Detach;
  Result := FData;
end;

function TLegacyString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

{$IFNDEF Lite}
class function TLegacyString.LengthOf(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;
{$ENDIF}

function TLegacyString.NextIndex(Index: Integer): Integer;
begin
  CheckIndex(Index);
  if FCodePage <> nil then
  begin
    Result := Index;
    while not (FData[Result] in [#0..#127] + FCodePage.LeadBytes) do
      Inc(Result);
  end
  else
    Result := Index + 1;
end;

function TLegacyString.NextIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
  CheckIndex(StartIndex);
  S := StrScan(FData, Count, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.PrevIndex(Index: Integer): Integer;
begin
  CheckIndex(Index);
  Result := Index - 1;
  if (ItemSize = 1) and (FCodePage <> nil) then
    while not (FData[Index] in [#0..#127] + FCodePage.LeadBytes) do
      Dec(Result);
end;

function TLegacyString.PrevIndex(Value: LegacyChar): Integer;
var
  S: PLegacyChar;
begin
  S := StrRScan(FData, Count, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
  CheckIndex(StartIndex);
  S := StrRScan(FData, Count - StartIndex, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

procedure TLegacyString.Load(Source: PReadableStream);
begin
  Load(Source, True);
end;

procedure TLegacyString.Load(Source: PReadableStream; AllowBOM: Boolean;
  SourceOptions: TRawByteOptions);
var
  BOM: TReadableBOM;
  W: TWideString;
  CP: TCodePage;
  Length: Integer;
begin
  Clear;

  if AllowBOM then
  begin
    BOM := Source.ReadBOM;
    case BOM of
      bomUTF8:
        Include(FOptions, soDetectUTF8);
      bomUTF16LE, bomUTF16BE:
        begin
          W.Create;
          try
            W.Load(Source, TByteOrder(Byte(BOM) - Byte(bomUTF16)));
            if soBigEndian in W.Options then
              W.SwapByteOrder;
            AsWideString(@W);
          finally
            W.Destroy;
          end;
          Exit;
        end;
      bomUTF7:
        begin
          Capacity := Source.Size - Source.Position;
          if Capacity <> 0 then
          begin
            CP.Create(CP_UTF7);
            W.Create;
            try
              Source.ReadBuffer(FData^, Capacity);
              W.Capacity := Capacity;
              Length := CP.EncodeUTF16(@Self, @W, 0, []);
              if Length <= 0 then
                raise ECodePage.Create(@Self, @CP, cmEncodeUTF16);
              W.Append(Length);
              AsWideString(@W);
            finally
              W.Destroy;
            end;
          end;
          Exit;
        end;
      bomGB18030:
        begin
          Length := Source.Size - Source.Position;
          if Length <> 0 then
          begin
            Capacity := Length + 1;
            Source.ReadBuffer(FData^, Length);
            if (FCodePage <> nil) and (FCodePage.Number = CP_GB18030) then
            begin
              Append(Length);
              FData[Length] := #0;
            end
            else
            begin
              CP.Create(CP_GB18030);
              W.Create;
              try
                W.Capacity := Length;
                Length := CP.EncodeUTF16(@Self, @W, 0, []);
                if Length <= 0 then
                  raise ECodePage.Create(@Self, @CP, cmEncodeUTF16);
                W.Append(Length);
                AsWideString(@W);
              finally
                W.Destroy;
              end;
            end;
          end;
          Exit;
        end;
    else
      if BOM <> bomNone then
        raise EUTF32.Create(@Self);
    end;
  end;

  Length := Source.Size - Source.Position;
  if Length <> 0 then
  begin
    Capacity := Length + 1;
    Source.ReadBuffer(FData^, Length);
    Append(Length);
    FData[Length] := #0;
    FOptions := SourceOptions;
  end;
end;

procedure TLegacyString.Save(Dest: PWritableStream);
begin
  Save(Dest, FCodePage <> nil);
end;

procedure TLegacyString.Save(Dest: PWritableStream; WriteBOM: Boolean);
begin
  if Count <> 0 then
  begin
    if WriteBOM then
      if FCodePage = nil then
      begin
        if FOptions = [] then
          Dest.WriteBOM(bomUTF8);
      end
      else
        case FCodePage.Number of
//          CP_UTF7:
//            Dest.WriteBOM(bomUTF7); // TODO
          CP_GB18030:
            Dest.WriteBOM(bomGB18030);
        end;
    Dest.WriteBuffer(FData^, Count);    
  end;
end;

procedure TLegacyString.SetCodePage(Value: PCodePage);
var
  Capture: TLegacyString;
  W: TWideString;
begin
  if (Count <> 0) and ((FCodePage <> Value) or
    ((Value <> nil) and (FCodePage.Number <> Value.Number))) then
  begin
    W.Create;
    try
      W.AsString(@Self);
      Capture.Create;
      Capture.FOptions := FOptions;
      Capture.FCodePage := FCodePage;
      W.FDataSource := @Capture;
      FCodePage := Value;
      AsWideString(@W);
    finally
      W.Destroy;
    end;
  end
  else
    FCodePage := Value;
end;

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), soFromTheWild);
end;

{$IFDEF CoreLiteVCL}
function TLegacyString.GetRawByteString: RawByteString;
begin
  SetString(Result, FData, Count);
{$IF UnicodeRTL}
  System.SetCodePage(Result, CP, False);
{$IFEND}
end;

procedure TLegacyString.SetRawByteString(Value: RawByteString);
begin
  Assign(Pointer(Value), Length(Value), soFromTheWild);
end;

function TLegacyString.GetUnicodeString: UnicodeString;
var
  W: TWideString;
begin
  W.Create;
  try
    W.AsString(@Self);
    SetString(Result, W.RawData, W.Count);
  finally
    W.Destroy;
  end;
end;

procedure TLegacyString.SetUnicodeString(Value: UnicodeString);
var
  W: TWideString;
begin
  W.Create;
  try
    W.Assign(Pointer(Value), Length(Value), soAttach);
    AsWideString(@W);
  finally
    W.Destroy;
  end;
end;
{$ENDIF}

{ TWideString }

constructor TWideString.Create;
begin
  inherited Create(sWideString, SizeOf(WideChar));
end;

function TWideString.AsRange(Index: Integer): TWideString;
begin
  Result.Create;
  Result.AsRange(@Self, Index);
end;

function TWideString.AsRange(Index, MaxCount: Integer): TWideString;
begin
  Result.Create;
  Result.AsRange(@Self, Index, MaxCount);
end;

function TWideString.AssignString(Index: Integer; Source: PLegacyString;
  EncodeOptions: TEncodeUTF16): TConvertResult;

function NextChar(var ConvertResult: TConvertResult): QuadChar;
var
  FirstByte, Bytes, B, C: Byte;
begin
  FirstByte := Byte(Source.FData[ConvertResult.SourceCount]);
  Inc(ConvertResult.SourceCount);

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
        ConvertResult.ErrorInfo.InvalidUTF8.StartingByte := FirstByte;
        Result := 0;
        Exit;
      end;

      B := Bytes;
      while (B <> 0) and (ConvertResult.SourceCount < Source.Count) do
      begin
        C := Byte(Source.FData[ConvertResult.SourceCount]);
        if C and $C0 = $80 then
        begin
          Result := (Result shl 6) or (C and $3F);
          Inc(ConvertResult.SourceCount);
          Dec(B);
        end
        else
          Break; // broken sequence
      end;

      if B = 0 then
      begin
        Inc(ConvertResult.SuccessBytes, Bytes + 1);
        Exit;
      end;
      ConvertResult.ErrorInfo.InvalidUTF8.RawData := FirstByte or (Bytes + 1) shl 8;
    end;
    ConvertResult.ErrorInfo.InvalidUTF8.StartingByte := FirstByte;
    Result := 0;
  end
  else
    Result := FirstByte;
end;

var
  Dest, Limit: PWideChar;
  CodePoint, Paired: QuadChar;
  W: Word;
  B: Byte;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if (Source.FCodePage = nil) or (soDetectUTF8 in Source.FOptions) then
  begin
    Dest := FData + Index;
    Limit := FData + Capacity;
    
    while (Result.SourceCount < Source.Count) and (Dest < Limit) do
    begin
      CodePoint := NextChar(Result);
      if Result.ErrorInfo.InvalidUTF8.RawData <> 0 then
        Break;

      case CodePoint of
        Low(THighSurrogates)..High(THighSurrogates):
          if coSurrogatePairs in EncodeOptions then
          begin
            Paired := NextChar(Result);
            if Result.ErrorInfo.InvalidUTF8.RawData <> 0 then
              Break;

            case Paired of
              Low(TLowSurrogates)..High(TLowSurrogates): // CESU-8
                begin
                  if coBigEndian in EncodeOptions then // Fast core
                    PLongWord(Dest)^ := Swap(CodePoint) or (Swap(Paired) shl 16)
                  else
                    PLongWord(Dest)^ := Word(CodePoint) or (Word(Paired) shl 16);
                  Inc(Dest, 2);
                  Inc(Result.SurrogatePairs);
                  Continue;
                end;
            end;
          end;

        Low(TLowSurrogates)..High(TLowSurrogates), $FFFE, $FFFF,
        Low(TNonUnicode)..High(TNonUnicode):
          ;

        Low(TUnicodeSMP)..High(TUnicodePUA):
          if coSurrogatePairs in EncodeOptions then
          begin
            W := CodePoint - Low(TUnicodeSMP);
            if coBigEndian in EncodeOptions then // Fast core
              PLongWord(Dest)^ :=
                Word(Swap(Low(THighSurrogates) + W shr 10)) or
                Word(Swap((Low(TLowSurrogates) + W and $3FF) shl 16))
            else
              PLongWord(Dest)^ :=
                Word(Low(THighSurrogates) + W shr 10) or
                Word((Low(TLowSurrogates) + W and $3FF) shl 16);
            Inc(Dest, 2);
            Inc(Result.SurrogatePairs);
            Continue;
          end;
      else
        W := CodePoint;
        if coBigEndian in EncodeOptions then
          W := Swap(W);
        Dest^ := WideChar(W);
        Inc(Dest);
        Continue;
      end;

      if (coReplaceInvalid in EncodeOptions) and
        ((Source.FCodePage = nil) or not (soLatin1 in Source.FOptions)) then
      begin
        W := $FFFD;
        if coBigEndian in EncodeOptions then
          W := Swap(W);
        Dest^ := WideChar(W);
        Inc(Dest);
      end
      else
        Break;
    end;

    if Result.ErrorInfo.RawData = 0 then
    begin
      Result.DestCount := Dest - (FData + Index);
      Exit;
    end;
  end;

  if Source.FCodePage <> nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.DestCount := Source.FCodePage.EncodeUTF16(Source, @Self, Index, EncodeOptions);
    if Result.DestCount <= 0 then
      raise ECodePage.Create(Source, Source.FCodePage, cmEncodeUTF16);
    Result.SourceCount := Source.Count;
  end
  else if coLatin1 in EncodeOptions then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Dest := FData + Index;
    Limit := FData + Count;

    while (Result.SourceCount < Source.Count) and (Dest < Limit) do
    begin
      B := Byte(Source.FData[Result.SourceCount]);
      Inc(Result.SourceCount);
      if B in [$00..$7F, $A0..$FF] then
      begin
        PWord(Dest)^ := B shl (Byte(coBigEndian in EncodeOptions) * 8); // Fast core
        if B in [$A0..$FF] then
          Dec(Result.SuccessBytes); // negative for Latin1
      end
      else if coReplaceInvalid in EncodeOptions then
      begin
        PWord(Dest)^ := Byte(LegacyReplacementChar) shl (Byte(coBigEndian in EncodeOptions) * 8); // Fast core
        Dec(Result.SuccessBytes);
      end
      else
      begin
        Result.ErrorInfo.InvalidChar := B;
        Break;
      end;
      Inc(Dest);
    end;

    Result.DestCount := Dest - (FData + Index);
  end;
end;

function TWideString.AssignWideString(Index: Integer; Source: PWideString;
  EncodeOptions: TEncodeRawBytes): TConvertResult;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);
  // TODO
end;

procedure TWideString.AsString(Source: PLegacyString; EncodeOptions: TEncodeUTF16);
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    Capacity := Source.Count + 1;
    with AssignString(0, Source, EncodeOptions) do
    begin
      if (DestCount = 0) and (Source.FCodePage <> nil) then
        raise ECodePage.Create(Source, Source.FCodePage, cmEncodeUTF16);
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

procedure TWideString.AsWideString(Source: PWideChar; Length: Integer;
  SourceOptions: TEndianSource);
begin
  Assign(Source, Length, SourceOptions);
end;

procedure TWideString.Detach;
begin
  if (Capacity <> 0) and (FData[Count] <> #0) then
  begin
    if AttachBuffer then
      Capacity := Count + 1;
    FData[Count] := #0;
  end;
end;

function TWideString.GetData: PWideChar;
begin
  Detach;
  Result := FData;
end;

{$IFNDEF Lite}
class function TWideString.LengthOf(Source: Pointer): Integer;
begin
  Result := WideStrLen(Source);
end;
{$ENDIF}

function TWideString.NextIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
  CheckIndex(StartIndex);
  W := WideStrScan(FData + StartIndex, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

function TWideString.PrevIndex(Value: WideChar): Integer;
var
  W: PWideChar;
begin
  W := WideStrRScan(FData, Count, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

function TWideString.PrevIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
  CheckIndex(StartIndex);
  W := WideStrRScan(FData, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

procedure TWideString.Load(Source: PReadableStream);
begin
  Load(Source, boFromBOM);
end;

procedure TWideString.Load(Source: PReadableStream; ByteOrder: TByteOrder;
  FallbackCP: Word);
var
  BOM: TReadableBOM;
  S: TLegacyString;
  Length: Integer;
  CP: TCodePage;
  CPNum: Word;
begin
  Clear;
  if ByteOrder = boFromBOM then
  begin
    CPNum := CP_UTF8;
    BOM := Source.ReadBOM;
    case BOM of
      bomUTF16LE, bomUTF16BE:
        CPNum := 0;
      bomUTF7, bomUTF8:
        CPNum := CP_UTF7 + Byte(BOM) - Byte(bomUTF7);
      bomGB18030:
        CPNum := CP_GB18030;
    else
      if BOM <> bomNone then
        raise EUTF32.Create(@Self);
    end;

    if CPNum <> 0 then
    begin
      if CPNum = CP_UTF8 then
        CPNum := FallbackCP;
    {$IFDEF CoreLiteVCL}
      if CPNum = 0 then
        CPNum := DefaultUnicodeCodePage;
    {$ENDIF}
      CP.Create(CPNum);
      S.Create;
      try
        S.FCodePage := @CP;
        S.Load(Source, False);
        AsString(@S);
      finally
        S.Destroy;
      end;
      Exit;
    end
  end
  else
    BOM := bomNone;

  Length := (Source.Size - Source.Position) div SizeOf(WideChar);
  if Length <> 0 then
  begin
    Capacity := Length + 1;
    Source.ReadBuffer(FData^, Length * SizeOf(WideChar));
    FOptions := [];
    if (ByteOrder = boBigEndian) or (BOM = bomUTF16BE) then
      Include(FOptions, soBigEndian);
    Append(Length);
    FData[Length] := #0;
  end;
end;

procedure TWideString.Save(Dest: PWritableStream);
begin
  Save(Dest, True);
end;

procedure TWideString.Save(Dest: PWritableStream; WriteBOM: Boolean);
begin
  if Count <> 0 then
  begin
    if WriteBOM then
      Dest.WriteBOM(TWritableBOM(Byte(bomUTF16) + Byte(soBigEndian in FOptions)));
    Dest.WriteBuffer(FData^, Count * SizeOf(WideChar));
  end;
end;

procedure TWideString.SwapByteOrder(Index, Length: Integer);
var
  W: PWideChar;
begin
{$IFDEF Debug}
  CheckRange(Index, Length);
{$ENDIF}
  W := FData + Index;
  SwapWideCharBytes(W, W, Length);
end;

procedure TWideString.SwapByteOrder;
begin
  SwapByteOrder(0, Count);
  Byte(FOptions) := Byte(FOptions) xor (1 shl Byte(soBigEndian));
end;

procedure TWideString.SetData(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value), []); // Fast core
end;

{$IFDEF CoreLiteVCL}
function TWideString.GetRawByteString: RawByteString;
var
  S: TLegacyString;
{$IF not UnicodeRTL}
  CP: TCodePage;
{$IFEND}
begin
  S.Create;
  try
  {$IF not UnicodeRTL}
    CP.Create(DefaultUnicodeCodePage);
    S.FCodePage := @CP;
  {$IFEND}
    S.AsWideString(@Self);
    SetString(Result, S.FData, S.Count);
  {$IF UnicodeRTL}
    System.SetCodePage(Result, DefaultUnicodeCodePage, False);
  {$IFEND}
  finally
    S.Destroy;
  end;
end;

procedure TWideString.SetRawByteString(Value: RawByteString);
var
  S: TLegacyString;
  CP: TCodePage;
  CPNum: Word;
begin
  S.Create;
  try
  {$IF UnicodeRTL}
    CPNum := StringCodePage(Value);
  {$ELSE}
    CPNum := DefaultUnicodeCodePage;
    Include(S.FOptions, soDetectUTF8);
  {$IFEND}
    if CPNum <> CP_UTF8 then
    begin
      CP.Create(CPNum);
      S.FCodePage := @CP;
    end;
    S.Assign(Pointer(Value), Length(Value), soAttach);
    AsString(@S);
  finally
    S.Destroy;
  end;
end;

function TWideString.GetUnicodeString: UnicodeString;
begin
  System.SetString(Result, FData, Count);
end;

procedure TWideString.SetUnicodeString(Value: UnicodeString);
begin
  Assign(Pointer(Value), Length(Value), []);
end;
{$ENDIF}

function TWideString.Compare(Value: PWideChar; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
    Value, Length, FData, Count) - CSTR_EQUAL;
end;

function TWideString.Compare(Value: PWideString; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  if Value <> nil then
    Result := CompareStringW(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
      Value.RawData, Value.Count, FData, Count) - CSTR_EQUAL
  else
    Result := 1;
end;

{ TLegacyCommandLineParam }

function TLegacyCommandLineParam.AsNextParam(CommandLine: PLegacyString): TLegacyString;
begin
  Clear;
  Result.Create;
  Result.AsRange(CommandLine, 0);

  while (Result.Count <> 0) and (Result.RawData^ in [#32, #9]) do
    Result.Delete(0);

  if Result.Count <> 0 then
  begin
    if Result.RawData^ = '"' then
    begin
      Result.Delete(0);
      AsRange(@Result, 0, Result.NextIndex('"'));
      FQuoted := True;
      Result.Delete(0, Count + 1);
      Exit;
    end;

    AsRange(@Result, 0, False);
    while (Result.Count <> 0) and not (Result.RawData^ in [#32, #9]) do
      Result.Delete(0);

    if Result.Count <> 0 then
      Delete(Result.RawData - FData, Result.Count);
  end;
  FQuoted := False;
end;

{ TWideCommandLineParam }

function TWideCommandLineParam.AsNextParam(CommandLine: PWideString): TWideString;
begin
  Clear;
  Result.Create;
  Result.AsRange(CommandLine, 0);

  while (Result.Count <> 0) and ((Result.RawData^ = #32) or (Result.RawData^ = #9)) do
    Result.Delete(0);

  if Result.Count <> 0 then
  begin
    if Result.RawData^ = '"' then
    begin
      Result.Delete(0);
      AsRange(@Result, 0, Result.NextIndex(WideChar('"')));
      FQuoted := True;
      Result.Delete(0, Count + 1);
      Exit;
    end;

    AsRange(@Result, 0, False);
    while (Result.Count <> 0) and (Result.RawData^ <> #32) and (Result.RawData^ <> #9) do
      Result.Delete(0);

    if Result.Count <> 0 then
      Delete(Result.RawData - FData, Result.Count);
  end;
  FQuoted := False;
end;

end.

