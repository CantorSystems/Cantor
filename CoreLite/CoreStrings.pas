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
  soDetectUTF8:
    * try to decode source as UTF-8, continue as code page or Latin1 if code page is null
}
  TStringOption = (soDetectUTF8, soBigEndian, soAttachBuffer, soNullTerminated);
const
  soLatin1 = soBigEndian;
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
  TConvertOption = (coSurrogatePairs, coBigEndian, coReplaceInvalid, coConvert, coSysReplacementChar);
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
  TConvertUTF8 = coSurrogatePairs..coConvert;
  TConvertUTF16 = coSurrogatePairs..coReplaceInvalid;
//  TConvertUTF32 = set of coBigEndian..coReplaceInvalid;

  TEncodeOptions = set of TConvertOption;
  TEncodeCodePage = set of TConvertCodePage;
  TEncodeRawBytes = set of TConvertUTF8;
  TEncodeUTF16 = set of TConvertUTF16;
//  TEncodeUTF32 = TConvertUTF32;

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
      0: (RawData: Word);
      1: (StartingByte,           // Bad UTF-8 sequence starting with byte $%02X
          BrokenSequence: Byte);  // Broken %u-byte UTF-8 sequence starting with byte $%02X
  end;

  TErrorInfo = record
    case Byte of
      0: (RawData: QuadWord);
      1: (InvalidChar: QuadChar;
          InvalidUTF8: TInvalidUTF8);
  end;

  TConvertInfo = record
    SourceCount, DestCount, SuccessBytes, // SuccessBytes < 0 for Latin1
    SurrogatePairs: Integer;
    ErrorInfo: TErrorInfo;
  end;

  PLegacyString = ^TLegacyString;
  PWideString = ^TWideString;

  PCodePage = ^TCodePage;
  TCodePage = object
  private
    FLeadBytes: TLeadBytes;
    FName: PCoreChar;
    FNumber: Word;
    FMaxCharBytes: Byte;
    FReplacementChar, FSysReplacementChar: LegacyChar;
  public
    constructor Create(CodePage: Word = CP_ACP; DefaultReplacementChar: LegacyChar = LegacyReplacementChar);
    destructor Destroy;
    function DecodeUTF16(Source: PWideString; Dest: PLegacyString; DestIndex: Integer;
      DestOptions: TEncodeCodePage): Integer;
    function EncodeUTF16(Source: PLegacyString; Dest: PWideString; DestIndex: Integer;
      DestOptions: TEncodeUTF16): Integer;

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
    function AsArray(Index: Integer; const Values: array of const): Integer; overload;
    function AsInteger(Index: Integer; Value: Int64; MinWidth: Integer = 0;
      Hexadecimal: THexadecimal = hexNone; FillChar: WideChar = #32): Integer; overload;
    procedure Assign(Source: Pointer; Length: Integer; Options: TStringSource); overload;
  public
    procedure AsArray(const Values: array of const); overload;
    procedure AsInteger(Value: Int64; MinWidth: Integer = 0; Hexadecimal: THexadecimal = hexNone;
      FillChar: WideChar = #32); overload;
    function AsInteger(var Value: Int64; Hexadecimal: Boolean = False): Boolean; overload;
    function AsInteger(Hexadecimal: Boolean = False): Int64; overload;
    function Estimate(const Values: array of const): Integer;
    class function LengthOf(Source: Pointer): Integer; virtual; abstract;
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
    function GetString: PLegacyChar;
    procedure SetCodePage(Value: PCodePage);
    procedure SetString(Value: PLegacyChar);
  protected
    function AsWideString(Index: Integer; Source: PWideString;
      DestOptions: TEncodeRawBytes = []): TConvertInfo; overload;
  public
    constructor Create(CP: PCodePage = nil);

    procedure AsString(Source: PLegacyChar; CP: PCodePage = nil;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsString(Source: PLegacyChar; Length: Integer; CP: PCodePage = nil;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsWideString(Source: PWideString; DestOptions: TEncodeRawBytes = []); overload;

    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number
    class function LengthOf(Source: Pointer): Integer; virtual;

    function NextIndex(Index: Integer): Integer; overload;
    function NextIndex(Value: LegacyChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Index: Integer): Integer; overload;
    function PrevIndex(Value: LegacyChar): Integer; overload;
    function PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer; overload;

    procedure Load(Source: PReadableStream; AllowBOM: Boolean = True;
      SourceOptions: TRawByteOptions = soFromTheWild);
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean = False);

    property CodePage: PCodePage read FCodePage write SetCodePage;
    property Data: PLegacyChar read GetString write SetString;
    property Options: TRawByteOptions read FOptions;
    property RawData: PLegacyChar read FData write SetString;
  end;

  TByteOrder = (boLittleEndian, boBigEndian, boFromBOM);

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
    function GetString: PWideChar;
    procedure SetString(Value: PWideChar);
  protected
    function AsString(Index: Integer; Source: PLegacyString;
      DestOptions: TEncodeUTF16 = coUTF16): Integer; overload;
  public
    constructor Create;

    procedure AsString(Source: PLegacyString; DestOptions: TEncodeUTF16 = coUTF16); overload;
    procedure AsWideString(Source: PWideChar; SourceOptions: TEndianSource = []); overload;
    procedure AsWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource = []); overload;

    class function LengthOf(Source: Pointer): Integer; virtual;
    function NextIndex(Value: WideChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Value: WideChar): Integer; overload;
    function PrevIndex(Value: WideChar; StartIndex: Integer): Integer; overload;

    procedure Load(Source: PReadableStream; ByteOrder: TByteOrder = boFromBOM);
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean = True);

    procedure SwapByteOrder(Index, Length: Integer); overload; 
    procedure SwapByteOrder; overload;

    property Data: PWideChar read GetString write SetString;
    property Options: TEndianOptions read FOptions;
    property RawData: PWideChar read FData write SetString;
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

  TConvertMode = (cpDecodeUTF16, cpEncodeUTF16);

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
    constructor Create(Source: PString; ErrorCode: LongWord; CodePage: PCodePage;
      Mode: TConvertMode);
    property ErrorCode: LongWord read FErrorCode;
    property CodePage: PCodePage read FCodePage;
  end;

  TCharSet = (usLatin1, usUTF8, usCESU8, usUTF16);

  EUnicode = class(EConvert)
  private
    FErrorInfo: TErrorInfo;
    FCharSet: TCharSet;
  public
    constructor Create(Source: PString; CharSet: TCharSet; Mode: TConvertMode;
      const Info: TErrorInfo);
    property CharSet: TCharSet read FCharSet;
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

function DecodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;
function EncodeSurrogatePair(Source: QuadChar; Options: TSurrogatePairOptions = []): QuadChar;

function MaxCharBytes(Options: TEncodeRawBytes): Byte;
function TranslateOptions(Source: TRawByteOptions): TEncodeRawBytes;

procedure RaiseLastCodePageError(Source: PString; CodePage: PCodePage; Mode: TConvertMode);

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

function MaxCharBytes(Options: TEncodeRawBytes): Byte;
begin
  if coCESU8 in Options then
    Result := 3 * 2
  else
    Result := 3 * Byte(not (coLatin1 in Options));
end;

function TranslateOptions(Source: TRawByteOptions): TEncodeRawBytes;
begin
  Result := TEncodeRawBytes(Byte(Source) and Byte(soLatin1));
  if Result = [] then
    Include(Result, coSurrogatePairs);
end;

procedure RaiseLastCodePageError(Source: PString; CodePage: PCodePage; Mode: TConvertMode);
var
  ErrorCode: LongWord;
begin
  ErrorCode := GetLastError;
  if ErrorCode <> 0 then
    raise ECodePage.Create(Source, ErrorCode, CodePage, Mode);
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
      // W.AsWideString(@Source); TODO
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

constructor ECodePage.Create(Source: PString; ErrorCode: LongWord; CodePage: PCodePage;
  Mode: TConvertMode);
const
  ModeNames: array[TConvertMode] of PLegacyChar = (sUTF16fromCP, sCPtoUTF16);
var
  Msg: TCoreStringRec;
begin
  Msg := SysErrorMessage(ErrorCode);
  try
    inherited Create(ModeNames[Mode], CP_LEGACY, [Msg.Value, CodePage.Number, CodePage.Name]);
  finally
    LocalFree(Msg.Handle);
  end;
  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FErrorCode := ErrorCode;
  FCodePage := CodePage;
end;

{ EUnicode }

constructor EUnicode.Create(Source: PString; CharSet: TCharSet; Mode: TConvertMode; const Info: TErrorInfo);
const
  CharSetNames: array[TCharSet] of PLegacyChar = (sLatin1, sUTF8, sCESU8, sUTF16);
var
  Buf: array[0..Length(sNonBMP)+8] of LegacyChar;
  Msg: PLegacyChar;
begin
  if Info.InvalidUTF8.RawData <> 0 then
    with Info.InvalidUTF8 do
      if BrokenSequence <> 0 then
        inherited Create(sBrokenUTF8, [BrokenSequence, StartingByte])
      else
        inherited Create(sBadUTF8, [StartingByte])
  else
    case Info.InvalidChar of
      Low(THighSurrogates)..High(THighSurrogates):
        inherited Create(sBrokenSurrogatePair, [CharSetNames[CharSet], Info.InvalidChar]);
      Low(TLowSurrogates)..High(TLowSurrogates):
        inherited Create(sBadSurrogatePair, [CharSetNames[CharSet], Info.InvalidChar]);
    else
      case Info.InvalidChar of
        $80..$9F:
          Msg := sInvalidChar;
        Low(TUnicodeSMP)..High(TUnicodePUA):
          Msg := sNonBMP;
      else
        Msg := sNonUnicode;
      end;
      FormatBuf(Msg, [Info.InvalidChar], Buf);
      inherited Create(sInvalidString, [CharSetNames[CharSet], @Buf]);
    end;
  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FCharSet := CharSet;
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
  Info: TCPInfoEx;
  P: PLegacyChar;
begin
  FillChar(Self, SizeOf(Self), 0);
  if not GetCPInfoEx(CodePage, 0, Info) then
    RaiseLastPlatformError(sCodePage, CodePage);

  with Info do
  begin
    FNumber := CodePage;
    FMaxCharBytes := MaxCharSize;
    FSysReplacementChar := LegacyChar(DefaultChar[0]);

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
  FReplacementChar := DefaultReplacementChar;
end;

destructor TCodePage.Destroy;
begin
  FreeMem(FName);
end;

function TCodePage.DecodeUTF16(Source: PWideString; Dest: PLegacyString;
  DestIndex: Integer; DestOptions: TEncodeCodePage): Integer;
var
  Replacement: PLegacyChar;
  ReplacementUsed: LongBool;
begin
{$IFDEF Debug}
  if soBigEndian in Source.Options then
    raise Exception.Create(sUnsupportedUTF16BE);
{$ENDIF}

  if coReplaceInvalid in DestOptions then
    if coSysReplacementChar in DestOptions then
      Replacement := @FSysReplacementChar
    else
      Replacement := @FReplacementChar
  else
    Replacement := nil;

  Result := {$IFDEF Tricks} System. {$ENDIF} WideCharToMultiByte(
    FNumber, WC_NO_BEST_FIT_CHARS, Source.RawData, Source.Count,
    Dest.RawData + DestIndex, Dest.Capacity - DestIndex, Replacement, @ReplacementUsed
  );
  if ReplacementUsed then
    Result := -Result;
end;

function TCodePage.EncodeUTF16(Source: PLegacyString; Dest: PWideString;
  DestIndex: Integer; DestOptions: TEncodeUTF16): Integer;
begin
  Result := {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(
    FNumber, MB_ERR_INVALID_CHARS and (Integer(coReplaceInvalid in DestOptions) - 1),
    Source.FData, Source.Count, Dest.FData + DestIndex, Dest.Capacity - DestIndex
  );
  if coBigEndian in DestOptions then
    SwapWideCharBytes(Dest.FData + DestIndex, Dest.FData + DestIndex, Result);
end;

{ TString }

function TString.AsArray(Index: Integer; const Values: array of const): Integer;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  Result := 0; // TODO
end;

procedure TString.AsArray(const Values: array of const);
var
  Length: Integer;
begin
  Clear;
  Capacity := Estimate(Values) + SizeOf(WideChar);
  Length := AsArray(0, Values);
  Append(Length);
  PWideChar(PLegacyString(@Self).FData + Length * ItemSize)^ := #0;
end;

const
  MaxDigits = 20;

function TString.AsInteger(Index: Integer; Value: Int64; MinWidth: Integer;
  Hexadecimal: THexadecimal; FillChar: WideChar): Integer;
var
  Buf: array[1..MaxDigits] of LegacyChar;
  Digits: PLegacyChar;
  Minus: Boolean;
  LowerCaseMask: Word;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  if Hexadecimal <> hexNone then
  begin
    Digits := @Buf[High(Buf) - SizeOf(Word)];
    Result := 0;
    LowerCaseMask := (Byte(Hexadecimal) - 1) * $20;
    Inc(LowerCaseMask, LowerCaseMask shl 8);
    repeat
      PWord(Digits)^ := Byte(HexDigits[Byte(Value) shr 4]) or
        (Byte(HexDigits[Byte(Value) and $F]) shl 8) or LowerCaseMask;
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
  Length := AsInteger(0, Value, MinWidth, Hexadecimal, FillChar);
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
                Value := Value shl 4 + PByte(Digit)^ and UpperCaseMask - Byte('A') + 10;
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
      if (Digit^ in ['+', '-']) and ((ItemSize = 1) or (Digit[1] = #0)) then
        Inc(Digit, ItemSize);
      while Digit < Limit do
        if (Digit^ in ['0'..'9']) and ((ItemSize = 1) or (Digit[1] = #0)) then
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

procedure TString.Assign(Source: Pointer; Length: Integer; Options: TStringSource);
begin
  if Source <> nil then
    if soAttachBuffer in Options then
      inherited Assign(Source, Length, Length + Integer(soNullTerminated in Options), True)
    else
    begin
      inherited Assign(Source, Length, Length + SizeOf(WideChar), False);
      PWideChar(PLegacyString(@Self).FData + Length * ItemSize)^ := #0;
    end
  else
    Clear;
  PLegacyString(@Self).FOptions := Options - soBuiltIn;
end;

function TString.Estimate(const Values: array of const): Integer;
begin
  Result := 0; // TODO
end;

{ TLegacyString }

constructor TLegacyString.Create(CP: PCodePage);
begin
  inherited Create(sLegacyString, SizeOf(LegacyChar));
  FCodePage := CP;
end;

procedure TLegacyString.AsString(Source: PLegacyChar; CP: PCodePage; SourceOptions: TRawByteSource);
begin
  AsString(Source, StrLen(Source), CP, SourceOptions + [soNullTerminated]);
end;

procedure TLegacyString.AsString(Source: PLegacyChar; Length: Integer; CP: PCodePage;
  SourceOptions: TRawByteSource);
begin
  if ((FCodePage = nil) and (soDetectUTF8 in FOptions)) or
    ((FCodePage <> nil) and (CP <> nil) and (FCodePage.FNumber = CP.FNumber))
  then
    Assign(Source, Length, SourceOptions)
  else
    // TODO: convert
end;

function TLegacyString.AsWideString(Index: Integer; Source: PWideString;
  DestOptions: TEncodeRawBytes): TConvertInfo;

function NextChar(var Index: Integer): Word;
begin
  Result := Word(Source.FData[Index]);
  if soBigEndian in Source.FOptions then
    Result := Swap(Result);
  Inc(Index);
end;

var
  CodePoint: QuadChar;
  Paired, W: Word;
  Dest, Limit: PLegacyChar;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);
  Dest := FData + Index;
  Limit := FData + Capacity;

  while (Dest < Limit) and (Result.SourceCount < Source.Count) do
  begin
    CodePoint := NextChar(Result.SourceCount);

    case Word(CodePoint) of
      $00..$7F:
        begin
          if (CodePoint = 0) and (DestOptions * coModifiedUTF8 = coModifiedUTF8) then
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
          Paired := NextChar(Result.SourceCount);

          case Paired of
            Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                CodePoint := (CodePoint - Low(THighSurrogates)) shl 10 +
                  Low(TUnicodeSMP) + Paired - Low(TLowSurrogates);
                if coCESU8 in DestOptions then
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
                else if not (coLatin1 in DestOptions) then
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
                else if coReplaceInvalid in DestOptions then
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

      Low(TLowSurrogates)..High(TLowSurrogates), $FEFF: // UTF-16 Big-Endian BOM
        if coReplaceInvalid in DestOptions then
        begin
          if DestOptions * coModifiedUTF8 = [coLatin1] then
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
      if DestOptions * coModifiedUTF8 = [coLatin1] then
      begin
        case Word(CodePoint) of
          $A0..$FF:
            begin
              Dest^ := LegacyChar(CodePoint);
              Dec(Result.SuccessBytes);
            end;
        else
          if coReplaceInvalid in DestOptions then
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

procedure TLegacyString.AsWideString(Source: PWideString; DestOptions: TEncodeRawBytes);
begin
  Clear;
  Capacity := Source.Count * MaxCharBytes(DestOptions) + 1;
  with AsWideString(0, Source, DestOptions) do
    if ErrorInfo.RawData = 0 then
    begin
      Append(DestCount);
      FData[DestCount] := #0;
    end
    else
      // TODO: raise
end;

function TLegacyString.GetString: PLegacyChar;
begin
  if Capacity <> 0 then
  begin
    if AttachBuffer then
      Capacity := Count + 1;
    FData[Count] := #0;
  end;
  Result := FData;
end;

function TLegacyString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

class function TLegacyString.LengthOf(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;

function TLegacyString.NextIndex(Index: Integer): Integer;
begin
  CheckIndex(@Self, Index);
  Result := Index + 1;
  if (ItemSize = 1) and (FCodePage <> nil) then
    while not (FData[Index] in [#0..#127] + FCodePage.LeadBytes) do
      Inc(Result);
end;

function TLegacyString.NextIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
  CheckIndex(@Self, StartIndex);
  S := StrScan(FData, Count, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.PrevIndex(Index: Integer): Integer;
begin
  CheckIndex(@Self, Index);
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
  CheckIndex(@Self, StartIndex);
  S := StrRScan(FData, Count - StartIndex, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

procedure TLegacyString.Load(Source: PReadableStream; AllowBOM: Boolean;
  SourceOptions: TRawByteOptions);
var
  BOM: TReadableBOM;
  W: TWideString;
  Length: Integer;
  CP: TCodePage;
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
            W.Load(Source, TByteOrder(Byte(BOM) - Byte(bomUTF16LE)));
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
            try
              W.Create;
              try
                Source.ReadBuffer(FData^, Capacity);
                W.Capacity := Capacity;
                W.Append(CP.EncodeUTF16(@Self, @W, 0, []));
                if W.Count = 0 then
                  RaiseLastCodePageError(@Self, @CP, cpEncodeUTF16);
                AsWideString(@W, TranslateOptions(FOptions));
              finally
                W.Destroy;
              end;
            finally
              CP.Destroy;
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
            if (FCodePage <> nil) and (FCodePage.FNumber = CP_GB18030) then
            begin
              Append(Length);
              FData[Length] := #0;
            end
            else
            begin
              CP.Create(CP_GB18030);
              try
                W.Create;
                try
                  W.Capacity := Length;
                  W.Append(CP.EncodeUTF16(@Self, @W, 0, []));
                  if W.Count = 0 then
                    RaiseLastCodePageError(@Self, @CP, cpEncodeUTF16);
                  AsWideString(@W, TranslateOptions(FOptions));
                finally
                  W.Destroy;
                end;
              finally
                CP.Destroy;
              end;
            end;
          end;
          Exit;
        end;
    else
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
  end;
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
      else if FCodePage.Number = CP_GB18030 then
        Dest.WriteBOM(bomGB18030);
    Dest.WriteBuffer(FData^, Count);    
  end;
end;

procedure TLegacyString.SetCodePage(Value: PCodePage);
var
  Capture: TLegacyString;
begin
  if (FCodePage <> Value) and (Count <> 0) and
    ((FCodePage = nil) or (Value = nil) or (FCodePage.FNumber <> Value.FNumber)) then
  begin
    Capture.Create(FCodePage);
    try
      Capture.Assign(@Self, amCapture);
      FCodePage := Value;
      AsString(Capture.FData, Capture.Count, Capture.FCodePage, Capture.FOptions);
    finally
      Capture.Destroy;
    end;
  end
  else
    FCodePage := Value;
end;

procedure TLegacyString.SetString(Value: PLegacyChar);
begin
  AsString(Value, StrLen(Value));
end;

{ TWideString }

constructor TWideString.Create;
begin
  inherited Create(sWideString, SizeOf(WideChar));
end;

function TWideString.AsString(Index: Integer; Source: PLegacyString;
  DestOptions: TEncodeUTF16): Integer;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  if Source <> nil then
  begin


    if Source.FCodePage <> nil then
    begin
      if Source.Count <> 0 then
      begin
        Result := Source.FCodePage.EncodeUTF16(Source, @Self, Index, DestOptions);
        //if (Result = 0) and not (coReplaceInvalid in DestOptions) then
          // TODO: raise
        Exit;
      end;
    end;
  end;
  Result := 0; // TODO: temp
end;

procedure TWideString.AsString(Source: PLegacyString; DestOptions: TEncodeUTF16);
var
  Length: Integer;
begin
  Clear;
  Capacity := Source.Count + 1;
  Length := AsString(0, Source, DestOptions);
  Append(Length);
  FData[Length] := #0;
end;

procedure TWideString.AsWideString(Source: PWideChar; SourceOptions: TEndianSource);
begin
  Assign(Source, WideStrLen(Source), SourceOptions + [soNullTerminated]);
end;

procedure TWideString.AsWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource);
begin
  Assign(Source, Length, SourceOptions); 
end;

function TWideString.GetString: PWideChar;
begin
  if Capacity <> 0 then
  begin
    if AttachBuffer then
      Capacity := Count + 1;
    FData[Count] := #0;
  end;
  Result := FData;
end;

class function TWideString.LengthOf(Source: Pointer): Integer;
begin
  Result := WideStrLen(Source);
end;

function TWideString.NextIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
  CheckIndex(@Self, StartIndex);
  W := WideStrScan(FData, Count, Value);
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
  CheckIndex(@Self, StartIndex);
  W := WideStrRScan(FData, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

procedure TWideString.Load(Source: PReadableStream; ByteOrder: TByteOrder);
begin

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
  CheckRange(@Self, Index, Length);
  W := FData + Index;
  SwapWideCharBytes(W, W, Length);
  Byte(FOptions) := Byte(FOptions) xor (1 shl Byte(soBigEndian));
end;

procedure TWideString.SwapByteOrder;
begin
  SwapByteOrder(0, Count);
end;

procedure TWideString.SetString(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value), []); // Fast core
end;

end.

