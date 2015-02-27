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
  TStringOption = (soDetectCharSet, soBigEndian, soAttachBuffer, soNullTerminated);
const
  soLatin1 = soBigEndian;
  soBuiltIn = [soAttachBuffer, soNullTerminated];
  soFromTheWild = [soDetectCharSet];

type
  TRawByteOptions = set of soDetectCharSet..soLatin1;
  TEndianOptions = set of soBigEndian..soBigEndian;

  TStringSource = set of TStringOption;
  TRawByteSource = set of soDetectCharSet..soNullTerminated;
  TEndianSource = set of soBigEndian..soNullTerminated;
{
  coForceInvalid -- replace invalid characters with:
    * U+007F -- for code page (used instead of U+001A to avoid compatibility issues)
    * U+FFFD -- for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coContinuous, coSurrogates, coBigEndian, coReplaceInvalid,
    coSysReplacementChar);
const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogates;
  coEncodeZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodeZero];  // UTF-8 compliance

type
  TConvertRawBytes = coLatin1..coReplaceInvalid;
  TConvertCodePaged = coReplaceInvalid..coSysReplacementChar;
  TConvertUTF8 = coContinuous..coReplaceInvalid;
  TConvertUTF16 = coContinuous..coReplaceInvalid;
//  TConvertUTF32 = set of coBigEndian..coReplaceInvalid;

  TEncodeOptions = set of TConvertOption;
  TEncodeCodePaged = set of TConvertCodePaged;
  TEncodeUTF8 = set of TConvertUTF8;
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
      DestOptions: TEncodeCodePaged): Boolean;
    function EncodeUTF16(Source: PLegacyString; Dest: PWideString; DestIndex: Integer;
      DestOptions: TEncodeUTF16): Integer;

    property LeadBytes: TLeadBytes read FLeadBytes;
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: PCoreChar read FName;
    property Number: Word read FNumber;
    property ReplacementChar: LegacyChar read FReplacementChar write FReplacementChar;
    property SysRelpacementChar: LegacyChar read FSysReplacementChar;
  end;

  TRawOptions = TRawByteOptions;
  TStringData = record
    case Byte of
      0: (RawString: Pointer; { hold to TCollection! }
          RawOptions: TRawByteOptions);
      1: (LegacyString: PLegacyChar;
          LegacyStringOptions: TRawByteOptions);
      2: (WideString: PWideChar;
          WideStringOptions: TEndianOptions);
  end;

  PString = ^TString;
  TString = object(TCollection)
  protected // prevent stupid warnings
    FData: TStringData;
    function AsArray(Index: Integer; const Values: array of const): Integer; overload;
    function AsInteger(Index: Integer; Value: Int64; Hexadecimal: Boolean = False;
      MinWidth: Integer = 0; FillChar: WideChar = #32): Integer; overload;
    procedure Assign(Source: Pointer; Length: Integer; Options: TStringSource); overload;
  public
    procedure AsArray(const Values: array of const); overload;
    procedure AsInteger(Value: Int64; Hexadecimal: Boolean = False; MinWidth: Integer = 0;
      FillChar: WideChar = #32); overload;
    function Estimate(const Values: array of const): Integer;
    class function LengthOf(Source: Pointer): Integer; virtual; abstract;
    function IsUTF8(ThresholdBytes: Integer = 4): Boolean; // TODO: magic number

    function NextIndex(Index: Integer): Integer;
    function PrevIndex(Index: Integer): Integer;

    property RawOptions: TRawByteOptions read FData.RawOptions;
    property RawString: Pointer read FData.RawString;
  end;

  TNumberFormat = (nfFillChar, nfThousandsSeparator, nfDecimalSeparator);
  TIntegerFormat = nfFillChar..nfThousandsSeparator;

  TLegacyCharNumberFormat = array[TNumberFormat] of LegacyChar;
  TLegacyCharIntegerFormat = array[TIntegerFormat] of LegacyChar;

  TLegacyString = object(TString)
  private
    FCodePage: PCodePage;
    function GetString: PLegacyChar;
    procedure SetString(Value: PLegacyChar);
  protected
    function AsWideString(Index: Integer; Source: PWideString;
      DestOptions: TEncodeUTF8 = []): Integer; overload;
  public
    constructor Create(CP: PCodePage = nil);

    procedure AsString(Source: PLegacyChar; CP: PCodePage = nil;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsString(Source: PLegacyChar; Length: Integer; CP: PCodePage = nil;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsWideString(Source: PWideString; DestOptions: TEncodeUTF8 = []); overload;

    class function LengthOf(Source: Pointer): Integer; virtual;
    function NextIndex(Value: LegacyChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Value: LegacyChar): Integer; overload;
    function PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer; overload;

    property CodePage: PCodePage read FCodePage {write FCodePage};
    property Data: PLegacyChar read GetString write SetString;
    property Options: TRawByteOptions read FData.LegacyStringOptions;
    property RawData: PLegacyChar read FData.LegacyString write SetString;
  end;

  PEndianString = ^TEndianString;
  TEndianString = object(TString)
  public
    procedure SwapByteOrder(Index, Length: Integer); overload;
    procedure SwapByteOrder; overload;
  end;

  TWideCharNumberFormat = array[TNumberFormat] of WideChar;
  TWideCharIntegerFormat = array[TIntegerFormat] of WideChar;

  TWideString = object(TEndianString)
  private
    function GetString: PWideChar;
    procedure SetString(Value: PWideChar);
  protected
    function AsString(Index: Integer; Source: PLegacyString;
      DestOptions: TEncodeUTF16 = []): Integer; overload;
  public
    constructor Create;

    procedure AsString(Source: PLegacyString; DestOptions: TEncodeUTF16 = []); overload;
    procedure AsWideString(Source: PWideChar; SourceOptions: TEndianSource = []); overload;
    procedure AsWideString(Source: PWideChar; Length: Integer; SourceOptions: TEndianSource = []); overload;

    class function LengthOf(Source: Pointer): Integer; virtual;
    function NextIndex(Value: WideChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Value: WideChar): Integer; overload;
    function PrevIndex(Value: WideChar; StartIndex: Integer): Integer; overload;

    property Data: PWideChar read GetString write SetString;
    property Options: TEndianOptions read FData.WideStringOptions;
    property RawData: PWideChar read FData.WideString write SetString;
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

function MaxCharBytes(Options: TEncodeUTF8): Byte;

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

function MaxCharBytes(Options: TEncodeUTF8): Byte;
begin
  if coCESU8 in Options then
    Result := 3 * 2
  else
    Result := 3 * Byte(not (coLatin1 in Options));
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

{ TCodePage }

constructor TCodePage.Create(CodePage: Word; DefaultReplacementChar: LegacyChar);
var
  Info: TCPInfoEx;
  P: PLegacyChar;
begin
  FillChar(Self, SizeOf(Self), 0);
  if not GetCPInfoEx(CodePage, 0, Info) then
    RaiseLastPlatformError {$IFDEF Debug} (sCodePage, CodePage) {$ENDIF} ;

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
  DestIndex: Integer; DestOptions: TEncodeCodePaged): Boolean;
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

  Dest.Insert(DestIndex, {$IFDEF Tricks} System. {$ENDIF} WideCharToMultiByte(
    FNumber, WC_NO_BEST_FIT_CHARS, Source.RawData, Source.Count,
    Dest.RawData + DestIndex, Dest.Capacity - DestIndex, Replacement, @ReplacementUsed
  ));
  Result := ReplacementUsed;
end;

function TCodePage.EncodeUTF16(Source: PLegacyString; Dest: PWideString;
  DestIndex: Integer; DestOptions: TEncodeUTF16): Integer;
begin
  Result := {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(
    FNumber, MB_ERR_INVALID_CHARS and (Integer(coReplaceInvalid in DestOptions) - 1),
    Source.FData.LegacyString, Source.Count, Dest.FData.WideString + DestIndex, Dest.Capacity - DestIndex
  );
  if (Result = 0) and (Source.Count <> 0) then
    RaiseLastPlatformError(sEncodingUTF16fromCP, FNumber);
  if coBigEndian in DestOptions then
    SwapWideCharBytes(Dest.FData.WideString + DestIndex, Dest.FData.WideString + DestIndex, Result);
  Dest.Insert(DestIndex, Result);
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
  PWideChar(PAddress(FData.RawString) + Length * ItemSize)^ := #0;
end;

const
  MaxDigits = 20;

function TString.AsInteger(Index: Integer; Value: Int64; Hexadecimal: Boolean;
  MinWidth: Integer; FillChar: WideChar): Integer;
var
  Buf: array[1..MaxDigits] of LegacyChar;
  Digits: PLegacyChar;
  Minus: Boolean;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  if Hexadecimal then
  begin
    Digits := @Buf[High(Buf) - SizeOf(Word)];
    Result := 0;
    repeat
      PWord(Digits)^ := Byte(HexDigits[Byte(Value) shr 4]) or (Byte(HexDigits[Byte(Value) and $F]) shl 8);
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
    if Result < Abs(MinWidth) then
    begin
      if MinWidth < 0 then
      begin
        System.FillChar(FData.LegacyString[Index], Abs(MinWidth) - Result, FillChar);
        Move(Digits^, FData.LegacyString[Index + Abs(MinWidth) - Result], Result);
      end
      else
      begin
        Move(Digits^, FData.LegacyString[Index], Result);
        System.FillChar(FData.LegacyString[Index + Result], MinWidth - Result, FillChar);
      end;
      Result := Abs(MinWidth);
    end
    else
      Move(Digits^, FData.LegacyString[Index], Result)
  else
    if Result < Abs(MinWidth) then
    begin
      if MinWidth < 0 then
      begin
        FillWideChar(FData.WideString[Index], Abs(MinWidth) - Result, FillChar);
        MoveZeroExpand(Digits^, FData.WideString[Index + Abs(MinWidth) - Result], Result);
      end
      else
      begin
        MoveZeroExpand(Digits^, FData.WideString[Index], Result);
        FillWideChar(FData.LegacyString[Index + Result], MinWidth - Result, FillChar);
      end;
      Result := Abs(MinWidth);
    end
    else
      MoveZeroExpand(Digits^, FData.WideString[Index], Result);
end;

procedure TString.AsInteger(Value: Int64; Hexadecimal: Boolean; MinWidth: Integer; FillChar: WideChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > MaxDigits then
    Length := Abs(MinWidth)
  else
    Length := MaxDigits;
  Capacity := Length + SizeOf(WideChar);
  Length := AsInteger(0, Value, Hexadecimal, MinWidth, FillChar);
  Append(Length);
  PWideChar(PAddress(FData.RawString) + Length * ItemSize)^ := #0;
end;

procedure TString.Assign(Source: Pointer; Length: Integer; Options: TStringSource);
begin
  if Source <> nil then
    if soAttachBuffer in Options then
      inherited Assign(Source, Length, Length + Integer(soNullTerminated in Options), True)
    else
    begin
      inherited Assign(Source, Length, Length + SizeOf(WideChar), False);
      PWideChar(FData.LegacyString + Length * ItemSize)^ := #0;
    end
  else
    Clear;
  FData.RawOptions := Options {$IFDEF Debug} - soBuiltIn {$ENDIF} ;
end;

function TString.Estimate(const Values: array of const): Integer;
begin
  Result := 0; // TODO
end;

function TString.IsUTF8(ThresholdBytes: Integer): Boolean;
begin
  Result := False; // TODO
end;

function TString.NextIndex(Index: Integer): Integer;
begin
  CheckIndex(@Self, Index);
  Result := Index + 1;
  if (ItemSize = 1) and (PLegacyString(@Self).FCodePage <> nil) then
    while not (FData.LegacyString[Index] in [#0..#127] + PLegacyString(@Self).FCodePage.LeadBytes) do
      Inc(Result);
end;

function TString.PrevIndex(Index: Integer): Integer;
begin
  CheckIndex(@Self, Index);
  Result := Index - 1;
  if (ItemSize = 1) and (PLegacyString(@Self).FCodePage <> nil) then
    while not (FData.LegacyString[Index] in [#0..#127] + PLegacyString(@Self).FCodePage.LeadBytes) do
      Dec(Result);
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
  if ((FCodePage = nil) and (soDetectCharSet in FData.RawOptions)) or
    ((FCodePage <> nil) and (CP <> nil) and (FCodePage.FNumber = CP.FNumber))
  then
    Assign(Source, Length, SourceOptions)
  else
    // TODO: convert
end;

function TLegacyString.AsWideString(Index: Integer; Source: PWideString;
  DestOptions: TEncodeUTF8): Integer;
begin
{$IFDEF Debug}
  CheckIndex(@Self, Index);
{$ENDIF}
  Result := 0;
end;

procedure TLegacyString.AsWideString(Source: PWideString; DestOptions: TEncodeUTF8);
var
  Length: Integer;
begin
  Clear;
  Capacity := Source.Count * MaxCharBytes(DestOptions) + 1;
  Length := AsWideString(0, Source, DestOptions);
  FData.LegacyString[Length] := #0;
end;

function TLegacyString.GetString: PLegacyChar;
begin
  if Capacity <> 0 then
  begin
    Capacity := Capacity; // detach buffer
    FData.LegacyString[Count] := #0;
  end;
  Result := FData.LegacyString;
end;

class function TLegacyString.LengthOf(Source: Pointer): Integer;
begin
  Result := StrLen(Source);
end;

function TLegacyString.NextIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
  CheckIndex(@Self, StartIndex);
  S := StrScan(FData.LegacyString, Count, Value);
  if S <> nil then
    Result := S - FData.LegacyString
  else
    Result := -1;
end;

function TLegacyString.PrevIndex(Value: LegacyChar): Integer;
var
  S: PLegacyChar;
begin
  S := StrRScan(FData.LegacyString, Count, Value);
  if S <> nil then
    Result := S - FData.LegacyString
  else
    Result := -1;
end;

function TLegacyString.PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
  CheckIndex(@Self, StartIndex);
  S := StrRScan(FData.LegacyString, Count - StartIndex, Value);
  if S <> nil then
    Result := S - FData.LegacyString
  else
    Result := -1;
end;

procedure TLegacyString.SetString(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), soFromTheWild);
end;

{ TEndianString }

procedure TEndianString.SwapByteOrder(Index, Length: Integer);
var
  W: PWideChar;
begin
  CheckRange(@Self, Index, Length);
  W := FData.WideString + Index;
  SwapWideCharBytes(W, W, Length);
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
  FData.WideString[Length] := #0;
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
    Capacity := Capacity; // detach buffer
    FData.WideString[Count] := #0;
  end;
  Result := FData.WideString;
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
  W := WideStrScan(FData.WideString, Count, Value);
  if W <> nil then
    Result := W - FData.WideString
  else
    Result := -1;
end;

function TWideString.PrevIndex(Value: WideChar): Integer;
var
  W: PWideChar;
begin
  W := WideStrRScan(FData.WideString, Count, Value);
  if W <> nil then
    Result := W - FData.WideString
  else
    Result := -1;
end;

function TWideString.PrevIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
  CheckIndex(@Self, StartIndex);
  W := WideStrRScan(FData.WideString, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData.WideString
  else
    Result := -1;
end;

procedure TWideString.SetString(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value), []);
end;

end.

