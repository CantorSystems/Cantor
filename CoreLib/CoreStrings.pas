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
  Windows, CoreUtils, CoreClasses;

type
  TStringOption = (soBigEndian, soAttachBuffer, soDetectCharSet);
const
  soLatin1 = soBigEndian;

type
  TStringOptions = set of soBigEndian..soAttachBuffer;
  TCodePageOptions = set of soAttachBuffer..soDetectCharSet;

  TConvertOption = (coCase, coKana, coPunctuation, coNonSpace, coWidth, coHanzi, coTurkic,
    coDiacritics, coComposition, coForceInvalid, coBigEndian, coNoSurrogates, coRangeBlocks);
const
  coLatin1 = coBigEndian;
  coAllowNonUnicode = coNoSurrogates;
  coCESU8 = coNoSurrogates;
  coModifiedUTF8 = coBigEndian;

type
  TEncodeLegacy = set of coNonSpace..coForceInvalid;
  TEncodeEndian = set of coForceInvalid..coNoSurrogates;
  TEncodeLatin = set of coForceInvalid..coLatin1;
  TEncodeUTF8 = set of coForceInvalid..coCESU8;

  TNormalForm = (NFC, NFD, NFKC, NFKD);

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
  TCodePage = class(TSharedObject)
  private
    FNumber: Word;
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
  {$ENDIF}

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeLegacy = []): Cardinal; overload;

    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PWideChar; Options: TEncodeLegacy = []): Cardinal; overload;

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; overload; virtual; abstract;
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PQuadChar; Options: TEncodeLegacy = []): Cardinal; overload;
  {$ENDIF}

  {$IFDEF Latin}
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLatin = []): Cardinal; overload; virtual; abstract;
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      Options: TEncodeLatin = []): Cardinal; overload;
  {$ENDIF}

    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeUTF8 = []): Cardinal; overload;

    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
      Source: PLegacyChar; Options: TEncodeEndian= []): Cardinal; overload;

  {$IFDEF UTF32}
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; overload; virtual; abstract;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      Options: TEncodeEndian = []): Cardinal; overload;
  {$ENDIF}

  // properties
    property Number: Word read FNumber;
  {$IFNDEF Lite}
    property Blocks: TUnicodeBlocks read FBlocks;
  {$ENDIF}
  end;

  TWideString = class;

  // mapping from 0 for EBCDIC compliance
  TSingleByteMap = array[LegacyChar] of WideChar;

  TMemoryCodePage = class(TCodePage)
  private
  {$IFDEF Lite}
    FName: PWideChar;
  {$ELSE}
    FName: TWideString;
  {$ENDIF}
    FSingleByteMap: TSingleByteMap;
    FWideMapLo, FWideMapHi: WideChar;
    function GetWideMapCount: Word;
  protected
    property SingleByteMap: TSingleByteMap read FSingleByteMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;

  // properties
    property Name: {$IFDEF Lite} PWideChar {$ELSE} TWideString {$ENDIF} read FName;

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

  {$IFDEF Latin}
    function DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLatin = []): Cardinal; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;
  {$ENDIF}

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;

    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; override;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;

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
  TWideMap = array[WideChar] of DoubleByteChar;

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
      SourceCount: Cardinal; Options: TEncodeLatin = []): Cardinal; override;
  {$ENDIF}

  {$IFDEF UTF32}
    function DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal; Source: PQuadChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;
  {$ENDIF}

    function DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;
    function DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal; Source: PWideChar;
      SourceCount: Cardinal; Options: TEncodeLegacy = []): Cardinal; override;

    function EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeUTF8 = []): Cardinal; override;
    function EncodeUTF16(Dest: PWideChar; DestCount: Cardinal; Source: PLegacyChar;
      SourceCount: Cardinal; Options: TEncodeEndian = []): Cardinal; override;

  // properties
    property SingleByteMap;
    property DoubleByteMap: TDoubleByteMap read FDoubleByteMap;
    property WideMap: PWideMap read FWideMap;
  end;

{ Strings }

  TString = class(TSharedObject)
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

  TMemoryString = class(TString)
  private
  {$IFNDEF Lite}
    FLanguage: Word;
  {$ENDIF}
    procedure SetCount(Value: Cardinal);
  public
    constructor Create(Source: Pointer; Options: TStringOptions = []); overload;
    constructor Create(Source: Pointer; Count: Cardinal;
      Options: TStringOptions = []); overload;

    procedure Assign(Value: Pointer; Count: Cardinal; Options: TStringOptions = []);

    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    function ByteCount: Cardinal; overload; override;

    class function Length(Source: Pointer): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; virtual; abstract;
  {$ENDIF}
  // properties
    property Count write SetCount;
  {$IFNDEF Lite}
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
    FOptions: TStringOptions;
  public
  // properties
    property Options: TStringOptions read FOptions;
  end;

  TCodePageString = class(TLegacyString)
  private
    FOptions: TCodePageOptions;
  public
  // properties
    property Options: TCodePageOptions read FOptions;
  end;

  TUTF8String = class(TLegacyString)
  private
    FOptions: TStringOptions;
  public
  // properties
    property Options: TStringOptions read FOptions;
  end;

  TWideString = class(TMemoryString)
  private
    FData: PWideChar;
    FOptions: TStringOptions;
    procedure SetData(Value: PWideChar);
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PWideChar read FData write SetData;
    property Options: TStringOptions read FOptions;
  end;

  TQuadString = class(TMemoryString)
  private
    FData: PQuadChar;
    FOptions: TStringOptions;
    procedure SetData(Value: PQuadChar);
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Source: Pointer): Cardinal; overload; override;
  {$IFNDEF Lite}
    class function Length(Source: Pointer; MaxLength: Cardinal): Cardinal; overload; override;
  {$ENDIF}
  // properties
    property Data: PQuadChar read FData write SetData;
    property Options: TStringOptions read FOptions;
  end;

implementation

uses
  Exceptions;

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

{ TCodePage }

{$IFDEF Latin}
function TCodePage.DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeLatin(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLatin): Cardinal;
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

function TCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeUTF8(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; Options: TEncodeLegacy): Cardinal;
begin
  Result := DecodeUTF16(Dest, DestCount, Source, WideStrLen(Source), Options);
end;

function TCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeUTF8): Cardinal;
begin
  Result := EncodeUTF8(Dest, DestCount, Source, StrLen(Source), Options);
end;

function TCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; Options: TEncodeEndian): Cardinal;
begin
  Result := EncodeUTF16(Dest, DestCount, Source, StrLen(Source), Options);
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
{$IFDEF Lite}
  FName := WideStrNew(Info.CodePageName);
{$ELSE}
  FName := TWideString.Create(@Info.CodePageName,
    WideStrLen(Info.CodePageName, Length(Info.CodePageName)));
{$ENDIF}

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
end;

destructor TMemoryCodePage.Destroy;
begin
{$IFDEF Lite}
  FreeMem(FName);
{$ELSE}
  FName.Free;
{$ENDIF}
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
      if W >= FWideMapLo then
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

{$IFDEF Latin}
function TSingleByteMemCodePage.DecodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
var
  I: Integer;
  C: LegacyChar;
begin
  if DestCount > SourceCount then
    DestCount := SourceCount;

  for I := 0 to DestCount - 1 do
  begin
    C := Source[I];
    if ((C in [#0..#$7F]) or ((C in [#$A0..#$FF]) and (coLatin1 in Options))) and
     (WideChar(C) >= FWideMapLo) and (WideChar(C) <= FWideMapHi)
    then
      Dest[I] := FWideMap[Word(C) - Word(FWideMapLo)]
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

function TSingleByteMemCodePage.EncodeLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLatin): Cardinal;
begin

end;
{$ENDIF}

{$IFDEF UTF32}
function TSingleByteMemCodePage.DecodeUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TSingleByteMemCodePage.EncodeUTF32(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
begin

end;
{$ENDIF}

function TSingleByteMemCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TSingleByteMemCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TSingleByteMemCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeUTF8): Cardinal;
begin

end;

function TSingleByteMemCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
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
      FWideMap[WideChar(Word(W) - Word(FWideMapLo))].SingleByte := L;
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
          with FWideMap[WideChar(Word(W) - Word(FWideMapLo))] do
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
        FWideMap[WideChar(Word(W) - Word(FWideMapLo))].SingleByte := L;
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
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLatin): Cardinal;
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

function TDoubleByteMemCodePage.DecodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.DecodeUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; Options: TEncodeLegacy): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeUTF8): Cardinal;
begin

end;

function TDoubleByteMemCodePage.EncodeUTF16(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; Options: TEncodeEndian): Cardinal;
begin

end;

{ TMemoryString }

constructor TMemoryString.Create(Source: Pointer; Options: TStringOptions);
begin
  Assign(Source, Length(Source), Options);
end;

constructor TMemoryString.Create(Source: Pointer; Count: Cardinal;
  Options: TStringOptions);
begin
  Assign(Source, Count, Options);
end;

function TMemoryString.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

procedure TMemoryString.SetCount(Value: Cardinal);
begin
  Assign(nil, Value);
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

