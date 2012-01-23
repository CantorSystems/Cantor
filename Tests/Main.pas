(*
    Core Library Tests

    Copyright (c) 2009 The Unified Environment Laboratory
*)

unit Main;

interface

uses
  Windows, SysUtils, Core, Strings, Strings2, Exceptions, Lite;

type
  TStrings = array of WideString;

  TStringTest = class
  protected
    function DecodeUtf8(const Source: UTF8String): WideString; virtual; abstract;
    function EncodeUtf8(const Source: WideString): UTF8String; virtual; abstract;
  public
    function Execute(const Str: WideString; Step: Integer = 0): Int64; overload;
    function Execute(const Strings: TStrings; Step: Integer): Int64; overload;
  end;

  TCoreStringTest = class(TStringTest)
  private
    FUtf8: TUtf8Decoder;
    FUtf16: TUtf16Decoder;
  protected
    function DecodeUtf8(const Source: UTF8String): WideString; override;
    function EncodeUtf8(const Source: WideString): UTF8String; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRangeStringTest = class(TCoreStringTest)
  protected
    function DecodeUtf8(const Source: UTF8String): WideString; override;
    function EncodeUtf8(const Source: WideString): UTF8String; override;
  public
    constructor Create(const Options: TUtfOptions);
  end;

  TDelphiStringTest = class(TStringTest)
  protected
    function DecodeUtf8(const Source: UTF8String): WideString; override;
    function EncodeUtf8(const Source: WideString): UTF8String; override;
  end;

  TWindowsStringTest = class(TStringTest)
  protected
    function DecodeUtf8(const Source: UTF8String): WideString; override;
    function EncodeUtf8(const Source: WideString): UTF8String; override;
  end;

{ Main functions }

procedure Title;
procedure Run;

{ Service functions }

var
  PerfFreq: Int64;

function PerfValue(Source: Int64): AnsiString;

{ Unit tests }

procedure SingleStringTest(Str: PWideChar; LineBreak: Integer = 1);
procedure RandomStringTest(StepCount, Asterisk, MaxLength: Integer); overload;

procedure CharSetTest;

procedure LockTest;

var
  CharSets: TLegacyCharSets;

implementation

uses
  Containers;

{ Main functions }

procedure Title;
begin
  WriteLn('== The Unified Environment Core Tests ==', 2);
end;

procedure Run;
const
  Surrogate: array[0..2] of WideChar = (WideChar($D800), WideChar($DC00), WideChar(0));
  Malformed: array[0..2] of WideChar = (WideChar($800), WideChar($C00), WideChar(0));
begin
  LockTest;
{  CharSetTest;
  SingleStringTest('Mother ушла на работу, а потом вернулась домой.');
  SingleStringTest(Malformed, 2);
  //SingleStringTest(Surrogate);
  RandomStringTest($FFFF, $FFFF div 64, $400);}
end;

{ Unit tests }

procedure SingleStringTest(Str: PWideChar; LineBreak: Integer);
var
  Test: TStringTest;
begin
  Test := TRangeStringTest.Create([siRangeBlocks]);
  try
    WriteLn(Format('Single string test: %hs s', 0, [PerfValue(Test.Execute(Str))]), LineBreak);
  finally
    Test.Free;
  end;
end;

procedure RandomStringTest(StepCount, Asterisk, MaxLength: Integer);

var
  Strings: TStrings;
  TotalLength: Int64;
  Test: TStringTest;

procedure RunTest(Msg: PLegacyChar);
var
  Time: Int64;
begin
//  raise EExclusiveLock.Create(Test);
  WriteLn(Msg);
  Time := Test.Execute(Strings, Asterisk);
  WriteLn;
  WriteLn(Format('Elapsed time: %hs s, performance: %hs cps', 0, [
    PerfValue(Time), FloatToStr(TotalLength / (Time / PerfFreq), 2)
  ]), 2);
end;

var
  W: WideString;
  Step, Idx, L, C: Integer;
  Block: TUnicodeBlock;
  Min, Max: Word;
begin
  WriteLn('Initializing');
  TotalLength := 0;
  SetLength(Strings, StepCount);
  for Step := 0 to StepCount - 1 do
  begin
    SetLength(W, MaxLength);
    Idx := 0;
    while Idx < MaxLength do
    begin
      L := Random(16) + 1;
      if Idx + L > MaxLength then
        L := MaxLength - Idx;
      Block := TUnicodeBlock(Random(Byte(High(TUnicodeBMPBlockRange)) -
        Byte(Low(TUnicodeBMPBlockRange)) + 1) + Byte(Low(TUnicodeBMPBlockRange)));
      if Block in [cbHighSurrogates, cbHighPrivateSurrogates, cbLowSurrogates] then
        Block := cbBasicLatin;
//      Block := cbCyrillic;
      Min := UnicodeBlockRanges[Block].Min;
      Max := UnicodeBlockRanges[Block].Max;
      for C := 1 to L do
        W[Idx + C] := WideChar(Random(Max - Min + 1) + Integer(Min));
      Inc(Idx, L);
      Inc(TotalLength, L);
      L := Random(4) + 1;
      if Idx + L > MaxLength then
        L := MaxLength - Idx;
      for C := 1 to L do
        W[Idx + C] := WideChar(Random(127 - 32 + 1) + 32);
      Inc(Idx, L);
      Inc(TotalLength, L);
    end;
    Strings[Step] := W;
    if Step mod Asterisk = 0 then
      WriteLn('*', 1, 0);
  end;
  WriteLn(2);

  Test := TCoreStringTest.Create;
  try
    RunTest('Core string test');
  finally
    Test.Free;
  end;

  Test := TRangeStringTest.Create([siRangeBlocks]);
  try
    RunTest('Range string test');
  finally
    Test.Free;
  end;

  Test := TDelphiStringTest.Create;
  try
    RunTest('Delphi string test');
  finally
    Test.Free;
  end;

  Test := TWindowsStringTest.Create;
  try
    RunTest('Windows string test');
  finally
    Test.Free;
  end;
end;

{function MaxCharBytes(const Options): Byte;
begin
  Result := SizeOf(LegacyChar);
end;}

procedure CharSetTest;
var
  C: TLegacyCharSet;
  S: TString;
begin
  C := CharSets[1257];
{  with TCharSetString.Create do
  try
    WriteLn(IntToStr(MaxCharBytes), 1);
  finally
    Free;
  end;}
  if C is TDoubleByteCharSet then
  begin
    CharSets.NewString(S, 0, nil);
    S := nil;
    try
    finally
      S.Free;
    end;
  end;
//    WriteLn(TSingleByteCharSet(C).WideMap + $80, $80, 1);
end;

procedure LockTest;
var
  A: TSharedObject;
begin
  A := TSharedObject.Create;
  try
    A.Lock;
    try
    finally
      A.Unlock;
    end;
  finally
    A.Free;
  end;
end;

{ Service functions }

function PerfValue(Source: Int64): AnsiString;
begin
  Result := FloatToStr(Source / PerfFreq, 6);
end;

{ TStringTest }

function TStringTest.Execute(const Str: WideString; Step: Integer): Int64;
var
  Start, Finish: Int64;
  U, T: UTF8String;
  W: WideString;
begin
  Result := 0;

  QueryPerformanceCounter(Start);
  U := EncodeUtf8(Str);
  QueryPerformanceCounter(Finish);
  Inc(Result, Finish - Start);

  T := UTF8Encode(Str);
  if U <> T then
    raise EString.Create('UTF-8 encode failed at step %u', [Step]);

  QueryPerformanceCounter(Start);
  W := DecodeUtf8(U);
  QueryPerformanceCounter(Finish);
  Inc(Result, Finish - Start);

  if W <> Str then
    raise EString.Create('UTF-8 decode failed at step %u', [Step]);
end;

function TStringTest.Execute(const Strings: TStrings; Step: Integer): Int64;
var
  I, T: Integer;
begin
  Result := 0;
  for I := Low(Strings) to High(Strings) do
  begin
    T := I + Low(Strings);
    Inc(Result, Execute(Strings[I], T + 1));

    if T mod Step = 0 then
      WriteLn('*', 1, 0);
  end;
end;

{ TCoreStringTest }

constructor TCoreStringTest.Create;
begin
  FUtf8 := TUtf8Decoder.Create;
  FUtf16 := TUtf16Decoder.Create;
end;

destructor TCoreStringTest.Destroy;
begin
  FUtf16.Free;
  FUtf8.Free;
  inherited;
end;

function TCoreStringTest.DecodeUtf8(const Source: UTF8String): WideString;
begin
  FUtf8.SetData(Pointer(Source), Length(Source));
  SetLength(Result, FUtf8.Count);
  SetLength(Result, FUtf8.EncodeUtf16(Pointer(Result)));
end;

function TCoreStringTest.EncodeUtf8(const Source: WideString): UTF8String;
begin
  FUtf16.SetData(Pointer(Source), Length(Source));
  SetLength(Result, FUtf16.Count * 3);
  SetLength(Result, FUtf16.EncodeUtf8(Pointer(Result)));
end;

{ TRangeStringTest }

constructor TRangeStringTest.Create(const Options: TUtfOptions);
begin
  inherited Create;
  FUtf8.Options := Options;
end;

function TRangeStringTest.DecodeUtf8(const Source: UTF8String): WideString;
begin
  FUtf8.SetData(Pointer(Source), Length(Source));
  SetLength(Result, FUtf8.Count);
  with FUtf8.EncodeUtf16Ex(Pointer(Result)) do
    SetLength(Result, Count + SurrogateCount);
{  with FUtf8.CharInfo do
    SetLength(Result, Count + SurrogateCount);
  FUtf8.EncodeUtf16Ex(Pointer(Result));}
end;

function TRangeStringTest.EncodeUtf8(const Source: WideString): UTF8String;
begin
  FUtf16.SetData(Pointer(Source), Length(Source));
  SetLength(Result, FUtf16.Count * 3);
  with FUtf16.EncodeUtf8Ex(Pointer(Result)) do
    SetLength(Result, Count + Utf8Bytes);
{  with FUtf16.CharInfo do
    SetLength(Result, Count + Utf8Bytes);
  FUtf16.EncodeUtf8Ex(Pointer(Result));}
end;

{ TDelphiStringTest }

function TDelphiStringTest.DecodeUtf8(const Source: UTF8String): WideString;
begin
  Result := UTF8Decode(Source);
end;

function TDelphiStringTest.EncodeUtf8(const Source: WideString): UTF8String;
begin
  Result := UTF8Encode(Source);
end;

{ TWindowsStringTest }

function TWindowsStringTest.DecodeUtf8(const Source: UTF8String): WideString;
var
  L: Integer;
begin
  L := Length(Source);
  SetLength(Result, L);
  SetLength(Result, {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
    (CP_UTF8, MB_ERR_INVALID_CHARS, Pointer(Source), L, Pointer(Result), L));
end;

function TWindowsStringTest.EncodeUtf8(const Source: WideString): UTF8String;
var
  S, W: Integer;
begin
  W := Length(Source);
  S := W * 3;
  SetLength(Result, S);
  SetLength(Result, {$IFDEF Tricks} System. {$ENDIF} WideCharToMultiByte
    (CP_UTF8, 0, Pointer(Source), W, Pointer(Result), S, nil, nil));
end;

procedure Foo(Args: array of string);
var
I: Cardinal;
begin
  for I := Low(Args) to High(Args) do
    Windows.MessageBox(0, PChar(Args[I]), nil, 0);
end;

initialization
  CharSets := TLegacyCharSets.Create;
  QueryPerformanceFrequency(PerfFreq);

finalization
  CharSets.Free;

end.
