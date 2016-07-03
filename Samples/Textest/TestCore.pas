(*
    Textest core unit
*)


unit TestCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreStrings, CoreApp;

type
  TRunOption = (roPause, roOEM);
  TRunOptions = set of TRunOption;

  TCommand = (cmNone, cmInto, cmCP);

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FFallbackCP, FDummy: Word;
    FCounter: TPerformanceCounter;
    FSourceFileName, FIntoFileName: TCoreString;
    procedure ParseCommandLine(Source: PWideChar);
  protected
    property Dummy: Word read FDummy;
  public
    destructor Destroy;
    procedure Run(CommandLine: PCoreChar);
  end;

  ECommandLine = class(Exception);

  ECommandParam = class(ECommandLine)
  private
    FCommand: TCommand;
    FParameter: PCoreString;
  public
    constructor Create(Command: TCommand; Param: PCoreString = nil);
    property Command: TCommand read FCommand;
    property Parameter: PCoreString read FParameter;
  end;

  EFallbackCP = class(ECommandLine)
  private
    FUTF8: Boolean;
  public
    constructor Create(UTF8: Boolean);
    property UTF8: Boolean read FUTF8;
  end;

implementation

uses
  Windows, CoreClasses, CoreConsts, TestConsts, Classes;

{ ECommandParam }

constructor ECommandParam.Create(Command: TCommand; Param: PCoreString);
const
  Messages: array[TCommand] of PLegacyChar =
    (sSourceFileNameParam, sIntoFileNameParam, sFallbackCodePageParam);
begin
  if (Param <> nil) and (Param.Count <> 0) then
    inherited Create(sDuplicate, DefaultSystemCodePage, [Messages[Command], Param.Data])
  else
    inherited Create(sMissing, [Messages[Command]]);
  FCommand := Command;
  FParameter := Param;
end;

{ EFallbackCP }

constructor EFallbackCP.Create(UTF8: Boolean);
begin
  inherited Create(sIsNotAFallbackCP, [{CP_UTF}7 + Byte(UTF8)]);
  FUTF8 := UTF8;
end;

{ TApplication }

destructor TApplication.Destroy;
begin
  FIntoFileName.Finalize;
  FSourceFileName.Finalize;
  inherited;
end;

procedure TApplication.ParseCommandLine(Source: PWideChar);
var
  CmdLine, Key: TWideString;
  Param: TCommandLineParam; 
  ParamCount: Integer;
  W: PWideChar;
begin
  CmdLine.Create;
  CmdLine.AsWideString(Source, WideStrLen(Source), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name

  Key.Create;
  ParamCount := 0;
  repeat
    CmdLine := Param.AsNextParam(@CmdLine);
    if Param.Count = 0 then
      Break;

    Inc(ParamCount);
    Key.AsRange(@Param, 1);
    if Param.IsKey then
    begin
      W := sOEM;
      if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
      begin
        if FFallbackCP <> 0 then
        begin
          Param.Skip;
          raise ECommandParam.Create(cmCP, @Param);
        end;
        FFallbackCP := CP_OEMCP;
        Param.Clear;
      end
      else
      begin
        W := sPause;
        if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
        begin
          Include(FOptions, roPause);
          Param.Clear;
        end;
      end;
    end;

    if Param.IsKey then
    begin
      W := sCP;
      if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
      begin
        CmdLine := Param.AsNextParam(@CmdLine);
        if (Param.Count = 0) or (FFallbackCP <> 0) then
          raise ECommandParam.Create(cmCP, @Param);
        FFallbackCP := Param.AsInteger;
        if FFallbackCP - CP_UTF7 in [0..1] then
          raise EFallbackCP.Create(FFallbackCP = CP_UTF8);
        Param.Clear;
      end
      else
      begin
        W := sInto;
        if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if (Param.Count = 0) or (FIntoFileName.Count <> 0) then
            raise ECommandParam.Create(cmInto, @Param);
          with FIntoFileName do
          begin
            Create;
            AsRange(@Param, 0);
          end;
          Param.Clear;
        end;
      end;
    end;

    if Param.Count <> 0 then
    begin
      if FSourceFileName.Count <> 0 then
        raise ECommandParam.Create(cmNone, @Param);
      FSourceFileName.Create;
      FSourceFileName.AsRange(@Param, 0);
    end;
  until False;

  if ParamCount = 0 then
  begin
    if Console.Redirection = [] then
      Include(FOptions, roPause);
  end
  else if FSourceFileName.Count = 0 then
    raise ECommandParam.Create(cmNone);
end;

type
  TDelphiStrings = class(TStrings)
  private
    FList: Pointer;
    FCount, FCapacity: Integer;
  protected
    property List: Pointer read FList;
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity;
  end;

procedure TApplication.Run;

procedure Stats(Elapsed: Double; CharCount, AverageStringLength,
  Capacity, LineCount, AverageCount, ItemSize: Integer);
const
  TestNames: array[Boolean] of PLegacyChar = ('Delphi', 'CoreLite');
var
  Time, Throughput, Percent: string[20];
  EstimatedCount, Overhead: Integer;
begin
  Str(Elapsed/100:1:4, Time);
  Time[Length(Time) + 1] := #0;
  Str(CharCount / (1024 * 1024 * 1024) / Elapsed * 100:1:2, Throughput);
  Throughput[Length(Throughput) + 1] := #0;
  Console.WriteLn(sBytesLinesSecondsGBs, 0,
    [TestNames[AverageCount <> 0], CharCount, LineCount, @Time[1], @Throughput[1]]);

  if AverageStringLength <> 0 then
  begin
    Console.WriteLn(sAverageLineLength, 0, [AverageCount]);

    EstimatedCount := CharCount div AverageStringLength;
    Str((EstimatedCount - LineCount) / LineCount * 100:1:1, Percent);
    Percent[Length(Percent) + 1] := #0;
    Console.WriteLn(sEstimationVariance, 0, [@Percent[1], AverageStringLength, EstimatedCount]);
  end;

  Overhead := Capacity - LineCount;
  Str(Overhead / Capacity * 100:1:1, Percent);
  Percent[Length(Percent) + 1] := #0;
  Console.WriteLn(sCapacityOverhead, 0, [@Percent[1], Overhead * ItemSize]);

  if AverageStringLength <> 0 then
  begin
    EstimatedCount := CharCount + ItemSize;
    Overhead := Capacity * ItemSize;
  end
  else
  begin
    EstimatedCount := CharCount + ItemSize * 2;
    Overhead := CharCount + LineCount * ItemSize * 2 + Overhead * ItemSize;
  end;
  Str(Overhead / EstimatedCount * 100:1:1, Percent);
  Percent[Length(Percent) + 1] := #0;
  Console.WriteLn(sTotalOverhead, 0, [@Percent[1], Overhead]);
end;

var
  CP: TCodePage;
  S: TLegacyString;
  Text: TLegacyText;

procedure CoreLiteTest(AverageStringLength: Integer);
var
  StartTime: QuadWord;
begin
  Console.WriteLn;
  try
    Text.Capacity := 0;
    StartTime := FCounter.Value;
    with Text do
    begin
      Capacity := S.Count div AverageStringLength;
      Delta := AverageStringsDelta;
    end;
    LoadText(Text.Append, @S, [toClear, toAttach]);
    Stats(FCounter.MillisecondsFrom(StartTime), S.Count, AverageStringLength,
      Text.Capacity, Text.Count, AverageCount(Text.FirstForward), SizeOf(TLegacyString));
  except
    on E: TObject do
    begin
      Console.WriteLn(sCoreLiteTestFailed, 0, [AverageStringLength]);
      ShowException(E);
    end;
  end;
end;

procedure DelphiTest;
var
  Strings: TStrings;
  StartTime: QuadWord;
  Txt: string;
begin
  Console.WriteLn;
  try
    Strings := TStringList.Create;
    try
      SetString(Txt, S.RawData, S.Count);
      if S.Count > 100 * 1024 * 1024 then
        S.Capacity := 0;
      StartTime := FCounter.Value;
      Strings.Text := Txt;
      Stats(FCounter.MillisecondsFrom(StartTime), Length(Txt), 0,
        TDelphiStrings(Strings).Capacity, Strings.Count, 0, SizeOf(Pointer));
    finally
      Strings.Free;
    end;
  except
    on E: TObject do
    begin
      Console.WriteLn(PLegacyChar(sDelphiTestFailed), StrLen(sDelphiTestFailed));
      ShowException(E);
    end;
  end;
end;

begin
  Console.WriteLn(PLegacyChar(sTitle), StrLen(sTitle), 2);
  ParseCommandLine(CommandLine);

  if FSourceFileName.Count <> 0 then
  begin
    if FFallbackCP <> 0 then
    begin
      CP.Create(FFallbackCP);
      Console.WriteLn(sFallbackCP, 0, [CP.Number, CP.Name]);
    end;

    Console.WriteLn(sFileNameFmt, 0, [PLegacyChar(sSourceFile), FSourceFileName.Data]);

    S.Create;
    try
      if FFallbackCP <> 0 then
        S.CodePage := @CP;
      LoadFile(S.Load, FSourceFileName.Data);
      Text.Create;
      try
        FCounter.Create;
        CoreLiteTest(25);
        CoreLiteTest(30);
        CoreLiteTest(64);
        CoreLiteTest(100);
        CoreLiteTest(200);
        if S.Count > 100 * 1024 * 1024 then
          Text.Capacity := 0; // let's make a hothouse for Delphi :-)
        DelphiTest;

        if (FIntoFileName.Count <> 0) and (Text.Count <> 0) then
        begin
          Console.WriteLn;
          Console.WriteLn(sFileNameFmt, 0, [PLegacyChar(sSavingInto), FIntoFileName.Data]);
          //SaveFile(Text.Save, FIntoFileName.RawData, Text.EstimateText.EstimatedLength);
        end;
      finally
        Text.Destroy;
      end;
    finally
      S.Destroy;
    end;
  end
  else
    Help(sUsage, sHelp);
end;

end.

