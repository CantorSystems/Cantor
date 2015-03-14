(*
    Textest core unit
*)


unit TestCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
  TRunOption = (roPause, roOEM);
  TRunOptions = set of TRunOption;

  TCommand = (cmNone, cmInto, cmCP);

  TApplication = object
  private
    FCounter: TPerformanceCounter;
    FConsole: TStreamConsole;
    FAppName, FSourceFileName, FIntoFileName: TCoreString;
    FFallbackCP: Word;
    FOptions: TRunOptions;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy;
    procedure Pause;
    procedure Run;
    procedure Help;
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
  Windows, CoreConsts, TestConsts, Classes;

{ ECommandParam }

constructor ECommandParam.Create(Command: TCommand; Param: PCoreString);
const
  Messages: array[TCommand] of PLegacyChar =
    (sSourceFileNameParam, sIntoFileNameParam, sFallbackCodePageParam);
begin
  if (Param <> nil) and (Param.Count <> 0) then
    inherited Create(sDuplicate, CP_LOCALIZATION, [Messages[Command], Param.Data])
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

constructor TApplication.Create(CommandLine: PWideChar);
var
  ExeName: array[0..MAX_PATH] of CoreChar;
  CmdLine, Key: TWideString;
  Param: TCommandLineParam;
  ExeNameLength, ParamCount: Integer;
  W: PWideChar;
begin
  with FConsole do
  begin
    Create;
    CodePage := CP_UTF8;
    WriteLn;
  end;

  ExeNameLength := GetModuleFileNameW(0, ExeName, Length(ExeName));
  if ExeNameLength = 0 then
    RaiseLastPlatformError {$IFDEF Debug} (sModuleFileName, Length(ExeName)) {$ENDIF} ;

  with FAppName do
  begin
    Create;
    AsWideString(ExeName, ExeNameLength, soAttach);
    Skip(LastIndex(PathDelimiter) + 1);
    Truncate(Count - LastIndex(WideChar('.')));
    Detach
  end;

  FConsole.WriteLn(sTextest, 0, [FAppName.RawData],  2);

  CmdLine.Create;
  CmdLine.AsWideString(CommandLine, WideStrLen(CommandLine), soAttach);
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
    if not Param.Quoted and (Param.RawData^ = '-') then
    begin
      W := sOEM;
      if Key.Compare(W + 1, PWord(W)^, True) = 0 then
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
        if Key.Compare(W + 1, PWord(W)^, True) = 0 then
        begin
          Include(FOptions, roPause);
          Param.Clear;
        end;
      end;
    end;

    if (Param.Count <> 0) and (Param.RawData^ = '-') then
    begin
      W := sCP;
      if Key.Compare(W + 1, PWord(W)^, True) = 0 then
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
        if Key.Compare(W + 1, PWord(W)^, True) = 0 then
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
    Include(FOptions, roPause)
  else if FSourceFileName.Count = 0 then
    raise ECommandParam.Create(cmNone);
end;

destructor TApplication.Destroy;
begin
  FIntoFileName.Finalize;
  FSourceFileName.Finalize;
  FAppName.Finalize;
  FConsole.Destroy;

  if roPause in FOptions then // placed here to show exceptions properly
    with FConsole do
    begin
      Create(True);
      try
        ReadLn(sPressEnterToExit, StrLen(sPressEnterToExit));
      finally
        Destroy;
      end;
    end;
end;

procedure TApplication.Help;
begin
  with FConsole do
  begin
    WriteLn(sUsage, 0, [FAppName.RawData], 2);
    WriteLn(PLegacyChar(sHelp), StrLen(sHelp));
  end;
end;

procedure TApplication.Pause;
begin
  Include(FOptions, roPause);
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
  FConsole.WriteLn('%hs: %d bytes, %d lines, %hs seconds, %hs GB/s', 0,
    [TestNames[AverageCount <> 0], CharCount, LineCount, @Time[1], @Throughput[1]]);

  if AverageStringLength <> 0 then
  begin
    FConsole.WriteLn('Average line length: %d', 0, [AverageCount]);

    EstimatedCount := CharCount div AverageStringLength;
    Str((EstimatedCount - LineCount) / LineCount * 100:1:1, Percent);
    Percent[Length(Percent) + 1] := #0;
    FConsole.WriteLn('Estimation variance: %hs%% (%d characters '#215' %d lines)', 0,
      [@Percent[1], AverageStringLength, EstimatedCount]);
  end;

  Overhead := Capacity - LineCount;
  Str(Overhead / Capacity * 100:1:1, Percent);
  Percent[Length(Percent) + 1] := #0;
  FConsole.WriteLn('Capacity overhead: %hs%% (%d bytes)', 0,
    [@Percent[1], Overhead * ItemSize]);

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
  FConsole.WriteLn('Total overhead: %hs%% (%d bytes)', 0, [@Percent[1], Overhead]);
end;

var
  CP: TCodePage;
  S: TLegacyString;
  Text: TLegacyStrings;

procedure CoreLiteTest(AverageStringLength: Integer);
var
  StartTime: QuadWord;
begin
  FConsole.WriteLn;
  try
    Text.Capacity := 0;
    StartTime := FCounter.Value;
    with Text do
    begin
      Capacity := S.Count div AverageStringLength;
      Delta := AverageStringsDelta;
    end;
    Text.AppendText(@S, True);
    Stats(FCounter.MillisecondsFrom(StartTime), S.Count, AverageStringLength,
      Text.Capacity, Text.Count, Text.AverageCount, SizeOf(TLegacyString));
  except
    on E: TObject do
    begin
      FConsole.WriteLn('CoreLite test fail, average string length %d', 0, [AverageStringLength]);
      ShowException(E);
    end;
  end;
end;

procedure DelphiTest;
const
  sDelphiTestFail = 'Delphi test fail';
var
  Strings: TStrings;
  StartTime: QuadWord;
  Txt: string;
begin
  FConsole.WriteLn;
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
      FConsole.WriteLn(PLegacyChar(sDelphiTestFail), StrLen(sDelphiTestFail));
      ShowException(E);
    end;
  end;
end;

begin
  if FSourceFileName.Count <> 0 then
  begin
    if FFallbackCP <> 0 then
    begin
      CP.Create(FFallbackCP);
      FConsole.WriteLn(sFallbackCP, 0, [CP.Number, CP.Name]);
    end;

    FConsole.WriteLn(sFileNameFmt, 0, [sSourceFile, FSourceFileName.Data]);

    S.Create;
    try
      LoadFile(S.Load, FSourceFileName.Data, faSequentialRead);
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
        if FIntoFileName.Count <> 0 then
        begin
          FConsole.WriteLn;
          FConsole.WriteLn(sFileNameFmt, 0, [sSavingInto, FIntoFileName.Data]);
          SaveFile(Text.Save, FIntoFileName.RawData, Text.EstimateText.Length);
        end;
      finally
        Text.Destroy;
      end;
    finally
      S.Destroy;
    end;
  end
  else
    Help;
end;

end.
