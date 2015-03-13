(*
    LiteConv core unit
*)

unit ConvCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
  TRunOption = (roPause, roOEM);
  TRunOptions = set of TRunOption;

  TCommand = (cmNone, cmInto, cmCP);

  TApplication = object
  private
    FConsole: TStreamConsole;
    FAppName, FSourceFileName: TCoreString;
    FCommandParams: array[TCommand] of TCoreString;
    FSourceFileCP: Word;
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

  ECodePageParam = class(ECommandLine)
  private
    FUTF8: Boolean;
  public
    constructor Create(UTF8: Boolean);
    property UTF8: Boolean read FUTF8;
  end;


implementation

uses
  Windows, CoreConsts, ConvConsts;

const
  CP_Big5 = 950;
  CP_GBK = 20936;
  CP_Shift_JIS  = 932;

{ ECommandParam }

constructor ECommandParam.Create(Command: TCommand; Param: PCoreString);
const
  Messages: array[TCommand] of PLegacyChar =
    (sSourceFileNameParam, sIntoFileNameParam, sCodePageParam);
begin
  if (Param <> nil) and (Param.Count <> 0) then
    inherited Create(sDuplicate, CP_LOCALIZATION, [Messages[Command], Param.Data])
  else
    inherited Create(sMissing, [Messages[Command]]);
  FCommand := Command;
  FParameter := Param;
end;

{ ECodePageParam }

constructor ECodePageParam.Create(UTF8: Boolean);
begin
//  inherited Create(sIsNotASourceFileCP, [{CP_UTF}7 + Byte(UTF8)]);
  FUTF8 := UTF8;
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PWideChar);
const
  Commands: array[cmInto..cmInto] of PWideChar = (sInto);
  RunOptions: array[roPause..roPause] of PWideChar = (sPause);
var
  ExeName: array[0..MAX_PATH] of CoreChar;
  CmdLine, Key: TWideString;
  Param: TCommandLineParam;
  Len, ParamCount: Integer;
  Cmd: TCommand;
  Opt: TRunOption;
  W: PWideChar;
begin
  with FConsole do
  begin
    Create;
    CodePage := CP_UTF8;
    WriteLn;
  end;

  Len := GetModuleFileNameW(0, ExeName, Length(ExeName));
  if Len = 0 then
    RaiseLastPlatformError {$IFDEF Debug} (sModuleFileName, Length(ExeName)) {$ENDIF} ;

  with FAppName do
  begin
    Create;
    AsWideString(ExeName, Len, soAttach);
    Skip(PrevIndex(PathDelimiter) + 1);
    Truncate(Count - PrevIndex(WideChar('.')));
    Detach;
  end;

  FConsole.WriteLn(sLiteConv, 0, [FAppName.Data],  2);

  CmdLine.Create;
  CmdLine.AsWideString(CommandLine, WideStrLen(CommandLine), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name

  ParamCount := 0;
  repeat
    CmdLine := Param.AsNextParam(@CmdLine);
    if Param.Count = 0 then
      Break;

    Inc(ParamCount);
    Key.Create;
    Key.AsRange(@Param, 1);
    if not Param.Quoted and (Param.RawData^ = '-') then
    begin
      W := sOEM;
      if Key.Compare(W + 1, PWord(W)^, True) = 0 then
      begin
        FSourceFileCP := CP_OEMCP;
        Param.Clear;
      end
      else
        for Opt := Low(RunOptions) to High(RunOptions) do
          if Key.Compare(RunOptions[Opt] + 1, PWord(RunOptions[Opt])^, True) = 0 then
          begin
            Include(FOptions, Opt);
            Param.Clear;
            Break;
          end;
    end;

    if (Param.Count <> 0) and (Param.RawData^ = '-') then
    begin
      {W := sCP;
      if Key.Compare(W + 1, PWord(W)^, True) = 0 then
      begin
        CmdLine := Param.AsNextParam(@CmdLine);
        if (Param.Count = 0) or (FSourceFileCP <> 0) then
          raise ECommandParam.Create(cmCP, @Param);
        FSourceFileCP := Param.AsInteger;
        if FSourceFileCP - CP_UTF7 in [0..1] then
          raise ECodePageParam.Create(FSourceFileCP = CP_UTF8);
        Param.Clear;
      end
      else}
        for Cmd := Low(Commands) to High(Commands) do
          if Key.Compare(Commands[Cmd] + 1, PWord(Commands[Cmd])^, True) = 0 then
          begin
            CmdLine := Param.AsNextParam(@CmdLine);
            if (Param.Count = 0) or (FCommandParams[Cmd].Count <> 0) then
              raise ECommandParam.Create(Cmd, @Param);
            with FCommandParams[Cmd] do
            begin
              Create;
              AsRange(@Param, 0);
            end;
            Param.Clear;
            Break;
          end;
    end;

    if Param.Count <> 0 then
    begin
      if FSourceFileName.Count <> 0 then
        raise ECommandParam.Create(cmNone, @FSourceFileName);
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
var
  Cmd: TCommand;
begin
  for Cmd := Low(FCommandParams) to High(FCommandParams) do
    FCommandParams[Cmd].Finalize;

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
var
  ACP, OEMCP: TCodePage;
begin
  with FConsole do
  begin
    WriteLn(sUsage, 0, [FAppName.Data, FAppName.RawData], 2);
    WriteLn(PLegacyChar(sHelp), StrLen(sHelp), 2);
    ACP.Create(CP_ACP);
    OEMCP.Create(CP_OEMCP);
    WriteLn(sEnvironment, CP_LOCALIZATION,
      [ACP.Number, ACP.Name, OEMCP.Number, OEMCP.Name]);
  end;
end;

procedure TApplication.Pause;
begin
  Include(FOptions, roPause);
end;

procedure TApplication.Run;
var
  CP: TCodePage;
begin
  if FSourceFileName.Count <> 0 then
  begin
    if FSourceFileCP <> 0 then
    begin
      CP.Create(FSourceFileCP);
      FConsole.WriteLn(sSourceFileCP, 0, [CP.Number, CP.Name]);
    end;
    FConsole.WriteLn(sSourceFileName, 0, [FSourceFileName.Data]);
  end
  else
    Help;
end;

end.

