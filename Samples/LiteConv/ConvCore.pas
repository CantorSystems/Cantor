(*
    LiteConv core unit
*)

unit ConvCore;

interface

uses
  CoreUtils, CoreExceptions, CoreStrings, CoreApp;

type
  TRunOption = (roPause, roNoLogo, roVersion, roRename, roOEM);
  TRunOptions = set of TRunOption;

  TCommand = (cmNone, cmInto, cmCP);

  TApplication = object(TConsoleApplication{<TRunOptions>})
  private
    FOptions: TRunOptions; // specialize <F>
    FSourceFileName: TCoreString;
    {FCommandParams: array[TCommand] of TCoreString;}
    FSourceCP: Word;
    procedure ParseCommandLine(Source: PCoreChar);
    procedure Help;
  public
    destructor Destroy;
    procedure Run(CommandLine: PCoreChar);
    property Options: TRunOptions read FOptions;
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
    inherited Create(sDuplicate, DefaultSystemCodePage, [Messages[Command], Param.Data])
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

destructor TApplication.Destroy;
{var
  Cmd: TCommand;}
begin
  {for Cmd := Low(FCommandParams) to High(FCommandParams) do
    FCommandParams[Cmd].Finalize;}
  FSourceFileName.Finalize;
  inherited;
end;

procedure TApplication.Help;
var
  ACP, OEMCP: TCodePage;
begin
  with Console do
  begin
    WriteLn(sUsage, 0, [AppName.Data, AppName.RawData], 2);
    WriteLn(PLegacyChar(sHelp), StrLen(sHelp), 2);
    ACP.Create(CP_ACP);
    OEMCP.Create(CP_OEMCP);
    WriteLn(sEnvironment, DefaultSystemCodePage,
      [ACP.Number, ACP.Name, OEMCP.Number, OEMCP.Name]);
    WriteLn;
    WriteLn(PLegacyChar(sAvoidCharCorruption), StrLen(sAvoidCharCorruption));
  end;
end;

procedure TApplication.ParseCommandLine(Source: PCoreChar);
const
  Commands: array[cmInto..cmInto] of PWideChar = (sInto); // requires file name after
  RunOptions: array[roPause..roRename] of PWideChar = (sPause, sNoLogo, sVersion, sRen);
var
  CmdLine, Key: TWideString;
  Param: TCommandLineParam;
  ParamCount: Integer;
  {Cmd: TCommand;}
  Opt: TRunOption;
  W: PWideChar;
begin
  CmdLine.Create;
  CmdLine.AsWideString(Source, WideStrLen(Source), soAttach);
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

    if Param.IsKey then
    begin
      W := sOEM;
      if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
      begin
        FSourceCP := CP_OEMCP;
        Param.Clear;
      end
      else
        for Opt := Low(RunOptions) to High(RunOptions) do
          if Key.Compare(RunOptions[Opt] + 1, PCharCode(RunOptions[Opt])^, True) = 0 then
          begin
            Include(FOptions, Opt);
            Param.Clear;
            Break;
          end;
    end;

    {if Param.IsKey then
    begin
      W := sCP;
      if Key.Compare(W + 1, PCharCode(W)^, True) = 0 then
      begin
        CmdLine := Param.AsNextParam(@CmdLine);
        if (Param.Count = 0) or (FSourceCP <> 0) then
          raise ECommandParam.Create(cmCP, @Param);
        FSourceCP := Param.AsInteger;
        if FSourceCP - CP_UTF7 in [0..1] then
          raise ECodePageParam.Create(FSourceCP = CP_UTF8);
        Param.Clear;
      end
      else
        for Cmd := Low(Commands) to High(Commands) do
          if Key.Compare(Commands[Cmd] + 1, PCharCode(Commands[Cmd])^, True) = 0 then
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
    end;}

    if Param.Count <> 0 then
    begin
      if FSourceFileName.Count <> 0 then
        raise ECommandParam.Create(cmNone, @FSourceFileName);
      FSourceFileName.Create;
      FSourceFileName.AsRange(@Param, 0);
    end;
  until False;

  if ParamCount = 0 then
  begin
    if Console.Redirection = [] then
      Include(FOptions, roPause);
  end
  else if (FSourceFileName.Count = 0) and not (roVersion in FOptions) then
    raise ECommandParam.Create(cmNone);
end;

procedure TApplication.Run(CommandLine: PCoreChar);
var
  CP: TCodePage;
  LegacyFileName: TLegacyString;
  UniFileName: TWideString;
begin
  ParseCommandLine(CommandLine);
  if Title(sTitle) then
    Exit;

  if FSourceFileName.Count <> 0 then
  begin
    if roRename in FOptions then
    begin
      CP.Create(FSourceCP);
      if not (roNoLogo in FOptions) then
        Console.WriteLn(sRenameUsingCP, 0, [CP.Number, CP.Name]);

      LegacyFileName.Create;
      try
        with LegacyFileName do
        begin
          CodePage := @CP;
          AsWideString(@FSourceFileName, [coReplaceInvalid]);
        end;

        if LegacyFileName.ValidateUTF8 > 0 then
        begin
          UniFileName.Create;
          try
            UniFileName.AsString(@LegacyFileName);
            if not MoveFileW(FSourceFileName.Data, UniFileName.Data) then
              RaiseLastPlatformError(FSourceFileName.RawData);
            Console.WriteLn(sRenamed, 0, [FSourceFileName.RawData, UniFileName.RawData]);
          finally
            UniFileName.Destroy;
          end;
        end
        else
          Console.WriteLn(sNoRenameNeeded, 0, [FSourceFileName.Data]);
      finally
        LegacyFileName.Destroy;
      end;
    end;

//    Console.WriteLn(sSourceFileName, 0, [FSourceFileName.Data]);}
  end
  else
    Help;
end;

end.
