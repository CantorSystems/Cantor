(*
    XML Test core unit
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
  Windows, XMLDoc, CoreXML, CoreConsts, TestConsts;

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

  FConsole.WriteLn(PLegacyChar(sXMLTest), StrLen(sXMLTest), 2);

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

procedure TApplication.Run;
var
  CP: TCodePage;
  S: TLegacyString;
  Doc: TXMLDocument;
  Name: TXMLName;
  Rslt, W: TWideString;
begin
  if FSourceFileName.Count <> 0 then
  begin
    CP.Create(FFallbackCP);
    if FFallbackCP <> 0 then
      FConsole.WriteLn(sFallbackCP, 0, [CP.Number, CP.Name]);

    FConsole.WriteLn(sFileNameFmt, 0, [PLegacyChar(sSourceFile), FSourceFileName.Data]);

    S.Create;
    try
      S.CodePage := @CP;
      LoadFile(S.Load, FSourceFileName.Data);

      Doc.Create;
      try
        //Doc.AsXML // TODO
        W.Create;
        try
          W.AsString(@S);
          Name.Create;
          try
            Rslt.Create;
            try
              Rslt.AsRange(@W, 0);
              Rslt.Skip(Name.AsXML(@Rslt, True));
              FConsole.WriteLn('Name: “%s” (“%s”, “%s”)', 0,
                [Name.Data, Name.Prefix.Data, Name.LocalName.Data]);
              FConsole.WriteLn('Rest of the text: “%s”', 0, [Rslt.Data]);
            finally
              Rslt.Destroy;
            end;
          finally
            Name.Destroy;
          end;
        finally
          W.Destroy;
        end;

        if (FIntoFileName.Count <> 0) and (S.Count <> 0) then
        begin
          if FFallbackCP <> 0 then
            S.CodePage := @CP;
          FConsole.WriteLn(sFileNameFmt, 0, [PLegacyChar(sSavingInto), FIntoFileName.Data]);
        //  SaveFile(Text.Save, FIntoFileName.RawData, Text.EstimateText.Length);
        end;
      finally
        Doc.Destroy;
      end;
    finally
      S.Destroy;
    end;
  end
  else
    Help;
end;

end.

