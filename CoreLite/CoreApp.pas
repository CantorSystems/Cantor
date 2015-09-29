(*
    Lite Core Library (CoreLite mini)

    Console application abstraction layer

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)
*)

unit CoreApp;

interface

uses
  Windows, CoreUtils, CoreWrappers, CoreStrings;

type
  TConsoleAppOptions = set of (caPause, caNoLogo);

  PConsoleApplication = ^TConsoleApplication;
  TConsoleApplication = object
  private
    FConsole: TStreamConsole;
    FAppName, FExeName: TCoreString;
  // { placeholder } FOptions: TConsoleAppOptions;
  protected
    procedure Help(UsageFmt, HelpMsg: PLegacyChar);
  public
    constructor Create(ConsoleCP: Word = CP_UTF8);
    destructor Destroy;
    procedure Pause;
    procedure Run(LogoFmt: PLegacyChar);

    property AppName: TCoreString read FAppName;
    property Console: TStreamConsole read FConsole;
    property ExeName: TCoreString read FExeName;
  end;

implementation

uses
  CoreExceptions, CoreConsts;

type
  PConsoleAppCast = ^TConsoleAppCast;
  TConsoleAppCast = object(TConsoleApplication)
    Options: TConsoleAppOptions;
  end;

{ TConsoleApplication }

constructor TConsoleApplication.Create(ConsoleCP: Word);
var
  Buf: array[0..MAX_PATH] of CoreChar;
  BufLen: Integer;
begin
  with FConsole do
  begin
    Create;
    CodePage := ConsoleCP;
  end;

  BufLen := GetModuleFileNameW(0, Buf, Length(Buf));
  if BufLen = 0 then
    RaiseLastPlatformError {$IFDEF Debug} (sModuleFileName, Length(Buf)) {$ENDIF} ;

  with FExeName do
  begin
    Create;
    AsWideString(Buf, BufLen);
  end;

  with FAppName do
  begin
    Create;
    AsRange(@FExeName, FExeName.LastIndex(PathDelimiter) + 1);
    Truncate(Count - LastIndex(WideChar('.')));
  end;
end;

destructor TConsoleApplication.Destroy;
begin
  FAppName.Finalize;
  FExeName.Finalize;
  FConsole.Destroy;

  if caPause in PConsoleAppCast(@Self).Options then
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

procedure TConsoleApplication.Help(UsageFmt, HelpMsg: PLegacyChar);
begin
  with FConsole do
  begin
    WriteLn;
    WriteLn(UsageFmt, 0, [FAppName.Data], 2);
    WriteLn(HelpMsg, StrLen(HelpMsg), 2);
    if CodePage = CP_UTF8 then
      WriteLn(PLegacyChar(sAvoidCharCorruption), StrLen(sAvoidCharCorruption));
  end;
end;

procedure TConsoleApplication.Pause;
begin
  Include(PConsoleAppCast(@Self).Options, caPause);
end;

procedure TConsoleApplication.Run(LogoFmt: PLegacyChar);
var
  VerInfo: TVersionInfo;
  Ver: TVersionBuffer;
begin
  if not (caNoLogo in PConsoleAppCast(@Self).Options) then
  begin
    FConsole.WriteLn;

    with VerInfo do
    begin
      Create(FExeName.RawData);
      try
        FormatVersion(Ver, sVersionAndRevision);
        if TranslationCount <> 0 then
          FConsole.WriteLn(LogoFmt, 0, [StringInfo(0, 'ProductName'), Ver,
            StringInfo(0, 'LegalCopyright')], 2);
      finally
        Destroy;
      end;
    end;
  end;
end;

end.

