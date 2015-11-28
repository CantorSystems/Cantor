(*
    Lite Core Library (CoreLite mini)

    Console application abstraction layer

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)
*)

unit CoreApp;

interface

uses
  Windows, CoreUtils, CoreClasses, CoreWrappers, CoreStrings;

type
  TConsoleAppOptions = set of (caPause, caNoLogo, caVersion);

  PConsoleApplication = ^TConsoleApplication;
  TConsoleApplication = object
  private
    FConsole: TStreamConsole;
    FAppName, FExeName: TCoreString;
  // { placeholder } FOptions: TConsoleAppOptions;
  protected
    procedure Help(UsageFmt, HelpMsg: PLegacyChar);
    function Logo(LogoFmt: PLegacyChar): Boolean;
    function Title(Text: PLegacyChar): Boolean;
  public
    constructor Create(ConsoleCP: Word = CP_UTF8);
    destructor Destroy;
    procedure Pause;

    property AppName: TCoreString read FAppName;
    property Console: TStreamConsole read FConsole;
    property ExeName: TCoreString read FExeName;
  end;

  PLegacyCommandLineParam = ^TLegacyCommandLineParam;
  TLegacyCommandLineParam = object(TLegacyString)
  private
    FQuoted: Boolean;
  public
    function AsNextParam(CommandLine: PLegacyString): TLegacyString;
    procedure Clear; virtual;
    function IsKey: Boolean;
    property Quoted: Boolean read FQuoted;
  end;

  PWideCommandLineParam = ^TWideCommandLineParam;
  TWideCommandLineParam = object(TWideString)
  private
    FQuoted: Boolean;
  public
    function AsNextParam(CommandLine: PWideString): TWideString;
    procedure Clear; virtual;
    function IsKey: Boolean;
    property Quoted: Boolean read FQuoted;
  end;

  PCommandLineParam = PWideCommandLineParam;
  TCommandLineParam = TWideCommandLineParam;

  PFileName = ^TFileName;
  TFileName = object(TCoreString)
  private
    FPathDelimiterIndex: Integer;
  public
    procedure AsTempName(Source: PCoreString);
    procedure Detach; virtual;
    procedure ChangeFileName(Source: PCoreChar; Length: Integer); overload;
    procedure ChangeFileName(Source: PCoreString); overload;
    function IsDotOrNull: Boolean;
    function IsPath: Boolean;
    function Width(MaxWidth: Integer): Integer;

    property PathDelimiterIndex: Integer read FPathDelimiterIndex;
  end;

  PFileNameList = ^TFileNameList;

  PFileNameListItem = ^TFileNameListItem;
  TFileNameListItem = object(TFileName)
  private
  { hold } FOwner: PFileNameList;
  { hold } FNext: PFileNameListItem;
  public
    property Owner: PFileNameList read FOwner;
    property Next: PFileNameListItem read FNext;
  end;

  TFileNameList = object(TList)
  private
  { hold } FFirst, FLast: PFileNameListItem;
  protected
    class function ListInfo: TListInfo; virtual;
  public
    property First: PFileNameListItem read FFirst;
    property Last: PFileNameListItem read FLast;
  end;

implementation

uses
  CoreExceptions, CoreConsts;

type
  PConsoleAppCast = ^TConsoleAppCast;
  TConsoleAppCast = object(TConsoleApplication)
    Options: TConsoleAppOptions;
  end;

{ TFileName }

procedure TFileName.AsTempName(Source: PCoreString);
type
  PPostfix = ^TPostfix;
  TPostfix = array[0..3] of CoreChar;
const
  Postfix: TPostfix = ('.', '$', '$', '$');
begin
  AsRange(Source, 0);
  Capacity := Count + Length(Postfix) + 1;
  PPostfix(RawData + Count)^ := Postfix;
  Append(Length(Postfix));
  RawData[Count] := #0;
end;

procedure TFileName.ChangeFileName(Source: PCoreChar; Length: Integer);
begin
  Truncate(Count - PathDelimiterIndex - 1);
  Append(Source, Length);
end;

procedure TFileName.ChangeFileName(Source: PCoreString);
begin
  ChangeFileName(Source.RawData, Source.Count);
end;

procedure TFileName.Detach;
begin
  inherited;
  FPathDelimiterIndex := LastIndex(PathDelimiter);
{$IFDEF MSWINDOWS}
  if FPathDelimiterIndex < 0 then
    FPathDelimiterIndex := LastIndex('/');
{$ENDIF}
end;

function TFileName.IsDotOrNull: Boolean;
begin
  Result := (TypeOf(Self) <> nil) and ((Count = 0) or (Count = 1) and (RawData^ = '.'));
end;

function TFileName.IsPath: Boolean;
begin
  Result := False;
  if Count <> 0 then
    case RawData[Count - 1] of
    {$IFDEF MSWINDOWS} '/', {$ENDIF} PathDelimiter:
      Inc(Result);
    end;
end;

function TFileName.Width(MaxWidth: Integer): Integer;
begin
  Result := Count;
  if (Result > MaxWidth) and (FPathDelimiterIndex >= 0) then
    Dec(Result, FPathDelimiterIndex + 1);
end;

{ TConsoleApplication }

constructor TConsoleApplication.Create(ConsoleCP: Word);
begin
  with FConsole do
  begin
    Create;
    CodePage := ConsoleCP;
  end;

  with FExeName do
  begin
    Create;
    with ModuleFileName do
      AsWideString(Value, Length);
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
  {$IFNDEF Lite}
    if caVersion in PConsoleAppCast(@Self).Options then
      WriteLn;
  {$ENDIF}
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

function TConsoleApplication.Logo(LogoFmt: PLegacyChar): Boolean;
var
  VerInfo: TVersionInfo;
  Ver: TVersionBuffer;
begin
  Result := caVersion in PConsoleAppCast(@Self).Options;
  if Result or not (caNoLogo in PConsoleAppCast(@Self).Options) then
  begin
    FConsole.WriteLn;
    with VerInfo do
    begin
      Create(FExeName.RawData);
      try
        FormatVersion(Ver, sVersionAndRevision);
        if TranslationCount <> 0 then
          FConsole.WriteLn(LogoFmt, 0, [StringInfo(0, 'ProductName'), Ver,
            StringInfo(0, 'LegalCopyright')], 1 + Byte(not Result));
      finally
        Destroy;
      end;
    end;
  end;
end;

function TConsoleApplication.Title(Text: PLegacyChar): Boolean;
begin
  Result := caVersion in PConsoleAppCast(@Self).Options;
  if Result or not (caNoLogo in PConsoleAppCast(@Self).Options) then
    with FConsole do
    begin
      WriteLn;
      WriteLn(Text, StrLen(Text), 1 + Byte(not Result));
    end;
end;

{ TLegacyCommandLineParam }

function TLegacyCommandLineParam.AsNextParam(CommandLine: PLegacyString): TLegacyString;
begin
  Result.Create;
  Result := CommandLine^;

  while (Result.Count <> 0) and (Result.RawData^ in [#32, #9, #10, #13]) do
    Result.Skip;

  if Result.Count <> 0 then
  begin
    if Result.RawData^ = '"' then
    begin
      Result.Skip;
      AsRange(@Result, 0, Result.RawNextIndex('"'));
      FQuoted := True;
      Result.Skip(Count + 1);
    end
    else
    begin
      AsRange(@Result, 0);
      FQuoted := False;
    end;
    
    while (Result.Count <> 0) and not (Result.RawData^ in [#32, #9, #10, #13]) do
      Result.Skip;

    Truncate(Result.Count);
  end
  else
    Clear;
end;

procedure TLegacyCommandLineParam.Clear;
begin
  inherited;
  FQuoted := False;
end;

function TLegacyCommandLineParam.IsKey: Boolean;
begin
  Result := (Count <> 0) and not FQuoted and (RawData^ = '-');
end;

{ TWideCommandLineParam }

function TWideCommandLineParam.AsNextParam(CommandLine: PWideString): TWideString;
begin
  Result.Create;
  Result := CommandLine^;

  while (Result.Count <> 0) and ((Result.RawData^ = #32) or (Result.RawData^ = #9) or
    (Result.RawData^ = #10) or (Result.RawData^ = #13))
  do
    Result.Skip;

  if Result.Count <> 0 then
  begin
    if Result.RawData^ = '"' then
    begin
      Result.Skip;
      AsRange(@Result, 0, Result.RawNextIndex(WideChar('"')));
      FQuoted := True;
      Result.Skip(Count + 1);
      Exit;
    end;

    AsRange(@Result, 0);
    while (Result.Count <> 0) and (Result.RawData^ <> #32) and (Result.RawData^ <> #9) and
      (Result.RawData^ <> #10) and (Result.RawData^ <> #13)
    do
      Result.Skip;

    Truncate(Result.Count);
  end
  else
    Clear;
end;

procedure TWideCommandLineParam.Clear;
begin
  inherited;
  FQuoted := False;
end;

function TWideCommandLineParam.IsKey: Boolean;
begin
  Result := (Count <> 0) and not FQuoted and (RawData^ = '-');
end;

{ TFileNameList }

class function TFileNameList.ListInfo: TListInfo;
begin
  with Result do
  begin
    ClassName := sFileNameList;
    ItemOffset := PAddress(@PFileNameListItem(nil).FOwner) - nil;
  end;
end;

end.

