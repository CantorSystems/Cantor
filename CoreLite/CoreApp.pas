(*
    Lite Core Library (CoreLite)

    Console application abstraction layer

    Copyright (c) 2015-2018 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Lite -- avoid useless doubts in code
      * Locale -- use additional locale-dependent translations for console output
*)

unit CoreApp;

interface

uses
  Windows, CoreUtils, CoreClasses, CoreWrappers, CoreStrings;

type
  PFileName = ^TFileName;
  TFileName = object(TCoreString)
  private
    FNameIndex: Integer;
  public
    procedure AsTempName(Source: PCoreString);
    procedure Detach; virtual;
    procedure ChangeFileName(Source: PCoreChar; Length: Integer); overload;
    procedure ChangeFileName(Source: PCoreString; Index: Integer); overload;
    procedure ChangeFileName(Source: PFileName); overload;
    function IsDot: Boolean;
    function IsDotOrNull: Boolean;
    function IsNull: Boolean;
    function IsPath: Boolean;
    procedure MakePath;
    function Width(MaxWidth: Integer): Integer;

    property NameIndex: Integer read FNameIndex;
  end;

  TConsoleAppOptions = set of (caPause, caNoLogo, caVersion);

  PConsoleApplication = ^TConsoleApplication;
  TConsoleApplication{<F>} = object
  private
    FConsole: TStreamConsole;
    FAppName: TCoreString;
    FExeName: TFileName;
  // FOptions: generic <F> as TConsoleAppOptions;
  protected
    procedure Help(UsageFmt, HelpMsg: PLegacyChar);
    function Logo(LogoFmt: PLegacyChar): Boolean;
    function Title(Text: PLegacyChar): Boolean;
  public
    constructor Create(ConsoleCP: Word = CP_UTF8);
    destructor Destroy;
    procedure Pause;
    procedure ShowException(E: TObject);

    property AppName: TCoreString read FAppName;
    property Console: TStreamConsole read FConsole;
    property ExeName: TFileName read FExeName;
  end;

  PLegacyCommandLineParam = ^TLegacyCommandLineParam;
  TLegacyCommandLineParam = object(TLegacyString)
  private
    FQuoted: Boolean;
  public
    function AsNextParam(CommandLine: PLegacyString): TLegacyString;
    procedure Clear; virtual;
    function Equals(Value: PLegacyChar; IgnoreCase: Boolean = True): Boolean;
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
    function Equals(Value: PWideChar; IgnoreCase: Boolean = True): Boolean;
    function IsKey: Boolean;

    property Quoted: Boolean read FQuoted;
  end;

  PCommandLineParam = PWideCommandLineParam;
  TCommandLineParam = TWideCommandLineParam;

  PFileNameList = ^TFileNameList;

  PFileNameListItem = ^TFileNameListItem;
  TFileNameListItem = object(TFileName{, TListItem<PFileNameList, PFileNameListItem>})
  private
    FOwner: PFileNameList;           // specialize <L>
    FPrev, FNext: PFileNameListItem; // specialize <I>
  public
    property Owner: PFileNameList read FOwner;
    property Prev: PFileNameListItem read FPrev;
    property Next: PFileNameListItem read FNext;
  end;

  TFileNameList = object(TList{<PFileNameListItem>})
  private
    FFirst, FLast: PFileNameListItem; // specialize <I>
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
  TConsoleAppCast = object(TConsoleApplication{<TConsoleAppOptions>})
    Options: TConsoleAppOptions; // specialize <F>
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
  Truncate(Count - FNameIndex);
  Append(Source, Length);
end;

procedure TFileName.ChangeFileName(Source: PCoreString; Index: Integer);
begin
  ChangeFileName(Source.RawData + Index, Source.Count - Index);
end;

procedure TFileName.ChangeFileName(Source: PFileName);
begin
  ChangeFileName(Source, Source.NameIndex);
end;

procedure TFileName.Detach;
begin
  inherited;
  FNameIndex := LastIndex(PathDelimiter) + 1;
{$IFDEF MSWINDOWS}
  if FNameIndex = 0 then
    FNameIndex := LastIndex('/') + 1;
{$ENDIF}
end;

function TFileName.IsDot: Boolean;
begin
  Result := (Count = 1) and (RawData^ = '.');
end;

function TFileName.IsDotOrNull: Boolean;
begin
  Result := (TypeOf(Self) <> nil) and ((Count = 0) or (Count = 1) and (RawData^ = '.'));
end;

function TFileName.IsNull: Boolean;
begin
  Result := (TypeOf(Self) <> nil) and (Count = 0);
end;

function TFileName.IsPath: Boolean;
begin
  Result := False;
  if {$IFNDEF Lite} (TypeOf(Self) <> nil) and {$ENDIF} (Count <> 0) then
    case RawData[Count - 1] of
    {$IFDEF MSWINDOWS} '/', {$ENDIF} PathDelimiter:
      Inc(Result);
    end;
end;

procedure TFileName.MakePath;
begin
  if IsPath then
    Detach
  else
  begin
    Append(PathDelimiter);
    FNameIndex := Count;
  end;
end;

function TFileName.Width(MaxWidth: Integer): Integer;
begin
  Result := Count;
  if Result > MaxWidth then
    Dec(Result, FNameIndex);
end;

{ TConsoleApplication }

constructor TConsoleApplication.Create(ConsoleCP: Word);
begin
  with FConsole do
  begin
    Create;
    CodePage := ConsoleCP;
  end;

{$IFDEF Lite}
  PPointer(@FExeName)^ := TypeOf(TFileName); // Fast core
{$ELSE}
  FExeName.Create;
{$ENDIF}
  with FExeName, ModuleFileName do
  begin
    AsWideString(Value, Length);
    FNameIndex := FileNameIndex; // Fast core
  end;

  with FAppName do
  begin
    Create;
    AsRange(@FExeName, FExeName.NameIndex);
    Truncate(Count - LastIndex(WideChar('.')));
  end;
end;

destructor TConsoleApplication.Destroy;
begin
  FAppName.Finalize;
  FExeName.Finalize;

  if caPause in PConsoleAppCast(@Self).Options then
  begin
    FConsole.WriteLn(Byte(NeedEOL) + 1);
    FConsole.ReadLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
      sPressEnterToExit, StrLen(sPressEnterToExit));
  end;

  FConsole.Destroy;
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
    WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF} HelpMsg, StrLen(HelpMsg), 2);
    if CodePage = CP_UTF8 then
      WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
        PLegacyChar(sAvoidCharCorruption), StrLen(sAvoidCharCorruption));
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
        FormatVersion(Ver, sVersionAndBuild);
        if TranslationCount <> 0 then
          FConsole.WriteLn(LogoFmt, 0, [StringInfo(0, 'ProductName'), Ver,
            StringInfo(0, 'LegalCopyright')], 1 + Byte(not Result));
      finally
        Destroy;
      end;
    end;
  end;
end;

procedure TConsoleApplication.ShowException(E: TObject);
begin
  CoreExceptions.ShowException(E);
  if not (caNoLogo in PConsoleAppCast(@Self).Options) and (FConsole.Redirection = []) then
    Pause;
end;

function TConsoleApplication.Title(Text: PLegacyChar): Boolean;
begin
  Result := caVersion in PConsoleAppCast(@Self).Options;
  if Result or not (caNoLogo in PConsoleAppCast(@Self).Options) then
    with FConsole do
    begin
      WriteLn;
      WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
        Text, StrLen(Text), 1 + Byte(not Result));
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

function TLegacyCommandLineParam.Equals(Value: PLegacyChar; IgnoreCase: Boolean): Boolean;
begin
  Result := Compare(Value + 1, PByte(Value)^, IgnoreCase) = 0;
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

function TWideCommandLineParam.Equals(Value: PWideChar; IgnoreCase: Boolean): Boolean;
begin
  Result := Compare(Value + 1, PWord(Value)^, IgnoreCase) = 0;
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

