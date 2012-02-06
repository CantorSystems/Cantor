(*
  Unified Environment by Freeman

  Legacy file system API implementation:
    - file find container (non-enumerable, wrapper of legacy file find API)
    - file name manipulation (based on string list container)

  Copyright (c) 2007 Freeman
*)

unit FileSystem;

interface

uses
  Core,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  // TODO: POSIX
{$ENDIF}
  Containers;

const
{$IFDEF MSWINDOWS}
  MAX_PATH = Windows.MAX_PATH;
  PathDelimiter = {$IFDEF UNICODE} UnicodeChar('\') {$ELSE} '\' {$ENDIF};
{$ELSE}
  MAX_PATH = 1024; // TODO: POSIX
  PathDelimiter = {$IFDEF UNICODE} UnicodeChar('/') {$ELSE} '/' {$ENDIF};
{$ENDIF}

type
  PCommandLine = ^TCommandLine;
  TCommandLine = object(TContainer)
  private
    FSource: PCoreChar;
    FParameter: CoreString;
  public
    constructor Init; overload;
    constructor Init(Source: PCoreChar); overload;
    destructor Done; virtual;
    function ForEach(Action: TForEach; Data: Pointer): PContainer; virtual;
  // properties
    property Parameter: CoreString read FParameter;
  end;

  PPath = ^TPath;
  TPath = object(TObject)
  private
    FStrings: PStringList;
    FDelimiter: CoreChar;
    function Name(TxtLength: Integer;
      TrailingDelimiter: Boolean = False): CoreString; overload;
  public
    constructor Init(const Source: CoreString;
      PathDelimiter: CoreChar = FileSystem.PathDelimiter);
    destructor Done; virtual;
    function Expand(const FileName: CoreString): CoreString;
    function Path(TrailingDelimiter: Boolean): CoreString;
    function Name: CoreString; overload;
    function Name(TrailingDelimiter: Boolean): CoreString; overload;
  // properties
    property Delimiter: CoreChar read FDelimiter write FDelimiter;
    property Strings: PStringList read FStrings;
  end;

  PFileName = PPath;
  TFileName = TPath;

{$IFDEF MSWINDOWS}
  TFileTime = Windows.TFileTime;
{$ELSE}
  // TODO: POSIX
{$ENDIF}

  PFileFind = ^TFileFind;
  TFileFind = object(TContainer)
  private
    FPath: PPath;
  {$IFDEF MSWINDOWS}
    {$IFDEF UNICODE}
      FFindData: TWin32FindDataW;
    {$ELSE}
      FFindData: TWin32FindDataA;
    {$ENDIF}
    FHandle: THandle;
  {$ELSE}
    // TODO: POSIX
  {$ENDIF}
    function GetAltName: CoreString;
    function GetName: CoreString;
    procedure ItemExists(Current: PObject; Data: Pointer; var Found: Boolean);
  public
    constructor Init(const Mask: CoreString;
      PathDelimiter: CoreChar = FileSystem.PathDelimiter);
    destructor Done; virtual;
    function Exists: Boolean;
    function ForEach(Action: TForEach; Data: Pointer): PContainer; virtual;
  // properties
    property AltName: CoreString read GetAltName;
    property Attributes: Cardinal read FFindData.dwFileAttributes;
    property CreationTime: TFileTime read FFindData.ftCreationTime;
    property LastAccessTime: TFileTime read FFindData.ftLastAccessTime;
    property LastWriteTime: TFileTime read FFindData.ftLastWriteTime;
    property Name: CoreString read GetName;
    property Path: PPath read FPath;
  end;

implementation

uses
{$IFDEF UNICODE}
  Unicode,
{$ENDIF}
  SysUtils;

{ TCommandLine }

constructor TCommandLine.Init;
begin
  FSource := {$IFDEF UNICODE} GetCommandLineW {$ELSE} GetCommandLineA {$ENDIF} ;
end;

constructor TCommandLine.Init(Source: PCoreChar);
begin
  FSource := Source;
end;

destructor TCommandLine.Done;
begin
end;

function TCommandLine.ForEach(Action: TForEach; Data: Pointer): PContainer;
var
  P, Tail: PCoreChar;
  Found: Boolean;
begin
  if FSource <> nil then
  begin
  {$IFDEF UNICODE}
    Tail := StrEndW(FSource);
  {$ELSE}
    Tail := StrEnd(FSource);
  {$ENDIF}
    Found := False;
    repeat
      {$IFDEF UNICODE}
        while (FSource^ = #32) or (FSource^ = #9) do
      {$ELSE}
        while FSource^ in [#9, #32] do
      {$ENDIF}
          Inc(FSource);
      if FSource^ = '"' then
      begin
        Inc(FSource);
      {$IFDEF UNICODE}
        P := StrScanW(FSource, UnicodeChar('"'), Tail - FSource);
      {$ELSE}
        P := StrScan(FSource, '"');
      {$ENDIF}
        if P = nil then
          P := Tail;
      end
      else
      begin
        P := FSource;
      {$IFDEF UNICODE}
        while (P^ <> #32) and (P^ <> #9) and (P^ <> #0) do
      {$ELSE}
        while not (P^ in [#9, #32, #0]) do
      {$ENDIF}
          Inc(P);
      end;
      SetString(FParameter, FSource, P - FSource);
      Action(@Self, Data, Found);
      if Found then
      begin
        Result := @Self;
        Exit;
      end;
      if P^ <> #0 then
        FSource := P + 1
      else
        Break;
    until False;
  end;
  Result := nil;
end;

{ TPath }

constructor TPath.Init(const Source: CoreString; PathDelimiter: CoreChar);
begin
  FDelimiter := PathDelimiter;
  FStrings := FStrings.Parse(Source, PathDelimiter);
end;

destructor TPath.Done;
begin
  FStrings.Free;
end;

function TPath.Expand(const FileName: CoreString): CoreString;
var
  Len: Integer;
  P: PCoreChar;
begin
  if FStrings <> nil then
  begin
    Len := Length(FileName);
    Result := Name(FStrings.TextLength(FDelimiter) + Len + 1, False);
    P := Pointer(Result);
    Inc(P, Length(Result) - Len - 1);
    P^ := FDelimiter;
    Inc(P);
    Move(FileName[1], P^, Len * SizeOf(CoreChar));
  end
  else
    Result := FileName;
end;

function TPath.Name: CoreString;
begin
  if FStrings <> nil then
    Result := PStringList(FStrings.Last).Data
  else
    Result := '';
end;

function TPath.Name(TxtLength: Integer; TrailingDelimiter: Boolean): CoreString;
begin
  Result := FStrings.Text(FDelimiter, @TxtLength);
  if TrailingDelimiter then
    Result[TxtLength] := FDelimiter;
end;

function TPath.Name(TrailingDelimiter: Boolean): CoreString;
begin
  if FStrings <> nil then
    Result := Name(FStrings.TextLength(FDelimiter) + Integer(TrailingDelimiter),
      TrailingDelimiter)
  else
    if TrailingDelimiter then
      Result := FDelimiter
    else
      Result := '';
end;

function TPath.Path(TrailingDelimiter: Boolean): CoreString;
begin
  if FStrings <> nil then
    Result := Name(FStrings.TextLength(FDelimiter) -
      Length(PStringList(FStrings.Last).Data), TrailingDelimiter)
  else
    Result := '';
end;

{ TFileFind }

constructor TFileFind.Init(const Mask: CoreString; PathDelimiter: CoreChar);
begin
  New(FPath, Init(Mask, PathDelimiter));
  if FPath.Strings <> nil then
    FPath.Strings.Last.Delete;
{$IFDEF MSWINDOWS}
  FHandle := {$IFDEF UNICODE} FindFirstFileW {$ELSE} FindFirstFileA {$ENDIF}
    (Pointer(Mask), FFindData);
{$ELSE}
  // TODO: POSIX
{$ENDIF}
end;

destructor TFileFind.Done;
begin
{$IFDEF MSWINDOWS}
  if FHandle <> INVALID_HANDLE_VALUE then
    FindClose(FHandle);
{$ENDIF}
  FPath.Free;
  inherited;
end;

function TFileFind.Exists: Boolean;
begin
  Result := ForEach(ItemExists, nil) <> nil;
end;

function TFileFind.ForEach(Action: TForEach; Data: Pointer): PContainer;
var
  Found: Boolean;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    Found := False;
    repeat
      Action(@Self, Data, Found);
      if Found then
      begin
        Result := @Self;
        Exit;
      end;
    until not {$IFDEF UNICODE} FindNextFileW {$ELSE} FindNextFileA {$ENDIF}
      (FHandle, FFindData);
  end;
  Result := nil;
end;

function TFileFind.GetAltName: CoreString;
begin
  Result := PCoreChar(@FFindData.cAlternateFileName);
end;

function TFileFind.GetName: CoreString;
begin
  Result := PCoreChar(@FFindData.cFileName);
end;

procedure TFileFind.ItemExists(Current: PObject; Data: Pointer;
  var Found: Boolean);
begin
  Found := True;
end;

end.

