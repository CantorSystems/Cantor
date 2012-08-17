(*
    Portable Executable (PE) Tool

    Command line interface implementation

    Copyright (c) 2012 The Unified Environment Laboratory
*)

unit CLI;

interface

uses
  CoreWrappers;

type
  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppFileName, FSourceFileName: PWideChar;
    procedure Help;
  public
    constructor Create(CommandLine: PWideChar);
    destructor Destroy; override;
    procedure Run;
  end;

{ Helper functions }

function ChangeFileExt(FileName, Ext: PWideChar): PWideChar;
function ParamStr(var CommandLine: PWideChar; var Quoted: Boolean): PWideChar;

implementation

uses
  Windows, CoreUtils, CoreClasses, CoreStrings, PetConsts;

{ Helper functions }

function ChangeFileExt(FileName, Ext: PWideChar): PWideChar;
var
  P: PWideChar;
  D, E, L: Cardinal;
begin
  if FileName <> nil then
  begin
    L := WideStrLen(FileName);
    P := WideStrRevScan(FileName, '.', L);
    if P <> nil then
      D := P - FileName
    else
      D := L;
    E := WideStrLen(Ext);
    GetMem(Result, (D + E + 1) * SizeOf(WideChar));
    Move(FileName^, Result^, D * SizeOf(WideChar));
    Move(Ext^, Result[D], (E + 1) * SizeOf(WideChar));
  end
  else
    Result := nil;
end;

function ParamStr(var CommandLine: PWideChar; var Quoted: Boolean): PWideChar;
var
  P: PWideChar;
  L: Cardinal;
begin
  if CommandLine <> nil then
  begin
    while (CommandLine^ = WideChar(32)) or (CommandLine^ = WideChar(9)) do
      Inc(CommandLine);
    if CommandLine^ = WideChar('"') then
    begin
      Inc(CommandLine);
      L := WideStrLen(CommandLine);
      P := WideStrScan(CommandLine, WideChar('"'), L);
      if P <> nil then
        L := P - CommandLine;
      Quoted := True;
    end
    else
    begin
      P := CommandLine;
      while (P^ <> WideChar(32)) and (P^ <> WideChar(9)) and (P^ <> WideChar(0)) do
        Inc(P);
      L := P - CommandLine;
      Quoted := False;
    end;
    Result := WideStrNew(CommandLine, L);
    CommandLine := P;
    if P^ <> WideChar(0) then
      Inc(CommandLine);
  end
  else
    Result := nil;
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PWideChar);
var
  Quoted: Boolean;
  Buf: array[Byte] of LegacyChar;
begin
  FConsole := TStreamConsole.Create;
  with FConsole do
  begin
  {$IFDEF Compat}
    CodePage := GetACP;
  {$ENDIF}
    WriteLn(@Buf, FormatBuf(sTitle, [sVersion], Buf), 2);
  end;

  FAppFileName := ParamStr(CommandLine, Quoted);
  FSourceFileName := ParamStr(CommandLine, Quoted);
end;

destructor TApplication.Destroy;
begin
  FreeMem(FSourceFileName);
  FreeMem(FAppFileName);

  with FConsole do
  begin
    ReadLn(sPressEnterToExit);
  {$IFDEF Compat}
    CodePage := GetOEMCP;
  {$ENDIF}
    Free;
  end;
end;

procedure TApplication.Help;
begin
  FConsole.WriteLn(sCat);
end;

procedure TApplication.Run;
var
  DestFileName: PWideChar;
begin
  if FSourceFileName <> nil then
  begin
    // TODO
    Exit;
  end;

  Help;
end;

end.

