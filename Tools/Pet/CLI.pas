(*
    Portable Executable (PE) Tool

    Command line interface implementation

    Copyright (c) 2012 The Unified Environment Laboratory
*)

unit CLI;

interface

uses
  CoreUtils, CoreWrappers;

type
  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppFileName, FSourceFileName: PCoreChar;
    procedure Help;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

{ Helper functions }

function ChangeFileExt(FileName, Ext: PCoreChar): PCoreChar;
function ParamStr(var CommandLine: PCoreChar; var Quoted: Boolean): PCoreChar;

implementation

uses
  Windows, CoreClasses, CoreStrings, PetConsts;

{ Helper functions }

function ChangeFileExt(FileName, Ext: PCoreChar): PCoreChar;
var
  P: PCoreChar;
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
    GetMem(Result, (D + E + 1) * SizeOf(CoreChar));
    Move(FileName^, Result^, D * SizeOf(CoreChar));
    Move(Ext^, Result[D], (E + 1) * SizeOf(CoreChar));
  end
  else
    Result := nil;
end;

function ParamStr(var CommandLine: PCoreChar; var Quoted: Boolean): PCoreChar;
var
  P: PCoreChar;
  L: Cardinal;
begin
  if CommandLine <> nil then
  begin
    while (CommandLine^ = CoreChar(32)) or (CommandLine^ = CoreChar(9)) do
      Inc(CommandLine);
    if CommandLine^ = CoreChar('"') then
    begin
      Inc(CommandLine);
      L := WideStrLen(CommandLine);
      P := WideStrScan(CommandLine, CoreChar('"'), L);
      if P <> nil then
        L := P - CommandLine;
      Quoted := True;
    end
    else
    begin
      P := CommandLine;
      while (P^ <> CoreChar(32)) and (P^ <> CoreChar(9)) and (P^ <> CoreChar(0)) do
        Inc(P);
      L := P - CommandLine;
      Quoted := False;
    end;
    Result := WideStrNew(CommandLine, L);
    CommandLine := P;
    if P^ <> CoreChar(0) then
      Inc(CommandLine);
  end
  else
    Result := nil;
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PCoreChar);
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
  DestFileName: PCoreChar;
begin
  if FSourceFileName <> nil then
  begin
    // TODO
    Exit;
  end;

  Help;
end;

end.

