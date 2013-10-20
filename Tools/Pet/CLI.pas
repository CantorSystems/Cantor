(*
    Pet's command line interface

    Copyright © 2013 Vladislav Javadov (Freeman)
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
  Windows, CoreClasses, CoreStrings, PetConsts, ExeImages;

{ Helper functions }

function ChangeFileExt(FileName, Ext: PCoreChar): PCoreChar;
var
  P: PCoreChar;
  D, E, L: Cardinal;
begin
  if FileName <> nil then
  begin
    L := WideStrLen(FileName);
    P := WideStrRScan(FileName, '.', L);
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
  FConsole.WriteLn(@Buf, FormatBuf(sTitle, [sVersion], Buf), 2);

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
    Free;
  end;
end;

procedure TApplication.Help;
begin
  FConsole.WriteLn(sCat);
end;

procedure TApplication.Run;

procedure ReadSource;
var
  F: TFileStream;
  Stub: TExeStub;
begin
  F := TFileStream.Create(FSourceFileName, faRead);
  try
    Stub := TExeStub.Create(F);
    try
      FConsole.WriteLn('Stub size: %d', [Stub.Size]);
    finally
      Stub.Free;
    end;
  finally
    F.Free;
  end;
end;

begin
  if FSourceFileName <> nil then
  begin
    ReadSource;
    Exit;
  end;

  Help;
end;

end.

