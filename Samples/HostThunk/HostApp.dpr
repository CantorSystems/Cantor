(*
    Host application using core library sample

    Copyright (c) 2013, 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program HostApp;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  HostThunk,
  Windows,
  CoreUtils,
  CoreExceptions,
  CoreWrappers;

const
  CoreLib = 'CoreLib.dll';

//procedure InitCore(const Thunk: TThunk); stdcall; external CoreLib name 'Init';
procedure InitCore(const Thunk: TThunk); external CoreLib name '@@Init';
procedure Fall; external CoreLib;

begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}
  UseExceptionMessageWrite;

{$IFDEF ForceMMX}
  if not MMX_Supported then
  begin
    ErrorMessage(sMMX, StrLen(sMMX));
    Halt(1);
  end;
{$ENDIF}

  try
    InitCore(MakeThunk);
    Fall;
  except
    on E: Exception do
    begin
      ShowException(E);
      ExitCode := 1;
    end;
  end;

  with TStreamConsole.Create(True) do
  try
    //CodePage := CP_UTF8;
    ReadLn('Press ENTER to exit');
  finally
    Free;
  end;

end.

