(*
    Portable Executable (PE) Tool

    Copyright (c) 2013 Vladislav Javadov (Freeman)

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Lite -- commonly lite version of code
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Pet;

{$APPTYPE CONSOLE}
{$R *.res}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreUtils,
  CoreExceptions,
  PetCore in 'PetCore.pas',
  PetConsts in 'PetConsts.pas';

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

  with TApplication.Create(GetCommandLineW) do
  begin
    try
      Run;
    except
      on E: Exception do
      begin
        ShowException(E);
        ExitCode := 1;
      end;
    end;
    Free;
  end;
end.
 
