(*
    Portable Executable (PE) Tool

    Copyright (c) 2013-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Kolibri -- proposed KOLIBRI format support
      * Lite -- CoreLite stream without virtual methods
      * Tricks -- when using tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Pet;

{$APPTYPE CONSOLE}
{$R Resources.res}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreExceptions,
  CoreUtils,
  PetCore in 'PetCore.pas',
  PetConsts in 'PetConsts.pas';

var
  Application: TApplication;
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
    Application.Create;
    Application.Run(GetCommandLineW);
  except
    on E: Exception do
    begin
      ShowException(E);
      Application.Pause;
      ExitCode := 1;
    end;
  end;
  Application.Destroy;
end.
 
