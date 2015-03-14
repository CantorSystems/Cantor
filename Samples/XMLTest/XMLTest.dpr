(*
    CoreLite XML Test

    Conditional defines (in Project Options -> Directories/Conditionals):
      * CoreLiteVCL -- in order to compile CoreLite with Delphi RTL classes
      * Debug -- for Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Lite -- CoreLite stream without virtual methods
      * Tricks -- when using tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program XMLTest;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreExceptions,
  CoreUtils,
  TestConsts in 'TestConsts.pas',
  TestCore in 'TestCore.pas';

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
    Application.Create(GetCommandLineW);
    Application.Run;
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
 