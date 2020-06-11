(*
    Textest -- simple text parsing test, CoreLite vs Delphi

    Conditional defines (in Project Options -> Directories/Conditionals):
      * CoreLiteVCL -- in order to compile CoreLite with Delphi RTL classes
      * Debug -- for Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Lite -- CoreLite stream without virtual methods
      * Tricks -- when using tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Textest;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows, CoreExceptions, CoreUtils, TestConsts, TestCore;

var
  Application: TApplication;
begin
{$IFDEF Tricks}
  InitFPU;
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
    on E: TObject do // TObject is for CoreLiteVCL
    begin
      Application.ShowException(E);
      ExitCode := 1;
    end;
  end;
  Application.Destroy;

end.
