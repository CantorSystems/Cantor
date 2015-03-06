(*
    LiteConv -- sample text encoding console application using pure CoreLite

    Conditional defines (in Project Options -> Directories/Conditionals):
      * Debug -- for Delphi IDE friendly exceptions
      * Lite -- CoreLite stream without virtual methods
      * Tricks -- when using tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program LiteConv;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  HeapMM,
{$ENDIF}
  Windows,
  CoreExceptions,
  ConvCore in 'ConvCore.pas',
  ConvConsts in 'ConvConsts.pas';

var
  Application: TApplication;
begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}
  UseExceptionMessageWrite;

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
 
