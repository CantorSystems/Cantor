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
  Windows, CoreExceptions, ConvCore, ConvConsts;

var
  Application: TApplication;
begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}
  UseExceptionMessageWrite;

  try
    Application.Create;
    Application.Run(GetCommandLineW);
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      ExitCode := 1;
    end;
  end;
  Application.Destroy;

end.
