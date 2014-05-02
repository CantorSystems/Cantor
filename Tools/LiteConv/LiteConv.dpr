(*
    Lite charset conversion utility (LiteConv)

    Copyright (c) 2013-2014 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Lite -- commonly lite version of code
      * Tricks  -- use tricky lite System unit
      * UTF32 -- enable UTF-32 and Unicode character blocks support

    Search path:  ..\..\CoreLite
*)

program LiteConv;

{$APPTYPE CONSOLE}
{$R *.res}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreUtils,
  CoreExceptions,
  ConvCore in 'ConvCore.pas',
  ConvConsts in 'ConvConsts.pas';

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
 