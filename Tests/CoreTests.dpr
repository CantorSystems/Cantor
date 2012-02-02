(*
    Core Library Tests

    Copyright (c) 2009-2012 The Unified Environment Laboratory

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Tricks  -- use tricky lite System unit

    Search path:  ..\CoreLib
*)

program CoreTests;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  ShareMM,
{$ENDIF}
  Exceptions,
  CoreWrappers,
  CoreUtils,
  Main in 'Main.pas';

const
  sMMX = 'This program requires MMX';
  sPlatformRequired = 'This program requires Windows 2000';
  //sPlatformRequired = PLegacyChar($00400050);

var
  App: TApplication;

begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}
{$IFDEF Compat}
  UseExceptionMessageWrite;
{$ENDIF}

{$IFDEF ForceMMX}
  if not MMX_Supported then
  begin
    ErrorMessage(sMMX, StrLen(sMMX));
    Exit;
  end;
{$ENDIF}

  if not IsPlatformUnicode then
  begin
    ErrorMessage(sPlatformRequired, StrLen(sPlatformRequired));
    Exit;
  end;

  Randomize;

  App := TApplication.Create;
  try
    try
      App.Run;
    except
      on E: Exception do
        ShowException(E);
    end;
  finally
    App.Free;
  end;

end.

