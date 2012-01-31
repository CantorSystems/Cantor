(*
    Core Library Tests

    Copyright (c) 2009 The Unified Environment Laboratory

    Conditional defines:
      * ForceMMX - allow MMX with FastCode
      * Tricks  - use tricky lite System unit

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

var
  App: TApplication;

begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}
//  UseExceptionMessageWrite;

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

