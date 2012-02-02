(*
    Core Library Tests

    Copyright (c) 2009-2012 The Unified Environment Laboratory

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * CustomStub -- get Windows requirement message from custom stub (planning)
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
  {$IFDEF CustomStub}
    ErrorMessage(PLegacyChar($0040002F), PByte($0040002E)^);
  {$ELSE}
    ErrorMessage(sPlatformRequired, StrLen(sPlatformRequired));
  {$ENDIF}
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

