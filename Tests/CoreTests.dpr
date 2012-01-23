(*
    Core Library Tests

    Copyright (c) 2009 The Unified Environment Laboratory

    Conditional defines:
      * ForceMMX - allow MMX with FastCode
      * Tricks  - use KOL/TuniLab lite System unit
      * Unicode - use Unicode-based (WideChar) API, Windows 2000 and above

    Search path:  ..\..\Core;..\..\Core\Common;..\..\Delphi
*)

program CoreTests;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  ShareMM,
{$ENDIF}
  Exceptions,
  Lite,
  Core,
  Strings,
  Main in 'Main.pas';

const
  sMMX = 'This program requires MMX';
  sPlatformRequired = 'This program requires Windows ' +
    {$IFDEF Unicode} '2000' {$ELSE} '98' {$ENDIF};
  sPressEnterToExit = 'Press ENTER to exit';

begin
{$IFDEF Tricks}
  UseErrorMessageWrite;
{$ENDIF}

{$IFDEF ForceMMX}
  if not MMX_Supported then
  begin
  {$IFDEF Tricks}
    ErrorMessage(sMMX, StrLen(sMMX));
  {$ELSE}
    WriteLn(sMMX);
  {$ENDIF}
    Exit;
  end;
{$ENDIF}

{$IFDEF Unicode}
  if not IsPlatformUnicode then
{$ELSE}
  if not IsPlatformWindows then
{$ENDIF}
  begin
  {$IFDEF Tricks}
    ErrorMessage(sPlatformRequired, StrLen(sPlatformRequired));
  {$ELSE}
    WriteLn(sPlatformRequired);
  {$ENDIF}
    Exit;
  end;

  Randomize;

  Title;

  try
    Run;
  except
    on E: Exception do
    begin
      ShowException(E);
      WriteLn;
    end;
  end;

  ReadLn(sPressEnterToExit);
end.

