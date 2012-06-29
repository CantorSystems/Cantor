(*
    Portable Executable (PE) Tool

    Copyright (c) 2012 The Unified Environment Laboratory

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * CustomStub -- get Windows requirement message from custom stub (planning)
      * ForceMMX -- allow MMX with FastCode
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLib
*)

program Ono;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  Exceptions,
  CoreUtils,
  CLI in 'CLI.pas';

const
  sMMX = 'This program requires MMX';
  sPlatformRequired = 'This program requires Windows 2000';

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

  with TApplication.Create(GetCommandLineW) do
  try
    try
      Run;
    except
      on E: Exception do
        ShowException(E);
    end;
  finally
    Free;
  end;
end.
 
