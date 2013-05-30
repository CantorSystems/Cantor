(*
    Pythia -- Delphi to FASM preprocessor for Kolibri OS

    Copyright (c) 2013 Vladislav Javadov (Freeman)

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * HX -- no Unicode support check for HX DOS Extender compatibility
      * Lite -- commonly lite version of code
      * LiteStrings -- no code page support for strings
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Pythia;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, {$ELSE} HeapMM, {$ENDIF} 
{$ENDIF}
  Windows,
  CoreUtils,
  CoreExceptions,
  PythiaCore in 'PythiaCore.pas',
  PythiaConsts in 'PythiaConsts.pas';

const
  sMMX = 'This program requires MMX';
  sPlatformRequired = 'This program requires Windows NT';

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
                     
{$IFNDEF HX}
  if not IsPlatformUnicode then
  begin
    ErrorMessage(sPlatformRequired, StrLen(sPlatformRequired));
    Exit;
  end;
{$ENDIF}

  with TApplication.Create(GetCommandLineW) do
  begin
    try
      Run;
    except
      on E: Exception do
        ShowException(E);
    end;
    Free;
  end;
end.
 
