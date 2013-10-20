(*
    Portable Executable (PE) Tool

    Copyright © 2013 Vladislav Javadov (Freeman)

    Conditional defines:
      * Compat -- use Delphi IDE friendly exceptions
      * CustomStub -- get Windows requirement message from custom stub (planning)
      * ForceMMX -- allow MMX with FastCode
      * HX -- don't check Unicode support for HX DOS Extender compatibility
      * Lite -- commonly lite version of code
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Pet;

{$APPTYPE CONSOLE}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreUtils,
  CoreExceptions,
  CLI in 'CLI.pas',
  PetConsts in 'PetConsts.pas';

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
  {$IFDEF CustomStub}
    ErrorMessage(PLegacyChar($0040002F), PByte($0040002E)^);
  {$ELSE}
    ErrorMessage(sPlatformRequired, StrLen(sPlatformRequired));
  {$ENDIF}
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
    Destroy;
  end;
end.
 
