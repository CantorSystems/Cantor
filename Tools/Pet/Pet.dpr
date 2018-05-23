(*
    Portable Executable (PE) Tool

    Copyright (c) 2013-2016 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Lite -- lite CoreWrappers.THandleStream and lite PetCore
      * NoASLR -- build without ALSR feature (Delphi 2007+)
      * Tricks -- when using tricky lite System unit

    Search path:  ..\..\CoreLite
*)

program Pet;

{$APPTYPE CONSOLE}
{$R Resources.res}

uses
{$IFDEF Tricks}
  {$IFDEF ForceMMX} ShareMM, CoreConsts, {$ELSE} HeapMM, {$ENDIF}
{$ENDIF}
  Windows,
  CoreExceptions,
  CoreUtils,
  PetCore in 'PetCore.pas',
  PetConsts in 'PetConsts.pas';

{$I ..\..\CoreLite\ImageHelper.inc}
{$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_NX_COMPAT}

{$IFDEF NoASLR}
  {$IF CompilerVersion >= 16}
    {$SetPEFlags
      IMAGE_FILE_RELOCS_STRIPPED or
      IMAGE_FILE_DEBUG_STRIPPED or
      IMAGE_FILE_LINE_NUMS_STRIPPED or
      IMAGE_FILE_LOCAL_SYMS_STRIPPED}
  {$IFEND}
{$ELSE}
  {$IF CompilerVersion >= 18}
    {$DYNAMICBASE ON}
  {$ELSE}
    {$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE}
  {$IFEND}
{$ENDIF}

var
  Application: TApplication;
begin
{$IFDEF Tricks}
  FpuInit;
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
