(*
    Core library for host application sample 

    Copyright (c) 2013, 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- use Delphi IDE friendly exceptions
      * ForceMMX -- allow MMX with FastCode
      * Tricks  -- use tricky lite System unit

    Search path:  ..\..\CoreLite
*)

library CoreLib;

{$IMAGEBASE $330000}

uses
{$IFDEF Tricks}
  HeapMM, // cannot start without a memory manager
{$ENDIF}
  HostThunk,
  Windows,
  CoreUtils,
  CoreExceptions;

type
  EFall = class(Exception);

procedure Fall;
begin
  raise EFall.Create('Wazzup?!');
end;

exports
  StdCallInit name 'Init',
  FastCallInit name '@@Init',
  Fall;

end.
