(*
    Memory manager powered by host program

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit HostMM;

interface

type
  THostMemoryManager = packed record
    GetMem: function(Size: Integer): Pointer; stdcall;
    FreeMem: function(P: Pointer): Integer; stdcall;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer; stdcall;
  end;

procedure SetHostMemoryManager(const MM: THostMemoryManager); stdcall;

{
exports
  SetMemoryManager name 'DelphiInit',
  SetHostMemoryManager name 'StdInit';
}

implementation

var
  SaveMemoryManager: TMemoryManager;
  HostMemoryManager: THostMemoryManager;

function WrapGetMem(Size: Integer): Pointer;
asm
        PUSH EAX
        JMP HostMemoryManager.GetMem
end;

function WrapFreeMem(P: Pointer): Integer;
asm
        PUSH EAX
        JMP HostMemoryManager.FreeMem
end;

function WrapReallocMem(P: Pointer; Size: Integer): Pointer;
asm
        PUSH EDX
        PUSH EAX
        JMP HostMemoryManager.ReallocMem
end;

procedure SetHostMemoryManager(const MM: THostMemoryManager); 
var
  Wrapper: TMemoryManager;
begin
  HostMemoryManager := MM;
  with Wrapper do
  begin
    GetMem := WrapGetMem;
    FreeMem := WrapFreeMem;
    ReallocMem := WrapReallocMem;
  end;
  SetMemoryManager(Wrapper);
end;

initialization
  GetMemoryManager(SaveMemoryManager);

finalization
  SetMemoryManager(SaveMemoryManager);

end.


