(*
    CoreLite host program thunk

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit HostThunk;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  CoreUtils;

type
  THostMemoryManager = packed record
    GetMem: function(Size: Integer): Pointer; stdcall;
    FreeMem: function(P: Pointer): Integer; stdcall;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer; stdcall;
  end;

  THostThunk = packed record
    MemoryManager: THostMemoryManager;
    ErrorMessage: procedure(Msg: PLegacyChar; Length: Integer); stdcall;
    ExceptionMessage: procedure(Msg: PWideChar); stdcall;
    SuppressErrorMessages: Boolean;
  end;

  TThunk = packed record
    MemoryManager: TMemoryManager;
    ErrorMessage: procedure(Msg: PLegacyChar; Length: Integer);
    ExceptionMessage: procedure(Msg: PWideChar);
    SuppressErrorMessages: Boolean;
  end;

{ Server side }

procedure StdInit(const Thunk: THostThunk); stdcall;
procedure FastInit(const Thunk: TThunk);

{ Client side }

function MakeThunk: TThunk;

implementation

uses
  CoreExceptions;

var
  SaveMemoryManager: TMemoryManager;
  HostApp: THostThunk;

function GetMemThunk(Size: Integer): Pointer;
asm
        POP ECX
        PUSH EAX
        PUSH ECX
        JMP HostApp.MemoryManager.GetMem
end;

function FreeMemThunk(P: Pointer): Integer;
asm
        POP ECX
        PUSH EAX
        PUSH ECX
        JMP HostApp.MemoryManager.FreeMem
end;

function ReallocMemThunk(P: Pointer; Size: Integer): Pointer;
asm
        POP ECX
        PUSH EDX
        PUSH EAX
        PUSH ECX
        JMP HostApp.MemoryManager.ReallocMem
end;

procedure ErrorMessageThunk(Msg: PLegacyChar; Length: Integer);
asm
        POP ECX
        PUSH EDX
        PUSH EAX
        PUSH ECX
        JMP HostApp.ErrorMessage
end;

procedure ExceptionMessageThunk(Msg: PWideChar);
asm
        POP ECX
        PUSH EAX
        PUSH ECX
        JMP HostApp.ExceptionMessage
end;

procedure StdInit(const Thunk: THostThunk);
var
  MM: TMemoryManager;
begin
  HostApp := Thunk;
  with MM do
  begin
    GetMem := GetMemThunk;
    FreeMem := FreeMemThunk;
    ReallocMem := ReallocMemThunk;
  end;
  SetMemoryManager(MM);
  ErrorMessage := ErrorMessageThunk;
  ExceptionMessage := ExceptionMessageThunk;
  NoErrMsg := Thunk.SuppressErrorMessages;
end;

procedure FastInit(const Thunk: TThunk);
begin
  SetMemoryManager(Thunk.MemoryManager);
  ErrorMessage := Thunk.ErrorMessage;
  ExceptionMessage := Thunk.ExceptionMessage;
  NoErrMsg := Thunk.SuppressErrorMessages;
end;

function MakeThunk: TThunk;
begin
  with Result do
  begin
    GetMemoryManager(MemoryManager);
    ErrorMessage := {$IFDEF Tricks} System {$ELSE} CoreUtils {$ENDIF}.ErrorMessage;
    ExceptionMessage := CoreExceptions.ExceptionMessage;
  {$IFDEF Tricks}
    SuppressErrorMessages := NoErrMsg;
  {$ENDIF}
  end;
end;

initialization
  GetMemoryManager(SaveMemoryManager);

finalization
  SetMemoryManager(SaveMemoryManager);
  ErrorMessage := nil;
  ExceptionMessage := nil;

end.


