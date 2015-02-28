(*
    CoreLite host program thunk

    Copyright (c) 2013, 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * ForceMMX -- check for MMX support on initialization
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

{ Core library side }

procedure StdCallInit(const Thunk: THostThunk); stdcall;
procedure FastCallInit(const Thunk: TThunk);

{ Host application side }

function MakeThunk: TThunk;

implementation

uses
  CoreConsts, CoreExceptions;

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

procedure StdCallInit(const Thunk: THostThunk);
var
  MM: TMemoryManager;
begin
  HostApp := Thunk;
  ErrorMessage := ErrorMessageThunk;
  ExceptionMessage := ExceptionMessageThunk;
{$IF defined(ForceMMX) and not defined(Tricks)}
  if not MMX_Supported then
    raise EMMX.Create; // until memory manager isn't set yet
{$IFEND}
  NoErrMsg := Thunk.SuppressErrorMessages;
  with MM do
  begin
    GetMem := GetMemThunk;
    FreeMem := FreeMemThunk;
    ReallocMem := ReallocMemThunk;
  end;
  SetMemoryManager(MM);
end;

procedure FastCallInit(const Thunk: TThunk);
begin
  ErrorMessage := Thunk.ErrorMessage;
  ExceptionMessage := Thunk.ExceptionMessage;
{$IF Defined(ForceMMX) and not Defined(Tricks)}
  if not MMX_Supported then
    raise EMMX.Create; // until memory manager isn't set yet
{$IFEND}
  NoErrMsg := Thunk.SuppressErrorMessages;
  SetMemoryManager(Thunk.MemoryManager);
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
  {$ELSE}
    SuppressErrorMessages := False;
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
