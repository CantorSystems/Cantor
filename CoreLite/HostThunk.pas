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
  {$IFDEF Tricks}
    ErrorMessage: procedure(Msg: PLegacyChar; Length: Integer); stdcall;
    ExceptionMessage: procedure(Msg: PWideChar; Length: Integer); stdcall;
  {$ELSE}
    IsConsole: Boolean;
  {$ENDIF}
    SuppressErrorMessages: Boolean;
  end;

  TThunk = packed record
    MemoryManager: TMemoryManager;
  {$IFDEF Tricks}
    ErrorMessage: procedure(Msg: PLegacyChar; Length: Integer);
    ExceptionMessage: procedure(Msg: PWideChar; Length: Integer);
  {$ELSE}
    IsConsole: Boolean;
  {$ENDIF}
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

{$IFDEF Tricks}
procedure ErrorMessageThunk(Msg: PLegacyChar; Length: Integer);
asm
        POP ECX
        PUSH EDX
        PUSH EAX
        PUSH ECX
        JMP HostApp.ErrorMessage
end;

procedure ExceptionMessageThunk(Msg: PWideChar; Length: Integer);
asm
        POP ECX
        PUSH EDX
        PUSH EAX
        PUSH ECX
        JMP HostApp.ErrorMessage
end;
{$ENDIF}

procedure StdCallInit(const Thunk: THostThunk);
var
  MM: TMemoryManager;
begin
  HostApp := Thunk;
{$IFDEF Tricks}
  ErrorMessage := ErrorMessageThunk;
  ExceptionMessage := ExceptionMessageThunk;
{$ELSE}
  IsConsole := Thunk.IsConsole;
{$ENDIF}
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
{$IFDEF Tricks}
  ErrorMessage := Thunk.ErrorMessage;
  ExceptionMessage := Thunk.ExceptionMessage;
{$ELSE}
  IsConsole := Thunk.IsConsole;
{$ENDIF}
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
  {$IFDEF Tricks}
    ErrorMessage := {$IFDEF Tricks} System {$ELSE} CoreUtils {$ENDIF}.ErrorMessage;
    ExceptionMessage := CoreExceptions.ExceptionMessage;
  {$ELSE}
    IsConsole := System.IsConsole;
  {$ENDIF}
    SuppressErrorMessages := NoErrMsg;
  end;
end;

initialization
  GetMemoryManager(SaveMemoryManager);

finalization
  SetMemoryManager(SaveMemoryManager);
{$IFDEF Tricks}
  ErrorMessage := nil;
  ExceptionMessage := nil;
{$ENDIF}

end.
