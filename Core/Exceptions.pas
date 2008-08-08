(*
    The Unified Environment, legacy Win32 core

    Core exceptions implementation

    Copyright (c) 2008-2009 The Unified Environment Laboratory
*)

unit Exceptions;

interface

uses
  Core, Strings, Localization;

{ Exceptions }

//var
//  ExceptionMessageClass: TStringClass;

type
  Exception = class;
  ExceptionClass = class of Exception;

  Exception = class(TObject) // platform-dependent, non-core class
  private
  {$IFDEF Lite}
    FMessage: CoreString;
  {$ELSE}
    FMessage: TUniString;
  {$ENDIF}
  public
  {$IFDEF Lite}
    constructor Create(const Msg: CoreString); overload;
    constructor Create(const Msg: CoreString; const Args: array of const); overload;
    constructor Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal); overload;
    constructor Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal;
      const Args: array of const); overload;
  // properties
    property Message: CoreString read FMessage;
  {$ELSE}
    destructor Destroy; override;
  // properties
    property Message: TUniString read FMessage;
  {$ENDIF}
  end;

  ECoreError = class(Exception);

  EAbort = class(ECoreError);

  EHeapException = class(ECoreError)
  private
    AllowFree: Boolean;
  public
    procedure FreeInstance; override;
  end;

  EOutOfMemory = class(EHeapException);
  EInvalidPointer = class(EHeapException);

  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    ExceptionInformation: array[0..14] of Cardinal;
  end;

  EExternal = class(ECoreError)
  private
    FExceptionRecord: PExceptionRecord;
  public
  // properties
    property ExceptionRecord: PExceptionRecord read FExceptionRecord;
  end;

  EIntError = class(EExternal);
  EDivByZero = class(EIntError);
  ERangeError = class(EIntError);
  EIntOverflow = class(EIntError);

  EMathError = class(EExternal);
  EInvalidOp = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow = class(EMathError);
  EUnderflow = class(EMathError);

  EInvalidCast = class(ECoreError);

  EAccessViolation = class(EExternal);
  EPrivInstruction = class(EExternal);
  EControlBreak = class(EExternal);
  EStackOverflow = class(EExternal);

  EConvertError = class(ECoreError);

{$IFOPT C+}
  EAssertionFailed = class(Exception);
{$ENDIF}

  EAbstractError = class(ECoreError);

  EPlatformError = class(ECoreError)
  private
    FErrorCode: Cardinal;
  public
    constructor Create; overload;
    constructor Create(ErrorCode: Cardinal); overload;
  // properties
    property ErrorCode: Cardinal read FErrorCode;
  end;

//  ESafecallException = class(ECoreError);

{ Exception handling routines }

{function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PAnsiChar; Size: Integer): Integer;}

procedure Abort;
procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
{$IF Defined(Tricks) and Defined(Unicode)}
procedure UseExceptionMessageBox;
{$IFEND}

procedure RaiseLastPlatformError;
function SysErrorMessage(ErrorCode: Integer; Buffer: PCoreChar;
  Count: Integer): Integer;

implementation

uses
{$IFDEF Lite}
  Lite, SysUtils, Unicode,
{$ENDIF}
  Windows, SysConst;

const // TODO: Localization
  sError = 'Error';
  sUnknownException = 'Module %s raised %s at %p';

{ Exception handling routines }

var
  OutOfMemory: EOutOfMemory;
  InvalidPointer: EInvalidPointer;

{type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;}

procedure Abort;

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP + 4]
  end;

begin
//  raise EAbort.Create(sOperationAborted) at ReturnAddr;
end;

procedure RaiseLastPlatformError;
var
  LastError: Cardinal;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    raise EPlatformError.Create(LastError);
end;

function SysErrorMessage(ErrorCode: Integer; Buffer: PCoreChar;
  Count: Integer): Integer;
begin
  Result := {$IFDEF Unicode} FormatMessageW {$ELSE} FormatMessageA {$ENDIF}
    (FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0, Buffer, Count, nil);
end;

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PCoreChar; Size: Integer): Integer; {$IFDEF Tricks} overload; {$ENDIF}

function ConvertAddr(Address: Pointer): Pointer;
asm
        TEST    EAX,EAX         { Always convert nil to nil }
        JE      @@1
        SUB     EAX, $1000      { offset from code start; code start set by linker to $1000 }
@@1:
end;

var
  ModuleName: array[0..MAX_PATH] of CoreChar;
  Info: TMemoryBasicInformation;
  ConvertedAddress: Pointer;
  Name: PCoreChar;
  ClassName: ShortString;
{$IFDEF Unicode}
  CoreClassName: array[0..256] of WideChar;
{$ENDIF}
  Msg: CoreString;
begin
  if ExceptObject is Exception then
  begin
    Msg := Exception(ExceptObject).Message;
    Result := Length(Msg);
    if Result > Size then
      Result := Size;
  {$IFDEF Unicode}
    StrLCopyW(Buffer, Pointer(Msg), Result);
  {$ELSE}
    StrLCopy(Buffer, Pointer(Msg), Result);
  {$ENDIF}
  end
  else
  begin
    VirtualQuery(ExceptAddr, Info, SizeOf(Info));
    if (Info.State <> MEM_COMMIT) or (
      {$IFDEF Unicode} GetModuleFileNameW {$ELSE} GetModuleFileNameA {$ENDIF}
        (THandle(Info.AllocationBase), ModuleName, Length(ModuleName)) = 0) then
    begin
      {$IFDEF Unicode} GetModuleFileNameW {$ELSE} GetModuleFileNameA {$ENDIF}
        (HInstance, ModuleName, Length(ModuleName));
      ConvertedAddress := ConvertAddr(ExceptAddr);
    end
    else
      Integer(ConvertedAddress) := Integer(ExceptAddr) - Integer(Info.AllocationBase);
  {$IFDEF Unicode}
    Name := StrRScanW(ModuleName, '\'); // TODO: PathDelimiter
    if Name <> nil then
      Inc(Name); // auto SizeOf(WideChar);
  {$ELSE}
    Name := AnsiStrRScan(ModuleName, '\') + 1; // TODO: PathDelimiter
    if Name <> nil then
      Inc(Name); // auto SizeOf(AnsiChar);
  {$ENDIF}
    ClassName := ExceptObject.ClassName;
  {$IFDEF Unicode}
    if MultiByteToWideChar(CP_ACP, 0, @ClassName[1], Length(ClassName),
      @CoreClassName, SizeOf(CoreClassName)) = 0
    then
      CoreClassName[0] := WideChar(#0);
    Result := WideFormatBuf(sException, [CoreClassName, Name, ConvertedAddress], Buffer);
  {$ELSE}
    ClassName[Length(ClassName) + 1] := #0; 
    Result := FormatBuf(sException, [@ClassName[1], Name, ConvertedAddress], Buffer);
  {$ENDIF}
  end;
end;

{$IF Defined(Tricks) and Defined(Unicode)}
procedure ExceptionErrorMessage(Msg: PCoreChar; MsgLen: Integer); overload;
var
  Buffer: array[0..1023] of AnsiChar;
begin
  ErrorMessage(Buffer, WideCharToMultiByte(CP_OEMCP, 0, Msg, MsgLen,
    @Buffer, SizeOf(Buffer), nil, nil));
end;

var
  ExceptionMessageProc: procedure(Msg: PCoreChar; MsgLen: Integer) = ExceptionErrorMessage;

procedure ExceptionMessageBox(Msg: PCoreChar; MsgLen: Integer);
begin
  MessageBoxW(0, Msg, sError, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
end;

procedure UseExceptionMessageBox;
begin
  ExceptionMessageProc := ExceptionMessageBox;
end;
{$IFEND}

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
var
  Buffer: array[0..1023] of CoreChar;
  Len: Integer;
{$IFNDEF Tricks}
  Dummy: Cardinal;
  hError: THandle;
  AnsiBuf: array[0..1023] of AnsiChar;
{$ENDIF}
begin
  Len := ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
{$IFDEF Tricks}
  {$IFDEF Unicode}
    ExceptionMessageProc(Buffer, Len);
  {$ELSE}
    ErrorMessage(Buffer, Len);
  {$ENDIF}
{$ELSE}
  if IsConsole then
  begin
    Flush(Output);
    hError := GetStdHandle(STD_ERROR_HANDLE);
  {$IFDEF Unicode}
    Len := WideCharToMultiByte(CP_OEMCP, 0, Buffer, Len, nil, 0, nil, nil);
    WriteFile(hError, Buffer, Len, Dummy, nil);
  {$ELSE}
    WriteFile(hError, Buffer, Len, Dummy, nil);
  {$ENDIF}
    WriteFile(hError, sLineBreak[1], Length(sLineBreak), Dummy, nil);
  end
  else
    MessageBox(0, Buffer, sExceptTitle, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
{$ENDIF}
end;

{ Error handlers }

procedure AbstractErrorHandler;
begin
  raise EAbstractError.Create;
end;

procedure ErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case ErrorCode of
    1: E := OutOfMemory;
    2: E := InvalidPointer;
//    3..24: with ExceptMap[ErrorCode] do E := EClass.Create(EIdent);
  else
    //E := CreateInOutError;
    Exit;
  end;
  raise E at ErrorAddr;
end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); 
begin
  ShowException(ExceptObject, ExceptAddr);
  Halt(1);
end;

{type
  TExceptRec = record
    EClass: ExceptionClass;
    EIdent: PAnsiChar;
  end;

const
  MapLimit = 24;
  ExceptMap: array[3..MapLimit] of TExceptRec = (
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: EAccessViolation; EIdent: SAccessViolation),
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlC; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: EVariantError; EIdent: SInvalidVarCast),
    (EClass: EVariantError; EIdent: SInvalidVarOp),
    (EClass: EVariantError; EIdent: SDispatchError),
    (EClass: EVariantError; EIdent: SVarArrayCreate),
    (EClass: EVariantError; EIdent: SVarNotArray),
    (EClass: EVariantError; EIdent: SVarArrayBounds),
    (EClass: EAssertionFailed; EIdent: SAssertionFailed),
    (EClass: EExternalException; EIdent: SExternalException),
    (EClass: EIntfCastError; EIdent: SIntfCastError),
    (EClass: ESafecallException; EIdent: SSafecallException)
  );}

function MapException(P: PExceptionRecord): TRuntimeError;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:
      Result := System.reDivByZero;
    STATUS_ARRAY_BOUNDS_EXCEEDED:
      Result := System.reRangeError;
    STATUS_INTEGER_OVERFLOW:
      Result := System.reIntOverflow;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:
      Result := System.reInvalidOp;
    STATUS_FLOAT_DIVIDE_BY_ZERO:
      Result := System.reZeroDivide;
    STATUS_FLOAT_OVERFLOW:
      Result := System.reOverflow;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:
      Result := System.reUnderflow;
    STATUS_ACCESS_VIOLATION:
      Result := System.reAccessViolation;
    STATUS_PRIVILEGED_INSTRUCTION:
      Result := System.rePrivInstruction;
    STATUS_CONTROL_C_EXIT:
      Result := System.reControlBreak;
    STATUS_STACK_OVERFLOW:
      Result := System.reStackOverflow;
    else
      Result := System.reExternalException;
  end;
end;

function GetExceptionClass(P: PExceptionRecord): ExceptionClass;
var
  ErrorCode: Byte;
begin
  ErrorCode := Byte(MapException(P));
  Result := Exception;
//  Result := ExceptMap[ErrorCode].EClass; // TODO
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  ErrorCode: Integer;

  function CreateAVObject: Exception;
  var
    AccessOp: string; // string ID indicating the access type READ or WRITE
    AccessAddress: Pointer;
    MemInfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of Char;
  begin
  {  with P^ do
    begin
      if ExceptionInformation[0] = 0 then
        AccessOp := SReadAccess
      else
        AccessOp := SWriteAccess;
      AccessAddress := Pointer(ExceptionInformation[1]);
      VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
      if (MemInfo.State = MEM_COMMIT) and
         (GetModuleFileName(THandle(MemInfo.AllocationBase), ModName, SizeOf(ModName)) <> 0) then
        Result := EAccessViolation.CreateFmt(sModuleAccessViolation,
          [ExceptionAddress, ExtractFileName(ModName), AccessOp,
          AccessAddress])
      else
        Result := EAccessViolation.CreateFmt(sAccessViolation,
          [ExceptionAddress, AccessOp, AccessAddress]);
    end;}
    Result := EPlatformError.Create;
  end;

begin
  ErrorCode := Byte(MapException(P));
  case ErrorCode of
    3..10, 12..21:
      Result := Exception.Create;
      // TODO with ExceptMap[ErrorCode] do Result := EClass.Create(EIdent);
    11: Result := CreateAVObject;
  else
    Result := EExternal.Create;
    // TODO: Result := EExternalException.CreateFmt(SExternalException, [P.ExceptionCode]);
  end;
  if Result is EExternal then
    EExternal(Result).FExceptionRecord := P;
end;

procedure InitExceptions;
begin
{$IFDEF Lite}
  OutOfMemory := EOutOfMemory.Create(sOutOfMemory, Length(sOutOfMemory), CP_ACP);
  InvalidPointer := EInvalidPointer.Create(sInvalidPointer, Length(sInvalidPointer), CP_ACP);
{$ELSE}
  // TODO
{$ENDIF}
  ErrorProc := ErrorHandler;
  ExceptProc := @ExceptHandler;
  System.ExceptionClass := Exception;

  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
{$IFOPT C+}
  AssertErrorProc := // TODO
{$ENDIF}
end;

procedure DoneExceptions;
begin
  OutOfMemory.AllowFree := True;
  OutOfMemory.FreeInstance;
  OutOfMemory := nil;

  InvalidPointer.AllowFree := True;
  InvalidPointer.Free;
  InvalidPointer := nil;

  ErrorProc := nil;
  ExceptProc := nil;
  System.ExceptionClass := nil;

  ExceptClsProc := nil;
  ExceptObjProc := nil;

{$IFOPT C+}
  AssertErrorProc := nil;
{$ENDIF}
end;

{ Exception }

{$IFDEF Lite}

constructor Exception.Create(const Msg: CoreString);
begin
  FMessage := Msg;
end;

constructor Exception.Create(const Msg: CoreString; const Args: array of const);
begin
  FMessage := Lite.Format(Msg, Args);
end;

constructor Exception.Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal);
begin
{$IFDEF Unicode}
  FMessage := DecodeString(Msg, Count, CodePage);
{$ELSE}
  SetString(FMessage, Msg, Count);
{$ENDIF}
end;

constructor Exception.Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal;
  const Args: array of const);
begin
  FMessage := Lite.Format(Msg, Count, CodePage, Args);
end;

{$ELSE}

destructor Exception.Destroy;
begin
  FMessage.Free;
  inherited;
end;

{$ENDIF}

{ EHeapException }

procedure EHeapException.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

{ EPlatformError }

constructor EPlatformError.Create;
begin
  Create(GetLastError);
end;

constructor EPlatformError.Create(ErrorCode: Cardinal);
{$IFDEF Lite}
begin
  Create(Lite.SysErrorMessage(ErrorCode));
end;
{$ELSE}
var
  Len: Integer;
begin
//  FMessage := TMemoryString.Create; // TODO
//  Len := {$IFDEF Unicode} FormatMessageW {$ELSE} FormatMessageA {$ENDIF}
//    (FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0, nil, 0, nil);
//  TMemoryString(Message).Length := Len;
//  {$IFDEF Unicode} FormatMessageW {$ELSE} FormatMessageA {$ENDIF}
//    (FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0, TMemoryString(Message).Data, Len, nil);
  FErrorCode := ErrorCode;
end;
{$ENDIF}

{ RTL error handler }

(*type
  TExceptRec = record
    EClass: ExceptionClass;
    EIdent: string;
  end;

const
  ExceptMap: array[3..{$IFOPT C+} 24 {$ELSE} 23 {$ENDIF}] of TExceptRec = (
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: EAccessViolation; EIdent: SAccessViolation),
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlC; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: EVariantError; EIdent: SInvalidVarCast),
    (EClass: EVariantError; EIdent: SInvalidVarOp),
    (EClass: EVariantError; EIdent: SDispatchError),
    (EClass: EVariantError; EIdent: SVarArrayCreate),
    (EClass: EVariantError; EIdent: SVarNotArray),
    (EClass: EVariantError; EIdent: SVarArrayBounds),
  {$IFOPT C+}
    (EClass: EAssertionFailed; EIdent: SAssertionFailed),
  {$ENDIF}  
    (EClass: EExternalException; EIdent: SExternalException),
    (EClass: EIntfCastError; EIdent: SIntfCastError),
    (EClass: ESafecallException; EIdent: SSafecallException));

TRuntimeError = (reNone, reOutOfMemory, reInvalidPtr, reDivByZero,
  reRangeError, reIntOverflow, reInvalidOp, reZeroDivide, reOverflow,
  reUnderflow, reInvalidCast, reAccessViolation, rePrivInstruction,
  reControlBreak, reStackOverflow,
  { reVar* used in Variants.pas }
  reVarTypeCast, reVarInvalidOp,
  reVarDispatch, reVarArrayCreate, reVarNotArray, reVarArrayBounds,
  reAssertionFailed,
  reExternalException, { not used here; in SysUtils }
  reIntfCastError, reSafeCallError);    

procedure ErrorHandler(ErrorCode: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case ErrorCode of
    1: E := OutOfMemory;
    2: E := InvalidPointer;
    3..24: with ExceptMap[ErrorCode] do E := EClass.Create(EIdent);
  else
    E := CreateInOutError;
  end;
  raise E at ErrorAddr;
end;

{$IFOPT C+}

{ Assertion error handler }

function CreateAssertException(const Message, FileName: string;
  LineNumber: Integer): Exception;
var
  S: string;
begin
  if Message <> '' then S := Message else S := SAssertionFailed;
  Result := EAssertionFailed.CreateFmt(SAssertError,
         [S, FileName, LineNumber]);
end;

{ This code is based on the following assumptions:                         }
{  - Our direct caller (AssertErrorHandler) has an EBP frame               }
{  - ErrorStack points to where the return address would be if the         }
{    user program had called System.@RaiseExcept directly                  }
procedure RaiseAssertException(const E: Exception; const ErrorAddr, ErrorStack: Pointer);
asm
        MOV     ESP,ECX
        MOV     [ESP],EDX
        MOV     EBP,[EBP]
        JMP     System.@RaiseExcept
end;

{ If you change this procedure, make sure it does not have any local variables }
{ or temps that need cleanup - they won't get cleaned up due to the way        }
{ RaiseAssertException frame works. Also, it can not have an exception frame.  }
procedure AssertErrorHandler(const Message, FileName: string;
  LineNumber: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
   E := CreateAssertException(Message, FileName, LineNumber);
   RaiseAssertException(E, ErrorAddr, PAnsiChar(@ErrorAddr)+4);
end;

{$ENDIF}

{ Abstract method invoke error handler }

procedure AbstractErrorHandler;
begin
  raise EAbstractError.CreateFmt(SAbstractError, ['']);
end;

function MapException(P: PExceptionRecord): Byte;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:
      Result := 3;
    STATUS_ARRAY_BOUNDS_EXCEEDED:
      Result := 4;
    STATUS_INTEGER_OVERFLOW:
      Result := 5;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:
      Result := 6;
    STATUS_FLOAT_DIVIDE_BY_ZERO:
      Result := 7;
    STATUS_FLOAT_OVERFLOW:
      Result := 8;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:
      Result := 9;
    STATUS_ACCESS_VIOLATION:
      Result := 11;
    STATUS_PRIVILEGED_INSTRUCTION:
      Result := 12;
    STATUS_CONTROL_C_EXIT:
      Result := 13;
    STATUS_STACK_OVERFLOW:
      Result := 14;
  else
    Result := 22; { must match System.reExternalException }
  end;
end;

function GetExceptionClass(P: PExceptionRecord): ExceptClass;
var
  ErrorCode: Byte;
begin
  ErrorCode := MapException(P);
  Result := ExceptMap[ErrorCode].EClass;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  ErrorCode: Integer;

  function CreateAVObject: Exception;
  var
    AccessOp: string; // string ID indicating the access type READ or WRITE
    AccessAddress: Pointer;
    MemInfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of Char;
  begin
    with P^ do
    begin
      if ExceptionInformation[0] = 0 then
        AccessOp := SReadAccess else
        AccessOp := SWriteAccess;
      AccessAddress := Pointer(ExceptionInformation[1]);
      VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
      if (MemInfo.State = MEM_COMMIT) and (GetModuleFileName(THandle(MemInfo.AllocationBase),
        ModName, SizeOf(ModName)) <> 0) then
        Result := EAccessViolation.CreateFmt(sModuleAccessViolation,
          [ExceptionAddress, ExtractFileName(ModName), AccessOp,
          AccessAddress])
      else Result := EAccessViolation.CreateFmt(sAccessViolation,
          [ExceptionAddress, AccessOp, AccessAddress]);
    end;
  end;

begin
  ErrorCode := MapException(P);
  case ErrorCode of
    3..10, 12..21:
      with ExceptMap[ErrorCode] do Result := EClass.Create(EIdent);
    11: Result := CreateAVObject;
  else
    Result := EExternalException.CreateFmt(SExternalException, [P.ExceptionCode]);
  end;
  if Result is EExternal then
  begin
    EExternal(Result).ExceptionRecord := P;
//    if P.ExceptionCode = $0EEFFACE then
//      Result.FMessage := 'C++ Exception';  // do not localize
  end;
end;

{ RTL exception handler }

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
begin
  ShowException(ExceptObject, ExceptAddr);
  Halt(1);
end;

end; *)

initialization
  InitExceptions;

finalization
  DoneExceptions;

end.
 