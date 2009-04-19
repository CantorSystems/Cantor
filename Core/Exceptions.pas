(*
    The Unified Environment, legacy Win32 core

    Core exceptions implementation

    Copyright (c) 2008-2009 The Unified Environment Laboratory
*)

unit Exceptions;

interface

uses
{$IFDEF Tricks}
  SysSfIni,
{$ENDIF}
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
    constructor Create(Msg: PAnsiChar; CodePage: Cardinal); overload;
    constructor Create(Msg: PAnsiChar; CodePage: Cardinal;
       const Args: array of const); overload;
    constructor Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal); overload;
    constructor Create(Msg: PAnsiChar; Count: Integer; CodePage: Cardinal;
      const Args: array of const); overload;
    constructor Create(const Msg: WideString); overload;
    constructor Create(const Msg: WideString; const Args: array of const); overload;
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
  EPrivilege = class(EExternal);
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
procedure ShowException(E: Exception); overload;
procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer); overload;
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

procedure Abort;

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP + 4]
  end;

begin
{$IFDEF Lite}
  raise EAbort.Create(sOperationAborted) at ReturnAddr;
{$ENDIF}
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
  while (Result > 0) and
  {$IFDEF Unicode}
    (Buffer[Result] >= WideChar(#0)) and (Buffer[Result] <= WideChar(#32))
  {$ELSE}
    (Buffer[Result] in [#0..#32])
  {$ENDIF}
  do
    Dec(Result);
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
    if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(CP_ACP, 0,
      @ClassName[1], Length(ClassName), @CoreClassName, SizeOf(CoreClassName)) = 0
    then
      CoreClassName[0] := WideChar(#0);
    Result := WideFormatBuf(sUnknownException, [Name, CoreClassName, ConvertedAddress], Buffer);
  {$ELSE}
    ClassName[Length(ClassName) + 1] := #0;
    Result := FormatBuf(sUnknownException, [Name, @ClassName[1], ConvertedAddress], Buffer);
  {$ENDIF}
  end;
end;

{$IF Defined(Tricks) and Defined(Unicode)}
procedure ExceptionErrorMessage(Msg: PCoreChar; MsgLen: Integer); overload;
var
  Buffer: array[0..1023] of AnsiChar;
begin
  ErrorMessage(Buffer, System.WideCharToMultiByte(CP_OEMCP, 0, Msg, MsgLen,
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

procedure ShowException(E: Exception); 
begin
  ShowException(E, ExceptAddr);
end;

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
    Flush(System.Output);
    hError := GetStdHandle(STD_ERROR_HANDLE);
  {$IFDEF Unicode}
    Len := WideCharToMultiByte(CP_OEMCP, 0, Buffer, Len,
      AnsiBuf, SizeOf(AnsiBuf), nil, nil);
    WriteFile(hError, AnsiBuf, Len, Dummy, nil);
  {$ELSE}
    WriteFile(hError, Buffer, Len, Dummy, nil);
  {$ENDIF}
    WriteFile(hError, sLineBreak[1], Length(sLineBreak), Dummy, nil);
  end
  else
    {$IFDEF Unicode} MessageBoxW {$ELSE} MessageBoxA {$ENDIF}
      (0, @Buffer, sException, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
{$ENDIF}
end;

{ Error handlers }

procedure AbstractErrorHandler;
begin
  raise EAbstractError.Create;
end;

type
  TExceptRec = record
    EClass: ExceptionClass;
    EIdent: PAnsiChar;
  end;

const
  MapLimit = {$IFOPT C+} 23 {$ELSE} 22 {$ENDIF}; //24;
  ExceptMap: array[3..MapLimit] of TExceptRec = (
    (EClass: EDivByZero; EIdent: SDivByZero),
    (EClass: ERangeError; EIdent: SRangeError),
    (EClass: EIntOverflow; EIdent: SIntOverflow),
    (EClass: EInvalidOp; EIdent: SInvalidOp),
    (EClass: EZeroDivide; EIdent: SZeroDivide),
    (EClass: EOverflow; EIdent: SOverflow),
    (EClass: EUnderflow; EIdent: SUnderflow),
    (EClass: EInvalidCast; EIdent: SInvalidCast),
    (EClass: nil; EIdent: nil), // EAccessViolation,
    (EClass: EPrivilege; EIdent: SPrivilege),
    (EClass: EControlBreak; EIdent: SControlC),
    (EClass: EStackOverflow; EIdent: SStackOverflow),
    (EClass: nil; EIdent: nil), // EVariantError
    (EClass: nil; EIdent: nil),
    (EClass: nil; EIdent: nil),
    (EClass: nil; EIdent: nil),
    (EClass: nil; EIdent: nil),
    (EClass: nil; EIdent: nil),
  {$IFOPT C+}
    (EClass: EAssertionFailed; EIdent: SAssertionFailed),
  {$ELSE}
    (EClass: nil; EIdent: nil),
  {$ENDIF}
    (EClass: EExternal; EIdent: SExternalException){,
    (EClass: EIntfCastError; EIdent: SIntfCastError),
    (EClass: ESafecallException; EIdent: SSafecallException)}
  );

procedure ErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case ErrorCode of
    1: E := OutOfMemory;
    2: E := InvalidPointer;
    3..MapLimit:
      with ExceptMap[ErrorCode] do
        E := EClass.Create(EIdent);
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
  Result := ExceptMap[ErrorCode].EClass;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  ErrorCode: Integer;

  function CreateAVObject: Exception;
  var
    AccessOp: PAnsiChar; // string ID indicating the access type READ or WRITE
    AccessAddress: Pointer;
    MemInfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of CoreChar;
  begin
    with P^ do
    begin
      if ExceptionInformation[0] = 0 then
        AccessOp := sReadAccess
      else
        AccessOp := sWriteAccess;
      AccessAddress := Pointer(ExceptionInformation[1]);
      VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
      if (MemInfo.State = MEM_COMMIT) and
         ({$IFDEF Unicode} GetModuleFileNameW {$ELSE} GetModuleFileNameA {$ENDIF}
         (THandle(MemInfo.AllocationBase), ModName, SizeOf(ModName)) <> 0) then
        Result := EAccessViolation.Create(sModuleAccessViolation,
          {$IFDEF Unicode} CP_ACP, {$ENDIF}
          [ExceptionAddress, ExtractFileName(ModName),
          {$IFDEF Unicode} DecodeString(AccessOp, CP_ACP), {$ELSE} AccessOp, {$ENDIF}
           AccessAddress])
      else
        Result := EAccessViolation.Create(sAccessViolation,
          {$IFDEF Unicode} CP_ACP, {$ENDIF} [ExceptionAddress,
          {$IFDEF Unicode} DecodeString(AccessOp, CP_ACP), {$ELSE} AccessOp, {$ENDIF}
            AccessAddress]);
    end;
  end;

begin
  ErrorCode := Byte(MapException(P));
  case ErrorCode of
    3..10, 12..21:
      with ExceptMap[ErrorCode] do Result := EClass.Create(EIdent);
    11: Result := CreateAVObject;
  else
    Result := EExternal.Create(sExternalException,
      {$IFDEF Unicode} CP_ACP, {$ENDIF} [P.ExceptionCode]);
  end;
  if Result is EExternal then
    EExternal(Result).FExceptionRecord := P;
end;

var
  ErrorMode: Cardinal;

procedure InitExceptions;
begin
{$IFDEF Lite}
  OutOfMemory := EOutOfMemory.Create(sOutOfMemory, CP_ACP);
  InvalidPointer := EInvalidPointer.Create(sInvalidPointer, CP_ACP);
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
  if IsConsole then
    ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
end;

procedure DoneExceptions;
begin
  if IsConsole then
    SetErrorMode(ErrorMode);

{$IFOPT C+}
  AssertErrorProc := nil;
{$ENDIF}

  ErrorProc := nil;
  ExceptProc := nil;
  System.ExceptionClass := nil;

  ExceptClsProc := nil;
  ExceptObjProc := nil;

  OutOfMemory.AllowFree := True;
  OutOfMemory.FreeInstance;
  OutOfMemory := nil;

  InvalidPointer.AllowFree := True;
  InvalidPointer.Free;
  InvalidPointer := nil;
end;

{ Exception }

{$IFDEF Lite}

constructor Exception.Create(Msg: PAnsiChar; CodePage: Cardinal);
begin
{$IFDEF Unicode}
  FMessage := DecodeString(Msg, StrLen(Msg), CodePage);
{$ELSE}
  SetString(FMessage, Msg, StrLen(Msg));
{$ENDIF}
end;

constructor Exception.Create(Msg: PAnsiChar; CodePage: Cardinal;
  const Args: array of const);
begin
  FMessage := Lite.Format(Msg, CodePage, Args);
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

constructor Exception.Create(const Msg: WideString);
begin
  FMessage := Msg;
end;

constructor Exception.Create(const Msg: WideString; const Args: array of const);
begin
  FMessage := WideFormat(Pointer(Msg), Args);
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

initialization
  InitExceptions;

finalization
  DoneExceptions;

end.
