(*
    Lite Core Library (CoreLite)

    Core exceptions implementation

    Copyright (c) 2008-2014 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- IDE friendly and SysUtils compatible exceptions and
          abstract method call diagnostic exceptions 
      * ForceMMX -- EMMX exception
      * Interfaces -- interface support
*)

unit CoreExceptions;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IFDEF Tricks}
  SysSfIni,
{$ENDIF}
  CoreUtils;

{ Exceptions }

type
  Exception = class;
  ExceptionClass = class of Exception;

  TExceptionOptions = set of (eoWideChar, eoFreeMessage, eoCanFree);

  Exception = class
  private
  {$IFDEF Debug}
    FDelphiMsg: PLegacyChar;
  {$ENDIF}
    FMessage: PWideChar;
    FOptions: TExceptionOptions;
  public
    constructor Create(Msg: PLegacyChar); overload;
    constructor Create(Msg: PLegacyChar; const Args: array of const); overload;
    constructor Create(Msg: PWideChar; Count: Integer); overload;
    constructor Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const); overload;
    destructor Destroy; override;
    procedure FreeInstance; override;

  {$IFDEF Debug}
    property DelphiMsg: PLegacyChar read FDelphiMsg;
  {$ENDIF}
    property Message: PWideChar read FMessage;
    property Options: TExceptionOptions read FOptions;
  end;

{$IFNDEF Debug}
  EAbstract = class(Exception)
  private
    procedure MethodCall(ClassType: TClass);
  end;

  EAbstractInstance = class(EAbstract)
  private
    FCallee: TObject;
  public
    constructor Create(Callee: TObject);
    property Callee: TObject read FCallee;
  end;

  EAbstractClass = class(EAbstract)
  private
    FCallee: TClass;
  public
    constructor Create(Callee: TClass);
    property Callee: TClass read FCallee;
  end;
{$ENDIF}

  EAbort = class(Exception);
  EInvalidCast = class(Exception);
{$IFOPT C+}
  EAssertionFailed = class(Exception);
{$ENDIF}

  EOutOfMemory = class(Exception);
  EInvalidPointer = class(Exception);

  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode, ExceptionFlags: LongWord;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: LongWord;
    ExceptionInformation: array [0..14] of CoreWord;
  end;

  EHard = class(Exception)
  private
    FInfo: PExceptionRecord;
  public
    property Info: PExceptionRecord read FInfo;
  end;

  EInteger = class(EHard);
  EDivByZero = class(EInteger);
  ERangeError = class(EInteger);
  EIntOverflow = class(EInteger);

  EFloat = class(EHard);
  EInvalidOp = class(EFloat);
  EZeroDivide = class(EFloat);
  EOverflow = class(EFloat);
  EUnderflow = class(EFloat);

  EPrivilege = class(EHard);
  EControlBreak = class(EHard);
  EStackOverflow = class(EHard);
  EAccessViolation = class(EHard);
  EGeneralFault = class(EHard);

{$IFDEF Interfaces}
  EInterface = class(Exception);
  EIntfNotSupported = class(EInterface);
  ESafecall = class(EInterface);
{$ENDIF}

  EPlatform = class(Exception)
  private
    FErrorCode: LongWord;
    FErrorSource: PCoreChar;
  public
    constructor Create(ErrorCode: LongWord; ErrorSource: PCoreChar = nil);
  {$IFNDEF Debug}
    destructor Destroy; override;
  {$ENDIF}
    property ErrorCode: LongWord read FErrorCode;
    property ErrorSource: PCoreChar read FErrorSource;
  end;

{$IFNDEF Tricks}
  EInOutError = EPlatform;
{$ENDIF}

{$IFDEF ForceMMX}
  EMMX = class(Exception)
  private
    // possible getting module name is unsafe because of use of MMX-powered procedures
  public
    constructor Create;
  end;
{$ENDIF}

{ Exception raising }

procedure Abort;
procedure RaiseLastPlatformError(ErrorSource: PCoreChar = nil);

{ Exception handling }

procedure DefaultExceptionMessage(Msg: PWideChar);
procedure ShowException(E: {$IFDEF Debug} TObject {$ELSE} Exception {$ENDIF});
procedure UseExceptionMessageBox;
procedure UseExceptionMessageWrite;

var
  ExceptionMessage: procedure(Msg: PWideChar) = DefaultExceptionMessage;

implementation

uses
  Windows, CoreConsts;

{ Error handlers }

var
  OutOfMemory: EOutOfMemory;
  InvalidPointer: EInvalidPointer;

{$IFOPT C+}
procedure AssertErrorHandler(Message, FileName: PLegacyChar;
  LineNumber: Integer; ErrorAddr: Pointer);
begin
  if Message = nil then
    Message := sAssertionFailed;
  raise EAssertionFailed.Create(sAssertError, [Message, FileName, LineNumber]);
end;
{$ENDIF}

type
  TExceptionRec = packed record
    ClassId: ExceptionClass;
    Ident: PLegacyChar;
  end;

const
  RuntimeExceptions: array[reDivByZero..reStackOverflow] of TExceptionRec = (
    (ClassId: EDivByZero; Ident: sDivByZero),
    (ClassId: ERangeError; Ident: sRangeError),
    (ClassId: EIntOverflow; Ident: sIntOverflow),
    (ClassId: EInvalidOp; Ident: sInvalidOp),
    (ClassId: EZeroDivide; Ident: sZeroDivide),
    (ClassId: EOverflow; Ident: sOverflow),
    (ClassId: EUnderflow; Ident: sUnderflow),
    (ClassId: EInvalidCast; Ident: sInvalidCast),
    (ClassId: EAccessViolation; Ident: nil),
    (ClassId: EPrivilege; Ident: sPrivilege),
    (ClassId: EControlBreak; Ident: sControlC),
    (ClassId: EStackOverflow; Ident: sStackOverflow)
  );

{$IFDEF Interfaces}
  InterfaceExceptions: array[reIntfCastError..reSafeCallError] of TExceptionRec = (
    (ClassId: EIntfNotSupported; Ident: sIntfNotSupported),
    (ClassId: ESafecall; Ident: sSafecallException)
  );
{$ENDIF}

procedure ErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case TRuntimeError(ErrorCode) of
    reOutOfMemory:
      E := OutOfMemory;
    reInvalidPtr:
      E := InvalidPointer;
    Low(RuntimeExceptions)..High(RuntimeExceptions):
      with RuntimeExceptions[TRuntimeError(ErrorCode)] do
        E := ClassId.Create(Ident);
  {$IFDEF Interfaces}
    Low(InterfaceExceptions)..High(InterfaceExceptions):
      with InterfaceExceptions[TRuntimeError(ErrorCode)] do
        E := ClassId.Create(Ident);
  {$ENDIF}
  {$IFOPT C+}
    reAssertionFailed:
      E := EAssertionFailed.Create(sAssertionFailed);
  {$ENDIF}
  else
{$IFDEF Tricks}
    Exit; // should never appear, just suppress a warning
{$ELSE}
    E := EInOutError.Create(IOResult);
{$ENDIF}
  end;
  raise E at ErrorAddr;
end;

procedure ExceptionHandler(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  // "as" typecast -- CoreLite exceptions only: build with runtime packages or die!
  ShowException(ExceptObject {$IFNDEF Debug} as Exception {$ENDIF});
  Halt(1);
end;

function MapHardError(P: PExceptionRecord): TRuntimeError;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:
      Result := reDivByZero;
    STATUS_ARRAY_BOUNDS_EXCEEDED:
      Result := reRangeError;
    STATUS_INTEGER_OVERFLOW:
      Result := reIntOverflow;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:
      Result := reInvalidOp;
    STATUS_FLOAT_DIVIDE_BY_ZERO:
      Result := reZeroDivide;
    STATUS_FLOAT_OVERFLOW:
      Result := reOverflow;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:
      Result := reUnderflow;
    STATUS_ACCESS_VIOLATION:
      Result := reAccessViolation;
    STATUS_PRIVILEGED_INSTRUCTION:
      Result := rePrivInstruction;
    STATUS_CONTROL_C_EXIT:
      Result := reControlBreak;
    STATUS_STACK_OVERFLOW:
      Result := reStackOverflow;
  else
    Result := reExternalException;
  end;
end;

function GetExceptionClass(P: PExceptionRecord): ExceptionClass;
var
  ErrorCode: TRuntimeError;
begin
  ErrorCode := MapHardError(P);
  if ErrorCode in [Low(RuntimeExceptions)..High(RuntimeExceptions)] then
    Result := RuntimeExceptions[ErrorCode].ClassId
  else
    Result := EGeneralFault;
end;

function GetExceptionObject(P: PExceptionRecord): Exception;

var
  ErrorCode: TRuntimeError;

function CreateAVObject: EHard;
var
  AccessOp: PLegacyChar;
  AccessAddress: Pointer;
  MemInfo: TMemoryBasicInformation;
  ModuleName: array[0..MAX_PATH] of WideChar;
  L: Integer;
  W: PWideChar;
begin
  with P^ do
  begin
    if ExceptionInformation[0] <> 0 then
      AccessOp := sWriteAccess
    else
      AccessOp := sReadAccess;
    AccessAddress := Pointer(ExceptionInformation[1]);
    VirtualQuery(ExceptionAddress, MemInfo, SizeOf(MemInfo));
    if MemInfo.State = MEM_COMMIT then
    begin
      L := GetModuleFileNameW(THandle(MemInfo.AllocationBase), ModuleName, Length(ModuleName));
      if L <> 0 then
      begin
        ModuleName[L] := WideChar(0);
        W := @ModuleName[L - 1];
        while W^ <> PathDelimiter do
          Dec(W);
        Inc(W);
        Result := EAccessViolation.Create(sModuleAccessViolation, CP_LEGACY,
          [ExceptionAddress, W, AccessOp, AccessAddress]);
        Exit;
      end;
    end;
    Result := EAccessViolation.Create(sAccessViolation,
      [ExceptionAddress, AccessOp, AccessAddress]);
  end;
end;

begin
  ErrorCode := MapHardError(P);
  case ErrorCode of
    reDivByZero..reInvalidCast, rePrivInstruction..reStackOverflow:
      with RuntimeExceptions[ErrorCode] do
        Result := ClassId.Create(Ident);
    reAccessViolation:
      Result := CreateAVObject;
  else
    Result := EGeneralFault.Create(sGeneralFault, [P.ExceptionCode]);
  end;
  if Result is EHard then
    EHard(Result).FInfo := P;
end;

{ Exceptions initialization and shutdown }

var
  ErrorMode: CoreWord;

procedure InitExceptions;
begin
  OutOfMemory := EOutOfMemory.Create(sOutOfMemory);
  InvalidPointer := EInvalidPointer.Create(sInvalidPointer);

  ErrorProc := ErrorHandler;
  ExceptProc := @ExceptionHandler;
  System.ExceptionClass := Exception;

  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;

{$IFNDEF Debug}
  @AbstractErrorProc := @EAbstract.MethodCall;
{$ENDIF}

{$IFOPT C+}
  AssertErrorProc := @AssertErrorHandler;
{$ENDIF}

  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
end;

procedure DoneExceptions;
begin
  SetErrorMode(ErrorMode);

{$IFOPT C+}
  AssertErrorProc := nil;
{$ENDIF}

{$IFNDEF Debug}
  AbstractErrorProc := nil;
{$ENDIF}

  ErrorProc := nil;
  ExceptProc := nil;
  System.ExceptionClass := nil;

  ExceptClsProc := nil;
  ExceptObjProc := nil;

  with OutOfMemory do
  begin
    Include(FOptions, eoCanFree);
    FreeInstance;
  end;
  OutOfMemory := nil;

  with InvalidPointer do
  begin
    Include(FOptions, eoCanFree);
    FreeInstance;
  end;
  InvalidPointer := nil;
end;

procedure ExceptionErrorMessage(Msg: PWideChar; CodePage: Word); // via ErrorMessage
asm
        PUSH ESI
        MOV ESI, EDX

        PUSH EAX
        CALL WideStrLen
        INC EAX
        POP EDX

        PUSH EBX
        PUSH EDI
        MOV EDI, ESP

        MOV ECX, EAX  // Length
        SHL ECX, 1
        ADD ECX, EAX  // 3 bytes per char for possible UTF-8
        ADD ECX, 3
        AND ECX, $FFFFFFFC
        SUB ESP, ECX
        MOV EBX, ESP

        PUSH 0
        PUSH 0
        PUSH ECX
        PUSH EBX
        PUSH EAX  // Length
        PUSH EDX  // Msg
        PUSH 0
        PUSH ESI  // CodePage
     {$IFDEF Tricks}
        CALL System.WideCharToMultiByte
     {$ELSE}
        CALL WideCharToMultiByte
     {$ENDIF}

        DEC EAX
        MOV EDX, EAX
        MOV EAX, EBX
        CALL ErrorMessage

        MOV ESP, EDI
        POP EDI
        POP EBX
        POP ESI
end;

procedure ExceptMsgBox(Msg: PWideChar; Len: Integer); // asm service
asm
        MOV ECX, EDX // MsgLen
        OR ECX, 1
        INC ECX
        SHL ECX, 1

        PUSH EDI
        MOV EDI, ESP
        SUB ESP, ECX
        MOV ECX, EDX
        SHL ECX, 1
        MOV EDX, ESP
        PUSH ECX
        CALL Move
        POP ECX
        MOV EAX, ESP
        ADD ECX, EAX
        MOV ECX.WideChar, '.'
        ADD ECX, 2
        MOV ECX.WideChar, 0
        MOV ECX, MB_ICONERROR
        CMP MainWindow, 0
        JNE @@1
        OR ECX, MB_TASKMODAL
@@1:
        PUSH ECX
        PUSH 0
        PUSH EAX
        PUSH MainWindow
        CALL MessageBoxW
        MOV ESP, EDI
        POP EDI
end;

procedure ExceptionMessageWrite(Msg: PWideChar);
asm
        PUSH EAX
        CALL GetConsoleOutputCP
        MOV EDX, EAX
        POP EAX
        JMP ExceptionErrorMessage
end;

procedure ExceptionMessageBox(Msg: PWideChar);
var
  L: Integer;
  Flags: LongWord;
  P: PWideChar;
begin
{$IFDEF Tricks}
  if not NoErrMsg then
{$ENDIF}
  begin
    L := WideStrLen(Msg);
    if L <> 0 then
    begin
      P := Msg + L - 1;
      if (P^ <> WideChar('.')) and (P^ <> WideChar('!')) and (P^ <> WideChar('?')) then
      begin
        ExceptMsgBox(Msg, L);
        Exit;
      end;
    end;
    Flags := MB_ICONERROR;
    if MainWindow = 0 then
      Flags := Flags or MB_TASKMODAL;
    MessageBoxW(MainWindow, Msg, nil, Flags);
  end;
end;

procedure DefaultExceptionMessage(Msg: PWideChar);
asm
        MOVZX EDX, IsConsole // 0 = ACP, 1 = OEMCP
        JMP ExceptionErrorMessage
end;

{ Exception raising }

procedure Abort;

function ReturnAddr: Pointer;
asm
        MOV EAX, [EBP + 4]
end;

begin
  raise EAbort.Create(sOperationAborted) at ReturnAddr;
end;

procedure RaiseLastPlatformError(ErrorSource: PCoreChar);
var
  LastError: LongWord;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    raise EPlatform.Create(LastError, ErrorSource);
end;

{ Exception handling }

procedure ShowException(E: {$IFDEF Debug} TObject {$ELSE} Exception {$ENDIF});
begin
{$IFDEF Debug}
  if (E is Exception) and (eoWideChar in Exception(E).Options) then
    ExceptionMessage(Exception(E).Message)
  else // treat as compatible or SysUtils exception, unsafe with 3rd-party exceptions
    ErrorMessage(Exception(E).DelphiMsg, PInteger(Exception(E).DelphiMsg - SizeOf(Integer))^);
{$ELSE}
  if eoWideChar in E.Options then
    ExceptionMessage(E.Message)
  else
    ErrorMessage(Pointer(E.Message), StrLen(Pointer(E.Message)));
{$ENDIF}
end;

procedure UseExceptionMessageBox;
begin
  ExceptionMessage := ExceptionMessageBox;
end;

procedure UseExceptionMessageWrite;
begin
  ExceptionMessage := ExceptionMessageWrite;
end;

{ Exception}

const
  DelphiStringBytes = SizeOf(LongInt) {$IF RTLVersion >= 20} * 3 {$IFEND};
  LegacyCharACP = CP_ACP or (SizeOf(LegacyChar) shl SizeOf(Word));

constructor Exception.Create(Msg: PLegacyChar);
{$IFDEF Debug}
var
  L: Integer;
begin
  L := StrLen(Msg);
  if L <> 0 then
  begin
    GetMem(FDelphiMsg, L + 1 + DelphiStringBytes);
  {$IF RTLVersion >= 20}
    PLongWord(FDelphiMsg)^ := LegacyCharACP;
    Inc(FDelphiMsg, SizeOf(LongInt) * 2);
  {$IFEND}
    PLongInt(FDelphiMsg)^ := L;
    Inc(FDelphiMsg, SizeOf(LongInt));
    Move(Msg^, FDelphiMsg^, L + 1);
  end;
{$ELSE}
begin
{$ENDIF}
  FMessage := Pointer(Msg);
  FOptions := [eoCanFree];
end;

constructor Exception.Create(Msg: PLegacyChar; const Args: array of const);
{$IFDEF Debug}
var
  L: Integer;
begin
  if Msg <> nil then
  begin
    GetMem(FDelphiMsg, StrLen(Msg) + EstimateArgs(Args) + 1 + DelphiStringBytes);
    L := FormatBuf(Msg, Args, FDelphiMsg + DelphiStringBytes);
    ReallocMem(FDelphiMsg, L + 1 + DelphiStringBytes);
  {$IF RTLVersion >= 20}
    PLongWord(FDelphiMsg)^ := LegacyCharACP;
    Inc(FDelphiMsg, SizeOf(LongInt) * 2);
  {$IFEND}
    PLongInt(FDelphiMsg)^ := L;
    Inc(FDelphiMsg, SizeOf(LongInt));
    FMessage := Pointer(FDelphiMsg);
    FOptions := [eoCanFree];
  end;
{$ELSE}
begin
  FMessage := Pointer(Format(Msg, Args));
  FOptions := [eoFreeMessage, eoCanFree];
{$ENDIF}
end;

function LegacyString(Msg: PWideChar; Count: Integer): PLegacyChar;
var
  L: Integer;
begin
  L := {$IFDEF Tricks} System. {$ENDIF}
    WideCharToMultiByte(CP_ACP, 0, Msg, Count, nil, 0, nil, nil);
  if L <> 0 then
  begin
    GetMem(Result, L + 1 + DelphiStringBytes);
  {$IF RTLVersion >= 20}
    PLongWord(FDelphiMsg)^ := LegacyCharACP;
    Inc(FDelphiMsg, SizeOf(LongInt) * 2);
  {$IFEND}
    PInteger(Result)^ := L;
    Inc(Result, SizeOf(Integer));
  {$IFDEF Tricks} System. {$ENDIF}
    WideCharToMultiByte(CP_ACP, 0, Msg, Count, Result, L, nil, nil);
    Result[L] := #0;
  end
  else
    Result := nil;
end;

constructor Exception.Create(Msg: PWideChar; Count: Integer);
begin
  if Msg <> nil then
  begin
    GetMem(FMessage, (Count + 1) * SizeOf(WideChar));
    Move(Msg^, FMessage^, Count * SizeOf(WideChar));
    FMessage[Count] := WideChar(0);
    FOptions := [eoWideChar, eoFreeMessage];
  {$IFDEF Debug}
    FDelphiMsg := LegacyString(Msg, Count);
  {$ENDIF}
  end;
  Include(FOptions, eoCanFree);
end;

constructor Exception.Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const);
begin
  FMessage := LegacyFormat(Msg, CodePage, 0, Args).Value;
{$IFDEF Debug}
  FDelphiMsg := LegacyString(FMessage, WideStrLen(FMessage));
{$ENDIF}
  FOptions := [eoWideChar, eoFreeMessage, eoCanFree];
end;

destructor Exception.Destroy;
begin
  if eoFreeMessage in FOptions then
    FreeMem(FMessage);
{$IFDEF Debug}
  if FDelphiMsg <> nil then
  begin
    Dec(FDelphiMsg, SizeOf(Integer));
    FreeMem(FDelphiMsg);
  end;
{$ENDIF}
end;

procedure Exception.FreeInstance;
begin
  if eoCanFree in FOptions then
    inherited FreeInstance;
end;

{$IFNDEF Debug}
procedure EAbstract.MethodCall(ClassType: TClass);
begin
  if {dirty hack!} Pointer(Self) = ClassType then
    raise EAbstractClass.Create(ClassType);
  raise EAbstractInstance.Create(Self);
end;

constructor EAbstractInstance.Create(Callee: TObject);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Callee);
  inherited Create(sAbstractInstance, [ClassName]);
  FCallee := Callee;
end;

constructor EAbstractClass.Create(Callee: TClass);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Callee);
  inherited Create(sAbstractClass, [ClassName]);
  FCallee := Callee;
end;
{$ENDIF}

{ EPlatform }

constructor EPlatform.Create(ErrorCode: LongWord; ErrorSource: PCoreChar);
var
  W: PCoreChar;
begin
{$IFDEF Debug}
  W := SysErrorMessage(ErrorCode);
  try
    if ErrorSource <> nil then
      inherited Create(sPlatformError, CP_LEGACY, [W, ErrorSource])
    else
      inherited Create(W, WideStrLen(W));
  finally
    LocalFree(THandle(W));
  end;
{$ELSE}
  if ErrorSource <> nil then
  begin
    W := SysErrorMessage(ErrorCode);
    inherited Create(sPlatformError, CP_LEGACY, [W, ErrorSource]);
  end
  else
    FMessage := SysErrorMessage(ErrorCode);
  FOptions := [eoWideChar, eoCanFree];
{$ENDIF}
  FErrorCode := ErrorCode;
  FErrorSource := ErrorSource;
end;

{$IFNDEF Debug}
destructor EPlatform.Destroy;
begin
  if FErrorSource = nil then
    LocalFree(THandle(FMessage))
  else
    inherited;
end;
{$ENDIF}

{$IFDEF ForceMMX}
constructor EMMX.Create;
begin
  inherited Create(sMMX);
end;
{$ENDIF}

initialization
  InitExceptions;

finalization
  DoneExceptions;

end.
