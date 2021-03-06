(*
    Lite Core Library (CoreLite)

    Core exceptions implementation

    Copyright (c) 2008-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * CoreLiteVCL -- to use in VCL-based projects
      * Debug -- IDE friendly and SysUtils compatible exceptions and
          abstract method call diagnostic exceptions 
      * ForceMMX -- EMMX exception
      * Interfaces -- interface and safecall exceptions
*)

unit CoreExceptions;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IFDEF Tricks}
  SafeInit,
{$ENDIF}
{$IFDEF CoreLiteVCL}
  SysUtils,
{$ENDIF}
  CoreUtils;

{ Exceptions }

type
  Exception = class;
  ExceptionClass = class of Exception;

  TExceptionOptions = set of (eoWideChar, eoFreeMessage, eoCanFree);

  TExceptionMessage = record
    case Byte of
      1: (AsString: PLegacyChar);
      2: (AsWideString: PWideChar);
      3: (Handle: THandle);
  end;

  Exception = class {$IFDEF CoreLiteVCL} (SysUtils.Exception) {$ENDIF}
  private
  {$IF defined(Debug) and not defined(CoreLiteVCL)}
    FDelphiMsg: string;
  {$IFEND}
    FMessage: TExceptionMessage;
    FOptions: TExceptionOptions;
  public
    constructor Create(Msg: PLegacyChar); overload;
    constructor Create(Msg: PLegacyChar; const Args: array of const); overload;
    constructor Create(Msg: PWideChar; Count: Integer); overload;
    constructor Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const); overload;
    destructor Destroy; override;
    procedure FreeInstance; override;

  {$IF defined(Debug) and not defined(CoreLiteVCL)}
    property DelphiMsg: string read FDelphiMsg;
  {$IFEND}
    property Message: TExceptionMessage read FMessage;
    property Options: TExceptionOptions read FOptions;
  end;

{$IF defined(Debug) or defined(CoreLiteVCL)}
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
{$IFEND}

  EAbort = class(Exception);
  EInvalidCast = class(Exception);
{$IFOPT C+}
  EAssertionFailed = class(Exception);
{$ENDIF}

  EMemory = class(Exception);

  EOutOfMemory = class(EMemory);
  EInvalidPointer = class(EMemory);

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

  EFloatingPoint = class(EHard);
  EInvalidOp = class(EFloatingPoint);
  EZeroDivide = class(EFloatingPoint);
  EOverflow = class(EFloatingPoint);
  EUnderflow = class(EFloatingPoint);

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

  TErrorSource = record
    case Byte of
      0: (Source: PCoreChar);
      1: (TextParam: PLegacyChar; IntParam: Integer);
  end;

  EPlatform = class(Exception)
  private
    FErrorCode: LongWord;
    FErrorSource: TErrorSource;
  public
    constructor Create(ErrorCode: LongWord; ErrorSource: PCoreChar {$IFNDEF Debug} = nil {$ENDIF}); overload;
    constructor Create(ErrorCode: LongWord; TextParam: PLegacyChar; IntParam: Integer); overload;
    destructor Destroy; override;

    property ErrorCode: LongWord read FErrorCode;
    property ErrorSource: TErrorSource read FErrorSource;
  end;

{$IFNDEF Tricks}
  EInOutError = EPlatform;
{$ENDIF}

{$IFDEF ForceMMX}
  EMMX = class(Exception)
  private
    // getting module name is unsafe because of use of MMX-powered procedures
  public
    constructor Create;
  end;
{$ENDIF}

{ Exception raising }

procedure Abort;
procedure RaiseLastPlatformError(ErrorSource: PCoreChar {$IFNDEF Debug} = nil {$ENDIF} ); overload;
procedure RaiseLastPlatformError(TextParam: PLegacyChar; IntParam: Integer); overload;

{ Exception handling }

procedure DefaultExceptionMessage(Msg: PWideChar; Count: Integer);
procedure ShowException(E: TObject);
procedure UseExceptionMessageBox;
procedure UseExceptionMessageWrite;

var
  ExceptionMessage: procedure(Msg: PWideChar; Length: Integer) = DefaultExceptionMessage;

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
    E := EInOutError.Create(IOResult {$IFDEF Debug}, sDelphiRuntimeIO, 0 {$ENDIF} );
{$ENDIF}
  end;
  raise E at ErrorAddr;
end;

procedure ExceptionHandler(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  ShowException(ExceptObject);
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
      with ModuleFileName(THandle(MemInfo.AllocationBase)) do
        if Length <> 0 then
        begin
          Result := EAccessViolation.Create(sModuleAccessViolation, DefaultSystemCodePage,
            [ExceptionAddress, @Value[FileNameIndex], WhitespaceOrLineBreak[IsConsole], AccessOp, AccessAddress]);
          Exit;
        end;
    Result := EAccessViolation.Create(sAccessViolation,
      [ExceptionAddress, WhitespaceOrLineBreak[IsConsole], AccessOp, AccessAddress]);
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
{$IFDEF CoreLiteVCL}
  SaveAbstractErrorProc: procedure;
{$ENDIF}

procedure InitExceptions;
begin
{$IFDEF CoreLiteVCL -- }

  @SaveAbstractErrorProc := @AbstractErrorProc;
  @AbstractErrorProc := @EAbstract.MethodCall;

{$ELSE ----------- }

  OutOfMemory := EOutOfMemory.Create(sOutOfMemory);
  Exclude(OutOfMemory.FOptions, eoCanFree);
  InvalidPointer := EInvalidPointer.Create(sInvalidPointer);
  Exclude(InvalidPointer.FOptions, eoCanFree);

  ErrorProc := ErrorHandler;
  ExceptProc := @ExceptionHandler;
  System.ExceptionClass := Exception;

  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
{$IFDEF Debug}
  @AbstractErrorProc := @EAbstract.MethodCall;
{$ENDIF}

{$IFOPT C+}
  AssertErrorProc := Pointer(@AssertErrorHandler);
{$ENDIF}

{$ENDIF ----------- }

  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
end;

procedure DoneExceptions;
begin
  SetErrorMode(ErrorMode);

{$IFDEF CoreLiteVCL -- }

  @AbstractErrorProc := @AbstractErrorProc;

{$ELSE ----------- }

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

{$ENDIF ----------- }
end;

procedure ExceptionErrorMessage(Msg: PWideChar; Count: Integer; CodePage: Word); // via ErrorMessage
asm
        PUSH EDI
        PUSH EBX

        MOV EBX, EDX  // Length
        SHL EBX, 1
        ADD EBX, EDX  // 3 bytes per char for possible UTF-8
        ADD EBX, 3    // stack align
        AND EBX, $FFFFFFFC
        SUB ESP, EBX
        MOV EDI, ESP

        PUSH 0
        PUSH 0
        PUSH EBX
        PUSH EDI
        PUSH EDX  // Length
        PUSH EAX  // Msg
        PUSH 0
        PUSH ECX  // CodePage
     {$IFDEF Tricks}
        CALL System.WideCharToMultiByte
     {$ELSE}
        CALL WideCharToMultiByte
     {$ENDIF}

        MOV EDX, EAX
        MOV EAX, EDI
        CALL ErrorMessage

        ADD ESP, EBX
        POP EBX
        POP EDI
end;

procedure ExceptMsgBox(Msg: PWideChar; Count: Integer); // asm service
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

procedure ExceptionMessageWrite(Msg: PWideChar; Count: Integer);
asm
        PUSH EAX
        PUSH EDX
        CALL GetConsoleOutputCP
        MOV ECX, EAX
        POP EDX
        POP EAX
        JMP ExceptionErrorMessage
end;

procedure ExceptionMessageBox(Msg: PWideChar; Count: Integer);
var
  Flags: LongWord;
  P: PWideChar;
begin
{$IFDEF Tricks}
  if not NoErrMsg then
{$ENDIF}
  begin
    if Count <> 0 then
    begin
      P := Msg + Count - 1;
      if (P^ <> '.') and (P^ <> '!') and (P^ <> '?') then
      begin
        ExceptMsgBox(Msg, Count);
        Exit;
      end;
    end;
    Flags := MB_ICONERROR;
    if MainWindow = 0 then
      Flags := Flags or MB_TASKMODAL;
    MessageBoxW(MainWindow, Msg, nil, Flags);
  end;
end;

procedure DefaultExceptionMessage(Msg: PWideChar; Count: Integer);
asm
        MOVZX ECX, IsConsole // 0 = ACP, 1 = OEMCP
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

procedure RaiseLastPlatformError(TextParam: PLegacyChar; IntParam: Integer);
var
  LastError: LongWord;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    raise EPlatform.Create(LastError, TextParam, IntParam);
end;

{ Exception handling }

procedure ShowException(E: TObject);
begin
  if E is Exception then
  begin
    if eoWideChar in Exception(E).FOptions then
    begin
      with Exception(E).FMessage do
        ExceptionMessage(AsWideString, WideStrLen(AsWideString));
      Exit;
    end
  end
{$IF defined(Debug) and not defined(CoreLiteVCL)}
  else
  begin
    with Exception(E) do
    {$IF UnicodeRTL}
      ExceptionMessage(Pointer(FDelphiMsg), Length(FDelphiMsg));
    {$ELSE}
      ErrorMessage(Pointer(FDelphiMsg), Length(FDelphiMsg));
    {$IFEND}
    Exit;
  end
{$ELSE}
  {$IFDEF CoreLiteVCL}
    else if E is SysUtils.Exception then
    begin
      with SysUtils.Exception(E) do
      {$IF UnicodeRTL}
        ExceptionMessage(Pointer(Message), Length(Message));
      {$ELSE}
        ErrorMessage(Pointer(Message), Length(Message));
      {$IFEND}
      Exit;
    end;
  {$ENDIF}
{$IFEND} ;
  with Exception(E).FMessage do
    ErrorMessage(AsString, CoreUtils.StrLen(AsString))
end;

procedure UseExceptionMessageBox;
begin
  ExceptionMessage := ExceptionMessageBox;
end;

procedure UseExceptionMessageWrite;
begin
  ExceptionMessage := ExceptionMessageWrite;
end;

function DelphiString(Source: PWideChar; Count: Integer): string;
begin
{$IF UnicodeRTL}
  SetString(Result, Source, Count);
{$ELSE}
  SetLength(Result, Count * MaxCharBytes(CP_ACP));
  SetLength(Result, {$IFDEF Tricks} System. {$ENDIF}
    WideCharToMultiByte(CP_ACP, 0, Source, Count, Pointer(Result), Length(Result), nil, nil));
{$IFEND}
end;

{ Exception }

constructor Exception.Create(Msg: PLegacyChar);
{$IFDEF CoreLiteVCL}
  var FDelphiMsg: string;
{$ENDIF}
begin
  FMessage.AsString := Msg;
  FOptions := [eoCanFree];
{$IF defined(Debug) or defined(CoreLiteVCL)}
  SetString(FDelphiMsg, Msg, CoreUtils.StrLen(Msg));
  {$IFDEF CoreLiteVCL} inherited Message := FDelphiMsg; {$ENDIF}
{$IFEND}
end;

constructor Exception.Create(Msg: PLegacyChar; const Args: array of const);
{$IFDEF CoreLiteVCL}
  var FDelphiMsg: string;
{$ENDIF}
begin
  with Format(Msg, 0, Args) do
  begin
    FMessage.AsString := Value;
  {$IF defined(Debug) or defined(CoreLiteVCL)}
    SetString(FDelphiMsg, Value, Length);
    {$IFDEF CoreLiteVCL} inherited Message := FDelphiMsg; {$ENDIF}
  {$IFEND}
  end;
  FOptions := [eoFreeMessage, eoCanFree];
end;

constructor Exception.Create(Msg: PWideChar; Count: Integer);
{$IFDEF CoreLiteVCL}
  var FDelphiMsg: string;
{$ENDIF}
begin
  if Msg <> nil then
  begin
    GetMem(FMessage.AsWideString, (Count + 1) * SizeOf(WideChar));
    Move(Msg^, FMessage.AsWideString^, Count * SizeOf(WideChar));
    FMessage.AsWideString[Count] := #0;
    FOptions := [eoWideChar, eoFreeMessage];
  {$IF defined(Debug) or defined(CoreLiteVCL)}
    FDelphiMsg := DelphiString(Msg, Count);
    {$IFDEF CoreLiteVCL} inherited Message := FDelphiMsg; {$ENDIF}
  {$IFEND}
  end;
  Include(FOptions, eoCanFree);
end;

constructor Exception.Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const);
{$IFDEF CoreLiteVCL}
  var FDelphiMsg: string;
{$ENDIF}
begin
  with Format(Msg, CodePage, 0, Args) do
  begin
    FMessage.AsWideString := Value;
  {$IF defined(Debug) or defined(CoreLiteVCL)}
    FDelphiMsg := DelphiString(Value, Length);
    {$IFDEF CoreLiteVCL} inherited Message := FDelphiMsg; {$ENDIF}
  {$IFEND}
  end;
  FOptions := [eoWideChar, eoFreeMessage, eoCanFree];
end;

destructor Exception.Destroy;
begin
  if eoFreeMessage in FOptions then
    FreeMem(FMessage.AsString);
{$IFDEF CoreLiteVCL}
  inherited;
{$ENDIF}
end;

procedure Exception.FreeInstance;
begin
  if eoCanFree in FOptions then
    inherited FreeInstance;
end;

{ EAbstract }

{$IF defined(Debug) or defined(CoreLiteVCL)}
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
{$IFEND}

{ EPlatform }

constructor EPlatform.Create(ErrorCode: LongWord; ErrorSource: PCoreChar);
var
{$IFDEF CoreLiteVCL}
  FDelphiMsg: string;
{$ENDIF}
  Msg: TCoreStringRec;
begin
  Msg := SysErrorMessage(ErrorCode);
  if ErrorSource <> nil then
  begin
    inherited Create(sPlatformError, DefaultSystemCodePage, [Msg.Value, ErrorSource]);
    LocalFree(Msg.Handle);
  end
  else
  begin
    FMessage.AsWideString := Msg.Value;
    FOptions := [eoWideChar, eoCanFree];
  {$IF defined(Debug) or defined(CoreLiteVCL)}
    FDelphiMsg := DelphiString(Msg.Value, Msg.Length);
    {$IFDEF CoreLiteVCL} inherited Create(FDelphiMsg); {$ENDIF}
  {$IFEND}
  end;
  FErrorCode := ErrorCode;
  FErrorSource.Source := ErrorSource;
end;

constructor EPlatform.Create(ErrorCode: LongWord; TextParam: PLegacyChar; IntParam: Integer);
const
  FmtLen = Length(sPlatformError2);
var
{$IFDEF CoreLiteVCL}
  FDelphiMsg: string;
{$ENDIF}
  Fmt: array[0..FmtLen] of LegacyChar;
  Msg: TCoreStringRec;
  P: PLegacyChar;
begin
  Fmt := sPlatformError2;
  P := Fmt;
  Inc(P, FmtLen);
  if IntParam = 0 then
    Dec(P, 3); // ' %d'
  P^ := #0;

  Msg := SysErrorMessage(ErrorCode);
  if TextParam <> nil then
  begin
    inherited Create(Fmt, DefaultSystemCodePage, [Msg.Value, TextParam, IntParam]);
    LocalFree(Msg.Handle);
  end
  else
  begin
    FMessage.AsWideString := Msg.Value;
    FOptions := [eoWideChar, eoCanFree];
  {$IF defined(Debug) or defined(CoreLiteVCL)}
    FDelphiMsg := DelphiString(Msg.Value, Msg.Length);
    {$IFDEF CoreLiteVCL} inherited Create(FDelphiMsg); {$ENDIF}
  {$IFEND}
  end;
  FErrorCode := ErrorCode;
  FErrorSource.TextParam := TextParam;
  FErrorSource.IntParam := IntParam;
end;

destructor EPlatform.Destroy;
begin
  if FErrorSource.Source = nil then
    LocalFree(FMessage.Handle)
  else
    inherited;
end;

{ EMMX }

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
