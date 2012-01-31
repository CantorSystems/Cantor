(*
    The Unified Environment Core Library

    Core exceptions implementation

    Copyright (c) 2008-2012 The Unified Environment Laboratory
*)

unit Exceptions;

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
    FMessage: PLegacyChar;
    FOptions: TExceptionOptions;
  public
    constructor Create(Msg: PLegacyChar); overload;
    constructor Create(Msg: PLegacyChar; const Args: array of const); overload;
    constructor Create(Msg: PWideChar; Count: Cardinal); overload;
    constructor Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const); overload;
    //constructor Create(Msg: PWideChar; const Args: array of const); overload;
    destructor Destroy; override;
    procedure FreeInstance; override;
  // properties
    property Message: PLegacyChar read FMessage;
    property Options: TExceptionOptions read FOptions;
  end;

  EAbort = class(Exception);
  EAbstract = class(Exception);
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
    ExceptionInformation: array [0..14] of Cardinal;
  end;

  EHard = class(Exception)
  private
    FInfo: PExceptionRecord;
  public
  // properties
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

  EPlatform = class(Exception)
  private
    FErrorCode: LongWord;
  public
    constructor Create(ErrorCode: LongWord);
    destructor Destroy; override;
  // properties
    property ErrorCode: LongWord read FErrorCode;
  end;

{$IFNDEF Tricks}
  EInOutError = EPlatform;
{$ENDIF}

  {ESharingViolation = class(EContainer)
  private
    FObj: TObject;
  public
    constructor Create(Obj: TObject);
  // properties
    property Obj: TObject read FObj;
  end;}

  EString = class(Exception);

{ Exception raising }

procedure Abort;
procedure RaiseLastPlatformError;

{ Exception handling }

procedure ShowException(E: Exception);
procedure UseExceptionMessageBox;
procedure UseExceptionMessageWrite;

implementation

uses
  Windows, CoreConsts;

{ Error handlers }

var
  OutOfMemory: EOutOfMemory;
  InvalidPointer: EInvalidPointer;

procedure AbstractErrorHandler;
begin
  raise EAbstract.Create(sAbstractError);
end;

{$IFOPT C+}
procedure AssertErrorHandler(Message, FileName: PLegacyChar;
  LineNumber: Cardinal; ErrorAddr: Pointer);
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
  ExceptionMap: array[reDivByZero..reStackOverflow] of TExceptionRec = (
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

procedure ErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer);
var
  E: Exception;
begin
  case TRuntimeError(ErrorCode) of
    reOutOfMemory:
      E := OutOfMemory;
    reInvalidPtr:
      E := InvalidPointer;
    reDivByZero..reStackOverflow:
      with ExceptionMap[TRuntimeError(ErrorCode)] do
        E := ClassId.Create(Ident);
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
  ShowException(ExceptObject as Exception); // build with runtime packages or die!
  Halt(1);
end;

function MapException(P: PExceptionRecord): TRuntimeError;
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
  ErrorCode := MapException(P);
  if ErrorCode in [Low(ExceptionMap)..High(ExceptionMap)] then
    Result := ExceptionMap[ErrorCode].ClassId
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
  L: Cardinal;
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
        Result := EAccessViolation.Create(sModuleAccessViolation,
          [ExceptionAddress, ModuleName, AccessOp, AccessAddress]); // TODO: ExtractFileName
        Exit;
      end;
    end;
    Result := EAccessViolation.Create(sAccessViolation,
      [ExceptionAddress, AccessOp, AccessAddress]);
  end;
end;

begin
  ErrorCode := MapException(P);
  case ErrorCode of
    reDivByZero..reInvalidCast, rePrivInstruction..reStackOverflow:
      with ExceptionMap[ErrorCode] do
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
  ErrorMode: Cardinal;

procedure InitExceptions;
begin
  OutOfMemory := EOutOfMemory.Create(sOutOfMemory);
  InvalidPointer := EInvalidPointer.Create(sInvalidPointer);

  ErrorProc := ErrorHandler;
  ExceptProc := @ExceptionHandler;
  System.ExceptionClass := Exception;

  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;

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

        MOV EDX, EAX
        MOV EAX, EBX
        CALL ErrorMessage

        MOV ESP, EDI
        POP EDI
        POP EBX
        POP ESI
end;

procedure ExceptMsgBox(Msg: PWideChar; Len: Cardinal); // asm service
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
  L, Flags: Cardinal;
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

var
  ExceptionMessageProc: procedure(Msg: PWideChar) = DefaultExceptionMessage;

{ Exception raising }

procedure Abort;

function ReturnAddr: Pointer;
asm
        MOV     EAX,[EBP + 4]
end;

begin
  raise EAbort.Create(sOperationAborted) at ReturnAddr;
end;

procedure RaiseLastPlatformError;
var
  LastError: LongWord;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    raise EPlatform.Create(LastError);
end;

{ Exception handling }

procedure ShowException(E: Exception);
begin
  if eoWideChar in E.Options then
    ExceptionMessageProc(PWideChar(E.Message))
  else
    ErrorMessage(E.Message, StrLen(E.Message));
end;

procedure UseExceptionMessageBox;
begin
  ExceptionMessageProc := ExceptionMessageBox;
end;

procedure UseExceptionMessageWrite;
begin
  ExceptionMessageProc := ExceptionMessageWrite;
end;

{ Exception}

constructor Exception.Create(Msg: PLegacyChar);
begin
  FMessage := Msg;
  FOptions := [eoCanFree];
end;

constructor Exception.Create(Msg: PLegacyChar; const Args: array of const);
begin
  FMessage := Format(Msg, Args);
  FOptions := [eoFreeMessage, eoCanFree];
end;

constructor Exception.Create(Msg: PWideChar; Count: Cardinal);
var
  Bytes: Cardinal;
begin
  Bytes := Count * SizeOf(WideChar);
  GetMem(FMessage, Bytes + SizeOf(WideChar));
  Move(Msg^, FMessage^, Bytes);
  PWideChar(FMessage)[Count] := WideChar(0);
  FOptions := [eoWideChar, eoFreeMessage, eoCanFree];
end;

constructor Exception.Create(Msg: PLegacyChar; CodePage: Word; const Args: array of const);
begin
  FMessage := Pointer(LegacyFormat(Msg, CodePage, Args));
  FOptions := [eoWideChar, eoFreeMessage, eoCanFree];
end;

destructor Exception.Destroy;
begin
  if eoFreeMessage in FOptions then
    FreeMem(FMessage);
end;

procedure Exception.FreeInstance;
begin
  if eoCanFree in FOptions then
    inherited FreeInstance;
end;

{ EPlatform }

constructor EPlatform.Create(ErrorCode: LongWord);
begin
  FMessage := Pointer(SysErrorMessage(ErrorCode));
  FOptions := [eoWideChar, eoCanFree];
  FErrorCode := ErrorCode;
end;

destructor EPlatform.Destroy;
begin
  LocalFree(Cardinal(FMessage)); 
end;

{ ESharingViolation }

{constructor ESharingViolation.Create(Obj: TObject);
var
  ClassName: array[0..256] of LegacyChar;
  P: PShortString;
  L, Idx: Cardinal;
begin
  P := PPointer(PPLegacyChar(Obj)^ + vmtClassName)^;
  L := Length(P^);
  Idx := Cardinal((L > 1) and (P^[1] in ['T', 't']));
  L := Length(P^) - Idx;
  Move(ClassName, P^[Idx + 1], L);
  ClassName[L] := #0;
  inherited Create(sSharingViolation, [ClassName]);
  FObj := Obj;
end;}

initialization
  InitExceptions;

finalization
  DoneExceptions;

end.