(*
    The Unified Environment Core Library

    Non-platform general classes

    Copyright (c) 2008-2012 The Unified Environment Laboratory
*)

unit CoreClasses;

interface

uses
  Exceptions, CoreUtils;

type
  TMutableObject = class;
  TNotifyEvent = procedure(Sender: TMutableObject) of object;

  TMutableObject = class
  end;

//  TObjectState = (osCreating, osLive, osDestroying);

{  TSharedObject = class(TObject, IInterface)
  private
    FRefCount, FUpdateCount: Integer;
//    FObjectState: TObjectState;
    FOnChanging, FOnChanged: TNotifyEvent;
  protected
    procedure Changing; virtual;
    procedure Changed; virtual;
  // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function BeginUpdate: Integer;
    function EndUpdate: Integer;
    function Lock: Boolean;
    function Ref: Integer;
    function Release: Integer;
    function Unlock: Boolean;
  // events
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;}

  TSharedOperation = (opConsistentRead, opSyncUpdate, opExclusiveLock);

  ESharingViolation = class(Exception)
  private
    FObj: TObject;
    FOperation: TSharedOperation;
  public
    constructor Create(Obj: TObject; Op: TSharedOperation);
  // properties
    property Obj: TObject read FObj;
    property Operation: TSharedOperation read FOperation;
  end;

  EIndex = class(Exception)
  private
    FObj: TObject;
    FIndex: Integer;
  public
    constructor Create(Obj: TObject; Idx, Lo, Hi: Integer); 
  // properties
    property Obj: TObject read FObj;
    property Index: Integer read FIndex;
  end;

implementation

uses
  CoreConsts;

{ ESharingViolation }

constructor ESharingViolation.Create(Obj: TObject; Op: TSharedOperation);
const
  Operations: array[TSharedOperation] of PLegacyChar =
    (sConsistentRead, sSyncUpdate, sExclusiveLock);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Obj);
  inherited Create(sSharingViolation, [Operations[Op], @ClassName]);
  FObj := Obj;
end;

{ EIndex }

constructor EIndex.Create(Obj: TObject; Idx, Lo, Hi: Integer);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Obj);
  inherited Create(sIndexOutOfBounds, [Lo, Hi, @ClassName, Idx]);
  FObj := Obj;
  FIndex := Idx;
end;

{ TSharedObject }

{destructor TSharedObject.Destroy;
asm
   LOCK DEC [EAX].FUpdateCount
        JNZ @@error
        PUSH EAX
        CALL Updated
        POP EAX
        CMP [EAX].FRefCount, 0
        JNE @@error
        RET
@@error:
        MOV EAX, reInvalidPtr
        JMP System.Error
end;

procedure TSharedObject.AfterConstruction;
asm
//   LOCK INC [EAX].FRefCount
   LOCK INC [EAX].FObjectState
        JMP EndUpdate
end;

procedure TSharedObject.BeforeDestruction;
asm
   LOCK INC [EAX].FObjectState
//   LOCK INC [EAX].FUpdateCount
end;

function TSharedObject.BeginUpdate: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, 1
   LOCK XADD [EAX].FUpdateCount, EDX
        MOV EAX, EDX
@@exit:
end;

function TSharedObject.EndUpdate: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, EAX
        MOV EAX, -1
   LOCK XADD [EDX].FUpdateCount, EAX
        DEC EAX
        JNZ @@exit
        MOV EAX, EDX
        CALL Updated
        XOR EAX, EAX
@@exit:
end;

procedure TSharedObject.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TSharedObject.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TSharedObject.Lock: Boolean;
asm
        TEST EAX, EAX
        JZ @@exit
        CALL BeginUpdate
        TEST EAX, EAX
        SETNZ AL
@@exit:
end;

class function TSharedObject.NewInstance: TObject;
asm
        CALL TObject.NewInstance
        INC [EAX].FUpdateCount
        DEC [EAX].FRefCount
end;

function TSharedObject.Ref: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, 1
   LOCK XADD [EAX].FRefCount, EDX
        MOV EAX, EDX
@@exit:
end;

function TSharedObject.Release: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, EAX
        MOV EAX, -1
   LOCK XADD [EDX].FRefCount, EAX
        DEC EAX
        MOV ECX, EAX
        OR ECX, [EDX].FUpdateCount
        JNZ @@exit
        MOV EAX, EDX
        CALL Destroy
        XOR EAX, EAX
@@exit:
end;

function TSharedObject.Unlock: Boolean;
asm
        TEST EAX, EAX
        JZ @@exit
        CALL EndUpdate
        TEST EAX, EAX
        SETNZ AL
@@exit:
end;}

{ IInterface }

{function TSharedObject._AddRef: Integer;
begin
  Result := 0; // TODO
end;

function TSharedObject._Release: Integer;
begin
  Result := 0; // TODO
end;

function TSharedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE; // TODO
end;}

end.
