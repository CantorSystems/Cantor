(*
    The Unified Environment Core Library

    Non-platform general classes

    Copyright (c) 2008-2012 The Unified Environment Laboratory

    Conditional defines:
      * Interfaces -- IInterface implementation
*)

unit CoreClasses;

interface

uses
  Exceptions, CoreUtils;

type
  TMutableObject = class;
  TNotifyEvent = procedure(Sender: TMutableObject) of object;

  TObjectState = (osConstruction, osLive, osDestruction);

  TMutableObject = class
  private
    FObjectState: TObjectState;
    FUpdateCount: Integer;
    FBeforeUpdate, FAfterUpdate: TNotifyEvent;
  protected
    procedure DoBeforeUpdate; virtual;
    procedure DoAfterUpdate; virtual;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure BeginConsistentRead;
    procedure EndConsistentRead; virtual;
    function TryConsistentRead: Boolean; virtual;

    procedure BeginUpdate;
    procedure EndUpdate; virtual;
    function TryUpdate: Boolean; virtual;

  // properties
    property ObjectState: TObjectState read FObjectState;
  // events
    property BeforeUpdate: TNotifyEvent read FBeforeUpdate write FBeforeUpdate;
    property AfterUpdate: TNotifyEvent read FAfterUpdate write FAfterUpdate;
  end;

  TSharedObject = class(TMutableObject, IInterface)
  private
    FRefCount: Integer;
  protected
    procedure DoBeforeUpdate; override;
    procedure DoAfterUpdate; override;
  {$IFDEF Interfaces}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  {$ENDIF}
  public
    procedure Lock;
    procedure Unlock; virtual;
    function TryLock: Boolean; virtual;

    function Ref: TSharedObject;
    procedure Release(AndFree: Boolean = True);
  end;

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

{ Core services }

procedure ReleaseAndNil(var Obj);

implementation

uses
  CoreConsts;

{ Core services }

procedure ReleaseAndNil(var Obj);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TSharedObject.Release
end;

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

{ TMutableObject }

destructor TMutableObject.Destroy;
begin
  EndUpdate;
end;

procedure TMutableObject.AfterConstruction;
begin
  Inc(FObjectState);
end;

procedure TMutableObject.BeforeDestruction;
begin
  Inc(FObjectState);
  BeginUpdate;
end;

procedure TMutableObject.DoBeforeUpdate;
begin
  if Assigned(FBeforeUpdate) then
    if FObjectState = osLive then
      FBeforeUpdate(Self)
    else
      FBeforeUpdate(nil);
end;

procedure TMutableObject.DoAfterUpdate;
begin
  if Assigned(FAfterUpdate) then
    if FObjectState = osLive then
      FAfterUpdate(Self)
    else
      FAfterUpdate(nil);
end;

function TMutableObject.TryConsistentRead: Boolean;
asm
        MOV ECX, EAX
        XOR EAX, EAX
        MOV EDX, EAX
        DEC EDX
   LOCK XADD [ECX].FUpdateCount, EDX
        JNS @@rollback
        INC EAX
        RET
@@rollback:
        MOV EDX, EAX
        INC EDX
   LOCK XADD [ECX].FUpdateCount, EDX
@@exit:
end;

procedure TMutableObject.BeginConsistentRead;
begin
  if not TryConsistentRead then
    raise ESharingViolation.Create(Self, opConsistentRead);
end;

procedure TMutableObject.EndConsistentRead;
asm
        XOR EDX, EDX
        INC EDX
   LOCK XADD [EAX].FUpdateCount, EDX
end;

function TMutableObject.TryUpdate: Boolean;
asm
        MOV ECX, EAX
        XOR EAX, EAX
        MOV EDX, EAX
        INC EDX
   LOCK XADD [ECX].FUpdateCount, EDX
        JG @@event
        MOV EDX, EAX
        DEC EDX
   LOCK XADD [ECX].FUpdateCount, EDX
        RET
@@event:
        MOV EAX, ECX
        CALL DoBeforeUpdate
        XOR EAX, EAX
        INC EAX
end;

procedure TMutableObject.BeginUpdate;
begin
  if not TryUpdate then
    raise ESharingViolation.Create(Self, opSyncUpdate);
end;

procedure TMutableObject.EndUpdate;
asm
        XOR EDX, EDX
        DEC EDX
   LOCK XADD [EAX].FUpdateCount, EDX
        JNZ @@exit
        CALL DoAfterUpdate
@@exit:
end;

{ TSharedObject }

procedure TSharedObject.DoBeforeUpdate;
begin
  if FObjectState = osDestruction then
    Lock;
  inherited;
end;

procedure TSharedObject.DoAfterUpdate;
begin
  inherited;
  if FObjectState = osDestruction then
    Unlock;
end;

function TSharedObject.TryLock: Boolean;
asm
        MOV EDX, -2
   LOCK XADD [EAX].FRefCount, EDX
        JS TryUpdate
        MOV EDX, 2
   LOCK XADD [EAX].FRefCount, EDX
        XOR EAX, EAX
end;

procedure TSharedObject.Lock;
begin
  if not TryLock then
    raise ESharingViolation.Create(Self, opExclusiveLock);
end;

procedure TSharedObject.Unlock;
asm
        MOV EDX, 2
   LOCK XADD [EAX].FRefCount, EDX
        JNS EndUpdate
end;

function TSharedObject.Ref: TSharedObject;
asm
        TEST EAX, EAX
        JZ @@exit
        XOR EDX, EDX
        INC EDX
   LOCK XADD [EAX].FRefCount, EDX
@@exit:
end;

procedure TSharedObject.Release(AndFree: Boolean);
asm
        TEST EAX, EAX
        JZ @@exit
        XOR ECX, ECX
        DEC ECX
   LOCK XADD [EAX].FRefCount, ECX
        JNZ @@exit
        TEST EDX, EDX
        JNZ Free
@@exit:
end;

{$IFDEF Interfaces}
function TSharedObject._AddRef: Integer;
asm
        MOV EDX, [EBP]
        JZ @@exit
        XOR EAX, EAX
        INC EAX
   LOCK XADD [EDX].FRefCount, EAX
        INC EAX
@@exit:
end;

function TSharedObject._Release: Integer;
asm
        MOV EDX, [EBP]
        XOR EAX, EAX
        DEC EAX
   LOCK XADD [EDX].FRefCount, EAX
        DEC EAX
        JNZ @@exit
        PUSH EAX
        MOV EAX, EDX
        CALL Free
        POP EAX
@@exit:
end;

function TSharedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;
{$ENDIF}

end.
