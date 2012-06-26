(*
    The Unified Environment Core Library

    Non-platform general classes

    Copyright (c) 2008-2012 The Unified Environment Laboratory

    Conditional defines:
      * Interfaces -- IInterface implementation
      * Lite -- no BeforeUpdate and AfterUpdate events of TMutableObject

    TODO:
      * Ref/Release semantics for containers
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
  {$IFNDEF Lite}
    FBeforeUpdate, FAfterUpdate: TNotifyEvent;
  {$ENDIF}
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
  {$IFNDEF Lite}
    property BeforeUpdate: TNotifyEvent read FBeforeUpdate write FBeforeUpdate;
    property AfterUpdate: TNotifyEvent read FAfterUpdate write FAfterUpdate;
  {$ENDIF}
  end;

  TSharedObject = class(TMutableObject {$IFDEF Interfaces}, IInterface {$ENDIF})
  private
    FRefCount: Integer;
  protected
  {$IFDEF Interfaces}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  {$ENDIF}
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure Lock;
    procedure Unlock; virtual;
    function TryLock: Boolean; virtual;

    function IsShared: Boolean;
    function Ref: TSharedObject;
    procedure Release(AndFree: Boolean = True);
  end;

  TSharingViolation = (svConsistentRead, svSyncUpdate, svExclusiveLock, svDestroy);

  ESharingViolation = class(Exception)
  private
    FObj: TObject;
    FOperation: TSharingViolation;
  public
    constructor Create(Obj: TObject; Op: TSharingViolation);
  // properties
    property Obj: TObject read FObj;
    property Operation: TSharingViolation read FOperation;
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

  TEnumerableItem = class(TSharedObject)
  private
  //  { placeholder }  FOwner: TEnumerable;
  public
    destructor Destroy; override;
    procedure Extract; virtual;

    procedure EndConsistentRead; override;
    function TryConsistentRead: Boolean; override;

    procedure EndUpdate; override;
    function TryUpdate: Boolean; override;

    procedure Unlock; override;
    function TryLock: Boolean; override;
  end;

  TEnumerable = class(TSharedObject) // TODO: container interfaces
  protected // but not private
    FCount: Cardinal;
  public
    procedure Clear; virtual; abstract;
    // properties
    property Count: Cardinal read FCount;
  end;

  TListItem = class(TEnumerableItem)
  private
  //  { placeholder }  FPrior, FNext: TListItem;
  public
    procedure Append(Item: TListItem);
    procedure Prepend(Item: TListItem);

    procedure Extract; override;
  end;

  TList = class(TEnumerable)
  private
  //  { placeholder }  FFirst, FLast: TListItem;
    procedure Deliver(Item: TListItem);
  public
    procedure Append(Item: TListItem);
    procedure Prepend(Item: TListItem);

    procedure Clear; override;
  end;

{ Core services }

procedure ReleaseAndNil(var Obj; AndFree: Boolean = True);

implementation

uses
  CoreConsts;

{ Core services }

procedure ReleaseAndNil(var Obj; AndFree: Boolean);
asm
        XOR ECX, ECX
        XCHG [EAX], ECX  // XCHG enforces LOCK
        MOV EAX, ECX
        JMP TSharedObject.Release
end;

{ ESharingViolation }

constructor ESharingViolation.Create(Obj: TObject; Op: TSharingViolation);
const
  Operations: array[TSharingViolation] of PLegacyChar =
    (sConsistentRead, sSyncUpdate, sExclusiveLock, sDestroy);
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
{$IFNDEF Lite}
  if Assigned(FBeforeUpdate) then
    if FObjectState = osLive then
      FBeforeUpdate(Self)
    else
      FBeforeUpdate(nil);
{$ENDIF}
end;

procedure TMutableObject.DoAfterUpdate;
begin
{$IFNDEF Lite}
  if Assigned(FAfterUpdate) then
    if FObjectState = osLive then
      FAfterUpdate(Self)
    else
      FAfterUpdate(nil);
{$ENDIF}
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
    raise ESharingViolation.Create(Self, svConsistentRead);
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
    raise ESharingViolation.Create(Self, svSyncUpdate);
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

destructor TSharedObject.Destroy;
begin
  Unlock;
  inherited;
end;

procedure TSharedObject.BeforeDestruction;
begin
  Lock;
  inherited;
end;

function TSharedObject.TryLock: Boolean;
asm
        MOV EDX, -2
   LOCK XADD [EAX].FRefCount, EDX
        JNS @@rollback
        CALL TryUpdate
        TEST EAX, EAX
        JNZ @@exit
@@rollback:
        MOV EDX, 2
   LOCK XADD [EAX].FRefCount, EDX
        XOR EAX, EAX
@@exit:
end;

procedure TSharedObject.Lock;
begin
  if not TryLock then
    raise ESharingViolation.Create(Self,
      TSharingViolation(Byte(svExclusiveLock) + Byte(FObjectState <> osLive)));
end;

procedure TSharedObject.Unlock;
asm
        MOV EDX, 2
   LOCK XADD [EAX].FRefCount, EDX
        JNS EndUpdate
end;

function TSharedObject.IsShared: Boolean;
asm
        XOR EDX, EDX
   LOCK CMP [EAX].FRefCount, EDX
        SETNZ DL
        MOV EAX, EDX
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

type
  TListCast = class;

  TListItemCast = class(TListItem)
    FOwner: TListCast;
    FPrior, FNext: TListItemCast;
  end;

  TListCast = class(TList)
    FFirst, FLast: TListItemCast;
  end;

{ TEnumerableItem }

destructor TEnumerableItem.Destroy;
begin
  Extract;
  inherited;
end;

procedure TEnumerableItem.Extract;
begin
  BeginUpdate;
  try
    if TListItemCast(Self).FOwner <> nil then
      Dec(TListItemCast(Self).FOwner.FCount);
    ReleaseAndNil(TListItemCast(Self).FOwner);
  finally
    EndUpdate;
  end;
end;

procedure TEnumerableItem.EndConsistentRead;
begin
  inherited;
  if TListItemCast(Self).FOwner <> nil then
    TListItemCast(Self).FOwner.EndConsistentRead;
end;

function TEnumerableItem.TryConsistentRead: Boolean;
begin
  Result := ((TListItemCast(Self).FOwner = nil) or TListItemCast(Self).FOwner.TryConsistentRead) and
    inherited TryConsistentRead;
end;

procedure TEnumerableItem.EndUpdate;
begin
  inherited;
  if TListItemCast(Self).FOwner <> nil then
    TListItemCast(Self).FOwner.EndUpdate;
end;

function TEnumerableItem.TryUpdate: Boolean;
begin
  Result := ((TListItemCast(Self).FOwner = nil) or TListItemCast(Self).FOwner.TryUpdate) and
    inherited TryUpdate;
end;

procedure TEnumerableItem.Unlock;
begin
  inherited;
  if TListItemCast(Self).FOwner <> nil then
    TListItemCast(Self).FOwner.Unlock;
end;

function TEnumerableItem.TryLock: Boolean;
begin
  Result := ((TListItemCast(Self).FOwner = nil) or TListItemCast(Self).FOwner.TryLock) and
    inherited TryLock;
end;

{ TListItem }

procedure TListItem.Append(Item: TListItem);
begin
  BeginUpdate;
  try
    if Item <> nil then
    begin
      Item.BeginUpdate;
      try
        Item.Extract;
        TListItemCast(Item).FOwner := TListCast(TListItemCast(Self).FOwner.Ref);
        TListItemCast(Item).FPrior := TListItemCast(Self.Ref);
        TListItemCast(Item).FNext :=  TListItemCast(TListItemCast(Self).FNext.Ref);
        TListItemCast(Self).FNext := TListItemCast(Item.Ref);
        if TListItemCast(Self).FOwner <> nil then
          with TListItemCast(Self).FOwner do
          begin
            Inc(FCount);
            if FLast = Self then
              FLast := TListItemCast(Item);
          end;
      finally
        Item.EndUpdate;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TListItem.Prepend(Item: TListItem);
begin
  BeginUpdate;
  try
    if Item <> nil then
    begin
      Item.BeginUpdate;
      try
        Item.Extract;
        TListItemCast(Item).FOwner := TListCast(TListItemCast(Self).FOwner.Ref);
        TListItemCast(Item).FNext := TListItemCast(Self.Ref);
        TListItemCast(Item).FPrior :=  TListItemCast(TListItemCast(Self).FPrior.Ref);
        TListItemCast(Self).FPrior := TListItemCast(Item.Ref);
        if TListItemCast(Self).FOwner <> nil then
          with TListItemCast(Self).FOwner do
          begin
            Inc(FCount);
            if FFirst = Self then
              FFirst := TListItemCast(Item);
          end;
      finally
        Item.EndUpdate;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TListItem.Extract;
begin
  BeginUpdate;
  try
    if TListItemCast(Self).FOwner <> nil then
      with TListItemCast(Self).FOwner do
      begin
        if FFirst = Self then
          FFirst := TListItemCast(Self).FNext;
        if FLast = Self then
          FLast := TListItemCast(Self).FPrior;
      end;
    if TListItemCast(Self).FPrior <> nil then
    begin
      TListItemCast(Self).FPrior.FNext.Release;
      TListItemCast(Self).FPrior.FNext := TListItemCast(Self).FNext;
    end;
    if TListItemCast(Self).FNext <> nil then
    begin
      TListItemCast(Self).FNext.FPrior.Release;
      TListItemCast(Self).FNext.FPrior := TListItemCast(Self).FPrior;
    end;
    inherited;
  finally
    EndUpdate;
  end;
end;

{ TList }

procedure TList.Deliver(Item: TListItem);
begin
  if Item <> nil then
  begin
    BeginUpdate;
    try
      Item.BeginUpdate;
      try
        Item.Extract;
        TListItemCast(Item).FOwner := TListCast(Self.Ref);
        TListCast(Self).FFirst := TListItemCast(Item);
        TListCast(Self).FLast := TListItemCast(Item);
      finally
        Item.EndUpdate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TList.Append(Item: TListItem);
begin
  if TListCast(Self).FLast <> nil then
    TListCast(Self).FLast.Append(Item)
  else
    Deliver(Item);
end;

procedure TList.Prepend(Item: TListItem);
begin
  if TListCast(Self).FFirst <> nil then
    TListCast(Self).FFirst.Prepend(Item)
  else
    Deliver(Item);
end;

procedure TList.Clear;
begin
  BeginUpdate;
  try
    while TListCast(Self).FFirst <> nil do
      TListCast(Self).FFirst.Release;
  finally
    EndUpdate;
  end;
end;

end.
