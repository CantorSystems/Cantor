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

  TObjectState = (osConstruction, osLife, osDestruction);

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

    procedure BeginRead;
    procedure EndRead; virtual;
    function TryRead: Boolean; virtual;

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

{$IFDEF Interfaces}
  TCoreObject = TSharedObject;
{$ELSE}
  TCoreObject = TMutableObject;
{$ENDIF}

  TContainedItem = class(TCoreObject)
  private
  //  { placeholder }  FOwner: TContainer;
  protected
    procedure DoExtract; virtual;
  public
    destructor Destroy; override;
    procedure Extract;

    procedure EndRead; override;
    function TryRead: Boolean; override;

    procedure EndUpdate; override;
    function TryUpdate: Boolean; override;

  {$IFDEF Interfaces}
    procedure Unlock; override;
    function TryLock: Boolean; override;
  {$ENDIF}
  end;

  TContainer = class(TSharedObject) // inheritance from TSharedObject needed for TString
  protected
    procedure DoClear; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Clear;
  end;

  TEnumerableItem = class(TContainedItem)
  protected
    procedure DoExtract; override;
  end;

  TEnumerable = class(TContainer)
  protected
    FCount: Cardinal; // not private for CoreStrings and other
  protected
  //  DoClear override not needed, Count will be changed (also automatically) in the descendants
  public
  // properties
    property Count: Cardinal read FCount;
  end;

  TIndexed = class(TEnumerable)
  protected
    procedure CheckIndex(Index: Cardinal);
  end;

  TListItem = class(TContainedItem)
  private
  //  { placeholder }  FPrior, FNext: TListItem;
  protected
    procedure DoExtract; override;
  public
    procedure Append(Item: TListItem);
    procedure Prepend(Item: TListItem);
  end;

  TList = class(TEnumerable)
  private
  //  { placeholder }  FFirst, FLast: TListItem;
    procedure Grab(Item: TListItem);
  protected
    procedure DoClear; override;
  public
    procedure Append(Item: TListItem);
    procedure Prepend(Item: TListItem);
  end;

  TObjects = class(TIndexed)
  private
    FCapacity, FDelta: Cardinal;
  //  { placeholder }  FItems: PObjectArray;
    function DoExtract(Index: Cardinal): TObject;
    procedure SetCapacity(Value: Cardinal);
    procedure SetCount(Value: Cardinal);
  protected
    procedure DoClear; override;
    procedure DoSetCapacity(Value: Cardinal); virtual;
  public
    constructor Create(Capacity, Delta: Cardinal);
    function Append(Item: TObject): Cardinal;
    procedure Exchange(Index1, Index2: Cardinal);
    function Extract(Index: Cardinal): TObject;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Cardinal; Item: TObject);
  // properites
    property Capacity: Cardinal read FCapacity write SetCapacity;
    property Count write SetCount;
    property Delta: Cardinal read FDelta write FDelta;
  end;

  TInnerObjects = class(TObjects)
  protected
    procedure DoSetCapacity(Value: Cardinal); override;
  end;

  TCollectionItem = class(TEnumerableItem)
  protected
    procedure DoExtract; override;
  end;

  TCollection = class(TObjects)
  end;

{ Exceptions }

  TSharingViolation = (svConsistentRead, svSyncUpdate, svExclusiveLock, svDestroy);

  ESharingViolation = class(Exception)
  private
    FObj: TMutableObject;
    FOperation: TSharingViolation;
  public
    constructor Create(Obj: TMutableObject; Op: TSharingViolation);
  // properties
    property Obj: TMutableObject read FObj;
    property Operation: TSharingViolation read FOperation;
  end;

  EContainer = class(Exception)
  private
    FContainer: TObject;
  public
  // properties
    property Container: TObject read FContainer;
  end;

  EIndex = class(EContainer)
  private
    FRangeMin, FRangeMax, FIndex: Cardinal;
  public
    constructor Create(Container: TObject; Index, RangeMin, RangeMax: Cardinal); overload; // because of ERange.Create
  // properties
    property Index: Cardinal read FIndex;
    property RangeMax: Cardinal read FRangeMax;
    property RangeMin: Cardinal read FRangeMin;
  end;

  ERange = class(EIndex)
  private
    FCount: Cardinal;
  public
    constructor Create(Container: TObject; Index, Count, RangeMin, RangeMax: Cardinal);
  // properties
    property Count: Cardinal read FCount;
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

constructor ESharingViolation.Create(Obj: TMutableObject; Op: TSharingViolation);
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

constructor EIndex.Create(Container: TObject; Index, RangeMin, RangeMax: Cardinal);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sIndexOutOfBounds, [RangeMin, RangeMax, @ClassName, Index]);
  FContainer := Container;
  FRangeMin := RangeMin;
  FRangeMax := RangeMax;
  FIndex := Index;
end;

{ ERange }

constructor ERange.Create(Container: TObject; Index, Count, RangeMin, RangeMax: Cardinal);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sRangeOutOfBounds, [RangeMin, RangeMax, @ClassName, Index, Count]);
  FContainer := Container;
  FRangeMin := RangeMin;
  FRangeMax := RangeMax;
  FIndex := Index;
  FCount := Count;
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
    if FObjectState = osLife then
      FBeforeUpdate(Self)
    else
      FBeforeUpdate(nil);
{$ENDIF}
end;

procedure TMutableObject.DoAfterUpdate;
begin
{$IFNDEF Lite}
  if Assigned(FAfterUpdate) then
    if FObjectState = osLife then
      FAfterUpdate(Self)
    else
      FAfterUpdate(nil);
{$ENDIF}
end;

function TMutableObject.TryRead: Boolean;
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
end;

procedure TMutableObject.BeginRead;
begin
  if not TryRead then
    raise ESharingViolation.Create(Self, svConsistentRead);
end;

procedure TMutableObject.EndRead;
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
      TSharingViolation(Byte(svExclusiveLock) + Byte(FObjectState <> osLife)));
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
    Owner: TListCast;
    Prior, Next: TListItemCast;
  end;

  TListCast = class(TList)
    First, Last: TListItemCast;
  end;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..MaxInt div SizeOf(TObject) - 1] of TObject;

  TObjectsCast = class(TObjects)
    Items: PObjectArray;
  end;

  TCollectionCast = class;

  TCollectionItemCast = class(TCollectionItem)
    Owner: TCollectionCast;
  end;

  PCollectionItems = ^TCollectionItems;
  TCollectionItems = array[0..MaxInt div SizeOf(TCollectionItem) - 1] of TCollectionItemCast;

  TCollectionCast = class(TCollection)
    Items: PCollectionItems;
  end;

{ TContainedItem }

destructor TContainedItem.Destroy;
begin
  DoExtract;
  inherited;
end;

procedure TContainedItem.DoExtract;
begin
  ReleaseAndNil(TListItemCast(Self).Owner, False); // upper link
end;

procedure TContainedItem.Extract;
begin
  BeginUpdate;
  try
    DoExtract;
  finally
    EndUpdate;
  end;
end;

procedure TContainedItem.EndRead;
begin
  inherited;
  if TListItemCast(Self).Owner <> nil then
    TListItemCast(Self).Owner.EndRead;
end;

function TContainedItem.TryRead: Boolean;
begin
  Result := ((TListItemCast(Self).Owner = nil) or TListItemCast(Self).Owner.TryRead) and
    inherited TryRead;
end;

procedure TContainedItem.EndUpdate;
begin
  inherited;
  if TListItemCast(Self).Owner <> nil then
    TListItemCast(Self).Owner.EndUpdate;
end;

function TContainedItem.TryUpdate: Boolean;
begin
  Result := ((TListItemCast(Self).Owner = nil) or TListItemCast(Self).Owner.TryUpdate) and
    inherited TryUpdate;
end;

{$IFDEF Interfaces}
procedure TContainedItem.Unlock;
begin
  inherited;
  if TListItemCast(Self).Owner <> nil then
    TListItemCast(Self).Owner.Unlock;
end;

function TContainedItem.TryLock: Boolean;
begin
  Result := ((TListItemCast(Self).Owner = nil) or TListItemCast(Self).Owner.TryLock) and
    inherited TryLock;
end;
{$ENDIF}

{ TContainer }

destructor TContainer.Destroy;
begin
  DoClear;
  inherited;
end;

procedure TContainer.Clear;
begin
  BeginUpdate;
  try
    DoClear;
  finally
    EndUpdate;
  end;
end;

{ TEnumerableItem }

procedure TEnumerableItem.DoExtract;
begin
  if TListItemCast(Self).Owner <> nil then
    Dec(TListItemCast(Self).Owner.FCount);
  inherited;
end;

{ TIndexed }

procedure TIndexed.CheckIndex(Index: Cardinal);
begin
  if Index > FCount then
    raise EIndex.Create(Self, Index, 0, FCount);
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
        Item.DoExtract;
        with TListItemCast(Item) do
        begin
          Owner := TListItemCast(Self).Owner;
          Prior := TListItemCast(Self);
          Next := TListItemCast(Self).Next;
        end;

        with TListItemCast(Self) do
        begin
          Next := TListItemCast(Item);
          if Owner <> nil then
            with Owner do
            begin
              Inc(FCount);
              if Last = Self then
                Last := TListItemCast(Item);
            end;
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
        Item.DoExtract;
        with TListItemCast(Item) do
        begin
          Owner := TListItemCast(Self).Owner;
          Prior := TListItemCast(Self).Prior;
          Next := TListItemCast(Self);
        end;

        with TListItemCast(Self) do
        begin
          Prior := TListItemCast(Item);
          if Owner <> nil then
            with Owner do
            begin
              Inc(FCount);
              if First = Self then
                First := TListItemCast(Item);
            end;
        end;
      finally
        Item.EndUpdate;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TListItem.DoExtract;
begin
  with TListItemCast(Self) do
  begin
    if Owner <> nil then
      with Owner do
      begin
        if First = Self then
          First := Next;
          
        if Last = Self then
          Last := Prior;
      end;

    if Prior <> nil then
      Prior.Next := Next;

    if Next <> nil then
      Next.Prior := Prior;
  end;

  inherited;
end;

{ TList }

procedure TList.Grab(Item: TListItem);
begin
  if Item <> nil then
  begin
    BeginUpdate;
    try
      Item.BeginUpdate;
      try
        with TListItemCast(Item) do
        begin
          DoExtract;
          Owner := TListCast(Self);
        end;

        with TListCast(Self) do
        begin
          First := TListItemCast(Item);
          Last := TListItemCast(Item);
        end;
      finally
        Item.EndUpdate;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TList.DoClear;
begin
  while TListCast(Self).First <> nil do
    TListCast(Self).First.Free;
end;

procedure TList.Append(Item: TListItem);
begin
  BeginUpdate;
  try
    if TListCast(Self).Last <> nil then
      TListCast(Self).Last.Append(Item)
    else
      Grab(Item);
  finally
    EndUpdate;
  end;
end;

procedure TList.Prepend(Item: TListItem);
begin
  BeginUpdate;
  try
    if TListCast(Self).First <> nil then
      TListCast(Self).First.Prepend(Item)
    else
      Grab(Item);
  finally
    EndUpdate;
  end;
end;

{ TObjects }

constructor TObjects.Create(Capacity, Delta: Cardinal);
begin
  FDelta := Delta;
  SetCapacity(Capacity);
end;

procedure TObjects.DoClear;
begin
  SetCapacity(0);
end;

function TObjects.DoExtract(Index: Cardinal): TObject;
begin
  with TObjectsCast(Self) do
  begin
    Result := Items[Index];
    if Index < FCount - 1 then
      Move(Items[Index + 1], Items[Index], (Count - Index - 1) * SizeOf(TObject));
    Items[FCount - 1] := nil;
  end;
end;

procedure TObjects.DoSetCapacity(Value: Cardinal);
begin
  with TObjectsCast(Self) do
  begin
    ReallocMem(Items, Value * SizeOf(TObject));
    FCapacity := Value;
  end;

  if Value < FCount then
    FCount := Value;
end;

function TObjects.Append(Item: TObject): Cardinal;
begin
  BeginUpdate;
  try
    if FCount = FCapacity then
      DoSetCapacity(FCapacity + FDelta);
    TObjectsCast(Self).Items[FCount] := Item;
    Result := FCount;
    Inc(FCount);
  finally
    EndUpdate;
  end;
end;

procedure TObjects.Exchange(Index1, Index2: Cardinal);
begin
  CheckIndex(Index1);
  CheckIndex(Index2);

  BeginUpdate;
  try
    with TObjectsCast(Self) do
      CoreUtils.Exchange(Pointer(Items[Index1]), Pointer(Items[Index2]));
  finally
    EndUpdate;
  end;
end;

function TObjects.Extract(Index: Cardinal): TObject;
begin
  CheckIndex(Index);

  BeginUpdate;
  try
    Result := DoExtract(Index);
    Dec(FCount); // here is for compatibility with TEnumerableItem descendants
  finally
    EndUpdate;
  end;
end;

function TObjects.IndexOf(Item: TObject): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if TObjectsCast(Self).Items[I] = Item then
    begin
      Result := I;
      Exit;
    end;

  Result := -1;
end;

procedure TObjects.Insert(Index: Cardinal; Item: TObject);
begin
  CheckIndex(Index);

  BeginUpdate;
  try
    if FCount = FCapacity then
      DoSetCapacity(FCapacity + FDelta);
    with TObjectsCast(Self) do
    begin
      Move(Items[Index], Items[Index + 1], (FCount - Index) * SizeOf(TObject));
      Items[Index] := Item;
    end;
    Inc(FCount);
  finally
    EndUpdate;
  end;
end;

procedure TObjects.SetCapacity(Value: Cardinal);
begin
  if FCapacity <> Value then
  begin
    BeginUpdate;
    try
      DoSetCapacity(Value);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TObjects.SetCount(Value: Cardinal);
begin
  if FCount <> Value then
  begin
    BeginUpdate;
    try
      if Value > FCapacity then
        DoSetCapacity(Value + (Value + FDelta - 1) mod FDelta);
      FillChar(TObjectsCast(Self).Items[FCount], Value - FCount, 0);
      FCount := Value;
    finally
      EndUpdate;
    end;
  end;
end;

{ TInnerObjects }

procedure TInnerObjects.DoSetCapacity(Value: Cardinal);
var
  I: Integer;
begin
  for I := FCount - 1 downto Value - 1 do
    TObjectsCast(Self).Items[I].Free;
  inherited;
end;

{ TCollectionItem }

procedure TCollectionItem.DoExtract;
begin
  with TCollectionItemCast(Self) do
    if Owner <> nil then
      Owner.DoExtract(Owner.IndexOf(Self));
  inherited;
end;

end.
