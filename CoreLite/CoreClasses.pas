(*
    Lite Core Library (CoreLite)

    Platform-independent general purpose classes

    Copyright (c) 2015-2017 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Lite -- TCoreObject.InitInstance with built-in SizeOf(Pointer)
*)

unit CoreClasses;

interface

uses
  Windows, CoreUtils, CoreExceptions;

const
  InstanceSizeIndex = -8; // for Delphi 6, 7

type
  PCoreObject = ^TCoreObject;
  TCoreObject = object
  protected
    procedure InitInstance;
  public
    constructor Create;
    destructor Destroy; virtual; abstract;
    procedure Finalize;
    procedure Free;
    function InstanceSize: Integer;
    function TypeInfo: Pointer;
  end;

  PClearable = ^TClearable;
  TClearable = object(TCoreObject)
  public
    destructor Destroy; virtual; 
    procedure Clear; virtual; abstract;
  end;

  PContainer = ^TContainer;
  TContainer = object(TClearable)
  public
    class function ClassName: PLegacyChar;
  end;

  PEnumerable = ^TEnumerable;
  TEnumerable = object(TContainer)
  private
    FCount: Integer;
  public
    procedure Clear; virtual;
    property Count: Integer read FCount;
  end;

  PIndexed = ^TIndexed;
  TIndexed = object(TEnumerable)
  protected
    procedure CheckIndex(Index: Integer);
    procedure CheckRange(Index, ItemCount: Integer);
  end;

  TItem = record
    Instance: Pointer;
    case Byte of
      0: (Index: Integer);
      1: (Prior: Pointer);
  end;

  TIteration = record
    Item: TItem;
    Last: function(var Item: TItem): Boolean of object;
  end;

  TIterator = function(var Iteration: TIteration): Boolean of object;

  TCollectionInfo = record
  { hold } ClassName: PLegacyChar;
    ItemSize: Integer;
  end;

  TItemMode = (imInline, imFreeMem, imFinalize, imFree);
  TSharingMode = (smCopy, smAttach, smCapture);
  TBufferKind = (bkAttached, bkExternal, bkAllocated);
  TBufferMode = bkAttached..bkExternal; // TODO

  PCollection = ^TCollection;
  TCollection = object(TIndexed)
  private
    FCapacity, FDelta: Integer;
    FItemMode: TItemMode;
    FBufferKind: TBufferKind;
  { placeholder } // FItems: Pointer;
    procedure Copy(Index: Integer; Collection: PCollection; Capture: Boolean);
    procedure Expand(Index, ItemCount: Integer);
    procedure FreeItems(Index, ItemCount: Integer);
    procedure SetCapacity(Value: Integer);
  protected
  { ordered } class function CollectionInfo: TCollectionInfo; virtual; abstract;
    function Append(ItemCount: Integer = 1): Integer; overload;
    procedure Append(Source: Pointer; ItemCount, ItemsCapacity: Integer); overload;
    procedure Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer;
      AttachBuffer: Boolean); overload;
    procedure Assign(Source: PCollection; Mode: TSharingMode); overload;
    procedure Attach;
    procedure CheckCapacity(ItemCount: Integer);
    procedure Cut(Index: Integer; ItemCount: Integer = 1); virtual;
    procedure Insert(Index: Integer; ItemCount: Integer = 1); overload;
  public
    constructor Create(CollectionItemMode: TItemMode = imInline);
    destructor Destroy; virtual;
    procedure Append(Collection: PCollection; Capture: Boolean = False); overload;
    procedure AsRange(Source: PCollection; Index: Integer;
      CopyProps: Boolean = True); overload;
    procedure AsRange(Source: PCollection; Index, ItemCount: Integer;
      CopyProps: Boolean = True); overload;
    procedure Clear; virtual;
    procedure Delete(Index: Integer; ItemCount: Integer = 1);
    function DeleteExisting(Index: Integer; ItemCount: Integer = 1): Integer;
    procedure Detach; virtual;
    procedure ExternalBuffer(Source: Pointer; ItemCount: Integer);
    function FirstBackward(var Iteration: TIteration): Boolean;
    function FirstForward(var Iteration: TIteration): Boolean;
    procedure Insert(Index: Integer; Collection: PCollection; Capture: Boolean = False); overload;
    function LastBackward(var Item: TItem): Boolean;
    function LastForward(var Item: TItem): Boolean;
    procedure Skip(ItemCount: Integer = 1);
    function TranslateCapacity(NewCount: Integer): Integer;
    function TranslateDelta: Integer;
    procedure Truncate(ItemCount: Integer);

    property BufferKind: TBufferKind read FBufferKind;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Delta: Integer read FDelta write FDelta;
    property ItemMode: TItemMode read FItemMode;
  end;

  PPointers = ^TPointers;
  TPointers = object(TCollection)
    function Append(Item: Pointer): Integer;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): Pointer;
    function ExtractExisting(Index: Integer): Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
  end;

  PCollections = PCollection;
  TCollections = TCollection;

  TListInfo = record
  { hold } ClassName: PLegacyChar;
    ItemOffset: Integer;
  end;

  PList = ^TList;
  TList = object(TEnumerable)
  private
    FItemMode: TItemMode;
  { placeholder } // FFirst, FLast: PListItem;
  protected
  { ordered } class function ListInfo: TListInfo; virtual; abstract;
  public
    constructor Create(ListItemMode: TItemMode = imFree);
    procedure Append(Item: Pointer); overload;
    function Append(FirstItem, LastItem: Pointer): Integer; overload;
    class procedure AppendItem(Item, AfterItem: Pointer); overload;
    class function AppendItem(FirstItem, LastItem, AfterItem: Pointer): Integer; overload;
    procedure Clear; virtual;
    class procedure Delete(Item: Pointer); overload;
    class function Delete(FirstItem, LastItem: Pointer): Integer; overload;
    class procedure Extract(Item: Pointer);
    function FirstBackward(var Iteration: TIteration): Boolean;
    function FirstForward(var Iteration: TIteration): Boolean;
    function LastBackward(var Item: TItem): Boolean;
    function LastForward(var Item: TItem): Boolean;
    //procedure Prepend(Item: Pointer; Before: Pointer = nil);
    function Skip(LastItem: Pointer): Integer;
    function Truncate(FirstItem: Pointer): Integer;

    property ItemMode: TItemMode read FItemMode;
  end;

  PCollectionList = PList;
  TCollectionList = TList;

  TCRC32Table = array[0..$FF] of LongWord;

  TCRC32 = object
  private
    FTable: TCRC32Table;
  public
    constructor Create(Polynomial: LongWord = $EDB88320);
    function CalcChecksum(const Buf; Count: Integer; Initial: LongWord = 0): LongWord;
    property Table: TCRC32Table read FTable;
  end;

{ Exceptions }

  EContainer = class(Exception);

  EIndexed = EContainer; // future class of (EContainer)

  EIndex = class(EIndexed)
  private
    FIndex: Integer;
  { placeholder } // FContainer: PIndexed;
  public
    constructor Create(Container: PIndexed; Index: Integer);
    property Index: Integer read FIndex;
  end;

  ERange = class(EIndexed)
  private
    FLowBound, FHighBound: Integer;
  { placeholder } // FContainer: PIndexed;
  public
    constructor Create(Container: PIndexed; Index, ItemCount: Integer);
    property LowBound: Integer read FLowBound;
    property HighBound: Integer read FHighBound;
  end;

  ECollectionIndex = class(EIndex)
  private
  { hold } FCollection: PCollection;
  public
    property Collection: PCollection read FCollection;
  end;

  ECollectionRange = class(ERange)
  private
  { hold } FCollection: PCollection;
  public
    property Collection: PCollection read FCollection;
  end;

  ECollection = EContainer; // future class of (EContainer)

  ECapacity = class(ECollection)
  private
    FCollection: PCollection;
    FItemCount: Integer;
  public
    constructor Create(Collection: PCollection; ItemCount: Integer);
    property Collection: PCollection read FCollection;
    property ItemCount: Integer read FItemCount;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);

function AverageCount(const Iterator: TIterator): Integer;
function TotalCount(const Iterator: TIterator): Integer;

procedure DetachItems(const Iterator: TIterator);

implementation

uses
  CoreConsts;

type
  PCollectionCast = ^TCollectionCast;
  TCollectionCast = object(TCollection)
    Items: PAddress;
    Props: Byte;
  end;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  PPointersCast = ^TPointersCast;
  TPointersCast = object(TPointers)
    Items: PPointerArray;
  end;

  PCollectionsCast = ^TCollectionsCast;
  TCollectionsCast = object(TCollections)
    Items: PCollection;
  end;

  PListCast = ^TListCast;
  TListCast = object(TList)
    First, Last: PAddress;
  end;

  PListItem = ^TListItem;
  TListItem = object
    Owner: PListCast;
    Prev, Next: PAddress;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TCoreObject.Free
end;

function AverageCount(const Iterator: TIterator): Integer;
begin
  Result := PCollection(TMethod(Iterator).Data).Count;
  if Result <> 0 then
    Result := Round32(TotalCount(Iterator) / Result);
end;

function TotalCount(const Iterator: TIterator): Integer;
var
  Iteration: TIteration;
begin
  Result := 0;
  if Iterator(Iteration) then
    with Iteration do
      repeat
        Inc(Result, PCollection(Item.Instance).Count);
      until Last(Item);
end;

procedure DetachItems(const Iterator: TIterator);
var
  Iteration: TIteration;
begin
  if Iterator(Iteration) then
    with Iteration do
      repeat
        PCollection(Item.Instance).Detach;
      until Last(Item);
end;

{ EIndex }

constructor EIndex.Create(Container: PIndexed; Index: Integer);
begin
  if Container <> nil then
    if Container.Count <> 0 then
      inherited Create(sIndexOutOfBounds, [Index, 0, Container.Count - 1, Container.ClassName])
    else
      inherited Create(sIndexOutOfEmpty, [Index, Container.ClassName])
  else
    inherited Create(sIndexOutOfNull, [Index]);
  ECollectionIndex(Self).FCollection := Pointer(Container);
  FIndex := Index;
end;

{ ERange }

constructor ERange.Create(Container: PIndexed; Index, ItemCount: Integer);
var
  LastIndex: Integer;
begin
  LastIndex := Index + ItemCount - 1;
  if Container <> nil then
    if Container.Count <> 0 then
      inherited Create(sRangeOutOfBounds, [Index, LastIndex, 0, Container.Count - 1, Container.ClassName])
    else
      inherited Create(sRangeOutOfEmpty, [Index, LastIndex, Container.ClassName])
  else
    inherited Create(sRangeOutOfNull, [Index, LastIndex]);
  ECollectionIndex(Self).FCollection := Pointer(Container);
  FLowBound := Index;
  FHighBound := LastIndex;
end;

{ ECapacity }

constructor ECapacity.Create(Collection: PCollection; ItemCount: Integer);
begin
  if Collection <> nil then
    inherited Create(sOutOfCapacity, [Collection.ClassName, Collection.Capacity, ItemCount])
  else
    inherited Create(sNullCapacity, [ItemCount]);
  FCollection := Collection;
end;

{ TCoreObject }

constructor TCoreObject.Create;
begin
  InitInstance;
end;

procedure TCoreObject.Finalize;
begin
  if TypeOf(Self) <> nil then
    Destroy;
end;

procedure TCoreObject.Free;
var
  Instance: PCoreObject;
begin
  if (@Self <> nil) and (TypeOf(Self) <> nil) then
  begin
    Instance := @Self;
    Dispose(Instance, Destroy);
  end;
end;

procedure TCoreObject.InitInstance;
begin
{$IFDEF Lite}
  FillChar(PEnumerable(@Self).FCount, InstanceSize - SizeOf(Pointer), 0);
{$ELSE}
  with PEnumerable(@Self)^ do
    FillChar(FCount, InstanceSize - (PAddress(@FCount) - PAddress(@Self)), 0);
{$ENDIF}     // ^--- first field after VMT
end;

function TCoreObject.InstanceSize: Integer;
begin
  if (@Self <> nil) and (TypeOf(Self) <> nil) then  // Fast core
    Result := PInteger(PAddress(TypeOf(Self)) + InstanceSizeIndex)^
  else
    Result := 0;
end;

function TCoreObject.TypeInfo: Pointer;
begin
  if @Self <> nil then
    Result := TypeOf(Self)
  else
    Result := nil;
end;

{ TClearable }

destructor TClearable.Destroy;
begin
  Clear;
end;

{ TContainer }

class function TContainer.ClassName: PLegacyChar;
begin
  Result := PCollection(@Self).CollectionInfo.ClassName;
end;

{ TEnumerable }

procedure TEnumerable.Clear;
begin
  FCount := 0;
end;

{ TIndexed }

procedure TIndexed.CheckIndex(Index: Integer);
begin
  if (@Self = nil) or (Index < 0) or (Index > FCount) then
    raise EIndex.Create(@Self, Index);
end;

procedure TIndexed.CheckRange(Index, ItemCount: Integer);
var
  LastIndex: Integer;
begin
  if (@Self <> nil) and (Index >= 0) and (Index < FCount) then
  begin
    LastIndex := Index + ItemCount - 1;
    if (LastIndex >= 0) and (LastIndex < FCount) then
      Exit;
  end;
  raise ERange.Create(@Self, Index, ItemCount);
end;

{ TCollection }

constructor TCollection.Create(CollectionItemMode: TItemMode);
begin
  InitInstance;
  FItemMode := CollectionItemMode;
end;

destructor TCollection.Destroy;
begin
  SetCapacity(0);
end;

function TCollection.Append(ItemCount: Integer): Integer;
begin
  Result := FCount;
  if FCount + ItemCount <= FCapacity then
    Inc(FCount, ItemCount)
  else
    Expand(Result, ItemCount);
end;

procedure TCollection.Append(Collection: PCollection; Capture: Boolean);
begin
  if Collection <> nil then
    Copy(FCount, Collection, Capture);
end;

procedure TCollection.Append(Source: Pointer; ItemCount, ItemsCapacity: Integer);
var
  NewCapacity: Integer;
begin
  NewCapacity := FCapacity + ItemsCapacity;
  if (FBufferKind = bkAttached) or (FCapacity < NewCapacity) then
    SetCapacity(NewCapacity);
  with CollectionInfo do
    Move(Source^, PCollectionCast(@Self).Items[FCount * ItemSize], ItemCount * ItemSize);
  Inc(FCount, ItemCount);
  FItemMode := imInline;
end;

procedure TCollection.Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer;
  AttachBuffer: Boolean);
begin
  Clear;
  if AttachBuffer then
  begin
    PCollectionCast(@Self).Items := Source;
    FCapacity := ItemsCapacity;
  end
  else
  begin
    if FCapacity < ItemsCapacity then
      SetCapacity(ItemsCapacity);
    Move(Source^, PCollectionCast(@Self).Items^, ItemCount * CollectionInfo.ItemSize);
  end;
  FBufferKind := TBufferKind(Byte(not AttachBuffer) shl 1); // Fast core
  FItemMode := imInline;
  FCount := ItemCount;
end;

procedure TCollection.Assign(Source: PCollection; Mode: TSharingMode);
var
  CanCapture: Boolean;
begin
  if Source <> nil then
  begin
    CanCapture := (Mode = smCapture) and (Source.BufferKind <> bkAttached);
    Assign(PCollectionCast(@Source).Items, Source.FCount, Source.FCapacity,
      (Mode = smAttach) or CanCapture);
    if CanCapture then
    begin
      FItemMode := Source.FItemMode;
      FBufferKind := bkAllocated;
      Source.FBufferKind := bkAttached;
    end;
  end
  else
    Clear;
end;

procedure TCollection.AsRange(Source: PCollection; Index: Integer; CopyProps: Boolean);
begin
  AsRange(Source, Index, Source.Count - Index, CopyProps);
end;

procedure TCollection.AsRange(Source: PCollection; Index, ItemCount: Integer;
  CopyProps: Boolean);
var
  SrcLen, DstLen: Integer;
begin
  if (Index >= 0) and (Index < Source.Count) and (Index + ItemCount <= Source.Count) then
    Assign(PCollectionCast(Source).Items + Index * Source.CollectionInfo.ItemSize, ItemCount,
      PCollectionCast(Source).FCapacity - Index, True)
  else
    Clear;

  if CopyProps and (Source <> nil) then
  begin
    SrcLen := Source.InstanceSize;
    DstLen := InstanceSize;
    if SrcLen < DstLen then
      DstLen := SrcLen;
    Move(PCollectionCast(Source).Props, PCollectionCast(@Self).Props,
      DstLen - (PAddress(@PCollectionCast(@Self).Props) - PAddress(@Self)));
  end;
end;

procedure TCollection.Attach;
begin
  if FCapacity <> 0 then
    FBufferKind := bkAttached;
end;

procedure TCollection.CheckCapacity(ItemCount: Integer);
var
  NewCount: Integer;
begin
  NewCount := FCount + ItemCount;
  if (NewCount < 0) or (NewCount > FCapacity) then
    raise ECapacity.Create(@Self, ItemCount);
end;

procedure TCollection.Clear;
begin
  if FBufferKind = bkAttached then
  begin
    PCollectionCast(@Self).Items := nil;
    FBufferKind := bkAllocated;
    FCapacity := 0;
  end
  else if FItemMode <> imInline then
    FreeItems(0, FCount);
  FCount := 0;
end;

procedure TCollection.Copy(Index: Integer; Collection: PCollection; Capture: Boolean);
var
  ItemSize, MinItemSize, SourceItemSize, I: Integer;
  Source, Dest: PAddress;
begin
  Expand(Index, Collection.Count);

  ItemSize := CollectionInfo.ItemSize;
  SourceItemSize := Collection.CollectionInfo.ItemSize;
  if ItemSize = SourceItemSize then
    with PCollectionCast(Collection)^ do
    begin
      Move(Items^, PCollectionCast(@Self).Items[Index * ItemSize], Count * ItemSize);
      if Capture and (FBufferKind <> bkAttached) then
      begin
        FreeMem(Items);
        Items := PCollectionCast(@Self).Items + Index * ItemSize;
        FCapacity := Count;
        FBufferKind := bkAttached;
      end;
      Exit;
    end
  else if ItemSize > SourceItemSize then
    MinItemSize := SourceItemSize
  else
    MinItemSize := ItemSize;

  Source := PCollectionCast(Collection).Items;
  Dest := PCollectionCast(@Self).Items + Index * ItemSize;
  for I := 0 to Collection.Count - 1 do
  begin
    Move(Source^, Dest^, MinItemSize);
    Inc(Source, SourceItemSize);
    Inc(Dest, ItemSize);
  end;
end;

procedure TCollection.Cut(Index, ItemCount: Integer);
var
  ItemSize, FirstBytes, LastBytes, NewCount, NewCapacity: Integer;
  NewItems: PAddress;
begin
  CheckCapacity(-ItemCount);

  ItemSize := CollectionInfo.ItemSize;
  NewCount := FCount - ItemCount;
  FirstBytes := Index * ItemSize;
  LastBytes := (Index + ItemCount) * ItemSize;

  if FBufferKind = bkAttached then
  begin
    if Index = 0 then
    begin
      Inc(PCollectionCast(@Self).Items, LastBytes);
      Dec(FCapacity, ItemCount);
    end
    else if Index + ItemCount < FCount then
    begin
      NewCapacity := TranslateCapacity(NewCount);
      GetMem(NewItems, NewCapacity * ItemSize);
      with PCollectionCast(@Self)^ do
      begin
        Move(Items^, NewItems^, FirstBytes);
        Move(Items[LastBytes], NewItems[FirstBytes], LastBytes);
        Items := NewItems;
      end;
      FCapacity := NewCapacity;
      FBufferKind := bkAllocated;
    end;
  end
  else
  begin
    if FItemMode <> imInline then
      FreeItems(Index, ItemCount);
    with PCollectionCast(@Self)^ do
      Move(Items[LastBytes], Items[FirstBytes], (NewCount - Index) * ItemSize);
  end;

  FCount := NewCount;
end;

procedure TCollection.Delete(Index, ItemCount: Integer);
begin
  if ItemCount <> 0 then
  begin
    CheckIndex(Index);
    Cut(Index, ItemCount);
  end;
end;

function TCollection.DeleteExisting(Index, ItemCount: Integer): Integer;
begin
  if Index >= 0 then
  begin
    Result := Count - Index;
    if ItemCount < Result then
      Result := ItemCount;
    Cut(Index, Result);
  end
  else
    Result := 0;
end;

procedure TCollection.Detach;
begin
  if FBufferKind = bkAttached then
    SetCapacity(FCount);
end;

procedure TCollection.Expand(Index, ItemCount: Integer);
var
  ItemSize, FirstBytes, NewCount, NewCapacity: Integer;
  NewItems, Src, Dst: PAddress;
begin
{$IFDEF Debug}
  if ItemCount < 0 then
    raise ERange.Create(@Self, Index, ItemCount);
{$ENDIF}
  ItemSize := CollectionInfo.ItemSize;
  NewCount := FCount + ItemCount;
  if (NewCount <= FCapacity) and (FBufferKind <> bkAttached) then
    with PCollectionCast(@Self)^ do
    begin
      Src := Items + Index * ItemSize;
      Dst := Items + (Index + ItemCount) * ItemSize;
      Move(Src^, Dst^, Items + FCount * ItemSize - Src);
    end
  else
  begin
    if FDelta = 0 then
      raise ECapacity.Create(@Self, ItemCount);
    NewCapacity := TranslateCapacity(NewCount);
    GetMem(NewItems, NewCapacity * ItemSize);
    FirstBytes := Index * ItemSize;
    with PCollectionCast(@Self)^ do
    begin
      Move(Items^, NewItems^, FirstBytes);
      Move(Items[FirstBytes], NewItems[FirstBytes + ItemCount * ItemSize],
        (FCount - Index) * ItemSize);
      Items := NewItems;
    end;
    FCapacity := NewCapacity;
    FBufferKind := bkAllocated;
  end;
  FCount := NewCount;
end;

procedure TCollection.ExternalBuffer(Source: Pointer; ItemCount: Integer);
begin
  Clear;
  PCollectionCast(@Self).Items := Source;
  FCapacity := ItemCount;
  FDelta := 0;
  FBufferKind := bkExternal;
end;

function TCollection.FirstBackward(var Iteration: TIteration): Boolean;
begin
  with Iteration, Item do
  begin
    Last := LastBackward;
    Index := FCount - 1;
    Instance := PCollectionCast(@Self).Items + Index * CollectionInfo.ItemSize;
    Result := Index >= 0;
  end;
end;

function TCollection.FirstForward(var Iteration: TIteration): Boolean;
begin
  with Iteration, Item do
  begin
    Last := LastForward;
    Index := 0;
    Instance := PCollectionCast(@Self).Items;
    Result := Index >= 0;
  end;
end;

procedure TCollection.FreeItems(Index, ItemCount: Integer);
var
  I, ItemSize: Integer;
  Item: Pointer;
begin
  ItemSize := CollectionInfo.ItemSize;
  Item := PCollectionCast(@Self).Items + (Index + ItemCount - 1) * ItemSize;
  for I := 0 to ItemCount - 1 do
  begin
    case FItemMode of
      imFreeMem:
        FreeMem(PPointer(Item)^);
      imFinalize:
        PCoreObject(Item).Finalize;
      imFree:
        PCoreObject(Item^).Free;
    end;
    Item := PAddress(Item) - ItemSize;
  end;
end;

procedure TCollection.Insert(Index, ItemCount: Integer);
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}  
  Expand(Index, ItemCount);
end;

procedure TCollection.Insert(Index: Integer; Collection: PCollection;
  Capture: Boolean);
begin
  if Collection <> nil then
  begin
    CheckIndex(Index);
    Copy(Index, Collection, Capture);
  end;
end;

function TCollection.LastBackward(var Item: TItem): Boolean;
begin
  with Item do
  begin
    Dec(Index);
    Instance := PCollectionCast(@Self).Items + Index * CollectionInfo.ItemSize;
    Result := Index < 0;
  end;
end;

function TCollection.LastForward(var Item: TItem): Boolean;
begin
  with Item do
  begin
    Inc(Index);
    Instance := PCollectionCast(@Self).Items + Index * CollectionInfo.ItemSize;
    Result := Index >= FCount;
  end;
end;

procedure TCollection.SetCapacity(Value: Integer);
var
  ItemSize: Integer;
  NewItems: Pointer;
begin
  if Value < 0 then
    Inc(Value, FCapacity);

  if (Value < FCount) and (FItemMode <> imInline) and (FBufferKind <> bkAttached) then
    FreeItems(Value, FCount - Value);

  ItemSize := CollectionInfo.ItemSize;
  if FBufferKind <> bkAllocated then
  begin
    if Value <> 0 then
    begin
      GetMem(NewItems, Value * ItemSize);
      Move(PCollectionCast(@Self).Items^, NewItems^, Count * ItemSize);
      PCollectionCast(@Self).Items := NewItems;
    end
    else
      PCollectionCast(@Self).Items := nil;
    FBufferKind := bkAllocated;
  end
  else if (FCount <> 0) and (Value <> 0) then
    ReallocMem(PCollectionCast(@Self).Items, Value * ItemSize)
  else
  begin
    FreeMemAndNil(PCollectionCast(@Self).Items);
    if Value <> 0 then
      GetMem(PCollectionCast(@Self).Items, Value * ItemSize);
  end;

  FCapacity := Value;
  if Value < FCount then
    FCount := Value;
end;

procedure TCollection.Skip(ItemCount: Integer);
begin
  if ItemCount <> 0 then
    Cut(0, ItemCount);
end;

function TCollection.TranslateCapacity(NewCount: Integer): Integer;
var
  GrowBy: Integer;
begin
  GrowBy := TranslateDelta;
  Result := NewCount + GrowBy - NewCount mod GrowBy;
end;

function TCollection.TranslateDelta: Integer;
var
  NewResult: Integer;
begin
  if FDelta < 0 then
  begin
    Result := -FDelta;
    if FCapacity > Result then
    begin
      NewResult := FCapacity div Result;
      if NewResult > 0 then
      begin
        Result := NewResult;
        Exit;
      end;
    end;
  end
  else
    Result := FDelta;
end;

procedure TCollection.Truncate(ItemCount: Integer);
var
  NewCount: Integer;
begin
  if ItemCount < 0 then
    CheckCapacity(-ItemCount);
  NewCount := FCount - ItemCount;
  if NewCount > 0 then
  begin
    if (FBufferKind <> bkAttached) and (FItemMode <> imInline) then
      FreeItems(NewCount, ItemCount);
    FCount := NewCount;
  end
  else
    Clear;
end;

{ TPointers }

function TPointers.Append(Item: Pointer): Integer;
begin
  Result := inherited Append;
  PPointersCast(@Self).Items[Result] := Item;
end;

procedure TPointers.Exchange(Index1, Index2: Integer);
begin
  CheckIndex(Index1);
  CheckIndex(Index2);
  with PPointersCast(@Self)^ do
    CoreUtils.Exchange(Items[Index1], Items[Index2]);
end;

function TPointers.Extract(Index: Integer): Pointer;
begin
  CheckIndex(Index);
  Result := PPointersCast(@Self).Items[Index];
  Delete(Index);
end;

function TPointers.ExtractExisting(Index: Integer): Pointer;
begin
  if Index >= 0 then
  begin
    Result := PPointersCast(@Self).Items[Index];
    Delete(Index);
  end
  else
    Result := nil;
end;

function TPointers.IndexOf(Item: Pointer): Integer;
begin
  for Result := 0 to FCount - 1 do
    if PPointersCast(@Self).Items[Result] = Item then
      Exit;
  Result := -1;
end;

procedure TPointers.Insert(Index: Integer; Item: Pointer);
begin
  inherited Insert(Index);
  PPointersCast(@Self).Items[Index] := Item;
end;

{ TList }

constructor TList.Create(ListItemMode: TItemMode);
begin
  InitInstance;
  FItemMode := ListItemMode;
end;

procedure TList.Append(Item: Pointer);
begin
  with PListCast(@Self)^ do
    if Last <> nil then
      AppendItem(Item, Last)
    else
    begin
      Extract(Item);
      PListItem(PAddress(Item) + ListInfo.ItemOffset).Owner := Pointer(@Self);
      First := Item;
      Last := Item;
      Inc(FCount);
    end;
end;

function TList.Append(FirstItem, LastItem: Pointer): Integer;
begin
  Result := AppendItem(FirstItem, LastItem, PListCast(@Self).Last); // TODO
end;

class procedure TList.AppendItem(Item, AfterItem: Pointer);
var
  After: PListItem;
begin
  with ListInfo, PListItem(PAddress(Item) + ItemOffset)^ do
  begin
    if Owner <> nil then
      Owner.Extract(Item);
    Prev := AfterItem;
    After := PListItem(PAddress(AfterItem) + ItemOffset);
    Next := After.Next;
    After.Next := Item;
    Owner := After.Owner;
    if Next = nil then
      Owner.Last := Item;
    Inc(Owner.FCount);
  end;
end;

class function TList.AppendItem(FirstItem, LastItem, AfterItem: Pointer): Integer;
begin
  Result := 0; // TODO
end;

procedure TList.Clear;
begin
  Delete(nil, nil);
end;

class procedure TList.Delete(Item: Pointer);
var
  DeleteMode: TItemMode;
begin
  DeleteMode := PListItem(PAddress(Item) + ListInfo.ItemOffset).Owner.ItemMode;
  Extract(Item);
  case DeleteMode of
    imFreeMem:
      FreeMem(Item);
    imFinalize:
      PCoreObject(Item).Finalize;
    imFree:
      PCoreObject(Item).Free;
  end;
end;

class function TList.Delete(FirstItem, LastItem: Pointer): Integer;
begin
  Result := 0; // TODO
end;

class procedure TList.Extract(Item: Pointer);
begin
  with ListInfo, PListItem(PAddress(Item) + ItemOffset)^ do
  begin
    if Prev <> nil then
      PListItem(PAddress(Prev) + ItemOffset).Next := Next
    else if Owner <> nil then
      Owner.First := Next;

    if Next <> nil then
      PListItem(PAddress(Next) + ItemOffset).Prev := Prev
    else if Owner <> nil then
      Owner.Last := Prev;

    if Owner <> nil then
      Dec(Owner.FCount);

    Owner := nil;
    Prev := nil;
    Next := nil;
  end;
end;

function TList.FirstBackward(var Iteration: TIteration): Boolean;
begin
  with Iteration, Item do
  begin
    Last := LastBackward;
    Instance := PListCast(@Self).Last;
    Result := Instance <> nil;
  end;
end;

function TList.FirstForward(var Iteration: TIteration): Boolean;
begin
  with Iteration, Item do
  begin
    Last := LastForward;
    Instance := PListCast(@Self).First;
    Result := Instance <> nil;
  end;
end;

function TList.LastBackward(var Item: TItem): Boolean;
begin
  with Item do
  begin
    Instance := PListItem(PAddress(Instance) + ListInfo.ItemOffset).Prev;
    Result := Instance = nil;
  end;
end;

function TList.LastForward(var Item: TItem): Boolean;
begin
  with Item do
  begin
    Instance := PListItem(PAddress(Instance) + ListInfo.ItemOffset).Next;
    Result := Instance = nil;
  end;
end;

{procedure TList.Prepend(Item, Before: Pointer);
begin
  if Before = nil then
    Before := PListCast(@Self).First; // TODO
end;}

function TList.Skip(LastItem: Pointer): Integer;
begin
  Result := Delete(nil, LastItem);
end;

function TList.Truncate(FirstItem: Pointer): Integer;
begin
  Result := Delete(FirstItem, nil);
end;

{ TCRC32 }

constructor TCRC32.Create(Polynomial: LongWord);
var
  I, J: Integer;
  T: LongWord;
begin
  for I := Low(FTable) to High(FTable) do
  begin
    T := I;
    for J := 0 to 7 do
      if T and 1 <> 0 then
        T := (T shr 1) xor Polynomial
      else
        T := T shr 1;
    FTable[I] := T;
  end;
end;

function TCRC32.CalcChecksum(const Buf; Count: Integer; Initial: LongWord): LongWord;
var
  I: Integer;
begin
  Result := not Initial;
  for I := 0 to Count - 1 do
    Result := FTable[(Result xor TByteArray(Buf)[I]) and $FF] xor (Result shr 8);
  Result := not Result;
end;

end.
