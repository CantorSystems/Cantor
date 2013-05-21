(*
    Lite Core Library (CoreLite)

    Non-platform general classes

    Copyright (c) 2012-2013 Vladislav Javadov (Freeman)
*)

unit CoreClasses;

interface

uses
  CoreUtils, CoreExceptions;

type
  TContainedItem = class
  private
  { placeholder } // FOwner: TContainer;
  public
    destructor Destroy; override;
    procedure Extract; virtual; abstract;
  end;

  TContainer = class
  public
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
  end;

  TEnumerableItem = class(TContainedItem)
  public
    procedure Extract; override;
  end;

  TEnumerable = class(TContainer)
  protected
    FCount: Integer; // not private for CoreStrings and other
  public
  //  Clear override not needed, Count will be changed (also automatically) in the descendants
    property Count: Integer read FCount;
  end;

  TIndexed = class(TEnumerable)
  protected
    procedure CheckIndex(Index: Integer);
  end;

  TListItem = class(TEnumerableItem)
  private
  { placeholder } // FPrior, FNext: TListItem;
  public
    procedure Append(Item: TListItem);
    procedure Extract; override;
    procedure Prepend(Item: TListItem);
  end;

  TList = class(TEnumerable)
  private
  { placeholder } // FFirst, FLast: TListItem;
    procedure Grab(Item: TListItem);
  public
    procedure Append(Item: TListItem);
    procedure Clear; override;
    procedure Prepend(Item: TListItem);
  end;

  TBalancedTreeItem = class(TEnumerableItem)
  private
  { placeholder } // FParent, FLeft, FRight: TBalancedTreeItem;
  protected
    function ParentOf(Item: TBalancedTreeItem): TBalancedTreeItem;
  public
    function Compare(Item: TBalancedTreeItem): Integer; virtual; abstract;
    procedure Extract; override;
    function Find(Item: TBalancedTreeItem): TBalancedTreeItem;
    function Insert(Item: TBalancedTreeItem; DupFree: Boolean = True): Boolean;
    function Root: TBalancedTreeItem;
  end;

  TBalancedTree = class(TEnumerable)
  private
  { placeholder } // FRoot: TBalancedTreeItem;
  public
    procedure Clear; override;
    function Find(Item: TBalancedTreeItem): TBalancedTreeItem;
    function Insert(Item: TBalancedTreeItem; DupFree: Boolean = True): Boolean;
  end;

  TRedBlackTreeItem = class(TBalancedTreeItem)
  { placeholder } // FRed: Boolean;
  protected
  end;

  TRedBlackTree = TBalancedTree;

  TObjects = class(TIndexed)
  private
    FCapacity, FDelta: Integer;
    FOwnsObjects: Boolean;
  { placeholder } // FItems: PObjectArray;
    procedure SetCapacity(Value: Integer);
    procedure SetCount(Value: Integer);
  public
    constructor Create(Initial, Delta: Integer; OwnsObjects: Boolean = False);
    function Append(Item: TObject): Integer;
    procedure Clear; override;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): TObject;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
  // properites
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count write SetCount;
    property Delta: Integer read FDelta write FDelta;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TCollectionItem = class(TContainedItem)
  public
    procedure Extract; override;
  end;

  TCollection = TObjects;

{ Exceptions }

  EContainer = class(Exception)         
  private
    FContainer: TObject;
  public
    property Container: TObject read FContainer;
  end;

  EIndex = class(EContainer)
  private
    FLowBound, FHighBound, FIndex: Integer;
  public
    constructor Create(Container: TObject; Index, LowBound, HighBound: Integer); overload; // because of ERange.Create

    property Index: Integer read FIndex;
    property HighBound: Integer read FHighBound;
    property LowBound: Integer read FLowBound;
  end;

  ERange = class(EIndex)
  private
    FCount: Integer;
  public
    constructor Create(Container: TObject; Index, Count, LowBound, HighBound: Integer);
    property Count: Integer read FCount;
  end;

implementation

uses
  CoreConsts;

type
  TListCast = class;

  TListItemCast = class(TListItem)
    Owner: TListCast;
    Prior, Next: TListItemCast;
  end;

  TListCast = class(TList)
    First, Last: TListItemCast;
  end;

  TBalancedTreeCast = class;

  TBalancedTreeItemCast = class(TBalancedTreeItem)
    Owner: TBalancedTreeCast;
    Parent, Left, Right: TBalancedTreeItemCast;
  end;

  TBalancedTreeCast = class(TBalancedTree)
    Root: TBalancedTreeItemCast;
  end;

  TRedBlackTreeCast = class;

  TRedBlackTreeItemCast = class(TRedBlackTreeItem)
    Owner: TRedBlackTreeCast;
    Parent, Left, Right: TRedBlackTreeItemCast;
    Red: Boolean;
  end;

  TRedBlackTreeCast = class(TRedBlackTree)
    Root: TRedBlackTreeItemCast;
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

{ EIndex }

constructor EIndex.Create(Container: TObject; Index, LowBound, HighBound: Integer);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sIndexOutOfBounds, [@ClassName, Index, LowBound, HighBound]);
  FContainer := Container;
  FLowBound := LowBound;
  FHighBound := HighBound;
  FIndex := Index;
end;

{ ERange }

constructor ERange.Create(Container: TObject; Index, Count, LowBound, HighBound: Integer);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sRangeOutOfBounds, [@ClassName, Index, Index + Count, LowBound, HighBound]);
  FContainer := Container;
  FLowBound := LowBound;
  FHighBound := HighBound;
  FIndex := Index;
  FCount := Count;
end;

{ TContainedItem }

destructor TContainedItem.Destroy;
begin
  Extract;
//  inherited;
end;

{ TContainer }

destructor TContainer.Destroy;
begin
  Clear;
//  inherited;
end;

{ TEnumerableItem }

procedure TEnumerableItem.Extract;
begin
  with TListItemCast(Self) do
    if Owner <> nil then
      Dec(Owner.FCount);
  inherited;
end;

{ TIndexed }

procedure TIndexed.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index > FCount) then
    raise EIndex.Create(Self, Index, 0, FCount);
end;

{ TListItem }

procedure TListItem.Append(Item: TListItem);
begin
  if Item <> nil then
  begin
    Item.Extract;
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
  end;
end;

procedure TListItem.Prepend(Item: TListItem);
begin
  if Item <> nil then
  begin
    Item.Extract;
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
  end;
end;

procedure TListItem.Extract;
begin
  with TListItemCast(Self) do
  begin
    if Owner <> nil then
    begin
      with Owner do
      begin
        if First = Self then
          First := Next;
        if Last = Self then
          Last := Prior;
      end;
      Owner := nil;
    end;

    if Prior <> nil then
    begin
      Prior.Next := Next;
      Prior := nil;
    end;

    if Next <> nil then
    begin
      Next.Prior := Prior;
      Next := nil;
    end;
  end;

  inherited;
end;

{ TList }

procedure TList.Grab(Item: TListItem);
begin
  if Item <> nil then
  begin
    with TListItemCast(Item) do
    begin
      Extract;
      Owner := TListCast(Self);
    end;

    with TListCast(Self) do
    begin
      First := TListItemCast(Item);
      Last := TListItemCast(Item);
    end;
  end;
end;

procedure TList.Clear;
begin
  with TListCast(Self) do
    while Last <> nil do
      Last.Destroy; // Fast core
end;

procedure TList.Append(Item: TListItem);
begin
  with TListCast(Self) do
    if Last <> nil then
      Last.Append(Item)
    else
      Grab(Item);
end;

procedure TList.Prepend(Item: TListItem);
begin
  with TListCast(Self) do
    if First <> nil then
      First.Prepend(Item)
    else
      Grab(Item);
end;

{ TBalancedTreeItem }

procedure TBalancedTreeItem.Extract;
begin
  inherited;
end;

function TBalancedTreeItem.Find(Item: TBalancedTreeItem): TBalancedTreeItem;
var
  Cmp: Integer;
begin
  Result := Root;
  while Result <> nil do
  begin
    Cmp := Result.Compare(Item);
    if Cmp < 0 then
      Result := TBalancedTreeItemCast(Result).Left
    else if Cmp > 0 then
      Result := TBalancedTreeItemCast(Result).Right
    else
      Break;
  end;
end;

function TBalancedTreeItem.Insert(Item: TBalancedTreeItem; DupFree: Boolean): Boolean;
var
  Cmp: Integer;
  P: TBalancedTreeItemCast;
begin
  if P <> nil then
  begin
    P := TBalancedTreeItemCast(Root);

{    while P <> nil do
    begin
      Cmp := P.Compare(Item);
      if Cmp < 0 then
        Result := TBalancedTreeItemCast(Result).Left
      else if Cmp > 0 then
        Result := TBalancedTreeItemCast(Result).Right
      else
        Break;
    end;}

    Result := True;
  end
  else
    Result := False;
end;

function TBalancedTreeItem.ParentOf(Item: TBalancedTreeItem): TBalancedTreeItem;
begin

end;

function TBalancedTreeItem.Root: TBalancedTreeItem;
begin
  with TBalancedTreeItemCast(Self) do
    if Owner <> nil then
      Result := Owner.Root
    else
    begin
      Result := Self;
      while Parent <> nil do
        Result := Parent;
    end;
end;

{ TBalancedTree }

procedure TBalancedTree.Clear;

procedure ClearItem(Item: TBalancedTreeItemCast);
begin
end;

begin // TODO: Fast clear
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Root.Extract;
end;

function TBalancedTree.Find(Item: TBalancedTreeItem): TBalancedTreeItem;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Find(Item)
    else
      Result := nil;
end;

function TBalancedTree.Insert(Item: TBalancedTreeItem; DupFree: Boolean): Boolean;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Insert(Item, DupFree)
    else if Item <> nil then
    begin
      Root := TBalancedTreeItemCast(Item);
      Root.Owner := TBalancedTreeCast(Self);
      Inc(FCount);
      Result := True;
    end
    else
      Result := False;
end;

{ TObjects }

constructor TObjects.Create(Initial, Delta: Integer; OwnsObjects: Boolean);
begin
  FDelta := Delta;
  FOwnsObjects := OwnsObjects;
  SetCapacity(Initial);
end;

procedure TObjects.Clear;
begin
  SetCapacity(0);
end;

function TObjects.Append(Item: TObject): Integer;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + FDelta);

  TObjectsCast(Self).Items[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TObjects.Exchange(Index1, Index2: Integer);
begin
  CheckIndex(Index1);
  CheckIndex(Index2);

  with TObjectsCast(Self) do
    CoreUtils.Exchange(Pointer(Items[Index1]), Pointer(Items[Index2]));
end;

function TObjects.Extract(Index: Integer): TObject;
begin
  CheckIndex(Index);

  with TObjectsCast(Self) do
  begin
    Result := Items[Index];
    Move(Items[Index + 1], Items[Index], (FCount - Index - 1) * SizeOf(TObject));
    Dec(FCount);
    Items[FCount] := nil;
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

procedure TObjects.Insert(Index: Integer; Item: TObject);
begin
  CheckIndex(Index);

  if FCount = FCapacity then
    SetCapacity(FCapacity + FDelta);

  with TObjectsCast(Self) do
  begin
    Move(Items[Index], Items[Index + 1], (FCount - Index) * SizeOf(TObject));
    Items[Index] := Item;
  end;
  Inc(FCount);
end;

procedure TObjects.SetCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FCount then
  begin
    CheckIndex(Value);
    if FOwnsObjects then
      for I := FCount - 1 downto Value do
        TObjectsCast(Self).Items[I].Free;
    FCount := Value;
  end;

  ReallocMem(TObjectsCast(Self).Items, Value * SizeOf(TObject));
  FCapacity := Value;
end;

procedure TObjects.SetCount(Value: Integer);
begin
  if FCount <> Value then
  begin
    SetCapacity(Value + (Value + FDelta - 1) mod FDelta);
    if Value > FCount then
      FillChar(TObjectsCast(Self).Items[FCount], (Value - FCount) * SizeOf(TObject), 0);
    FCount := Value;
  end;
end;

{ TCollectionItem }

procedure TCollectionItem.Extract;
begin
  with TCollectionItemCast(Self) do
    if Owner <> nil then
      Owner.Extract(Owner.IndexOf(Self));
  inherited;
end;

end.
