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
    procedure CheckIndex(Index: Integer); virtual;
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
  private         // Parent at the end for TListItem compliance
  { placeholder } // FLeft, FRight, FParent: TBalancedTreeItem;
  protected
    function RotateLeft(Parent, Item: TBalancedTreeItem): TBalancedTreeItem; virtual; abstract;
    function RotateRight(Parent, Item: TBalancedTreeItem): TBalancedTreeItem; virtual; abstract;
  public
    function Compare(Item: TBalancedTreeItem): Integer; virtual; abstract;
    function Depth: Integer;
    procedure Extract; override;
    function Find(Item: TBalancedTreeItem): TBalancedTreeItem;
    function Insert(Item: TBalancedTreeItem; FreeDuplicates: Boolean = True): Boolean;
    function Level: Integer;
    function Rebalance(FreeDuplicates: Boolean = True): TBalancedTreeItem;
    function Root: TBalancedTreeItem;
  end;

  TBalancedTree = class(TEnumerable)
  { placeholder } // FRoot: TBalancedTreeItem;
  public
    procedure Clear; override;
    function Depth: Integer;
    function Find(Item: TBalancedTreeItem): TBalancedTreeItem;
    function Insert(Item: TBalancedTreeItem; FreeDuplicates: Boolean = True): Boolean;
    function Rebalance(FreeDuplicates: Boolean = True): TBalancedTreeItem;
  end;

  TRedBlackTreeItem = class(TBalancedTreeItem)
  { placeholder } // FRed: Boolean;
  private
    procedure Invert;
  protected
    function RotateLeft(Parent, Item: TBalancedTreeItem): TBalancedTreeItem; override;
    function RotateRight(Parent, Item: TBalancedTreeItem): TBalancedTreeItem; override;
  end;

  TRedBlackTree = TBalancedTree;

  TArray = class(TIndexed)
  private
    FCapacity, FDelta: Integer;
  { placeholder } // FItems: Pointer;
    procedure Grow;
    procedure SetCount(Value: Integer);
  protected
    function Append: Integer;
    procedure CheckIndex(Index: Integer); override;
    procedure Extract(Index: Integer);
    procedure Insert(Index: Integer);
    class function ItemSize: Integer; virtual;
    procedure SetCapacity(Value: Integer); virtual;
  public
    constructor Create(Initial, Delta: Integer); overload;
    procedure Clear; override;
    function TranslateDelta: Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count write SetCount;
    property Delta: Integer read FDelta write FDelta;
  end;

  {TSortedArray = class(TArray)
  protected
    function Insert: Integer;
  end;}

  TPointers = class(TArray)
  private
  { placeholder } // FOwnsObjects: Boolean;
  protected
    class procedure FreeItem(const Item); virtual;
    procedure SetCapacity(Value: Integer); override;
  public
    constructor Create(Initial, Delta: Integer; OwnsObjects: Boolean = False); overload;
    constructor Create(OwnsObjects: Boolean = False); overload;
    function Append(Item: Pointer): Integer; {$IFNDEF Lite} virtual; {$ENDIF} overload;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): Pointer; {$IFNDEF Lite} virtual; {$ENDIF}
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer); {$IFNDEF Lite} virtual; {$ENDIF}
  end;

  TObjects = class(TPointers)
  protected
    class procedure FreeItem(const Item); override;
  end;

  TCollectionItem = class(TContainedItem)
  public
    procedure Extract; override;
  end;

  TCollection = class(TObjects)
  public
    function Append(Item: TObject): Integer; {$IFNDEF Lite} override; {$ENDIF}
    function Extract(Index: Integer): TObject; {$IFNDEF Lite} override; {$ENDIF}
    procedure Insert(Index: Integer; Item: TObject); {$IFNDEF Lite} override; {$ENDIF}
  end;

  TCRC32Table = array[0..$FF] of LongWord;

  TCRC32 = class
  private
    FTable: TCRC32Table;
  public
    constructor Create(Polynomial: LongWord = $EDB88320);
    function CalcChecksum(const Buf; Count: Integer; Initial: LongWord = 0): LongWord;
    property Table: TCRC32Table read FTable;
  end;

{ Exceptions }

  EContainer = class(Exception)         
  private
    FContainer: TObject;
  public
    property Container: TObject read FContainer;
  end;

  EIndex = class(EContainer)
  private
    FIndex: Integer;
  public
    constructor Create(Container: TObject; Index, LowBound, HighBound: Integer); overload;
    constructor Create(Container: TObject; Index: Integer); overload;
    property Index: Integer read FIndex;
  end;

  ERange = class(EIndex)
  private
    FCount: Integer;
  public
    constructor Create(Container: TObject; Index, Count, LowBound, HighBound: Integer);
    property Count: Integer read FCount;
  end;

{$IFNDEF Lite}
  EFixed = class(EContainer)
  public
    constructor Create(Container: TObject; Capacity: Integer);
  end;
{$ENDIF}

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
    Left, Right, Parent: TBalancedTreeItemCast;
  end;

  TBalancedTreeCast = class(TBalancedTree)
    Root: TBalancedTreeItemCast;
  end;

  TRedBlackTreeCast = class;

  TRedBlackTreeItemCast = class(TRedBlackTreeItem)
    Owner: TRedBlackTreeCast;
    Left, Right, Parent: TRedBlackTreeItemCast;
    Red: Boolean;
  end;

  TRedBlackTreeCast = class(TRedBlackTree)
    Root: TRedBlackTreeItemCast;
  end;

  TArrayCast = class(TArray)
    Items: PLegacyChar;
  end;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  TPointersCast = class(TPointers)
    Items: PPointerArray;
    OwnsObjects: Boolean;
  end;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..MaxInt div SizeOf(TObject) - 1] of TObject;

  TObjectsCast = class(TObjects)
    Items: PObjectArray;
    OwnsObjects: Boolean;
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
  FIndex := Index;
end;

constructor EIndex.Create(Container: TObject; Index: Integer);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sIndexOfNull, [@ClassName, Index]);
  FContainer := Container;
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
  FIndex := Index;
  FCount := Count;
end;

{ EFixed }

{$IFNDEF Lite}
constructor EFixed.Create(Container: TObject; Capacity: Integer);
var
  ClassName: TClassName;
begin
  FriendlyClassName(ClassName, Container);
  inherited Create(sFixedCapacity, [@ClassName, Capacity]);
  FContainer := Container;
end;
{$ENDIF}

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
    raise EIndex.Create(Self, Index, 0, FCount - 1);
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

  Inc(FCount);
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

function TBalancedTreeItem.Depth: Integer;

function DepthFrom(Item: TBalancedTreeItemCast; Value: Integer): Integer;
begin
  Inc(Value);
  if Item.Left <> nil then
    Value := DepthFrom(Item.Left, Value);
  if Item.Right <> nil then
  begin
    Result := DepthFrom(Item.Right, Value);
    if Result > Value then
      Exit;
  end;
  Result := Value;
end;

begin
  Result := DepthFrom(TBalancedTreeItemCast(Self), 0);
end;

procedure TBalancedTreeItem.Extract;
begin
  inherited; // TODO
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

function TBalancedTreeItem.Insert(Item: TBalancedTreeItem; FreeDuplicates: Boolean): Boolean;
var
  Cmp: Integer;
  P: TBalancedTreeItemCast;
begin
  if Item <> nil then
  begin
    Item.Extract;
    P := TBalancedTreeItemCast(Root);

    while P <> nil do
    begin
      Cmp := P.Compare(Item);
      if Cmp < 0 then
        if P.Left <> nil then
          P := P.Left
        else
        begin
          RotateLeft(P, Item);
          Break;
        end
      else if Cmp > 0 then
        if P.Right <> nil then
          P := P.Right
        else
        begin
          RotateRight(P, Item);
          Break;
        end
      else
      begin
        if FreeDuplicates then
          Item.Free;
        Break;
      end;
    end;

    Inc(TBalancedTreeItemCast(Self).Owner.FCount);
    Result := True;
  end
  else
    Result := False;
end;

function TBalancedTreeItem.Level: Integer;
var
  P: TBalancedTreeItemCast;
begin
  Result := 0;
  P := TBalancedTreeItemCast(Self).Parent;
  while P <> nil do
  begin
    Inc(Result);
    P := P.Parent;
  end;
  Inc(Result); // small core
end;

function TBalancedTreeItem.Rebalance(FreeDuplicates: Boolean): TBalancedTreeItem;
begin
  Result := nil; // TODO
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
  with Item do
  begin
    Owner := nil;
    Parent := nil;
    if Left <> nil then
      ClearItem(Left);
    if Right <> nil then
      ClearItem(Right);
    Free;
  end;
end;

begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
     ClearItem(Root);  // fast Clear
  FCount := 0;
end;

function TBalancedTree.Depth: Integer;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Depth
    else
      Result := 0;
end;

function TBalancedTree.Find(Item: TBalancedTreeItem): TBalancedTreeItem;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Find(Item)
    else
      Result := nil;
end;

function TBalancedTree.Insert(Item: TBalancedTreeItem; FreeDuplicates: Boolean): Boolean;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Insert(Item, FreeDuplicates)
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

function TBalancedTree.Rebalance(FreeDuplicates: Boolean): TBalancedTreeItem;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Rebalance(FreeDuplicates)
    else
      Result := nil; // no duplicates
end;

{ TRedBlackTreeItem }

procedure TRedBlackTreeItem.Invert;
begin
  with TRedBlackTreeItemCast(Self) do
  begin
    Red := not Red;
    if Left <> nil then
      Left.Invert;
    if Right <> nil then
      Right.Invert;
  end;
end;

function TRedBlackTreeItem.RotateLeft(Parent, Item: TBalancedTreeItem): TBalancedTreeItem;
begin
  TRedBlackTreeItemCast(Parent).Left := TRedBlackTreeItemCast(Item);
  TRedBlackTreeItemCast(Item).Parent := TRedBlackTreeItemCast(Parent);
  TRedBlackTreeItemCast(Item).Owner := TRedBlackTreeItemCast(Parent).Owner;
  Result := Parent;
end;

function TRedBlackTreeItem.RotateRight(Parent, Item: TBalancedTreeItem): TBalancedTreeItem;
begin
  TRedBlackTreeItemCast(Parent).Right := TRedBlackTreeItemCast(Item);
  TRedBlackTreeItemCast(Item).Parent := TRedBlackTreeItemCast(Parent);
  TRedBlackTreeItemCast(Item).Owner := TRedBlackTreeItemCast(Parent).Owner;
  Result := Parent;
end;

{ TArray }

constructor TArray.Create(Initial, Delta: Integer);
begin
  FDelta := Delta;
  SetCapacity(Initial);
end;

function TArray.Append: Integer;
begin
  Grow;
  Result := FCount;
  Inc(FCount);
end;

procedure TArray.CheckIndex(Index: Integer);
begin
  if TArrayCast(Self).Items = nil then
    raise EIndex.Create(Self, Index)
  else
    inherited;
end;

procedure TArray.Clear;
begin
  SetCount(0);
end;

procedure TArray.Extract(Index: Integer);
var
  Idx, S: Integer;
begin
{$IFNDEF Lite}
  CheckIndex(Index);
{$ENDIF}
  S := ItemSize;
  Idx := Index * S;
  with TArrayCast(Self) do
  begin
    Move(Items[Idx + S], Items[Idx], (FCount - Index - 1) * S);
    Dec(FCount);
  end;
end;

procedure TArray.Grow;
begin
  if (FCapacity = 0) or (FCapacity = FCount) then
  begin
  {$IFNDEF Lite}
    if FDelta = 0 then
      raise EFixed.Create(Self, FCapacity);
  {$ENDIF}
    SetCapacity(FCapacity + TranslateDelta);
  end;
end;

procedure TArray.Insert(Index: Integer);
var
  Idx, S: Integer;
begin
  CheckIndex(Index);
  Grow;
  S := ItemSize;
  Idx := Index * S;
  with TArrayCast(Self) do
    Move(Items[Idx], Items[Idx + S], (FCount - Index) * S);
  Inc(FCount);
end;

class function TArray.ItemSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

procedure TArray.SetCapacity(Value: Integer);
begin
  ReallocMem(TArrayCast(Self).Items, Value * ItemSize);
  FCapacity := Value;
  if FCount > FCapacity then
    FCount := FCapacity;
end;

procedure TArray.SetCount(Value: Integer);
var
  D, S: Integer;
begin
  if FCount <> Value then
  begin
    if FDelta <> 0 then
    begin
      D := TranslateDelta;
      SetCapacity(Value + (Value + D - 1) mod D);
    end
    else
      SetCapacity(Value);
    S := ItemSize;
    if Value > FCount then
      FillChar(TArrayCast(Self).Items[FCount * S], (Value - FCount) * S, 0);
    FCount := Value;
  end;
end;

function TArray.TranslateDelta: Integer;
begin
  if FDelta < 0 then
  begin
    Result := FCapacity div Abs(FDelta);
  {$IFNDEF Lite}
    if Result = 0 then
      Result := Abs(FDelta)
  {$ENDIF}
  end
  else
    Result := FDelta;
end;

{ TSortedArray }

{function TSortedArray.Insert: Integer;
begin

end;}

{ TPointers }

constructor TPointers.Create(Initial, Delta: Integer; OwnsObjects: Boolean);
begin
  inherited Create(Initial, Delta);
  TPointersCast(Self).OwnsObjects := OwnsObjects;
end;

constructor TPointers.Create(OwnsObjects: Boolean);
begin
  TPointersCast(Self).OwnsObjects := OwnsObjects;
end;

function TPointers.Append(Item: Pointer): Integer;
begin
  Result := inherited Append;
  TPointersCast(Self).Items[Result] := Item;
end;

procedure TPointers.Exchange(Index1, Index2: Integer);
begin
  CheckIndex(Index1);
  CheckIndex(Index2);
  with TPointersCast(Self) do
    CoreUtils.Exchange(Items[Index1], Items[Index2]);
end;

function TPointers.Extract(Index: Integer): Pointer;
begin
  CheckIndex(Index);
  Result := TPointersCast(Self).Items[Index];
  inherited Extract(Index);
  TPointersCast(Self).Items[FCount] := nil;
end;

class procedure TPointers.FreeItem(const Item);
begin
  FreeMem(Pointer(Item));
end;

function TPointers.IndexOf(Item: Pointer): Integer;
begin
  for Result := 0 to FCount - 1 do
    if TPointersCast(Self).Items[Result] = Item then
      Exit;
  Result := -1;
end;

procedure TPointers.Insert(Index: Integer; Item: Pointer);
begin
  inherited Insert(Index);
  TPointersCast(Self).Items[Index] := Item;
end;

procedure TPointers.SetCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FCount then
  begin
    CheckIndex(Value); // for negative values
    if TPointersCast(Self).OwnsObjects then
      for I := FCount - 1 downto Value do
        FreeItem(TPointersCast(Self).Items[I]);
  end;
  inherited;
end;

{ TObjects }

class procedure TObjects.FreeItem(const Item);
begin
  TObject(Item).Free;
end;

{ TCollectionItem }

procedure TCollectionItem.Extract;
begin
  with TCollectionItemCast(Self) do
    if Owner <> nil then
      Owner.Extract(Owner.IndexOf(Self));
  inherited;
end;

{ TCollection }

function TCollection.Append(Item: TObject): Integer;
begin
  if Item <> nil then
    TCollectionItem(Item).Extract;
  Result := inherited Append(Item);
end;

function TCollection.Extract(Index: Integer): TObject;
begin
  Result := inherited Extract(Index);
  if Result <> nil then
    TCollectionItem(Result).Extract;
end;

procedure TCollection.Insert(Index: Integer; Item: TObject);
begin
  if Item <> nil then
    TCollectionItem(Item).Extract;
  inherited Insert(Index, Item);
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
