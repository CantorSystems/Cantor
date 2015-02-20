(*
    Lite Core Library (CoreLite mini)

    Platform-independent general purpose classes

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)
*)

unit CoreClasses;

interface

uses
  Windows, CoreUtils, CoreExceptions;

type
  PCoreObject = ^TCoreObject;
  TCoreObject = object
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Free;
  end;

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

  TBalancedTreeNode = class(TEnumerableItem)
  private         // Parent at the end for TListItem compliance
  { placeholder } // FLeft, FRight, FParent: TBalancedTreeNode;
  protected
    function RotateLeft(Parent, Item: TBalancedTreeNode): TBalancedTreeNode; virtual; abstract;
    function RotateRight(Parent, Item: TBalancedTreeNode): TBalancedTreeNode; virtual; abstract;
  public
    function Compare(Item: TBalancedTreeNode): Integer; virtual; abstract;
    function Depth: Integer;
    procedure Extract; override;
    function Find(Item: TBalancedTreeNode): TBalancedTreeNode;
    function Insert(Item: TBalancedTreeNode; FreeDuplicates: Boolean = True): Boolean;
    function Level: Integer;
    function Rebalance(FreeDuplicates: Boolean = True): TBalancedTreeNode;
    function Root: TBalancedTreeNode;
  end;

  TBalancedTree = class(TEnumerable)
  { placeholder } // FRoot: TBalancedTreeNode;
  public
    procedure Clear; override;
    function Depth: Integer;
    function Find(Item: TBalancedTreeNode): TBalancedTreeNode;
    function Insert(Item: TBalancedTreeNode; FreeDuplicates: Boolean = True): Boolean;
    function Rebalance(FreeDuplicates: Boolean = True): TBalancedTreeNode;
  end;

  TRedBlackTreeNode = class(TBalancedTreeNode)
  { placeholder } // FRed: Boolean;
  private
    procedure Invert;
  protected
    function RotateLeft(Parent, Item: TBalancedTreeNode): TBalancedTreeNode; override;
    function RotateRight(Parent, Item: TBalancedTreeNode): TBalancedTreeNode; override;
  end;

  TRedBlackTree = TBalancedTree;

  TArray = class(TIndexed)
  private
    FCapacity, FDelta: Integer;
  { placeholder } // FItems: Pointer;
    procedure Grow;
    procedure SetCount(Value: Integer);
  protected
    function Append: Integer; overload;
    procedure CheckIndex(Index: Integer); override;
    procedure Extract(Index: Integer); overload;
    procedure Insert(Index: Integer); overload;
    class function ItemSize: Integer; virtual;
    procedure SetCapacity(Value: Integer); virtual;
  public
    constructor Create(Initial, Delta: Integer); overload;
    function Append(const Item): Integer; overload;
    procedure Clear; override;
    procedure Extract(Index: Integer; var Item); overload;
    procedure Insert(Index: Integer; const Item); overload;
    function TranslateDelta: Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count write SetCount;
    property Delta: Integer read FDelta write FDelta;
  end;

  {TSortedArray = class(TArray)
  protected
    function Insert: Integer;
  end;}

  TDataHolder = class(TArray)
  private
  { placeholder } // FOwnsData: Boolean;
  protected
    class procedure FreeItem(const Item); virtual;
    procedure SetCapacity(Value: Integer); override;
  public
    constructor Create(Initial, Delta: Integer; OwnsItems: Boolean); overload;
    constructor Create(OwnsItems: Boolean); overload;
  end;

  TPointers = class(TDataHolder)
  public
    function Append(Item: Pointer): Integer; {$IFNDEF Lite} virtual; {$ENDIF}
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
    function Append(Item: Pointer): Integer; {$IFNDEF Lite} override; {$ENDIF}
    function Extract(Index: Integer): Pointer; {$IFNDEF Lite} override; {$ENDIF}
    procedure Insert(Index: Integer; Item: Pointer); {$IFNDEF Lite} override; {$ENDIF}
  end;

  TStringArray = class(TDataHolder)
  protected
    class procedure FreeItem(const Item); override;
    class function ItemSize: Integer; override;
  end;

  PLegacyStringRecArray = ^TLegacyStringRecArray;
  TLegacyStringRecArray = array[0..MaxInt div SizeOf(TLegacyStringRec) - 1] of TLegacyStringRec;

  TLegacyStringArray = class(TStringArray)
  private
  { hold } FItems: PLegacyStringRecArray;
  { hold } FOwnsData: Boolean;
  public
    function Append(const Item: TLegacyStringRec): Integer; overload;
    function Append(Str: PLegacyChar): Integer; overload;
    function Append(Str: PLegacyChar; Count: Integer): Integer; overload;
    function Extract(Index: Integer): TLegacyStringRec;
    function IndexOf(Str: PLegacyChar; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    function IndexOf(Str: PLegacyChar; Count: Integer; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    procedure Insert(Index: Integer; const Item: TLegacyStringRec); overload;
    procedure Insert(Index: Integer; Str: PLegacyChar); overload;
    procedure Insert(Index: Integer; Str: PLegacyChar; Count: Integer); overload;

    property Items: PLegacyStringRecArray read FItems;
    property OwnsData: Boolean read FOwnsData;
  end;

  PWideStringRecArray = ^TWideStringRecArray;
  TWideStringRecArray = array[0..MaxInt div SizeOf(TWideStringRec) - 1] of TWideStringRec;

  TWideStringArray = class(TStringArray)
  private
  { hold } FItems: PWideStringRecArray;
  { hold } FOwnsData: Boolean;
  public
    function Append(const Item: TWideStringRec): Integer; overload;
    function Append(Str: PWideChar): Integer; overload;
    function Append(Str: PWideChar; Count: Integer): Integer; overload;
    function Extract(Index: Integer): TWideStringRec;
    function IndexOf(Str: PWideChar; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    function IndexOf(Str: PWideChar; Count: Integer; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    procedure Insert(Index: Integer; const Item: TWideStringRec); overload;
    procedure Insert(Index: Integer; Str: PWideChar); overload;
    procedure Insert(Index: Integer; Str: PWideChar; Count: Integer); overload;

    property Items: PWideStringRecArray read FItems;
    property OwnsData: Boolean read FOwnsData;
  end;

  TCoreStringArray = TWideStringArray; // TODO: non-Unicode

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

  TBalancedTreeNodeCast = class(TBalancedTreeNode)
    Owner: TBalancedTreeCast;
    Left, Right, Parent: TBalancedTreeNodeCast;
  end;

  TBalancedTreeCast = class(TBalancedTree)
    Root: TBalancedTreeNodeCast;
  end;

  TRedBlackTreeCast = class;

  TRedBlackTreeNodeCast = class(TRedBlackTreeNode)
    Owner: TRedBlackTreeCast;
    Left, Right, Parent: TRedBlackTreeNodeCast;
    Red: Boolean;
  end;

  TRedBlackTreeCast = class(TRedBlackTree)
    Root: TRedBlackTreeNodeCast;
  end;

  TArrayCast = class(TArray)
    Items: PLegacyChar;
  end;

  TDataHolderCast = class(TDataHolder)
    Items: PLegacyChar;
    OwnsData: Boolean;
  end;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  TPointersCast = class(TPointers)
    Items: PPointerArray;
    OwnsItems: Boolean;
  end;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..MaxInt div SizeOf(TObject) - 1] of TObject;

  TObjectsCast = class(TObjects)
    Items: PObjectArray;
    OwnsObjects: Boolean;
  end;

  TCollectionCast = class;

  TCollectionItemCast = class(TCollectionItem)
    Owner: TCollection;
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

{ TCoreObject }

constructor TCoreObject.Create;
begin
end;

destructor TCoreObject.Destroy;
begin
end;

procedure TCoreObject.Free;
var
  Instance: PCoreObject;
begin
  if @Self <> nil then
  begin
    Instance := @Self;
    Dispose(Instance, Destroy);
  end;
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

{ TBalancedTreeNode }

function TBalancedTreeNode.Depth: Integer;

function DepthFrom(Item: TBalancedTreeNodeCast; Value: Integer): Integer;
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
  Result := DepthFrom(TBalancedTreeNodeCast(Self), 0);
end;

procedure TBalancedTreeNode.Extract;
begin
  inherited; // TODO
end;

function TBalancedTreeNode.Find(Item: TBalancedTreeNode): TBalancedTreeNode;
var
  Cmp: Integer;
begin
  Result := Root;
  while Result <> nil do
  begin
    Cmp := Result.Compare(Item);
    if Cmp < 0 then
      Result := TBalancedTreeNodeCast(Result).Left
    else if Cmp > 0 then
      Result := TBalancedTreeNodeCast(Result).Right
    else
      Break;
  end;
end;

function TBalancedTreeNode.Insert(Item: TBalancedTreeNode; FreeDuplicates: Boolean): Boolean;
var
  Cmp: Integer;
  P: TBalancedTreeNodeCast;
begin
  if Item <> nil then
  begin
    Item.Extract;
    P := TBalancedTreeNodeCast(Root);

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

    Inc(TBalancedTreeNodeCast(Self).Owner.FCount);
    Result := True;
  end
  else
    Result := False;
end;

function TBalancedTreeNode.Level: Integer;
var
  P: TBalancedTreeNodeCast;
begin
  Result := 0;
  P := TBalancedTreeNodeCast(Self).Parent;
  while P <> nil do
  begin
    Inc(Result);
    P := P.Parent;
  end;
  Inc(Result); // small core
end;

function TBalancedTreeNode.Rebalance(FreeDuplicates: Boolean): TBalancedTreeNode;
begin
  Result := nil; // TODO
end;

function TBalancedTreeNode.Root: TBalancedTreeNode;
begin
  with TBalancedTreeNodeCast(Self) do
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

procedure ClearItem(Item: TBalancedTreeNodeCast);
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

function TBalancedTree.Find(Item: TBalancedTreeNode): TBalancedTreeNode;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Find(Item)
    else
      Result := nil;
end;

function TBalancedTree.Insert(Item: TBalancedTreeNode; FreeDuplicates: Boolean): Boolean;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Insert(Item, FreeDuplicates)
    else if Item <> nil then
    begin
      Root := TBalancedTreeNodeCast(Item);
      Root.Owner := TBalancedTreeCast(Self);
      Inc(FCount);
      Result := True;
    end
    else
      Result := False;
end;

function TBalancedTree.Rebalance(FreeDuplicates: Boolean): TBalancedTreeNode;
begin
  with TBalancedTreeCast(Self) do
    if Root <> nil then
      Result := Root.Rebalance(FreeDuplicates)
    else
      Result := nil; // no duplicates
end;

{ TRedBlackTreeNode }

procedure TRedBlackTreeNode.Invert;
begin
  with TRedBlackTreeNodeCast(Self) do
  begin
    Red := not Red;
    if Left <> nil then
      Left.Invert;
    if Right <> nil then
      Right.Invert;
  end;
end;

function TRedBlackTreeNode.RotateLeft(Parent, Item: TBalancedTreeNode): TBalancedTreeNode;
begin
  TRedBlackTreeNodeCast(Parent).Left := TRedBlackTreeNodeCast(Item);
  TRedBlackTreeNodeCast(Item).Parent := TRedBlackTreeNodeCast(Parent);
  TRedBlackTreeNodeCast(Item).Owner := TRedBlackTreeNodeCast(Parent).Owner;
  Result := Parent;
end;

function TRedBlackTreeNode.RotateRight(Parent, Item: TBalancedTreeNode): TBalancedTreeNode;
begin
  TRedBlackTreeNodeCast(Parent).Right := TRedBlackTreeNodeCast(Item);
  TRedBlackTreeNodeCast(Item).Parent := TRedBlackTreeNodeCast(Parent);
  TRedBlackTreeNodeCast(Item).Owner := TRedBlackTreeNodeCast(Parent).Owner;
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

function TArray.Append(const Item): Integer;
var
  Bytes: Integer;
begin
  Result := Append;
  Bytes := ItemSize;
  Move(Item, TArrayCast(Self).Items[Result * Bytes], Bytes);
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

procedure TArray.Extract(Index: Integer; var Item);
var
  Bytes: Integer;
begin
  CheckIndex(Index);
  Bytes := ItemSize;
  Move(TArrayCast(Self).Items[Index * Bytes], Item, Bytes);
  Extract(Index);
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

procedure TArray.Insert(Index: Integer; const Item);
var
  Bytes: Integer;
begin
  Insert(Index);
  Bytes := ItemSize;
  Move(Item, TArrayCast(Self).Items[Index * Bytes], Bytes);
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
  Bytes, Delta: Integer;
begin
  if FCount <> Value then
  begin
    if FDelta <> 0 then
    begin
      Delta := TranslateDelta;
      SetCapacity(Value + (Value + Delta - 1) mod Delta);
    end
    else
      SetCapacity(Value);
    Bytes := ItemSize;
    if Value > FCount then
      FillChar(TArrayCast(Self).Items[FCount * Bytes], (Value - FCount) * Bytes, 0);
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

{ TDataHolder }

constructor TDataHolder.Create(Initial, Delta: Integer; OwnsItems: Boolean);
begin
  inherited Create(Initial, Delta);
  TDataHolderCast(Self).OwnsData := OwnsItems;
end;

constructor TDataHolder.Create(OwnsItems: Boolean);
begin
  TDataHolderCast(Self).OwnsData := OwnsItems;
end;

class procedure TDataHolder.FreeItem(const Item);
begin
  FreeMem(Pointer(Item));
end;

procedure TDataHolder.SetCapacity(Value: Integer);
var
  I, Bytes: Integer;
begin
  if Value < FCount then
  begin
    CheckIndex(Value); // for negative values
    if TDataHolderCast(Self).OwnsData then
    begin
      Bytes := ItemSize;
      for I := FCount - 1 downto Value do
        FreeItem(TDataHolderCast(Self).Items[I * Bytes]);
    end;
  end;
  inherited;
end;

{ TPointers }

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


function TCollection.Append(Item: Pointer): Integer;
begin
  if Item <> nil then
    TCollectionItem(Item).Extract;
  Result := inherited Append(Item);
  if Item <> nil then
    TCollectionItemCast(Item).Owner := Self;
end;

function TCollection.Extract(Index: Integer): Pointer;
begin
  Result := inherited Extract(Index);
  if Result <> nil then
    TCollectionItemCast(Result).Owner := nil;
end;

procedure TCollection.Insert(Index: Integer; Item: Pointer);
begin
  if Item <> nil then
    TCollectionItem(Item).Extract;
  inherited Insert(Index, Item);
  if Item <> nil then
    TCollectionItemCast(Item).Owner := Self;
end;

{ TStringArray }

class procedure TStringArray.FreeItem(const Item);
begin
  FreeMem(TCoreStringRec(Item).Value);
end;

class function TStringArray.ItemSize: Integer;
begin
  Result := SizeOf(TCoreStringRec);
end;

{ TLegacyStringArray }

function TLegacyStringArray.Append(const Item: TLegacyStringRec): Integer;
begin
  Result := inherited Append;
  FItems[Result] := Item;
end;

function TLegacyStringArray.Append(Str: PLegacyChar): Integer;
begin
  Result := Append(Str, StrLen(Str));
end;

function TLegacyStringArray.Append(Str: PLegacyChar; Count: Integer): Integer;
begin
  Result := inherited Append;
  with FItems[Result] do
  begin
    Value := Str;
    Length := Count;
  end;
end;

function TLegacyStringArray.Extract(Index: Integer): TLegacyStringRec;
begin
  CheckIndex(Index);
  Result := FItems[Index];
  Extract(Index);
end;

function TLegacyStringArray.IndexOf(Str: PLegacyChar; IgnoreFlags, Locale: LongWord): Integer;
begin
  Result := IndexOf(Str, StrLen(Str), IgnoreFlags, Locale);
end;

function TLegacyStringArray.IndexOf(Str: PLegacyChar; Count: Integer;
  IgnoreFlags, Locale: LongWord): Integer;
begin
  for Result := 0 to Count - 1 do
    with FItems[Result] do
      if StrComp(Value, Length, Str, Count, IgnoreFlags, Locale) = 0 then
        Exit;
  Result := -1;
end;

procedure TLegacyStringArray.Insert(Index: Integer; const Item: TLegacyStringRec);
begin
  inherited Insert(Index);
  FItems[Index] := Item;
end;

procedure TLegacyStringArray.Insert(Index: Integer; Str: PLegacyChar);
begin
  Insert(Index, Str, StrLen(Str));
end;

procedure TLegacyStringArray.Insert(Index: Integer; Str: PLegacyChar; Count: Integer);
begin
  inherited Insert(Index);
  with FItems[Index] do
  begin
    Value := Str;
    Length := Count;
  end;
end;

{ TWideStringArray }

function TWideStringArray.Append(const Item: TWideStringRec): Integer;
begin
  Result := inherited Append;
  FItems[Result] := Item;
end;

function TWideStringArray.Append(Str: PWideChar): Integer;
begin
  Result := Append(Str, WideStrLen(Str));
end;

function TWideStringArray.Append(Str: PWideChar; Count: Integer): Integer;
begin
  Result := inherited Append;
  with FItems[Result] do
  begin
    Value := Str;
    Length := Count;
  end;
end;

function TWideStringArray.Extract(Index: Integer): TWideStringRec;
begin
  CheckIndex(Index);
  Result := FItems[Index];
  Extract(Index);
end;

function TWideStringArray.IndexOf(Str: PWideChar; IgnoreFlags, Locale: LongWord): Integer;
begin
  Result := IndexOf(Str, WideStrLen(Str), IgnoreFlags, Locale);
end;

function TWideStringArray.IndexOf(Str: PWideChar; Count: Integer;
  IgnoreFlags, Locale: LongWord): Integer;
begin
  for Result := 0 to Count - 1 do
    with FItems[Result] do
      if WideStrComp(Value, Length, Str, Count, IgnoreFlags, Locale) = 0 then
        Exit;
  Result := -1;
end;

procedure TWideStringArray.Insert(Index: Integer; const Item: TWideStringRec);
begin
  inherited Insert(Index);
  FItems[Index] := Item;
end;

procedure TWideStringArray.Insert(Index: Integer; Str: PWideChar);
begin
  Insert(Index, Str, WideStrLen(Str));
end;

procedure TWideStringArray.Insert(Index: Integer; Str: PWideChar; Count: Integer);
begin
  inherited Insert(Index);
  with FItems[Index] do
  begin
    Value := Str;
    Length := Count;
  end;
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
