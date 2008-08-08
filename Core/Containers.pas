(*
    The Unified Environment, legacy Win32 core

    Core list, balanced tree and other containers implementation

    Copyright (c) 2008-2009 The Unified Environment Laboratory
*)

unit Containers;

interface

uses
  Core, Exceptions;

type
  EList = class(Exception);

  TListItem = class;
  TList = class;

  IListItem = interface
    function GetNext(Sender: TList): IListItem;
    function GetOwner: TList;
    function SetNext(Sender: TList; Value: IListItem): IListItem;
    function SetOwner(Value: TList): TList;
  end;

  TListItem = class(TInterfacedObject, IListItem)
  private
    //FOwner: TList;  { need to be placed in same order in the descedants }
    //FNext: IListItem;
  protected
    procedure CheckList(Value: TList);
  // IItem
    function GetNext(Sender: TList): IListItem;
    function GetOwner: TList;
    function SetNext(Sender: TList; Value: IListItem): IListItem;
    function SetOwner(Value: TList): TList;
  public
    destructor Destroy; override;
    procedure Append(Item: IListItem; Siblings: Boolean = False);
    procedure Exchange(Item: IListItem; Siblings: Boolean = False);
    procedure Prepend(Item: IListItem; Siblings: Boolean = False);
    procedure Remove(Siblings: Boolean = False);
  end;

  TList = class
  private
    FCount: Integer;
    FFirst, FLast: IListItem;
    function GetNext(Item: IListItem): IListItem;
    function GetPrevious(Item: IListItem): IListItem;
    procedure Update(LastItem: IListItem);
  public
    destructor Destroy; override;
    function Append(Item, Value: IListItem; Siblings: Boolean = False): Integer; overload;
    function Append(Value: IListItem; Siblings: Boolean = False): Integer; overload;
    procedure Clear;
    procedure Exchange(Item1, Item2: IListItem; Siblings: Boolean = False);
    function Prepend(Item, Value: IListItem; Siblings: Boolean = False): Integer; overload;
    function Prepend(Value: IListItem; Siblings: Boolean = False): Integer; overload;
    function Remove(Item: IListItem; Siblings: Boolean = False): Integer;
  // properties
    property Count: Integer read FCount;
    property First: IListItem read FFirst;
    property Last: IListItem read FLast;
    property Next[Item: IListItem]: IListItem read GetNext;
    property Previous[Item: IListItem]: IListItem read GetPrevious;
  end;

implementation

type
  TListItemHack = class(TListItem)
  private
    FOwner: TList;
    FNext: IListItem;
  end;

{ TListItem }

destructor TListItem.Destroy;
begin
  Remove;
  inherited;
end;

procedure TListItem.Append(Item: IListItem; Siblings: Boolean);
begin
  with TListItemHack(Self) do
  begin
    CheckList(FOwner);
    FOwner.Append(Self, Item, Siblings);
  end;
end;

procedure TListItem.CheckList(Value: TList);
begin
  with TListItemHack(Self) do
    if (FOwner = nil) or (FOwner <> Value) then
      raise EList.Create; // TODO: item out of list
end;

procedure TListItem.Exchange(Item: IListItem; Siblings: Boolean);
begin
  with TListItemHack(Self) do
  begin
    CheckList(FOwner);
    FOwner.Exchange(Self, Item, Siblings);
  end;
end;

function TListItem.GetNext(Sender: TList): IListItem;
begin
  Result := TListItemHack(Self).FNext;
end;

function TListItem.GetOwner: TList;
begin
  Result := TListItemHack(Self).FOwner;
end;

procedure TListItem.Prepend(Item: IListItem; Siblings: Boolean);
begin
  with TListItemHack(Self) do
  begin
    CheckList(FOwner);
    FOwner.Prepend(Self, Item, Siblings);
  end;
end;

procedure TListItem.Remove(Siblings: Boolean);
begin
  with TListItemHack(Self) do
  begin
    CheckList(FOwner);
    FOwner.Remove(Self, Siblings);
  end;
end;

function TListItem.SetNext(Sender: TList; Value: IListItem): IListItem;
begin
  with TListItemHack(Self) do
  begin
    Result := FNext;
    FNext := TListItemHack(Value);
  end;
end;

function TListItem.SetOwner(Value: TList): TList;
begin
  with TListItemHack(Self) do
  begin
    Result := FOwner;
    FOwner := Value;
  end;
end;

{ TList }

destructor TList.Destroy;
begin
  Clear;
  inherited;
end;

function MoveItems(Item: IListItem; Siblings: Boolean; Dest: TList;
  var Last: IListItem): Integer;
var
  Source: TList;
  P: IListItem;
begin
  Last := Item;
  if Siblings then
  begin
    Result := 0;
    repeat
      Source := Last.SetOwner(Dest);
      P := Last.GetNext(Source);
      if P = nil then
        Break;
      Last := P;
      Inc(Result);
    until False;
  end
  else
  begin
    Source := Last.SetOwner(Dest);
    Result := 1;
  end;
  if Source <> nil then
    with Source do
    begin
      FLast := Previous[Item];
      if FLast = nil then
        FFirst := nil;
      Dec(FCount, Result);
    end;
end;

function TList.Append(Item, Value: IListItem; Siblings: Boolean): Integer;
var
  L: IListItem;
begin
  if Value <> nil then
  begin
    Result := MoveItems(Value, Siblings, Self, L);
    L.SetNext(Self, Item.SetNext(Self, Value));
    if FFirst = nil then
      FFirst := Value;
    Update(L);
    Inc(FCount, Result);
  end
  else
    Result := 0;
end;

function TList.Append(Value: IListItem; Siblings: Boolean): Integer;
begin
  Result := Append(FLast, Value, Siblings);
end;

procedure TList.Clear;
var
  Item: IListItem;
begin
  Item := FFirst;
  while Item <> nil do
  begin
    Item.SetOwner(nil);
    Item := Item.SetNext(Self, nil);
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

procedure TList.Exchange(Item1, Item2: IListItem; Siblings: Boolean);
begin
  raise EList.Create; // TODO: Not implemented yet
end;

function TList.GetNext(Item: IListItem): IListItem;
begin
  Result := Item.GetNext(Self);
end;

function TList.GetPrevious(Item: IListItem): IListItem;
var
  Next: IListItem;
begin
  Result := FFirst;
  while Result <> nil do
  begin
    Next := Result.GetNext(Self);
    if Next = Item then
      Exit;
    Result := Next;
  end;
end;

function TList.Prepend(Item, Value: IListItem; Siblings: Boolean): Integer;
var
  L, P: IListItem;
begin
  if Value <> nil then
  begin
    Result := MoveItems(Value, Siblings, Self, L);
    P := Previous[Item];
    if P <> nil then
      L.SetNext(Self, P.SetNext(Self, Value))
    else
    begin
      FFirst := Value;
      L.SetNext(Self, Item);
    end;
    Update(L);
    Inc(FCount, Result);
  end
  else
    Result := 0;
end;

function TList.Prepend(Value: IListItem; Siblings: Boolean): Integer;
begin
  Result := Prepend(FLast, Value, Siblings);
end;

function TList.Remove(Item: IListItem; Siblings: Boolean): Integer;
var
  L: IListItem;
begin
  if Item <> nil then
    Result := MoveItems(Item, Siblings, nil, L)
  else
    Result := 0;
end;

procedure TList.Update(LastItem: IListItem);
begin
  repeat
    FLast := LastItem;
    LastItem := FLast.GetNext(Self);
    if LastItem = nil then
      Break;
  until False;
end;

end.
