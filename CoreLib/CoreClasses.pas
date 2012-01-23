(*
    The Unified Environment Core Library

    Common shared object, list, balanced tree
    and other containers implementation

    Copyright (c) 2008-2010 The Unified Environment Laboratory
*)

unit Containers;

interface

uses
  Core, Exceptions;

type
  TObjectState = (osCreating, osLive, osDestroying);

  TSharedObject = class;
  TSharedClass = class of TSharedObject;

  TNotifyEvent = procedure(Sender: TObject) of object;

  TSharedObject = class
  private
    FRefCount, FUpdateCount: Integer;
    FObjectState: TObjectState;
    FOnUpdate: TNotifyEvent;
  protected
    procedure Updated; virtual;
  // properties
    property ObjectState: TObjectState read FObjectState;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function BeginUpdate: Integer;
    function EndUpdate: Integer;
    function Lock: Boolean;
    class function NewInstance: TObject; override;
    function Ref: Integer;
    function Release: Integer;
    function Unlock: Boolean;
  // properties
    property RefCount: Integer read FRefCount;
    property UpdateCount: Integer read FUpdateCount;
  // events
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  EContainerError = class(Exception);
  EListError = class(EContainerError);
  EBinTreeError = class(EContainerError);

  TList = class;        // sequential container
  TBinaryTree = class;  // key-preserved container

  TListItem = class(TSharedObject)
  private
  //  FOwner: TList;  { need to be placed in the same order in the descedants }
  //  FNext: TListItem;
  protected
    procedure Append(Owner: TList; Item: TListItem); overload;
    function AppendAll(Owner: TList; Item: TListItem): Cardinal; overload;
  {  function Exchange(Owner: TList; Item: TListItem;
      Siblings: Boolean = False): Cardinal; overload;}
    procedure Prepend(Owner: TList; Item: TListItem); overload;
    function PrependAll(Owner: TList; Item: TListItem): Cardinal; overload;
  public
    destructor Destroy; override;
    procedure Append(Item: TListItem); overload;
    function AppendAll(Item: TListItem): Cardinal; overload;
  //  function Exchange(Item: TListItem; Siblings: Boolean = False): Cardinal; overload;
    procedure Prepend(Item: TListItem); overload;
    function PrependAll(Item: TListItem): Cardinal; overload;
    procedure Remove;
    function RemoveAll: Cardinal;
  end;

  TList = class(TSharedObject)
  private
    FCount: Cardinal;
  //  FFirst, FLast: TListItem;  { need to be placed in the same order in the descedants }
  public
    destructor Destroy; override;
    function Append(Value: TListItem; Siblings: Boolean = False): Cardinal; overload;
    procedure Clear;
    procedure Exchange(Item1, Item2: TListItem; Siblings: Boolean = False);
    function Prepend(Value: TListItem; Siblings: Boolean = False): Cardinal; overload;
  // properties
    property Count: Cardinal read FCount;
  end;

  TBinaryTree = class
  end;

implementation

uses
  Windows;

type
  TCoreList = class;

  TCoreListItem = class(TListItem)
  private
    FOwner: TCoreList;
    FNext: TCoreListItem;
  end;

  TCoreList = class(TList)
  private
    FFirst, FLast: TCoreListItem;
  end;

{ TSharedObject }

destructor TSharedObject.Destroy;
asm
{$IFDEF Multithread}
   LOCK DEC [EAX].FUpdateCount
{$ELSE}
        DEC [EAX].FUpdateCount
{$ENDIF}
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
{$IFDEF Multithread}
   LOCK INC [EAX].FRefCount
   LOCK INC [EAX].FObjectState
{$ELSE}
        INC [EAX].FRefCount
        INC [EAX].FObjectState
{$ENDIF}
        JMP EndUpdate
end;

procedure TSharedObject.BeforeDestruction;
asm
{$IFDEF Multithread}
   LOCK INC [EAX].FObjectState
   LOCK INC [EAX].FUpdateCount
{$ELSE}
        INC [EAX].FObjectState
        INC [EAX].FUpdateCount
{$ENDIF}
end;

function TSharedObject.BeginUpdate: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, 1
{$IFDEF Multithread}
   LOCK XADD [EAX].FUpdateCount, EDX
{$ELSE}
        XADD [EAX].FUpdateCount, EDX
{$ENDIF}
        MOV EAX, EDX
@@exit:
end;

function TSharedObject.EndUpdate: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, EAX
        MOV EAX, -1
{$IFDEF Multithread}
   LOCK XADD [EDX].FUpdateCount, EAX
{$ELSE}
        XADD [EDX].FUpdateCount, EAX
{$ENDIF}
        DEC EAX
        JNZ @@exit
        MOV EAX, EDX
        CALL Updated
        XOR EAX, EAX
@@exit:
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
{$IFDEF Multithread}
   LOCK XADD [EAX].FRefCount, EDX
{$ELSE}
        XADD [EAX].FRefCount, EDX
{$ENDIF}
        MOV EAX, EDX
@@exit:
end;

function TSharedObject.Release: Integer;
asm
        TEST EAX, EAX
        JZ @@exit
        MOV EDX, EAX
        MOV EAX, -1
{$IFDEF Multithread}
   LOCK XADD [EDX].FRefCount, EAX
{$ELSE}
        XADD [EDX].FRefCount, EAX
{$ENDIF}
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
end;

procedure TSharedObject.Updated;
begin
  if Assigned(FOnUpdate) then
    if FObjectState <> osDestroying then
      FOnUpdate(Self)
    else
      FOnUpdate(nil);
end;

{ TListItem }

destructor TListItem.Destroy;
begin
  Remove;
  inherited;
end;

procedure TListItem.Append(Item: TListItem);
begin
  Lock;
  try
    Append(TCoreListItem(Self).FOwner, Item);
  finally
    Unlock;
  end;
end;

procedure TListItem.Append(Owner: TList; Item: TListItem);
begin
  BeginUpdate;
  try
    //
  finally
    EndUpdate;
  end;
end;

function TListItem.AppendAll(Item: TListItem): Cardinal;
begin
  Lock;
  try
    Result := AppendAll(TCoreListItem(Self).FOwner, Item);
  finally
    Unlock;
  end;
end;

function TListItem.AppendAll(Owner: TList; Item: TListItem): Cardinal;
var
  Nxt: TListItem;
begin
  BeginUpdate;
  try
    Result := 0;
  finally
    EndUpdate;
  end;
end;

{function TListItem.Exchange(Item: TListItem; Siblings: Boolean): Cardinal;
begin
  Result := Exchange(TCoreListItem(Self).FOwner, Item, Siblings);
end;

function TListItem.Exchange(Owner: TList; Item: TListItem;
  Siblings: Boolean): Cardinal;
begin

end;}

procedure TListItem.Prepend(Item: TListItem);
begin
  Lock;
  try
    Prepend(TCoreListItem(Self).FOwner, Item);
  finally
    Unlock;
  end;
end;

procedure TListItem.Prepend(Owner: TList; Item: TListItem);
begin
  BeginUpdate;
  try
    //
  finally
    EndUpdate;
  end;
end;

function TListItem.PrependAll(Item: TListItem): Cardinal;
begin
  Lock;
  try
    Result := PrependAll(TCoreListItem(Self).FOwner, Item);
  finally
    Unlock;
  end;
end;

function TListItem.PrependAll(Owner: TList; Item: TListItem): Cardinal;
begin
  BeginUpdate;
  try
    Result := 0;
  finally
    EndUpdate;
  end;
end;

procedure TListItem.Remove;
begin
  BeginUpdate;
  try
  finally
    EndUpdate;
  end;
end;

function TListItem.RemoveAll: Cardinal;
var
  List: TList;
  Next: TCoreListItem;
begin
  BeginUpdate;
  try
    List := TCoreListItem(Self).FOwner;
    if List <> nil then
    begin
      List.BeginUpdate;
      try
        Remove;
        Result := 1;

        Dec(List.FCount, Result);
      finally
        List.EndUpdate;
      end;
    end
    else
  finally
    EndUpdate;
  end;
end;

{ TList }

destructor TList.Destroy;
begin
  Clear;
  inherited;
end;

function MoveItems(Item: TListItem; Siblings: Boolean; Dest: TList;
  var Last: TListItem): Cardinal;
var
  Source: TList;
  P: TListItem;
begin
  Last := Item;
  if Siblings then
  begin
    Result := 0;
    repeat
//      Source := Last.SetOwner(Dest);
//      P := Last.GetNext(Source);
      if P = nil then
        Break;
      Last := P;
      Inc(Result);
    until False;
  end
  else
  begin
//    Source := Last.SetOwner(Dest);
    Result := 1;
  end;
  if Source <> nil then
    with Source do
    begin
//      FLast := Prev[Item];
//      if FLast = nil then
//        FFirst := nil;
      Dec(FCount, Result);
    end;
end;

{function TList.Append(Item, Value: TListItem; Siblings: Boolean): Cardinal;
var
  L: TListItem;
begin
  if Value <> nil then
  begin
    Result := MoveItems(Value, Siblings, Self, L);
    if Item <> nil then
      L.SetNext(Self, Item.SetNext(Self, Value));
    if FFirst = nil then
      FFirst := Value;
    while L <> nil do
    begin
      FLast := L;
      L := L.GetNext(Self);
    end;
    Inc(FCount, Result);
  end
  else
    Result := 0;
end;}

function TList.Append(Value: TListItem; Siblings: Boolean): Cardinal;
begin
{  if TCoreList(Self).FLast <> nil then
  begin
    Result := TCoreList(Self).FLast.Append(Value, Siblings);
    while TCoreList(Self).FLast.FNext <> nil do
      FLast := TCoreList(Self).FLast.FNext;
  end}
end;

procedure TList.Clear;
var
  Item: TListItem;
begin

{  Item := FFirst;
  while Item <> nil do
  begin
    Item.SetOwner(nil);
    Item := Item.SetNext(Self, nil);
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;}
end;

procedure TList.Exchange(Item1, Item2: TListItem; Siblings: Boolean);
begin
  raise EListError.Create; // TODO: Not implemented yet
end;

{function TList.Prepend(Item, Value: TListItem; Siblings: Boolean): Cardinal;
var
  L, P: TListItem;
begin
  if Value <> nil then
  begin
    Result := MoveItems(Value, Siblings, Self, L);
    P := Prev[Item];
    if P <> nil then
      L.SetNext(Self, P.SetNext(Self, Value))
    else
    begin
      FFirst := Value;
      L.SetNext(Self, Item);
    end;
    while L <> nil do
    begin
      FLast := L;
      L := L.GetNext(Self);
    end;
    Inc(FCount, Result);
  end
  else
    Result := 0;
end;}

function TList.Prepend(Value: TListItem; Siblings: Boolean): Cardinal;
begin
//  Result := Prepend(FFirst, Value, Siblings);
end;

{function TList.Remove(Item: TListItem; Siblings: Boolean): Cardinal;
var
  L: TListItem;
begin
  if Item <> nil then
    Result := MoveItems(Item, Siblings, nil, L)
  else
    Result := 0;
end;}

end.
