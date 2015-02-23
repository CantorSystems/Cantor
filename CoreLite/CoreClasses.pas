(*
    Lite Core Library (CoreLite mini)

    Platform-independent general purpose classes

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)
*)

unit CoreClasses;

interface

uses
  Windows, CoreUtils, CoreExceptions;

const
  InstanceSizeIndex = -8; // for Delphi 6, 7

type
  PObject = ^TObject;
  TObject = object
    constructor Create;
  end;

  PCoreObject = ^TCoreObject;
  TCoreObject = object(TObject)
  protected
    procedure Cast(DestName: PLegacyChar; DestInfo: Pointer);
  public
    destructor Destroy; virtual;
    procedure Finalize;
    procedure Free;
    function InstanceSize: Integer;
    function IsType(Info: Pointer): Boolean; // null <> null :-)
    function TypeInfo: Pointer;
  end;

  PContainer = ^TContainer;
  TContainer = object(TCoreObject)
  private
    FClassName: PLegacyChar;
  protected
    procedure Cast(Source: PContainer);
  public
    constructor Create(Name: PLegacyChar);
    property ClassName: PLegacyChar read FClassName;
  end;

  PEnumerable = ^TEnumerable;
  TEnumerable = object(TContainer)
  private  
    FCount: Integer; // for the descendants like TString
  public
    property Count: Integer read FCount;
  end;

  PClearable = ^TClearable;
  TClearable = object(TEnumerable)
  public
    procedure Clear; virtual;
  end;

  TCollectionItemMode = (imInline, imFreeMem, imFinalize, imFree);

  PCollection = ^TCollection;
  TCollection = object(TClearable)
  private
    FCapacity, FDelta, FItemSize: Integer;
    FAttachBuffer: Boolean;
    FItemMode: TCollectionItemMode;
  { placeholder } // FItems: Pointer;
    procedure FreeItems(Index, ItemCount: Integer);
    procedure Resize(Index, ItemCount: Integer);
    procedure SetCapacity(Value: Integer);
  protected
    function Append(ItemCount: Integer = 1): Integer;
    procedure Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer;
      Attach: Boolean = False);
    procedure Insert(Index: Integer; ItemCount: Integer = 1);
  public
    constructor Create(Name: PLegacyChar; BytesPerItem: Integer;
      CollectionItemMode: TCollectionItemMode);
    destructor Destroy; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer; ItemCount: Integer = 1);
    function TranslateCapacity(NewCount: Integer): Integer;
    function TranslateDelta: Integer;

    property AttachBuffer: Boolean read FAttachBuffer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Delta: Integer read FDelta write FDelta;
    property FreeMode: TCollectionItemMode read FItemMode;
    property ItemSize: Integer read FItemSize;
  end;

  PPointers = ^TPointers;
  TPointers = object(TCollection)
    function Append(Item: Pointer): Integer;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
  end;

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

  ECast = class(Exception)
  private
    FInstance: PCoreObject;
    FDestInfo: Pointer;
    FDestName: PLegacyChar;
  public
    constructor Create(Instance: PCoreObject; DestName: PLegacyChar; DestInfo: Pointer);
    property Instance: PCoreObject read FInstance;
    property DestInfo: Pointer read FDestInfo;
    property DestName: PLegacyChar read FDestName;
  end;

  EContainer = class(Exception)
  end;

  ECollection = class(EContainer)
  private
    FCollection: PCollection;
  public
    property Collection: PCollection read FCollection;
  end;

  EIndex = class(ECollection)
  private
    FIndex: Integer;
  public
    constructor Create(Collection: PCollection; Index: Integer);
    property Index: Integer read FIndex;
  end;

  ERange = class(ECollection)
  private
    FLowBound, FHighBound: Integer;
  public
    constructor Create(Collection: PCollection; Index, ItemCount: Integer);
    property LowBound: Integer read FLowBound;
    property HighBound: Integer read FHighBound;
  end;

  EFixed = class(ECollection)
  public
    constructor Create(Collection: PCollection);
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);

procedure CheckIndex(Collection: PCollection; Index: Integer);
procedure CheckRange(Collection: PCollection; Index, ItemCount: Integer);

implementation

uses
  CoreConsts;

type
  PCollectionCast = ^TCollectionCast;
  TCollectionCast = object(TCollection)
    Items: PAddress;
  end;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  PPointersCast = ^TPointersCast;
  TPointersCast = object(TPointers)
    Items: PPointerArray;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TCoreObject.Free
end;

procedure CheckIndex(Collection: PCollection; Index: Integer);
begin
  if (Collection = nil) or (Index < 0) or (Index > Collection.Count) then
    raise EIndex.Create(Collection, Index);
end;

procedure CheckRange(Collection: PCollection; Index, ItemCount: Integer);
begin
  if (Collection <> nil) and (Index > 0) and (Index < Collection.Count) then
  begin
    Inc(Index, ItemCount - 1);
    if (Index > 0) and (Index < Collection.Count) then
      Exit;
  end;
  raise ERange.Create(Collection, Index, ItemCount);
end;

{ ECast }

constructor ECast.Create(Instance: PCoreObject; DestName: PLegacyChar; DestInfo: Pointer);
var
  Msg: PLegacyChar;
begin
  if Instance <> nil then
    if TypeOf(Instance^) <> nil then
      if Instance.IsType(TypeOf(TContainer)) then
      begin
        if DestInfo <> nil then
          Msg := sCastMistmatch2
        else
          Msg := sCastToNull2;
        inherited Create(Msg, [PContainer(Instance).ClassName, DestName]);
        Msg := nil;
      end
      else
        if DestInfo <> nil then
          Msg := sCastMistmatch
        else
          Msg := sCastToNull
    else
      Msg := sCastUntyped
  else
    Msg := sCastNull;
  inherited Create(Msg, [DestName]);
  FInstance := Instance;
  FDestInfo := DestInfo;
  FDestName := DestName;
end;

{ EIndex }

constructor EIndex.Create(Collection: PCollection; Index: Integer);
begin
  if Collection <> nil then
    inherited Create(sIndexOutOfBounds, [Collection.ClassName, Index, 0, Collection.Count - 1])
  else
    inherited Create(sIndexOfNull, [Index]);
  FCollection := Collection;
  FIndex := Index;
end;

{ ERange }

constructor ERange.Create(Collection: PCollection; Index, ItemCount: Integer);
var
  LastIndex: Integer;
begin
  LastIndex := Index + ItemCount - 1;
  if Collection <> nil then
    inherited Create(sRangeOutOfBounds, [Collection.ClassName, Index, LastIndex, 0, Collection.Count - 1])
  else
    inherited Create(sRangeOfNull, [Index, LastIndex]);
  FCollection := Collection;
  FLowBound := Index;
  FHighBound := LastIndex;
end;

{ EFixed }

constructor EFixed.Create(Collection: PCollection);
begin
  if Collection <> nil then
    inherited Create(sFixedCapacity, [Collection.ClassName, Collection.Capacity])
  else                                                            
    inherited Create(sNullCapacity);
  FCollection := Collection;
end;

{ TObject }

constructor TObject.Create;
begin
end;

{ TCoreObject }

destructor TCoreObject.Destroy;
begin
end;

procedure TCoreObject.Cast(DestName: PLegacyChar; DestInfo: Pointer);
begin
  if (DestInfo <> nil) and (InstanceSize = PInteger(PAddress(DestInfo) + InstanceSizeIndex)^) then
    PPointer(@Self)^ := DestInfo
  else
    raise ECast.Create(@Self, DestName, DestInfo);
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

function TCoreObject.InstanceSize: Integer;
begin
  if (@Self <> nil) and (TypeOf(Self) <> nil) then  // Fast core
    Result := PInteger(PAddress(TypeOf(Self)) + InstanceSizeIndex)^
  else
    Result := 0;
end;

function TCoreObject.IsType(Info: Pointer): Boolean;
var
  This: Pointer;
begin
  if Info <> nil then
  begin
    This := TypeInfo;
    while (This <> nil) and (This <> Info) do
      This := PPointer(This)^;
  end;
  Result := False;
end;

function TCoreObject.TypeInfo: Pointer;
begin
  if @Self <> nil then
    Result := TypeOf(Self)
  else
    Result := nil;
end;

{ TContainer }

constructor TContainer.Create(Name: PLegacyChar);
begin
  FClassName := Name;
end;

procedure TContainer.Cast(Source: PContainer);
begin
  inherited Cast(FClassName, Source.TypeInfo);
end;

{ TClearable }

procedure TClearable.Clear;
begin
  FCount := 0;
end;

{ TCollection }

constructor TCollection.Create(Name: PLegacyChar; BytesPerItem: Integer;
  CollectionItemMode: TCollectionItemMode);
begin
  inherited Create(Name);
  FItemSize := BytesPerItem;
  FItemMode := CollectionItemMode;
end;

destructor TCollection.Destroy;
begin
  SetCapacity(0);
end;

function TCollection.Append(ItemCount: Integer): Integer;
begin
  Result := FCount;
  Resize(Result, ItemCount);
end;

procedure TCollection.Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer;
  Attach: Boolean);
var
  ByteCount: Integer;
begin
  Clear;
  if Attach then
    PCollectionCast(@Self).Items := Source
  else
  begin
    SetCapacity(ItemsCapacity);
    Move(Source^, PCollectionCast(@Self).Items^, ItemCount * FItemSize);
  end;
  FAttachBuffer := Attach;
  FItemMode := imInline;
  FCount := ItemCount;
  FCapacity := ItemsCapacity;
end;

procedure TCollection.Clear;
begin
  if FAttachBuffer then
  begin
    PCollectionCast(@Self).Items := nil;
    FAttachBuffer := False;
    FCapacity := 0;
  end
  else if FItemMode <> imInline then
    FreeItems(0, FCount);
  FCount := 0;
end;

procedure TCollection.Delete(Index, ItemCount: Integer);
var
  FirstBytes, LastBytes, NewCount, NewCapacity: Integer;
  NewItems: PAddress;
begin
  CheckRange(@Self, Index, ItemCount);

  NewCount := FCount - ItemCount;
  FirstBytes := Index * FItemSize;
  LastBytes := ItemCount * FItemSize;

  if FAttachBuffer then
  begin
    NewCapacity := TranslateCapacity(NewCount);
    GetMem(NewItems, NewCapacity * FItemSize);
    with PCollectionCast(@Self)^ do
    begin
      Move(Items^, NewItems^, FirstBytes);
      Move(Items[LastBytes], NewItems[FirstBytes], LastBytes);
      Items := NewItems;
    end;
    FCapacity := NewCapacity;
    FAttachBuffer := False;
  end
  else
  begin
    if FItemMode <> imInline then
      FreeItems(Index, ItemCount);
    with PCollectionCast(@Self)^ do
      Move(Items[LastBytes], Items[FirstBytes], LastBytes);
  end;

  FCount := NewCount;
end;

procedure TCollection.FreeItems(Index, ItemCount: Integer);
var
  I: Integer;
  Item: Pointer;
begin
  Item := PCollectionCast(@Self).Items + (Index + ItemCount - 1) * FItemSize;
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
    Item := PAddress(Item) - FItemSize;
  end;
end;

procedure TCollection.Insert(Index, ItemCount: Integer);
begin
  CheckIndex(@Self, Index);
  Resize(Index, ItemCount);
end;

procedure TCollection.Resize(Index, ItemCount: Integer);
var
  FirstBytes, NewCount, NewCapacity: Integer;
  NewItems, Src, Dst: PAddress;
begin
  NewCount := FCount + ItemCount;
  if (NewCount <= FCapacity) and not FAttachBuffer then
    with PCollectionCast(@Self)^ do
    begin
      Src := Items + (Index + ItemCount) * FItemSize;
      Dst := Items + ItemCount * FItemSize;
      Move(Src, Dst, Items + FCount * FItemSize - Src);
    end
  else
  begin
    if FDelta = 0 then
      raise EFixed.Create(@Self);
    NewCapacity := TranslateCapacity(NewCount);
    GetMem(NewItems, NewCapacity * FItemSize);
    FirstBytes := Index * FItemSize;
    with PCollectionCast(@Self)^ do
    begin
      Move(Items^, NewItems^, FirstBytes);
      Move(Items[FirstBytes], NewItems[FirstBytes + ItemCount * FItemSize],
        (FCount - Index) * FItemSize);
      Items := NewItems;
    end;
    FCapacity := NewCapacity;
    FAttachBuffer := False;
  end;
  FCount := NewCount;
end;

procedure TCollection.SetCapacity(Value: Integer);
var
  NewItems: Pointer;
begin
  if Value < 0 then
    Inc(Value, FCapacity);

  if (Value < FCount) and (FItemMode <> imInline) and not FAttachBuffer then
    FreeItems(Value, FCount - Value);

  if FAttachBuffer then
  begin
    GetMem(NewItems, Value * FItemSize);
    Move(PCollectionCast(@Self).Items^, NewItems^, FCount * FItemSize);
    PCollectionCast(@Self).Items := NewItems;
    FAttachBuffer := False;
  end
  else
    ReallocMem(PCollectionCast(@Self).Items, Value * FItemSize);

  FCapacity := Value;
  if Value < FCount then
    FCount := Value;
end;

function TCollection.TranslateCapacity(NewCount: Integer): Integer;
var
  GrowBy: Integer;
begin
  GrowBy := TranslateDelta;
  Result := NewCount + (NewCount + GrowBy - 1) mod GrowBy;
end;

function TCollection.TranslateDelta: Integer;
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

{ TPointers }

function TPointers.Append(Item: Pointer): Integer;
begin
  Result := inherited Append;
  PPointersCast(@Self).Items[Result] := Item;
end;

procedure TPointers.Exchange(Index1, Index2: Integer);
begin
  CheckIndex(@Self, Index1);
  CheckIndex(@Self, Index2);
  with PPointersCast(@Self)^ do
    CoreUtils.Exchange(Items[Index1], Items[Index2]);
end;

function TPointers.Extract(Index: Integer): Pointer;
begin
  CheckIndex(@Self, Index);
  Result := PPointersCast(@Self).Items[Index];
  Delete(Index);
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
