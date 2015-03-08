(*
    Lite Core Library (CoreLite mini)

    Platform-independent general purpose classes

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

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
    procedure Cast(DestName: PLegacyChar; DestInfo: Pointer);
    procedure InitInstance;
  public
    constructor Create;
    destructor Destroy; virtual; abstract;
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
    FCount: Integer;
  public
    property Count: Integer read FCount;
  end;

  PIndexed = ^TIndexed;
  TIndexed = object(TEnumerable)
  protected
    procedure CheckIndex(Index: Integer);
    procedure CheckRange(Index, ItemCount: Integer);
  end;

  PClearable = ^TClearable;
  TClearable = object(TIndexed)
  public
    procedure Clear; virtual;
  end;

  TCollectionItemMode = (imInline, imFreeMem, imFinalize, imFree);
  TSharingMode = (smCopy, smAttach, smCapture); 

  PCollection = ^TCollection;
  TCollection = object(TClearable)
  private
    FCapacity, FDelta: Integer;
    FItemMode: TCollectionItemMode;
    FItemSize: Integer;
    FAttachBuffer: Boolean;
  { placeholder } // FItems: Pointer;
    procedure Copy(Index: Integer; Collection: PCollection; Capture: Boolean);
    procedure Cut(Index, ItemCount: Integer);
    procedure Expand(Index, ItemCount: Integer);
    procedure FreeItems(Index, ItemCount: Integer);
    procedure SetCapacity(Value: Integer);
  protected
    function Append(ItemCount: Integer = 1): Integer; overload;
    procedure Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer; Attach: Boolean); overload;
    procedure Assign(Source: PCollection; Mode: TSharingMode); overload;
    procedure Detach;
    procedure CheckCapacity(ItemCount: Integer);
    procedure Insert(Index: Integer; ItemCount: Integer = 1); overload;
  public
    constructor Create(Name: PLegacyChar; BytesPerItem: Integer;
      CollectionItemMode: TCollectionItemMode = imInline);
    destructor Destroy; virtual;
    procedure Append(Collection: PCollection; Capture: Boolean = False); overload;
    procedure AsRange(Source: PCollection; Index: Integer;
      CopyProps: Boolean = True); overload;
    procedure AsRange(Source: PCollection; Index, ItemCount: Integer;
      CopyProps: Boolean = True); overload;
    procedure Clear; virtual;
    procedure Delete(Index: Integer; ItemCount: Integer = 1);
    procedure Insert(Index: Integer; Collection: PCollection; Capture: Boolean = False); overload;
    procedure Skip(ItemCount: Integer = 1);
    function TranslateCapacity(NewCount: Integer): Integer;
    function TranslateDelta: Integer;
    procedure Truncate(ItemCount: Integer);

    property AttachBuffer: Boolean read FAttachBuffer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Delta: Integer read FDelta write FDelta;
    property ItemMode: TCollectionItemMode read FItemMode;
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

  PCollections = ^TCollection;
  TCollections = object(TCollection)
  private
    function CalcCount(Index, ItemCount: Integer): Integer;
  public
    function AverageCount: Integer;
    function TotalCount: Integer; overload;
    function TotalCount(Index: Integer): Integer; overload;
    function TotalCount(Index, ItemCount: Integer): Integer; overload;
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

  EContainer = class(Exception);

  EIndexed = EContainer; // future class of (EContainer)

  EIndex = class(EIndexed)
  private
    FIndex: Integer;
  public
    constructor Create(Container: PIndexed; Index: Integer);
    property Index: Integer read FIndex;
  end;

  ERange = class(EIndexed)
  private
    FLowBound, FHighBound: Integer;
  public
    constructor Create(Container: PIndexed; Index, ItemCount: Integer);
    property LowBound: Integer read FLowBound;
    property HighBound: Integer read FHighBound;
  end;

  ECollectionIndex = class(EIndex)
  private
    FCollection: PCollection;
  public
    property Collection: PCollection read FCollection;
  end;

  ECollectionRange = class(ERange)
  private
    FCollection: PCollection;
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
    Items: TCollection;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TCoreObject.Free
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
    inherited Create(sOutOfCapacity, [Collection.Count, ItemCount,
      WhitespaceOrLineBreak[IsConsole], Collection.ClassName, Collection.Capacity])
  else
    inherited Create(sNullCapacity, [ItemCount]);
  FCollection := Collection;
end;

{ TCoreObject }

constructor TCoreObject.Create;
begin
  InitInstance;
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

procedure TCoreObject.InitInstance;
begin
{$IFDEF Lite}
  FillChar(PContainer(@Self).FClassName, InstanceSize - SizeOf(Pointer), 0);
{$ELSE}
  with PContainer(@Self)^ do
    FillChar(FClassName, InstanceSize - (PAddress(@FClassName) - PAddress(@Self)), 0);
{$ENDIF}     // ^--- first field after VMT

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
    while This <> nil do
    begin
      if This = Info then
      begin
        Result := True;
        Exit;
      end;
      This := PPointer(This)^;
    end;
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
  InitInstance;
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
  Expand(Result, ItemCount);
end;

procedure TCollection.Append(Collection: PCollection; Capture: Boolean);
begin
  if Collection <> nil then
    Copy(FCount, Collection, Capture)
end;

procedure TCollection.Assign(Source: Pointer; ItemCount, ItemsCapacity: Integer;
  Attach: Boolean);
begin
  Clear;
  if Attach then
  begin
    PCollectionCast(@Self).Items := Source;
    FCapacity := ItemsCapacity;
  end
  else
  begin
    SetCapacity(ItemsCapacity);
    Move(Source^, PCollectionCast(@Self).Items^, ItemCount * FItemSize);
  end;
  FAttachBuffer := Attach;
  FItemMode := imInline;
  FCount := ItemCount;
end;

procedure TCollection.Assign(Source: PCollection; Mode: TSharingMode);
var
  CanCapture: Boolean;
begin
  if Source <> nil then
  begin
    CanCapture := (Mode = smCapture) and not Source.AttachBuffer;
    Assign(PCollectionCast(@Source).Items, Source.FCount, Source.FCapacity,
      (Mode = smAttach) or CanCapture);
    if CanCapture then
    begin
      FItemMode := Source.FItemMode;
      FAttachBuffer := False;
      Source.FAttachBuffer := True;
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
    Assign(PCollectionCast(Source).Items + Index * Source.ItemSize, ItemCount,
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

procedure TCollection.Copy(Index: Integer; Collection: PCollection;
  Capture: Boolean);
var
  MinItemSize, I: Integer;
  Source, Dest: PAddress;
begin
  Expand(Index, Collection.Count);

  if FItemSize < Collection.ItemSize then
    MinItemSize := FItemSize
  else if FItemSize > Collection.ItemSize then
    MinItemSize := Collection.ItemSize
  else
    with PCollectionCast(Collection)^ do
    begin
      Move(Items^, PCollectionCast(@Self).Items[Index * FItemSize], Count * FItemSize);
      if Capture and not FAttachBuffer then
      begin
        FreeMem(Items);
        Items := PCollectionCast(@Self).Items + Index * FItemSize;
        FCapacity := Count;
        FAttachBuffer := True;
      end;
      Exit;
    end;

  Source := PCollectionCast(Collection).Items;
  Dest := PCollectionCast(@Self).Items + Index * FItemSize;
  for I := 0 to Collection.Count - 1 do
  begin
    Move(Source^, Dest^, MinItemSize);
    Inc(Source, PCollectionCast(Collection).ItemSize);
    Inc(Dest, FItemSize);
  end;
end;

procedure TCollection.Cut(Index, ItemCount: Integer);
var
  FirstBytes, LastBytes, NewCount, NewCapacity: Integer;
  NewItems: PAddress;
begin
  CheckCapacity(-ItemCount);

  NewCount := FCount - ItemCount;
  FirstBytes := Index * FItemSize;
  LastBytes := ItemCount * FItemSize;

  if FAttachBuffer then
  begin
    if Index = 0 then
    begin
      Inc(PCollectionCast(@Self).Items, LastBytes);
      Dec(FCapacity, ItemCount);
    end
    else if Index + ItemCount < FCount then
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
    end;
  end
  else
  begin
    if FItemMode <> imInline then
      FreeItems(Index, ItemCount);
    with PCollectionCast(@Self)^ do
      Move(Items[LastBytes], Items[FirstBytes], (NewCount - Index) * FItemSize);
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

procedure TCollection.Detach;
begin
  if FCapacity <> 0 then
    FAttachBuffer := True;
end;

procedure TCollection.Expand(Index, ItemCount: Integer);
var
  FirstBytes, NewCount, NewCapacity: Integer;
  NewItems, Src, Dst: PAddress;
begin
{$IFDEF Debug}
  if ItemCount < 0 then
    raise ERange.Create(@Self, Index, ItemCount);
{$ENDIF}
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
      raise ECapacity.Create(@Self, ItemCount);
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
  CheckIndex(Index);
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

procedure TCollection.SetCapacity(Value: Integer);
var
  NewItems: Pointer;
begin
  if Value < 0 then
    Inc(Value, FCapacity);

  if (Value < FCount) and (FItemMode <> imInline) and not FAttachBuffer then
    FreeItems(Value, FCount - Value);

  if AttachBuffer then
  begin
    if Value <> 0 then
    begin
      GetMem(NewItems, Value * FItemSize);
      Move(PCollectionCast(@Self).Items^, NewItems^, FCount * FItemSize);
      PCollectionCast(@Self).Items := NewItems;
    end
    else
      PCollectionCast(@Self).Items := nil;
    FAttachBuffer := False;
  end
  else
    ReallocMem(PCollectionCast(@Self).Items, Value * FItemSize);

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
  Result := NewCount + (NewCount + GrowBy - 1) mod GrowBy;
end;

function TCollection.TranslateDelta: Integer;
begin
  if FDelta < 0 then
  begin
    Result := FCapacity div Abs(FDelta);
    if Result = 0 then
      Result := Abs(FDelta);
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
    if not FAttachBuffer and (FItemMode <> imInline) then
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

{ TCollections }

function TCollections.AverageCount: Integer;
var
  I: Integer;
  Item: PCollection;
begin
  Result := 0;
  if FCount <> 0 then
  begin
    Item := @PCollectionsCast(@Self).Items;
    for I := 0 to FCount - 1 do
    begin
      Inc(Result, Item.Count);
      Inc(PAddress(Item), FItemSize);
    end;
    Result := Result div FCount;
  end;
end;

function TCollections.CalcCount(Index, ItemCount: Integer): Integer;
var
  I: Integer;
  Item: PCollection;
begin
  Result := 0;
  Item := PCollection(PAddress(@PCollectionsCast(@Self).Items) + Index * FItemSize);
  for I := Index to Index + ItemCount - 1 do
  begin
    Inc(Result, Item.Count);
    Inc(PAddress(Item), FItemSize);
  end;
end;

function TCollections.TotalCount: Integer;
begin
  Result := CalcCount(0, FCount);
end;

function TCollections.TotalCount(Index: Integer): Integer;
begin
  CheckIndex(Index);
  Result := CalcCount(Index, FCount - Index);
end;

function TCollections.TotalCount(Index, ItemCount: Integer): Integer;
begin
  CheckRange(Index, ItemCount);
  Result := CalcCount(Index, ItemCount);
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
