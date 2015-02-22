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
  protected  // prevent "Internal error: SY2394"
    FCount: Integer;
  public
    property Count: Integer read FCount;
  end;

  PClearable = ^TClearable;
  TClearable = object(TEnumerable)
  public
    destructor Destroy; virtual;
    procedure Clear; virtual; abstract;
  end;

  PExpandable = ^TExpandable;
  TExpandable = object(TEnumerable)
  protected
    procedure SetCount(Value: Integer); virtual; abstract;
  public
    property Count write SetCount;
  end;

  PCapable = ^TCapable;
  TCapable = object(TExpandable)
  private
    FCapacity, FDelta: Integer;
  protected
    procedure Grow;
    procedure SetCapacity(Value: Integer); virtual; abstract;
  public
    function TranslateDelta: Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Delta: Integer read FDelta write FDelta;
  end;

  PArray = ^TArray;
  TArray = object(TCapable)
  private
  { placeholder } // FItems: Pointer;
  { placeholder } // FOwnsItems: Boolean;
  protected
    function Append: Integer; overload;
    procedure Extract(Index: Integer); overload;
    class procedure FreeItem(const Item); virtual;
    procedure Insert(Index: Integer); overload;
    class function ItemSize: Integer; virtual;
    procedure SetCapacity(Value: Integer); virtual;
    procedure SetCount(Value: Integer); virtual;
  public
    constructor Create(ArrayName: PLegacyChar; Initial, GrowBy: Integer;
      OwnerOfItems: Boolean = False);
    function Append(const Item): Integer; overload;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Extract(Index: Integer; var Item); overload;
    procedure Insert(Index: Integer; const Item); overload;
  end;

  PPointers = ^TPointers;
  TPointers = object(TArray)
  public
    function Append(Item: Pointer): Integer;
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
  end;

  PObjects = ^TObjects;
  TObjects = object(TPointers)
  protected
    class procedure FreeItem(const Item); virtual;
  end;

  PStringArray = ^TStringArray;
  TStringArray = object(TArray)
  protected
    class procedure FreeItem(const Item); virtual;
    class function ItemSize: Integer; virtual;
  end;

  PLegacyStringRecArray = ^TLegacyStringRecArray;
  TLegacyStringRecArray = array[0..MaxInt div SizeOf(TLegacyStringRec) - 1] of TLegacyStringRec;

  PLegacyStringArray = ^TLegacyStringArray;
  TLegacyStringArray = object(TStringArray)
  private
  { hold } FItems: PLegacyStringRecArray;
  { hold } FOwnsItems: Boolean;
  public
    function Append(const Item: TLegacyStringRec): Integer; overload;
    function Append(Str: PLegacyChar): Integer; overload;
    function Append(Str: PLegacyChar; Len: Integer): Integer; overload;
    function Extract(Index: Integer): TLegacyStringRec;
    function IndexOf(Str: PLegacyChar; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    function IndexOf(Str: PLegacyChar; Len: Integer; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    procedure Insert(Index: Integer; const Item: TLegacyStringRec); overload;
    procedure Insert(Index: Integer; Str: PLegacyChar); overload;
    procedure Insert(Index: Integer; Str: PLegacyChar; Len: Integer); overload;

    property Items: PLegacyStringRecArray read FItems;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  PWideStringRecArray = ^TWideStringRecArray;
  TWideStringRecArray = array[0..MaxInt div SizeOf(TWideStringRec) - 1] of TWideStringRec;

  PWideStringArray = ^TWideStringArray;
  TWideStringArray = object(TStringArray)
  private
  { hold } FItems: PWideStringRecArray;
  { hold } FOwnsItems: Boolean;
  public
    function Append(const Item: TWideStringRec): Integer; overload;
    function Append(Str: PWideChar): Integer; overload;
    function Append(Str: PWideChar; Len: Integer): Integer; overload;
    function Extract(Index: Integer): TWideStringRec;
    function IndexOf(Str: PWideChar; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    function IndexOf(Str: PWideChar; Len: Integer; IgnoreFlags: LongWord = NORM_IGNORECASE;
      Locale: LongWord = LOCALE_USER_DEFAULT): Integer; overload;
    procedure Insert(Index: Integer; const Item: TWideStringRec); overload;
    procedure Insert(Index: Integer; Str: PWideChar); overload;
    procedure Insert(Index: Integer; Str: PWideChar; Len: Integer); overload;

    property Items: PWideStringRecArray read FItems;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  PCoreStringArray = PWideStringArray;
  TCoreStringArray = TWideStringArray; // TODO: non-Unicode

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

  EEnumerable = class(EContainer)
  private
    FContainer: PEnumerable;
  public
    property Container: PEnumerable read FContainer;
  end;

  EIndex = class(EEnumerable)
  private
    FContainer: PEnumerable;
    FIndex: Integer;
  public
    constructor Create(Container: PEnumerable; Index: Integer);
    property Index: Integer read FIndex;
  end;

  ERange = class(EEnumerable)
  private
    FLowBound, FHighBound: Integer;
  public
    constructor Create(Container: PEnumerable; LowBound, HighBound: Integer);
    property LowBound: Integer read FLowBound;
    property HighBound: Integer read FHighBound;
  end;

  EFixed = class(EContainer)
  private
    FContainer: PExpandable;
  public
    constructor Create(Container: PExpandable);
    property Container: PExpandable read FContainer;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);

procedure CheckIndex(Container: PEnumerable; Index: Integer);
procedure CheckRange(Container: PEnumerable; LowBound, HighBound: Integer);

implementation

uses
  CoreConsts;

type
  PArrayCast = ^TArrayCast;
  TArrayCast = object(TArray)
    Items: PLegacyChar;
    OwnsItems: Boolean;
  end;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  PPointersCast = ^TPointersCast;
  TPointersCast = object(TPointers)
    Items: PPointerArray;
    OwnsItems: Boolean;
  end;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..MaxInt div SizeOf(PObject) - 1] of PObject;

  PObjectsCast = ^TObjectsCast;
  TObjectsCast = object(TObjects)
    Items: PObjectArray;
    OwnsObjects: Boolean;
  end;

{ Helper functions }

procedure FreeAndNil(var Instance: PCoreObject);
asm
        XOR EDX, EDX
        XCHG [EAX], EDX  // XCHG enforces LOCK
        MOV EAX, EDX
        JMP TCoreObject.Free
end;

procedure CheckIndex(Container: PEnumerable; Index: Integer);
begin
  if (Container = nil) or (Index < 0) or (Index > Container.Count) then
    raise EIndex.Create(Container, Index);
end;

procedure CheckRange(Container: PEnumerable; LowBound, HighBound: Integer);
begin
  if (Container = nil) or (LowBound < 0) or (LowBound > Container.Count) or
    (HighBound < 0) or (HighBound > Container.Count)
  then
    raise ERange.Create(Container, LowBound, HighBound);
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

constructor EIndex.Create(Container: PEnumerable; Index: Integer);
begin
  if Container <> nil then
    inherited Create(sIndexOutOfBounds, [Container.ClassName, Index, 0, Container.Count])
  else
    inherited Create(sIndexOfNull, [Index]);
  FContainer := Container;
  FIndex := Index;
end;

{ ERange }

constructor ERange.Create(Container: PEnumerable; LowBound, HighBound: Integer);
begin
  if Container <> nil then
    inherited Create(sRangeOutOfBounds, [Container.ClassName, LowBound, HighBound, 0, Container.Count])
  else
    inherited Create(sRangeOfNull, [LowBound, HighBound]);
  FContainer := Container;
  FLowBound := LowBound;
  FHighBound := HighBound;
end;

{ EFixed }

constructor EFixed.Create(Container: PExpandable);
begin
  if Container <> nil then
    inherited Create(sFixedCapacity, [Container.ClassName, Container.FCount])
  else                                                            // ^-- Internal error: SY2394
    inherited Create(sNullCapacity);
  FContainer := Container;
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

destructor TClearable.Destroy;
begin
  Clear;
end;

{ TCapable }

procedure TCapable.Grow;
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

function TCapable.TranslateDelta: Integer;
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

{ TArray }

constructor TArray.Create(ArrayName: PLegacyChar; Initial, GrowBy: Integer;
  OwnerOfItems: Boolean);
begin
  inherited Create(ArrayName);
  with PArrayCast(@Self)^ do
  begin
    Delta := GrowBy;
    OwnsItems := OwnerOfItems;
  end;
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
  Move(Item, PArrayCast(@Self).Items[Result * Bytes], Bytes);
end;

procedure TArray.Clear;
begin
  SetCount(0);
end;

procedure TArray.Delete(Index: Integer);
begin
  CheckIndex(@Self, Index);
  with PArrayCast(@Self)^ do
    if OwnsItems then
      FreeItem(Items[Index * ItemSize]);
  Extract(Index);    
end;

procedure TArray.Extract(Index: Integer);
var
  Idx, Bytes: Integer;
begin
{$IFDEF Debug} // always after caller's check
  CheckIndex(@Self, Index);
{$ENDIF}
  Bytes := ItemSize;
  Idx := Index * Bytes;
  with PArrayCast(@Self)^ do
  begin
    Move(Items[Idx + Bytes], Items[Idx], (FCount - Index - 1) * Bytes);
    Dec(FCount);
  end;
end;

procedure TArray.Extract(Index: Integer; var Item);
var
  Bytes: Integer;
begin
  CheckIndex(@Self, Index);
  Bytes := ItemSize;
  Move(PArrayCast(@Self).Items[Index * Bytes], Item, Bytes);
  Extract(Index);
end;

class procedure TArray.FreeItem(const Item);
begin
  FreeMem(Pointer(Item));
end;

procedure TArray.Insert(Index: Integer);
var
  Idx, S: Integer;
begin
  CheckIndex(@Self, Index);
  Grow;
  S := ItemSize;
  Idx := Index * S;
  with PArrayCast(@Self)^ do
    Move(Items[Idx], Items[Idx + S], (FCount - Index) * S);
  Inc(FCount);
end;

procedure TArray.Insert(Index: Integer; const Item);
var
  Bytes: Integer;
begin
  Insert(Index);
  Bytes := ItemSize;
  Move(Item, PArrayCast(@Self).Items[Index * Bytes], Bytes);
end;

class function TArray.ItemSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

procedure TArray.SetCapacity(Value: Integer);
var
  I, Bytes: Integer;
begin
  if Value < FCount then
  begin
    CheckIndex(@Self, Value); // for negative values
    if PArrayCast(@Self).OwnsItems then
    begin
      Bytes := ItemSize;
      for I := FCount - 1 downto Value do
        FreeItem(PArrayCast(@Self).Items[I * Bytes]);
    end;
  end;
  ReallocMem(PArrayCast(@Self).Items, Value * ItemSize);
  FCapacity := Value;
  if FCount > FCapacity then
    FCount := FCapacity;
end;

procedure TArray.SetCount(Value: Integer);
var
  Bytes, GrowBy: Integer;
begin
  if FCount <> Value then
  begin
    if FDelta <> 0 then
    begin
      GrowBy := TranslateDelta;
      SetCapacity(Value + (Value + GrowBy - 1) mod GrowBy);
    end
    else
      SetCapacity(Value);
    Bytes := ItemSize;
    if Value > FCount then
      FillChar(PArrayCast(@Self).Items[FCount * Bytes], (Value - FCount) * Bytes, 0);
    FCount := Value;
  end;
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
  inherited Extract(Index);
  PPointersCast(@Self).Items[FCount] := nil;
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

{ TObjects }

class procedure TObjects.FreeItem(const Item);
begin
  PCoreObject(Item).Free;
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

function TLegacyStringArray.Append(Str: PLegacyChar; Len: Integer): Integer;
begin
  Result := inherited Append;
  with FItems[Result] do
  begin
    Value := Str;
    Length := Len;
  end;
end;

function TLegacyStringArray.Extract(Index: Integer): TLegacyStringRec;
begin
  CheckIndex(@Self, Index);
  Result := FItems[Index];
  Extract(Index);
end;

function TLegacyStringArray.IndexOf(Str: PLegacyChar; IgnoreFlags, Locale: LongWord): Integer;
begin
  Result := IndexOf(Str, StrLen(Str), IgnoreFlags, Locale);
end;

function TLegacyStringArray.IndexOf(Str: PLegacyChar; Len: Integer;
  IgnoreFlags, Locale: LongWord): Integer;
begin
  for Result := 0 to FCount - 1 do
    with FItems[Result] do
      if StrComp(Value, Length, Str, Len, IgnoreFlags, Locale) = 0 then
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

procedure TLegacyStringArray.Insert(Index: Integer; Str: PLegacyChar; Len: Integer);
begin
  inherited Insert(Index);
  with FItems[Index] do
  begin
    Value := Str;
    Length := Len;
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

function TWideStringArray.Append(Str: PWideChar; Len: Integer): Integer;
begin
  Result := inherited Append;
  with FItems[Result] do
  begin
    Value := Str;
    Length := Len;
  end;
end;

function TWideStringArray.Extract(Index: Integer): TWideStringRec;
begin
  CheckIndex(@Self, Index);
  Result := FItems[Index];
  Extract(Index);
end;

function TWideStringArray.IndexOf(Str: PWideChar; IgnoreFlags, Locale: LongWord): Integer;
begin
  Result := IndexOf(Str, WideStrLen(Str), IgnoreFlags, Locale);
end;

function TWideStringArray.IndexOf(Str: PWideChar; Len: Integer;
  IgnoreFlags, Locale: LongWord): Integer;
begin
  for Result := 0 to FCount - 1 do
    with FItems[Result] do
      if WideStrComp(Value, Length, Str, Len, IgnoreFlags, Locale) = 0 then
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

procedure TWideStringArray.Insert(Index: Integer; Str: PWideChar; Len: Integer);
begin
  inherited Insert(Index);
  with FItems[Index] do
  begin
    Value := Str;
    Length := Len;
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
