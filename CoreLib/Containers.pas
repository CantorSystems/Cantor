(*
  Unified Environment by Freeman

  Legacy container implementation:
    - abstract (non-enumerable)
    - enumerable
    - linked list (enumerable)
    - object, data and string list based on linked list container
    - abstract binary tree (enumerable)
    - balanced tree based on red-black tree algorithm
    - namespace based on balanced tree

  Copyright (c) 2007 Freeman

  Custom defines (turned off by default):

    TRANSACTIONS - Simple transaction support (TContainer.BeginUpdate and
                   EndUpdate methods)

    TRICKS       - Some address tricks to faster TText string manipulation
*)

unit Containers;

interface

uses
  Core, Storage;

type
  TForEach = procedure(Current: PObject; Data: Pointer; var Found: Boolean) of object;

  PContainer = ^TContainer;
  TContainer = object(TObject)
{$IFDEF TRANSACTIONS}
  private
    FLockCount: Integer;
  protected
    procedure BeforeUpdate; virtual;
    procedure AfterUpdate; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
{$ELSE}
  public
{$ENDIF}
    function ForEach(Action: TForEach; Data: Pointer): PContainer; virtual; abstract;
  end;

  PEnumerable = ^TEnumerable;
  TEnumerable = object(TContainer)
  private
    procedure ItemCount(Current: PObject; Data: Pointer; var Found: Boolean);
  public
    function Count: Integer;
  end;

  PList = ^TList;
  TList = object(TEnumerable)
  private
    FPrior, FNext: PList;
    procedure SetNext(Value: PList);
    procedure SetPrior(Value: PList);
  public
    destructor Done; virtual;
    function Delete: PList;
    procedure Exchange(List: PList); // TODO
    function Extract: PList;
    function ForEach(Action: TForEach; Data: Pointer): PContainer; virtual;
    function First: PList;
    procedure Invert;                // TODO
    function Last: PList;
  // properties
    property Next: PList read FNext write SetNext;
    property Prior: PList read FPrior write SetPrior;
  end;

  POwnerList = ^TOwnerList;
  TOwnerList = object(TList)
  private
    FOwnsData: Boolean;
  protected
    procedure FreeData; virtual; abstract;
  public
    constructor Init(IsOwnsData: Boolean);
    destructor Done; virtual;
  // properties
    property OwnsData: Boolean read FOwnsData;
  end;

  PDataList = ^TDataList;
  TDataList = object(TOwnerList)
  private
    FData: Pointer;
    procedure SetData(Value: Pointer);
  protected
    procedure FreeData; virtual;
  public
    function InsertNext(Value: Pointer): PDataList;
    function InsertPrior(Value: Pointer): PDataList;
  // properties
    property Data: Pointer read FData write SetData;
  end;

  PObjectList = ^TObjectList;
  TObjectList = object(TOwnerList)
  private
    FData: PObject;
    procedure SetData(Value: PObject);
  protected
    procedure FreeData; virtual;
  public
    function InsertNext(Value: PObject): PObjectList;
    function InsertPrior(Value: PObject): PObjectList;
  // properties
    property Data: PObject read FData write SetData;
  end;

  PStrings = PContainer;
  TStrings = TContainer;

{$IFDEF TRICKS}
  TGetStringResult = PCoreString;
{$ELSE}
  TGetStringResult = CoreString;
{$ENDIF}

  PText = ^TText;
  TText = object(TObject)
  private
  {$IFDEF TRICKS}
    FStringField: LongWord;
  {$ENDIF}
    FLineBreak: CoreString;
    procedure ItemText(Current: PObject; Data: Pointer; var Found: Boolean);
    procedure ItemTextLength(Current: PObject; Data: Pointer; var Found: Boolean);
  protected
    {class} function GetString(Strings: PStrings): TGetStringResult; virtual; abstract;
    function NeedLineBreak(Strings: PStrings): Boolean; virtual; abstract;
  public
    function Text(Strings: PStrings; const LineBreak: CoreString = sLineBreak;
      TxtLength: PInteger = nil): CoreString;
    function TextLength(Strings: PStrings;
      const LineBreak: CoreString = sLineBreak): Integer;
  end;

  PParser = ^TParser;
  TParser = object(TObject)
  protected
    {class} function NewString(Strings: PStrings;
      const Value: CoreString): PStrings; virtual; abstract;
  public
    {class} function Parse(const Source: CoreString): PStrings; overload;
    {class} function Parse(const Source: CoreString;
      LineBreak: CoreChar): PStrings; overload;
    {class} function Parse(const Source,
      LineBreakChars: CoreString): PStrings; overload;
  end;

  PStringListText = ^TStringListText;
  TStringListText = object(TText)
  private
    FTail: PList;
  protected
    {class} function GetString(Strings: PStrings): TGetStringResult; virtual;
    function NeedLineBreak(Strings: PStrings): Boolean; virtual;
  end;

  PStringListParser = ^TStringListParser;
  TStringListParser = object(TParser)
  protected
    {class} function NewString(Strings: PStrings;
      const Value: CoreString): PStrings; virtual;
  end;

  PStringList = ^TStringList;
  TStringList = object(TList)
  private
    FData: CoreString;
  protected
    {class} procedure InitParser(var Instance: TStringListParser);
    procedure InitText(var Instance: TStringListText);
  public
    function InsertNext(const Value: CoreString): PStringList;
    function InsertPrior(const Value: CoreString): PStringList;
    {class} function Parse(const Source: CoreString): PStringList; overload;
    {class} function Parse(const Source: CoreString;
      LineBreak: CoreChar): PStringList; overload;
    {class} function Parse(const Source,
      LineBreakChars: CoreString): PStringList; overload;
    function Text(const LineBreak: CoreString = sLineBreak;
      TxtLength: PInteger = nil): CoreString;
    function TextLength(const LineBreak: CoreString = sLineBreak): Integer;
  // properties
    property Data: CoreString read FData write FData;
  end;

  PBinaryTree = ^TBinaryTree;
  TBinaryTree = object(TEnumerable)
  private
    FParent, FLeft, FRight: PBinaryTree;
    procedure ItemDepth(Current: PObject; Data: Pointer; var Found: Boolean);
  protected
    function Add(Node: PBinaryTree): PBinaryTree;
    function Compare(const Key1, Key2): Integer; virtual; abstract;
    function Find(const Key; var Exact: Integer): PBinaryTree;
    function KeyData: Pointer; virtual; abstract;
  public
    destructor Done; virtual;
    function Delete: PBinaryTree;
    function Depth: Integer;
    function Extract: PBinaryTree;
    function ForEach(Action: TForEach; Data: Pointer): PContainer; virtual;
    function Level: Integer;
    function Root: PBinaryTree;
  // properties
    property Left: PBinaryTree read FLeft;
    property Parent: PBinaryTree read FParent;
    property Right: PBinaryTree read FRight;
  end;

  PRedBlackTree = ^TRedBlackTree;
  TRedBlackTree = object(TBinaryTree)
  private
    FRed: Boolean;
  protected
  public
  // properties
    property Red: Boolean read FRed;
  end;

  PBalancedTree = PRedBlackTree;
  TBalancedTree = TRedBlackTree;

  PNamespaceText = ^TNamespaceText;
  TNamespaceText = object(TText)
  private
    FLast: PBinaryTree;
  protected
    {class} function GetString(Strings: PStrings): TGetStringResult; virtual;
    function NeedLineBreak(Strings: PStrings): Boolean; virtual;
  end;

  PNamespaceParser = ^TNamespaceParser;
  TNamespaceParser = object(TParser)
  protected
    {class} function NewString(Strings: PStrings;
      const Value: CoreString): PStrings; virtual;
  end;

  PNamespace = ^TNamespace;
  TNamespace = object(TBalancedTree)
  private
    FName: CoreString;
    function GetLeft: PNamespace;
    function GetParent: PNamespace;
    function GetRight: PNamespace;
  protected
    function Compare(const Key1, Key2): Integer; virtual;
    {class} procedure InitParser(var Instance: TNamespaceParser);
    procedure InitText(var Instance: TNamespaceText);
    function KeyData: Pointer; virtual;
  public
    function Add(const Value: CoreString): PNamespace;
    function Delete: PNamespace;
    function Extract: PNamespace;
    function Find(const Key: CoreString): PNamespace;
    {class} function Parse(const Source: CoreString): PNamespace; overload;
    {class} function Parse(const Source: CoreString;
      LineBreak: CoreChar): PNamespace; overload;
    {class} function Parse(const Source,
      LineBreakChars: CoreString): PNamespace; overload;
    function Root: PNamespace;
    function Text(const LineBreak: CoreString = sLineBreak;
      TxtLength: PInteger = nil): CoreString;
    function TextLength(const LineBreak: CoreString = sLineBreak): Integer;
  // properties
    property Left: PNamespace read GetLeft;
    property Name: CoreString read FName;
    property Parent: PNamespace read GetParent;
    property Right: PNamespace read GetRight;
  end;

implementation

uses
  SysUtils, Unicode;

{ TContainer }

{$IFDEF TRANSACTIONS}
procedure TContainer.AfterUpdate;
begin
end;

procedure TContainer.BeforeUpdate;
begin
end;

procedure TContainer.BeginUpdate;
begin
  if FLockCount = 0 then
{$IFDEF MULTITHREAD}
  begin
    InterlockedIncrement(FLockCount);
    BeforeUpdate;
  end
  else
    InterlockedIncrement(FLockCount);
{$ELSE}
    BeforeUpdate;
  Inc(FLockCount);
{$ENDIF}
end;

procedure TContainer.EndUpdate;
begin
{$IFDEF MULTITHREAD}
  if InterlockedDecrement(FLockCount) then
    AfterUpdate;
{$ELSE}
  Dec(FLockCount);
  if FLockCount = 0 then
    AfterUpdate;
{$ENDIF}
end;
{$ENDIF}

{ TEnumerable }

function TEnumerable.Count: Integer;
begin
  Result := 0;
  ForEach(ItemCount, @Result);
end;

procedure TEnumerable.ItemCount(Current: PObject; Data: Pointer; var Found: Boolean);
begin
  Inc(PInteger(Data)^);
end;

{ TList }

destructor TList.Done;
begin
  while (FPrior <> nil) and (FNext <> nil) do
    FPrior.Delete;
  inherited;
end;

function TList.Delete: PList;
begin
  Result := Extract;
  Free;
end;

procedure TList.Exchange(List: PList); // TODO
begin
  if List <> nil then
  begin
//    if List.FPrior <> nil then
//      Exchange(Pointer
    if FNext <> List then
      Core.Exchange(Pointer(FNext), Pointer(List.FNext));
    if FPrior <> List then
      Core.Exchange(Pointer(FPrior), Pointer(List.FPrior));
  end
  else
    Invert;
end;

function TList.Extract: PList;
begin
  Result := FNext;
  if Result <> nil then
  begin
    FNext := nil;
    Result.FPrior := FPrior;
    FPrior := nil;
  end
  else
  begin
    Result := FPrior;
    if Result <> nil then
    begin
      Result.FNext := nil;
      FPrior := nil;
    end;
  end;
end;

function TList.First: PList;
begin
  Result := @Self;
  while Result.Prior <> nil do
    Result := Result.Prior;
end;

function TList.ForEach(Action: TForEach; Data: Pointer): PContainer;
var
  Found: Boolean;
begin
  Result := @Self;
  Found := False;
  while Result <> nil do
  begin
    Action(Result, Data, Found);
    if Found then
      Exit;
    Result := PList(Result).Next;
  end;
  Result := Prior;
  while Result <> nil do
  begin
    Action(Result, Data, Found);
    if Found then
      Exit;
    Result := PList(Result).Prior;
  end;
end;

procedure TList.Invert; // TODO
begin
  if FNext <> nil then
    if FPrior <> nil then
      Core.Exchange(Pointer(FNext), Pointer(FPrior))
    else
      Prior := Last
  else
    Next := First;
end;

function TList.Last: PList;
begin
  Result := @Self;
  while Result.Next <> nil do
    Result := Result.Next;
end;

procedure TList.SetNext(Value: PList);
var
  NewFirst: PList;
begin
  if Value <> nil then
  begin
    NewFirst := Value.First;
    if FNext <> NewFirst then
    begin
      Value.Last.Next := FNext;
      FNext := NewFirst;
      FNext.FPrior := @Self;
    end;
  end
  else
    if FNext <> nil then
    begin
      FNext.FPrior := nil;
      FreeAndNil(PObject(FNext));
    end;
end;

procedure TList.SetPrior(Value: PList);
var
  NewLast: PList;
begin
  if Value <> nil then
  begin
    NewLast := Value.Last;
    if FNext <> NewLast then
    begin
      Value.First.Prior := FPrior;
      FPrior := NewLast;
      FPrior.FNext := @Self;
    end;
  end
  else
    if FPrior <> nil then
    begin
      FPrior.FNext := nil;
      FreeAndNil(PObject(FPrior));
    end;
end;

{ TOwnerList }

constructor TOwnerList.Init(IsOwnsData: Boolean);
begin
  FOwnsData := IsOwnsData;
end;

destructor TOwnerList.Done;
begin
  if FOwnsData then
    FreeData;
  inherited;
end;

{ TDataList }

procedure TDataList.FreeData;
begin
  FreeAndNil(FData);
end;

function TDataList.InsertNext(Value: Pointer): PDataList;
begin
  New(Result, Init(FOwnsData));
  Result.FData := Value;
  if @Self <> nil then
    Next := Result;
end;

function TDataList.InsertPrior(Value: Pointer): PDataList;
begin
  New(Result, Init(FOwnsData));
  Result.FData := Value;
  if @Self <> nil then
    Prior := Result;
end;

procedure TDataList.SetData(Value: Pointer);
begin
  if FData <> Value then
  begin
    if OwnsData then
      FreeData;
    FData := Value;
  end;
end;

{ TObjectList }

procedure TObjectList.FreeData;
begin
  FreeAndNil(FData);
end;

function TObjectList.InsertNext(Value: PObject): PObjectList;
begin
  New(Result, Init(FOwnsData));
  Result.FData := Value;
  if @Self <> nil then
    Next := Result;
end;

function TObjectList.InsertPrior(Value: PObject): PObjectList;
begin
  New(Result, Init(FOwnsData));
  Result.FData := Value;
  if @Self <> nil then
    Prior := Result;
end;

procedure TObjectList.SetData(Value: PObject);
begin
  if FData <> Value then
  begin
    if OwnsData then
      FreeData;
    FData := Value;
  end;
end;

{ TText }

type
  PTextData = ^TTextData;
  TTextData = record
    Buffer: PCoreChar;
    Length: Integer;
  end;

procedure TText.ItemText(Current: PObject; Data: Pointer; var Found: Boolean);

procedure MoveString(const S: CoreString);
var
  L: Integer;
begin
  L := Length(S);
  if L <= PTextData(Data).Length then
  begin
    Move(S[1], PTextData(Data).Buffer^, L * SizeOf(CoreChar));
    with PTextData(Data)^ do
    begin
      Inc(Buffer, L);
      Dec(Length, L);
    end;
  end
  else
    Found := True;
end;

begin
{$IFDEF TRICKS}
  MoveString(PCoreString(LongWord(Current) + FStringField)^);
{$ELSE}
  MoveString(GetString(PStrings(Current)));
{$ENDIF}
  if not Found and NeedLineBreak(PStrings(Current)) then
    MoveString(FLineBreak);
end;

procedure TText.ItemTextLength(Current: PObject; Data: Pointer; var Found: Boolean);
begin
{$IFDEF TRICKS}
  Inc(PInteger(Data)^, Length(PCoreString(LongWord(Current) + FStringField)^) +
    Length(FLineBreak));
{$ELSE}
  Inc(PInteger(Data)^, Length(GetString(PStrings(Current))));
  if NeedLineBreak(PStrings(Current)) then
    Inc(PInteger(Data)^, Length(FLineBreak));
{$ENDIF}
end;

function TText.Text(Strings: PStrings; const LineBreak: CoreString;
  TxtLength: PInteger): CoreString;
var
  Data: TTextData;
begin
{$IFDEF TRICKS}
  FStringField := LongWord(GetString(Strings)) - LongWord(Strings);
{$ENDIF}
  FLineBreak := LineBreak;
  if TxtLength <> nil then
    Data.Length := TxtLength^
  else
    Data.Length := TextLength(Strings, FLineBreak);
  SetLength(Result, Data.Length);
  Data.Buffer := Pointer(Result);
  Strings.ForEach(ItemText, @Data);
end;

function TText.TextLength(Strings: PStrings; const LineBreak: CoreString): Integer;
begin
{$IFDEF TRICKS}
  FStringField := LongWord(GetString(Strings)) - LongWord(Strings);
{$ENDIF}
  FLineBreak := LineBreak;
  Result := 0;
  Strings.ForEach(ItemTextLength, @Result);
{$IFDEF TRICKS}
  Dec(Result, Length(FLineBreak));
{$ENDIF}
end;

{ TParser }

{class} function TParser.Parse(const Source: CoreString): PStrings;

var
  T: PCoreChar;

procedure Add(ToChar: PCoreChar);
var
  S: CoreString;
begin
  SetString(S, T, ToChar - T);
  Result := NewString(Result, S);
end;

var
  P, Tail: PCoreChar;
  WasCR: Boolean;
begin
  Result := nil;
  T := Pointer(Source);
  if T <> nil then
  begin
    Tail := T + Length(Source);
    while T <> Tail do
    begin
    {$IFDEF UNICODE}
      P := StrScanW(T, UnicodeChar(#13), Tail - T);
    {$ELSE}
      P := StrScan(T, #13);
    {$ENDIF}
      if P <> nil then
        WasCR := True
      else
      begin
        WasCR := False;
      {$IFDEF UNICODE}
        P := StrScanW(T, UnicodeChar(#10), Tail - T);
      {$ELSE}
        P := StrScan(T, #10);
      {$ENDIF}
        if P = nil then
        begin
          Add(Tail);
          Exit;
        end;
      end;
      Add(P);
      T := P + 1;
      if WasCR and
        {$IFDEF UNICODE} (T^ = #10) {$ELSE} (T^ = #10) {$ENDIF}
      then
        Inc(T);
    end;
  end;
end;

{class} function TParser.Parse(const Source: CoreString;
  LineBreak: CoreChar): PStrings;

var
  T: PCoreChar;

procedure Add(ToChar: PCoreChar);
var
  S: CoreString;
begin
  SetString(S, T, ToChar - T);
  Result := NewString(Result, S);
end;

var
  P, Tail: PCoreChar;
begin
  Result := nil;
  T := Pointer(Source);
  if T <> nil then
  begin
    Tail := T + Length(Source);
    while T <> Tail do
    begin
    {$IFDEF UNICODE}
      P := StrScanW(T, LineBreak, Tail - T);
    {$ELSE}
      P := StrScan(T, LineBreak);
    {$ENDIF}
      if P <> nil then
      begin
        Add(P);
        T := P + 1;
      end
      else
      begin
        Add(Tail);
        Exit;
      end;
    end;
  end;
end;

{class} function TParser.Parse(const Source, LineBreakChars: CoreString): PStrings;

var
  T: PCoreChar;

procedure Add(ToChar: PCoreChar);
var
  S: CoreString;
begin
  SetString(S, T, ToChar - T);
  if S <> '' then
    Result := NewString(Result, S);
end;

var
  P, Tail: PCoreChar;
begin
  Result := nil;
  T := Pointer(Source);
  if T <> nil then
  begin
    if LineBreakChars <> '' then
    begin
      Tail := T + Length(Source);
      while T <> Tail do
      begin
        P := T;
        while
        {$IFDEF UNICODE}
          StrScanW(Pointer(LineBreakChars), P^, Length(LineBreakChars))
        {$ELSE}
          StrScan(Pointer(LineBreakChars), P^)
        {$ENDIF}
          = nil do
        begin
          Inc(P);
          if P = Tail then
          begin
            Add(Tail);
            Exit;
          end;
        end;
        Add(P);
        T := P + 1;
      end;
    end
    else
      Result := NewString(Result, Source);
  end;
end;

{ TStringListText }

{class} function TStringListText.GetString(Strings: PStrings): TGetStringResult;
begin
  Result := {$IFDEF TRICKS} @ {$ENDIF} PStringList(Strings).FData;
end;

function TStringListText.NeedLineBreak(Strings: PStrings): Boolean;
begin
  Result := PStringList(Strings).FNext <> FTail;
end;

{ TStringListParser }

{class} function TStringListParser.NewString(Strings: PStrings;
  const Value: CoreString): PStrings;
begin
  Result := PStringList(Strings).InsertNext(Value);
end;

{ TStringList }

{class} procedure TStringList.InitParser(var Instance: TStringListParser);
begin
  Instance.Init;
end;

procedure TStringList.InitText(var Instance: TStringListText);
begin
  with Instance do
  begin
    Init;
    FTail := FPrior;
  end;
end;

function TStringList.InsertNext(const Value: CoreString): PStringList;
begin
  New(Result, Init);
  Result.FData := Value;
  if @Self <> nil then
    Next := Result;
end;

function TStringList.InsertPrior(const Value: CoreString): PStringList;
begin
  New(Result, Init);
  Result.FData := Value;
  if @Self <> nil then
    Prior := Result;
end;

{class} function TStringList.Parse(const Source: CoreString): PStringList;
var
  P: TStringListParser;
begin
  InitParser(P);
  Result := PStringList(PStringList(P.Parse(Source)).First);
end;

{class} function TStringList.Parse(const Source: CoreString;
  LineBreak: CoreChar): PStringList;
var
  P: TStringListParser;
begin
  InitParser(P);
  Result := PStringList(PStringList(P.Parse(Source, LineBreak)).First);
end;

{class} function TStringList.Parse(const Source,
  LineBreakChars: CoreString): PStringList;
var
  P: TStringListParser;
begin
  InitParser(P);
  Result := PStringList(PStringList(P.Parse(Source, LineBreakChars)).First);
end;

function TStringList.Text(const LineBreak: CoreString;
  TxtLength: PInteger): CoreString;
var
  T: TStringListText;
begin
  InitText(T);
  Result := T.Text(@Self, LineBreak, TxtLength);
end;

function TStringList.TextLength(const LineBreak: CoreString): Integer;
var
  T: TStringListText;
begin
  InitText(T);
  Result := T.TextLength(@Self, LineBreak);
end;

{ TBinaryTree }

destructor TBinaryTree.Done;
begin
  FLeft.Free;
  FRight.Free;
end;

function TBinaryTree.Add(Node: PBinaryTree): PBinaryTree;
var
  Exact: Integer;
  P: PBinaryTree;
begin
  P := Find(Node.KeyData^, Exact);
  if P <> nil then
  begin
    if Exact < 0 then
      P.FLeft := Node
    else if Exact > 0 then
      P.FRight := Node
    else
    begin
      Result := nil;
      Exit;
    end;
    Node.FParent := P;
    Result := Root;
  end
  else
    Result := Node;
end;

{type
  PDepth = ^TDepth;
  TDepth = record
    Current, Max: Integer;
  end;}

function TBinaryTree.Depth: Integer; // select max(level) from this with siblings
begin
  Result := 0;
  ForEach(ItemDepth, @Result);
end;
{var
  D: TDepth;
begin
  D.Max := 0;
  ForEach(ItemDepth, @D);
  Result := D.Max;
end;}

function TBinaryTree.Delete: PBinaryTree;
begin
  Result := Extract;
  Free;
end;

function TBinaryTree.Extract: PBinaryTree;
begin
  Result := FParent;
  if Result <> nil then
  begin
    if Result.FLeft = @Self then
      Result.FLeft := nil
    else
      Result.FRight := nil;
    FParent := nil;

    if FLeft <> nil then
    begin
      Result := Result.Add(FLeft);
      FLeft := nil;
    end;

    if FRight <> nil then
    begin
      Result := Result.Add(FRight);
      FRight := nil;
    end;
  end
  else
    if FLeft <> nil then
    begin
      if FRight <> nil then
        Result := FLeft.Add(FRight);
    end
    else
      Result := FRight;
end;

function TBinaryTree.Find(const Key; var Exact: Integer): PBinaryTree;

procedure DoFind(P: PBinaryTree);
begin
  Exact := Compare(P.KeyData^, Key);
  if Exact < 0 then
  begin
    if P.FLeft <> nil then
    begin
      DoFind(P.FLeft);
      Exit;
    end;
  end
  else if Exact > 0 then
  begin
    if P.FRight <> nil then
    begin
      DoFind(P.FRight);
      Exit;
    end;
  end;
  Result := P;
end;

begin
  DoFind(Root);
end;

function TBinaryTree.ForEach(Action: TForEach; Data: Pointer): PContainer;

var
  Found: Boolean;

procedure DoEach(Item: PBinaryTree);
begin
  if Item.FLeft <> nil then
    DoEach(Item.FLeft);
  if not Found then
  begin
    Action(Item, Data, Found);
    if Found then
      Result := Item
    else
      if (Item.FRight <> nil) then
        DoEach(Item.FRight);
  end;
end;

begin
  Found := False;
  Result := nil;
  DoEach(Root);
end;

procedure TBinaryTree.ItemDepth(Current: PObject; Data: Pointer; var Found: Boolean);
var
  CurrentDepth: Integer;
begin
  if (PBinaryTree(Current).FLeft = nil) and (PBinaryTree(Current).FRight = nil) then
  begin
    CurrentDepth := PBinaryTree(Current).Level;
    if CurrentDepth > PInteger(Data)^ then
      PInteger(Data)^ := CurrentDepth;
  end;
end;
{begin
  if PBinaryTree(Current).FParent = nil then
    PDepth(Data).Current = 0
  else PBinaryTree(Current).FParent.FLeft = @Self then
    Inc(PDepth(Data).Current)
  if (PBinaryTree(Current).FLeft = nil) and (PBinaryTree(Current).FRight = nil) then
    with PDepth(Data)^ do
    begin
      if Current > Max then
        Max := Current;
    end;
end;}

function TBinaryTree.Level: Integer;
var
  P: PBinaryTree;
begin
  Result := 0;
  P := Parent;
  while P <> nil do
  begin
    Inc(Result);
    P := P.Parent;
  end;
end;

function TBinaryTree.Root: PBinaryTree;
begin
  Result := @Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

{ TRedBlackTree }

{ TNamespaceText }

{class} function TNamespaceText.GetString(Strings: PStrings): TGetStringResult;
begin
  Result := {$IFDEF TRICKS} @ {$ENDIF} PNamespace(Strings).FName;
end;

function TNamespaceText.NeedLineBreak(Strings: PStrings): Boolean;
begin
  if FLast <> PBinaryTree(Strings).FParent then
    Result := True
  else
  begin
    FLast := PBinaryTree(Strings).FRight;
    Result := FLast <> nil;
  end;
end;

{ TNamespaceParser }

{class} function TNamespaceParser.NewString(Strings: PStrings;
  const Value: CoreString): PStrings;
begin
  Result := PNamespace(Strings).Add(Value);
  if Result = nil then
    Result := Strings;
end;

{ TNamespace }

function TNamespace.Add(const Value: CoreString): PNamespace;
begin
  New(Result, Init);
  Result.FName := Value;
  if @Self <> nil then
    Result := PNamespace(inherited Add(Result));
end;

function TNamespace.Compare(const Key1, Key2): Integer;
begin
{$IFDEF UNICODE}
  Result := StrCompW(Pointer(Key1), Pointer(Key2));
{$ELSE}
  Result := CompareStr(MultiAnsiString(Key1), MultiAnsiString(Key2));
{$ENDIF}
end;

function TNamespace.Delete: PNamespace;
begin
  Result := PNamespace(inherited Delete);
end;

function TNamespace.Extract: PNamespace;
begin
  Result := PNamespace(inherited Extract);
end;

function TNamespace.Find(const Key: CoreString): PNamespace;
var
  Exact: Integer;
begin
  Result := PNamespace(inherited Find(Key, Exact));
  if Exact <> 0 then
    Result := nil;
end;

function TNamespace.GetLeft: PNamespace;
begin
  Result := PNamespace(inherited Left);
end;

function TNamespace.GetParent: PNamespace;
begin
  Result := PNamespace(inherited Parent);
end;

{class} procedure TNamespace.InitParser(var Instance: TNamespaceParser);
begin
  Instance.Init;
end;

procedure TNamespace.InitText(var Instance: TNamespaceText);
begin
  Instance.Init;
end;

function TNamespace.GetRight: PNamespace;
begin
  Result := PNamespace(inherited Right);
end;

function TNamespace.KeyData: Pointer;
begin
  Result := @FName;
end;

{class} function TNamespace.Parse(const Source: CoreString): PNamespace;
var
  P: TNamespaceParser;
begin
  InitParser(P);
  Result := PNamespace(P.Parse(Source));
end;

{class} function TNamespace.Parse(const Source: CoreString;
  LineBreak: CoreChar): PNamespace;
var
  P: TNamespaceParser;
begin
  InitParser(P);
  Result := PNamespace(P.Parse(Source, LineBreak));
end;

{class} function TNamespace.Parse(const Source,
  LineBreakChars: CoreString): PNamespace;
var
  P: TNamespaceParser;
begin
  InitParser(P);
  Result := PNamespace(P.Parse(Source, LineBreakChars));
end;

function TNamespace.Root: PNamespace;
begin
  Result := PNamespace(inherited Root);
end;

function TNamespace.Text(const LineBreak: CoreString;
  TxtLength: PInteger): CoreString;
var
  T: TNamespaceText;
begin
  InitText(T);
  Result := T.Text(@Self, LineBreak, TxtLength);
end;

function TNamespace.TextLength(const LineBreak: CoreString): Integer;
var
  T: TNamespaceText;
begin
  InitText(T);
  Result := T.TextLength(@Self, LineBreak);
end;

end.
