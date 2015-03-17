(*
    Lite Core Library (CoreLite mini)

    XML parser, based on W3C Recommendation (26 Nov 2008)
    Extensible Markup Language (XML) 1.0 (Fifth Edition)
    http://www.w3.org/TR/REC-xml

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- diagnostic exceptions
*)

unit CoreXML;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
  PXMLName = ^TXMLName;
  TXMLName = object(TWideString)
  private
    FPrefix, FLocalName: TWideString;
  public
    constructor Create;
    destructor Destroy; virtual;
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): Integer;
    procedure Clear; virtual;

    property LocalName: TWideString read FLocalName;
    property Prefix: TWideString read FPrefix;
  end;

  PXMLEscapedString = ^TXMLEscapedString;
  TXMLEscapedString = object(TWideString)
  public
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): Integer;
  end;

  PXMLObject = ^TXMLObject;
  TXMLObject = object(TClearable)
  private
    FName: TXMLName;
    FText: TXMLEscapedString;
    FXML: TWideString;
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Clear; virtual;

    property LocalName: TWideString read FName.LocalName;
    property Name: TXMLName read FName;
    property Prefix: TWideString read FName.Prefix;
    property Text: TXMLEscapedString read FText;
    property XML: TWideString read FXML;
  end;

  PXMLAttribute = ^TXMLAttribute;
  TXMLAttribute = object(TXMLObject)
  public
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): Integer;
  end;

  PXMLAttributeArray = ^TXMLAttributeArray;
  TXMLAttributeArray = array[0..MaxInt div SizeOf(TXMLAttribute) - 1] of TXMLAttribute;

  PXMLAttributes = ^TXMLAttributes;
  TXMLAttributes = object(TCollection)
  private
  { hold } FItems: PXMLAttributeArray;
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append: PXMLAttribute;
    function Insert(Index: Integer): PXMLAttribute;

    property Items: PXMLAttributeArray read FItems;
  end;

  PXMLNode = ^TXMLNode;
  PXMLNodeArray = ^TXMLNodeArray;

  TXMLNodes = object(TCollection)
  private
  { hold } FItems: PXMLNodeArray;
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append: PXMLNode;
    function Insert(Index: Integer): PXMLNode;

    property Items: PXMLNodeArray read FItems;
  end;

  TXMLNodeType = (ntText, ntCData, ntInstruction, ntComment);

  TXMLNode = object(TXMLObject)
  private
    FNodeType: TXMLNodeType; // one byte, needs align
    FAttributes: TXMLAttributes;
    FChildNodes: TXMLNodes;
  protected
    FPadding: array[0..2] of Byte; // aligning instance size to 4-byte boundary
  public
    constructor Create;
    destructor Destroy; virtual;
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): Integer;
    procedure Clear; virtual;

    property Attributes: TXMLAttributes read FAttributes;
    property ChildNodes: TXMLNodes read FChildNodes;
    property NodeType: TXMLNodeType read FNodeType;
  end;

  TXMLNodeArray = array[0..MaxInt div SizeOf(TXMLNode) - 1] of TXMLNode;

  PXMLDocument = ^TXMLDocument;
  TXMLDocument = object(TXMLNode)
  private
    FHeader: TXMLNode;
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Clear; virtual;

    property Header: TXMLNode read FHeader;
  end;

{ Exceptions }

  EXML = class(Exception);   

{ Delphi XML compatibility }

type
  TNodeType = TXMLNodeType;
const
  ntProcessingInst = ntInstruction;

implementation

uses
  CoreConsts;

type
  TEntity = packed record
    Ident: PWideChar;
    Value: WideChar;
  end;

const     // short Unicode strings
  sAmp  = #3'amp';
  sApos = #4'apos';
  sQuot = #4'quot';
  sGt   = #2'gt';
  sLt   = #2'lt';

  Entities: array[0..4] of TEntity = (
    (Ident: sAmp; Value: '&'),
    (Ident: sLt; Value: '<'),
    (Ident: sGt; Value: '>'),
    (Ident: sQuot; Value: '"'),
    (Ident: sApos; Value: '''')
  );

{ TXMLName }

constructor TXMLName.Create;
begin
  inherited Create;
  FLocalName.Create;
  FPrefix.Create;
end;

destructor TXMLName.Destroy;
begin
  FPrefix.Finalize;
  FLocalName.Finalize;
  inherited;
end;

function TXMLName.AsXML(Source: PWideString; AttachBuffer: Boolean): Integer;
var
  First, Next, Limit, Colon: PWideChar;
  L: Integer;
begin
  if (Source <> nil) and (Source.Count <> 0) then
  begin
  {$IFDEF Debug}
    if soBigEndian in Source.Options then
      raise EBigEndian.Create(Source);
  {$ENDIF}
    First := Source.RawData;
    case First^ of
      'a'..'z', ':', 'A'..'Z', '_', WideChar($C0)..WideChar($D6),
      WideChar($D8)..WideChar($F6), WideChar($F8)..WideChar($2FF),
      WideChar($370)..WideChar($37D), WideChar($37F)..WideChar($1FFF),
      WideChar($200C)..WideChar($200D), WideChar($2070)..WideChar($218F),
      WideChar($2C00)..WideChar($2FEF), WideChar($3001)..WideChar(High(THighSurrogates)),
      WideChar($F900)..WideChar($FDCF), WideChar($FDF0)..WideChar($FFFD):
        Next := First + 1;
    else
      raise EXML.Create('Invalid XML name starting with “%c” (U+%04X)',
        CP_LOCALIZATION, [PWord(First)^, PWord(First)^]);
    end;

    Colon := nil;
    Limit := First + Source.Count;
    while Next < Limit do
    begin
      case Next^ of
        'a'..'z', '0'..'9', 'A'..'Z', '.', '-', '_', WideChar($B7),
        WideChar($C0)..WideChar($D6), WideChar($D8)..WideChar($F6),
        WideChar($F8)..WideChar($37D), WideChar($37F)..WideChar($1FFF),
        WideChar($200C)..WideChar($200D), WideChar($203F)..WideChar($2040),
        WideChar($2070)..WideChar($218F), WideChar($2C00)..WideChar($2FEF),
        WideChar($3001)..WideChar(High(TLowSurrogates)),
        WideChar($F900)..WideChar($FDCF), WideChar($FDF0)..WideChar($FFFD):
          Inc(Next);
        ':':
          begin
            Colon := Next;
            Inc(Next);
          end;
      else
        Break;
      end;
    end;

    Result := Next - First;
    AsRange(Source, First - Source.RawData, Result);
    if not AttachBuffer then
      Detach;

    if Colon <> nil then
    begin
      L := Colon - First;
      FPrefix.AsRange(@Self, 0, L);
      FLocalName.AsRange(@Self, L + 1);
    end
    else
    begin
      FLocalName.AsRange(@Self, 0);
      FPrefix.Clear;
    end;
  end
  else
  begin
    Clear;
    Result := 0;
  end;
end;

procedure TXMLName.Clear;
begin
  inherited;
  FPrefix.Clear;
  FLocalName.Clear;
end;

{ TXMLEscapedString }

function TXMLEscapedString.AsXML(Source: PWideString; AttachBuffer: Boolean): Integer;
var
  First, Next, Last, Limit, Semicolon: PWideChar;
  I, L: Integer;
  Unescaped: QuadChar;
  T: Word;
  W: TWideString;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
  {$IFDEF Debug}
    if soBigEndian in Source.Options then
      raise EBigEndian.Create(Source);
  {$ENDIF}
    W.Create;
    try
      First := Source.RawData;
      Limit := First + Source.Count;

      Next := First;
      repeat
        case Next^ of
          '&':
            begin
              if Count = 0 then
              begin
                Last := Next;
                while Last < Limit do
                begin
                  case Last^ of
                    '"', '''', '<', '>':
                      Break;
                  else
                    Inc(Last);
                  end;
                end;
                Assign(First, Last - First, Source.Options - soAttach);
                Truncate(Last - Next);
                Limit := Last;
                AttachBuffer := False;
              end
              else
              begin
                L := Next - First;
                Move(First^, RawData[Count], L * SizeOf(WideChar));
                Append(L);
              end;

              Last := Next + 1;
              Semicolon := WideStrScan(Last, Limit - Last, ';');
              if Semicolon = nil then
              begin
                AsRange(Source, 0, Limit - Source.RawData);
                raise EXML.Create('Unescaped ampersand at “%s”', CP_LOCALIZATION, [Data]);
              end;

              L := Semicolon - Last;
              if soBigEndian in Source.Options then
              begin
                W.AsWideString(Last, L);
                W.SwapByteOrder;
              end
              else
                W.AsWideString(Last, L, soAttach);

              First := Semicolon + 1;
              Next := First;

              Unescaped := 0;
              if W.RawData^ = 'x' then
              begin
                W.Skip;
                Unescaped := W.AsHexadecimal;
              end
              else
              begin
                for I := Low(Entities) to High(Entities) do
                  with Entities[I] do
                    if W.Compare(Ident + 1, PWord(Ident)^, True) = 0 then
                    begin
                      Unescaped := QuadChar(Value);
                      Break;
                    end;
              end;

              if Unescaped = 0 then
                Unescaped := W.AsInteger;

              if Unescaped <= High(TUnicodeBMP) then
              begin
                RawData[Count] := WideChar(Unescaped);
                Append;
              end
              else
              begin
                T := Unescaped - Low(TUnicodeSMP);
                if soBigEndian in Options then
                  PLongWord(RawData + Count)^ :=
                    Swap(Low(THighSurrogates) + T shr 10) or
                    Swap((Low(TLowSurrogates) + T and $3FF) shl 16)
                else
                  PLongWord(RawData + Count)^ :=
                    Word(Low(THighSurrogates) + T shr 10) or
                    Word((Low(TLowSurrogates) + T and $3FF) shl 16);
                Append(2);
              end;
            end;
          '"', '''', '<', '>':
            Break;
        else
          Inc(Next);
        end;
      until Next >= Limit;

      L := Next - First;
      if Count = 0 then
      begin
        AsRange(Source, 0, L);
        if not AttachBuffer then
          Detach;
      end
      else
      begin
        Move(First^, RawData[Count], L * SizeOf(WideChar));
        Append(L);
        RawData[Count] := #0;
      end;
    finally
      W.Destroy;
    end;
    Result := Next - Source.RawData;
  end
  else
    Result := 0;
end;

{ TXMLObject }

constructor TXMLObject.Create;
begin
  FXML.Create;
  FName.Create;
  FText.Create;
end;

destructor TXMLObject.Destroy;
begin
  FText.Finalize;
  FName.Finalize;
  FXML.Finalize;
  inherited;
end;

procedure TXMLObject.Clear;
begin
  FText.Clear;
  FName.Clear;
  FXML.Clear;
  inherited;
end;

{ TXMLAttribute }

function TXMLAttribute.AsXML(Source: PWideString; AttachBuffer: Boolean): Integer;
var
  First, Next, Limit: PWideChar;
  L: Integer;
  Quote: WideChar;
  W: TWideString;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
  {$IFDEF Debug}
    if soBigEndian in Source.Options then
      raise EBigEndian.Create(Source);
  {$ENDIF}
    First := Source.RawData;
    Limit := First + Source.Count;

    repeat
      case First^ of
        #32, #9, #10, #13:
          Inc(First);
      else
        Break;
      end;
    until First >= Limit;

    W.Create;
    try
      W.AsRange(Source, First - Source.RawData);
      Next := First + FName.AsXML(@W, AttachBuffer);
      while Next < Limit do
        case Next^ of
          #32, #9, #10, #13:
            Inc(Next);
        else
          Break;
        end;

      if Next < Limit then
      begin
        if Next^ = '=' then
        begin
          Inc(Next);
          while Next < Limit do
            case Next^ of
              #32, #9, #10, #13:
                Inc(Next);
            else
              Break;
            end;

          if Next < Limit then
          begin
            Quote := Next^;
            case Quote of
              '"', '''':
                begin
                  Inc(Next);
                  if Next < Limit then
                  begin
                    L := Next - Source.RawData;
                    W.AsRange(Source, L, False);
                    L := FText.AsXML(@W, AttachBuffer);
                    Inc(Next, L);
                    if Next < Limit then
                    begin
                      if Next^ = Quote then
                      begin
                        Inc(Next);
                        Result := Next - First;
                        L := First - Source.RawData;
                        FXML.AsRange(Source, L, Result);
                        Inc(Result, L);
                        Exit;
                      end;
                    end;
                  end;
                  W.AsRange(Source, Source.RawData - First, Next - First);
                  raise EXML.Create('Unterminated attribute value: “%s”', CP_LOCALIZATION, [W.Data]);
                end;
            end;
          end;
        end;
      end;
      raise EXML.Create('No value for attribute named “%s”', CP_LOCALIZATION, [FName.Data]);
    finally
      W.Destroy;
    end;
  end;
  Result := 0;
end;

{ TXMLAttributes }

function TXMLAttributes.Append: PXMLAttribute;
begin
  Result := @FItems[inherited Append];
  Result.Create;
end;

class function TXMLAttributes.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sXMLAttributes;
    ItemSize := SizeOf(TXMLAttribute);
  end;
end;

function TXMLAttributes.Insert(Index: Integer): PXMLAttribute;
begin
  CheckIndex(Index);
  inherited Insert(Index);
  Result := @FItems[Index];
  Result.Create;
end;

{ TXMLNodes }

function TXMLNodes.Append: PXMLNode;
begin
  Result := @FItems[inherited Append];
  Result.Create;
end;

class function TXMLNodes.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sXMLNodes;
    ItemSize := SizeOf(TXMLNode);
  end;
end;

function TXMLNodes.Insert(Index: Integer): PXMLNode;
begin
  CheckIndex(Index);
  inherited Insert(Index);
  Result := @FItems[Index];
  Result.Create;
end;

{ TXMLNode }

constructor TXMLNode.Create;
begin
  inherited;
  with FAttributes do
  begin
    Create;
    Delta := -1;
  end;
  with FChildNodes do
  begin
    Create;
    Delta := -1;
  end;
end;

destructor TXMLNode.Destroy;
begin
  FChildNodes.Finalize;
  FAttributes.Finalize;
end;

function TXMLNode.AsXML(Source: PWideString; AttachBuffer: Boolean): Integer;
var
  First, Next, Last, Limit: PWideChar;
  L: Integer;
//  T, U: Word;
  W: TWideString;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
  {$IFDEF Debug}
    if soBigEndian in Source.Options then
      raise EBigEndian.Create(Source);
  {$ENDIF}
    First := Source.RawData;
    Limit := First + Source.Count;

    repeat
      case First^ of
        #32, #9, #10, #13:
          Inc(First);
      else
        Break;
      end;
    until First >= Limit;

    if First < Limit then
    begin
      if First^ <> '<' then
        with FXML do
        begin
          AsNextLine(Source);
          Skip(First - Source.RawData);
          raise EXML.Create('Unexpected character “%c” (U+%04X) at “%s”', CP_LOCALIZATION,
            [PWord(First)^, PWord(First)^, FXML.Data]);
        end;

      W.Create;
      try
        Next := First + 1;
        W.AsRange(Source, Next - Source.RawData);
        L := FName.AsXML(@W, AttachBuffer);
        Inc(Next, L);

        while Next < Limit do
          case Next^ of
            #32, #9, #10, #13:
              begin
                W.Skip(L);
                L := FAttributes.Append.AsXML(@W, AttachBuffer);
                Inc(Next, L);
              end;
          else
            Break;
          end;

        if Next < Limit then
        begin
          case Next^ of
            '/':
              begin
              end;
            '>':
              begin
                // text and/or child nodes
              end;
          end;
        end;
      finally
        W.Destroy;
      end;
    end
    else
      Next := First;
    Result := Next - Source.RawData;
  end
  else
    Result := 0;
end;

procedure TXMLNode.Clear;
begin
  FChildNodes.Clear;
  FAttributes.Clear;
  inherited;
end;

{ TXMLDocument }

constructor TXMLDocument.Create;
begin
  inherited;
  FHeader.Create;
end;

destructor TXMLDocument.Destroy;
begin
  FHeader.Finalize;
  inherited;
end;

procedure TXMLDocument.Clear;
begin
  FHeader.Clear;
  inherited;
end;

end.

