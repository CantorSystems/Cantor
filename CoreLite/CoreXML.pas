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
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): TWideString;
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
    function AsXML(Source: PWideString; AttachBuffer: Boolean = False): TWideString;
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
    FPadding: array[0..2] of Byte; // aligning instance size to 4-byte boundary
  protected
    property PaddingByte: Byte read FPadding[0];
  public
    constructor Create;
    destructor Destroy; virtual;
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
      case First^ of
        ':', 'A'..'Z', '_', 'a'..'z', WideChar($C0)..WideChar($D6),
        WideChar($D8)..WideChar($F6), WideChar($F8)..WideChar($2FF),
        WideChar($370)..WideChar($37D),
        WideChar($37F)..WideChar($1FFF), WideChar($200C)..WideChar($200D),
        WideChar($2070)..WideChar($218F), WideChar($2C00)..WideChar($2FEF),
        WideChar($3001)..WideChar(High(THighSurrogates)),
        WideChar($F900)..WideChar($FDCF), WideChar($FDF0)..WideChar($FFFD):
          Next := First + 1;
      else
        raise EXML.Create('Invalid XML name starting with “%c” (U+%04X)',
          CP_LOCALIZATION, [PWord(First)^, PWord(First)^]);
      end;

      Colon := nil;
      while Next < Limit do
        case Next^ of
          '0'..'9', '.', '-', 'A'..'Z', '_', 'a'..'z',
          WideChar($B7), WideChar($C0)..WideChar($D6),
          WideChar($D8)..WideChar($F6), WideChar($F8)..WideChar($2FF),
          WideChar($300)..WideChar($37D),
          WideChar($37F)..WideChar($1FFF), WideChar($200C)..WideChar($200D),
          WideChar($203F)..WideChar($2040),
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

      AsRange(Source, First - Source.RawData, Next - First);
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

      Result := Next - Source.RawData;
      Exit;
    end
  end;

  Clear;
  Result := 0;
end;

procedure TXMLName.Clear;
begin
  inherited;
  FPrefix.Clear;
  FLocalName.Clear;
end;

{ TXMLEscapedString }

function TXMLEscapedString.AsXML(Source: PWideString; AttachBuffer: Boolean): TWideString;
var
  P, Limit, Next, Semicolon: PWideChar;
  L: Integer;
  Unescaped: QuadChar;
  T: Word;
  W: TWideString;
begin
  Result.Create;

{  if (Source <> nil) and (Source.Count <> 0) then
  begin
    P := Source.RawData;
    Limit := P + Source.Count;
    Next := nil;

    if (Source.CodePage = nil) or (Source.CodePage.LeadBytes = []) then

    repeat
      case P^ of
        '&':
          begin
            if Next = nil then
            begin
              Next := P + 1;
              if (Source.CodePage = nil) or (Source.CodePage.LeadBytes = []) then
                while (Next < Limit) and not (Next^ in ['<', '>', #0]) do
                  Inc(Next)
              else
                while (Next < Limit) and not (Next^ in ['<', '>', #0]) do
                begin
                  if Next^ in [#1..#127] then
                    Inc(Next)
                  else if Next^ in Source.CodePage.LeadBytes then
                  begin
                    Inc(Next);
                    while (Next < Limit) and not ((Next^ in [#1..#127]) or
                      (Next^ in Source.CodePage.LeadBytes))
                    do
                      Inc(Next);
                  end;
                end;
              Clear;
              Capacity := Next - P + 1;
              Limit := Next;
              Next := Source.RawData;
              AttachBuffer := False;
            end;
            L := P - Next;
            Move(RawData[Count], Next^, L);
            Append(L);

            Inc(P);
            Semicolon := StrScan(P, Limit - P, ';');
            if Semicolon = nil then
            begin
              AsRange(Source, 0, Limit - Source.RawData);
              W.Create;
              try
                W.AsString(@Self);
                raise EXML.Create('Unescaped semicolon at “%s”', CP_LOCALIZATION, [W.RawData]);
              finally
                W.Destroy;
              end;
            end;

            L := Semicolon - P;
            if L = SizeOf(Word) then
              case PWord(P)^ or $2020 of
                $746C: Unescaped := QuadChar('<');
                $7467: Unescaped := QuadChar('>');
              end
            else if L = SizeOf(LongWord) then
              case PLongWord(P)^ or $20202020 of
                $746F7571: Unescaped := QuadChar('"');
                $736F7061: Unescaped := QuadChar('''');
              end
            else if (L = 3) and (PLongWord(P)^ or $20202020 = $3B706D61) then
              Unescaped := QuadChar('&')
            else if (L > 2) and (PByte(P)^ or $20 = Byte('x')) then
            begin
              S.AsString(P + 1, L - 1);
              Unescaped := S.AsHexadecimal;
            end
            else
            begin
              S.AsString(P, L);
              Unescaped := S.AsInteger;
            end;

            if Unescaped < 128 then
            begin
              RawData[Count] := LegacyChar(Unescaped);
              Append;
            end
            else
            begin
              if Unescaped >= Low(TUnicodeSMP) then
              begin
                T := Unescaped - Low(TUnicodeSMP);
                Unescaped := Word(Low(THighSurrogates) + T shr 10) or
                  Word((Low(TLowSurrogates) + T and $3FF) shl 16);
                L := 2;
              end
              else
                L := 1;
              W.AsWideString(PWideChar(@Unescaped), L, soAttach);
              if (CodePage <> nil) and (AssignWideString(Count, @W).ErrorInfo.RawData <> 0) then
                CodePage := nil;

          end;
      end;
    until P >= Limit;

    if P^ <> #0 then
      while (P < Limit) and (P^ in [#32, #9, #10, #13, #0]) do
        Inc(P);

    if (P < Limit) and (P^ <> #0) then
    begin
      Result.AsRange(Source, P - Source.RawData, Limit - P);
      Exit;
    end;
  end;}

  Clear;
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

function TXMLAttribute.AsXML(Source: PWideString; AttachBuffer: Boolean): TWideString;
begin

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

