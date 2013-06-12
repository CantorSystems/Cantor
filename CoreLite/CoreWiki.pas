(*
    Lite Core Library (CoreLite)

    Wiki engine implementation

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

unit CoreWiki;

interface

uses
  CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

type
  TTextType = (ttNormal, ttNoBreak, ttCode, ttKeyboard, ttSample,
    ttStrong, ttEmphasis, ttInsert, ttDelete, ttSmall, ttMark, ttSuperscript,
    ttSubscript, ttVariable, ttDefinition, ttCite, ttEmotion); // ttQuote, ttSpelling // Ruby?

type
  TParagraphType = (ptNormal, ptCode, ptHeading, ptLeft, ptCenter, ptRight, ptJustify);

  TGroupType = (gtIndent, gtQuote, gtBullet, gtSquare, gtDisc, gtDecimal,
    gtLeadingZero, gtLowerLatin, gtUpperLatin, gtLowerRoman, gtUpperRoman,
    gtTable, gtRow, gtHeadingRow);

  TEntity = packed record
    Code: WideChar;
    Value: PLegacyChar;
  end;

{$I Entities.inc}

type
  TWikiLinkOptions = set of (wlMergeText, wlNumberwiki);

  TWikiLink = class
  private
    FPrefix, FPath, FPathPlus: TLegacyString;
    FOptions: TWikiLinkOptions;
  public
    destructor Destroy; override;

    property Options: TWikiLinkOptions read FOptions;
    property Prefix: TLegacyString read FPrefix;
    property Path: TLegacyString read FPath;
    property PathPlus: TLegacyString read FPathPlus;
  end;

  TWikiItem = class(TListItem)
  private
  { placeholder } // FParent: TWikiItem;
  { placeholder } // FLanguage: TLegacyString;
  { placeholder } // FItems: TList;
  { placeholder } // FAnchor, FStyle: TLegacyString;
  protected
    function Parse(Source, Limit: PLegacyChar): PLegacyChar; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Extract; override;
    function LanguageContext: TLegacyString;
  end;

  TWikiItems = class(TList)
  private
  { hold } FFirst, FLast: TWikiItem;
  public
    property First: TWikiItem read FFirst;
    property Last: TWikiItem read FLast;
  end;

  TWikiTexts = class;

  TWikiText = class(TWikiItem)
  private
  { hold } FOwner: TWikiItems;
  { hold } FPrior, FNext: TWikiText;
  { hold } FParent: TWikiItem;
  { hold } FLanguage: TLegacyString;
  { hold } FItems: TWikiTexts;
  { hold } FAnchor, FStyle: TLegacyString;
    FTextType: TTextType; // holds TWikiParagraph.ParagraphStyle and TWikiGroup.GroupStyle
    FValue: TLegacyString;
    FLink: TWikiLink;
    FEntityCode: WideChar;
  protected
    function Parse(Source, Limit: PLegacyChar): PLegacyChar; override;
  public
    destructor Destroy; override;

    property Anchor: TLegacyString read FAnchor;
    property EntityCode: WideChar read FEntityCode;
    property Items: TWikiTexts read FItems;
    property Next: TWikiText read FNext;
    property Owner: TWikiItems read FOwner;
    property Parent: TWikiItem read FParent;
    property Prior: TWikiText read FPrior;
    property TextType: TTextType read FTextType;
    property Style: TLegacyString read FStyle;
    property Value: TLegacyString read FValue;
  end;

  TWikiTexts = class(TList)
  private
  { hold } FFirst, FLast: TWikiText;
  public
    property First: TWikiText read FFirst;
    property Last: TWikiText read FLast;
  end;

  TWikiBlock = class(TWikiItem)
  private
  { hold } FOwner, FPrior, FNext, FParent: TWikiBlock;
  { hold } FLanguage: TLegacyString;
  public
    property Next: TWikiBlock read FNext;
    property Owner: TWikiBlock read FOwner;
    property Parent: TWikiBlock read FParent;
    property Prior: TWikiBlock read FPrior;
  end;

  TWikiParagraph = class(TWikiBlock)
  private
  { hold } FItems: TWikiTexts;
  { hold } FAnchor, FStyle: TLegacyString;
    FParagraphType: TParagraphType;
  protected  
    function Parse(Source, Limit: PLegacyChar): PLegacyChar; override;
  public
    property Anchor: TLegacyString read FAnchor;
    property Items: TWikiTexts read FItems;
    property ParagraphType: TParagraphType read FParagraphType;
    property Style: TLegacyString read FStyle;
  end;

  TWikiBlocks = class;

  TWikiGroup = class(TWikiBlock)
  private
  { hold } FItems: TWikiBlocks;
  { hold } FAnchor, FStyle: TLegacyString;
    FGroupType: TGroupType;
  protected
    function Parse(Source, Limit: PLegacyChar): PLegacyChar; override;
  public
    property Anchor: TLegacyString read FAnchor;
    property GroupType: TGroupType read FGroupType;
    property Items: TWikiBlocks read FItems;
    property Style: TLegacyString read FStyle;
  end;

  TWikiBlocks = class(TList)
  private
  { hold } FFirst, FLast: TWikiBlock;
  public
    property First: TWikiBlock read FFirst;
    property Last: TWikiBlock read FLast;
  end;

  TWikiCluster = class;

  TWikiDocument = class(TRedBlackTreeItem)
  private
  { hold } FOwner: TWikiCluster;
  { hold } FLeft, FRight, FParent: TWikiDocument;
    FLanguage: TLegacyString; // ordered to hold TWikiText/TWikiBlock
    FItems: TWikiBlocks;
    FPath, FName: TLegacyString;
    FIsUTF8: Boolean;
    FCharSet, FTitle, FSource: TLegacyString;
    FCluster: TWikiCluster;
  protected
    function Parse(P, Limit: PLegacyChar; WikiMode: Boolean): PLegacyChar;
  public
    destructor Destroy; override;
    procedure Clear;
    function Compare(Item: TBalancedTreeItem): Integer; override;
    procedure Load(Stream: TReadableStream; WikiMode: Boolean = True); overload;
    procedure Load(FileName: PCoreChar; WikiMode: Boolean = True); overload;

    property IsUTF8: Boolean read FIsUTF8;
    property Items: TWikiBlocks read FItems;
    property Language: TLegacyString read FLanguage;
    property Left: TWikiDocument read FLeft;
    property Name: TLegacyString read FName;
    property Owner: TWikiCluster read FOwner;
    property Parent: TWikiDocument read FParent;
    property Path: TLegacyString read FPath;
    property Right: TWikiDocument read FRight;
  end;

  TWikiCluster = class(TRedBlackTree)
  private
  { hold } FRoot: TWikiDocument;
  public
    property Root: TWikiDocument read FRoot;
  end;

{function CharIndex(Source: Char): Integer;
function ExtCharIndex(Source: WideChar): Integer;}

//function HTMLEncoding(CodePage: Word = CP_ACP): CoreString; overload;

implementation

uses
  Windows;

const
  TextTags: array[ttNoBreak..ttSuperscript] of LegacyChar = '#%><*/_-+!^';

(*function FindCharIndex(Source: WideChar; Chars: PCharCodes; Count: Integer): Integer;
var
  Min, Max: Integer;
begin
  Min := 0;
  Max := Count - 1;
  while Min <= Max do
  begin
    Result := (Max + Min) div 2;
    if Chars[Result].Source <> Source then
      if Chars[Result].Source > Source then
        Max := Result - 1
      else
        Min := Result + 1
    else
      Exit;
  end;
  Result := -1;
end;

function CharIndex(Source: Char): Integer;
begin
  Result := FindCharIndex(WideChar(Source), @Chars, Length(Chars));
end;

function ExtCharIndex(Source: WideChar): Integer;
begin
  Result := FindCharIndex(Source, @ExtChars, Length(ExtChars));
end;*)

(*function HTMLEncoding(CodePage: Word): CoreString;

var
  Key: HKEY;

procedure QueryKey(const Ident: CoreString);
var
  ValType, Len: DWORD;
begin
  if ( {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
    (Key, Pointer(Ident), nil, @ValType, nil, @Len) = ERROR_SUCCESS) and
    (ValType = REG_SZ) then
  begin
    SetLength(Result, Len div SizeOf(CoreChar) - 1);
    {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
      (Key, Pointer(Ident), nil, @ValType, Pointer(Result), @Len);
  end;
end;

var
  Branch: CoreString;
begin
  case CodePage of
    CP_ACP:
      CodePage := GetACP;
    CP_OEMCP:
      CodePage := GetOEMCP;
  end;
  Branch := 'MIME\Database\Codepage\' + IntToStr(CodePage);
  if {$IFDEF UNICODE} RegOpenKeyExW {$ELSE} RegOpenKeyExA {$ENDIF}
    (HKEY_CLASSES_ROOT, Pointer(Branch), 0, KEY_QUERY_VALUE, Key) = ERROR_SUCCESS then
  begin
    QueryKey('WebCharset');
    if Result = '' then
      QueryKey('BodyCharset');
  end
  else
    Result := '';
end;*)

{ TWikiLink }

destructor TWikiLink.Destroy;
begin
  FPathPlus.Free;
  FPath.Free;
  FPrefix.Free;
//  inherited;
end;

{ TWikiItem }

destructor TWikiItem.Destroy;
begin
  with TWikiText(Self) do
  begin
    FStyle.Free;
    FAnchor.Free;
    FLanguage.Free;
    FItems.Free;
  end;
  inherited;
end;

procedure TWikiItem.Extract;
begin
  TWikiText(Self).FParent := nil;
  inherited;
end;

function TWikiItem.LanguageContext: TLegacyString;

function LanguageOf(Item: TWikiBlock): TLegacyString;
begin
  repeat
    Result := Item.FLanguage;
    if Result <> nil then
      Exit;
    Item := Item.FParent;
  until Item = nil;
end;

begin
  Result := LanguageOf(TWikiBlock(Self));
end;

{ TWikiText }

destructor TWikiText.Destroy;
begin
  FLink.Free;
  FValue.Free;
  inherited;
end;

function TWikiText.Parse(Source, Limit: PLegacyChar): PLegacyChar;
var
  T: TTextType;
  Tag: LegacyChar;
begin
  while not (Source^ in [#13, #10]) and (Source < Limit) do
    for T := Low(TextTags) to High(TextTags) do
    begin
      Tag := TextTags[T];
      if (Source^ = Tag) and (Source[1] = Tag) then
    end;
end;

{ TWikiParagraph }

function TWikiParagraph.Parse(Source, Limit: PLegacyChar): PLegacyChar;
begin
  Result := Limit; // TODO
end;

{ TWikiGroup }

function TWikiGroup.Parse(Source, Limit: PLegacyChar): PLegacyChar;
begin
  Result := Limit; // TODO
end;

{ TWikiDocument }

destructor TWikiDocument.Destroy;
begin
  FCluster.Free;
  FSource.Free;
  FTitle.Free;
  FCharSet.Free;
  FName.Free;
  FPath.Free;
  FItems.Free;
  FLanguage.Free;
  inherited;
end;

procedure TWikiDocument.Clear;
begin
  if FCluster <> nil then
    FCluster.Clear;
  if FItems <> nil then
    FItems.Clear;
  if FSource <> nil then
    FSource.Clear;
  if FTitle <> nil then
    FTitle.Clear;
  if FPath <> nil then
    FPath.Clear;
  if FName <> nil then
    FName.Clear;
  if FLanguage <> nil then
    FLanguage.Clear;
  if FCharSet <> nil then
    FCharSet.Clear;
{$IFNDEF Lite}
  FIsUTF8 := False;
{$ENDIF}
end;

procedure TWikiDocument.Load(Stream: TReadableStream; WikiMode: Boolean);
var
  P, Limit: PLegacyChar;
begin
{$IFNDEF Lite}
  Clear;
{$ENDIF}  
  if FSource = nil then
    FSource := TLegacyString.Create;
  with FSource do
  begin
    Load(Stream);
    P := Data;
    Limit := P + Count;
  end;
  while P < Limit do
    P := Parse(P, Limit, WikiMode);
end;

procedure TWikiDocument.Load(FileName: PCoreChar; WikiMode: Boolean);
var
  F: TReadableStream;
begin
  F := TFileStream.Create(FileName, faRead);
  try
    Load(F, WikiMode);
  finally
    F.Free;
  end;
end;

function TWikiDocument.Parse(P, Limit: PLegacyChar; WikiMode: Boolean): PLegacyChar;
begin
  Result := Limit; // TODO
end;

function TWikiDocument.Compare(Item: TBalancedTreeItem): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    TWikiDocument(Item).FPath.Data, TWikiDocument(Item).FPath.Count, FPath.Data, FPath.Count) - 2;
end;

end.

