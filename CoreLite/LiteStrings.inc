(*
    Lite Core Library (CoreLite)

    Lite strings declaration -- no character set translation and code page support

    Copyright (c) 2012-2013 Vladislav Javadov (Freeman)
*)

type
  TString = class(TSubstring)
  private
  { placeholder } // FData: Pointer;
  { placeholder } // FOptions: TStringOptions;
      procedure SetCount(Value: Integer);
  protected
    procedure AcceptRange(Index, Count: Integer);
    procedure CheckIndex(Index: Integer); override;
  public
    procedure Clear; override;
    procedure Insert(Source: Pointer; Count: Integer; SourceOptions: TStringOptions = []; DestIndex: Integer = 0);

    function Load(Source: TReadableStream; SourceOptions: TStringOptions = []; DestIndex: Integer = 0): Integer; overload;
    function Load(FileName: PCoreChar; SourceOptions: TStringOptions = []; DestIndex: Integer = 0): Integer; overload;

    property Count write SetCount;
  end;

  TLegacyString = class(TString)
  private
  { hold } FData: PLegacyChar;
  { hold } FOptions: TLegacyOptions;
    procedure SetData(Value: PLegacyChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
  {$ENDIF}
    procedure Format(Source: PLegacyChar; const Args: array of const); overload;

    property Data: PLegacyChar read FData write SetData;
    property Options: TLegacyOptions read FOptions;
  end;

{$IFNDEF Lite}
  TEndianString = class(TString)
  public
    procedure SwapByteOrder; virtual; abstract;
  end;
{$ELSE}
  TEndianString = TString;
{$ENDIF}

  TWideString = class(TEndianString)
  private
  { hold } FData: PWideChar;
  { hold } FOptions: TEndianOptions;
    procedure SetData(Value: PWideChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    procedure SwapByteOrder; override;
  {$ENDIF}
    procedure Format(Source: PWideChar; const Args: array of const); overload;

    property Data: PWideChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TQuadString = class(TEndianString)
  private
  { hold } FData: PQuadChar;
  { hold } FOptions: TEndianOptions;
    procedure SetData(Value: PQuadChar);
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
    procedure SwapByteOrder; override;
  {$ENDIF}

    property Data: PQuadChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

  TSharedString = class(TString)
  private
  { hold } FData: Pointer;
  { hold } FOptions: TStringOptions;
  { hold to TLegacyString } FCodePage: TCodePage;
    FParent: TString; // use soAttachBuffer to share other shared string
    function GetAsLegacyChar: PLegacyChar;
    function GetAsWideChar: PWideChar;
    procedure SetAsLegacyChar(Value: PLegacyChar);
    procedure SetAsWideChar(Value: PWideChar);
  {$IFDEF UTF32}
    function GetAsQuadChar: PQuadChar;
    procedure SetAsQuadChar(Value: PQuadChar);
  {$ENDIF}
  public
    {class} function ByteCount(Count: Integer): Integer; override;
    procedure Clear; override;
    {class} function Length(Source: Pointer): Integer; override;
  {$IFNDEF Lite}
    {class} function Length(Source: Pointer; MaxLength: Integer): Integer; override;
  {$ENDIF}

    function Insert(var Info: TStringInfo; Source: PLegacyChar; Count: Integer; CodePage: TCodePage = nil;
      SourceOptions: TLegacySource = []; DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
    function Insert(var Info: TStringInfo; Source: PWideChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$IFDEF UTF32}
    function Insert(var Info: TStringInfo; Source: PQuadChar; Count: Integer; SourceOptions: TEndianSource = [];
      DestIndex: Integer = 0; DestOptions: TEncodeOptions = []): Integer; overload;
  {$ENDIF}

    property AsCoreChar: PWideChar read GetAsWideChar write SetAsWideChar; // TODO: non-Unicode
    property AsLegacyChar: PLegacyChar read GetAsLegacyChar write SetAsLegacyChar;
    property AsWideChar: PWideChar read GetAsWideChar write SetAsWideChar;
  {$IFDEF UTF32}
    property AsQuadChar: PQuadChar read GetAsQuadChar write SetAsQuadChar;
  {$ENDIF}
    property CodePage: TCodePage read FCodePage;
    property Data: Pointer read FData;
    property Options: TStringOptions read FOptions;
    property Parent: TString read FParent;
  end;

  
