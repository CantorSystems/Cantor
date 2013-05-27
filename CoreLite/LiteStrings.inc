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
    procedure CheckIndex(Index: Integer); override;
  public
    procedure Clear; override;
    procedure Insert(Source: Pointer; Count: Integer; SourceOptions: TStringOptions = []; DestIndex: Integer = 0);

  // Format still without Count parameter due to Windows implementation uses ASCIIZ
    procedure Format(Source: Pointer; const Args: array of const); overload; virtual; abstract;
    procedure Format(const Args: array of const); overload;

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
    procedure Format(Source: Pointer; const Args: array of const); override;

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
    procedure Format(Source: Pointer; const Args: array of const); override;

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
    procedure Format(Source: Pointer; const Args: array of const); override;

    property Data: PQuadChar read FData write SetData;
    property Options: TEndianOptions read FOptions;
  end;

