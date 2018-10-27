(*
    Lite Core Library (CoreLite)

    Core strings and character sets implementation

    Copyright (c) 2015-2018 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * CoreLiteVCL -- TCoreString and UnicodeString types for VCL applications
      * Debug -- diagnostic exceptions
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses;

type
{
  soDetectUTF8:
    * try to decode source as UTF-8, continue as code page or Latin1 if code page is null
}
  TStringOption = (soDetectUTF8, soBigEndian, soAttachBuffer, soNullTerminated);
const
  soLatin1 = soBigEndian;
  soAttach = [soAttachBuffer];
  soBuiltIn = [soAttachBuffer, soNullTerminated];
  soFromTheWild = [soDetectUTF8];

type
  TRawByteOptions = set of soDetectUTF8..soLatin1;
  TEndianOptions = set of soBigEndian..soBigEndian;

  TStringSource = set of TStringOption;
  TRawByteSource = set of soDetectUTF8..soNullTerminated;
  TEndianSource = set of soBigEndian..soNullTerminated;
{
  coReplaceInvalid -- replace invalid characters with:
    * U+007F -- for code page and Latin1 (used instead of U+001A to avoid compatibility issues)
    * U+FFFD -- for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coAttachBuffer, coSurrogatePairs, coBigEndian, coReplaceInvalid, coSysReplacementChar);
const
  coLatin1 = coBigEndian; // only without coCESU8

//  coUTF8 = []; // that's by default
  coCESU8 = coSurrogatePairs;
  coEncodeZero = coBigEndian;  // with coCESU8 only, otherwise is coLatin1
  coModifiedUTF8 = [coCESU8, coEncodeZero];
  coUTF16 = [coSurrogatePairs];
  coAttach = [coAttachBuffer];

type
  TEncodeCodePage = set of coReplaceInvalid..coSysReplacementChar;
  TEncodeRawBytes = set of coSurrogatePairs..coSysReplacementChar;
  TEncodeUTF16 = set of coSurrogatePairs..coReplaceInvalid;
//  TEncodeUTF32 = set of coBigEndian..coReplaceInvalid;

  TEncodeOptions = TEncodeRawBytes;

  TAggregateRawBytes = set of coAttachBuffer..coSysReplacementChar;
  TAggregateUTF16 = set of coAttachBuffer..coReplaceInvalid;
//  TEncodeUTF32 = set of coBigEndian..coReplaceInvalid; // order fail

  TAggregateOptions = TAggregateRawBytes;

const
  FirstLeadByte = LegacyChar(#$80); // for Unicode RTL
  LastLeadByte = LegacyChar(#$FF);
type
  TLeadByte = FirstLeadByte..LastLeadByte;
  TLeadBytes = set of TLeadByte;

  TSurrogatePair = packed record
    HighSurrogate, LowSurrogate: WideChar; // always big-endian
  end;

  TInvalidUTF8 = record
    case Byte of
      0: (StartingByte,           // Bad UTF-8 sequence starting with byte $%02X
          BrokenSequence: Byte);  // Broken %u-byte UTF-8 sequence starting with byte $%02X
      1: (RawData: Word);
  end;

  TErrorInfo = record
    case Byte of
      0: (InvalidChar: QuadChar;
          InvalidUTF8: TInvalidUTF8);
      1: (RawData: QuadWord);
  end;

  TConvertResult = record
    SourceCount, DestCount, SuccessBytes, // SuccessBytes < 0 for Latin1
    SurrogatePairs: Integer;
    ErrorInfo: TErrorInfo;
  end;

{$IF defined(CoreLiteVCL) and not UnicodeRTL}
  RawByteString = AnsiString;
  UnicodeString = WideString;
{$IFEND}

  PLegacyString = ^TLegacyString;
  PWideString = ^TWideString;

  PCodePage = ^TCodePage;
  TCodePage = object
  private
    FNumber: Word;
    FLocaleDependent: Boolean;
    FMaxCharBytes: Byte;
    FReplacementChar, FSysReplacementChar: LegacyChar;
    FLeadBytes: TLeadBytes;
    FName: TCodePageName;
  public
    constructor Create(CodePage: Word = CP_THREAD_ACP; DefaultReplacementChar: LegacyChar = LegacyReplacementChar);
    function DecodeUTF16(Source: PWideString; Dest: PLegacyString; DestIndex: Integer;
      EncodeOptions: TEncodeCodePage): Integer;
    function EncodeUTF16(Source: PLegacyString; Dest: PWideString; DestIndex: Integer;
      EncodeOptions: TEncodeUTF16): Integer;

    property LeadBytes: TLeadBytes read FLeadBytes;
    property LocaleDependent: Boolean read FLocaleDependent;
    property MaxCharBytes: Byte read FMaxCharBytes;
    property Name: TCodePageName read FName;
    property Number: Word read FNumber;
    property ReplacementChar: LegacyChar read FReplacementChar write FReplacementChar;
    property SysRelpacementChar: LegacyChar read FSysReplacementChar;
  end;

  TBinaryData = record
    Data: Pointer;
    Length: Integer;
  end;

  PStringFormatter = ^TStringFormatter;

  PString = ^TString;
  TString{<A, F>} = object(TCollection{<A>})
  private
  // FOptions: generic <F> as TStringOptions;
  protected
    procedure Assign(Source: Pointer; Length: Integer; Options: TStringSource); overload;
    function AssignArray(Index: Integer; Formatter: PStringFormatter;
      const Args: array of const): Integer;
    function CharSetName(EncodeOptions: TEncodeOptions): PLegacyChar;
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual; abstract;
    procedure SetRawByteString(Value: RawByteString); virtual; abstract;
    function GetUnicodeString: UnicodeString; virtual; abstract;
    procedure SetUnicodeString(Value: UnicodeString); virtual; abstract;
  {$ENDIF}
  public
    procedure AsArray(Formatter: PStringFormatter; const Args: array of const);
    function AsBinaryData(DetachBuffer: Boolean): TBinaryData;
    function AsHexadecimal: QuadInt; overload;
    function AsInteger: QuadInt; overload;
  {$IFDEF CoreLiteVCL}
    property AsRawByteString: RawByteString read GetRawByteString write SetRawByteString;
    property AsUnicodeString: UnicodeString read GetUnicodeString write SetUnicodeString;
  {$ENDIF}
    function TryHexadecimal(var Value: QuadInt): Boolean; overload;
    function TryInteger(var Value: QuadInt): Boolean; overload;
  end;

  TLegacyString = object(TString{<PLegacyChar, TRawByteOptions>})
  private
    FData: PLegacyChar;        // specialize <A>
    FOptions: TRawByteOptions; // specialize <F>
    FCodePage: PCodePage;
    function AssignDigits(Index: Integer; Digits: PLegacyChar;
      Length, MinWidth: Integer; FillChar: LegacyChar): Integer;
    function GetData: PLegacyChar;
    procedure SetCodePage(Value: PCodePage);
    procedure SetData(Value: PLegacyChar);
  protected
    class function CollectionInfo: TCollectionInfo; virtual;

    procedure AssignHexBuffer(Index: Integer; const Buffer; Length: Integer;
      LowerCase: Boolean = False);
    function AssignHexadecimal(Index: Integer; Value: QuadInt; MinWidth: Integer = 0;
      LowerCase: Boolean = False; FillChar: LegacyChar = #32): Integer;
    function AssignInteger(Index: Integer; Value: QuadInt; MinWidth: Integer = 0;
      FillChar: LegacyChar = #32): Integer;
    function AssignFloat(Index: Integer; const Value: Extended;
      MinWidth, Precision: Integer; FillChar: LegacyChar = #32): Integer;
    function AssignString(Index: Integer; Source: PLegacyString;
      EncodeOptions: TEncodeRawBytes = []): TConvertResult;
    function AssignWideString(Index: Integer; Source: PWideString;
      EncodeOptions: TEncodeRawBytes = []): TConvertResult;

    function Compatible(Value: PLegacyString): Boolean;
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual;
    procedure SetRawByteString(Value: RawByteString); virtual;
    function GetUnicodeString: UnicodeString; virtual;
    procedure SetUnicodeString(Value: UnicodeString); virtual;
  {$ENDIF}
  public
    procedure Append(Source: PLegacyChar; Length: Integer); overload;

    procedure AsHexBuffer(const Value; Length: Integer;
      LowerCase: Boolean = False);
    procedure AsHexadecimal(Value: QuadInt; MinWidth: Integer = 0;
      LowerCase: Boolean = False; FillChar: LegacyChar = #32); overload;
    procedure AsInteger(Value: QuadInt; MinWidth: Integer = 0;
      FillChar: LegacyChar = #32); overload;
    procedure AsFloat(const Value: Extended; MinWidth, Precision: Integer;
      FillChar: LegacyChar = #32);
    procedure AsPercentage(const Ratio: Double; Precision: Integer = 1;
      FillChar: LegacyChar = #32);

    function AsNextLine(Source: PLegacyString; StartIndex: Integer = 0): Integer; overload;
    function AsNextLine(Source: PLegacyString; Delimiter: LegacyChar;
      StartIndex: Integer = 0): Integer; overload;

    function AsRange(Index: Integer): TLegacyString; overload;
    function AsRange(Index, MaxCount: Integer): TLegacyString; overload;

    procedure AsString(Source: PLegacyChar; Length: Integer;
      SourceOptions: TRawByteSource = soFromTheWild); overload;
    procedure AsString(Source: PLegacyString; EncodeOptions: TEncodeRawBytes = []); overload;
    procedure AsWideString(Source: PWideString; EncodeOptions: TEncodeRawBytes = []);

    function Compare(Value: PLegacyChar; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;
    function Compare(Value: PLegacyString; IgnoreCase: Boolean = False): Integer; overload;

    procedure Detach; virtual;
    function IsBinaryData: Boolean;
    function ValidateUTF8: Integer;

    function LastIndex(Value: LegacyChar): Integer;
    function NextIndex(Index: Integer): Integer; overload;
    function NextIndex(Value: LegacyChar; StartIndex: Integer = 0): Integer; overload;
    function PrevIndex(Index: Integer): Integer; overload;
    function PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer; overload;

    function RawNextIndex(Index: Integer): Integer; overload;
    function RawNextIndex(Value: LegacyChar; StartIndex: Integer = 0): Integer; overload;
    function RawPrevIndex(Index: Integer): Integer; overload;
    function RawPrevIndex(Value: LegacyChar; StartIndex: Integer): Integer; overload;

    function RawNextLine(Source: PLegacyString; StartIndex: Integer = 0): Integer; overload;
    function RawNextLine(Source: PLegacyString; Delimiter: LegacyChar;
      StartIndex: Integer = 0): Integer; overload;

    procedure Load(Source: PReadableStream); overload;
    function Load(Source: PReadableStream; AllowBOM: Boolean;
      SourceOptions: TRawByteOptions = soFromTheWild): TReadableBOM; overload;
    procedure Save(Dest: PWritableStream); overload;
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean); overload;

    property CodePage: PCodePage read FCodePage write SetCodePage;
    property Data: PLegacyChar read GetData write SetData;
    property Options: TRawByteOptions read FOptions;
    property RawData: PLegacyChar read FData write SetData;
  end;

  TByteOrder = (boFromBOM, boConvertToNative, boLittleEndian, boBigEndian);

  PEndianString = PString;
  TEndianString{<L, F>} = TString{<L, F>};

(*  PEndianString = ^TEndianString;
  TEndianString{<L, F>} = object(TString{<L, F>})
  protected
  //  procedure SwapByteOrder(Index, Length: Integer); overload; virtual; abstract;
  public
  //  procedure SwapByteOrder; overload;
  end;*)

  TWideString = object(TEndianString{<PWideChar, TEndianOptions>})
  private
    FData: PWideChar;         // specialize <A>
    FOptions: TEndianOptions; // specialize <F>
    FDataSource: PLegacyString;
    function AssignDigits(Index: Integer; Digits: PLegacyChar;
      Length, MinWidth: Integer; FillChar: WideChar): Integer;
    function GetData: PWideChar;
    procedure SetData(Value: PWideChar);
  protected
    class function CollectionInfo: TCollectionInfo; virtual;

    procedure AssignHexBuffer(Index: Integer; const Buffer; Length: Integer;
      LowerCase: Boolean = False);
    function AssignHexadecimal(Index: Integer; Value: QuadInt; MinWidth: Integer = 0;
      LowerCase: Boolean = False; FillChar: WideChar = #32): Integer;
    function AssignInteger(Index: Integer; Value: QuadInt; MinWidth: Integer = 0;
      FillChar: WideChar = #32): Integer;
    function AssignFloat(Index: Integer; const Value: Extended;
      MinWidth, Precision: Integer; FillChar: WideChar = #32): Integer;
    function AssignString(Index: Integer; Source: PLegacyString;
      EncodeOptions: TEncodeUTF16 = coUTF16): TConvertResult;
    function AssignWideString(Index: Integer; Source: PWideString;
      EncodeOptions: TEncodeUTF16 = coUTF16): TConvertResult;

    procedure SwapByteOrder(Index, Length: Integer); overload;
  {$IFDEF CoreLiteVCL}
    function GetRawByteString: RawByteString; virtual;
    procedure SetRawByteString(Value: RawByteString); virtual;
    function GetUnicodeString: UnicodeString; virtual;
    procedure SetUnicodeString(Value: UnicodeString); virtual;
  {$ENDIF}
  public
    procedure Append(Source: PWideChar; Length: Integer); overload;

    procedure AsHexBuffer(const Value; Length: Integer;
      LowerCase: Boolean = False);
    procedure AsHexadecimal(Value: QuadInt; MinWidth: Integer = 0;
      LowerCase: Boolean = False; FillChar: WideChar = #32); overload;
    procedure AsInteger(Value: QuadInt; MinWidth: Integer = 0;
      FillChar: WideChar = #32); overload;
    procedure AsFloat(const Value: Extended; MinWidth, Precision: Integer;
      FillChar: WideChar = #32);
    procedure AsPercentage(const Ratio: Double; Precision: Integer = 1;
      FillChar: WideChar = #32);

    function AsNextLine(Source: PWideString; StartIndex: Integer = 0): Integer; overload;
    function AsNextLine(Source: PWideString; Delimiter: WideChar;
      StartIndex: Integer = 0): Integer; overload;

    function AsRange(Index: Integer): TWideString; overload;
    function AsRange(Index, MaxCount: Integer): TWideString; overload;

    procedure AsString(Source: PLegacyString; EncodeOptions: TEncodeUTF16 = coUTF16);
    procedure AsWideString(Source: PWideChar; Length: Integer;
      SourceOptions: TEndianSource = []); overload;
    procedure AsWideString(Source: PWideString; EncodeOptions: TEncodeUTF16 = coUTF16); overload;

    function Compare(Value: PWideChar; Length: Integer;
      IgnoreCase: Boolean = False): Integer; overload;
    function Compare(Value: PWideString; IgnoreCase: Boolean = False): Integer; overload;

    procedure Detach; virtual;

    function LastIndex(Value: WideChar): Integer;
    function NextIndex(Value: WideChar; StartIndex: Integer = 0): Integer;
    function PrevIndex(Value: WideChar; StartIndex: Integer): Integer;

    function RawNextIndex(Value: WideChar; StartIndex: Integer = 0): Integer;
    function RawPrevIndex(Value: WideChar; StartIndex: Integer): Integer;

    function RawNextLine(Source: PWideString; StartIndex: Integer = 0): Integer; overload;
    function RawNextLine(Source: PWideString; Delimiter: WideChar;
      StartIndex: Integer = 0): Integer; overload;

    procedure Load(Source: PReadableStream); overload;
    function Load(Source: PReadableStream; ByteOrder: TByteOrder;
      FallbackCP: PCodePage = nil): TReadableBOM; overload;
    procedure Save(Dest: PWritableStream); overload;
    procedure Save(Dest: PWritableStream; WriteBOM: Boolean); overload;

    procedure SwapByteOrder; overload;

    property Data: PWideChar read GetData write SetData;
    property Options: TEndianOptions read FOptions;
    property RawData: PWideChar read FData write SetData;
  end;

  PCoreString = PWideString;
  TCoreString = TWideString;

  TBinaryScale = (bsBytes, bsKilobytes, bsMegabytes, bsGigabytes, bsTerabytes, bsPetabytes,
    bsExabytes, bsZettabytes, bsYottabytes);

  TDecimalFormat = object
    ZeroSign, ThousandSeparator, DecimalSeparator, PercentSign, BinaryScaleSeparator: LiteChar;
    BinaryScale: array[TBinaryScale] of LiteChar; // K, M, G, T, P, E, Z, Y
  protected
    procedure Init(Locale: Word; UseNativeDigits: Boolean = False);
  end;

  PDecimalFormatArray = ^TDecimalFormatArray;
  TDecimalFormatArray = array[0..MaxInt div SizeOf(TDecimalFormat) - 1] of TDecimalFormat;

  TDecimalFormats = object(TCollection{<PDecimalFormatArray>})
  private
    FItems: PDecimalFormatArray; // specialize <A>
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append(Locale: Word; UseNativeDigits: Boolean = False): Integer;
    procedure Insert(Index: Integer; Locale: Word; UseNativeDigits: Boolean = False);
    property Items: PDecimalFormatArray read FItems;
  end;

  THexadecimalFormat = record
    ChunkBytes: Integer;
    ChunkSeparator: QuadChar;
  end;

  PHexadecimalFormatArray = ^THexadecimalFormatArray;
  THexadecimalFormatArray = array[0..MaxInt div SizeOf(THexadecimalFormat) - 1] of THexadecimalFormat;

  THexadecimalFormats = object(TCollection{<PHexadecimalFormatArray>})
  private
    FItems: PHexadecimalFormatArray; // specialize <A>
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append(HexChunkBytes: Integer; HexChunkSeparator: QuadChar): Integer;
    procedure Insert(Index, HexChunkBytes: Integer; HexChunkSeparator: QuadChar);
    property Items: PHexadecimalFormatArray read FItems;
  end;

  TFormatResult = record
    SourceCount, DestCount: Integer;
    InvalidChar: QuadChar; // TODO
  end;

  TStringFormatter = object(TCoreObject) (* {“0:+-05i1?{(none)}”} *)
  private
    FDecimalFormats: TDecimalFormats;
    FHexadecimalFormats: THexadecimalFormats;
    FDefaultDecimalFormat, FDefaultHexadecimalFormat: Integer;
  protected
    function IsNullAddress(Index: Integer; const Value): Boolean; virtual;
    function IsNullCardinal(Index: Integer; Value: QuadWord): Boolean; virtual;
    function IsNullChar(Index: Integer; Value: QuadChar): Boolean; virtual;
    function IsNullFloat(Index: Integer; const Value: Extended): Boolean; virtual;
    function IsNullInteger(Index: Integer; Value: QuadInt): Boolean; virtual;
    function IsNullPercent(Index: Integer; const Value: Double): Boolean; virtual;
    function IsNullString(Index: Integer; Value: Pointer): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; virtual;
    function Estimate(const Args: array of const): Integer; overload;
    function Estimate(Fmt: PLegacyString; const Args: array of const): Integer; overload;
    function Estimate(Fmt: PWideString; const Args: array of const): Integer; overload;
//    function Format(Fmt: PLegacyString; const Args: array of const; Dest: P

    property DecimalFormats: TDecimalFormats read FDecimalFormats;
    property HexadecimalFormats: THexadecimalFormats read FHexadecimalFormats;
  end;

  PStrings = PCollections;
  TStrings{<A>} = TCollections{<A>};

  PLegacyStrings = PStrings;
  TLegacyStrings{<A>} = TStrings{<A>};

  PWideStrings = PStrings;
  TWideStrings{<A>} = TStrings{<A>};

  PLegacyStringArray = ^TLegacyStringArray;
  TLegacyStringArray = array[0..MaxInt div SizeOf(TLegacyString) - 1] of TLegacyString;

  PLegacyText = ^TLegacyText;
  TLegacyText = object(TLegacyStrings{<PLegacyStringArray>})
  private
    FItems: PLegacyStringArray; // specialize <A>
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append: PLegacyString; overload;
    property Items: PLegacyStringArray read FItems;
  end;

  PWideStringArray = ^TWideStringArray;
  TWideStringArray = array[0..MaxInt div SizeOf(TWideString) - 1] of TWideString;

  PWideText = ^TWideText;
  TWideText = object(TWideStrings{<PWideStringArray>})
  private
    FItems: PWideStringArray; // specialize <A>
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
  public
    function Append: PWideString; overload;
    property Items: PWideStringArray read FItems;
  end;

  PCoreText = PWideText;
  TCoreText = TWideText;

  PStringList = PCollectionList;
  TStringList{<I>} = TCollectionList{<I>};

  PLegacyStringList = PStringList;
  TLegacyStringList{<I>} = TStringList{<I>};

  PLegacyTextList = ^TLegacyTextList;

  PLegacyTextListItem = ^TLegacyTextListItem;
  TLegacyTextListItem = object(TLegacyString{, TListItem<PLegacyTextList, PLegacyTextListItem>})
  private
    FOwner: PLegacyTextList;           // specialize <L>
    FPrev, FNext: PLegacyTextListItem; // specialize <I>
  public
    property Next: PLegacyTextListItem read FNext;
    property Owner: PLegacyTextList read FOwner;
    property Prev: PLegacyTextListItem read FPrev;
  end;

  TLegacyTextList = object(TLegacyStringList{<PLegacyTextListItem>})
  private
    FFirst, FLast: PLegacyTextListItem; // specialize <I>
  protected
    class function ListInfo: TListInfo; virtual;
  public
    function Append: PLegacyString; overload;

    property First: PLegacyTextListItem read FFirst;
    property Last: PLegacyTextListItem read FLast;
  end;

  PWideStringList = PStringList;
  TWideStringList{<I>} = TStringList{<I>};

  PWideTextList = ^TWideTextList;

  PWideTextListItem = ^TWideTextListItem;
  TWideTextListItem = object(TWideString{, TListItem<PWideTextList, PWideTextListItem>})
  private
    FOwner: PWideTextList;           // specialize <L>
    FPrev, FNext: PWideTextListItem; // specialize <I>
  public
    property Next: PWideTextListItem read FNext;
    property Owner: PWideTextList read FOwner;
    property Prev: PWideTextListItem read FPrev;
  end;

  TWideTextList = object(TWideStringList{<PWideTextListItem>})
  private
    FFirst, FLast: PWideTextListItem; // specialize <I>
  protected
    class function ListInfo: TListInfo; virtual;
  public
    function Append: PWideString; overload;

    property First: PWideTextListItem read FFirst;
    property Last: PWideTextListItem read FLast;
  end;

  PCoreTextListItem = PWideTextListItem;
  TCoreTextListItem = TWideTextListItem;

  PCoreTextList = PWideTextList;
  TCoreTextList = TWideTextList;

{ Exceptions }

  TErrorSource = record
    case Byte of
      1: (AsString: PLegacyString);
      2: (AsWideString: PWideString);
  end;

  EString = class(Exception)
  private
    FSource: TErrorSource;
  public
    property Source: TErrorSource read FSource;
  end;

  EIntegerString = class(EString)
  private
    FHexadecimal: Boolean;
  public
    constructor Create(Source: PString; Hexadecimal: Boolean);
    property Hexadecimal: Boolean read FHexadecimal;
  end;

  TConvertMode = (cmDecodeUTF16, cmEncodeUTF16);

  EConvert = class(EString)
  private
    FMode: TConvertMode;
  public
    property Mode: TConvertMode read FMode;
  end;

  ECodePage = class(EConvert)
  private
    FErrorCode: LongWord;
    FCodePage: PCodePage;
  public
    constructor Create(Source: PString; CodePage: PCodePage; Mode: TConvertMode);
    property ErrorCode: LongWord read FErrorCode;
    property CodePage: PCodePage read FCodePage;
  end;

  TCharSet = (usLatin1, usUTF8, usCESU8, usUTF16);

  EUnicode = class(EConvert)
  private
    FErrorInfo: TErrorInfo;
    FEncodeOptions: TEncodeOptions;
  public
    constructor Create(Source: PString; EncodeOptions: TEncodeOptions; const Info: TErrorInfo);
    property EncodeOptions: TEncodeOptions read FEncodeOptions;
    property ErrorInfo: TErrorInfo read FErrorInfo;
  end;

  EBigEndian = class(EString)
  public
    constructor Create(Source: PWideString);
  end;

  EUTF32 = class(EString)
  public
    constructor Create(Source: PString);
  end;

{ Helpers }

type
  THighSurrogates = $D800..$DBFF;
  TLowSurrogates  = $DC00..$DFFF;

  TUnicodeBMP     = $000000..$00FFFF;    // Basic Multilingual Plane
  TUnicodeSMP     = $010000..$01FFFF;    // Supplementary Multilingual Plane
  TUnicodeSIP     = $020000..$02FFFF;    // Supplementary Ideographic Plane
  TUnicodeSSP     = $0E0000..$0EFFFF;    // Supplementary Special-purpose Plane
  TUnicodeSPUA    = $0F0000..$10FFFF;   // Supplementary Private Use Area
  TUnicodeSPUAA   = $0F0000..$0FFFFF;  // Supplementary Private Use Area A
  TUnicodeSPUAB   = $100000..$10FFFF;  // Supplementary Private Use Area B

  TNonUnicode     = $110000..$FFFFFFFF;

const
  AverageStringLength = 64;
  AverageStringsDelta = -4;

  DefaultDecimalFormat: TDecimalFormat = (
    ZeroSign: LiteChar('0');
    ThousandSeparator: LiteChar(0);
    DecimalSeparator: LiteChar('.');
    PercentSign: LiteChar('%');
    BinaryScaleSeparator: LiteChar(160); // non-breaking space
    BinaryScale: (LiteChar(0), LiteChar('K'), LiteChar('M'), LiteChar('G'), LiteChar('T'),
      LiteChar('P'), LiteChar('E'), LiteChar('Z'), LiteChar('Y'))
  );

type
  TTextOptions = set of (toClear, toAttach);

  TNewLegacyLineFunc = function: PLegacyString of object;
  TNewWideLineFunc = function: PWideString of object;

function LoadText(const NewLine: TNewLegacyLineFunc; Source: PLegacyString;
  DestOptions: TTextOptions = [toClear]): Integer; overload;
function LoadText(const NewLine: TNewLegacyLineFunc; Source: PLegacyString;
  Delimiter: LegacyChar; DestOptions: TTextOptions = [toClear]): Integer; overload;
function LoadText(const NewLine: TNewLegacyLineFunc; Source: PWideString; CP: PCodePage = nil;
  EncodeOptions: TEncodeRawBytes = []; ClearDest: Boolean = True): Integer; overload;
function LoadText(const NewLine: TNewLegacyLineFunc; Source: PWideString; Delimiter: WideChar;
  CP: PCodePage = nil; EncodeOptions: TEncodeRawBytes = []; ClearDest: Boolean = True): Integer; overload;

function LoadWideText(const NewLine: TNewWideLineFunc; Source: PLegacyString;
  EncodeOptions: TEncodeUTF16 = []; ClearDest: Boolean = True): Integer; overload;
function LoadWideText(const NewLine: TNewWideLineFunc; Source: PLegacyString; Delimiter: LegacyChar;
  EncodeOptions: TEncodeUTF16 = []; ClearDest: Boolean = True): Integer; overload;
function LoadWideText(const NewLine: TNewWideLineFunc; Source: PWideString;
  DestOptions: TTextOptions = [toClear]): Integer; overload;
function LoadWideText(const NewLine: TNewWideLineFunc; Source: PWideString;
  Delimiter: WideChar; DestOptions: TTextOptions = [toClear]): Integer; overload;

type  
  TEstimatedText = record
    CodePage: PCodePage;
    EstimatedLength: Integer;
    case Byte of
      1: (Latin1: Boolean);
      2: (BigEndian: Boolean);
  end;

//function EstimateText

//function SaveText(

implementation

uses
  CoreConsts;

const
  BOMCP: array[Boolean] of Word = (CP_GB18030, CP_UTF7);

{ Helpers }

function LoadText(const NewLine: TNewLegacyLineFunc; Source: PLegacyString;
  DestOptions: TTextOptions): Integer;
var
  Index: Integer;
begin
  if toClear in DestOptions then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    while Index < Source.Count do
    begin
      with NewLine^ do
      begin
        Index := RawNextLine(Source, Index);
        if not (toAttach in DestOptions) then
          Detach;
      end;
      Inc(Result);
    end;
  end;
end;

function LoadText(const NewLine: TNewLegacyLineFunc; Source: PLegacyString;
  Delimiter: LegacyChar; DestOptions: TTextOptions): Integer;
var
  Index: Integer;
begin
  if toClear in DestOptions then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    while Index < Source.Count do
    begin
      with NewLine^ do
      begin
        Index := RawNextLine(Source, Delimiter, Index);
        if not (toAttach in DestOptions) then
          Detach;
      end;
      Inc(Result);
    end;
  end;
end;

function LoadText(const NewLine: TNewLegacyLineFunc; Source: PWideString; CP: PCodePage;
  EncodeOptions: TEncodeRawBytes; ClearDest: Boolean): Integer;
var
  Index: Integer;
  Temp: TWideString;
begin
  if ClearDest then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    Temp.Create;
    while Index < Source.Count do
    begin
      Index := Temp.RawNextLine(Source, Index);
      with NewLine^ do
      begin
        CodePage := CP;
        AsWideString(@Temp, EncodeOptions);
      end;
      Inc(Result);
    end;
  end;
end;

function LoadText(const NewLine: TNewLegacyLineFunc; Source: PWideString; Delimiter: WideChar;
  CP: PCodePage; EncodeOptions: TEncodeRawBytes; ClearDest: Boolean): Integer;
var
  Index: Integer;
  Temp: TWideString;
begin
  if ClearDest then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    Temp.Create;
    while Index < Source.Count do
    begin
      Index := Temp.RawNextLine(Source, Delimiter, Index);
      with NewLine^ do
      begin
        CodePage := CP;
        AsWideString(@Temp, EncodeOptions);
      end;
      Inc(Result);
    end;
  end;
end;

function LoadWideText(const NewLine: TNewWideLineFunc; Source: PLegacyString;
  EncodeOptions: TEncodeUTF16; ClearDest: Boolean): Integer;
var
  Index: Integer;
  Temp: TLegacyString;
begin
  if ClearDest then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    Temp.Create;
    while Index < Source.Count do
    begin
      Index := Temp.RawNextLine(Source, Index);
      NewLine.AsString(@Temp, EncodeOptions);
      Inc(Result);
    end;
  end;
end;

function LoadWideText(const NewLine: TNewWideLineFunc; Source: PLegacyString;
  Delimiter: LegacyChar; EncodeOptions: TEncodeUTF16; ClearDest: Boolean): Integer;
var
  Index: Integer;
  Temp: TLegacyString;
begin
  if ClearDest then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    Temp.Create;
    while Index < Source.Count do
    begin
      Index := Temp.RawNextLine(Source, Delimiter, Index);
      NewLine.AsString(@Temp, EncodeOptions);
      Inc(Result);
    end;
  end;
end;

function LoadWideText(const NewLine: TNewWideLineFunc; Source: PWideString;
  DestOptions: TTextOptions): Integer;
var
  Index: Integer;
begin
  if toClear in DestOptions then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    while Index < Source.Count do
    begin
      with NewLine^ do
      begin
        Index := RawNextLine(Source, Index);
        if not (toAttach in DestOptions) then
          Detach;
      end;
      Inc(Result);
    end;
  end;
end;

function LoadWideText(const NewLine: TNewWideLineFunc; Source: PWideString;
  Delimiter: WideChar; DestOptions: TTextOptions): Integer;
var
  Index: Integer;
begin
  if toClear in DestOptions then
    PClearable(TMethod(NewLine).Data).Clear;
  Result := 0;
  if Source <> nil then
  begin
    Index := 0;
    while Index < Source.Count do
    begin
      with NewLine^ do
      begin
        Index := RawNextLine(Source, Delimiter, Index);
        if not (toAttach in DestOptions) then
          Detach;
      end;
      Inc(Result);
    end;
  end;
end;

{ EIntegerString }

constructor EIntegerString.Create(Source: PString; Hexadecimal: Boolean);
const
  ValueType: array[Boolean] of PLegacyChar = (sInteger, sHexadecimal);
var
  W: TWideString;
begin
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    W.Create;
    try
      if Source.CollectionInfo.ItemSize = SizeOf(WideChar) then
        W.AsRange(Source, 0) // TODO: big-endian
      else
        W.AsString(PLegacyString(Source));
      inherited Create(sInvalidInteger, DefaultSystemCodePage, [W.Data, ValueType[Hexadecimal]]);
    finally
      W.Destroy;
    end;
  end
  else
    inherited Create(sNullInteger, [ValueType[Hexadecimal]]);
  FSource.AsString := Pointer(Source);
  FHexadecimal := Hexadecimal;
end;

{ ECodePage }

constructor ECodePage.Create(Source: PString; CodePage: PCodePage; Mode: TConvertMode);
const
  ModeNames: array[TConvertMode] of PLegacyChar = (sSystemUTF16toCP, sSystemCPtoUTF16);
var
  ErrorCode: LongWord;
  Msg: TCoreStringRec;
  CharSet: PLegacyChar;
begin
  ErrorCode := GetLastError;
  if ErrorCode <> 0 then
  begin
    Msg := SysErrorMessage(ErrorCode);
    try
      inherited Create(ModeNames[Mode], DefaultSystemCodePage, [Msg.Value, CodePage.Number, CodePage.Name]);
    finally
      LocalFree(Msg.Handle);
    end;
    CharSet := nil;
  end
  else if (PWideString(Source).FDataSource <> nil) and
    (PWideString(Source).FDataSource.CollectionInfo.ItemSize = SizeOf(LegacyChar))
  then
    if PWideString(Source).FDataSource.CodePage <> nil then
    begin
      with PWideString(Source).FDataSource.CodePage^ do
        inherited Create(sCPtoCP, DefaultSystemCodePage, [Number, Name, CodePage.Number, CodePage.Name]);
      CharSet := nil;
    end
    else
      with PWideString(Source).FDataSource^ do
        CharSet := CharSetName(TEncodeRawBytes(FOptions * [soLatin1]))
  else
    CharSet := sUTF16;

  if CharSet <> nil then
    inherited Create(sUnicodetoCP, DefaultSystemCodePage, [CharSet, CodePage.Number, CodePage.Name]);

  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FErrorCode := ErrorCode;
  FCodePage := CodePage;
end;

{ EUnicode }

constructor EUnicode.Create(Source: PString; EncodeOptions: TEncodeOptions; const Info: TErrorInfo);
var
  Buf: array[0..Length(sNonBMP)+8] of LegacyChar;
  CharSet, Msg: PLegacyChar;
begin
  if Info.InvalidUTF8.RawData <> 0 then
    with Info.InvalidUTF8 do
      if BrokenSequence <> 0 then
        inherited Create(sBrokenUTF8, [BrokenSequence, StartingByte])
      else
        inherited Create(sBadUTF8, [StartingByte])
  else
  begin
    CharSet := Source.CharSetName(EncodeOptions);
    case Info.InvalidChar of
      Low(THighSurrogates)..High(THighSurrogates):
        inherited Create(sBrokenSurrogatePair, [CharSet, Info.InvalidChar]);
      Low(TLowSurrogates)..High(TLowSurrogates):
        inherited Create(sBadSurrogatePair, [CharSet, Info.InvalidChar]);
    else
      case Info.InvalidChar of
        $80..$9F:
          Msg := sInvalidChar;
        Low(TUnicodeSMP)..High(TUnicodeSPUA):
          Msg := sNonBMP;
      else
        if coLatin1 in EncodeOptions then
          Msg := sNonLatin1
        else
          Msg := sNonUnicode;
      end;
      FormatBuf(Msg, [Info.InvalidChar, WhitespaceOrLineBreak[IsConsole]], Buf);
      inherited Create(sInvalidString, [CharSet, @Buf]);
    end;
  end;
  FSource.AsString := Pointer(Source);
  FMode := Mode;
  FEncodeOptions := EncodeOptions;
  FErrorInfo := Info;
end;

{ EBigEndian }

constructor EBigEndian.Create(Source: PWideString);
begin
  inherited Create(sNotNativeUTF16BE);
  FSource.AsWideString := Source;
end;

{ EUTF32 }

constructor EUTF32.Create(Source: PString);
begin
  inherited Create(sUTF32notSupported);
  FSource.AsString := Pointer(Source);
end;

{ TCodePage }

constructor TCodePage.Create(CodePage: Word; DefaultReplacementChar: LegacyChar);
var
  P, Limit: PLegacyChar;
  Info: TCPInfoEx;
begin
  FillChar(Self, SizeOf(Self), 0);
  FReplacementChar := DefaultReplacementChar;

  if not GetCPInfoEx(CodePage, 0, Info) then
    RaiseLastPlatformError(sCodePage, CodePage);

  FNumber := Info.CodePage;
  FLocaleDependent := FNumber <> CodePage;
  FMaxCharBytes := Info.MaxCharSize;
  FSysReplacementChar := LegacyChar(Info.DefaultChar[0]);

  with ExtractCodePageName(Info) do
  begin
    Move(Value^, FName, Length * SizeOf(CoreChar));
    FName[Length] := #0;
  end;

  P := Pointer(@Info.LeadByte);
  Limit := P + SizeOf(Info.LeadByte);
  while P < Limit do
    if (P[0] <> #0) and (P[1] <> #0) then
    begin
      FLeadBytes := FLeadBytes + [P[0]..P[1]];
      Inc(P, 2);
    end
    else
      Break;
end;

function TCodePage.DecodeUTF16(Source: PWideString; Dest: PLegacyString;
  DestIndex: Integer; EncodeOptions: TEncodeCodePage): Integer;
var
  Replacement: PLegacyChar;
  ReplacementUsed: LongBool;
  ReplaceResult: ^LongBool;
  IsUnicode: Boolean;
begin
{$IFDEF Debug}
  if soBigEndian in Source.Options then
    raise EBigEndian.Create(Source);
{$ENDIF}

  IsUnicode := FNumber - CP_UTF7 in [0..1];
  if IsUnicode then
  begin
    Replacement := nil;
    ReplaceResult := nil;
    ReplacementUsed := False;
  end
  else
  begin
    if coSysReplacementChar in EncodeOptions then
      Replacement := @FSysReplacementChar
    else
      Replacement := @FReplacementChar;
    ReplaceResult := @ReplacementUsed;
  end;

  Result := {$IFDEF Tricks} System. {$ENDIF} WideCharToMultiByte(
    FNumber, WC_NO_BEST_FIT_CHARS and (Integer(IsUnicode) - 1), Source.RawData, Source.Count,
    Dest.RawData + DestIndex, Dest.Capacity - DestIndex, Replacement, Pointer(ReplaceResult)
  );
  if ReplacementUsed then
    Result := -Result;
end;

function TCodePage.EncodeUTF16(Source: PLegacyString; Dest: PWideString;
  DestIndex: Integer; EncodeOptions: TEncodeUTF16): Integer;
begin
  Result := {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar(
    FNumber, MB_ERR_INVALID_CHARS and (Integer((coReplaceInvalid in EncodeOptions) or (FNumber = CP_GB18030)) - 1),
    Source.RawData, Source.Count, Dest.RawData + DestIndex, Dest.Capacity - DestIndex
  );
  if coBigEndian in EncodeOptions then
    SwapWideCharBytes(Dest.RawData + DestIndex, Dest.RawData + DestIndex, Result);
end;

{ TString }

procedure TString.Assign(Source: Pointer; Length: Integer; Options: TStringSource);
begin
  if Source <> nil then
    if soAttachBuffer in Options then
      inherited Assign(Source, Length, Length + Integer(soNullTerminated in Options), True)
    else
    begin
      inherited Assign(Source, Length, Length + SizeOf(WideChar), False);
      PWideChar(PLegacyString(@Self).RawData + Length * CollectionInfo.ItemSize)^ := #0;
    end
  else
    Clear;
  PLegacyString(@Self).FOptions := Options - soBuiltIn;
end;

function TString.AssignArray(Index: Integer; Formatter: PStringFormatter;
  const Args: array of const): Integer;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  Result := 0; // TODO
end;

procedure TString.AsArray(Formatter: PStringFormatter; const Args: array of const);
var
  Length: Integer;
begin
  Clear;
  Length := Formatter.Estimate(Args);
  if Length <> 0 then
  begin
    Inc(Length, SizeOf(WideChar));
    if (BufferKind = bkAttached) or (Capacity < Length)  then
      Capacity := Length;
    Length := AssignArray(0, Formatter, Args);
    Append(Length);
    PWideChar(PLegacyString(@Self).FData + Length * CollectionInfo.ItemSize)^ := #0;
  end
end;

function TString.AsBinaryData(DetachBuffer: Boolean): TBinaryData;
begin
  if DetachBuffer then
  begin
    Detach;
    inherited Attach;
  end;
  Result.Data := PLegacyString(@Self).FData;
  Result.Length := Count;
end;

function TString.AsHexadecimal: QuadInt;
begin
  if not TryHexadecimal(Result) then
    raise EIntegerString.Create(@Self, True);
end;

function TString.AsInteger: QuadInt;
begin
  if not TryInteger(Result) then
    raise EIntegerString.Create(@Self, False);
end;

function TString.CharSetName(EncodeOptions: TEncodeOptions): PLegacyChar;
begin
  if CollectionInfo.ItemSize = SizeOf(LegacyChar) then
    if coSurrogatePairs in EncodeOptions then
      Result := sCESU8
    else if coLatin1 in EncodeOptions then
      Result := sLatin1
    else
      Result := sUTF8
  else
    Result := sUTF16;
end;

function TString.TryHexadecimal(var Value: QuadInt): Boolean;
const
  UpperCaseMask = not $20;
var
  ItemSize: Integer;
  Digit, Limit: PLegacyChar;
begin
  if Count <> 0 then
  begin
    ItemSize := CollectionInfo.ItemSize;
    Digit := PLegacyString(@Self).FData;
    Limit := Digit + Count * ItemSize;
    Value := 0;
    while Digit < Limit do
    begin
      if (ItemSize = 1) or (Digit[1] = #0) then
        case Digit^ of
          '0'..'9':
            begin
              Value := Value shl 4 + PByte(Digit)^ - Byte('0');
              Inc(Digit, ItemSize);
              Continue;
            end;
          'A'..'F', 'a'..'f':
            begin
              Value := Value shl 4 + PByte(Digit)^ and UpperCaseMask - Byte('A') + $A;
              Inc(Digit, ItemSize);
              Continue;
            end;
        end;
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function TString.TryInteger(var Value: QuadInt): Boolean;
var
  ItemSize: Integer;
  Digit, Limit: PLegacyChar;
  Minus: Boolean;
begin
  if Count <> 0 then
  begin
    ItemSize := CollectionInfo.ItemSize;
    Digit := PLegacyString(@Self).FData;
    Limit := Digit + Count * ItemSize;
    Value := 0;

    Minus := Digit^ = '-';
    if (Digit^ in ['+', '-']) and ((ItemSize = 1) or (Digit[1] = #0)) then // Fast core
      Inc(Digit, ItemSize);

    while Digit < Limit do
      if (Digit^ in ['0'..'9']) and ((ItemSize = 1) or (Digit[1] = #0)) then // Fast core
      begin
        Value := Value * 10 + PByte(Digit)^ - Byte('0');
        Inc(Digit, ItemSize);
      end
      else
      begin
        Result := False;
        Exit;
      end;

    if Minus then
      Value := -Value;
  end;
  Result := True;
end;

{ TLegacyString }

class function TLegacyString.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sLegacyString;
    ItemSize := SizeOf(LegacyChar);
  end;
end;

procedure TLegacyString.Append(Source: PLegacyChar; Length: Integer);
begin
  inherited Append(Source, Length, Length + 1);
  FData[Count] := #0;
end;

function TLegacyString.AssignDigits(Index: Integer; Digits: PLegacyChar;
  Length, MinWidth: Integer; FillChar: LegacyChar): Integer;
begin
  if Length < Abs(MinWidth) then
  begin
    if MinWidth < 0 then
    begin
      Result := -MinWidth - Length;
      System.FillChar(FData[Index], Result, FillChar);
      Move(Digits^, FData[Index + Result], Length);
      Inc(Result, Length);
    end
    else
    begin
      Move(Digits^, FData[Index], Length);
      System.FillChar(FData[Index + Length], MinWidth - Length, FillChar);
      Result := MinWidth;
    end;
    Exit;
  end
  else
    Move(Digits^, FData[Index], Length);
  Result := Length;
end;

procedure TLegacyString.AssignHexBuffer(Index: Integer; const Buffer;
  Length: Integer; LowerCase: Boolean);
var
  LowerCaseMask: Word;
  I: Integer;
  B: Byte;
  Dest: PWord;
begin
  LowerCaseMask := Byte(LowerCase) * $20;
  Inc(LowerCaseMask, LowerCaseMask shl 8);

  Dest := PWord(FData + Index);
  for I := 0 to Length - 1 do
  begin
    B := TByteArray(Buffer)[I];
    Dest^ := Byte(HexDigits[B shr 4]) or
      (Byte(HexDigits[B and $F]) shl 8) or LowerCaseMask; // Fast core
    Inc(Dest);
  end;
end;

function TLegacyString.AssignHexadecimal(Index: Integer; Value: QuadInt;
  MinWidth: Integer; LowerCase: Boolean; FillChar: LegacyChar): Integer;
begin
  with FormatHexadecimal(Value, MinWidth, LowerCase) do
    Result := AssignDigits(Index, Digits, PLegacyChar(@Data) + Length(Data) - Digits, MinWidth, FillChar);
end;

function TLegacyString.AssignInteger(Index: Integer; Value: QuadInt;
  MinWidth: Integer; FillChar: LegacyChar): Integer;
begin
  with FormatInteger(Value, MinWidth) do
    Result := AssignDigits(Index, Digits, PLegacyChar(@Data) + Length(Data) - Digits, MinWidth, FillChar);
end;

function TLegacyString.AssignFloat(Index: Integer; const Value: Extended;
  MinWidth, Precision: Integer; FillChar: LegacyChar): Integer;
var
  Digits: string[DecimalExtended];
begin
  Str(Value:Abs(MinWidth):Precision, Digits);
  Result := AssignDigits(Index, PLegacyChar(@Digits[1]), Length(Digits), MinWidth, FillChar);
end;

function TLegacyString.AssignString(Index: Integer; Source: PLegacyString;
  EncodeOptions: TEncodeRawBytes): TConvertResult;
var
  W: TWideString;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  if soDetectUTF8 in FOptions then
    ValidateUTF8;
  if Source.Compatible(@Self) then
  begin
    Move(FData[Index], Source.RawData^, Source.Count);
    FillChar(Result, SizeOf(Result), 0);
    Result.SourceCount := Source.Count;
    Result.DestCount := Source.Count;
  end
  else
  begin
    W.Create;
    try
      W.AsString(Source);
      Result := AssignWideString(Index, @W, EncodeOptions);
    finally
      W.Destroy;
    end;
  end;
end;

function TLegacyString.AssignWideString(Index: Integer; Source: PWideString;
  EncodeOptions: TEncodeRawBytes): TConvertResult;
var
  CodePoint: QuadChar;
  Paired, W: Word;
  Dest, Limit: PLegacyChar;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if FCodePage <> nil then
  begin
    Result.DestCount := FCodePage.DecodeUTF16(Source, @Self, Index, EncodeOptions);
    if Result.DestCount > 0 then
      Result.SourceCount := Source.Count;
    Exit;
  end;

  Dest := FData + Index;
  Limit := FData + Capacity;

  while (Dest < Limit) and (Result.SourceCount < Source.Count) do
  begin
    CodePoint := Word(Source.FData[Result.SourceCount]);
    if soBigEndian in Source.FOptions then
      CodePoint := Swap(CodePoint);
    Inc(Result.SourceCount);

    case Word(CodePoint) of
      $00..$7F:
        begin
          if (CodePoint = 0) and (EncodeOptions * coModifiedUTF8 = coModifiedUTF8) then
          begin
            if Limit - Dest < 2 then
            begin
              Dec(Result.SourceCount);
              Break;
            end;
            PWord(Dest)^ := $80C0; // Fast core
            Inc(Dest, SizeOf(Word));
            Inc(Result.SuccessBytes, SizeOf(Word));
          end
          else
          begin
            Dest^ := LegacyChar(CodePoint);
            Inc(Dest);
          end;
          Continue;
        end;

      Low(THighSurrogates)..High(THighSurrogates):
        begin
          Paired := Word(Source.FData[Result.SourceCount]);
          if soBigEndian in Source.FOptions then
            Paired := Swap(Paired);
          Inc(Result.SourceCount);

          case Paired of
            Low(TLowSurrogates)..High(TLowSurrogates):
              begin
                CodePoint := (CodePoint - Low(THighSurrogates)) shl 10 +
                  Low(TUnicodeSMP) + Paired - Low(TLowSurrogates);
                if coCESU8 in EncodeOptions then
                begin
                  if Limit - Dest < 6 then
                  begin
                    Dec(Result.SourceCount, 2);
                    Break;
                  end;
                  W := CodePoint;
                  PLongWord(Dest)^ := // Fast core
                    ($E0 or Byte(W shr 12)) or
                    (($80 or (Byte(W shr 6) and $3F)) shl 8) or
                    (($80 or Byte(W and $3F)) shl 16) or
                    (($E0 or Byte(Paired shr 12)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  PWord(Dest)^ :=
                    (($80 or (Byte(Paired shr 6) and $3F)) shl 8) or
                    (($80 or Byte(Paired and $3F)) shl 16);
                  Inc(Dest, SizeOf(Word));
                  with Result do
                  begin
                    Inc(SuccessBytes, SizeOf(LongWord) + SizeOf(Word));
                    Inc(SurrogatePairs);
                  end;
                end
                else if not (coLatin1 in EncodeOptions) then
                begin
                  if Limit - Dest < 4 then
                  begin
                    Dec(Result.SourceCount, 2);
                    Break;
                  end;
                  PLongWord(Dest)^ := // Fast core
                    ($F0 or Byte(CodePoint shr 18)) or
                    (($80 or Byte(CodePoint shr 12)) shl 8) or
                    (($80 or (Byte(CodePoint shr 6) and $3F)) shl 16) or
                    (($80 or Byte(CodePoint and $3F)) shl 24);
                  Inc(Dest, SizeOf(LongWord));
                  with Result do
                  begin
                    Inc(SuccessBytes, SizeOf(LongWord));
                    Inc(SurrogatePairs);
                  end;
                end
                else if coReplaceInvalid in EncodeOptions then
                begin
                  Dest^ := LegacyReplacementChar;
                  Inc(Dest);
                  Continue;
                end
                else
                begin
                  Result.ErrorInfo.InvalidChar := CodePoint;
                  Break;
                end;
              end;
          end;
        end;

      Low(TLowSurrogates)..High(TLowSurrogates), $FFFE, $FEFF: // UTF-16 BOM
        if coReplaceInvalid in EncodeOptions then
        begin
          if EncodeOptions * coModifiedUTF8 = [coLatin1] then
          begin
            Dest^ := LegacyReplacementChar;
            Inc(Dest);
          end
          else
          begin
            if Limit - Dest < 3 then
            begin
              Dec(Result.SourceCount);
              Break;
            end;
            PLongWord(Dest)^ := $BDBFEF; // U+FFFD in UTF-8
            Inc(Dest, 3);
            Inc(Result.SuccessBytes, 3);
          end;
          Continue;
        end
        else
        begin
          Result.ErrorInfo.InvalidChar := CodePoint;
          Break;
        end;
    else
      if EncodeOptions * coModifiedUTF8 = [coLatin1] then
      begin
        case Word(CodePoint) of
          $A0..$FF:
            begin
              Dest^ := LegacyChar(CodePoint);
              Dec(Result.SuccessBytes);
            end;
        else
          if coReplaceInvalid in EncodeOptions then
            Dest^ := LegacyReplacementChar
          else
          begin
            Result.ErrorInfo.InvalidChar := CodePoint;
            Break;
          end;
        end;
        Inc(Dest);
        Continue;
      end
      else
      begin
        W := CodePoint;
        case W of
          $80..$7FF:
            begin
              if Limit - Dest < 2 then
              begin
                Dec(Result.SourceCount);
                Break;
              end;
              PWord(Dest)^ := // Fast core
                ($C0 or (W shr 6)) or
                (($80 or (W and $3F)) shl 8);
              Inc(Dest, SizeOf(Word));
              Inc(Result.SuccessBytes, SizeOf(Word));
            end;
        else
          if Limit - Dest < 3 then
          begin
            Dec(Result.SourceCount);
            Break;
          end;
          PLongWord(Dest)^ := // Fast core
            ($E0 or Byte(W shr 12)) or
            (($80 or (Byte(W shr 6) and $3F)) shl 8) or
            (($80 or Byte(W and $3F)) shl 16);
          Inc(Dest, 3);
          Inc(Result.SuccessBytes, 3);
        end;
      end;
    end;
  end;

  Result.DestCount := Dest - (FData + Index);
end;

procedure TLegacyString.AsHexBuffer(const Value; Length: Integer;
  LowerCase: Boolean);
var
  L: Integer;
begin
  Clear;
  L := Length * 2;
  if (BufferKind = bkAttached) or (Capacity < L) then
    Capacity := L + 1;
  AssignHexBuffer(0, Value, Length, LowerCase);
  Append(L);
  FData[L] := #0;
end;

procedure TLegacyString.AsHexadecimal(Value: QuadInt; MinWidth: Integer;
  LowerCase: Boolean; FillChar: LegacyChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > HexQuadInt then
    Length := Abs(MinWidth)
  else
    Length := HexQuadInt;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignHexadecimal(0, Value, MinWidth, LowerCase, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TLegacyString.AsInteger(Value: QuadInt; MinWidth: Integer;
  FillChar: LegacyChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > DecimalQuadInt then
    Length := Abs(MinWidth)
  else
    Length := DecimalQuadInt;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignInteger(0, Value, MinWidth, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TLegacyString.AsFloat(const Value: Extended; MinWidth, Precision: Integer;
  FillChar: LegacyChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > DecimalExtended then
    Length := Abs(MinWidth)
  else
    Length := DecimalExtended;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignFloat(0, Value, MinWidth, Precision, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TLegacyString.AsPercentage(const Ratio: Double; Precision: Integer;
  FillChar: LegacyChar);
begin
  AsFloat(Ratio * 100, 2, Precision, FillChar);
  PWord(FData + Count)^ := Byte('%'); // Fast core
  Append(1);
end;

function TLegacyString.AsNextLine(Source: PLegacyString; StartIndex: Integer): Integer;
begin
  if Source <> nil then
  begin
    Source.CheckIndex(StartIndex);
    Result := RawNextLine(Source, StartIndex);
  end
  else
  begin
    Clear;
    Result := 0;
  end;
end;

function TLegacyString.AsNextLine(Source: PLegacyString; Delimiter: LegacyChar;
  StartIndex: Integer): Integer;
begin
  if Source <> nil then
  begin
    Source.CheckIndex(StartIndex);
    Result := RawNextLine(Source, Delimiter, StartIndex);
  end
  else
  begin
    Clear;
    Result := 0;
  end;
end;

function TLegacyString.AsRange(Index: Integer): TLegacyString;
begin
  Result.Create;
  Result.AsRange(@Self, Index);
end;

function TLegacyString.AsRange(Index, MaxCount: Integer): TLegacyString;
begin
  Result.Create;
  Result.AsRange(@Self, Index, MaxCount);
end;

procedure TLegacyString.AsString(Source: PLegacyChar; Length: Integer;
  SourceOptions: TRawByteSource);
begin
  Assign(Source, Length, SourceOptions);
end;

procedure TLegacyString.AsString(Source: PLegacyString; EncodeOptions: TEncodeRawBytes);
var
  CharBytes: Integer;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    if FCodePage <> nil then
      CharBytes := FCodePage.MaxCharBytes
    else
      CharBytes := 3;

    CharBytes := (Source.Count + 1) * CharBytes;
    if (BufferKind = bkAttached) or (Capacity < CharBytes) then
      Capacity := CharBytes;

    with AssignString(0, Source, EncodeOptions) do
    begin
      if (DestCount <= 0) and (FCodePage <> nil) then
        raise ECodePage.Create(Source, FCodePage, cmDecodeUTF16);
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);

      if FCodePage <> nil then
        Exclude(FOptions, soDetectUTF8);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

procedure TLegacyString.AsWideString(Source: PWideString; EncodeOptions: TEncodeRawBytes);
var
  CharBytes: Integer;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    if FCodePage <> nil then
      CharBytes := FCodePage.MaxCharBytes
    else
      CharBytes := 3;

    CharBytes := (Source.Count + 1) * CharBytes;
    if (BufferKind = bkAttached) or (Capacity < CharBytes) then
      Capacity := CharBytes;

    with AssignWideString(0, Source, EncodeOptions) do
    begin
      if (DestCount <= 0) and (FCodePage <> nil) and not (coReplaceInvalid in EncodeOptions) then
        raise ECodePage.Create(Source, FCodePage, cmDecodeUTF16);
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);

      if FCodePage <> nil then
        Exclude(FOptions, soDetectUTF8);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

function TLegacyString.Compare(Value: PLegacyChar; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
    Value, Count, FData, Count) - CSTR_EQUAL;
end;

function TLegacyString.Compare(Value: PLegacyString; IgnoreCase: Boolean): Integer;
begin
  if Value <> nil then
    Result := CompareStringA(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
      Value.RawData, Value.Count, FData, Count) - CSTR_EQUAL
  else
    Result := 1;
end;

function TLegacyString.Compatible(Value: PLegacyString): Boolean;
begin
  if soDetectUTF8 in FOptions then
    ValidateUTF8;
  Result := (Count = 0) or (Value.Count = 0) or (FOptions = Value.Options) or
    (FCodePage = Value.CodePage) or (FCodePage <> nil) and (FCodePage.Number = Value.CodePage.Number);
end;

procedure TLegacyString.Detach;
begin
  if BufferKind = bkAttached then
    Capacity := Count + 1;
  if Capacity > Count then
    FData[Count] := #0;
end;

function TLegacyString.GetData: PLegacyChar;
begin
  if Count <> 0 then
  begin
    if (Count < Capacity) or (FData[Count] <> #0) then
      Detach;
    Result := FData;
  end
  else
    Result := nil;
end;

function TLegacyString.IsBinaryData: Boolean;
var
  P, Limit: PLegacyChar;
begin
  P := FData;
  Limit := P + Count;
  while P < Limit do
  begin
    if P^ in [#1..#8, #11..#12, #14..#31] then
    begin
      Result := True;
      Exit;
    end;
    Inc(P);
  end;
  Result := False;
end;

function TLegacyString.LastIndex(Value: LegacyChar): Integer;
var
  S: PLegacyChar;
begin
  S := StrRScan(FData, Count, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.NextIndex(Index: Integer): Integer;
begin
  CheckIndex(Index);
  Result := RawNextIndex(Index);
end;

function TLegacyString.NextIndex(Value: LegacyChar; StartIndex: Integer): Integer;
begin
  CheckIndex(StartIndex);
  Result := RawNextIndex(Value, StartIndex);
end;

function TLegacyString.PrevIndex(Index: Integer): Integer;
begin
  CheckIndex(Index);
  Result := RawPrevIndex(Index);
end;

function TLegacyString.PrevIndex(Value: LegacyChar; StartIndex: Integer): Integer;
begin
  CheckIndex(StartIndex);
  Result := RawPrevIndex(Value, StartIndex);
end;

function TLegacyString.RawNextIndex(Index: Integer): Integer;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  if FCodePage <> nil then
  begin
    Result := Index;
    while not (FData[Result] in [#0..#127] + FCodePage.LeadBytes) do
      Inc(Result);
  end
  else
    Result := Index + 1;
end;

function TLegacyString.RawNextIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
{$IFDEF Debug}
  CheckIndex(StartIndex);
{$ENDIF}
  S := StrScan(FData + StartIndex, Count - StartIndex, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.RawPrevIndex(Index: Integer): Integer;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  Result := Index - 1;
  if FCodePage <> nil then
    while not (FData[Result] in [#0..#127] + FCodePage.LeadBytes) do
      Dec(Result)
  else
    Result := Index - 1;
end;

function TLegacyString.RawPrevIndex(Value: LegacyChar; StartIndex: Integer): Integer;
var
  S: PLegacyChar;
begin
{$IFDEF Debug}
  CheckIndex(StartIndex);
{$ENDIF}
  S := StrRScan(FData + StartIndex, Count - StartIndex, Value);
  if S <> nil then
    Result := S - FData
  else
    Result := -1;
end;

function TLegacyString.RawNextLine(Source: PLegacyString; StartIndex: Integer): Integer;
var
  P, Limit: PLegacyChar;
begin
{$IFDEF Debug}
  Source.CheckIndex(StartIndex);
{$ENDIF}
  P := Source.RawData + StartIndex;
  Limit := P + Source.Count;
  repeat
    if P^ in [#10, #13, #0] then
      Break;
    Inc(P);
  until P >= Limit;

  Result := P - Source.RawData;
  AsRange(Source, 0, Result);

  if P < Limit then
  begin
    if (P^ = #13) and (P + 1 < Limit) and (P[1] = #10) then
      Inc(Result);
    if Source.RawData[Result] <> #0 then
    begin
      Inc(Result);
      Exit;
    end;
  end;
  Result := Source.Count;
end;

function TLegacyString.RawNextLine(Source: PLegacyString; Delimiter: LegacyChar;
  StartIndex: Integer): Integer;
begin
{$IFDEF Debug}
  Source.CheckIndex(StartIndex);
{$ENDIF}
  Result := Source.RawNextIndex(Delimiter, StartIndex);
  if Result >= 0 then
  begin
    AsRange(Source, StartIndex, Result - StartIndex);
    Inc(Result);
  end
  else
  begin
    AsRange(Source, StartIndex);
    Result := Source.Count;
  end;
end;

procedure TLegacyString.Load(Source: PReadableStream);
begin
  Load(Source, True);
end;

function TLegacyString.Load(Source: PReadableStream; AllowBOM: Boolean;
  SourceOptions: TRawByteOptions): TReadableBOM;
var
  W: TWideString;
  CP: TCodePage;
  Length: Integer;
begin
  Clear;

  if AllowBOM then
  begin
    Result := Source.ReadBOM;
    case Result of
      bomUTF8:
        Include(FOptions, soDetectUTF8);
      bomUTF16LE, bomUTF16BE:
        begin
          W.Create;
          try
            W.Load(Source, TByteOrder(Byte(Result) - Byte(bomUTF16)));
            if soBigEndian in W.Options then
              W.SwapByteOrder;
            if soDetectUTF8 in FOptions then
            begin
              FCodePage := nil;
              Exclude(FOptions, soDetectUTF8);
            end;
            AsWideString(@W);
          finally
            W.Destroy;
          end;
          Exit;
        end;
      bomUTF7, bomGB18030:
        begin
          Load(Source, False, SourceOptions);
          CP.Create(BOMCP[Result = bomUTF7]);
          FCodePage := @CP;
          W.Create;
          try
            W.AsString(@Self);
            if soDetectUTF8 in FOptions then
            begin
              FCodePage := nil;
              Exclude(FOptions, soDetectUTF8);
            end;
            AsWideString(@W);
          finally
            W.Destroy;
          end;
          Exit;
        end;
    else
      if Result <> bomNone then
        raise EUTF32.Create(@Self);
    end;
  end
  else
    Result := bomNone;

  Length := Source.Size - Source.Position;
  if Length <> 0 then
  begin
    if (BufferKind = bkAttached) or (Capacity < Length + 1) then
      Capacity := Length + 1;
    Source.ReadBuffer(FData^, Length);
    Append(Length);
    FData[Length] := #0;
    FOptions := SourceOptions;
  end;
end;

procedure TLegacyString.Save(Dest: PWritableStream);
begin
  Save(Dest, FCodePage <> nil);
end;

procedure TLegacyString.Save(Dest: PWritableStream; WriteBOM: Boolean);
begin
  if Count <> 0 then
  begin
    if WriteBOM then
      if FCodePage = nil then
      begin
        if FOptions = [] then
          Dest.WriteBOM(bomUTF8);
      end
      else
        case FCodePage.Number of
//          CP_UTF7:
//            Dest.WriteBOM(bomUTF7); // TODO
          CP_GB18030:
            Dest.WriteBOM(bomGB18030);
        end;
    Dest.WriteBuffer(FData^, Count);    
  end;
end;

procedure TLegacyString.SetCodePage(Value: PCodePage);
var
  Capture: TLegacyString;
  W: TWideString;
begin
  if (Count <> 0) and ((FCodePage <> Value) or
    (Value <> nil) and (FCodePage.Number <> Value.Number)) then
  begin
    W.Create;
    try
      W.AsString(@Self);
      Capture.Create;
      Capture.FOptions := FOptions;
      Capture.FCodePage := FCodePage;
      W.FDataSource := @Capture;
      FCodePage := Value;
      AsWideString(@W);
    finally
      W.Destroy;
    end;
  end
  else
    FCodePage := Value;
end;

procedure TLegacyString.SetData(Value: PLegacyChar);
begin
  Assign(Value, StrLen(Value), soFromTheWild);
end;

{$IFDEF CoreLiteVCL}
function TLegacyString.GetRawByteString: RawByteString;
{$IF UnicodeRTL}
  var CP: Word;
{$IFEND}
begin
  SetString(Result, FData, Count);
{$IF UnicodeRTL}
  if FCodePage <> nil then
    CP := FCodePage.Number
  else
    CP := CP_UTF8;
  System.SetCodePage(Result, CP, False);
{$IFEND}
end;

procedure TLegacyString.SetRawByteString(Value: RawByteString);
begin
  Assign(Pointer(Value), Length(Value), soFromTheWild);
end;

function TLegacyString.GetUnicodeString: UnicodeString;
var
  W: TWideString;
begin
  W.Create;
  try
    W.AsString(@Self);
    SetString(Result, W.RawData, W.Count);
  finally
    W.Destroy;
  end;
end;

procedure TLegacyString.SetUnicodeString(Value: UnicodeString);
var
  W: TWideString;
begin
  W.Create;
  try
    W.Assign(Pointer(Value), Length(Value), soAttach);
    AsWideString(@W);
  finally
    W.Destroy;
  end;
end;
{$ENDIF}

function TLegacyString.ValidateUTF8: Integer;
var
  Dummy: array[0..1023] of WideChar;
  W: TWideString;
  R: TLegacyString;
  Idx: Integer;
begin
  with W do
  begin
    Create;
    Assign(@Dummy, Length(Dummy), soAttach);
  end;
  with R do
  begin
    Create;
    FCodePage := nil;
    FOptions := [soDetectUTF8];
  end;
  Result := 0;
  Idx := 0;
  while Idx < Count do
  begin
    R.AsRange(@Self, Idx, False);
    with W.AssignString(0, @R) do
    begin
      if ErrorInfo.RawData <> 0 then
      begin
        Exclude(FOptions, soDetectUTF8);
        Result := SuccessBytes; // negative
        Break;
      end;
      Inc(Idx, SourceCount);
      Inc(Result, SuccessBytes);
    end;
  end;
  if Result > 0 then
    FCodePage := nil;
end;

{ TWideString }

class function TWideString.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sWideString;
    ItemSize := SizeOf(WideChar);
  end;
end;

procedure TWideString.Append(Source: PWideChar; Length: Integer);
begin
  inherited Append(Source, Length, Length + 1);
  FData[Count] := #0;
end;

function TWideString.AssignDigits(Index: Integer; Digits: PLegacyChar;
  Length, MinWidth: Integer; FillChar: WideChar): Integer;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  if Length < Abs(MinWidth) then
  begin
    if MinWidth < 0 then
    begin
      Result := -MinWidth - Length;
      FillWideChar(FData[Index], Result, FillChar);
      MoveBytesExpand(Digits^, FData[Index + Result], Length);
      Inc(Result, Length);
    end
    else
    begin
      MoveBytesExpand(Digits^, FData[Index], Length);
      FillWideChar(FData[Index + Length], MinWidth - Length, FillChar);
      Result := MinWidth;
    end;
    Exit;
  end
  else
    MoveBytesExpand(Digits^, FData[Index], Length);
  Result := Length;
end;

procedure TWideString.AssignHexBuffer(Index: Integer; const Buffer;
  Length: Integer; LowerCase: Boolean);
var
  LowerCaseMask: LongWord;
  I: Integer;
  B: Byte;
  Dest: PLongWord;
begin
  LowerCaseMask := Byte(LowerCase) * $2020; // Fast core

  Dest := PLongWord(FData + Index);
  for I := 0 to Length - 1 do
  begin
    B := TByteArray(Buffer)[I];
    Dest^ := Byte(HexDigits[B shr 4]) or
      (Byte(HexDigits[B and $F]) shl 16) or LowerCaseMask; // Fast core
    Inc(Dest);
  end;
end;

function TWideString.AssignHexadecimal(Index: Integer; Value: QuadInt;
  MinWidth: Integer; LowerCase: Boolean; FillChar: WideChar): Integer;
begin
  with FormatHexadecimal(Value, MinWidth, LowerCase) do
    Result := AssignDigits(Index, Digits, PLegacyChar(@Data) + Length(Data) - Digits, MinWidth, FillChar);
end;

function TWideString.AssignInteger(Index: Integer; Value: QuadInt;
  MinWidth: Integer; FillChar: WideChar): Integer;
begin
  with FormatInteger(Value, MinWidth) do
    Result := AssignDigits(Index, Digits, PLegacyChar(@Data) + Length(Data) - Digits, MinWidth, FillChar);
end;

function TWideString.AssignFloat(Index: Integer; const Value: Extended;
  MinWidth, Precision: Integer; FillChar: WideChar): Integer;
var
  Digits: string[DecimalExtended];
begin
  Str(Value:Abs(MinWidth):Precision, Digits);
  Result := AssignDigits(Index, PLegacyChar(@Digits[1]), Length(Digits), MinWidth, FillChar);
end;

function TWideString.AssignString(Index: Integer; Source: PLegacyString;
  EncodeOptions: TEncodeUTF16): TConvertResult;

function NextChar(var ConvertResult: TConvertResult): QuadChar;
var
  FirstByte, Bytes, B, C: Byte;
begin
  FirstByte := Byte(Source.FData[ConvertResult.SourceCount]);
  Inc(ConvertResult.SourceCount);

  if FirstByte and $80 <> 0 then
  begin
    if FirstByte and $40 <> 0 then
    begin
      if FirstByte and $20 = 0 then
      begin
        Result := FirstByte and $1F;
        Bytes := 1;
      end
      else if FirstByte and $10 = 0 then
      begin
        Result := FirstByte and $0F;
        Bytes := 2;
      end
      else if FirstByte and $08 = 0 then
      begin
        Result := FirstByte and $07;
        Bytes := 3;
      end
      else
      begin
        ConvertResult.ErrorInfo.InvalidUTF8.StartingByte := FirstByte;
        Result := 0;
        Exit;
      end;

      B := Bytes;
      while (B <> 0) and (ConvertResult.SourceCount < Source.Count) do
      begin
        C := Byte(Source.FData[ConvertResult.SourceCount]);
        if C and $C0 = $80 then
        begin
          Result := (Result shl 6) or (C and $3F);
          Inc(ConvertResult.SourceCount);
          Dec(B);
        end
        else
          Break; // broken sequence
      end;

      if B = 0 then
      begin
        Inc(ConvertResult.SuccessBytes, Bytes + 1);
        Exit;
      end;
      ConvertResult.ErrorInfo.InvalidUTF8.RawData := FirstByte or (Bytes + 1) shl 8;
    end;
    ConvertResult.ErrorInfo.InvalidUTF8.StartingByte := FirstByte;
    Result := 0;
  end
  else
    Result := FirstByte;
end;

var
  Dest, Limit: PWideChar;
  CodePoint, Paired: QuadChar;
  B: Byte;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if (Source.FCodePage = nil) or (soDetectUTF8 in Source.FOptions) then
  begin
    Dest := FData + Index;
    Limit := FData + Capacity;

    while (Result.SourceCount < Source.Count) and (Dest < Limit) do
    begin
      CodePoint := NextChar(Result);
      if Result.ErrorInfo.InvalidUTF8.RawData <> 0 then
        Break;

      case CodePoint of
        Low(THighSurrogates)..High(THighSurrogates):
          if coSurrogatePairs in EncodeOptions then
          begin
            Paired := NextChar(Result);
            if Result.ErrorInfo.InvalidUTF8.RawData <> 0 then
              Break;

            case Paired of
              Low(TLowSurrogates)..High(TLowSurrogates): // CESU-8
                begin
                  if Limit - Dest < 2 then
                  begin
                    Dec(Result.SourceCount, 6);
                    Break;
                  end;
                  if coBigEndian in EncodeOptions then // Fast core
                    PLongWord(Dest)^ := Swap(CodePoint) or (Swap(Paired) shl 16)
                  else
                    PLongWord(Dest)^ := Word(CodePoint) or (Word(Paired) shl 16);
                  Inc(Dest, 2);
                  Inc(Result.SurrogatePairs);
                  Continue;
                end;
            end;
          end;

        Low(TUnicodeSMP)..High(TUnicodeSPUA):
          if coSurrogatePairs in EncodeOptions then
          begin
            if Limit - Dest < 2 then
            begin
              Dec(Result.SourceCount, 4);
              Break;
            end;
            if coBigEndian in EncodeOptions then // Fast core
              PLongWord(Dest)^ :=
                Swap(Low(THighSurrogates) or Word(CodePoint shr 10)) or
                Swap(Low(TLowSurrogates) or Word(CodePoint) and $3FF) shl 16
            else
              PLongWord(Dest)^ :=
                (Low(THighSurrogates) or Word(CodePoint shr 10)) or
                (Low(TLowSurrogates) or Word(CodePoint) and $3FF) shl 16;
            Inc(Dest, 2);
            Inc(Result.SurrogatePairs);
            Continue;
          end;

        Low(TLowSurrogates)..High(TLowSurrogates), $FFFE, $FFFF,
        Low(TNonUnicode)..High(TNonUnicode):
          ;
      else
        if coBigEndian in EncodeOptions then
          PWord(Dest)^ := Swap(CodePoint)
        else
          PWord(Dest)^ := CodePoint;
        Inc(Dest);
        Continue;
      end;

      if (coReplaceInvalid in EncodeOptions) and
        ((Source.FCodePage = nil) or not (soLatin1 in Source.FOptions)) then
      begin
        if coBigEndian in EncodeOptions then
          PWord(Dest)^ := Swap($FFFD)
        else
          PWord(Dest)^ := $FFFD;
        Inc(Dest);
      end
      else
        Break;
    end;

    if Result.ErrorInfo.RawData = 0 then
    begin
      Result.DestCount := Dest - (FData + Index);
      Exit;
    end;
  end;

  if Source.FCodePage <> nil then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.DestCount := Source.FCodePage.EncodeUTF16(Source, @Self, Index, EncodeOptions);
    if Result.DestCount <= 0 then
      raise ECodePage.Create(Source, Source.FCodePage, cmEncodeUTF16);
    Result.SourceCount := Source.Count;
  end
  else if coLatin1 in EncodeOptions then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Dest := FData + Index;
    Limit := FData + Count;

    while (Result.SourceCount < Source.Count) and (Dest < Limit) do
    begin
      B := Byte(Source.FData[Result.SourceCount]);
      Inc(Result.SourceCount);
      if B in [$00..$7F, $A0..$FF] then
      begin
        PWord(Dest)^ := B shl (Byte(coBigEndian in EncodeOptions) * 8); // Fast core
        if B in [$A0..$FF] then
          Dec(Result.SuccessBytes); // negative for Latin1
      end
      else if coReplaceInvalid in EncodeOptions then
      begin
        PWord(Dest)^ := Byte(LegacyReplacementChar) shl (Byte(coBigEndian in EncodeOptions) * 8); // Fast core
        Dec(Result.SuccessBytes);
      end
      else
      begin
        Result.ErrorInfo.InvalidChar := B;
        Break;
      end;
      Inc(Dest);
    end;

    Result.DestCount := Dest - (FData + Index);
  end;
end;

function TWideString.AssignWideString(Index: Integer; Source: PWideString;
  EncodeOptions: TEncodeUTF16): TConvertResult;
var
  Dest, Limit: PWideChar;
  W: Word;
begin
{$IFDEF Debug}
  CheckIndex(Index);
{$ENDIF}
  FillChar(Result, SizeOf(Result), 0);

  if EncodeOptions * [coSurrogatePairs, coReplaceInvalid] = [coReplaceInvalid] then
  begin
    Dest := FData + Index;
    Limit := FData + Capacity;

    while Dest < Limit do
    begin
      W := Word(Source.RawData[Result.SourceCount]);
      Inc(Result.SourceCount);
      if soBigEndian in Source.Options then
        W := Swap(W);

      case W of
        Low(THighSurrogates)..High(THighSurrogates),
        Low(TLowSurrogates)..High(TLowSurrogates), $FEFF, $FFFE:
          W := $FFFD;
      end;

      if soBigEndian in FOptions then
        W := Swap(W);
      PWord(Dest)^ := W;
      Inc(Dest);
    end;

    Result.DestCount := Dest - (FData + Index);
  end
  else
  begin
    if FOptions = Source.Options then
      Move(FData[Index], Source.RawData^, Source.Count * SizeOf(WideChar))
    else
      SwapWideCharBytes(Source.RawData, FData + Index, Source.Count);
    Result.SourceCount := Source.Count;
    Result.DestCount := Source.Count;
  end;
end;

procedure TWideString.AsHexBuffer(const Value; Length: Integer;
  LowerCase: Boolean);
var
  L: Integer;
begin
  Clear;
  L := Length * 2;
  if (BufferKind = bkAttached) or (Capacity < L) then
    Capacity := L + 1;
  AssignHexBuffer(0, Value, Length, LowerCase);
  Append(L);
  FData[L] := #0;
end;

procedure TWideString.AsHexadecimal(Value: QuadInt; MinWidth: Integer;
  LowerCase: Boolean; FillChar: WideChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > HexQuadInt then
    Length := Abs(MinWidth)
  else
    Length := HexQuadInt;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignHexadecimal(0, Value, MinWidth, LowerCase, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TWideString.AsInteger(Value: QuadInt; MinWidth: Integer;
  FillChar: WideChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > DecimalQuadInt then
    Length := Abs(MinWidth)
  else
    Length := DecimalQuadInt;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignInteger(0, Value, MinWidth, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TWideString.AsFloat(const Value: Extended; MinWidth, Precision: Integer;
  FillChar: WideChar);
var
  Length: Integer;
begin
  Clear;
  if Abs(MinWidth) > DecimalExtended then
    Length := Abs(MinWidth)
  else
    Length := DecimalExtended;
  Inc(Length);
  if (BufferKind = bkAttached) or (Capacity < Length) then
    Capacity := Length;
  Length := AssignFloat(0, Value, MinWidth, Precision, FillChar);
  Append(Length);
  FData[Length] := #0;
end;

procedure TWideString.AsPercentage(const Ratio: Double; Precision: Integer;
  FillChar: WideChar);
begin
  AsFloat(Ratio * 100, 2, Precision, FillChar);
  PLongWord(FData + Count)^ := Byte('%'); // Fast core
  Append(1);
end;

function TWideString.AsNextLine(Source: PWideString; StartIndex: Integer): Integer;
begin
  if Source <> nil then
  begin
    Source.CheckIndex(StartIndex);
    Result := RawNextLine(Source, StartIndex);
  end
  else
  begin
    Clear;
    Result := 0;
  end;
end;

function TWideString.AsNextLine(Source: PWideString; Delimiter: WideChar;
  StartIndex: Integer): Integer;
begin
  if Source <> nil then
  begin
    Source.CheckIndex(StartIndex);
    Result := RawNextLine(Source, Delimiter, StartIndex);
  end
  else
  begin
    Clear;
    Result := 0;
  end;
end;

function TWideString.AsRange(Index: Integer): TWideString;
begin
  Result.Create;
  Result.AsRange(@Self, Index);
end;

function TWideString.AsRange(Index, MaxCount: Integer): TWideString;
begin
  Result.Create;
  Result.AsRange(@Self, Index, MaxCount);
end;

procedure TWideString.AsString(Source: PLegacyString; EncodeOptions: TEncodeUTF16);
var
  Length: Integer;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    Length := Source.Count + 1;
    if (BufferKind = bkAttached) or (Capacity < Length) then
      Capacity := Length;
      
    with AssignString(0, Source, EncodeOptions) do
    begin
      if (DestCount = 0) and (Source.FCodePage <> nil) then
        raise ECodePage.Create(Source, Source.FCodePage, cmEncodeUTF16);
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

procedure TWideString.AsWideString(Source: PWideChar; Length: Integer;
  SourceOptions: TEndianSource);
begin
  Assign(Source, Length, SourceOptions);
end;

procedure TWideString.AsWideString(Source: PWideString; EncodeOptions: TEncodeUTF16);
var
  Length: Integer;
begin
  Clear;
  if (Source <> nil) and (Source.Count <> 0) then
  begin
    Length := Source.Count + 1;
    if (BufferKind = bkAttached) or (Capacity < Length) then
      Capacity := Length;
      
    with AssignWideString(0, Source, EncodeOptions) do
    begin
      if ErrorInfo.RawData <> 0 then
        raise EUnicode.Create(Source, EncodeOptions, ErrorInfo);
      Append(DestCount);
      FData[DestCount] := #0;
    end;
  end;
end;

function TWideString.Compare(Value: PWideChar; Length: Integer;
  IgnoreCase: Boolean): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
    Value, Length, FData, Count) - CSTR_EQUAL;
end;

function TWideString.Compare(Value: PWideString; IgnoreCase: Boolean): Integer;
begin
  if Value <> nil then
    Result := CompareStringW(LOCALE_USER_DEFAULT, Byte(IgnoreCase),
      Value.RawData, Value.Count, FData, Count) - CSTR_EQUAL
  else
    Result := 1;
end;

procedure TWideString.Detach;
begin
  if BufferKind = bkAttached then
    Capacity := Count + 1;
  if Capacity > Count then
    FData[Count] := #0;
end;

function TWideString.GetData: PWideChar;
begin
  if Count <> 0 then
  begin
    if (Count < Capacity) or (FData[Count] <> #0) then
      Detach;
    Result := FData;
  end
  else
    Result := nil;
end;

function TWideString.LastIndex(Value: WideChar): Integer;
var
  W: PWideChar;
begin
  W := WideStrRScan(FData, Count, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

function TWideString.NextIndex(Value: WideChar; StartIndex: Integer): Integer;
begin
  CheckIndex(StartIndex);
  Result := RawNextIndex(Value, StartIndex);
end;

function TWideString.PrevIndex(Value: WideChar; StartIndex: Integer): Integer;
begin
  CheckIndex(StartIndex);
  Result := RawPrevIndex(Value, StartIndex);
end;

function TWideString.RawNextIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
{$IFDEF Debug}
  CheckIndex(StartIndex);
{$ENDIF}
  W := WideStrScan(FData + StartIndex, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

function TWideString.RawPrevIndex(Value: WideChar; StartIndex: Integer): Integer;
var
  W: PWideChar;
begin
{$IFDEF Debug}
  CheckIndex(StartIndex);
{$ENDIF}
  W := WideStrRScan(FData + StartIndex, Count - StartIndex, Value);
  if W <> nil then
    Result := W - FData
  else
    Result := -1;
end;

function TWideString.RawNextLine(Source: PWideString; StartIndex: Integer): Integer;
var
  P, Limit: PWideChar;
begin
{$IFDEF Debug}
  Source.CheckIndex(StartIndex);
{$ENDIF}
  P := Source.RawData + StartIndex;
  Limit := P + Source.Count;
  repeat
    case P^ of
      #10, #13, #0:
        Break;
    end;
    Inc(P);
  until P >= Limit;

  Result := P - Source.RawData;
  AsRange(Source, 0, Result);

  if P < Limit then
  begin
    if (P^ = #13) and (P + 1 < Limit) and (P[1] = #10) then
      Inc(Result);
    if Source.RawData[Result] <> #0 then
    begin
      Inc(Result);
      Exit;
    end;
  end;
  Result := Source.Count;
end;

function TWideString.RawNextLine(Source: PWideString; Delimiter: WideChar;
  StartIndex: Integer): Integer;
begin
{$IFDEF Debug}
  Source.CheckIndex(StartIndex);
{$ENDIF}
  Result := Source.RawNextIndex(Delimiter, StartIndex);
  if Result >= 0 then
  begin
    AsRange(Source, StartIndex, Result - StartIndex);
    Inc(Result);
  end
  else
  begin
    AsRange(Source, StartIndex);
    Result := Source.Count;
  end;
end;

procedure TWideString.Load(Source: PReadableStream);
var
  FallbackCP: TCodePage;
begin
  FallbackCP.Create;
  Load(Source, boFromBOM, @FallbackCP);
end;

function TWideString.Load(Source: PReadableStream; ByteOrder: TByteOrder;
  FallbackCP: PCodePage = nil): TReadableBOM;
var
  CP: TCodePage;
  S: TLegacyString;
  Length: Integer;
begin
  Clear;
  if ByteOrder = boFromBOM then
  begin
    Result := Source.ReadBOM;
    case Result of
      bomUTF16LE, bomUTF16BE:
        FallbackCP := nil;
      bomUTF7:
        begin
          CP.Create(CP_UTF7);
          FallbackCP := @CP;
        end;
      bomGB18030:
        begin
          CP.Create(CP_GB18030);
          FallbackCP := @CP;
        end;
    else
      if Result <> bomNone then
        raise EUTF32.Create(@Self);
    end;

    if FallbackCP <> nil then
    begin
      S.Create;
      try
        S.FCodePage := FallbackCP;
        S.Load(Source, False);
        AsString(@S);
      finally
        S.Destroy;
      end;
      Exit;
    end
  end
  else
    Result := bomNone;

  Length := (Source.Size - Source.Position) div SizeOf(WideChar);
  if Length <> 0 then
  begin
    if (BufferKind = bkAttached) or (Capacity < Length + 1) then
      Capacity := Length + 1;
    Source.ReadBuffer(FData^, Length * SizeOf(WideChar));
    FOptions := [];
    if Result = bomUTF16BE then
    begin
      Include(FOptions, soBigEndian);
      case ByteOrder of
        boConvertToNative:
          SwapWideCharBytes(FData, FData, Length);
        boBigEndian:
          Include(FOptions, soBigEndian);
      end;
    end;
    Append(Length);
    FData[Length] := #0;
  end;
end;

procedure TWideString.Save(Dest: PWritableStream);
begin
  Save(Dest, True);
end;

procedure TWideString.Save(Dest: PWritableStream; WriteBOM: Boolean);
begin
  if Count <> 0 then
  begin
    if WriteBOM then
      Dest.WriteBOM(TWritableBOM(Byte(bomUTF16) + Byte(soBigEndian in FOptions)));
    Dest.WriteBuffer(FData^, Count * SizeOf(WideChar));
  end;
end;

procedure TWideString.SwapByteOrder(Index, Length: Integer);
var
  W: PWideChar;
begin
{$IFDEF Debug}
  CheckRange(Index, Length);
{$ENDIF}
  W := FData + Index;
  SwapWideCharBytes(W, W, Length);
end;

procedure TWideString.SwapByteOrder;
begin
  SwapByteOrder(0, Count);
  Byte(FOptions) := Byte(FOptions) xor (1 shl Byte(soBigEndian));
end;

procedure TWideString.SetData(Value: PWideChar);
begin
  Assign(Value, WideStrLen(Value), []); // Fast core
end;

{$IFDEF CoreLiteVCL}
function TWideString.GetRawByteString: RawByteString;
var
  S: TLegacyString;
{$IF not UnicodeRTL}
  CP: TCodePage;
{$IFEND}
begin
  S.Create;
  try
  {$IF not UnicodeRTL}
    CP.Create;
    S.FCodePage := @CP;
  {$IFEND}
    S.AsWideString(@Self);
    SetString(Result, S.FData, S.Count);
  {$IF UnicodeRTL}
    System.SetCodePage(Result, CP.Number, False);
  {$IFEND}
  finally
    S.Destroy;
  end;
end;

procedure TWideString.SetRawByteString(Value: RawByteString);
var
  S: TLegacyString;
  CP: TCodePage;
  CPNum: Word;
begin
  S.Create;
  try
  {$IF UnicodeRTL}
    CPNum := StringCodePage(Value);
  {$ELSE}
    CPNum := CP_ACP;
    Include(S.FOptions, soDetectUTF8);
  {$IFEND}
    if CPNum <> CP_UTF8 then
    begin
      CP.Create(CPNum);
      S.FCodePage := @CP;
    end;
    S.Assign(Pointer(Value), Length(Value), soAttach);
    AsString(@S);
  finally
    S.Destroy;
  end;
end;

function TWideString.GetUnicodeString: UnicodeString;
begin
  System.SetString(Result, FData, Count);
end;

procedure TWideString.SetUnicodeString(Value: UnicodeString);
begin
  Assign(Pointer(Value), Length(Value), []);
end;
{$ENDIF}

{ TDecimalFormat }

procedure TDecimalFormat.Init(Locale: Word; UseNativeDigits: Boolean);
var
  Info: array[0..10] of CoreChar;
begin
  Self := DefaultDecimalFormat;
  if Locale <> 0 then
  begin
    if GetLocaleInfoW(Locale, LOCALE_STHOUSAND, PWideChar(@Info), Length(Info)) = 0 then
      RaiseLastPlatformError {$IFDEF Debug} (sLocaleInfo, Length(Info)) {$ENDIF} ;
    ThousandSeparator := LiteChar(Info[0]);

    if GetLocaleInfoW(Locale, LOCALE_SDECIMAL, PWideChar(@Info), Length(Info)) = 0 then
      RaiseLastPlatformError {$IFDEF Debug} (sLocaleInfo, Length(Info)) {$ENDIF} ;
    DecimalSeparator := LiteChar(Info[0]);

    if UseNativeDigits then
    begin
      if GetLocaleInfoW(Locale, LOCALE_SNATIVEDIGITS, PWideChar(@Info), Length(Info)) = 0 then
        RaiseLastPlatformError {$IFDEF Debug} (sLocaleInfo, Length(Info)) {$ENDIF} ;
      ZeroSign := LiteChar(Info[0]);
    end;
  end;
end;

{ TDecimalFormats }

function TDecimalFormats.Append(Locale: Word; UseNativeDigits: Boolean): Integer;
begin
  Result := inherited Append;
  FItems[Result].Init(Locale, UseNativeDigits);
end;

class function TDecimalFormats.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sDecimalFormats;
    ItemSize := SizeOf(TDecimalFormat);
  end;
end;

procedure TDecimalFormats.Insert(Index: Integer; Locale: Word; UseNativeDigits: Boolean);
begin
  inherited Insert(Index);
  FItems[Index].Init(Locale, UseNativeDigits);
end;

{ THexadecimalFormats }

function THexadecimalFormats.Append(HexChunkBytes: Integer; HexChunkSeparator: QuadChar): Integer;
begin
  Result := inherited Append;
  with FItems[Result] do
  begin
    ChunkBytes := HexChunkBytes;
    ChunkSeparator := HexChunkSeparator;
  end;
end;

class function THexadecimalFormats.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sHexadecimalFormats;
    ItemSize := SizeOf(THexadecimalFormat);
  end;
end;

procedure THexadecimalFormats.Insert(Index, HexChunkBytes: Integer; HexChunkSeparator: QuadChar);
begin
  inherited Insert(Index);
  with FItems[Index] do
  begin
    ChunkBytes := HexChunkBytes;
    ChunkSeparator := HexChunkSeparator;
  end;
end;

{ TStringFormatter }

constructor TStringFormatter.Create;
begin
  FDecimalFormats.Create;
  FHexadecimalFormats.Create;
  FDefaultDecimalFormat := -1;
  FDefaultHexadecimalFormat := -1;
end;

destructor TStringFormatter.Destroy;
begin
  FHexadecimalFormats.Finalize;
  FDecimalFormats.Finalize;
  inherited;
end;

function TStringFormatter.Estimate(const Args: array of const): Integer;
begin
  Result := 0; // TODO
end;

function TStringFormatter.Estimate(Fmt: PLegacyString; const Args: array of const): Integer;
var
  Index: Integer;
  Token: TLegacyString;
begin
  Token.Create;
  Result := 0;
  Index := 0;
(*  repeat         
//    Token.AsRange(Fmt, 0, Fmt.RawNextIndex('{', ));
    if Index < 0 then
      Break;

    Inc(Index);
    Token := Fmt.RawData + Index;
    while (Token < Limit) and (Token <> '}') do
    begin
      Index := Fmt.RawNextIndex(Index);
      Token := Fmt.RawData + Index;
      if (Token < Limit) and (Token = '%') then
      begin
        Inc(Token);
        if (Token < Limit) and (Token <> '%') then
        begin
          case Token^ of
            '0'..'9': ;
            '?': ;
          else
            case LegacyChar(PByte(Token)^ and not $20) of
              'A', 'X': ;
              'C': ;
              'D': ;
              'F', 'P': ;
              'I', 'U': ;
              'K': ;
              'S': ;
            end;
          end;
        end
        else
          Inc(Index);
      end;
    end;
  until False; *)
end;

function TStringFormatter.Estimate(Fmt: PWideString; const Args: array of const): Integer;
begin
  Result := 0; // TODO
end;

function TStringFormatter.IsNullAddress(Index: Integer; const Value): Boolean;
begin
  Result := IsNullCardinal(Index, CoreWord(Value));
end;

function TStringFormatter.IsNullCardinal(Index: Integer; Value: QuadWord): Boolean;
begin
  Result := Value = 0;
end;

function TStringFormatter.IsNullChar(Index: Integer; Value: QuadChar): Boolean;
begin
  Result := Value = 0;
end;

function TStringFormatter.IsNullFloat(Index: Integer; const Value: Extended): Boolean;
begin
  Result := IsNaN(Value) or IsInfinite(Value) or (Value = 0.0);
end;

function TStringFormatter.IsNullInteger(Index: Integer; Value: QuadInt): Boolean;
begin
  Result := Value = 0;
end;

function TStringFormatter.IsNullPercent(Index: Integer; const Value: Double): Boolean;
begin
  Result := IsNullFloat(Index, Value);
end;

function TStringFormatter.IsNullString(Index: Integer; Value: Pointer): Boolean;
begin
  Result := (Value = nil) or (PEnumerable(Value).Count = 0); // TODO
end;

{ TLegacyText }

function TLegacyText.Append: PLegacyString;
begin
  Result := @FItems[inherited Append];
  Result.Create;
end;

class function TLegacyText.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sLegacyText;
    ItemSize := SizeOf(TLegacyString);
  end;
end;

{ TWideText }

function TWideText.Append: PWideString;
begin
  Result := @FItems[inherited Append];
  Result.Create;
end;

class function TWideText.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sWideText;
    ItemSize := SizeOf(TWideString);
  end;
end;

{ TLegacyTextList }

function TLegacyTextList.Append: PLegacyString;
begin
  Result := New(PLegacyTextListItem, Create);
  inherited Append(Result);
end;

class function TLegacyTextList.ListInfo: TListInfo;
begin
  with Result do
  begin
    ClassName := sLegacyText;
    ItemOffset := PAddress(@PLegacyTextListItem(nil).FOwner) - nil;
  end;
end;

{ TWideTextList }

function TWideTextList.Append: PWideString;
begin
  Result := New(PWideTextListItem, Create);
  inherited Append(Result);
end;

class function TWideTextList.ListInfo: TListInfo;
begin
  with Result do
  begin
    ClassName := sWideText;
    ItemOffset := PAddress(@PWideTextListItem(nil).FOwner) - nil;
  end;
end;

end.
