// copyright 2010

unit Strings2;

interface

uses
  Windows, CoreUtils, CoreClasses, Strings;

type
  TStringOption = (soBigEndian, soAttachBuffer, soDetectCharSet);
const
  soLatin1 = soBigEndian;

type
  TStringOptions = set of soBigEndian..soAttachBuffer;
  TCharSetOptions = set of soAttachBuffer..soDetectCharSet;

  TCharStrOptions = TStringOptions;

  TLegacyStrOptions = TStringOptions; // soLatin1..soAttachBuffer
  TLatinStrOptions = TLegacyStrOptions;

  TUTFCharStrOptions = set of soAttachBuffer..soAttachBuffer;
  TUTF7StrOptions = TUTFCharStrOptions;
  TUTF8StrOptions = TUTFCharStrOptions;

  TEndianStrOptions = TStringOptions;

  TWideStrOptions = TEndianStrOptions;
  TUTF16StrOptions = TWideStrOptions;

  TQuadStrOptions = TEndianStrOptions;
  TUTF32StrOptions = TQuadStrOptions;

type
  TEncodeStrOption = (esBigEndian, esForceInvalid, esRangeBlocks);
const
  esLatin1 = esBigEndian;

type
  TEncodeStrOptions = set of esBigEndian..esForceInvalid;

  TEncodeLegacyStrOptions = TEncodeStrOptions; // esLatin1..esForceInvalid
  TEncodeLatinOptions = TEncodeLegacyStrOptions;

  TEncodeUTFCharOptions = set of esForceInvalid..esForceInvalid;
  TEncodeUTF7Options = TEncodeUTFCharOptions;
  TEncodeUTF8Options = TEncodeUTFCharOptions;

  TEncodeEndianStrOptions = TEncodeStrOptions;

  TEncodeWideStrOptions = TEncodeEndianStrOptions;
  TEncodeUTF16Options = TEncodeWideStrOptions;

  TEncodeQuadStrOptions = TEncodeEndianStrOptions;
  TEncodeUTF32Options = TEncodeQuadStrOptions;

  // CESU-8: encode surrogates as is
  // Modified UTF-8: CESU-8 plus encode #0 as ($C0, $80)
  TUTF8Compliance = (cpRegular, cpAllowNonUnicode, cpCESU8, cpModified);
  TUTF32Compliance = cpRegular..cpAllowNonUnicode;

{$IFNDEF Lite}
  TEncodeStrOptionsEx = set of TEncodeStrOption;

  TEncodeLegacyStrOptionsEx = TEncodeStrOptionsEx; // esLatin1..esRangeBlocks
  TEncodeLatinOptionsEx = TEncodeLegacyStrOptionsEx;

  TEncodeUTFCharOptionsEx = TEncodeStrOptionsEx;
  TEncodeUTF7OptionsEx = TEncodeUTFCharOptionsEx;
  TEncodeUTF8OptionsEx = TEncodeUTFCharOptionsEx;

  TEncodeEndianStrOptionsEx = TEncodeStrOptionsEx;

  TEncodeWideStrOptionsEx = TEncodeEndianStrOptionsEx;
  TEncodeUTF16OptionsEx = TEncodeWideStrOptionsEx;

  TEncodeQuadStrOptionsEx = TEncodeEndianStrOptionsEx;
  TEncodeUTF32OptionsEx = TEncodeQuadStrOptionsEx;

  PStringInfo = ^TStringInfo;
  TStringInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: Pointer);
      1: (InvalidCount: Cardinal);
  end;

  PLatinStrInfo = ^TLatinStrInfo;
  TLatinStrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, Latin1Count: Cardinal);
  end;

  PLegacyStrInfo = ^TLegacyStrInfo;
  TLegacyStrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, MultiByteCount: Cardinal);
  end;

  PUTF7StrInfo = ^TUTF7StrInfo;
  TUTF7StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PLegacyChar);
      1: (InvalidCount, SurrogateCount, UnicodeCount: Cardinal;
          DirectChars: set of #$00..#$7F);
  end;

  PUTF8StrInfo = ^TUTF8StrInfo;
  TUTF8StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PLegacyChar; NonUnicodeCount: Cardinal);
      1: (InvalidCount, SurrogateCount, UnicodeCount: Cardinal;
          Compliance: TUTF8Compliance);
  end;

  PUTF16StrInfo = ^TUTF16StrInfo;
  TUTF16StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PWideChar);
      1: (InvalidCount, SurrogateCount: Cardinal);
  end;

  PUTF32StrInfo = ^TUTF32StrInfo;
  TUTF32StrInfo = record
    Count: Cardinal;
    Blocks: TCharBlocks;
    case Byte of
      0: (InvalidChar: PQuadChar);
      1: (InvalidCount, NonUnicodeCount: Cardinal);
  end;
{$ENDIF}

  TCharString = class;
  TWideString = class;
  TQuadString = class;

  TSubString = class(TSharedObject)
  private
    FCount: Cardinal;
  {$IFNDEF Lite}
    FLanguage: Word;
  {$ENDIF}
  //  FData: Pointer;  { need to be placed in the same order in the descedants }
  //  FOptions: TStringOptions;
  //  FOwner: TSubString;
  protected
    function GetCodePage: Cardinal; virtual; abstract;
  public
    destructor Destroy; override;

    class function ByteCount(Count: Cardinal): Cardinal; overload; virtual; abstract;
    class function ByteCount(Data: Pointer): Cardinal; overload;
    function ByteCount: Cardinal; overload;

    procedure EncodeLatin(Dest: TCharString;
      Options: TEncodeLatinOptions = []); overload;
  {$IFNDEF Lite}
    procedure EncodeLatin(const Info; Dest: TCharString;
      Options: TEncodeLatinOptions = []); overload;
  {$ENDIF}
    function EncodeLatin(Dest: TCharString; Index: Cardinal;
      Options: TEncodeLatinOptions = []): Cardinal; overload; virtual; abstract;

  {$IFNDEF Lite}
    function EncodeLatinEx(Dest: TCharString;
      Options: TEncodeLatinOptionsEx = []): TLatinStrInfo; overload;
    procedure EncodeLatinEx(var Info: TLatinStrInfo; Dest: TCharString;
      Index: Cardinal; Options: TEncodeLatinOptionsEx = []); overload; virtual; abstract;
  {$ENDIF}

  {$IFDEF UTF7}
    procedure EncodeUTF7(Dest: TCharString;
      Options: TEncodeUTF7Options = []); overload;
  {$IFNDEF Lite}
    procedure EncodeUTF7(const Info; Dest: TCharString;
      Options: TEncodeUTF7Options = []); overload;
  {$ENDIF}
    function EncodeUTF7(Dest: TCharString; Index: Cardinal;
      Options: TEncodeUTF7Options = []): Cardinal; overload; virtual; abstract;

  {$IFNDEF Lite}
    function EncodeUTF7Ex(Dest: TCharString;
      Options: TEncodeUTF7OptionsEx = []): TUTF7StrInfo; overload;
    procedure EncodeUTF7Ex(var Info: TUTF7StrInfo; Dest: TCharString;
      Index: Cardinal; Options: TEncodeUTF7OptionsEx = []); overload; virtual; abstract;
  {$ENDIF}
  {$ENDIF}

    procedure EncodeUTF8(Dest: TCharString; Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular); overload;
  {$IFNDEF Lite}
    procedure EncodeUTF8(const Info; Dest: TCharString;
      Options: TEncodeUTF8Options = []; Compliance: TUTF8Compliance = cpRegular); overload;
  {$ENDIF}
    function EncodeUTF8(Dest: TCharString; Index: Cardinal; Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; overload; virtual; abstract;

  {$IFNDEF Lite}
    function EncodeUTF8Ex(Dest: TCharString; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular): TUTF8StrInfo; overload;
    procedure EncodeUTF8Ex(var Info: TUTF8StrInfo; Dest: TCharString;
      Index: Cardinal; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); overload; virtual; abstract;
  {$ENDIF}

    procedure EncodeUTF16(Dest: TWideString;
      Options: TEncodeUTF16Options = []); overload;
  {$IFNDEF Lite}
    procedure EncodeUTF16(const Info; Dest: TWideString;
      Options: TEncodeUTF16Options = []); overload;
  {$ENDIF}
    function EncodeUTF16(Dest: TWideString; Index: Cardinal;
      Options: TEncodeUTF16Options = []): Cardinal; overload; virtual; abstract;

  {$IFNDEF Lite}
    function EncodeUTF16Ex(Dest: TWideString;
      Options: TEncodeUTF16OptionsEx = []): TUTF16StrInfo; overload;
    procedure EncodeUTF16Ex(var Info: TUTF16StrInfo; Dest: TWideString;
      Index: Cardinal; Options: TEncodeUTF16OptionsEx = []); overload; virtual; abstract;
  {$ENDIF}

  {$IFDEF UTF32}
    procedure EncodeUTF32(Dest: TQuadString; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular); overload;
  {$IFNDEF Lite}
    procedure EncodeUTF32(const Info; Dest: TQuadString; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular); overload;
  {$ENDIF}
    function EncodeUTF32(Dest: TQuadString; Index: Cardinal; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; overload; virtual; abstract;

  {$IFNDEF Lite}
    function EncodeUTF32Ex(Dest: TQuadString; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular): TUTF32StrInfo; overload;
    procedure EncodeUTF32Ex(var Info: TUTF32StrInfo; Dest: TQuadString;
      Index: Cardinal; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); overload; virtual; abstract;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF Lite}
    function EstimateLatinEx(Options: TEncodeLatinOptionsEx = []): TLatinStrInfo; overload;
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions): Cardinal; overload; virtual; abstract;

    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); overload; virtual; abstract;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options = []): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; overload; virtual; abstract;

    function EstimateUTF7Ex(Options: TEncodeUTF7OptionsEx = []): TUTF7StrInfo; overload;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); overload; virtual; abstract;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; overload; virtual; abstract;

    function EstimateUTF8Ex(Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular): TUTF8StrInfo; overload;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); overload; virtual; abstract;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; overload; virtual; abstract;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; overload; virtual; abstract;

    function EstimateUTF16Ex(Options: TEncodeUTF16OptionsEx = []): TUTF16StrInfo; overload;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); overload; virtual; abstract;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options;
      Compliance: TUTF32Compliance = cpRegular): Cardinal; overload; virtual; abstract;

    function EstimateUTF32Ex(Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular): TUTF32StrInfo; overload;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); overload; virtual; abstract;
  {$IFEND}

    class function Length(Data: Pointer): Cardinal; virtual; abstract;
  // properties
    property CodePage: Cardinal read GetCodePage;
    property Count: Cardinal read FCount;
  {$IFNDEF Lite}
    property Language: Word read FLanguage;
  {$ENDIF}
  end;

  TCharSubString = class(TSubString)
  private
    FData: PLegacyChar;
    FOptions: TCharStrOptions;
    FOwner: TCharSubString;
  public
  // properties
    property Data: PLegacyChar read FData;
    property Options: TCharStrOptions read FOptions;
    property Owner: TCharSubString read FOwner;
  end;

  TLatinSubString = class(TSubString)
  private
    FData: PLegacyChar;
    FOptions: TLatinStrOptions;
    FOwner: TLatinSubString;
  public
  // properties
    property Data: PLegacyChar read FData;
    property Options: TLatinStrOptions read FOptions;
    property Owner: TLatinSubString read FOwner;
  end;

  TUTFCharSubString = TCharSubString;
  TUTF7SubString = TUTFCharSubString;
  TUTF8SubString = TUTFCharSubString;

  TWideSubString = class(TSubString)
  private
    FData: PWideChar;
    FOptions: TWideStrOptions;
    FOwner: TWideSubString;
  public
  // properties
    property Data: PWideChar read FData;
    property Options: TWideStrOptions read FOptions;
    property Owner: TWideSubString read FOwner;
  end;

  TUTF16SubString = TWideSubString;

  TQuadSubString = class(TSubString)
  private
    FData: PQuadChar;
    FOptions: TQuadStrOptions;
    FOwner: TQuadSubString;
  public
  // properties
    property Data: PQuadChar read FData;
    property Options: TQuadStrOptions read FOptions;
    property Owner: TQuadSubString read FOwner;
  end;

  TUTF32SubString = TQuadSubString;

  TLegacyCharSet = class;

  TLegacySubString = class(TSubString)
  private
    FData: PLegacyChar;
    FOptions: TLegacyStrOptions;
    FOwner: TLegacySubString;
    FCharSet: TLegacyCharSet;
  public
  // properties
    property CharSet: TLegacyCharSet read FCharSet;
    property Data: PLegacyChar read FData;
    property Options: TLegacyStrOptions read FOptions;
    property Owner: TLegacySubString read FOwner;
  end;

{$IFDEF Unicode}
  TCoreSubString = TUTF16SubString;
{$ELSE}
  TCoreSubString = TLegacySubString;
{$ENDIF}

  TString = class(TSubString)
  private
    procedure SetCount(Value: Cardinal);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure SetData(Data: Pointer; Options: TStringOptions = []); overload;
    procedure SetData(Data: Pointer; Count: Cardinal; Options: TStringOptions = []); overload;
  // properties
    property Count write SetCount;
  {$IFNDEF Lite} {$WARNINGS OFF}
    property Language write FLanguage;
  {$WARNINGS ON} {$ENDIF}
  end;

  TCharString = class(TString)
  private
    FData: PLegacyChar;
  //  FOptions: TCharStrOptions;
  //  FOwner: TCharSubString;
    procedure SetData(Value: PLegacyChar); overload;
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Data: Pointer): Cardinal; override;
  // properties
    property Data: PLegacyChar read FData write SetData;
  end;

  TLatinString = class(TCharString)
  private
  //  FData: PLegacyChar;
    FOptions: TLatinStrOptions;
    FOwner: TLatinSubString;
  {$IFNDEF Lite}
    procedure EstimateEndianEx(var Info; Options: TEncodeEndianStrOptionsEx);
  {$ENDIF}
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}

  // properties
    property Options: TLatinStrOptions read FOptions;
    property Owner: TLatinSubString read FOwner;
  end;

  TUTFCharString = class(TCharString)
  private
  //  FData: PLegacyChar;
    FOptions: TUTFCharStrOptions;
    FOwner: TUTFCharSubString;
  public
  // properties
    property Options: TUTFCharStrOptions read FOptions;
    property Owner: TUTFCharSubString read FOwner;
  end;

  TUTF7String = class(TUTFCharString)
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}
  end;

  TUTF8String = class(TUTFCharString)
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}
  end;

  TWideString = class(TString)
  private
    FData: PWideChar;
    FOptions: TEndianStrOptions;
    FOwner: TWideSubString;
    procedure SetData(Value: PWideChar); overload;
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Data: Pointer): Cardinal; override;
  // properties
    property Data: PWideChar read FData write SetData;
    property Options: TEndianStrOptions read FOptions;
    property Owner: TWideSubString read FOwner;
  end;

  TUTF16String = class(TWideString)
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}
  end;

  TQuadString = class(TString)
  private
    FData: PQuadChar;
    FOptions: TEndianStrOptions;
    FOwner: TQuadSubString;
    procedure SetData(Value: PQuadChar); overload;
  public
    class function ByteCount(Count: Cardinal): Cardinal; override;
    class function Length(Data: Pointer): Cardinal; override;
  // properties
    property Data: PQuadChar read FData write SetData;
    property Options: TEndianStrOptions read FOptions;
    property Owner: TQuadSubString read FOwner;
  end;

  TUTF32String = class(TQuadString)
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;
  {$ENDIF}

    function EstimateUTF16(Options: TEncodeUTF16Options = []): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}
  end;

  TLegacyString = class(TLatinString)
  private
    FCharSet: TLegacyCharSet;
  protected
    function GetCodePage: Cardinal; override;
  public
  {$IFNDEF Lite}
    class function EstimateLatin(const Info;
      Options: TEncodeLatinOptions = []): Cardinal; override;
    procedure EstimateLatinEx(var Info: TLatinStrInfo;
      Options: TEncodeLatinOptionsEx = []); override;
  {$ENDIF}

  {$IFDEF UTF7}
    function EstimateUTF7(Options: TEncodeUTF7Options): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF7(const Info;
      Options: TEncodeUTF7Options): Cardinal; override;
    procedure EstimateUTF7Ex(var Info: TUTF7StrInfo;
      Options: TEncodeUTF7OptionsEx = []); override;
  {$ENDIF}
  {$ENDIF}

    function EstimateUTF8(Options: TEncodeUTF8Options = [];
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
  {$IFNDEF Lite}
    class function EstimateUTF8(const Info; Options: TEncodeUTF8Options;
      Compliance: TUTF8Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF8Ex(var Info: TUTF8StrInfo; Options: TEncodeUTF8OptionsEx = [];
      Compliance: TUTF8Compliance = cpRegular); override;

    class function EstimateUTF16(const Info;
      Options: TEncodeUTF16Options): Cardinal; override;
    procedure EstimateUTF16Ex(var Info: TUTF16StrInfo;
      Options: TEncodeUTF16OptionsEx = []); override;
  {$ENDIF}

  {$IF Defined(UTF32) and not Defined(Lite)}
    class function EstimateUTF32(const Info; Options: TEncodeUTF32Options = [];
      Compliance: TUTF32Compliance = cpRegular): Cardinal; override;
    procedure EstimateUTF32Ex(var Info: TUTF32StrInfo; Options: TEncodeUTF32OptionsEx = [];
      Compliance: TUTF32Compliance = cpRegular); override;
  {$IFEND}

  // properties
    property CharSet: TLegacyCharSet read FCharSet write FCharSet;
  end;

{$IFDEF Unicode}
  TCoreString = TUTF16String;
{$ELSE}
  TCoreString = TLegacyString;
{$ENDIF}

  // mapping from 0 for EBCDIC compliance
  TSingleByteMap = array[LegacyChar] of WideChar;

  TLegacyCharSet = class(TListItem)
  private
    FOwner: TList;
    FNext: TListItem;
  {$IFNDEF Lite}
    FBlocks: TUnicodeBlocks;
  {$ENDIF}
    FCodePage: Cardinal;
    FCodePageName: PCoreChar;
    FMap: TSingleByteMap;
    FWideMapMin, FWideMapMax: WideChar;
    function GetWideMapCount: Word;
  protected
  // properties
    property Map: TSingleByteMap read FMap;
    property Next: TListItem read FNext;
    property Owner: TList read FOwner;
    property SingleByteMap: TSingleByteMap read FMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; virtual; abstract;
  // properties
  {$IFNDEF Lite}
    property Blocks: TUnicodeBlocks read FBlocks;
  {$ENDIF}
    property CodePage: Cardinal read FCodePage;
    property CodePageName: PCoreChar read FCodePageName;
    property WideMapCount: Word read GetWideMapCount;
    property WideMapMin: WideChar read FWideMapMin;
    property WideMapMax: WideChar read FWideMapMax;
  end;

  TSingleByteCharSet = class(TLegacyCharSet)
  private
    FWideMap: PLegacyChar;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; override;
  // properties
    property Map;
    property WideMap: PLegacyChar read FWideMap;
  end;

  PDoubleByteChar = ^TDoubleByteChar;
  TDoubleByteChar = packed record
    case Byte of
      0: (SingleByte: LegacyChar);
      1: (LeadByte, TrailByte: LegacyChar);
  end;

  TLeadByte = #$80..#$FF;
  TTrailByteMap = ^TSingleByteMap;
  TDoubleByteMap = array[TLeadByte] of TTrailByteMap;

  PWideMap = ^TWideMap;
  TWideMap = array[Word] of TDoubleByteChar;

  TDoubleByteCharSet = class(TLegacyCharSet)
  private
    FMap: TDoubleByteMap;
    FWideMap: PWideMap;
  public
    constructor Create(const Info: TCPInfoEx);
    destructor Destroy; override;
    class function MaxCharBytes: Byte; override;
  // properties
    property DoubleByteMap: TDoubleByteMap read FMap;
    property WideMap: PWideMap read FWideMap;
  end;

  TLegacyCharSets = class
  private
    FACP, FOEMCP: Cardinal;
    FItems: TList;
    function GetItem(CodePage: Cardinal): TLegacyCharSet;
    function GetItemCount: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    function Find(CodePage: Cardinal): TLegacyCharSet;
    procedure NewString(var Dest: TString; CodePage: Cardinal; Data: Pointer;
      Options: TCharSetOptions = []); overload;
    procedure NewString(var Dest: TString; CodePage: Cardinal; Data: Pointer;
      Count: Cardinal; Options: TCharSetOptions = []); overload;
    function Translate(CodePage: Cardinal): Cardinal;
  // properties
    property ACP: Cardinal read FACP write FACP;
    property ItemCount: Cardinal read GetItemCount;
    property Items[CodePage: Cardinal]: TLegacyCharSet read GetItem; default;
    property OEMCP: Cardinal read FOEMCP write FOEMCP;
  end;

implementation

uses
  Exceptions;

{ TSubString }

destructor TSubString.Destroy;
begin
  with TCharSubString(Self) do
  begin
    FOwner.Release;
    if not (soAttachBuffer in FOptions) then
      FreeMem(FData);
  end;
  inherited;
end;

function TSubString.ByteCount: Cardinal;
begin
  Result := ByteCount(FCount);
end;

class function TSubString.ByteCount(Data: Pointer): Cardinal;
begin
  Result := ByteCount(Length(Data));
end;

procedure TSubString.EncodeLatin(Dest: TCharString; Options: TEncodeLatinOptions);
begin
  Dest.SetCount(FCount);
  Dest.SetCount(EncodeLatin(Dest, 0, Options));
end;

{$IFNDEF Lite}
procedure TSubString.EncodeLatin(const Info; Dest: TCharString;
  Options: TEncodeLatinOptions);
begin
  Dest.SetCount(EstimateLatin(Info, Options));
  Dest.SetCount(EncodeLatin(Dest, 0, Options));
end;

function TSubString.EncodeLatinEx(Dest: TCharString;
  Options: TEncodeLatinOptionsEx): TLatinStrInfo;
begin
  Dest.SetCount(FCount);
  FillChar(Result, SizeOf(Result), 0);
  EncodeLatinEx(Result, Dest, 0, Options);
  Dest.SetCount(Result.Count);
end;
{$ENDIF}

{$IFDEF UTF7}
procedure TSubString.EncodeUTF7(Dest: TCharString; Options: TEncodeUTF7Options);
begin
  Dest.SetCount(EstimateUTF7(Options));
  Dest.SetCount(EncodeUTF7(Dest, 0, Options));
end;

{$IFNDEF Lite}
procedure TSubString.EncodeUTF7(const Info; Dest: TCharString;
  Options: TEncodeUTF7Options);
begin
  Dest.SetCount(EstimateUTF7(Info, Options));
  Dest.SetCount(EncodeUTF7(Dest, 0, Options));
end;

function TSubString.EncodeUTF7Ex(Dest: TCharString;
  Options: TEncodeUTF7OptionsEx): TUTF7StrInfo;
begin
  Dest.SetCount(EstimateUTF7(Options));
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF7Ex(Result, Dest, 0, Options);
  Dest.SetCount(Result.Count);
end;
{$ENDIF}
{$ENDIF}

procedure TSubString.EncodeUTF8(Dest: TCharString; Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance);
begin
  Dest.SetCount(EstimateUTF8(Options, Compliance));
  Dest.SetCount(EncodeUTF8(Dest, 0, Options, Compliance));
end;

{$IFNDEF Lite}
procedure TSubString.EncodeUTF8(const Info; Dest: TCharString;
  Options: TEncodeUTF8Options; Compliance: TUTF8Compliance);
begin
  Dest.SetCount(EstimateUTF8(Info, Options, Compliance));
  Dest.SetCount(EncodeUTF8(Dest, 0, Options, Compliance));
end;

function TSubString.EncodeUTF8Ex(Dest: TCharString; Options: TEncodeUTF8OptionsEx;
  Compliance: TUTF8Compliance): TUTF8StrInfo;
begin
  Dest.SetCount(EstimateUTF8(Options, Compliance));
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF8Ex(Result, Dest, 0, Options, Compliance);
  Dest.SetCount(Result.Count);
end;
{$ENDIF}

procedure TSubString.EncodeUTF16(Dest: TWideString; Options: TEncodeUTF16Options);
begin
  Dest.SetCount(EstimateUTF16(Options));
  Dest.SetCount(EncodeUTF16(Dest, 0, Options));
end;

{$IFNDEF Lite}
procedure TSubString.EncodeUTF16(const Info; Dest: TWideString;
  Options: TEncodeUTF16Options);
begin
  Dest.SetCount(EstimateUTF16(Info, Options));
  Dest.SetCount(EncodeUTF16(Dest, 0, Options));
end;

function TSubString.EncodeUTF16Ex(Dest: TWideString;
  Options: TEncodeUTF16OptionsEx): TUTF16StrInfo;
begin
  Dest.SetCount(EstimateUTF16(Options));
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF16Ex(Result, Dest, 0, Options);
  Dest.SetCount(Result.Count);
end;
{$ENDIF}

{$IFDEF UTF32}
procedure TSubString.EncodeUTF32(Dest: TQuadString; Options: TEncodeUTF32Options;
  Compliance: TUTF32Compliance);
begin
  Dest.SetCount(FCount);
  Dest.SetCount(EncodeUTF32(Dest, 0, Options));
end;

{$IFNDEF Lite}
procedure TSubString.EncodeUTF32(const Info; Dest: TQuadString;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance);
begin
  Dest.SetCount(EstimateUTF32(Info, Options));
  Dest.SetCount(EncodeUTF32(Dest, 0, Options));
end;

function TSubString.EncodeUTF32Ex(Dest: TQuadString;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance): TUTF32StrInfo;
begin
  Dest.SetCount(FCount);
  FillChar(Result, SizeOf(Result), 0);
  EncodeUTF32Ex(Result, Dest, 0, Options);
  Dest.SetCount(Result.Count);
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF Lite}
function TSubString.EstimateLatinEx(Options: TEncodeLatinOptionsEx): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EstimateLatinEx(Result, Options);
end;

{$IFDEF UTF7}
function TSubString.EstimateUTF7Ex(Options: TEncodeUTF7OptionsEx): TUTF7StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EstimateUTF7Ex(Result, Options);
end;
{$ENDIF}

function TSubString.EstimateUTF8Ex(Options: TEncodeUTF8OptionsEx;
  Compliance: TUTF8Compliance): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EstimateUTF8Ex(Result, Options, Compliance);
end;

function TSubString.EstimateUTF16Ex(Options: TEncodeUTF16OptionsEx): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EstimateUTF16Ex(Result, Options);
end;

{$IFDEF UTF32}
function TSubString.EstimateUTF32Ex(Options: TEncodeUTF32OptionsEx;
  Compliance: TUTF32Compliance): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  EstimateUTF32Ex(Result, Options);
end;
{$ENDIF}
{$ENDIF}

{ TString }

destructor TString.Destroy;
begin
  Clear;
  inherited;
end;

procedure TString.Clear;
begin
  SetData(nil, 0);
end;

procedure TString.SetCount(Value: Cardinal);
begin
  SetData(nil, Value);
end;

procedure TString.SetData(Data: Pointer; Options: TStringOptions);
begin
  SetData(Data, Length(Data), Options);
end;

procedure TString.SetData(Data: Pointer; Count: Cardinal; Options: TStringOptions);
var
  Bytes, ZeroBytes: Cardinal;
begin
  if not (soAttachBuffer in TUTF8String(Self).FOptions) then
    FreeMemAndNil(TUTF8String(Self).FData);
  TUTF8String(Self).FOptions := Options;
  if soAttachBuffer in Options then
  begin
    TUTF8String(Self).FData := Data;
    FCount := Count;
    if Data = nil then
      Exclude(TUTF8String(Self).FOptions, soAttachBuffer);
  end
  else
  begin
    if Count <> 0 then
    begin
      Bytes := ByteCount(Count);
      ZeroBytes := ByteCount(1);
      ReallocMem(TUTF8String(Self).FData, Bytes + ZeroBytes);
      if (Data <> nil) and (TUTF8String(Self).FData <> Data) then
        Move(Data^, TUTF8String(Self).FData^, Bytes);
      FillChar(TUTF8String(Self).FData[Bytes], ZeroBytes, 0);
    end;
    FCount := Count;
  end;
end;

{ TCharString }

class function TCharString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count;
end;

class function TCharString.Length(Data: Pointer): Cardinal;
begin
  Result:= StrLen(Data);
end;

procedure TCharString.SetData(Value: PLegacyChar);
begin
  SetData(Value, StrLen(Value));
end;

{ TLatinString }

{$IFNDEF Lite}
procedure TLatinString.EstimateEndianEx(var Info;
  Options: TEncodeEndianStrOptionsEx);
var
  I: Cardinal;
  T, X: LongWord;
begin
  if FCount <> 0 then
  begin
    Inc(TStringInfo(Info).Count, FCount);
    // Fast core
    if soLatin1 in FOptions then
    begin
      if esRangeBlocks in Options then
      begin
        for I := 0 to FCount div SizeOf(LongWord) - 1 do
        begin
          T := PLongWordArray(FData)[I];
          if T and $80808080 <> 0 then
          begin
            Include(TStringInfo(Info).Blocks, cbLatin1Supplement);
            if
              ((T and $FF) = 0) or
              ((T and $FF00) = 0) or
              ((T and $FF0000) = 0) or
              ((T and $FF000000) = 0) then
            begin
              Include(TStringInfo(Info).Blocks, cbBasicLatin);
              Exit;
            end;
          end
          else
            with TStringInfo(Info) do
            begin
              if cbLatin1Supplement in Blocks then
              begin
                Include(Blocks, cbBasicLatin);
                Exit;
              end;
            end;
        end;

        for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
          Include(TStringInfo(Info).Blocks,
            TUnicodeBlock(Byte(cbBasicLatin) + (Byte(FData[I]) and $80) shr 7)
          );
      end;
    end
    else if esForceInvalid in Options then
    begin
      X := 0;

      for I := 0 to FCount div SizeOf(LongWord) - 1 do
      begin
        T := PLongWordArray(FData)[I];
        Inc(X,
          (T and $80) shr 7 +
          (T and $8000) shr 15 +
          (T and $800000) shr 23 +
          (T and $80000000) shr 31
        );
      end;

      for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
        Inc(X, (Byte(FData[I]) and $80) shr 7);

      if X <> 0 then
        with TStringInfo(Info) do
        begin
          Inc(InvalidCount, X);
          Include(Blocks, cbLatin1Supplement);
          if X <> FCount then
            Include(Blocks, cbBasicLatin);
        end;
    end
    else
    begin
      for I := 0 to FCount div SizeOf(LongWord) - 1 do
      begin
        if PLongWordArray(FData)[I] and $80808080 <> 0 then
        begin
          with TStringInfo(Info) do
          begin
            InvalidChar := FData + I * SizeOf(LongWord);
            while Byte(InvalidChar^) and $80 = 0 do
              Inc(PLegacyChar(InvalidChar));
            T := InvalidChar - FData;
            if T <> 0 then
            begin
              Inc(Count, T);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
      end;

      for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
      begin
        if Byte(FData[I]) and $80 <> 0 then
        begin
          with TStringInfo(Info) do
          begin
            InvalidChar := FData + I;
            if I <> 0 then
            begin
              Inc(Count, I);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
      end;

      Include(TStringInfo(Info).Blocks, cbBasicLatin);
    end;
  end;
end;

class function TLatinString.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  Result := TLatinStrInfo(Info).Count;
  if Options * [esLatin1, esForceInvalid] = [] then
    Dec(Result, TLatinStrInfo(Info).Latin1Count);
end;

procedure TLatinString.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx = []);
var
  I: Cardinal;
  T, X: LongWord;
begin
  if FCount <> 0 then
  begin
    // Fast core
    if Options * [esLatin1, esForceInvalid] <> [] then
    begin
      X := 0;

      for I := 0 to FCount div SizeOf(LongWord) - 1 do
      begin
        T := PLongWordArray(FData)[I];
        Inc(X,
          (T and $80) shr 7 +
          (T and $8000) shr 15 +
          (T and $800000) shr 23 +
          (T and $80000000) shr 31
        );
      end;

      for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
        Inc(X, (Byte(FData[I]) and $80) shr 7);

      if esLatin1 in Options then
        with Info do
        begin
          Inc(Latin1Count, X);
          if X < FCount then
            Include(Blocks, cbBasicLatin);
          Include(Blocks, cbLatin1Supplement);
        end
      else
        with Info do
        begin
          Inc(InvalidCount, X);
          Include(Blocks, cbBasicLatin);
        end;
      Inc(Info.Count, FCount);
    end
    else
    begin
      for I := 0 to FCount div SizeOf(LongWord) - 1 do
      begin
        if PLongWordArray(FData)[I] and $80808080 <> 0 then
        begin
          with Info do
          begin
            InvalidChar := FData + I * SizeOf(LongWord);
            while Byte(InvalidChar^) and $80 = 0 do
              Inc(InvalidChar);
            T := InvalidChar - FData;
            if T <> 0 then
            begin
              Inc(Count, T);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
      end;

      for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
      begin
        if Byte(FData[I]) and $80 <> 0 then
        begin
          with Info do
          begin
            InvalidChar := FData + I;
            if I <> 0 then
            begin
              Inc(Count, I);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
      end;

      with Info do
      begin
        Inc(Count, FCount);
        Include(Blocks, cbBasicLatin);
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF UTF7}
function TLatinString.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  if soLatin1 in FOptions then
    Result := FCount * 4
  else if esForceInvalid in Options then
    Result := FCount * Unknown_UTF7_Bytes
  else
    Result := FCount * 2; { due to +-}
end;

{$IFNDEF Lite}
class function TLatinString.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  with TLatinStrInfo(Info) do
    if (Latin1Count = 0) and (esForceInvalid in Options) then
      Result := Count * Unknown_UTF7_Bytes
    else
      Result := (Count - Latin1Count) * 2 + Latin1Count * 4;
end;

procedure TLatinString.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TLatinString.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  if soLatin1 in FOptions then
    Result := FCount * 2
  else if esForceInvalid in Options then
    Result := FCount * Unknown_UTF8_Bytes
  else
    Result := FCount;
end;

{$IFNDEF Lite}
class function TLatinString.EstimateUTF8(const Info;
  Options: TEncodeUTF8Options; Compliance: TUTF8Compliance): Cardinal;
begin
  with TLatinStrInfo(Info) do
    if (Latin1Count = 0) and (esForceInvalid in Options) then
      Result := Count * Unknown_UTF8_Bytes
    else
      Result := Count + Latin1Count;
end;

procedure TLatinString.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
var
  B: Byte;
  I: Cardinal;
  T, X, Z: LongWord;
begin
  if FCount <> 0 then
  begin
    // Fast core
    if (soLatin1 in FOptions) or (esForceInvalid in Options) then
    begin
      X := 0;

      if Compliance = cpModified then
      begin
        Z := 0;

        for I := 0 to FCount div SizeOf(LongWord) - 1 do
        begin
          T := PLongWordArray(FData)[I];
          Inc(X,
            (T and $80) shr 7 +
            (T and $8000) shr 15 +
            (T and $800000) shr 23 +
            (T and $80000000) shr 31
          );
          Inc(Z,
            Byte(T or $FF = 0) +
            Byte(T or $FF00 = 0) +
            Byte(T or $FF0000 = 0) +
            Byte(T or $FF000000 = 0)
          );
        end;

        for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
        begin
          B := Byte(FData[I]);
          Inc(X, (B and $80) shr 7);
          Inc(Z, Byte(B = 0));
        end;

        Inc(Info.Count, Z);
      end
      else
      begin
        for I := 0 to FCount div SizeOf(LongWord) - 1 do
        begin
          T := PLongWordArray(FData)[I];
          Inc(X,
            (T and $80) shr 7 +
            (T and $8000) shr 15 +
            (T and $800000) shr 23 +
            (T and $80000000) shr 31
          );
        end;

        for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
          Inc(X, (Byte(FData[I]) and $80) shr 7);
      end;

      with Info do
      begin
        Inc(Count, FCount);
        Inc(UnicodeCount, FCount);
        if X <> 0 then
          if soLatin1 in FOptions then
          begin
            Inc(Count, X);
            Include(Blocks, cbLatin1Supplement);
          end
          else
          begin
            Inc(Count, X * Unknown_UTF8_Bytes);
            Include(Blocks, cbSpecials);
          end;
        if X <> FCount then
          Include(Blocks, cbBasicLatin);
      end;
    end
    else
    begin
      Z := 0;

      for I := 0 to FCount div SizeOf(LongWord) - 1 do
      begin
        T := PLongWordArray(FData)[I];
        if T and $80808080 <> 0 then
        begin
          with Info do
          begin
            InvalidChar := FData + I * SizeOf(LongWord);
            while Byte(InvalidChar^) and $80 = 0 do
              Inc(InvalidChar);
            T := InvalidChar - FData;
            if T <> 0 then
            begin
              Inc(Count, T);
              if Compliance = cpModified then
                Inc(Count, Z);
              Inc(UnicodeCount, T);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
        if Compliance = cpModified then
          Inc(Z,
            Byte(T or $FF = 0) +
            Byte(T or $FF00 = 0) +
            Byte(T or $FF0000 = 0) +
            Byte(T or $FF000000 = 0)
          );
      end;

      for I := FCount - FCount mod SizeOf(LongWord) to FCount - 1 do
      begin
        B := Byte(FData[I]);
        if B and $80 <> 0 then
        begin
          with Info do
          begin
            InvalidChar := FData + I;
            if I <> 0 then
            begin
              Inc(Count, I);
              if Compliance = cpModified then
                Inc(Count, Z);
              Inc(UnicodeCount, I);
              Include(Blocks, cbBasicLatin);
            end;
          end;
          Exit;
        end;
        Inc(Z, Byte(B = 0));
      end;

      with Info do
      begin
        Inc(Count, FCount);
        if Compliance = cpModified then
          Inc(Count, Z);
        Inc(UnicodeCount, FCount);
        Include(Blocks, cbBasicLatin);
      end;
    end;
  end;
end;
{$ENDIF}

function TLatinString.EstimateUTF16(Options: TEncodeUTF16Options): Cardinal;
begin
  Result := FCount;
end;

{$IFNDEF Lite}
class function TLatinString.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  Result := TLatinStrInfo(Info).Count;
end;

procedure TLatinString.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  EstimateEndianEx(Info, Options);
end;

{$IFDEF UTF32}
class function TLatinString.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  Result := TLatinStrInfo(Info).Count;
end;

procedure TLatinString.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  EstimateEndianEx(Info, Options);
end;
{$ENDIF}
{$ENDIF}

function TLatinString.GetCodePage: Cardinal;
begin
  if soLatin1 in FOptions then
    Result := CP_ISO_Latin1
  else
    Result := CP_US_ASCII;
end;

{ TUTF7String }

{$IFNDEF Lite}
class function TUTF7String.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  Result := TUTF7StrInfo(Info).UnicodeCount;
  if not (esForceInvalid in Options) then
    Dec(Result, TUTF7StrInfo(Info).SurrogateCount);
end;

procedure TUTF7String.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx);
begin
  // TODO
end;
{$ENDIF}

{$IFDEF UTF7}
function TUTF7String.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  if esForceInvalid in Options then
    Result := FCount * Unknown_UTF7_Bytes
  else
    Result := FCount;
end;

{$IFNDEF Lite}
class function TUTF7String.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  with TUTF7StrInfo(Info) do
    Result := (UnicodeCount + SurrogateCount) * Unknown_UTF7_Bytes;
end;

procedure TUTF7String.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF7String.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  Result := FCount div 2;
end;

{$IFNDEF Lite}
class function TUTF7String.EstimateUTF8(const Info;
  Options: TEncodeUTF8Options; Compliance: TUTF8Compliance): Cardinal;
begin
  with TUTF7StrInfo(Info) do
    if Compliance in [cpRegular..cpAllowNonUnicode] then
      Result := UnicodeCount * 4
    else
      Result := (UnicodeCount + SurrogateCount) * 3;
end;

procedure TUTF7String.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
begin
  // TODO
end;
{$ENDIF}

function TUTF7String.EstimateUTF16(Options: TEncodeUTF16Options): Cardinal;
begin
  Result := FCount;
end;

{$IFNDEF Lite}
class function TUTF7String.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  with TUTF7StrInfo(Info) do
    Result := UnicodeCount + SurrogateCount;
end;

procedure TUTF7String.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  // TODO
end;

{$IFDEF UTF32}
class function TUTF7String.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  Result := TUTF7StrInfo(Info).UnicodeCount;
end;

procedure TUTF7String.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF7String.GetCodePage: Cardinal;
begin
  Result := CP_UTF7;
end;

{ TUTF8String }

{$IFNDEF Lite}
class function TUTF8String.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  with TUTF8StrInfo(Info) do
  begin
    Result := UnicodeCount;
    if Compliance in [cpRegular..cpAllowNonUnicode] then
    begin
      if esForceInvalid in Options then
        Inc(Result, NonUnicodeCount);
    end
    else if not (esForceInvalid in Options) then
      Dec(Result, SurrogateCount);
  end;
end;

procedure TUTF8String.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx);
begin
  if FCount <> 0 then
  begin
    // TODO
  end;
end;
{$ENDIF}

{$IFDEF UTF7}
function TUTF8String.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  Result := (FCount div 2) * Unknown_UTF7_Bytes;
end;

{$IFNDEF Lite}
class function TUTF8String.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  with TUTF8StrInfo(Info) do
    if esForceInvalid in Options then
      Result := (UnicodeCount + NonUnicodeCount) * Unknown_UTF7_Bytes
    else
      Result := UnicodeCount * Unknown_UTF7_Bytes;
end;

procedure TUTF8String.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF8String.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  if esForceInvalid in Options then
    Result := FCount * Unknown_UTF8_Bytes
  else if Compliance in [cpRegular..cpAllowNonUnicode] then
    Result := FCount
  else
    Result := FCount * 2 - FCount div 2;
end;

{$IFNDEF Lite}
class function TUTF8String.EstimateUTF8(const Info; Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  if TUTF8StrInfo(Info).Compliance = Compliance then
    Result := TUTF8StrInfo(Info).Count
  else
    case Compliance of
      cpRegular:
        Result := TUTF8StrInfo(Info).UnicodeCount * 4;
      cpAllowNonUnicode:
        with TUTF8StrInfo(Info) do
          Result := UnicodeCount * 4 + NonUnicodeCount * 6;
    else
      Result := TUTF8StrInfo(Info).UnicodeCount * 6; {3 + 3}
    end;
end;

procedure TUTF8String.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
begin
  // TODO
end;
{$ENDIF}

function TUTF8String.EstimateUTF16(Options: TEncodeUTF16Options): Cardinal;
begin
  Result := FCount;
end;

{$IFNDEF Lite}
class function TUTF8String.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  with TUTF8StrInfo(Info) do
    if esForceInvalid in Options then
      Result := UnicodeCount + NonUnicodeCount
    else
      Result := UnicodeCount;
end;

procedure TUTF8String.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  // TODO
end;

{$IFDEF UTF32}
class function TUTF8String.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  with TUTF8StrInfo(Info) do
    Result := UnicodeCount + NonUnicodeCount;
end;

procedure TUTF8String.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF8String.GetCodePage: Cardinal;
begin
  Result := CP_UTF8;
end;

{ TWideString }

class function TWideString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count * SizeOf(WideChar);
end;

class function TWideString.Length(Data: Pointer): Cardinal;
begin
  Result := WideStrLen(Data);
end;

procedure TWideString.SetData(Value: PWideChar);
begin
  SetData(Value, WideStrLen(Value));
end;

{ TUTF16String }

{$IFNDEF Lite}
class function TUTF16String.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  with TUTF16StrInfo(Info) do
  begin
    Result := Count - SurrogateCount;
    if not (esForceInvalid in Options) then
      Dec(Result, SurrogateCount);
  end;
end;

procedure TUTF16String.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx);
begin
  // TODO
end;
{$ENDIF}

{$IFDEF UTF7}
function TUTF16String.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  Result := FCount * Unknown_UTF7_Bytes;
end;

{$IFNDEF Lite}
class function TUTF16String.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  Result := TUTF16StrInfo(Info).Count * Unknown_UTF7_Bytes;
end;

procedure TUTF16String.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF16String.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  Result := FCount * 3;
end;

{$IFNDEF Lite}
class function TUTF16String.EstimateUTF8(const Info; Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  with TUTF16StrInfo(Info) do
    if Compliance in [cpRegular..cpAllowNonUnicode] then
      Result := Count * 3 + SurrogateCount
    else
      Result := (Count + SurrogateCount) * 3;
end;

procedure TUTF16String.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
begin
  // TODO
end;
{$ENDIF}

function TUTF16String.EstimateUTF16(Options: TEncodeUTF16Options): Cardinal;
begin
  Result := FCount;
end;

{$IFNDEF Lite}
class function TUTF16String.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  Result := TUTF16StrInfo(Info).Count;
end;

procedure TUTF16String.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  // TODO
end;

{$IFDEF UTF32}
class function TUTF16String.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  with TUTF16StrInfo(Info) do
    Result := Count - SurrogateCount;
end;

procedure TUTF16String.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF16String.GetCodePage: Cardinal;
begin
  Result := CP_UTF16 + Byte(soBigEndian in FOptions);
end;

{ TQuadString }

class function TQuadString.ByteCount(Count: Cardinal): Cardinal;
begin
  Result := Count * SizeOf(QuadChar);
end;

class function TQuadString.Length(Data: Pointer): Cardinal;
begin
  Result := QuadStrLen(Data);
end;

procedure TQuadString.SetData(Value: PQuadChar);
begin
  SetData(Value, QuadStrLen(Value));
end;

{ TUTF32String }

{$IFNDEF Lite}
class function TUTF32String.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  Result := TUTF32StrInfo(Info).Count;
  if not (esForceInvalid in Options) then
    Dec(Result, TUTF32StrInfo(Info).NonUnicodeCount);
end;

procedure TUTF32String.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx);
begin
  // TODO
end;
{$ENDIF}

{$IFDEF UTF7}
function TUTF32String.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  Result := FCount * Unknown_UTF7_Bytes * 2;
end;

{$IFNDEF Lite}
class function TUTF32String.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  with TUTF32StrInfo(Info) do
  begin
    Result := (Count - NonUnicodeCount) * Unknown_UTF7_Bytes * 2;
    if esForceInvalid in Options then
      Inc(Result, NonUnicodeCount * Unknown_UTF7_Bytes);
  end;
end;

procedure TUTF32String.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF32String.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  if Compliance = cpRegular then
    Result := FCount * 4
  else
    Result := FCount * 6;
end;

{$IFNDEF Lite}
class function TUTF32String.EstimateUTF8(const Info; Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  case Compliance of
    cpRegular:
      with TUTF32StrInfo(Info) do
        Result := (Count - NonUnicodeCount) * 4 + NonUnicodeCount * Unknown_UTF8_Bytes;
    cpAllowNonUnicode:
      with TUTF32StrInfo(Info) do
        Result := (Count - NonUnicodeCount) * 4 + NonUnicodeCount * 6;
  else
    with TUTF32StrInfo(Info) do
    begin
      Result := (Count - NonUnicodeCount) * 6;
      if esForceInvalid in Options then
        Inc(Result, NonUnicodeCount * Unknown_UTF8_Bytes);
    end;
  end;
end;

procedure TUTF32String.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
begin
  // TODO
end;
{$ENDIF}

function TUTF32String.EstimateUTF16(Options: TEncodeUTF16Options): Cardinal;
begin
  Result := FCount * 2;
end;

{$IFNDEF Lite}
class function TUTF32String.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  with TUTF32StrInfo(Info) do
    if esForceInvalid in Options then
      Result := Count
    else
      Result := Count - NonUnicodeCount;
end;

procedure TUTF32String.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  // TODO
end;

{$IFDEF UTF32}
class function TUTF32String.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  if (Compliance = cpAllowNonUnicode) or (esForceInvalid in Options) then
    Result := TUTF32StrInfo(Info).Count
  else
    with TUTF32StrInfo(Info) do
      Result := Count - NonUnicodeCount;
end;

procedure TUTF32String.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TUTF32String.GetCodePage: Cardinal;
begin
  Result := CP_UTF32 + Byte(soBigEndian in FOptions);
end;

{ TLegacyString }

{$IFNDEF Lite}
class function TLegacyString.EstimateLatin(const Info;
  Options: TEncodeLatinOptions): Cardinal;
begin
  with TLegacyStrInfo(Info) do
  begin
    Result := Count - MultiByteCount;
    if Options * [esLatin1, esForceInvalid] = [] then
      Dec(Result, MultiByteCount);
  end;
end;

procedure TLegacyString.EstimateLatinEx(var Info: TLatinStrInfo;
  Options: TEncodeLatinOptionsEx);
begin
  // TODO
end;
{$ENDIF}

{$IFDEF UTF7}
function TLegacyString.EstimateUTF7(Options: TEncodeUTF7Options): Cardinal;
begin
  if FCharSet <> nil then
    Result := FCount * Unknown_UTF7_Bytes
  else
    Result := inherited EstimateUTF7(Options);
end;

{$IFNDEF Lite}
class function TLegacyString.EstimateUTF7(const Info;
  Options: TEncodeUTF7Options): Cardinal;
begin
  with TLegacyStrInfo(Info) do
    Result := (Count - MultiByteCount) * Unknown_UTF7_Bytes;
end;

procedure TLegacyString.EstimateUTF7Ex(var Info: TUTF7StrInfo;
  Options: TEncodeUTF7OptionsEx);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TLegacyString.EstimateUTF8(Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  if FCharSet <> nil then
    Result := FCount * Unknown_UTF8_Bytes
  else
    Result := inherited EstimateUTF8(Options, Compliance)
end;

{$IFNDEF Lite}
class function TLegacyString.EstimateUTF8(const Info; Options: TEncodeUTF8Options;
  Compliance: TUTF8Compliance): Cardinal;
begin
  with TLegacyStrInfo(Info) do
    Result := (Count - MultiByteCount) * Unknown_UTF8_Bytes;
end;

procedure TLegacyString.EstimateUTF8Ex(var Info: TUTF8StrInfo;
  Options: TEncodeUTF8OptionsEx; Compliance: TUTF8Compliance);
begin
  // TODO
end;

class function TLegacyString.EstimateUTF16(const Info;
  Options: TEncodeUTF16Options): Cardinal;
begin
  with TLegacyStrInfo(Info) do
    Result := Count - MultiByteCount;
end;

procedure TLegacyString.EstimateUTF16Ex(var Info: TUTF16StrInfo;
  Options: TEncodeUTF16OptionsEx);
begin
  // TODO
end;

{$IFDEF UTF32}
class function TLegacyString.EstimateUTF32(const Info;
  Options: TEncodeUTF32Options; Compliance: TUTF32Compliance): Cardinal;
begin
  with TLegacyStrInfo(Info) do
    Result := Count - MultiByteCount;
end;

procedure TLegacyString.EstimateUTF32Ex(var Info: TUTF32StrInfo;
  Options: TEncodeUTF32OptionsEx; Compliance: TUTF32Compliance);
begin
  // TODO
end;
{$ENDIF}
{$ENDIF}

function TLegacyString.GetCodePage: Cardinal;
begin
  if FCharSet <> nil then
    Result := FCharSet.CodePage
  else
    Result := inherited GetCodePage;
end;

{ TLegacyCharSet }

constructor TLegacyCharSet.Create(const Info: TCPInfoEx);
var
  SourceMap: array[Byte] of LegacyChar;
  B, L: Byte;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
  C: LegacyChar;
  I: Cardinal;
  T: LongWord;
  W: WideChar;
begin
  FCodePage := Info.CodePage;
  FCodePageName := {$IFDEF Unicode} WideStrNew {$ELSE} StrNew {$ENDIF}
    (Info.CodePageName);

  // Fast core
  T := $03020100;
  for I := 0 to SizeOf(SourceMap) div SizeOf(T) - 1 do
  begin
    PLongWordArray(@SourceMap)[I] := T;
    Inc(T, $04040404);
  end;

  B := 0;
  while (B < MAX_LEADBYTES) and (Info.LeadByte[B] <> 0) do
  begin
    L := Info.LeadByte[B];
    FillChar(SourceMap[L], Info.LeadByte[B + 1] - L + 1, 0);
    Inc(B, 2);
  end;

  if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
    (FCodePage, 0, SourceMap, Length(SourceMap), FMap, Length(FMap)) = 0
  then
    RaiseLastPlatformError;

  FWideMapMin := WideChar($100);
{$IFNDEF Lite}
  Block := cbUnknown;
{$ENDIF}
  for C := Low(C) to High(C) do
  begin
    W := FMap[C];
    if (W <> WideChar(0)) and (W <> WideChar(C)) then
      if W = Info.UnicodeDefaultChar then
        FMap[C] := WideChar(0)
      else
      begin
        if W > FWideMapMax then
          FWideMapMax := W
        else if W < FWideMapMin then
          FWideMapMin := W;
      {$IFNDEF Lite}
        Block := FindCharBlock(QuadChar(W), Block);
        Include(FBlocks, Block);
      {$ENDIF}
      end;
  end;
end;

destructor TLegacyCharSet.Destroy;
begin
  FreeMem(FCodePageName);
  inherited;
end;

function TLegacyCharSet.GetWideMapCount: Word;
begin
  Result := Word(FWideMapMax) - Word(FWideMapMin) + 1;
end;

{ TSingleByteCharSet }

constructor TSingleByteCharSet.Create(const Info: TCPInfoEx);
var
  C: LegacyChar;
  W: WideChar;
begin
  inherited;
  if FWideMapMin <= FWideMapMax then
  begin
    FWideMap := AllocMem(Word(FWideMapMax) - Word(FWideMapMin) + 1);
    for C := Low(C) to High(C) do
    begin
      W := FMap[C];
      if W >= FWideMapMin then
        FWideMap[Word(W) - Word(FWideMapMin)] := C;
    end;
  end
  else
  begin
    FWideMapMin := WideChar(0);
    FWideMapMax := WideChar(0);
  end;
end;

destructor TSingleByteCharSet.Destroy;
begin
  FreeMem(FWideMap);
  inherited;
end;

class function TSingleByteCharSet.MaxCharBytes: Byte;
begin
  Result := SizeOf(Char);
end;

{ TDoubleByteCharSet }

constructor TDoubleByteCharSet.Create(const Info: TCPInfoEx);
var
  SourceMap: array[Byte] of TDoubleByteChar;
  B: Byte;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
  C, L: Char;
  I: Word;
  P: TTrailByteMap;
  T: LongWord;
  W: WideChar;
begin
  inherited;

{$IFNDEF Lite}
  Block := cbUnknown;
{$ENDIF}
  B := 0;
  while (B < MAX_LEADBYTES) and (Info.LeadByte[B] <> 0) do
  begin
    for L := LegacyChar(Info.LeadByte[B]) to LegacyChar(Info.LeadByte[B + 1]) do
    begin
      // Fast core
      T := $02000100 or (Byte(L) shl 16) or Byte(L);
      for I := 0 to SizeOf(SourceMap) div SizeOf(T) - 1 do
      begin
        PLongWordArray(@SourceMap)[I] := T;
        Inc(T, $02000200);
      end;
      GetMem(P, SizeOf(TSingleByteMap));
      FMap[L] := P;
      if {$IFDEF Tricks} System. {$ENDIF} MultiByteToWideChar
        (FCodePage, 0, @SourceMap, Length(SourceMap) * SizeOf(TDoubleByteChar),
          PWideChar(P), SizeOf(TSingleByteMap) div SizeOf(WideChar)) = 0
      then
        RaiseLastPlatformError;

      for C := Low(C) to High(C) do
      begin
        W := P[C];
        if W <> WideChar(0) then
          if W = Info.UnicodeDefaultChar then
            P[C] := WideChar(0)
          else
          begin
            if W > FWideMapMax then
              FWideMapMax := W
            else if W < FWideMapMin then
              FWideMapMin := W;
          {$IFNDEF Lite}
            Block := FindCharBlock(QuadChar(W), Block);
            Include(FBlocks, Block);
          {$ENDIF}
          end;
      end;
    end;
    Inc(B, 2);
  end;

  FWideMap := AllocMem((Word(FWideMapMax) - Word(FWideMapMin) + 1) * SizeOf(TDoubleByteChar));

  for L := Low(L) to Pred(Low(TLeadByte)) do
  begin
    W := SingleByteMap[L];
    if W >= FWideMapMin then
      FWideMap[Word(W) - Word(FWideMapMin)].SingleByte := L;
  end;

  for L := Low(TLeadByte) to High(TLeadByte) do
  begin
    P := FMap[L];
    if P <> nil then
    begin
      for C := Low(C) to High(C) do
      begin
        W := P[C];
        if W >= FWideMapMin then
          with FWideMap[Word(W) - Word(FWideMapMin)] do
          begin
            LeadByte := L;
            TrailByte := C;
          end;
      end;
    end
    else
    begin
      W := SingleByteMap[L];
      if W >= FWideMapMin then
        FWideMap[Word(W) - Word(FWideMapMin)].SingleByte := L;
    end;
  end
end;

destructor TDoubleByteCharSet.Destroy;
var
  L: TLeadByte;
begin
  FreeMem(FWideMap);
  for L := Low(L) to High(L) do
    FreeMem(FMap[L]);
  inherited;
end;

class function TDoubleByteCharSet.MaxCharBytes: Byte;
begin
  Result := SizeOf(TDoubleByteChar);
end;

{ TLegacyCharSets }

constructor TLegacyCharSets.Create;
begin
  FACP := GetACP;
  FOEMCP := GetOEMCP;
  FItems := TList.Create;
end;

destructor TLegacyCharSets.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLegacyCharSets.Find(CodePage: Cardinal): TLegacyCharSet;
begin
  case CodePage of
    CP_ACP:
      CodePage := FACP;
    CP_OEMCP:
      CodePage := FOEMCP;
  end;
//  Result := TLegacyCharSet(FItems.First);
  while Result <> nil do
  begin
    if Result.CodePage = CodePage then
      Exit;
    Result := TLegacyCharSet(Result.Next);
  end;
end;

function TLegacyCharSets.GetItem(CodePage: Cardinal): TLegacyCharSet;
var
  Info: TCPInfoEx;
begin
  case CodePage of
    CP_US_ASCII, CP_ISO_Latin1:
      Result := nil;
  else
    Result := Find(CodePage);
    if Result = nil then
    begin
      if not GetCPInfoEx(CodePage, 0, Info) then
        RaiseLastPlatformError;
      case Info.MaxCharSize of
        1: Result := TSingleByteCharSet.Create(Info);
        2: Result := TDoubleByteCharSet.Create(Info);
      else
        raise EString.Create; // TODO: Not impemented yet
      end;  
      FItems.Append(Result);
    end;
  end;
end;

function TLegacyCharSets.GetItemCount: Cardinal;
begin
  Result := FItems.Count;
end;

function DetectCodePage(CodePage: Cardinal; var Data: Pointer; var Count: Cardinal;
  var Options: TCharSetOptions): Cardinal;
begin
  if Count >= SizeOf(Word) then
    case PWord(Data)^ of
      BOM_UTF16_LE:
        begin
          Exclude(Options, soBigEndian);
          if Count >= SizeOf(LongWord) then
            if PLongWord(Data)^ = BOM_UTF32_LE then
            begin
              Inc(PQuadChar(Data));
              Dec(Count, SizeOf(LongWord));
              Result := CP_UTF32_LE;
              Exit;
            end
            else
            begin
              Inc(PWideChar(Data));
              Dec(Count, SizeOf(Word));
              Result := CP_UTF16_LE;
              Exit;
            end;
        end;
      BOM_UTF16_BE:
        begin
          Inc(PWideChar(Data));
          Dec(Count, SizeOf(Word));
          Include(Options, soBigEndian);
          Result := CP_UTF16_BE;
          Exit;
        end;
    else
      if Count >= SizeOf(LongWord) then
        case PLongWord(Data)^ and $00FFFFFF of
          BOM_UTF8:
            begin
              Inc(PLegacyChar(Data), 3);
              Dec(Count, 3);
              Result := CP_UTF8;
              Exit;
            end;
          BOM_UTF7:
            begin
              Inc(PLegacyChar(Data), 3);
              Dec(Count, 3);
              Result := CP_UTF7;
              Exit;
            end;
        else
          if PLongWord(Data)^ = BOM_UTF32_BE then
          begin
            Inc(PQuadChar(Data));
            Dec(Count, SizeOf(QuadChar));
            Include(Options, soBigEndian);
            Result := CP_UTF32_BE;
            Exit;
          end
        end
    end;
  Result := CodePage;
end;

procedure TLegacyCharSets.NewString(var Dest: TString; CodePage: Cardinal;
  Data: Pointer; Options: TCharSetOptions);
var
  Count, CP: Cardinal;
  CharSet: TLegacyCharSet;
begin
  if soDetectCharSet in Options then
  begin
    Count := MaxInt;
    CP := DetectCodePage(CodePage, Data, Count, Options);
    Exclude(Options, soDetectCharSet);
  end
  else
    CP := CodePage;
  case CP of
    CP_UTF8:
      begin
        Dest := TUTF8String.Create;
        Dest.SetData(Data, Options);
        // TODO: continue autodetect
      end;
    CP_US_ASCII, CP_ISO_Latin1:
      begin
        Dest := TLatinString.Create;
        if CP = CP_ISO_Latin1 then
          Include(Options, soLatin1)
        else
          Exclude(Options, soLatin1);
        Dest.SetData(Data, Options);
      end;
    CP_UTF16_LE, CP_UTF16_BE:
      begin
        Dest := TUTF16String.Create;
        Dest.SetData(Data, Options);
      end;
    CP_UTF32_LE, CP_UTF32_BE:
      begin
        Dest := TUTF32String.Create;
        Dest.SetData(Data, Options);
      end;
    CP_UTF7, // TODO: UTF-7 support
      {begin
        Dest := TUTF7String.Create;
        Dest.SetData(Data, Options);
        // TODO: continue autodetect
      end;}
    CP_ISO2022_JP..CP_ISO2022_TC, // 50220..50229, e. g. whole ISO-2022
    CP_GB18030:
      raise EString.Create; // TODO: Not yet implemented
  else
    CharSet := GetItem(CP);
    Dest := TLegacyString.Create;
    TLegacyString(Dest).CharSet := CharSet;
    Dest.SetData(Data, Options);
  end;
end;

procedure TLegacyCharSets.NewString(var Dest: TString; CodePage: Cardinal;
  Data: Pointer; Count: Cardinal; Options: TCharSetOptions);
var
  CP: Cardinal;
  CharSet: TLegacyCharSet;
begin
  if soDetectCharSet in Options then
  begin
    Count := MaxInt;
    CP := DetectCodePage(CodePage, Data, Count, Options);
  end
  else
    CP := CodePage;
  case CP of
    CP_UTF8:
      begin
        Dest := TUTF8String.Create;
        Dest.SetData(Data, Count, Options);
        // TODO: continue autodetect
      end;
    CP_US_ASCII, CP_ISO_Latin1:
      begin
        Dest := TLatinString.Create;
        if CP = CP_ISO_Latin1 then
          Include(Options, soLatin1)
        else
          Exclude(Options, soLatin1);
        Dest.SetData(Data, Count, Options);
      end;
    CP_UTF16_LE, CP_UTF16_BE:
      begin
        Dest := TUTF16String.Create;
        Dest.SetData(Data, Count, Options);
      end;
    CP_UTF32_LE, CP_UTF32_BE:
      begin
        Dest := TUTF32String.Create;
        Dest.SetData(Data, Count, Options);
      end;
    CP_UTF7, // TODO: UTF-7 support
      {begin
        Dest := TUTF7String.Create;
        Dest.SetData(Data, Count, Options);
        // TODO: continue autodetect
      end;}
    CP_ISO2022_JP..CP_ISO2022_TC, // 50220..50229, e. g. whole ISO-2022
    CP_GB18030:
      raise EString.Create; // TODO: Not implemented yet
  else
    CharSet := GetItem(CP);
    Dest := TLegacyString.Create;
    TLegacyString(Dest).CharSet := CharSet;
    Dest.SetData(Data, Count, Options);
  end;
end;

function TLegacyCharSets.Translate(CodePage: Cardinal): Cardinal;
begin
  case CodePage of
    CP_ACP:
      Result := FACP;
    CP_OEMCP:
      Result := FOEMCP;
  else
    Result := CodePage;
  end;
end;

end.

