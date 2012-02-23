(*
    Kolibri OS Unicode Library (UnicodeLib)

    Based on CoreLib with cross-compilation using Delphi

    See full CoreStrings source here:
      http://websvn.tunilab.org/wsvn/TuniLab.UniEnv/Legacy/CoreLib/CoreStrings.pas

    Copyright (c) 2012 The Unified Environment Laboratory

    Conditional defines:
      * UTF32 -- UTF-32 character set support
*)

{$UNDEF Latin} // use built-in Windows-1252 code page support under Kolibri
{$DEFINE Lite} // no Unicode character blocks for Kolibri

type
{
  soDetectCharSet:
    * Code page source: try to decode source as UTF-8, continue as code page on fail
    * UTF-8 source: try to decode source as code page, continue as UTF-8 on fail
    * UTF-16 and UTF-32: try to detect byte order
}
  TStringOption = (soDetectCharSet, soBigEndian, soAttachBuffer);

  TLegacyOptions = set of soAttachBuffer..soAttachBuffer;
  TEndianOptions = set of soBigEndian..soAttachBuffer;

  TLegacySource = set of soDetectCharSet..soDetectCharSet;
  TEndianSource = set of soDetectCharSet..soBigEndian;

{
  coForceInvalid -- replace invalid characters with:
    * U+001A for code page (recommended by Unicode for single-byte character sets)
    * U+FFFD for Unicode Transformation Formats (official Unicode replacement character)
}
  TConvertOption = (coForceInvalid, coBigEndian, coSurrogates);

const
  coCESU8 = coSurrogates;
  coEncodedZero = coBigEndian;
  coModifiedUTF8 = [coCESU8, coEncodedZero]; // UTF-8 compliance

type
  TEncodeLegacy = set of coForceInvalid..coForceInvalid;
  TEncodeUTF32 = set of coForceInvalid..coBigEndian;
  TEncodeUTF16 = set of coForceInvalid..coSurrogates;
  TEncodeUTF8 = set of coForceInvalid..coCESU8;

{ Byte order detection }

function DetectUTF32(Source: QuadChar; Options: TEndianSource): TEndianSource; stdcall;
function DetectUTF16(Source: PWideChar; Options: TEndianSource): TEndianSource; stdcall;

{ Estimate required bytes (UTF-8) or characters (UTF-16) }
                                                // CoreLib requires overload directive to compile
                                                // has no mean for Kolibri
function EstimateUTF8(Count, SurrogateCount: Cardinal; Options: TEncodeUTF8): Cardinal; overload; stdcall;
function EstimateUTF16(Count, SurrogateCount: Cardinal; Options: TEncodeUTF16): Cardinal; overload; stdcall;

{ Displayable and MIME charset names (for browsers) }

type   // False = continue, True = eureka! (stop) :-)
  TEnumCodePages = function(CodePage: Word; DisplayName, MIME: PLegacyChar): Boolean;

function EnumCodePages(Callback: TEnumCodePages): Word;
function GetCharSetDisplayName(CodePage: Word; Language: Byte): PLegacyChar; stdcall; // "Кириллица (DOS)"
function GetCharSetMIME(CodePage: Word; Language: Byte): PLegacyChar; stdcall; // "pc866"

{
  Count means:
    * Bytes for code page and UTF-8
    * Words for UTF-16
    * Double words (characters) for UTF-32

  InvalidChar:
    *  Optional pointer to a single UTF-32 character that receives
       a Unicode character unrepresentable at the destination code page when
       coForceInvalid flag is not set
    *  Received $00..$FF character when decoding UTF-8 means broken UTF-8 sequence
    *  Receive on incomplete CESU-8/UTF-16 surrogate pair:
      *  $D800..$DBFF -- high surrogate
      *  $DC00..$DFFF -- low surrogate
}

{ Convert from Unicode Transformation Formats (UTF) to code page }

function FromUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal; stdcall;
function FromUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal; stdcall;
{$IFDEF UTF32}
function FromUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; CodePage: Word; DestOptions: TEncodeLegacy; InvalidChar: PQuadChar): Cardinal; stdcall;
{$ENDIF}

{ Convert from code page to Unicode Transformation Formats (UTF) }

function ToUTF8(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal; stdcall;
function ToUTF16(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal; stdcall;
{$IFDEF UTF32}
function ToUTF32(Source: PLegacyChar; Count: Cardinal; CodePage: Word; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal; stdcall;
{$ENDIF}

{ Convert between Unicode Transformation Formats (UTF) }
                                                // CoreLib requires overload directive to compile
                                                // has no mean for Kolibri
function UTF16ToUTF16(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
function UTF8ToUTF16(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal; overload; stdcall;

function UTF16ToUTF8(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
function UTF8ToUTF8(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal; overload; stdcall;

{$IFDEF UTF32}
function UTF32ToUTF8(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PLegacyChar; DestOptions: TEncodeUTF8; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
function UTF32ToUTF16(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PWideChar; DestOptions: TEncodeUTF16; InvalidChar: PQuadChar): Cardinal; overload; stdcall;

function UTF8ToUTF32(Source: PLegacyChar; Count: Cardinal; SourceOptions: TLegacySource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
function UTF16ToUTF32(Source: PWideChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
function UTF32ToUTF32(Source: PQuadChar; Count: Cardinal; SourceOptions: TEndianSource;
  Dest: PQuadChar; DestOptions: TEncodeUTF32; InvalidChar: PQuadChar): Cardinal; overload; stdcall;
{$ENDIF}

{ Kolibri raw code page implementation }

type
  TRawMap = array[#$80..#$FF] of WideChar; // no EBCDIC support

  TRawCodePage = object
  private
    FSingleByteMap: TRawMap;
    FWideMap: PLegacyChar;
    FWideMapLo, FWideMapHi: WideChar;
    function IsEBCDIC: Boolean;
    class function MaxCharBytes: Byte;
  public
    constructor Create(const RawMap: TRawMap);
    destructor Destroy;

  {$IFDEF UTF32}
    function FromUTF32(var Info: TLegacyStrInfo; Source: PQuadChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy): Boolean;
    function ToUTF32(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PQuadChar;
      DestOptions: TEncodeUTF32): Boolean;
  {$ENDIF}

    function FromUTF16(var Info: TLegacyStrInfo; Source: PWideChar;
      Count: Cardinal; SourceOptions: TEndianSource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy): Boolean;
    function ToUTF16(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PWideChar;
      DestOptions: TEncodeUTF16): Boolean;

    function FromUTF8(var Info: TLegacyStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeLegacy): Boolean;
    function ToUTF8(var Info: TUnicodeStrInfo; Source: PLegacyChar;
      Count: Cardinal; SourceOptions: TLegacySource; Dest: PLegacyChar;
      DestOptions: TEncodeUTF8): Boolean;

  // properties
    property Map: TRawMap read FSingleByteMap;
    property WideMap: PLegacyChar read FWideMap;
    property WideMapLo: WideChar read FWideMapLo;
    property WideMapHi: WideChar read FWideMapHi;
  end;

  TSingleByteMemCodePage = TRawCodePage;

  TSupportedCodePage = 0..27;
  TCodePages = array[TSupportedCodePage] of Word;
  TCodePageNames = array[TSupportedCodePage] of PLegacyChar;

