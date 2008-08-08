(*
    The Unified Environment, legacy Win32 core

    Core string, code page, character set and MIME encoding implementation

    Copyright (c) 2007-2008 The Unified Environment Laboratory

    TODO: GB18030 support
*)

unit Strings;

interface

uses
  Windows, Core, Storage;

{$I CodePage.inc}

const
  CP_ACP        = Windows.CP_ACP;
  CP_OEMCP      = Windows.CP_OEMCP;
  CP_UTF7       = Windows.CP_UTF7;
  CP_UTF8       = Windows.CP_UTF8;

  CP_UTF16_LE   = CP_UCS2_LE;
  CP_UTF16_BE   = CP_UCS2_BE;

  CP_UTF16      = CP_UTF16_LE;

  CP_UTF32_LE   = CP_UCS4_LE;
  CP_UTF32_BE   = CP_UCS4_BE;

  CP_UTF32      = CP_UTF32_LE;

  BOM_UTF16_BE  = $FFFE;
  BOM_UTF16_LE  = $FEFF;
  BOM_UTF32_BE  = $FFFE0000;
  BOM_UTF32_LE  = $0000FEFF;
  BOM_UTF8      = $BFBBEF;
  BOM_UTF7      = $762F2B;
  BOM_UTF7_LastBytes = [$38, $39, $2B, $2F];

// UTF-16 to UTF-32: $10000 + (H - $D800) * $400 + (L - $DC00)

type
  TCodePageType = (cpMultiByte, cpWideChar, cpQuadChar);
  TCodePageInfo = record
    MaxCharBytes: Integer;
    CodePageType: TCodePageType;
  end;

function CodePageInfo(CodePage: Cardinal): TCodePageInfo;
function DestCodePage(Source1, Source2: Cardinal): Cardinal;
function IsQuadChar(CodePage: Cardinal): Boolean;
function IsWideChar(CodePage: Cardinal): Boolean;
function TranslateCodePage(CodePage: Cardinal): Cardinal;

function StrLenQ(Str: PQuadChar): Integer;
function QuadCharToWideChar(Source: PQuadChar; Count: Integer;
  Dest: PWideChar; StopOnInvalidChar: Boolean): Integer;
function WideCharToQuadChar(Source: PWideChar; Count: Integer;
  Dest: PQuadChar): Integer;

procedure QuadCharSwapByteOrder(Source, Dest: PQuadChar; Count: Integer);
procedure WideCharSwapByteOrder(Source, Dest: PWideChar; Count: Integer);

function QuickFind(S: PChar; Count: Integer; SortedSet: PChar;
  SortedCount: Integer): PChar; overload;
function QuickFind(S: PQuadChar; Count: Integer; SortedSet: PQuadChar;
  SortedCount: Integer): PQuadChar; overload;
function QuickFind(S: PWideChar; Count: Integer; SortedSet: PWideChar;
  SortedCount: Integer): PWideChar; overload;

procedure QuickSort(S: PChar; Count: Integer); overload;
procedure QuickSort(S: PQuadChar; Count: Integer); overload;
procedure QuickSort(S: PWideChar; Count: Integer); overload;

procedure WriteBOM(S: TWriteableStream; CodePage: Cardinal);

{ Missing in Windows.pas }

const
  MB_PRECOMPOSED        = $00000001;  // use precomposed chars
  MB_COMPOSITE          = $00000002;  // use composite chars
  MB_USEGLYPHCHARS      = $00000004;  // use glyph chars, not ctrl chars
  MB_ERR_INVALID_CHARS  = $00000008;  // error for invalid chars

  WC_COMPOSITECHECK     = $00000200;  // convert composite to precomposed
  WC_DISCARDNS          = $00000010;  // discard non-spacing chars
  WC_SEPCHARS           = $00000020;  // generate separate chars
  WC_DEFAULTCHAR        = $00000040;  // replace with default char

{ Error codes }

  E_ACCESS_TO_NULL_STRING       = 0;
  E_INDEX_OUT_OF_BOUNDS         = 1;
  E_CANNOT_FORMAT_BE            = 2;
  E_CANNOT_FORMAT_UTF32         = 3;
  E_CANNOT_CONVERT_TO_INT       = 4;
  E_CANNOT_CONVERT_TO_INT64     = 5;
  E_CANNOT_CONVERT_TO_FLOAT     = 6;
  E_CANNOT_CONVERT_TO_MULTIBYTE = 7;
  E_CANNOT_CONVERT_TO_UTF16     = 8;
  E_CANNOT_CONVERT_TO_CODEPAGE  = 9;
    
type
  PStringData = ^TStringData;
  TStringData = object
    CodePage: Cardinal;
    RefCount: LongInt;
    Length: Integer;
  end;

  PStringConst = ^TStringConst;
  TStringConst = object(TStringData)
    Buffer: Pointer;
  end;

  TStringBuffer = record
    case Integer of
    //  -1: (CoreChars: array[0..0] of CoreChar);
       1: (Chars: array[0..0] of AnsiChar);
       2: (WideChars: array[0..0] of WideChar);
       4: (QuadChars: array[0..0] of QuadChar);
  end;

  PStringVar = ^TStringVar;
  TStringVar = object(TStringData)
    Buffer: TStringBuffer;
  end;

  TAutoDetectCodePage = set of (cpBOM, cpUTF7, cpUTF8);
  TTrimOptions = set of (toLeft, toRight);

//  TSubString = class;
  TString = class;

  TSubString = TString;
  TUniString = TString;

  TString = class(TObject)
  private
    FData: PStringData;
    FIndex, FLength: Integer;
    procedure AssignData(S: Pointer; Count: Integer; CodePage: Cardinal;
      Index: Integer; AttachToBuffer: Boolean); overload;
    function AssignData(S: TString): Boolean; overload;
    procedure CheckCharIndex(Value: Integer; ForUpdate: Boolean);
    procedure CheckQuadCharIndex(Value: Integer; ForUpdate: Boolean);
    procedure CheckWideCharIndex(Value: Integer; ForUpdate: Boolean);
    function GetAsChar(CodePage: Cardinal): PAnsiChar;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: LongInt;
    function GetAsString(CodePage: Cardinal): AnsiString;
    function GetAsWideChar(BigEndian: Boolean): PWideChar;
    function GetAsWideString(BigEndian: Boolean): WideString;
    function GetByteLength: Integer;
    function GetChar(Index: Integer): AnsiChar;
    function GetCharLength: Integer;
    function GetCodePage(Translate: Boolean): Cardinal;
    function GetIsQuadChar: Boolean;
    function GetIsWideChar: Boolean;
    function GetQuadChar(Index: Integer): QuadChar;
    function GetWideChar(Index: Integer): WideChar;
    procedure InsertAt(Index: Integer; Ch: AnsiChar; CodePage: Cardinal;
      Count: Integer); overload;
    procedure InsertAt(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
      Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: PAnsiChar; ByteCount: Integer;
      CodePage: Cardinal; Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Count: Integer); overload;
    procedure InsertAt(Index: Integer; S: TString; Count: Integer); overload;
    function NewData(S: Pointer; Count: Integer; CodePage: Cardinal;
      Index: Integer; AttachToBuffer, KeepData: Boolean): PStringData;
    procedure SetAsChar(CodePage: Cardinal; Value: PAnsiChar);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsInteger(Value: LongInt);
    procedure SetAsString(CodePage: Cardinal; const Value: AnsiString);
    procedure SetAsWideChar(BigEndian: Boolean; Value: PWideChar);
    procedure SetAsWideString(BigEndian: Boolean; const Value: WideString);
    procedure SetByteLength(Value: Integer);
    procedure SetChar(Index: Integer; Value: AnsiChar);
    procedure SetCharLength(Value: Integer);
    procedure SetCodePage(Translate: Boolean; Value: Cardinal);
    procedure SetIsQuadChar(Value: Boolean);
    procedure SetIsWideChar(Value: Boolean);
    procedure SetLength(Value: Integer);
    procedure SetQuadChar(Index: Integer; Value: QuadChar);
    procedure SetWideChar(Index: Integer; Value: WideChar);
  protected
    procedure Error(Code: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(Ch: AnsiChar; CodePage: Cardinal; Count: Integer = 1); overload;
    procedure Append(Ch: QuadChar; BigEndian: Boolean; Count: Integer = 1); overload;
    procedure Append(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
      Count: Integer = 1); overload;
    procedure Append(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Count: Integer = 1); overload;
    procedure Append(S: TString; Count: Integer = 1); overload;
    procedure Assign(S: TString);
    procedure CheckIndex(Index: Integer);
    procedure Clear;
    function CodePageInfo: TCodePageInfo;
    function CompareWith(S: TString; CaseSensitive: Boolean): Integer;
    function Data: Pointer;
    procedure Format(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
      const Args: array of const); overload;
    procedure Format(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      const Args: array of const); overload;
    procedure Format(S: TString; const Args: array of const); overload;
    function GetData(S: PAnsiChar; ByteCount: Integer;
      CodePage: Cardinal): Integer; overload;
    function GetData(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
      var BytesWritten: Integer): Boolean; overload;
    function GetData(S: PWideChar; CharCount: Integer;
      BigEndian: Boolean): Integer; overload;
    function GetData(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      var CharsWritten: Integer): Boolean; overload;
    function HasIndex(Index: Integer): Boolean;
    function IndexOf(Ch: AnsiChar; CodePage: Cardinal; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOf(Ch: QuadChar; BigEndian: Boolean; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOf(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOf(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOf(S: TString; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    procedure IndexOf(Source: TString; Ch: AnsiChar; CodePage: Cardinal;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; Ch: QuadChar; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; S: PAnsiChar; ByteCount: Integer;
      CodePage: Cardinal; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source: TString; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOf(Source, S: TString; Index: Integer = 0;
      Count: Integer = 1); overload;
    function IndexOfChar(S: PAnsiChar; ByteCount: Integer; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    function IndexOfChar(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      Index: Integer = 0; Count: Integer = 1): Integer; overload;
    function IndexOfChar(S: TString; Index: Integer = 0;
      Count: Integer = 1): Integer; overload;
    procedure IndexOfChar(Source: TString; S: PAnsiChar; ByteCount: Integer;
      Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOfChar(Source: TString; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Index: Integer = 0; Count: Integer = 1); overload;
    procedure IndexOfChar(Source, S: TString; Index: Integer = 0;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; Ch: AnsiChar; CodePage: Cardinal;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
      Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: PAnsiChar; ByteCount: Integer;
      CodePage: Cardinal; Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: PWideChar; CharCount: Integer;
      BigEndian: Boolean; Count: Integer = 1); overload;
    procedure Insert(Index: Integer; S: TString; Count: Integer = 1); overload;
    procedure Load(S: TReadableStream; DefaultCodePage: Cardinal;
      AutoDetect: TAutoDetectCodePage);
    procedure LowerCase; overload;
    procedure LowerCase(S: TString); overload;
    function NextChar(Index: Integer): Integer;
    function PrevChar(Index: Integer): Integer;
    function Save(S: TWriteableStream; WriteBOM: Boolean): Boolean;
    procedure SetData(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
      AttachToBuffer: Boolean = False); overload;
    procedure SetData(S: PWideChar; CharCount: Integer; BigEndian: Boolean;
      AttachToBuffer: Boolean = False); overload;
    procedure SubString(S: TString; Index: Integer); overload;
    procedure SubString(S: TString; Index, Length: Integer); overload;
//    procedure Trim(Options: TTrimOptions; What
    function TryCodePage(CodePage: Cardinal; Translate: Boolean): Boolean;
    function TryAsFloat(var Value: Double): Boolean;
    function TryAsInt64(var Value: Int64): Boolean;
    function TryAsInteger(var Value: LongInt): Boolean;
    procedure UpperCase; overload;
    procedure UpperCase(S: TString); overload;
  // properties
    property Chars[Index: Integer]: AnsiChar read GetChar
      write SetChar;
    property AsChar[CodePage: Cardinal]: PAnsiChar read GetAsChar
      write SetAsChar;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsString[CodePage: Cardinal]: AnsiString read GetAsString
      write SetAsString;
    property AsWideChar[BigEndian: Boolean]: PWideChar read GetAsWideChar
      write SetAsWideChar;
    property AsWideString[BigEndian: Boolean]: WideString read GetAsWideString
      write SetAsWideString;
    property ByteLength: Integer read GetByteLength write SetByteLength;
    property CodePage[Translate: Boolean]: Cardinal read GetCodePage
      write SetCodePage;
    property CharLength: Integer read GetCharLength write SetCharLength;
    property IsQuadChar: Boolean read GetIsQuadChar write SetIsQuadChar;
    property IsWideChar: Boolean read GetIsWideChar write SetIsWideChar;
    property Length: Integer read FLength write SetLength;
    property QuadChars[Index: Integer]: QuadChar read GetQuadChar write SetQuadChar;
    property WideChars[Index: Integer]: WideChar read GetWideChar write SetWideChar;
  end;

  TCoreString = class(TUniString)
  protected
    procedure Error(Code: Integer); override;
  public
    procedure SysErrorMessage(ErrorCode: Integer);
  // properties
  {$IFNDEF Unicode}
    property Chars; default;
  {$ELSE}
    property WideChars; default;
  {$ENDIF}
  end;

{ MIME support }

function MIME_CodePage(Charset: PCoreChar): Cardinal;
function MIME_Charset(CodePage: Cardinal; Charset: TString): Boolean; overload;
function MIME_Charset(BigEndian, QuadChar: Boolean;
  Charset: TString): Boolean; overload;

implementation

uses
  Unicode, Exceptions, SysUtils;

{ Utilites }

function CodePageInfo(CodePage: Cardinal): TCodePageInfo;
var
  Info: TCPInfo;
begin
  if IsWideChar(CodePage) then
    with Result do
    begin
      MaxCharBytes := SizeOf(WideChar);
      CodePageType := cpWideChar;
    end
  else if IsQuadChar(CodePage) then
    with Result do
    begin
      MaxCharBytes := SizeOf(QuadChar);
      CodePageType := cpQuadChar;
    end
  else
  begin
    if GetCPInfo(CodePage, Info) then
      Result.MaxCharBytes := Info.MaxCharSize
    else
      Result.MaxCharBytes := 0;
    Result.CodePageType := cpMultiByte;
  end;
end;

function DestCodePage(Source1, Source2: Cardinal): Cardinal;
begin
  case Source1 of
    CP_UTF32_LE, CP_UTF32_BE:
      begin
        Result := Source1;
        Exit;
      end;
    CP_UTF7:
      Result := CP_UTF16;
  else
    Result := Source1;
  end;
  case Source2 of
    CP_UTF32_LE, CP_UTF32_BE:
      Result := Source2;
    CP_UTF7, CP_UTF16_LE:
      if Result <> CP_UTF16_BE then
        Result := CP_UTF16;
    CP_UTF16_BE:
      if Result <> CP_UTF16_BE then
        Result := Source2;
  else
    if TranslateCodePage(Result) <> TranslateCodePage(Source2) then
      Result := CP_UTF16;
  end;
end;

function IsQuadChar(CodePage: Cardinal): Boolean;
begin
  Result := CodePage - CP_UTF32_LE in [0, 1];
end;

function IsWideChar(CodePage: Cardinal): Boolean;
begin
  Result := CodePage - CP_UTF16_LE in [0, 1];
end;

function TranslateCodePage(CodePage: Cardinal): Cardinal;
begin
  case CodePage of
    CP_ACP:
      Result := GetACP;
    CP_OEMCP:
      Result := GetOEMCP;
  else
    Result := CodePage;
  end;
end;

function StrLenQ(Str: PQuadChar): Integer;
asm
         MOV EDX, EDI
         MOV EDI, EAX
         MOV ECX, 0FFFFFFFFH
         XOR EAX, EAX
         REPNE SCASD
         MOV EAX, 0FFFFFFFEH
         SUB EAX, ECX
         MOV EDI, EDX
end;

function QuadCharToWideChar(Source: PQuadChar; Count: Integer;
  Dest: PWideChar; StopOnInvalidChar: Boolean): Integer;
var
  P: PQuadChar;
begin
  Result := 0;
  P := Source;
  while Count <> 0 do
  begin
    case LongWord(P^) of
      $D800..$DBFF, $DC00..$DFFF:
        if StopOnInvalidChar then
        begin
          Result := -(LongWord(P) - LongWord(Source)) div SizeOf(QuadChar);
          Exit;
        end;
      $10000..$10FFFF:
      begin
        if Dest <> nil then
        begin
          // y = ((x ? 0001 0000) / 400) + D800
          // z = ((x ? 0001 0000) % 400) + DC00
          Dest^ := WideChar((LongWord(P^) - $10000) mod $400 + $DC00);
          Inc(Dest);
          Dest^ := WideChar((LongWord(P^) - $10000) div $400 + $D800);
          Inc(Dest);
        end;
        Inc(Result, 2);
      end;
      $110000..$FFFFFFFF: ;
        // no mapping defined
    else
      if Dest <> nil then
      begin
        Dest^ := WideChar(P^);
        Inc(Dest);
      end;
      Inc(Result);
    end;
    Inc(P);
    Dec(Count);
  end;
end;

function WideCharToQuadChar(Source: PWideChar; Count: Integer;
  Dest: PQuadChar): Integer;
var
  HasSurrogate: Boolean;
begin
  Result := 0;
  while Count <> 0 do
  begin
    case Word(Source^) of
      // QuadChar($10000 + (Source[0] - $DC00) + (Source[1] - $D800) * $400;
      $DC00..$DFFF: // low surrogates
        begin
          if Dest <> nil then
          begin
            Dest^ := QuadChar($10000 + Word(Source^) - $DC00);
            Inc(Dest);
          end;
          Inc(Result);
        end;
      $D800..$DBFF: // high surrogates
        begin
          if Result <> 0 then
          begin
            Dec(Source);
            case Word(Source^) of
              $DC00..$DFFF:
                HasSurrogate := True;
            else
              HasSurrogate := False;
            end;
            Inc(Source);
          end
          else
            HasSurrogate := False;
          if Dest <> nil then
          begin
            if HasSurrogate then
            begin
              Dec(Dest);
              Inc(Dest^, (Word(Source^) - $D800) * $400);
              Inc(Dest);
            end
            else
            begin
              Dest^ := $10000 + (Word(Source^) - $D800) * $400;
              Inc(Dest);
              Inc(Result);
            end;
          end
          else if not HasSurrogate then
            Inc(Result);
        end;
    else
      if Dest <> nil then
      begin
        Dest^ := QuadChar(Source^);
        Inc(Dest);
      end;
      Inc(Result);
    end;
    Inc(Source);
    Dec(Count);
  end;
end;

procedure QuadCharSwapByteOrder(Source, Dest: PQuadChar; Count: Integer);
asm
         OR EAX, EAX
         JZ @@2

         PUSH ESI
         PUSH EDI

         MOV ESI, EAX
         MOV EDI, EDX
@@1:
         LODSD
         BSWAP EAX
         STOSD
         DEC ECX
         JNZ @@1

         POP EDI
         POP ESI
@@2:
end;

procedure WideCharSwapByteOrder(Source, Dest: PWideChar; Count: Integer);
asm
         OR EAX, EAX
         JZ @@2

         PUSH ESI
         PUSH EDI

         MOV ESI, EAX
         MOV EDI, EDX
@@1:
         LODSW
         XCHG AL, AH
         STOSW
         DEC ECX
         JNZ @@1

         POP EDI
         POP ESI
@@2:
end;

function QuickFind(S: PAnsiChar; Count: Integer; SortedSet: PAnsiChar;
  SortedCount: Integer): PAnsiChar;
begin
  Result := nil; // TODO
end;

function QuickFind(S: PQuadChar; Count: Integer; SortedSet: PQuadChar;
  SortedCount: Integer): PQuadChar;
begin
  Result := nil; // TODO
end;

function QuickFind(S: PWideChar; Count: Integer; SortedSet: PWideChar;
  SortedCount: Integer): PWideChar;
begin
  Result := nil; // TODO
end;

procedure QuickSort(S: PAnsiChar; Count: Integer);
begin
  // TODO
end;

procedure QuickSort(S: PQuadChar; Count: Integer);
begin
  // TODO
end;

procedure QuickSort(S: PWideChar; Count: Integer);
begin
  // TODO
end;

procedure WriteBOM(S: TWriteableStream; CodePage: Cardinal);
var
  BOM: LongWord;
begin
  case CodePage of
    CP_UTF7:
      begin
        BOM := BOM_UTF7;
        S.WriteBuffer(BOM, 3);
      end;
    CP_UTF8:
      begin
        BOM := BOM_UTF8;
        S.WriteBuffer(BOM, 3);
      end;
    CP_UTF16_LE:
      begin
        BOM := BOM_UTF16_LE;
        S.WriteBuffer(BOM, SizeOf(Word));
      end;
    CP_UTF16_BE:
      begin
        BOM := BOM_UTF16_BE;
        S.WriteBuffer(BOM, SizeOf(Word));
      end;
    CP_UTF32_LE:
      begin
        BOM := BOM_UTF32_LE;
        S.WriteBuffer(BOM, SizeOf(LongWord));
      end;
    CP_UTF32_BE:
      begin
        BOM := BOM_UTF32_BE;
        S.WriteBuffer(BOM, SizeOf(LongWord));
      end;
  end;
end;

{ Helper functions }

function DataBuffer(Data: PStringData): Pointer;
begin
  if Data.RefCount < 0 then
    Result := PStringConst(Data).Buffer
  else
    Result := @PStringVar(Data).Buffer;
end;

{ MIME support }

//const
//  MIME

function MIME_CodePage(Charset: PCoreChar): Cardinal;
begin
  Result := Cardinal(-1);
end;

function MIME_Charset(CodePage: Cardinal; Charset: TString): Boolean;
begin
  Result := False;
end;

function MIME_Charset(BigEndian, QuadChar: Boolean; Charset: TString): Boolean;
const
  CodePage: array[Boolean] of Cardinal = (CP_UTF16, CP_UTF32);
begin
  Result := MIME_Charset(CodePage[QuadChar] + Byte(BigEndian), Charset);
end;

{ TString }

constructor TString.Create;
begin
  FIndex := -1;
end;

destructor TString.Destroy;
begin
  Clear;
  inherited;
end;

procedure TString.Append(Ch: AnsiChar; CodePage: Cardinal; Count: Integer);
begin
  InsertAt(FLength, Ch, CodePage, Count);
end;

procedure TString.Append(Ch: QuadChar; BigEndian: Boolean; Count: Integer);
begin
  InsertAt(FLength, Ch, BigEndian, Count);
end;

procedure TString.Append(S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal; Count: Integer);
begin
  InsertAt(FLength, S, ByteCount, CodePage, Count);
end;

procedure TString.Append(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Count: Integer);
begin
  InsertAt(FLength, S, CharCount, BigEndian, Count);
end;

procedure TString.Append(S: TString; Count: Integer);
begin
  InsertAt(FLength, S, Count);
end;

procedure TString.Assign(S: TString);
begin
  if (S <> nil) and (S.FData <> nil)
    {$IFDEF Multithread} and (S.FData.RefCount <> 0) {$ENDIF} then
  begin
    if AssignData(S) then
    begin
      FIndex := 0;
      FLength := S.FLength;
    end;
  end
  else
    Clear;
end;

procedure TString.AssignData(S: Pointer; Count: Integer; CodePage: Cardinal;
  Index: Integer; AttachToBuffer: Boolean);
var
  Data: PStringData;
begin
  Data := NewData(S, Count, CodePage, Index, AttachToBuffer, False);
  if Data <> nil then
  begin
    if AttachToBuffer then
      FIndex := Index
    else
      FIndex := 0;
    FLength := Data.Length;
    FData := Data;  // thread-safe assignment order
  end
  else
  begin
    FreeMemAndNil(FData);  // thread-safe free order
    FLength := 0;
    FIndex := -1;
  end;
end;

function TString.AssignData(S: TString): Boolean;
begin
  if (S <> Self) and (S.FData.RefCount <> 0) then
  begin
    if S.FData.RefCount > 0 then
    {$IFDEF Multithread}
      InterlockedIncrement(S.FData.RefCount);
    {$ELSE}
      Inc(S.FData.RefCount);
    {$ENDIF}
    FData := S.FData;
    Result := True;
  end
  else
    Result := False;
end;

procedure TString.CheckCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    case FData.CodePage of
      CP_UTF16_LE, CP_UCS2_BE, CP_UTF32_LE, CP_UTF32_BE:
        begin
          CodePage[True] := CP_ACP;
          CheckIndex(Value);
          Exit;
        end;
    end;
    CheckIndex(Value);
    if ForUpdate then
      SetData(nil, FLength, FData.CodePage);
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.CheckIndex(Index: Integer);
begin
  if not HasIndex(Index) then
    Error(E_INDEX_OUT_OF_BOUNDS);
end;

procedure TString.CheckQuadCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    if not Strings.IsQuadChar(FData.CodePage) then
    begin
      IsQuadChar := True;
      CheckIndex(Value);
    end
    else
    begin
      CheckIndex(Value);
      if ForUpdate then
        SetData(nil, FLength, FData.CodePage);
    end;
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.CheckWideCharIndex(Value: Integer; ForUpdate: Boolean);
begin
  if FData <> nil then
  begin
    if not Strings.IsWideChar(FData.CodePage) then
    begin
      IsQuadChar := True;
      CheckIndex(Value);
    end
    else
    begin
      CheckIndex(Value);
      if ForUpdate then
        SetData(nil, FLength, FData.CodePage);
    end;
  end
  else
    Error(E_ACCESS_TO_NULL_STRING);
end;

procedure TString.Clear;
begin
  AssignData(nil, 0, 0, 0, False);
end;

function TString.CodePageInfo: TCodePageInfo;
begin
  Result := Strings.CodePageInfo(CodePage[False]);
end;

function TString.CompareWith(S: TString; CaseSensitive: Boolean): Integer;
var
  CodePage: Cardinal;
begin
  if S <> nil then
    if FData <> nil then
      if S.FData <> nil then
      begin
        CodePage := DestCodePage(FData.CodePage, S.FData.CodePage);
        case CodePage of
          CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
            Result := CompareStringW(LOCALE_USER_DEFAULT, Integer(not CaseSensitive),
              AsWideChar[False], Length, S.AsWideChar[False], S.Length);
        else
          Result := CompareString(LOCALE_USER_DEFAULT, Integer(not CaseSensitive),
            AsChar[CodePage], Length, S.AsChar[CodePage], S.Length);
        end;
      end
      else
        Result := 1
    else
      Result := S.Length
  else
    Result := Length;
end;

function TString.Data: Pointer;
begin
  if FData <> nil then
  begin
    Result := DataBuffer(FData);
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        Result := PWideChar(Result) + FIndex;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := @QuadString(Result)[FIndex] // Delphi 6 workaround!
    else
      Result := PAnsiChar(Result) + FIndex;
    end;
  end
  else
    Result := nil;
end;

procedure TString.Error(Code: Integer);
begin
  RunError(219); // invalid typecast
end;

procedure TString.Format(S: TString; const Args: array of const);
var
  P: PAnsiChar;
begin
  if S <> nil then
  begin
    P := S.Data;
    if P <> nil then
    begin
      Format(P, S.FLength, S.FData.CodePage, Args); // hack :)
      Exit;
    end;
  end;
  Length := 0;
end;

procedure TString.Format(S: PAnsiChar; ByteCount: Integer; CodePage: Cardinal;
  const Args: array of const);
var
  Buf: array[0..2047] of AnsiChar;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Format(Pointer(S), ByteCount div SizeOf(WideChar), CodePage = CP_UTF16_BE, Args);
    CP_UTF32_LE, CP_UTF32_BE:
      Error(E_CANNOT_FORMAT_UTF32);
  else
    SetData(Buf, FormatBuf(S, Args, Buf), CodePage);
  end;
end;

procedure TString.Format(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; const Args: array of const);
var
  Buf: array[0..2047] of WideChar;
begin
  if BigEndian then
    Error(E_CANNOT_FORMAT_BE)
  else
    SetData(Buf, WideFormatBuf(S, Args, Buf), False);
end;

function TString.GetAsChar(CodePage: Cardinal): PAnsiChar;
begin
  AssignData(nil, FLength, CodePage, 0, False);
//  Result := @FData.Chars[FIndex];
end;

function TString.GetAsFloat: Double;
begin
  if not TryAsFloat(Result) then
    Error(E_CANNOT_CONVERT_TO_FLOAT);
end;

function TString.GetAsInt64: Int64;
begin
  if not TryAsInt64(Result) then
    Error(E_CANNOT_CONVERT_TO_INT64);
end;

function TString.GetAsInteger: LongInt;
begin
  if not TryAsInteger(Result) then
    Error(E_CANNOT_CONVERT_TO_INT);
end;

function TString.GetAsString(CodePage: Cardinal): AnsiString;
begin
  System.SetLength(Result, GetData(nil, 0, CodePage));
  GetData(Pointer(Result), System.Length(Result), CodePage);
end;

function TString.GetAsWideChar(BigEndian: Boolean): PWideChar;
begin
  AssignData(nil, FLength, CP_UTF16 + Byte(BigEndian), 0, False);
//  Result := @FData.WideChars[FIndex];
end;

function TString.GetAsWideString(BigEndian: Boolean): WideString;
begin
  System.SetLength(Result, GetData(nil, 0, BigEndian));
  GetData(Pointer(Result), System.Length(Result), BigEndian);
end;

function TString.GetByteLength: Integer;
begin
  Result := FLength shl Integer(CodePageInfo.CodePageType);
end;

function TString.GetChar(Index: Integer): AnsiChar;
begin
  CheckCharIndex(Index, False);
  Result := PAnsiChar(DataBuffer(FData))[FIndex + Index];
end;

function TString.GetCharLength: Integer;
begin
  Result := GetData(nil, 0, CP_UTF32) div SizeOf(QuadChar);
end;

function TString.GetCodePage(Translate: Boolean): Cardinal;
begin
  if FData <> nil then
    if Translate then
      Result := TranslateCodePage(FData.CodePage)
    else
      Result := FData.CodePage
  else
    Result := Cardinal(-1);
end;

function TString.GetData(S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal): Integer;
begin
  if not GetData(S, ByteCount, CodePage, Result) then
  begin
    Error(E_CANNOT_CONVERT_TO_MULTIBYTE);
    Result := 0; // FData is null
  end;
end;

function TString.GetData(S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal; var BytesWritten: Integer): Boolean;
var
  P: PWideChar;
  CharCount: Integer;
  Buffer: PAnsiChar;
begin
  if FData <> nil then
  begin
    if Strings.IsWideChar(FData.CodePage) then
    begin
      Result := GetData(Pointer(S), ByteCount div SizeOf(WideChar),
        CodePage = CP_UTF16_BE, CharCount);
      BytesWritten := CharCount * SizeOf(WideChar);
      Exit;
    end;

    if ByteCount > FLength then
      BytesWritten := FLength
    else
      BytesWritten := ByteCount;

    if TranslateCodePage(FData.CodePage) <> TranslateCodePage(CodePage) then
    begin
      Buffer := PAnsiChar(DataBuffer(FData)) + FIndex;
      CharCount := {$IFDEF Tricks} System. {$ENDIF}
        MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
          Buffer, BytesWritten, nil, 0);
      GetMem(P, CharCount * SizeOf(WideChar));
      try
        CharCount := {$IFDEF Tricks} System. {$ENDIF}
          MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
            Buffer, BytesWritten, P, CharCount);
        BytesWritten := {$IFDEF Tricks} System. {$ENDIF}
          WideCharToMultiByte(CodePage, 0, P, CharCount, S, BytesWritten,
            nil, @Result);
        Result := not Result; // no default chars
      finally
        FreeMem(P);
      end;
    end
    else
    begin
      Move(PAnsiChar(DataBuffer(FData))[FIndex], S[0], BytesWritten);
      Result := True;
    end;
  end
  else
    Result := False;
end;

function TString.GetData(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean): Integer;
begin
  if not GetData(S, CharCount, BigEndian, Result) then
  begin
    Error(E_CANNOT_CONVERT_TO_UTF16);
    Result := 0; // FData is null
  end;
end;

function TString.GetData(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; var CharsWritten: Integer): Boolean;
begin
  if FData <> nil then
  begin
    if Strings.IsWideChar(FData.CodePage) then
    begin
      if CharCount > FLength then
        CharsWritten := FLength
      else
        CharsWritten := CharCount;

      if FData.CodePage = CP_UTF16 + Byte(BigEndian) then
        Move(PWideChar(DataBuffer(FData))[FIndex], S[0], CharsWritten * SizeOf(WideChar))
      else
        WideCharSwapByteOrder(PWideChar(DataBuffer(FData)) + FIndex, S, CharsWritten);

      Result := True;
    end
    else
    begin
      CharsWritten := {$IFDEF Tricks} System. {$ENDIF}
        MultiByteToWideChar(FData.CodePage, MB_ERR_INVALID_CHARS,
          PAnsiChar(DataBuffer(FData)) + FIndex, FLength, S, CharCount);
      if BigEndian then
        WideCharSwapByteOrder(S, S, CharsWritten);
      Result := CharsWritten <> 0;
    end;
  end
  else
    Result := False;
end;

function TString.GetIsQuadChar: Boolean;
begin
  Result := (FData <> nil) and Strings.IsQuadChar(FData.CodePage);
end;

function TString.GetIsWideChar: Boolean;
begin
  Result := (FData <> nil) and Strings.IsWideChar(FData.CodePage);
end;

function TString.GetQuadChar(Index: Integer): QuadChar;
begin
  CheckQuadCharIndex(Index, False);
  // Sic! Delphi 6 could not compile it with PQuadChar
  Result := QuadString(DataBuffer(FData))[FIndex + Index];
end;

function TString.GetWideChar(Index: Integer): WideChar;
begin
  CheckWideCharIndex(Index, False);
  Result := PWideChar(DataBuffer(FData))[FIndex + Index];
end;

function TString.HasIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FLength);
end;

function TString.IndexOf(Ch: AnsiChar; CodePage: Cardinal; Index,
  Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(Ch: QuadChar; BigEndian: Boolean; Index,
  Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOf(S: TString; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

procedure TString.IndexOf(Source: TString; Ch: AnsiChar;
  CodePage: Cardinal; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(Ch, CodePage, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; Ch: QuadChar;
  BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(Ch, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; S: PAnsiChar;
  ByteCount: Integer; CodePage: Cardinal; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, ByteCount, CodePage, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source: TString; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, CharCount, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOf(Source, S: TString; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOf(S, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

function TString.IndexOfChar(S: TString; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOfChar(S: PAnsiChar; ByteCount: Integer;
  Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

function TString.IndexOfChar(S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Index, Count: Integer): Integer;
begin
  Result := -1; // TODO
end;

procedure TString.IndexOfChar(Source: TString; S: PAnsiChar;
  ByteCount: Integer; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, ByteCount, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOfChar(Source: TString; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, CharCount, BigEndian, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.IndexOfChar(Source, S: TString; Index, Count: Integer);
var
  Idx: Integer;
begin
  if Source <> nil then
  begin
    Idx := Source.IndexOfChar(S, Index, Count);
    if Idx >= 0 then
    begin
      SubString(Source, Idx);
      Exit;
    end;
  end;
  Clear;
end;

procedure TString.Insert(Index: Integer; Ch: AnsiChar; CodePage: Cardinal;
  Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, Ch, CodePage, Count);
end;

procedure TString.Insert(Index: Integer; Ch: QuadChar; BigEndian: Boolean;
  Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, Ch, BigEndian, Count);
end;

procedure TString.Insert(Index: Integer; S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, ByteCount, CodePage, Count);
end;

procedure TString.Insert(Index: Integer; S: PWideChar; CharCount: Integer;
  BigEndian: Boolean; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, CharCount, BigEndian, Count);
end;

procedure TString.Insert(Index: Integer; S: TString; Count: Integer);
begin
  if FData <> nil then
    CheckIndex(Index)
  else if Index <> 0 then
    Error(E_INDEX_OUT_OF_BOUNDS);
  InsertAt(Index, S, Count);
end;

procedure TString.InsertAt(Index: Integer; Ch: AnsiChar;
  CodePage: Cardinal; Count: Integer);
var
  CP: Cardinal;
  Q: QuadChar;
begin
  if FData <> nil then
    CP := DestCodePage(FData.CodePage, CodePage)
  else
    CP := CodePage;
  AssignData(nil, FLength + Count * SizeOf(Ch), CP, Index, False);
  if TranslateCodePage(CP) <> TranslateCodePage(CodePage) then
    case CP of
      CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
        if {$IFDEF Tricks} System. {$ENDIF}
          MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS,
            @Ch, SizeOf(Ch), @Q, SizeOf(Ch)) <> 0 then
        begin
           case CP of
             CP_UTF16_BE:
               LongRec(Q).Lo := Swap(LongRec(Q).Lo);
             CP_UTF32_BE:
               asm
                 MOV EDX, Q
                 BSWAP EDX
                 XOR DX, DX
                 MOV Q, EDX
               end;
           end;
           case CP of
             CP_UTF16_LE, CP_UTF16_BE:
               begin
               //  FillChar(FData.WideChars[FIndex + Index], Count, LongRec(Q).Lo);
                 Exit;
               end;
             CP_UTF32_LE, CP_UTF32_BE:
               begin
               //  FillChar(FData.QuadChars[FIndex + Index], Count, Q);
                 Exit;
               end;
           end;
        end
        else
          Error(E_CANNOT_CONVERT_TO_UTF16);
    end;
//  FillChar(FData.Chars[FIndex + Index], Count, Ch);
end;

procedure TString.InsertAt(Index: Integer; Ch: QuadChar;
  BigEndian: Boolean; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: PAnsiChar;
  ByteCount: Integer; CodePage: Cardinal; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: PWideChar;
  CharCount: Integer; BigEndian: Boolean; Count: Integer);
begin

end;

procedure TString.InsertAt(Index: Integer; S: TString;
  Count: Integer);
begin

end;

procedure TString.Load(S: TReadableStream; DefaultCodePage: Cardinal;
  AutoDetect: TAutoDetectCodePage);

{var
  BOM: LongRec;
  ByteCount: Cardinal;
  T: MultibyteString;
  BOMLen: Byte;

procedure LoadUTF(UTF: LongWord; CP: Cardinal);
begin
  if BOM.Lo = LongRec(UTF).Lo then
  begin
    ByteCount := S.Read(BOM.Hi, 1);
    if ByteCount <> 0 then
    begin
      if BOM.Bytes[2] = LongRec(UTF).Bytes[2] then
      begin
        if CodePage <> nil then
          CodePage^ := CP;
        Result := LoadMultibyteText(S, CP);
        Exit;
      end;
      BOMLen := 3;
    end
    else
    begin
      SetString(Result, PAnsiChar(@BOM), SizeOf(BOM.Lo));
      Exit;
    end;
  end
  else
    BOMLen := 2;
end;}

procedure LoadUTF16(BigEndian: Boolean);
begin
  AssignData(nil, (S.Size - S.Position) div SizeOf(WideChar),
    CP_UTF16 + Byte(BigEndian), 0, False);
//  FLength := S.Read(FData.WideChars, FLength);
end;

begin
{  if cpBOM in AutoDetect then
  ByteCount := S.Read(BOM.Lo, SizeOf(BOM.Lo));
  if ByteCount = SizeOf(BOM.Lo) then
    case BOM.Lo of
      BOM_UTF16_LE:
        LoadUTF16(False);
      BOM_BE16:
        LoadUTF16(True);
    else
      if Word(BOM) = 0 then
        // TODO: LoadUTF32(True) // 0 = no BOM, means big-endian by ISO/IEC convensions
      else if BOM.Lo = 0 then
        // TODO: BOM.Hi + LoadUTF16(True) // see the comment above :)
      else
      LoadUTF(BOM_UTF8, CP_UTF8);
      if Result <> '' then
        Exit;
      LoadUTF(BOM_UTF7, CP_UTF7);
      if Result <> '' then
        Exit;
      SetLength(T, S.GetSize - S.GetPosition + BOMLen);
      Move(BOM, T[1], BOMLen);
      SetLength(T, S.Read(T[BOMLen + 1], Length(T) - BOMLen) + BOMLen);
      if AutoDetect then
      begin
        if IsTextUTF8(T) then
        begin
          if CodePage <> nil then
            CodePage^ := CP_UTF8;
          Result := DecodeString(T, CP_UTF8);
          Exit;
        end;
        if IsTextUTF7(T) then
        begin
          if CodePage <> nil then
            CodePage^ := CP_UTF7;
          Result := DecodeString(T, CP_UTF7);
          Exit;
        end;
      end;
      Result := T;
    end
  else 
    AssignData(@BOM, ByteCount, DefaultCodePage, 0, False);}
end;

procedure TString.LowerCase;
begin
  if FData <> nil then
  begin
    CodePage[True] := CP_UTF16;
//    CharLowerBuffW(@FData.Buffer, FLength);
  end;
end;

procedure TString.LowerCase(S: TString);
begin
  if (S <> nil) and (S.FData <> nil) then
  begin
    SetLength(S.Length);
//    FLength := S.GetData(@FData.Buffer, Length, False);
//    CharLowerBuffW(@FData.Buffer, FLength);
  end
  else
    Clear;
end;

function TString.NewData(S: Pointer; Count: Integer; CodePage: Cardinal;
  Index: Integer; AttachToBuffer, KeepData: Boolean): PStringData;
var
  Bytes, CharBytes, NewSize: Integer;
  OldData: PStringData;
begin
  OldData := nil;

  if (FData <> nil) and (FData.RefCount > 0) then // 0 means data being in other transaction
  begin
  {$IFDEF Multithread}
    if InterlockedDecrement(FData.RefCount) <> 0 then
  {$ELSE}
    Dec(FData.RefCount);
    if FData.RefCount <> 0 then
  {$ENDIF}
      OldData := FData;
  end;

  if Count <> 0 then
  begin
    case CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        begin
          CharBytes := SizeOf(WideChar);
          Bytes := Count * SizeOf(WideChar);
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        begin
          CharBytes := SizeOf(QuadChar);
          Bytes := Count * SizeOf(WideChar);
        end;
    else
      CharBytes := SizeOf(AnsiChar);
      Bytes := Count;
    end;

    if AttachToBuffer then
      NewSize := SizeOf(TStringData)
    else                                                                 // #0
      NewSize := SizeOf(TStringData) - SizeOf(Pointer) + Index + Bytes + CharBytes;

    if (OldData <> nil) and (not KeepData or
      (TranslateCodePage(OldData.CodePage) = TranslateCodePage(CodePage))) then
    begin
      Result := FData;
      ReallocMem(Result, NewSize);
    end
    else
    begin
      GetMem(Result, NewSize);

      if AttachToBuffer then
//        Result.Buffer := S
      else
        if S <> nil then
//          Move(S^, PAnsiChar(@Result.Buffer)[Index * CharBytes], Bytes)
        else
//          if not GetData(PAnsiChar(@Result.Buffer) + Index * CharBytes, Bytes,
//            CodePage, NewSize) and KeepData then
          begin
            FreeMemAndNil(Result);
            if OldData = nil then
              Inc(FData.RefCount); // =0 meaning data renewal
            Exit;
          end;
    end;

    if AttachToBuffer then
      Result.RefCount := -1
    else
    begin
      {case CodePage of
        CP_UTF16_LE, CP_UTF16_BE:
          PWideChar(@Result.Buffer)[Count] := WideChar(#0);
        CP_UTF32_LE, CP_UTF32_BE:
          QuadString(@Result.Buffer)[Count] := QuadChar(#0);
      else
        PAnsiChar(@Result.Buffer)[Count] := #0;
      end;}
      Result.RefCount := 1;
    end;

    Result.CodePage := CodePage;
    Result.Length := Count;

  {$IFDEF Multithread}
    FreeMemAndNil(OldData);
  {$ELSE}
    FreeMem(OldData);
  {$ENDIF}
  end
  else
    Result := nil;
end;

function TString.NextChar(Index: Integer): Integer;
var
  P: Pointer;
begin
  if HasIndex(Index + 1) then
    case FData.CodePage of
      CP_UTF16_LE:
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          if (WordRec(P^).Hi in [$DC..$DF]) and
            (WordRec(PWordArray(P)[1]).Hi in [$D8..$DB]) and (Index + 2 < FLength)
          then
            Result := Index + 2
          else
            Result := Index + 1;
        end;
      CP_UTF16_BE:
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          if (WordRec(P^).Lo in [$D8..$DB]) and
            (WordRec(PWordArray(P)[1]).Lo in [$DC..$DF]) and (Index + 2 < FLength)
          then
            Result := Index + 2
          else
            Result := Index + 1;
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := Index + 1;
    else
      P := PAnsiChar(DataBuffer(FData)) + FIndex + Index;
      // TODO: UTF7/8 (not supported by CharNextExA)
      Result := CharNextExA(FData.CodePage, P, 0) - PAnsiChar(P) + Index;
    end
  else
    Result := -1;
end;

function TString.PrevChar(Index: Integer): Integer;
var
  P: Pointer;
  W: PWideChar;
begin
  if HasIndex(Index - 1) then
    case FData.CodePage of
      CP_UTF16_LE: // CharPrevW does not support UTF-16 surrogates
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          W := P;
          if WordRec(W^).Hi in [$D8..$DB] then
            Dec(W);
          if WordRec(W^).Hi in [$DC..$DF] then
          begin
            Dec(W);
            if WordRec(W^).Hi in [$D8..$DB] then
              Dec(W);
          end
          else
            Dec(W);
          Result := Index - (PWideChar(P) - W);
        end;
      CP_UTF16_BE: // Windows has no UTF-16 BE support
        begin
          P := PWideChar(DataBuffer(FData)) + FIndex + Index;
          W := P;
          if WordRec(W^).Lo in [$DC..$DF] then
            Dec(W);
          if WordRec(W^).Lo in [$D8..$DB] then
          begin
            Dec(W);
            if WordRec(W^).Lo in [$DC..$DF] then
              Dec(W);
          end
          else
            Dec(W);
          Result := Index - (PWideChar(P) - W);
        end;
      CP_UTF32_LE, CP_UTF32_BE:
        Result := Index - 1;
    else
      P := PAnsiChar(DataBuffer(FData)) + FIndex;
      // TODO: UTF7/8 (isn't supported by CharPrevExA)
      Result := PAnsiChar(P) + Index - CharPrevExA(FData.CodePage, P, PAnsiChar(P) + Index, 0);
    end
  else
    Result := -1;
end;

function TString.Save(S: TWriteableStream; WriteBOM: Boolean): Boolean;
var
  P: Pointer;
begin
  P := Data;
  if P <> nil then
  begin
    if WriteBOM then
      Strings.WriteBOM(S, FData.CodePage);
    S.WriteBuffer(P^, ByteLength);
    Result := True;
  end
  else
    Result := False;
end;

procedure TString.SetAsChar(CodePage: Cardinal; Value: PAnsiChar);
var
  Count: Integer;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Count := StrLenW(Pointer(Value));
    CP_UTF32_LE, CP_UTF32_BE:
      Count := StrLenQ(Pointer(Value));
  else
    Count := StrLen(Value);
  end;
  AssignData(Value, Count, CodePage, 0, False);
end;

procedure TString.SetAsFloat(const Value: Double);
var
  S: string[24];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsInt64(Value: Int64);
var
  S: string[24];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsInteger(Value: Integer);
var
  S: string[12];
begin
  Str(Value, S);
  SetData(@S, System.Length(S), CP_ACP); // TODO
end;

procedure TString.SetAsString(CodePage: Cardinal; const Value: AnsiString);
begin
  SetData(Pointer(Value), System.Length(Value), CodePage,
    PLongInt(PAnsiChar(Pointer(Value)) - SizeOf(Integer) - SizeOf(LongInt))^  < 0); // Borland magic
end;

procedure TString.SetAsWideChar(BigEndian: Boolean; Value: PWideChar);
begin
  AssignData(Value, StrLenW(Value), CP_UTF16 + Byte(BigEndian), 0, False);
end;

procedure TString.SetAsWideString(BigEndian: Boolean; const Value: WideString);
begin
  AssignData(Pointer(Value), System.Length(Value), CP_UTF16 + Byte(BigEndian), 0, False);
end;

procedure TString.SetByteLength(Value: Integer);
begin
  if FData <> nil then
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE:
        Value := Value div SizeOf(WideChar);
      CP_UTF32_LE, CP_UTF32_BE:
        Value := Value div SizeOf(QuadChar);
    end;
{$IFDEF Unicode}
  Length := Value div SizeOf(WideChar);
{$ELSE}
  Length := Value;
{$ENDIF}
end;

procedure TString.SetChar(Index: Integer; Value: AnsiChar);
begin
  CheckCharIndex(Index, True);
//  FData.Chars[FIndex + Index] := Value;
end;

procedure TString.SetCharLength(Value: Integer);
begin
  if FData <> nil then
    case FData.CodePage of
      CP_UTF16_LE, CP_UTF16_BE, CP_UTF32_LE, CP_UTF32_BE:
        { Value := Value } ; // :)
    else
      Value := Value * CodePageInfo.MaxCharBytes;
    end;
  Length := Value;
end;

procedure TString.SetCodePage(Translate: Boolean; Value: Cardinal);
begin
  if not TryCodePage(Value, Translate) then
    Error(E_CANNOT_CONVERT_TO_CODEPAGE);
end;

procedure TString.SetData(S: PAnsiChar; ByteCount: Integer;
  CodePage: Cardinal; AttachToBuffer: Boolean);
var
  Count: Integer;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      Count := ByteCount div SizeOf(WideChar);
    CP_UTF32_LE, CP_UTF32_BE:
      Count := ByteCount div SizeOf(QuadChar);
  else
    Count := ByteCount;
  end;
  AssignData(S, Count, CodePage, 0, AttachToBuffer);
end;

procedure TString.SetData(S: PWideChar; CharCount: Integer; BigEndian,
  AttachToBuffer: Boolean);
begin
  AssignData(S, CharCount, CP_UTF16 + Byte(BigEndian), 0, AttachToBuffer);
end;

procedure TString.SetIsQuadChar(Value: Boolean);
begin
  if not IsQuadChar then
    CodePage[True] := CP_UTF32;
end;

procedure TString.SetIsWideChar(Value: Boolean);
begin
  if not IsWideChar then
    CodePage[True] := CP_UTF16;
end;

procedure TString.SetLength(Value: Integer);
var
  CodePage: Cardinal;
begin
  if FData <> nil then
    CodePage := FData.CodePage
  else
  {$IFDEF Unicode}
    CodePage := CP_UTF16;
  {$ELSE}
    CodePage := CP_ACP;
  {$ENDIF}
  AssignData(nil, Value, CodePage, 0, False);
end;

procedure TString.SetQuadChar(Index: Integer; Value: QuadChar);
begin
  CheckQuadCharIndex(Index, True);
//  FData.QuadChars[FIndex + Index] := Value;
end;

procedure TString.SetWideChar(Index: Integer; Value: WideChar);
begin
  CheckWideCharIndex(Index, True);
//  FData.WideChars[FIndex + Index] := Value;
end;

procedure TString.SubString(S: TString; Index: Integer);
begin
  SubString(S, Index, S.Length - Index);
end;

procedure TString.SubString(S: TString; Index, Length: Integer);
begin
  if (S <> nil) and (S.FData <> nil)
    {$IFDEF Multithread} and (S.FData.RefCount <> 0) {$ENDIF} then
  begin
    S.CheckIndex(Index);
    S.CheckIndex(Length - 1);
    if AssignData(S) then
    begin
      FIndex := Index;
      FLength := Length;
    end
    else
    begin
      Inc(FIndex, Index);
      FLength := Length;
    end;
  end
  else
    Clear;
end;

function TString.TryAsFloat(var Value: Double): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryAsInt64(var Value: Int64): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryAsInteger(var Value: Integer): Boolean;
var
  Error: Integer;
begin
  Val(AsString[CodePage[False]], Value, Error); // TODO
  Result := Error = 0;
end;

function TString.TryCodePage(CodePage: Cardinal; Translate: Boolean): Boolean;
var
  CP: Integer;
  AttachToBuffer: Boolean;
  Data: PStringData;
begin
  if FData <> nil then
  begin
    if Translate then
    begin
      CP := CodePage;
      AttachToBuffer := False;
    end
    else
    begin
      CP := FData.CodePage;
      AttachToBuffer := FData.RefCount < 0;
    end;

    Data := NewData(nil, FLength, CP, 0, AttachToBuffer, True);
    if Data <> nil then
    begin
      FIndex := 0;
      FLength := FData.Length;
      Data.CodePage := CodePage;
      FData := Data;
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
    GetMem(FData, SizeOf(TStringData) - SizeOf(Pointer));
    FData.RefCount := 1;
    FData.CodePage := CodePage;
    FData.Length := 0;

    FIndex := 0;
    FLength := 0;

    Result := True;
  end;
end;

procedure TString.UpperCase;
begin

end;

procedure TString.UpperCase(S: TString);
begin

end;

{ TCoreString }

procedure TCoreString.Error(Code: Integer);
begin
  // TODO: exception
end;

procedure TCoreString.SysErrorMessage(ErrorCode: Integer);
var
  Msg: array[0..$FF] of CoreChar;
begin
  SetData(Msg, Exceptions.SysErrorMessage(ErrorCode, Msg, SizeOf(Msg)),
    {$IFDEF Unicode} False {$ELSE} CP_ACP {$ENDIF} );
end;

end.

