(*
  Unified Environment by Freeman

  Legacy storage implementation:
    - abstract stream (readable and writable simultaneously, see Ono comment
      for right UE stream implementation)
    - handle (console and file) stream
    - console
    - base text I/O with code page, multibyte and UTF encoding support 

  Copyright (c) 2007 Freeman

  Custom defines (turned off by default):

    CONSOLE_API - Use console API instead of file I/O. This option lacks
                  console I/O redirection due to Win32 console implementation

    CONSOLE_EX  - Adds MaxWidth parameter to one of overloaded TConsole.WriteLn
                  method which allows clip strings longer to MaxWidth and draw
                  elipsis instead of clipped characters. Helpful for single-line
                  override output, for example, processing file names

    NO_STREAM_SEEK - Removes stream seek option (TStream Get/SetPosition,
                  Get/SetSize) to reduce code and import section. Unfortunately,
                  Delphi linker does not remove really unused virtual methods
*)

unit Storage;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
// TODO: POSIX
{$ENDIF}
  Core;

{$IFDEF MSWINDOWS}
const
  STD_INPUT_HANDLE            = Windows.STD_INPUT_HANDLE;
  STD_OUTPUT_HANDLE           = Windows.STD_OUTPUT_HANDLE;
  STD_ERROR_HANDLE            = Windows.STD_ERROR_HANDLE;

  GENERIC_READ                = Windows.GENERIC_READ;
  GENERIC_WRITE               = Windows.GENERIC_WRITE;
  GENERIC_EXECUTE             = Windows.GENERIC_EXECUTE;
  GENERIC_ALL                 = Windows.GENERIC_ALL;

  CREATE_NEW                  = Windows.CREATE_NEW;
  CREATE_ALWAYS               = Windows.CREATE_ALWAYS;
  OPEN_EXISTING               = Windows.OPEN_EXISTING;
  OPEN_ALWAYS                 = Windows.OPEN_ALWAYS;
  TRUNCATE_EXISTING           = Windows.TRUNCATE_EXISTING;

  FILE_SHARE_READ             = Windows.FILE_SHARE_READ;
  FILE_SHARE_WRITE            = Windows.FILE_SHARE_WRITE;
  FILE_SHARE_DELETE           = Windows.FILE_SHARE_DELETE;

  FILE_ATTRIBUTE_READONLY     = Windows.FILE_ATTRIBUTE_READONLY;
  FILE_ATTRIBUTE_HIDDEN       = Windows.FILE_ATTRIBUTE_HIDDEN;
  FILE_ATTRIBUTE_SYSTEM       = Windows.FILE_ATTRIBUTE_SYSTEM;
  FILE_ATTRIBUTE_DIRECTORY    = Windows.FILE_ATTRIBUTE_DIRECTORY;
  FILE_ATTRIBUTE_ARCHIVE      = Windows.FILE_ATTRIBUTE_ARCHIVE;
  FILE_ATTRIBUTE_NORMAL       = Windows.FILE_ATTRIBUTE_NORMAL;
  FILE_ATTRIBUTE_TEMPORARY    = Windows.FILE_ATTRIBUTE_TEMPORARY;
  FILE_ATTRIBUTE_COMPRESSED   = Windows.FILE_ATTRIBUTE_COMPRESSED;
  FILE_ATTRIBUTE_OFFLINE      = Windows.FILE_ATTRIBUTE_OFFLINE;
{$ELSE}
// TODO: POSIX
{$ENDIF}

(* ONO

using System;

public class Stream(Limited, Readable, Writable)
                 // probably existing System.Limited is System.Limited ;)

  public class Unlimited
    public offset(Int64 value);
  end;

  public class Limited(Unlimited)
    public offset(Int64 value)
      position += value;
    end;
    public position(Int64 value);
    public Int64 position;
    public Int64 size;
    public size(Int64 value);
  end;

  public class Readable
    public Object read;
  end;

  public class Writable
    public write(Object value);
  end;

end;

public class FileStream(Stream)
  // tradidional file implementation
end;

final class Stream.MyUnlimited(Stream.Unlimited s)
  public delta = s.offset; // lambda expression
end;

*)

type
  PStream = ^TStream;
  TStream = object(TObject)
  public
    function GetPosition: Int64; virtual; abstract;
    function GetSize: Int64; virtual; abstract;
    function Read(var Buf; Count: Cardinal): Cardinal; virtual; abstract;
    procedure ReadBuffer(var Buf; Count: Cardinal);
  {$IFNDEF NO_STREAM_SEEK}
    procedure SetPosition(Value: Int64); virtual; abstract;
    procedure SetSize(Value: Int64); virtual; abstract;
  {$ENDIF}
    function Write(const Buf; Count: Cardinal): Cardinal; virtual; abstract;
    procedure WriteBuffer(const Buf; Count: Cardinal);
  end;

  PHandleStream = ^THandleStream;
  THandleStream = object(TStream)
  private
    FHandle: THandle;
  public
    constructor Init(const FileName: CoreString; Access, Share, Creation: Cardinal;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL); overload;
    constructor Init(StdHandleType: Cardinal); overload;
    destructor Done; virtual;
    function GetPosition: Int64; virtual;
    function GetSize: Int64; virtual;
    function Open(const FileName: CoreString; Access, Share, Creation: Cardinal;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL): Boolean;
    function Read(var Buf; Count: Cardinal): Cardinal; virtual;
  {$IFNDEF NO_STREAM_SEEK}
    procedure SetPosition(Value: Int64); virtual;
    procedure SetSize(Value: Int64); virtual;
  {$ENDIF}
    function Write(const Buf; Count: Cardinal): Cardinal; virtual;
  // properties
    property Handle: THandle read FHandle;
  end;

  PFileStream = PHandleStream;
  TFileStream = THandleStream;

  PStdStream = PHandleStream;
  TStdStream = THandleStream;

  PConsoleStream = PHandleStream;
  TConsoleStream = THandleStream;

  PConsole = ^TConsole;
  TConsole = object(TObject)
  private
    FInput, FOutput: THandle;
  {$IFNDEF UNICODE}
    function GetCodePage: Cardinal;
    procedure SetCodePage(Value: Cardinal);
  {$ENDIF}
  public
    constructor Init; {$IFNDEF UNICODE} overload;
    constructor Init(CP: Cardinal); overload; {$ENDIF}
    procedure ReadLn(const Prompt: CoreString; LineBreaks: Integer = 1);
    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(const Text: CoreString; LineBreaks: Integer = 1
      {$IFDEF CONSOLE_EX}; MaxWidth: Integer = High(Integer){$ENDIF}); overload;
  // properties
  {$IFNDEF UNICODE}
    property CodePage: Cardinal read GetCodePage write SetCodePage;
  {$ENDIF}
    property Input: THandle read FInput;
    property Output: THandle read FOutput;
  end;

{ Text I/O }

const
  BOM_BE16    = $FFFE;
  BOM_LE16    = $FEFF;
  BOM_UTF8    = $BFBBEF;
  BOM_UTF7    = $762F2B;

procedure WriteBOM(S: PStream; CodePage: Cardinal = DefaultCP); overload;
procedure WriteBOM(S: PStream; BigEndian: Boolean = DefaultBE); overload;

function LoadText(S: PStream; CodePage: PCardinal = nil;
  AutoDetect: Boolean = True): CoreString; overload;
function LoadMultibyteText(S: PStream; CodePage: Cardinal = DefaultCP): CoreString; overload;
function LoadUnicodeText(S: PStream; BigEndian: Boolean = DefaultBE): CoreString; overload;

procedure SaveText(S: PStream; const Text: CoreString;
  UTF8: Boolean = DefaultUTF8; WriteBOM: Boolean = True); overload;
procedure SaveMultibyteText(S: PStream; const Text: CoreString;
  CodePage: Cardinal = DefaultCP; WriteBOM: Boolean = True); overload;
procedure SaveUnicodeText(S: PStream; const Text: CoreString;
  BigEndian: Boolean = DefaultBE; WriteBOM: Boolean = True); overload;

implementation

uses
  SysUtils;

{ Text I/O }

procedure WriteBOM(S: PStream; CodePage: Cardinal);
var
  BOM: LongWord;
begin
  case CodePage of
    CP_UTF7:
      BOM := BOM_UTF7;
    CP_UTF8:
      BOM := BOM_UTF8;
    CP_UTF16_LE, CP_UTF16_BE:
    begin
      WriteBOM(S, CodePage = CP_UTF16_BE);
    end;
  else
    Exit;
  end;
  S.WriteBuffer(BOM, 3);
end;

procedure WriteBOM(S: PStream; BigEndian: Boolean);
const
  BOM: array[Boolean] of Word = (BOM_LE16, BOM_BE16);
begin
  S.WriteBuffer(BOM[BigEndian], SizeOf(Word));
end;

function LoadText(S: PStream; CodePage: PCardinal; AutoDetect: Boolean): CoreString;

var
  BOM: LongRec;
  ByteCount: Cardinal;
  T: MultiAnsiString;
  BOMLen: Byte;

{procedure LoadUTF(UTF: LongWord; CP: Cardinal);
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
      SetString(Result, PChar(@BOM), SizeOf(BOM.Lo));
      Exit;
    end;
  end
  else
    BOMLen := 2;
end;}

begin
  ByteCount := S.Read(BOM.Lo, SizeOf(BOM.Lo));
  if ByteCount <> 0 then
    if ByteCount = SizeOf(BOM.Lo) then
      case BOM.Lo of
        BOM_LE16:
          begin
            if CodePage <> nil then
              CodePage^ := CP_UTF16_LE;
            Result := LoadUnicodeText(S, False);
            Exit;
          end;
        BOM_BE16:
          begin
            if CodePage <> nil then
              CodePage^ := CP_UTF16_BE;
            Result := LoadUnicodeText(S, True);
            Exit;
          end;
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
    else // S.Size = 1
      Result := MultiAnsiChar(BOM.Bytes[0])
  else // S.Size = 0
    Result := '';

  if CodePage <> nil then
    CodePage^ := DefaultCP;
end;

function LoadMultibyteText(S: PStream; CodePage: Cardinal): CoreString; overload;
var
  T: MultiAnsiString;
begin
  SetLength(T, S.GetSize);
  SetLength(T, S.Read(T[1], Length(T)));
  Result := DecodeString(T, CodePage);
end;

function LoadUnicodeText(S: PStream; BigEndian: Boolean): CoreString;
var
  L: Cardinal;
  W: UnicodeString;
begin
  L := S.GetSize;
  SetLength(W, L div SizeOf(UnicodeChar));
  SetLength(W, S.Read(W[1], L) div SizeOf(UnicodeChar));
  Result := DecodeString(W, BigEndian);
end;

procedure SaveText(S: PStream; const Text: CoreString; UTF8, WriteBOM: Boolean);
begin
  if UTF8 then
    SaveMultibyteText(S, Text, CP_UTF8, WriteBOM)
  else
{$IFDEF UNICODE}
    SaveUnicodeText(S, Text, DefaultBE, WriteBOM);
{$ELSE}
    SaveMultibyteText(S, Text, DefaultCP, WriteBOM);
{$ENDIF}
end;

procedure SaveMultibyteText(S: PStream; const Text: CoreString;
  CodePage: Cardinal; WriteBOM: Boolean); overload;
var
  T: MultiAnsiString;
begin
  case CodePage of
    CP_UTF16_LE, CP_UTF16_BE:
      SaveUnicodeText(S, Text, CodePage = CP_UTF16_BE, WriteBOM);
  else
    if Text <> '' then
    begin
      if WriteBOM then
        Storage.WriteBOM(S, CodePage);
      T := EncodeString(Text, CodePage);
      S.WriteBuffer(T[1], Length(T));
    end;
  end;
end;

procedure SaveUnicodeText(S: PStream; const Text: CoreString; BigEndian,
  WriteBOM: Boolean);
var
  W: UnicodeString;
begin
  if Text <> '' then
  begin
    if WriteBOM then
      Storage.WriteBOM(S, BigEndian);
    W := EncodeString(Text, BigEndian);
    S.WriteBuffer(W[1], Length(W) * SizeOf(UnicodeChar));
  end;
end;

{ TStream }

procedure TStream.ReadBuffer(var Buf; Count: Cardinal);
var
  Bytes: Cardinal;
begin
  Bytes := Read(Buf, Count);
  if Bytes <> Count then
    CoreError(EIOError);
end;

procedure TStream.WriteBuffer(const Buf; Count: Cardinal);
var
  Bytes: Cardinal;
begin
  Bytes := Write(Buf, Count);
  if Bytes <> Count then
    CoreError(EIOError);
end;

{ THandleStream }

constructor THandleStream.Init(const FileName: CoreString; Access, Share,
  Creation, Attributes: Cardinal);
begin
  if not Open(FileName, Access, Share, Creation, Attributes) then
    CoreError(EIOError);
end;

constructor THandleStream.Init(StdHandleType: Cardinal);
begin
  FHandle := GetStdHandle(StdHandleType);
  if FHandle = INVALID_HANDLE_VALUE then
    CoreError(EIOError);
end;

destructor THandleStream.Done;
begin
  CloseHandle(FHandle);
  inherited;
end;

function THandleStream.GetPosition: Int64;
begin
  with Int64Rec(Result) do
  begin
    Hi := 0;
    Lo := SetFilePointer(FHandle, 0, @Hi, FILE_CURRENT);
  end;
end;

function THandleStream.GetSize: Int64;
begin
  with Int64Rec(Result) do
    Lo := GetFileSize(FHandle, @Hi);
end;

function THandleStream.Open(const FileName: CoreString; Access, Share,
  Creation, Attributes: Cardinal): Boolean;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := {$IFDEF UNICODE} CreateFileW {$ELSE} CreateFileA {$ENDIF}
    (Pointer(FileName), Access, Share, nil, Creation, Attributes, 0);
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function THandleStream.Read(var Buf; Count: Cardinal): Cardinal;
begin
  ReadFile(FHandle, Buf, Count, Result, nil);
end;

{$IFNDEF NO_STREAM_SEEK}
procedure THandleStream.SetPosition(Value: Int64);
begin
  with Int64Rec(Value) do
    Lo := SetFilePointer(FHandle, 0, @Hi, FILE_BEGIN);
end;

procedure THandleStream.SetSize(Value: Int64);
var
  Pos: Int64;
begin
  Pos := GetPosition;
  try
    SetPosition(Value);
    SetEndOfFile(FHandle);
  finally
    SetPosition(Pos);
  end;
end;
{$ENDIF}

function THandleStream.Write(const Buf; Count: Cardinal): Cardinal;
begin
  WriteFile(FHandle, Buf, Count, Result, nil);
end;

{ TConsole }

constructor TConsole.Init;
{$IFNDEF UNICODE}
begin
  Init(GetACP);
end;

constructor TConsole.Init(CP: Cardinal);
{$ENDIF}
begin
  FInput := GetStdHandle(STD_INPUT_HANDLE);
  FOutput := GetStdHandle(STD_OUTPUT_HANDLE);
{$IFNDEF UNICODE}
  CodePage := CP;
{$ENDIF}
end;

{$IFNDEF UNICODE}
function TConsole.GetCodePage: Cardinal;
begin
  Result := GetConsoleCP;
end;
{$ENDIF}

const
  sElipsis = '...';

procedure TConsole.ReadLn(const Prompt: CoreString; LineBreaks: Integer);
var
  Dummy: array[0..$FF] of CoreChar;
  Read: Cardinal;
begin
  WriteLn(Prompt, 0);
  WriteLn(sElipsis, 0);
{$IFDEF CONSOLE_API}
  {$IFDEF UNICODE} ReadConsoleW {$ELSE} ReadConsoleA {$ENDIF}
    (FInput, @Dummy, SizeOf(Dummy), Read, nil);
{$ELSE}
  ReadFile(FInput, Dummy, SizeOf(Dummy), Read, nil);
{$ENDIF}
  WriteLn(LineBreaks - 1);
end;

{$IFNDEF UNICODE}
procedure TConsole.SetCodePage(Value: Cardinal);
begin
  SetConsoleCP(Value);
  SetConsoleOutputCP(Value);
end;
{$ENDIF}

procedure TConsole.WriteLn(LineBreaks: Integer);
var
{$IFDEF CONSOLE_API}
  LineBreak: PCoreChar;
{$ELSE}
  LineBreak: PMultiAnsiChar;
{$ENDIF}
  I: Integer;
  Written: Cardinal;
begin
  LineBreak := sLineBreak;
  for I := 0 to LineBreaks - 1 do
  {$IFDEF CONSOLE_API}
    {$IFDEF UNICODE} WriteConsoleW {$ELSE} WriteConsoleA {$ENDIF}
      (FOutput, LineBreak, Length(sLineBreak), Written, nil);
  {$ELSE}
    WriteFile(FOutput, LineBreak^, Length(sLineBreak), Written, nil);
  {$ENDIF}
end;

procedure TConsole.WriteLn(const Text: CoreString; LineBreaks: Integer
  {$IFDEF CONSOLE_EX}; MaxWidth: Integer {$ENDIF} );
var
  L: Integer;

{$IFDEF CONSOLE_API}
  P: PCoreChar;
{$ELSE}
  P: PMultiAnsiChar;
{$ENDIF}

{$IFDEF CONSOLE_EX}
  {$IFDEF CONSOLE_API}
    Elipsis: PCoreChar;
  {$ELSE}
    Elipsis: PMultiAnsiChar;
  {$ENDIF}
{$ENDIF}

  Written: Cardinal;
begin
{$IF Defined(UNICODE) and not Defined(CONSOLE_API)}
  P := Pointer(EncodeString(Text, GetConsoleCP));
{$ELSE}
  P := Pointer(Text);
{$IFEND}

  L := Length(Text);

{$IFDEF CONSOLE_EX}
  if L >= MaxWidth then
  begin
    Inc(P, L - MaxWidth + Length(sElipsis));
    L := MaxWidth - Length(sElipsis);
    Elipsis := sElipsis;
  {$IFDEF CONSOLE_API}
    {$IFDEF UNICODE} WriteConsoleW {$ELSE} WriteConsoleA {$ENDIF}
      (FOutput, Elipsis, Length(sElipsis), Written, nil);
  {$ELSE}
    WriteFile(FOutput, Elipsis^, Length(sElipsis), Written, nil);
  {$ENDIF}
  end;
{$ENDIF}

{$IFDEF CONSOLE_API}
  {$IFDEF UNICODE} WriteConsoleW {$ELSE} WriteConsoleA {$ENDIF}
    (FOutput, P, L, Written, nil);
{$ELSE}
  WriteFile(FOutput, P^, L, Written, nil);
{$ENDIF}

  WriteLn(LineBreaks);
end;

end.

