(*
    The Unified Environment Core Library

    OOP-style platform-dependent wrappers

    Copyright (c) 2007-2012 The Unified Environment Laboratory

    Conditional defines:
      * Lite -- allow lite TStream implementation -- without virtual methods
                but only THandleStream descendant
*)

unit CoreWrappers;

interface

uses
  Windows, CoreUtils;

{$IFDEF Lite}
  {$I LiteStreams.inc}                       
{$ELSE}
type
  TReadableStream = class
  protected
    function GetPosition: QuadWord; virtual; abstract;
    function GetSize: QuadWord; virtual; abstract;
    procedure SetPosition(Value: QuadWord); virtual; abstract;
  public
    function Read(var Data; Count: LongWord): LongWord; virtual; abstract;
    procedure ReadBuffer(var Data; Count: LongWord);
  // properties
    property Position: QuadWord read GetPosition write SetPosition;
    property Size: QuadWord read GetSize;
  end;

  TWritableStream = class(TReadableStream)
  protected
    procedure SetSize(Value: QuadWord); virtual; abstract;
  public
    function Write(const Buf; Count: LongWord): LongWord; virtual; abstract;
    procedure WriteBuffer(const Data; Count: LongWord);
  // properties
    property Size write SetSize;
  end;

  TFileAccess = set of (faShareRead, faShareWrite, faShareDelete, // ordered
    faWrite, faKeep, faOverwrite, faDeleteOnClose, faSequential, faRandom,
    faNoBuffering, faOverlapped, faWriteThrough);
  TFileAttributes = set of (faReadOnly, faHidden, faSystem, faVolumeLabel,
    faDirectory, faArchive, faNormal, faTemporary, faSparsed, faReparsePoint,
    faCompressed, faOffline, faNonIndexable, faEncrypted, fa0x8000, faVirtual);

const
  faRead = [faShareRead];
  faRewrite = [faWrite, faOverwrite, faShareRead];

type
  THandleStream = class(TWritableStream)
  private
    FHandle: THandle;
  protected
    function GetPosition: QuadWord; override;
    function GetSize: QuadWord; override;
    procedure SetPosition(Value: QuadWord); override;
    procedure SetSize(Value: QuadWord); override;
  public
    constructor Create(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]); overload;
    destructor Destroy; override;

    function Open(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]): Boolean; overload;

    function Seek(Offset: QuadWord; Origin: LongWord): QuadWord;
    function Read(var Data; Count: LongWord): Cardinal; override;
    function Write(const Data; Count: LongWord): LongWord; override;

    function Lock(Offset, Count: QuadWord): Boolean;
    function Unlock(Offset, Count: QuadWord): Boolean;
  // properties
    property Handle: THandle read FHandle;
  end;
{$ENDIF}

  TStream = TWritableStream;

  TFileStream = THandleStream;
  TStdStream = THandleStream;

  TMappingOption = (maRead, maWrite, maCopy, maExecute, maImage, maReserve, maNoCache);
  TCreateFileMapping = set of maWrite..maNoCache;
  TOpenFileMapping = set of maRead..maCopy;
  TMapViewAccess = maRead..maCopy{maExecute};

  TFileMapping = class
  private
    FHandle: THandle;
    function AssignHandle(Value: THandle): Boolean;
  public
    constructor Create(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PWideChar = nil); overload;
    constructor Create(MappingName: PWideChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True); overload;

    destructor Destroy; override;

    function Open(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PWideChar = nil): Boolean; overload;
    function Open(MappingName: PWideChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True): Boolean; overload;

    function MapView(Access: TMapViewAccess; Offset: QuadWord = 0;
      Count: Cardinal = 0): Pointer;
    procedure UnmapView(P: Pointer);
  // properties
    property Handle: THandle read FHandle;
  end;

  TFileStreamMapping = class(TFileMapping)
  private
    FStream: TFileStream;
  public
    constructor Create(FileName: PWideChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PWideChar = nil); overload;
    destructor Destroy; override;
    function Open(FileName: PWideChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PWideChar = nil): Boolean; overload;
  // properties
    property Stream: TFileStream read FStream;
  end;
                                             
  TConsole = class(TObject)
  private
    FInput, FOutput: THandle;
  public
    constructor Create(ErrorOutput: Boolean = False);
  // properties
    property Input: THandle read FInput;
    property Output: THandle read FOutput;
  end;

  TStreamConsole = class(TConsole)
  private
    function GetCodePage: Word;
    procedure SetCodePage(Value: Word);
  public
    procedure ReadLn(Prompt: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PLegacyChar; Count: LongWord; LineBreaks: Integer); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count: LongWord; LineBreaks: Integer); overload;
  // properties
    property CodePage: Word read GetCodePage write SetCodePage;
  end;

  TScreenConsole = class(TConsole)
  private
    procedure SetTextAttribute(Value: Word);
  public
    procedure ReadLn(Prompt: PWideChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PWideChar; Count: LongWord; LineBreaks: Integer); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count: LongWord; LineBreaks: Integer); overload;
  // properties
    property TextAttribute: Word write SetTextAttribute;
  end;

  TPerformanceCounter = class
  private
    FFrequency: QuadWord;
    function GetValue: QuadWord;
  public
    constructor Create;
    function ElapsedMilliseconds(StartValue: QuadWord): Double;
    function MillisecondsBetween(Value1, Value2: QuadWord): Double;
  // properties
    property Frequency: QuadWord read FFrequency;
    property Value: QuadWord read GetValue;
  end;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadWord;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;

implementation

uses
  Exceptions;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
  external kernel32 name 'GetFileSizeEx';
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadWord;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;
  external kernel32 name 'SetFilePointerEx';

{ TReadableStream }

procedure TReadableStream.ReadBuffer(var Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Read(Data, Count);
  if Bytes <> Count then
    RaiseLastPlatformError; // TODO: exception
end;

{ TWritableStream }

procedure TWritableStream.WriteBuffer(const Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Write(Data, Count);
  if Bytes <> Count then
    RaiseLastPlatformError; // TODO: exception
end;

{ THandleStream }

constructor THandleStream.Create(FileName: PWideChar; Access: TFileAccess;
  Attributes: TFileAttributes);
begin
  if not Open(FileName, Access, Attributes) then
    RaiseLastPlatformError;
end;

destructor THandleStream.Destroy;
begin
  CloseHandle(FHandle);
end;

function THandleStream.GetPosition: QuadWord;
begin
  // GetFileSizeEx available since Windows 2000
  if not SetFilePointerEx(FHandle, 0, @Result, FILE_CURRENT) then
    Result := -1;
end;

function THandleStream.GetSize: QuadWord;
begin
  if not GetFileSizeEx(FHandle, Result) then
    Result := -1;
end;

function THandleStream.Lock(Offset, Count: QuadWord): Boolean;
begin
  Result := LockFile(Handle, QuadRec(Offset).Lo, QuadRec(Offset).Hi,
    QuadRec(Count).Lo, QuadRec(Count).Hi);
end;

function THandleStream.Open(FileName: PWideChar; Access: TFileAccess;
  Attributes: TFileAttributes): Boolean;
const
  SharingMask = FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
var
  ReadWrite, Creation, Sharing: LongWord;
  NewHandle: THandle;
begin
  if faWrite in Access then
  begin
    ReadWrite := GENERIC_WRITE;
    if faOverwrite in Access then
      Creation := CREATE_ALWAYS
    else if faKeep in Access then
      Creation := CREATE_NEW
    else
      Creation := OPEN_ALWAYS;
  end
  else
  begin
    ReadWrite := GENERIC_READ;
    Creation := OPEN_EXISTING;
  end;
  Sharing := Word(Access) and SharingMask;
  Access := Access * [faDeleteOnClose..faWriteThrough];
  NewHandle := CreateFileW(FileName, ReadWrite, Sharing, nil, Creation,
    Word(Attributes) or (PWord(@Access)^ shl 20), 0);
  if NewHandle <> INVALID_HANDLE_VALUE then
  begin
    if FHandle <> 0 then
      CloseHandle(FHandle);
    FHandle := NewHandle;
    Result := True;
  end
  else
    Result := False;
end;

function THandleStream.Read(var Data; Count: LongWord): Cardinal;
begin
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FHandle, Data, Count, Result, nil);
end;

function THandleStream.Seek(Offset: QuadWord; Origin: LongWord): QuadWord;
begin
  // SetFilePointerEx available since Windows 2000
  if not SetFilePointerEx(FHandle, Offset, @Result, Origin) then
    Result := -1;
end;

procedure THandleStream.SetPosition(Value: QuadWord);
begin
  SetFilePointerEx(FHandle, Value, nil, FILE_BEGIN);
end;

procedure THandleStream.SetSize(Value: QuadWord);
var
  Pos: QuadWord;
begin
  Pos := GetPosition;
  try
    SetPosition(Value);
    SetEndOfFile(FHandle);
  finally
    SetPosition(Pos);
  end;
end;

function THandleStream.Unlock(Offset, Count: QuadWord): Boolean;
begin
  Result := UnlockFile(Handle, QuadRec(Offset).Lo, QuadRec(Offset).Hi,
    QuadRec(Count).Lo, QuadRec(Count).Hi);
end;

function THandleStream.Write(const Data; Count: LongWord): Cardinal;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FHandle, Data, Count, Result, nil);
end;

{ TFileMapping }

constructor TFileMapping.Create(hFile: THandle; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PWideChar);
begin
  if not Open(hFile, Options, Size, MappingName) then
    RaiseLastPlatformError;
end;

constructor TFileMapping.Create(MappingName: PWideChar; Options: TOpenFileMapping;
  InheritHandle: Boolean);
begin
  if not Open(MappingName, Options, InheritHandle) then
    RaiseLastPlatformError;
end;

destructor TFileMapping.Destroy;
begin
  CloseHandle(FHandle);
end;

function TFileMapping.AssignHandle(Value: THandle): Boolean;
begin
  if Value <> 0 then
  begin
    if FHandle <> 0 then
      CloseHandle(FHandle);
    FHandle := Value;
    Result := True;
  end
  else
    Result := False;
end;

function TFileMapping.MapView(Access: TMapViewAccess; Offset: QuadWord;
  Count: Cardinal): Pointer;
const
  // maRead, maWrite, maCopy{, maExecute}
  MapAccess: array[TMapViewAccess] of LongWord =
    (FILE_MAP_READ, FILE_MAP_WRITE, FILE_MAP_COPY{, FILE_MAP_EXECUTE});
begin
  Result := MapViewOfFile(FHandle, MapAccess[Access],
    QuadRec(Offset).Hi, QuadRec(Offset).Lo, Count);
end;

function TFileMapping.Open(hFile: THandle; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PWideChar): Boolean;
var
  // maWrite, maCopy, maExecute, maImage, maReserve, maNoCache
  Access: LongWord;
begin
  if maExecute in Options then
    if maWrite in Options then
      Access := PAGE_EXECUTE_READWRITE
    else
      Access := PAGE_EXECUTE_READ
  else
    if maCopy in Options then
      Access := PAGE_WRITECOPY
    else if maWrite in Options then
      Access := PAGE_READWRITE
    else
      Access := PAGE_READONLY;

  if maImage in Options then
    Access := Access or SEC_IMAGE;
  if maReserve in Options then
    Access := Access or SEC_RESERVE;
  if maNoCache in Options then
    Access := Access or SEC_NOCACHE;

  Result := AssignHandle(CreateFileMappingW(hFile, nil, Access,
    QuadRec(Size).Hi, QuadRec(Size).Lo, MappingName));
end;

function TFileMapping.Open(MappingName: PWideChar; Options: TOpenFileMapping;
  InheritHandle: Boolean): Boolean;
var
  Access: LongWord;
begin
  Access := 0;
  if maRead in Options then
    Access := Access or FILE_MAP_READ;
  if maWrite in Options then
    Access := Access or FILE_MAP_WRITE;
  if maCopy in Options then
    Access := Access or FILE_MAP_COPY;
  Result := AssignHandle(OpenFileMappingW(Access, InheritHandle, MappingName));
end;

procedure TFileMapping.UnmapView(P: Pointer);
begin
  UnmapViewOfFile(P);
end;

{ TFileStreamMapping }

constructor TFileStreamMapping.Create(FileName: PWideChar; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PWideChar);
begin
  FStream := TFileStream.Create;
  if not Open(FileName, Options, Size, MappingName) then
    RaiseLastPlatformError;
end;

destructor TFileStreamMapping.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TFileStreamMapping.Open(FileName: PWideChar; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PWideChar): Boolean;
const
  MapOptions: array[Boolean] of TFileAccess = (faRead, faRewrite);
begin
  Result := FStream.Open(FileName, MapOptions[maWrite in Options]) and
    Open(FStream.Handle, Options, Size, MappingName);
end;

{ TConsole }

constructor TConsole.Create(ErrorOutput: Boolean);
const
  Outputs: array[Boolean] of LongWord = (STD_OUTPUT_HANDLE, STD_ERROR_HANDLE);
begin
  FInput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(STD_INPUT_HANDLE);
  FOutput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(Outputs[ErrorOutput]);
end;

{ TStreamConsole }

function TStreamConsole.GetCodePage: Word;
begin
  Result := GetConsoleOutputCP;
end;

procedure TStreamConsole.ReadLn(Prompt: PLegacyChar; LineBreaks: Integer);
begin
  ReadLn(Prompt, StrLen(Prompt), LineBreaks);
end;

procedure TStreamConsole.ReadLn(Prompt: PLegacyChar; Count: LongWord; LineBreaks: Integer);
var
  Dummy: array[0..$FF] of LegacyChar; // preventing flood
  BytesRead: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(@Elipsis, SizeOf(Elipsis), 0);
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FInput, Dummy, SizeOf(Dummy), BytesRead, nil);
  WriteLn(LineBreaks - 1);
end;

procedure TStreamConsole.SetCodePage(Value: Word);
begin
  SetConsoleCP(Value);
  SetConsoleOutputCP(Value);
end;

procedure TStreamConsole.WriteLn(LineBreaks: Integer);
var
  I: Integer;
  BytesWritten: LongWord;
begin
  for I := 0 to LineBreaks - 1 do
  {$IFDEF Tricks} System. {$ENDIF}
    WriteFile(FOutput, LF, SizeOf(LF), BytesWritten, nil);
end;

procedure TStreamConsole.WriteLn(Text: PLegacyChar; LineBreaks: Integer);
begin
  WriteLn(Text, StrLen(Text), LineBreaks);
end;

procedure TStreamConsole.WriteLn(Text: PLegacyChar; Count: LongWord; LineBreaks: Integer);
var
  BytesWritten: LongWord;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FOutput, Text^, Count, BytesWritten, nil);
  WriteLn(LineBreaks);
end;

{ TScreenConsole }

procedure TScreenConsole.ReadLn(Prompt: PWideChar; LineBreaks: Integer);
begin
  ReadLn(Prompt, WideStrLen(Prompt), LineBreaks);
end;

procedure TScreenConsole.ReadLn(Prompt: PWideChar; Count: LongWord; LineBreaks: Integer);
var
  Dummy: array[0..$FF] of WideChar; // preventing flood
  Read: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(@WideElipsis, 1, 0);
  ReadConsoleW(FInput, @Dummy, Length(Dummy), Read, nil);
  WriteLn(LineBreaks - 1);
end;

procedure TScreenConsole.SetTextAttribute(Value: Word);
begin
  SetConsoleTextAttribute(FOutput, Value);
end;

procedure TScreenConsole.WriteLn(LineBreaks: Integer);
var
  I: Integer;
  Written: LongWord;
begin
  for I := 0 to LineBreaks - 1 do
    WriteConsoleW(FOutput, @WideLF, 1, Written, nil);
end;

procedure TScreenConsole.WriteLn(Text: PWideChar; LineBreaks: Integer);
begin
  WriteLn(Text, WideStrLen(Text), LineBreaks);
end;

procedure TScreenConsole.WriteLn(Text: PWideChar; Count: LongWord; LineBreaks: Integer);
var
  Written: LongWord;
begin
  WriteConsoleW(FOutput, Text, Count, Written, nil);
  WriteLn(LineBreaks);
end;

{ TPerformanceCounter }

constructor TPerformanceCounter.Create;
begin
  if not QueryPerformanceFrequency(FFrequency) then
    RaiseLastPlatformError;
end;

function TPerformanceCounter.ElapsedMilliseconds(StartValue: QuadWord): Double;
begin
  Result := GetValue - StartValue / FFrequency;
end;

function TPerformanceCounter.GetValue: QuadWord;
begin
  if not QueryPerformanceCounter(Result) then
    RaiseLastPlatformError;
end;

function TPerformanceCounter.MillisecondsBetween(Value1, Value2: QuadWord): Double;
begin
  Result := (Value2 - Value1) / FFrequency;
end;

end.

