(*
    Lite Core Library (CoreLite)

    OOP-style platform-dependent wrappers

    Copyright (c) 2007-2013 Vladislav Javadov (Freeman)

    Conditional defines:
      * Lite -- allow lite TStream implementation -- without virtual methods
                but only THandleStream descendant
*)

unit CoreWrappers;

interface

uses
  Windows, CoreUtils, CoreExceptions;

type
  TFileAccess = set of (faShareRead, faShareWrite, faShareDelete, // ordered
    faWrite, faKeep, faOverwrite, faDeleteOnClose, faSequential, faRandom,
    faNoBuffering, faOverlapped, faWriteThrough);

  TFileAttributes = set of (faReadOnly, faHidden, faSystem, faVolumeLabel,
    faDirectory, faArchive, faNormal, faTemporary, faSparsed, faReparsePoint,
    faCompressed, faOffline, faNonIndexable, faEncrypted, fa0x8000, faVirtual);

  TSeekOrigin = (soBegin, soCurrent, soEnd);

const
  faRead = [faShareRead];
  faRewrite = [faWrite, faOverwrite, faShareRead];

  faSequentialRead = faRead + [faSequential];
  faSequentialRewrite = faRewrite + [faSequential];

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

    property Position: QuadWord read GetPosition write SetPosition;
    property Size: QuadWord read GetSize;
  end;

  TWritableStream = class(TReadableStream)
  protected
    procedure SetSize(Value: QuadWord); virtual; abstract;
  public
    function Write(const Buf; Count: LongWord): LongWord; virtual; abstract;
    procedure WriteBuffer(const Data; Count: LongWord);

    property Size write SetSize;
  end;

  THandleStream = class(TWritableStream)
  private
    FHandle: THandle;
  protected
    function GetPosition: QuadWord; override;
    function GetSize: QuadWord; override;
    procedure SetPosition(Value: QuadWord); override;
    procedure SetSize(Value: QuadWord); override;
  public
    constructor Create(FileName: PCoreChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]); overload;
    destructor Destroy; override;

    function Open(FileName: PCoreChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]): Boolean; overload;

    function Seek(Offset: QuadWord; Origin: TSeekOrigin): QuadWord;
    function Read(var Data; Count: LongWord): LongWord; override;
    function Write(const Data; Count: LongWord): LongWord; override;

    function Lock(Offset, Count: QuadWord): Boolean;
    function Unlock(Offset, Count: QuadWord): Boolean;

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
      Size: QuadWord = 0; MappingName: PCoreChar = nil); overload;
    constructor Create(MappingName: PCoreChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True); overload;

    destructor Destroy; override;

    function Open(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil): Boolean; overload;
    function Open(MappingName: PCoreChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True): Boolean; overload;

    function MapView(Access: TMapViewAccess; Offset: QuadWord = 0;
      Count: CoreWord = 0): Pointer;
    procedure UnmapView(P: Pointer);

    property Handle: THandle read FHandle;
  end;

  TFileStreamMapping = class(TFileMapping)
  private
    FStream: TFileStream;
  public
    constructor Create(FileName: PCoreChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil); overload;
    destructor Destroy; override;
    function Open(FileName: PCoreChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil): Boolean; overload;

    property Stream: TFileStream read FStream;
  end;

  TConsole = class(TObject)
  private
    FInput, FOutput: THandle;
    function GetCodePage: Word;
    procedure SetCodePage(Value: Word);
  public
    constructor Create(ErrorOutput: Boolean = False);

    property CodePage: Word read GetCodePage write SetCodePage;
    property Input: THandle read FInput;
    property Output: THandle read FOutput;
  end;

  TStreamConsole = class(TConsole)
  public
  {$IFDEF Compat}
    constructor Create(ErrorOutput: Boolean = False);
    destructor Destroy; override;
  {$ENDIF}
    procedure ReadLn(Prompt: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer); overload;
    procedure WriteLn(Fmt: PLegacyChar; const Args: array of const;
      LineBreaks: Integer = 1); overload;
    procedure WriteLn(Fmt: PLegacyChar; FixedWidth: Integer; const Args: array of const;
      LineBreaks: Integer = 1); overload;
  end;

  TScreenConsole = class(TConsole)
  private
    procedure SetTextAttribute(Value: Word);
  public
    procedure ReadLn(Prompt: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer); overload;

    procedure ReadLn(Prompt: PWideChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PWideChar; Count, LineBreaks: Integer); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;

    procedure WriteLn(Text: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer); overload;

    procedure WriteLn(Text: PWideChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count, LineBreaks: Integer); overload;

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

    property Frequency: QuadWord read FFrequency;
    property Value: QuadWord read GetValue;
  end;

  TVersionFlags = set of (verDebug, verPreRelease, verPatched, verPrivateBuild,
    verInfoInferred, verSpecialBuild);
  TVersionOS = set of (osDOS, os2_16, os2_32, osNT, osWin16, osPM16, osPM32, osWin32);
  TVersionFileType = (ftUnknown, ftApplication, ftDLL, ftDriver, ftFont, ftVxD, ftStaticLib);
  TVersionDriverType = (drvUnknown, drvPrinter, drvKeyboard, drvLanguage, drvDisplay,
    drvMouse, drvNetwork, drvSystem, drvIFS, drvSound, drvComm, drvIME, drvVersionedPrinter);
  TVersionFontType = (fntUnknown, fntRaster, fntVector, fntTrueType);

  TVersion = packed record
    Minor, Major, Build, Release: Word; // platform
  end;

  TFixedVersionInfo = record
    FileVersion, ProductVersion: TVersion;
    Flags: TVersionFlags;
    OS: TVersionOS;
  {$IFNDEF Lite}
    TimeStamp: QuadWord;
  {$ENDIF}
  case FileType: TVersionFileType of
    ftDriver: (DriverType: TVersionDriverType);
    ftFont: (FontType: TVersionFontType);
  end;

  TTranslation = packed record
    Locale, CodePage: Word; // platform
  end;

  PTranslationArray = ^TTranslationArray;
  TTranslationArray = array[0..MaxInt div SizeOf(TTranslation) - 1] of TTranslation;

  TVersionBuffer = array[0..39] of LegacyChar;
  TFormatVersionOptions = set of (fvProductVersion, fvSkipZeroRelease);

  TVersionInfo = class
  private
    FData: Pointer;
    FTranslations: PTranslationArray;
  public
    constructor Create(FileName: PCoreChar); overload;
    destructor Destroy; override;
    function Open(FileName: PCoreChar): Boolean;

    function FixedInfo: TFixedVersionInfo; overload;
    function FixedInfo(var Info: TFixedVersionInfo): Boolean; overload;

    function FormatVersion(var Buffer: TVersionBuffer;
      Options: TFormatVersionOptions = [fvProductVersion..fvSkipZeroRelease];
      BuildFlags: TVersionFlags = [verDebug..verPatched]): Integer;

    function StringInfo(TranslationIndex: LongWord; Ident: PLegacyChar;
      var Info: PCoreChar; var Length: LongWord): Boolean; overload;
    function StringInfo(TranslationIndex: LongWord; Ident: PLegacyChar): PCoreChar; overload;

    function TranslationCount: CoreWord;

    property Data: Pointer read FData;
    property Translations: PTranslationArray read FTranslations;
  end;

{ Exceptions }

  TStreamAccess = (saRead, saWrite);

  EStream = class(Exception)
  private
    FActualBytes, FRequiredBytes: LongWord;
    FAccess: TStreamAccess;
  public
    constructor Create(Access: TStreamAccess; ActualBytes, RequiredBytes: LongWord);

    property Access: TStreamAccess read FAccess;
    property ActualBytes: LongWord read FActualBytes;
    property RequiredBytes: LongWord read FRequiredBytes;
  end;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadWord;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;

implementation

uses
  CoreConsts;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
  external kernel32 name 'GetFileSizeEx';
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadWord;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;
  external kernel32 name 'SetFilePointerEx';

{ EStream }

constructor EStream.Create(Access: TStreamAccess; ActualBytes,
  RequiredBytes: LongWord);
const
  Error: array[TStreamAccess] of PLegacyChar = (sStreamReadError, sStreamWriteError);
  Op: array[TStreamAccess] of PLegacyChar = (sStreamRead, sStreamWrote);
begin
  inherited Create(sStreamError, [Error[Access], Op[Access], ActualBytes, RequiredBytes]);
  FAccess := Access;
  FActualBytes := ActualBytes;
  FRequiredBytes := RequiredBytes;
end;

{ TReadableStream }

procedure TReadableStream.ReadBuffer(var Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Read(Data, Count);
  if Bytes <> Count then
    raise EStream.Create(saRead, Bytes, Count);
end;

{ TWritableStream }

procedure TWritableStream.WriteBuffer(const Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Write(Data, Count);
  if Bytes <> Count then
    raise EStream.Create(saWrite, Bytes, Count);
end;

{ THandleStream }

constructor THandleStream.Create(FileName: PCoreChar; Access: TFileAccess;
  Attributes: TFileAttributes);
begin
  if not Open(FileName, Access, Attributes) then
    RaiseLastPlatformError(FileName);
end;

destructor THandleStream.Destroy;
begin
  CloseHandle(FHandle);
end;

function THandleStream.GetPosition: QuadWord;
begin
  // SetFilePointerEx available since Windows 2000
  if not SetFilePointerEx(FHandle, 0, @Result, FILE_CURRENT) then
    Result := -1;
end;

function THandleStream.GetSize: QuadWord;
begin
  // GetFileSizeEx available since Windows 2000
  if not GetFileSizeEx(FHandle, Result) then
    Result := -1;
end;

function THandleStream.Lock(Offset, Count: QuadWord): Boolean;
begin
  Result := LockFile(Handle, QuadRec(Offset).Lo, QuadRec(Offset).Hi,
    QuadRec(Count).Lo, QuadRec(Count).Hi);
end;

function THandleStream.Open(FileName: PCoreChar; Access: TFileAccess;
  Attributes: TFileAttributes): Boolean;
const
  SharingMask = FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
var
  ReadWrite, Creation, Sharing: LongWord;
  NewHandle: THandle;
begin
  if faWrite in Access then
  begin
    ReadWrite := GENERIC_READ or GENERIC_WRITE;
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

function THandleStream.Read(var Data; Count: LongWord): LongWord;
begin
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FHandle, Data, Count, Result, nil);  // TODO: Windows x64
end;

function THandleStream.Seek(Offset: QuadWord; Origin: TSeekOrigin): QuadWord;
begin
  // SetFilePointerEx available since Windows 2000
  if not SetFilePointerEx(FHandle, Offset, @Result, LongWord(Origin)) then
    Result := QuadWord(-1);
end;

procedure THandleStream.SetPosition(Value: QuadWord);
begin
  if not SetFilePointerEx(FHandle, Value, nil, FILE_BEGIN) then
    RaiseLastPlatformError;
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

function THandleStream.Write(const Data; Count: LongWord): LongWord;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FHandle, Data, Count, Result, nil);  // TODO: Windows x64
end;

{ TFileMapping }

constructor TFileMapping.Create(hFile: THandle; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar);
begin
  if not Open(hFile, Options, Size, MappingName) then
    RaiseLastPlatformError;
end;

constructor TFileMapping.Create(MappingName: PCoreChar; Options: TOpenFileMapping;
  InheritHandle: Boolean);
begin
  if not Open(MappingName, Options, InheritHandle) then
    RaiseLastPlatformError(MappingName);
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
  Count: CoreWord): Pointer;
const
  // maRead, maWrite, maCopy{, maExecute}
  MapAccess: array[TMapViewAccess] of LongWord =
    (FILE_MAP_READ, FILE_MAP_WRITE, FILE_MAP_COPY{, FILE_MAP_EXECUTE});
begin
  Result := MapViewOfFile(FHandle, MapAccess[Access],
    QuadRec(Offset).Hi, QuadRec(Offset).Lo, Count);
end;

function TFileMapping.Open(hFile: THandle; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar): Boolean;
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

function TFileMapping.Open(MappingName: PCoreChar; Options: TOpenFileMapping;
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

constructor TFileStreamMapping.Create(FileName: PCoreChar; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar);
begin
  FStream := TFileStream.Create;
  if not Open(FileName, Options, Size, MappingName) then
    RaiseLastPlatformError(FileName);
end;

destructor TFileStreamMapping.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TFileStreamMapping.Open(FileName: PCoreChar; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar): Boolean;
const
  MapOptions: array[Boolean] of TFileAccess = (faRead, faRewrite);
begin
  if FStream.Open(FileName, MapOptions[maWrite in Options] + [faRandom]) then
  begin
    if maWrite in Options then
      FStream.Size := Size;
    Result := Open(FStream.Handle, Options, Size, MappingName);
  end
  else
    Result := False;
end;

{ TConsole }

constructor TConsole.Create(ErrorOutput: Boolean);
begin
  FInput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(STD_INPUT_HANDLE);
  FOutput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(STD_OUTPUT_HANDLE - Byte(ErrorOutput));
end;

function TConsole.GetCodePage: Word;
begin
  Result := GetConsoleOutputCP;
end;

procedure TConsole.SetCodePage(Value: Word);
begin
  SetConsoleCP(Value);
  SetConsoleOutputCP(Value);
end;

{ TStreamConsole }

{$IFDEF Compat}
constructor TStreamConsole.Create;
begin
  inherited;
  SetCodePage(GetACP); // SysUtils-compatible exceptions are ANSI
end;

destructor TStreamConsole.Destroy;
begin
  SetCodePage(GetOEMCP); // restore OEM code page for the caller like cmd or FAR
  inherited;
end;
{$ENDIF}

procedure TStreamConsole.ReadLn(Prompt: PLegacyChar; LineBreaks: Integer);
begin
  WriteLn;
  ReadLn(Prompt, StrLen(Prompt), LineBreaks);
end;

procedure TStreamConsole.ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer);
var
  Dummy: array[0..$FF] of LegacyChar; // preventing flood
  BytesRead: LongWord;
begin
  WriteLn(Prompt, Count, 0);
  WriteLn(@Ellipsis, SizeOf(Ellipsis), 0);
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FInput, Dummy, SizeOf(Dummy), BytesRead, nil);
  WriteLn(LineBreaks - 1);
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

procedure TStreamConsole.WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer);
var
  BytesWritten: LongWord;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FOutput, Text^, Count, BytesWritten, nil);  // TODO: Windows x64
  WriteLn(LineBreaks);
end;

procedure TStreamConsole.WriteLn(Fmt: PLegacyChar; const Args: array of const;
  LineBreaks: Integer);
begin
  WriteLn(Fmt, 0, Args, LineBreaks);
end;

procedure TStreamConsole.WriteLn(Fmt: PLegacyChar; FixedWidth: Integer;
  const Args: array of const; LineBreaks: Integer);
var
  P: PLegacyChar;
begin
  GetMem(P, StrLen(Fmt) + FixedWidth + EstimateArgs(Args) + 1);
  try
    WriteLn(P, FormatBuf(Fmt, Args, P), LineBreaks);
  finally
    FreeMem(P);
  end;
end;

{ TScreenConsole }

procedure TScreenConsole.ReadLn(Prompt: PLegacyChar; LineBreaks: Integer);
begin
  ReadLn(Prompt, StrLen(Prompt), LineBreaks);
end;

procedure TScreenConsole.ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer);
var
  Dummy: array[0..$FF] of LegacyChar; // preventing flood
  Read: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(Ellipsis, Length(Ellipsis), 0);
  ReadConsoleA(FInput, @Dummy, Length(Dummy), Read, nil);
  WriteLn(LineBreaks - 1);
end;

procedure TScreenConsole.ReadLn(Prompt: PWideChar; LineBreaks: Integer);
begin
  ReadLn(Prompt, WideStrLen(Prompt), LineBreaks);
end;

procedure TScreenConsole.ReadLn(Prompt: PWideChar; Count, LineBreaks: Integer);
var
  Dummy: array[0..$FF] of WideChar; // preventing flood
  Read: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(PWideChar(@WideEllipsis), 1, 0);
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
    WriteConsoleW(FOutput, @WideLF, 1, Written, nil);  // TODO: Unicode
end;

procedure TScreenConsole.WriteLn(Text: PLegacyChar; LineBreaks: Integer);
begin
  WriteLn(Text, StrLen(Text), LineBreaks);
end;

procedure TScreenConsole.WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer);
var
  Written: LongWord;
begin
  WriteConsoleA(FOutput, Text, Count, Written, nil);  // TODO: Windows x64
  WriteLn(LineBreaks);
end;

procedure TScreenConsole.WriteLn(Text: PWideChar; LineBreaks: Integer);
begin
  WriteLn(Text, WideStrLen(Text), LineBreaks);
end;

procedure TScreenConsole.WriteLn(Text: PWideChar; Count, LineBreaks: Integer);
var
  Written: LongWord;
begin
  WriteConsoleW(FOutput, Text, Count, Written, nil);  // TODO: Windows x64
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

{ TVersionInfo }

constructor TVersionInfo.Create(FileName: PCoreChar);
begin
  if not Open(FileName) then
    RaiseLastPlatformError(sVS_VERSION_INFO);
end;

destructor TVersionInfo.Destroy;
begin
  FreeMem(FData);
end;

function TVersionInfo.Open(FileName: PCoreChar): Boolean;
var
  DataSize, Dummy: LongWord;
begin
  DataSize := GetFileVersionInfoSizeW(FileName, Dummy);
  if DataSize <> 0 then
  begin
    GetMem(FData, DataSize);
    Result := GetFileVersionInfoW(FileName, 0, DataSize, FData);
  end
  else
    Result := False;
end;

function TVersionInfo.FixedInfo: TFixedVersionInfo;
begin
  if not FixedInfo(Result) then
    RaiseLastPlatformError;
end;

function TVersionInfo.FixedInfo(var Info: TFixedVersionInfo): Boolean;
var
  P: PVSFixedFileInfo;
  Size: LongWord;
begin
  if VerQueryValueW(FData, '\', Pointer(P), Size) and (Size <> 0) then
  begin
    with Info, P^ do
    begin
      QuadWord(FileVersion) := PQuadWord(@dwFileVersionMS)^;
      QuadWord(ProductVersion) := PQuadWord(@dwProductVersionMS)^;
      Byte(Flags) := dwFileFlags and dwFileFlagsMask;
      Byte(OS) := (1 shl (dwFileOS and $FFFF - 1)) or (1 shl (dwFileOS shr 16 - 1 + 4));
      Byte(FileType) := dwFileType;
      Byte(DriverType) := dwFileSubtype;
    {$IFNDEF Lite}
      QuadWord(TimeStamp) := PQuadWord(@dwFileDateMS)^;
    {$ENDIF}
    end;
    Result := True;
  end
  else
    Result := False;
end;

function TVersionInfo.FormatVersion(var Buffer: TVersionBuffer;
  Options: TFormatVersionOptions; BuildFlags: TVersionFlags): Integer;
const
  MinFmt: PLegacyChar = '%u.%u';
var
  Info: TFixedVersionInfo;
  Ver: TVersion;
  Fmt: array[0..9] of LegacyChar;
  Tmp: TVersionBuffer;
begin
  Info := FixedInfo;
  if fvProductVersion in Options then
    Ver := Info.ProductVersion
  else
    Ver := Info.FileVersion;

  if (fvSkipZeroRelease in Options) and (Ver.Release = 0) then
  begin
    if Info.Flags * BuildFlags = [] then
    begin
      Result := FormatBuf(MinFmt, [Ver.Major, Ver.Minor], Buffer);
      Exit;
    end;
    FormatBuf(MinFmt, [Ver.Major, Ver.Minor], Tmp);
  end
  else
  begin
    PQuadWord(@Fmt)^ := PQuadWord(MinFmt)^; // Fast core
    PLongWord(@Fmt[5])^ := PLongWord(MinFmt + 2)^;
    if Info.Flags * BuildFlags = [] then
    begin
      Result := FormatBuf(Fmt, [Ver.Major, Ver.Minor, Ver.Release], Buffer);
      Exit;
    end;
    FormatBuf(Fmt, [Ver.Major, Ver.Minor, Ver.Release], Tmp)
  end;
  Result := FormatBuf(sVersionAndBuild, [Tmp, Ver.Build], Buffer);
end;

function TVersionInfo.StringInfo(TranslationIndex: LongWord;
  Ident: PLegacyChar; var Info: PCoreChar; var Length: LongWord): Boolean;
var
  W: PWideChar;
begin
  with FTranslations[TranslationIndex] do
    W := LegacyFormat('\StringFileInfo\%04X%04X\%hs', CP_LEGACY, [Locale, CodePage, Ident]);
  try
    Result := VerQueryValueW(FData, W, Pointer(Info), Length);
    if Result then
      Length := Length div SizeOf(WideChar); // TODO: non-Unicode
  finally
    FreeMem(W);
  end;
end;

function TVersionInfo.StringInfo(TranslationIndex: LongWord; Ident: PLegacyChar): PCoreChar;
var
  L: LongWord;
begin
  if not StringInfo(TranslationIndex, Ident, Result, L) then
    Result := nil;
end;

function TVersionInfo.TranslationCount: CoreWord;
begin
  if VerQueryValueW(FData, '\VarFileInfo\Translation', Pointer(FTranslations), Result) then
    Result :=  Result div SizeOf(TTranslation)
  else
    Result := 0;
end;

end.
