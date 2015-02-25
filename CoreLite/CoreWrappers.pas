(*
    Lite Core Library (CoreLite mini)

    OOP-style platform-dependent wrappers

    Copyright (c) 2007-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- force ANSI code page for TStreamConsole
      * Lite -- allow lite TStream implementation -- without virtual methods,
                but only THandleStream descendant
*)

unit CoreWrappers;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreClasses;

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

  faRandomRead = faRead + [faRandom];
  faRandomRewrite = faRewrite + [faRandom];

{$IFDEF Lite}
  {$I LiteStreams.inc}
{$ELSE}
type
  PReadableStream = ^TReadableStream;
  TReadableStream = object(TCoreObject)
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

  PWritableStream = ^TWritableStream;
  TWritableStream = object(TReadableStream)
  protected
    procedure SetSize(Value: QuadWord); virtual; abstract;
  public
    function Write(const Buf; Count: LongWord): LongWord; virtual; abstract;
    procedure WriteBuffer(const Data; Count: LongWord);

    property Size write SetSize;
  end;

  PHandleStream = ^THandleStream;
  THandleStream = object(TWritableStream)
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
    destructor Destroy; virtual;

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

  PStream = PWritableStream;
  TStream = TWritableStream;

  PFileStream = PHandleStream;
  TFileStream = THandleStream;

  TMappingOption = (maRead, maWrite, maCopy, maExecute, maImage, maReserve, maNoCache);
  TCreateFileMapping = set of maWrite..maNoCache;
  TOpenFileMapping = set of maRead..maCopy;
  TMapViewAccess = maRead..maCopy{maExecute};

  PFileMapping = ^TFileMapping;
  TFileMapping = object(TCoreObject)
  private
    FHandle: THandle;
    function AssignHandle(Value: THandle): Boolean;
  public
    constructor Create(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil); overload;
    constructor Create(MappingName: PCoreChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True); overload;

    destructor Destroy; virtual;

    function Open(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil): Boolean; overload;
    function Open(MappingName: PCoreChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True): Boolean; overload;

    function MapView(Access: TMapViewAccess; Offset: QuadWord = 0;
      Count: CoreWord = 0): Pointer;
    procedure UnmapView(P: Pointer);

    property Handle: THandle read FHandle;
  end;

  PFileStreamMapping = ^TFileStreamMapping;
  TFileStreamMapping = object(TFileMapping)
  private
    FStream: TFileStream;
  public
    constructor Create(FileName: PCoreChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil); overload;
    destructor Destroy; virtual;
    function Open(FileName: PCoreChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil): Boolean; overload;

    property Stream: TFileStream read FStream;
  end;

  TReadableStreamEvent = procedure(const Stream: TReadableStream) of object;
  TWritableStreamEvent = procedure(const Stream: TWritableStream) of object;

  PLoadHelper = ^TLoadHelper;
  TLoadHelper = object
    BeforeLoad, AfterLoad: TReadableStreamEvent;
    procedure Load(HostLoad: TReadableStreamEvent; FileName: PCoreChar;
      Access: TFileAccess = faSequentialRead);
  end;

  PSaveHelper = ^TSaveHelper;
  TSaveHelper = object
    BeforeSave, AfterSave: TWritableStreamEvent;
    procedure Save(HostSave: TWritableStreamEvent; FileName: PCoreChar; FileSize: Int64;
      Access: TFileAccess = faSequentialRewrite; Attributes: TFileAttributes = [faNormal]);
  end;

  PConsole = ^TConsole;
  TConsole = object
  private
    FInput, FOutput: THandle;
    FCodePage, FInputCP, FOutputCP: Word;
    procedure SetCodePage(Value: Word);
  public
    constructor Create(ErrorOutput: Boolean = False);
    destructor Destroy;

    property CodePage: Word read FCodePage write SetCodePage;
    property Input: THandle read FInput;
    property Output: THandle read FOutput;
  end;

  PStreamConsole = ^TStreamConsole;
  TStreamConsole = object(TConsole)
  public
  {$IFDEF Debug}
    constructor Create(ErrorOutput: Boolean = False);
  {$ENDIF}
    procedure ReadLn(Prompt: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer); overload;
    procedure WriteLn(Fmt: PLegacyChar; FixedWidth: Integer;
      const Args: array of const; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count, LineBreaks: Integer); overload;
  end;

  PScreenConsole = ^TScreenConsole;
  TScreenConsole = object(TConsole)
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

  PPerformanceCounter = ^TPerformanceCounter;
  TPerformanceCounter = object
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

  PVersionInfo = ^TVersionInfo;
  TVersionInfo = object(TObject)
  private
    FData: Pointer;
    FTranslations: PTranslationArray;
  public
    constructor Create(FileName: PCoreChar); overload;
    destructor Destroy;
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
    RaiseLastPlatformError(sPosition, Value);
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
    RaiseLastPlatformError(MappingName);
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
{$IFNDEF Lite}
  FStream.Create;
{$ENDIF}
  if not Open(FileName, Options, Size, MappingName) then
    RaiseLastPlatformError(FileName);
end;

destructor TFileStreamMapping.Destroy;
begin
  inherited;
{$IFDEF Lite}
  FStream.Destroy;
{$ELSE}
  FStream.Finalize;
{$ENDIF}
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

{ TLoadHelper }

procedure TLoadHelper.Load(HostLoad: TReadableStreamEvent; FileName: PCoreChar;
  Access: TFileAccess);
var
  F: TFileStream;
begin
  F.Create(FileName, Access);
  try
    if Assigned(BeforeLoad) then
      BeforeLoad(F);
    HostLoad(F);
    if Assigned(AfterLoad) then
      AfterLoad(F);
  finally
    F.Destroy;
  end;
end;

{ TSaveHelper }

procedure TSaveHelper.Save(HostSave: TWritableStreamEvent; FileName: PCoreChar;
  FileSize: Int64; Access: TFileAccess; Attributes: TFileAttributes);
var
  F: TFileStream;
begin
  F.Create(FileName, Access, Attributes);
  try
    if Assigned(BeforeSave) then
      BeforeSave(F);
    F.Size := FileSize;  
    HostSave(F);
    if Assigned(AfterSave) then
      AfterSave(F);
  finally
    F.Destroy;
  end;
end;

{ TConsole }

constructor TConsole.Create(ErrorOutput: Boolean);
begin
  FInput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(STD_INPUT_HANDLE);
  FOutput := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(STD_OUTPUT_HANDLE - Byte(ErrorOutput));
  FCodePage := GetConsoleOutputCP;
end;

destructor TConsole.Destroy;
begin
  if FInputCP <> 0 then
    SetConsoleCP(FInputCP);
  if FOutputCP <> 0 then
    SetConsoleOutputCP(FOutputCP);
end;

procedure TConsole.SetCodePage(Value: Word);
begin
  if FInputCP = 0 then
    FInputCP := GetConsoleCP;
  if FOutputCP = 0 then
    FOutputCP := GetConsoleOutputCP;
  if not SetConsoleCP(Value) or not SetConsoleOutputCP(Value) then
    RaiseLastPlatformError {$IFDEF Debug} (sConsoleCodePage, Value) {$ENDIF} ;
  FCodePage := Value;
end;

{ TStreamConsole }

{$IFDEF Debug}
constructor TStreamConsole.Create;
begin
  inherited;
  SetCodePage(GetACP); // SysUtils-compatible exceptions are ANSI
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
  WriteLn(PLegacyChar(@Ellipsis), SizeOf(Ellipsis), 0);
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FInput, Dummy, SizeOf(Dummy), BytesRead, nil);
  WriteLn(LineBreaks - 1);
end;

procedure TStreamConsole.WriteLn(LineBreaks: Integer);
var
  I: Integer;
  BytesWritten, LFx4: LongWord;
begin // Fast core
  LFx4 := Byte(LF) or (Byte(LF) shl 8) or (Byte(LF) shl 16) or (Byte(LF) shl 24);
  for I := 0 to LineBreaks div 4 - 1 do {$IFDEF Tricks} System. {$ENDIF}
    WriteFile(FOutput, LFx4, 4, BytesWritten, nil);
  if LineBreaks mod 4 <> 0 then {$IFDEF Tricks} System. {$ENDIF}
    WriteFile(FOutput, LFx4, LineBreaks mod 4, BytesWritten, nil);
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

procedure TStreamConsole.WriteLn(Fmt: PLegacyChar; FixedWidth: Integer;
  const Args: array of const; LineBreaks: Integer);
var
  S: TLegacyStringRec;
  W: TWideStringRec;
begin
  if FCodePage = CP_UTF8 then
  begin
    W := FormatString(Fmt, CP_LEGACY, FixedWidth, Args);
    try
      WriteLn(W.Value, W.Length, LineBreaks);
    finally
      FreeMem(W.Value);
    end;
  end
  else
  begin
    S := Format(Fmt, FixedWidth, Args);
    try
      WriteLn(S.Value, S.Length, LineBreaks);
    finally
      FreeMem(S.Value);
    end;
  end;
end;

procedure TStreamConsole.WriteLn(Text: PWideChar; Count, LineBreaks: Integer);
var
  S: TLegacyStringRec;
begin
  S := DecodeUTF16(Text, Count, FCodePage);
  try
    WriteLn(S.Value, S.Length, LineBreaks);
  finally
    FreeMem(S.Value);
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
  WideLFx4: QuadRec;
begin // Fast core
  with WideLFx4 do
  begin
    Lo := Word(WideLF) or (Word(WideLF) shl 16);
    Hi := Lo;
  end;
  for I := 0 to LineBreaks div 4 - 1 do
    WriteConsoleW(FOutput, @WideLFx4, 4, Written, nil);
  if LineBreaks mod 4 <> 0 then
    WriteConsoleW(FOutput, @WideLFx4, LineBreaks mod 4, Written, nil);
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
    RaiseLastPlatformError {$IFDEF Debug} (sPerformanceFrequency, 0) {$ENDIF} ;
end;

function TPerformanceCounter.ElapsedMilliseconds(StartValue: QuadWord): Double;
begin
  Result := GetValue - StartValue / FFrequency;
end;

function TPerformanceCounter.GetValue: QuadWord;
begin
  if not QueryPerformanceCounter(Result) then
    RaiseLastPlatformError {$IFDEF Debug} (sPerformanceCounter, 0) {$ENDIF} ;
end;

function TPerformanceCounter.MillisecondsBetween(Value1, Value2: QuadWord): Double;
begin
  Result := (Value2 - Value1) / FFrequency;
end;

{ TVersionInfo }

constructor TVersionInfo.Create(FileName: PCoreChar);
begin
  if not Open(FileName) then
    RaiseLastPlatformError(sVS_VERSION_INFO, 0);
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
    RaiseLastPlatformError(sFixedVersionInfo, 0);
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
    W := FormatString('\StringFileInfo\%04X%04X\%hs', CP_LEGACY, 0, [Locale, CodePage, Ident]).Value;
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

