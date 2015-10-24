(*
    Lite Core Library (CoreLite mini)

    OOP-style platform-dependent wrappers

    Copyright (c) 2007-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- force ANSI code page for TStreamConsole
      * Lite -- THandleStream and TFileMapping without virtual methods
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

type
  TByteOrderMark = (bomNone, bomUTF7, bomUTF8, bomUTF16LE, bomUTF16BE,
    bomUTF32LE, bomUTF32BE, bomGB18030);
  TReadableBOM = bomNone..bomGB18030;
  TWritableBOM = bomUTF8..bomGB18030;

const
  bomUTF16 = bomUTF16LE;
  bomUTF32 = bomUTF32LE;

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
    function ReadBOM: TReadableBOM;
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
    procedure WriteBOM(Value: TWritableBOM);
    procedure WriteBuffer(const Data; Count: LongWord);

    property Size write SetSize;
  end;

  PHandleStream = ^THandleStream;
  THandleStream = object(TWritableStream)
  private
    FHandle: THandle;
  protected
    function GetPosition: QuadWord; virtual;
    function GetSize: QuadWord; virtual;
    procedure SetPosition(Value: QuadWord); virtual;
    procedure SetSize(Value: QuadWord); virtual;
  public
    constructor Create(FileName: PCoreChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]); overload;
    destructor Destroy; virtual;

    function Open(FileName: PCoreChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]): Boolean; overload;

    function Seek(Offset: QuadWord; Origin: TSeekOrigin): QuadWord;
    function Read(var Data; Count: LongWord): LongWord; virtual;
    function Write(const Data; Count: LongWord): LongWord; virtual;

    function Lock(Offset, Count: QuadWord): Boolean;
    function Unlock(Offset, Count: QuadWord): Boolean;

    property Handle: THandle read FHandle;
  end;
{$ENDIF}

  PStream = PWritableStream;
  TStream = TWritableStream;

  PFileInformation = ^TFileInformation;
  TFileInformation = packed record
    CreationTime, LastAccessTime, LastWriteTime: TFileTime;
    Attributes: LongWord;
  end;

  PFileStream = PHandleStream;
  TFileStream = object(THandleStream)
  private
    function GetInformation: TFileInformation;
  //  procedure SetInformation(const Value: TFileInformation); // TODO Windows6
  public
    procedure SetTime(Creation, LastAccess, LastWrite: PFileTime); overload;
    procedure SetTime(const Info: TFileInformation); overload;

    property Information: TFileInformation read GetInformation {write SetInformation};
  end;

  TMappingOption = (maRead, maWrite, maCopy, maExecute, maImage, maReserve, maNoCache);
  TCreateFileMapping = set of maWrite..maNoCache;
  TOpenFileMapping = set of maRead..maCopy;
  TMapViewAccess = maRead..maCopy{maExecute};

  PFileMapping = ^TFileMapping;
  TFileMapping = object {$IFNDEF Lite} (TCoreObject) {$ENDIF}
  private
    FHandle: THandle;
    function AssignHandle(Value: THandle): Boolean;
  public
    constructor Create(hFile: THandle; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil); overload;
    constructor Create(MappingName: PCoreChar; Options: TOpenFileMapping;
      InheritHandle: Boolean = True); overload;

    destructor Destroy; {$IFNDEF Lite} virtual; {$ENDIF}

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
    destructor Destroy; {$IFNDEF Lite} virtual; {$ENDIF}
    function Open(FileName: PCoreChar; Options: TCreateFileMapping;
      Size: QuadWord = 0; MappingName: PCoreChar = nil): Boolean; overload;

    property Stream: TFileStream read FStream;
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
    procedure ReadLn(Prompt: PLegacyChar; Count: Integer; LineBreaks: Integer = 1);

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count: Integer; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Fmt: PLegacyChar; FixedWidth: Integer;
      const Args: array of const; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count: Integer; LineBreaks: Integer = 1); overload;
  end;

  PScreenConsole = ^TScreenConsole;
  TScreenConsole = object(TConsole)
  private
    procedure SetTextAttribute(Value: Word);
  public
    procedure ReadLn(Prompt: PLegacyChar; Count: Integer; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PWideChar; Count: Integer; LineBreaks: Integer = 1); overload;

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PLegacyChar; Count: Integer; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count: Integer; LineBreaks: Integer = 1); overload;

    property TextAttribute: Word write SetTextAttribute;
  end;

  PPerformanceCounter = ^TPerformanceCounter;
  TPerformanceCounter = object
  private
    FFrequency: QuadWord;
    function GetValue: QuadWord;
  public
    constructor Create;
    function MillisecondsFrom(StartValue: QuadWord): Double;
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

const
  fvDefault = [fvProductVersion..fvSkipZeroRelease];
  verDefault = [verDebug..verPatched];

type
  PVersionInfo = ^TVersionInfo;
  TVersionInfo = object
  private
    FData: Pointer;
    FTranslations: PTranslationArray;
  public
    constructor Create(FileName: PCoreChar); overload;
    destructor Destroy;
    function Open(FileName: PCoreChar): Boolean;

    function FixedInfo: TFixedVersionInfo; overload;
    function FixedInfo(var Info: TFixedVersionInfo): Boolean; overload;

    function FormatVersion(var Buffer: TVersionBuffer; BuildFormat: PLegacyChar;
      Options: TFormatVersionOptions = fvDefault; BuildFlags: TVersionFlags = verDefault): Integer;

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

{ Helper functions }

type
  TLoadProc = procedure(Stream: PReadableStream) of object;
  TSaveProc = procedure(Stream: PWritableStream) of object;

  TLoadFileResult = record
    FileInfo: TFileInformation;
    FileSize: QuadWord;
    BytesRead: CoreWord;
  end;

  TSaveOptions = set of (soBackup, soCopyAttr, soCopyTime);

function LoadFile(const LoadProc: TLoadProc; FileName: PCoreChar;
  Access: TFileAccess = faSequentialRead): TLoadFileResult;

procedure SaveFile(const SaveProc: TSaveProc; FileName: PCoreChar; FileSize: QuadWord;
  Access: TFileAccess = faSequentialRewrite); overload;
procedure SaveFile(const SaveProc: TSaveProc; FileName: PCoreChar; FileSize: QuadWord;
  const FileInfo: TFileInformation; Access: TFileAccess = faSequentialRewrite); overload;
procedure SaveFile(const SaveProc: TSaveProc; BackupFileName, FileName: PCoreChar;
  FileSize: QuadWord; Access: TFileAccess = faSequentialRewrite;
  Options: TSaveOptions = [soBackup..soCopyTime]); overload;

{ Import Windows functions for Delphi 6/7 }

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadInt;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;

implementation

uses
  CoreConsts;

function GetFileSizeEx(hFile: THandle; var lpFileSize: QuadWord): LongBool; stdcall;
  external kernel32 name 'GetFileSizeEx';
function SetFilePointerEx(hFile: THandle; liDistanceToMove: QuadInt;
  lpNewFilePointer: PQuadWord; dwMoveMethod: LongWord): LongBool; stdcall;
  external kernel32 name 'SetFilePointerEx';

{ Helper functions }

function LoadFile(const LoadProc: TLoadProc; FileName: PCoreChar;
  Access: TFileAccess): TLoadFileResult;
var
  F: TFileStream;
begin
  F.Create(FileName, Access);
  try
    Result.FileSize := F.Size;
    Result.FileInfo := F.Information;
    LoadProc(@F);
    Result.BytesRead := F.Position;
  finally
    F.Destroy;
  end;
end;

procedure SaveFile(const SaveProc: TSaveProc; FileName: PCoreChar; FileSize: QuadWord;
  Access: TFileAccess);
var
  F: TFileStream;
begin
  F.Create(FileName, Access);
  try
    F.SetSize(FileSize);
    SaveProc(@F);
    F.SetSize(F.Position);
  finally
    F.Destroy;
  end;
end;

procedure SaveFile(const SaveProc: TSaveProc; FileName: PCoreChar; FileSize: QuadWord;
  const FileInfo: TFileInformation; Access: TFileAccess);
var
  F: TFileStream;
begin
  F.Create(FileName, Access);
  try
    if (FileInfo.Attributes <> 0) and not SetFileAttributesW(FileName, FileInfo.Attributes) then
      RaiseLastPlatformError(FileName);
    F.SetSize(FileSize);
    SaveProc(@F);
    F.SetSize(F.Position);
    F.SetTime(FileInfo);
  finally
    F.Destroy;
  end;
end;

procedure SaveFile(const SaveProc: TSaveProc; BackupFileName, FileName: PCoreChar;
  FileSize: QuadWord; Access: TFileAccess; Options: TSaveOptions);
var
  OldInfo: TWin32FileAttributeData;
  NewInfo: TFileInformation;
begin
  if not MoveFileExW(FileName, BackupFileName,
    MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH or MOVEFILE_REPLACE_EXISTING)
  then
    RaiseLastPlatformError(FileName);
  if Options * [soCopyAttr..soCopyTime] <> [] then
  begin
    if not GetFileAttributesExW(BackupFileName, GetFileExMaxInfoLevel, @OldInfo) then
      RaiseLastPlatformError(BackupFileName);
    if soCopyAttr in Options then
      NewInfo.Attributes := OldInfo.dwFileAttributes
    else
      NewInfo.Attributes := 0;
    if soCopyTime in Options then
      Move(OldInfo.ftCreationTime, NewInfo.CreationTime, SizeOf(TFileTime) * 3)
    else
      FillChar(NewInfo.CreationTime, SizeOf(TFileTime) * 3, 0);
  end;
  SaveFile(SaveProc, FileName, FileSize, NewInfo, Access);
  if not (soBackup in Options) and not DeleteFileW(BackupFileName) then
    RaiseLastPlatformError(BackupFileName);
end;

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

function TReadableStream.ReadBOM: TReadableBOM;
var
  BOM: Word;
  Pos: QuadWord;
begin
  Pos := GetPosition;
  ReadBuffer(BOM, SizeOf(BOM));
  case BOM of
    $FEFF:
      begin
        ReadBuffer(BOM, SizeOf(BOM));
        if BOM = 0 then
          Result := bomUTF32LE
        else
        begin
          SetPosition(Pos + SizeOf(BOM));
          Result := bomUTF16LE;
        end;
        Exit;
      end;
    $FFFE:
      begin
        Result := bomUTF16BE;
        Exit;
      end;
    $BBEF:
      begin
        ReadBuffer(BOM, SizeOf(Byte));
        if Byte(BOM) = $BF then
        begin
          Result := bomUTF8;
          Exit;
        end;
      end;
    $0000:
      begin
        ReadBuffer(BOM, SizeOf(BOM));
        if BOM = $FFFE then
        begin
          Result := bomUTF32BE;
          Exit;
        end;
      end;
    $2F2B:
      begin
        ReadBuffer(BOM, SizeOf(BOM));
        if Byte(BOM) = $76 then
          case BOM shr 8 of
            $38:
              begin
                ReadBuffer(BOM, SizeOf(Byte));
                if Byte(BOM) <> $2D then
                  SetPosition(Pos + SizeOf(BOM) * 2)
                else
                begin
                  Result := bomUTF7;
                  Exit;
                end;
              end;
            $39, $2B, $2F:
              begin
                Result := bomUTF7;
                Exit;
              end;
          end;
      end;
    $3184:
      begin
        ReadBuffer(BOM, SizeOf(BOM));
        if BOM = $3395 then
        begin
          Result := bomGB18030;
          Exit;
        end;
      end;
  end;
  SetPosition(Pos);
  Result := bomNone;
end;

procedure TReadableStream.ReadBuffer(var Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Read(Data, Count);
  if Bytes <> Count then
    raise EStream.Create(saRead, Bytes, Count);
end;

{ TWritableStream }

procedure TWritableStream.WriteBOM(Value: TWritableBOM);
const
  UTF8: array[0..2] of Byte = ($EF, $BB, $BF);
  GB18030: array[0..3] of Byte = ($84, $31, $95, $33);
var
  BOM: LongWord;
begin
  case Value of
    bomUTF8:
      WriteBuffer(UTF8, SizeOf(UTF8));
    bomGB18030:
      WriteBuffer(GB18030, SizeOf(GB18030));
  else
    BOM := $FEFF;
    if Value in [bomUTF16BE, bomUTF32BE] then
    begin
      BOM := Swap(BOM);
      if Value = bomUTF32BE then
        BOM := BOM shl 16;
    end;     
    WriteBuffer(BOM, (Byte(Value) - 1) div 2 * 2); // Fast core
  end;
end;

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
{$IFDEF Lite}
  FHandle := 0;
{$ENDIF}  
  if not Open(FileName, Access, Attributes) then
    RaiseLastPlatformError(FileName);
end;

destructor THandleStream.Destroy;
begin
  CloseHandle(FHandle);
end;

function THandleStream.GetPosition: QuadWord;
begin
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

{ TFileStream }

function TFileStream.GetInformation: TFileInformation;
var
  Info: TByHandleFileInformation;
begin
  if GetFileInformationByHandle(FHandle, Info) then
    with Info, Result do
    begin
      Attributes := dwFileAttributes;
      CreationTime := ftCreationTime;
      LastAccessTime := ftLastAccessTime;
      LastWriteTime := ftLastWriteTime;
    end
  else
    RaiseLastPlatformError(nil);
end;

procedure TFileStream.SetTime(Creation, LastAccess, LastWrite: PFileTime);
begin
  if not SetFileTime(FHandle, Creation, LastAccess, LastWrite) then
    RaiseLastPlatformError(nil);
end;

procedure TFileStream.SetTime(const Info: TFileInformation);
var
  Creation, LastAccess, LastWrite: PFileTime;
begin
  with Info do
  begin
    if QuadWord(CreationTime) <> 0 then
      Creation := @CreationTime
    else
      Creation := nil;
    if QuadWord(LastAccessTime) <> 0 then
      LastAccess := @LastAccessTime
    else
      LastAccess := nil;
    if QuadWord(LastWriteTime) <> 0 then
      LastWrite := @LastWriteTime
    else
      LastWrite := nil;
  end;
  SetTime(Creation, LastAccess, LastWrite);
end;

{ TFileMapping }

constructor TFileMapping.Create(hFile: THandle; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar);
begin
{$IFDEF Lite}
  FHandle := 0;
{$ENDIF}
  if not Open(hFile, Options, Size, MappingName) then
    RaiseLastPlatformError(MappingName);
end;

constructor TFileMapping.Create(MappingName: PCoreChar; Options: TOpenFileMapping;
  InheritHandle: Boolean);
begin
  FHandle := 0;
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
{$IFDEF Lite}
  FStream.FHandle := 0;
{$ELSE}  
  FStream.Create;
{$ENDIF}
  FHandle := 0;
  if not Open(FileName, Options, Size, MappingName) then
    RaiseLastPlatformError(FileName);
end;

destructor TFileStreamMapping.Destroy;
begin
  inherited;
  FStream.Destroy;
end;

function TFileStreamMapping.Open(FileName: PCoreChar; Options: TCreateFileMapping;
  Size: QuadWord; MappingName: PCoreChar): Boolean;
const
  MapOptions: array[Boolean] of TFileAccess = (faRead, faRewrite);
begin
  if FStream.Open(FileName, MapOptions[maWrite in Options] + [faRandom]) then
  begin
    if maWrite in Options then
      FStream.SetSize(Size);
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
  FCodePage := GetConsoleOutputCP;
  FInputCP := 0;
  FOutputCP := 0;
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

procedure TStreamConsole.ReadLn(Prompt: PLegacyChar; Count, LineBreaks: Integer);
var
  Dummy: array[0..$FF] of LegacyChar; // preventing flood
  BytesRead: LongWord;
begin
  WriteLn;
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
  LFx4 := $0A0A0A0A;
  for I := 0 to LineBreaks div 4 - 1 do {$IFDEF Tricks} System. {$ENDIF}
    WriteFile(FOutput, LFx4, 4, BytesWritten, nil);
  if LineBreaks mod 4 <> 0 then {$IFDEF Tricks} System. {$ENDIF}
    WriteFile(FOutput, LFx4, LineBreaks mod 4, BytesWritten, nil);
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
    W := Format(Fmt, DefaultSystemCodePage, FixedWidth, Args);
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
  S := DecodeUTF16(Text, Count, True, FCodePage);
  try
    WriteLn(S.Value, S.Length, LineBreaks);
  finally
    FreeMem(S.Value);
  end;
end;

{ TScreenConsole }

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
  WideLFx4: QuadWord;
begin // Fast core
  WideLFx4 := $000A000A000A000A;
  for I := 0 to LineBreaks div 4 - 1 do
    WriteConsoleW(FOutput, @WideLFx4, 4, Written, nil);
  if LineBreaks mod 4 <> 0 then
    WriteConsoleW(FOutput, @WideLFx4, LineBreaks mod 4, Written, nil);
end;

procedure TScreenConsole.WriteLn(Text: PLegacyChar; Count, LineBreaks: Integer);
var
  Written: LongWord;
begin
  WriteConsoleA(FOutput, Text, Count, Written, nil);  // TODO: Windows x64
  WriteLn(LineBreaks);
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
    RaiseLastPlatformError(sPerformanceFrequency, 0);
end;

function TPerformanceCounter.MillisecondsFrom(StartValue: QuadWord): Double;
begin
  Result := (GetValue - StartValue) / FFrequency;
end;

function TPerformanceCounter.GetValue: QuadWord;
begin
  if not QueryPerformanceCounter(Result) then
    RaiseLastPlatformError(sPerformanceCounter, 0);
end;

function TPerformanceCounter.MillisecondsBetween(Value1, Value2: QuadWord): Double;
begin
  Result := (Value2 - Value1) / FFrequency;
end;

{ TVersionInfo }

constructor TVersionInfo.Create(FileName: PCoreChar);
begin
  FData := nil;
  FTranslations := nil;
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

function TVersionInfo.FormatVersion(var Buffer: TVersionBuffer; BuildFormat: PLegacyChar;
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
  Result := FormatBuf(BuildFormat, [Tmp, Ver.Build], Buffer);
end;

function TVersionInfo.StringInfo(TranslationIndex: LongWord;
  Ident: PLegacyChar; var Info: PCoreChar; var Length: LongWord): Boolean;
var
  W: PCoreChar;
begin
  with FTranslations[TranslationIndex] do
    W := Format('\StringFileInfo\%04X%04X\%hs', DefaultSystemCodePage, 0, [Locale, CodePage, Ident]).Value;
  try
    Result := VerQueryValueW(FData, W, Pointer(Info), Length);
    if Result then
      Length := Length div SizeOf(CoreChar); // TODO: non-Unicode
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

