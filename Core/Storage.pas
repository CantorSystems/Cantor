(*
    The Unified Environment, legacy Win32 core

    Core stream and file system I/O implementation

    Copyright (c) 2007-2009 The Unified Environment Laboratory
*)

unit Storage;

interface

uses
  Windows, Core;

{ Streams }

type
  TReadableStream = class(TObject)
  protected
    procedure Error(Code: Integer); virtual; abstract;
    function GetPosition: Int64; virtual; abstract;
    function GetSize: Int64; virtual; abstract;
    procedure SetPosition(Value: Int64); virtual; abstract;
  public
    function Read(var Data; Count: Integer): Integer; virtual; abstract;
    procedure ReadBuffer(var Data; Count: Integer);
  // properties
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
  end;

  TWriteableStream = class(TReadableStream)
  protected
    procedure SetSize(Value: Int64); virtual; abstract;
  public
    function Write(const Buf; Count: Integer): Integer; virtual; abstract;
    procedure WriteBuffer(const Data; Count: Integer);
  // properties
    property Size write SetSize;
  end;

  TCoreStream = class(TWriteableStream)
  protected
    procedure Error(Code: Integer); override;
  end;

  TStream = TCoreStream;

const
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

type
  THandleStream = class(TStream)
  private
    FHandle: THandle;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
    procedure SetPosition(Value: Int64); override;
    procedure SetSize(Value: Int64); override;
  public
    constructor Create(FileName: PCoreChar; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL); overload;
    constructor Create(Drive: CoreChar; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL); overload;
    constructor Create(PhysicalDrive: Byte; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL); overload;
    constructor Create(StdHandleType: Cardinal); overload;
    destructor Destroy; override;
    function Open(FileName: PCoreChar; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL): Boolean; overload;
    function Open(Drive: CoreChar; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL): Boolean; overload;
    function Open(PhysicalDrive: Byte; Access, Creation: Cardinal;
      Share: Cardinal = FILE_SHARE_READ;
      Attributes: Cardinal = FILE_ATTRIBUTE_NORMAL): Boolean; overload;
    function Read(var Data; Count: Integer): Integer; override;
    function Seek(Offset: Int64; Origin: Cardinal): Int64;
    function Write(const Data; Count: Integer): Integer; override;
  // properties
    property Handle: THandle read FHandle;
  end;

  TFileStream = THandleStream;
  TStdStream = THandleStream;

{ File system }

const
  MAX_PATH = Windows.MAX_PATH;
  PathDelimiter = CoreChar('\');

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): LongBool; stdcall;
function SetFilePointerEx(hFile: THandle; liDistanceToMove: Int64;
  lpNewFilePointer: PInt64; dwMoveMethod: Cardinal): LongBool; stdcall;

implementation

uses
  SysUtils, Exceptions;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): LongBool; stdcall;
  external kernel32 name 'GetFileSizeEx';
function SetFilePointerEx(hFile: THandle; liDistanceToMove: Int64;
  lpNewFilePointer: PInt64; dwMoveMethod: Cardinal): LongBool; stdcall;
  external kernel32 name 'SetFilePointerEx';

{ TReadableStream }

procedure TReadableStream.ReadBuffer(var Data; Count: Integer);
var
  Bytes: Integer;
begin
  Bytes := Read(Data, Count);
  if Bytes <> Count then
    // TODO: exception
end;

{ TWriteableStream }

procedure TWriteableStream.WriteBuffer(const Data; Count: Integer);
var
  Bytes: Integer;
begin
  Bytes := Write(Data, Count);
  if Bytes <> Count then
    // TODO: exception
end;

{ TCoreStream }

procedure TCoreStream.Error(Code: Integer);
begin
  // TODO: exception
end;

{ THandleStream }

constructor THandleStream.Create(FileName: PCoreChar; Access, Creation,
  Share, Attributes: Cardinal);
begin
  if not Open(FileName, Access, Creation, Share, Attributes) then
    RaiseLastPlatformError;
end;

constructor THandleStream.Create(Drive: CoreChar; Access, Creation,
  Share, Attributes: Cardinal);
begin
  if not Open(Drive, Access, Creation, Share, Attributes) then
    RaiseLastPlatformError;
end;

constructor THandleStream.Create(PhysicalDrive: Byte; Access, Creation,
  Share, Attributes: Cardinal);
begin
  if not Open(PhysicalDrive, Access, Creation, Share, Attributes) then
    RaiseLastPlatformError;
end;

constructor THandleStream.Create(StdHandleType: Cardinal);
begin
  FHandle := {$IFDEF Tricks} System. {$ENDIF} GetStdHandle(StdHandleType);
  if FHandle = INVALID_HANDLE_VALUE then
    RaiseLastPlatformError;
end;

destructor THandleStream.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

function THandleStream.GetPosition: Int64;
begin
{$IFDEF Unicode} // since Windows 2000
  if not SetFilePointerEx(FHandle, 0, @Result, FILE_CURRENT) then
    Result := -1;
{$ELSE}
  with Int64Rec(Result) do
  begin
    Hi := 0;
    Lo := SetFilePointer(FHandle, 0, @Hi, FILE_CURRENT);
  end;
{$ENDIF}
end;

function THandleStream.GetSize: Int64;
begin
{$IFDEF Unicode} // since Windows 2000
  if not GetFileSizeEx(FHandle, Result) then
    Result := -1;
{$ELSE}
  with Int64Rec(Result) do
    Lo := GetFileSize(FHandle, @Hi);
{$ENDIF}
end;

function THandleStream.Open(FileName: PCoreChar; Access, Creation,
  Share, Attributes: Cardinal): Boolean;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := {$IFDEF Unicode} CreateFileW {$ELSE} CreateFileA {$ENDIF}
    (FileName, Access, Share, nil, Creation, Attributes, 0);
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

const
  DriveLetter = 4;
var
  DrivePath: array[0..6] of CoreChar = ('\', '\', '.', '\', '@', ':', #0);

function THandleStream.Open(Drive: CoreChar; Access, Creation, Share,
  Attributes: Cardinal): Boolean;
begin
  DrivePath[DriveLetter] := Drive;
  Result := Open(DrivePath, Access, Creation, Share, Attributes);
end;

const
  PhysicalDriveLetter = 17;
var
  PhysicalDrivePath: array[0..18] of CoreChar =
    ('\', '\', '.', '\', 'P', 'h', 'y', 's', 'i', 'c', 'a', 'l',
     'D', 'r', 'i', 'v', 'e', '@', #0);

function THandleStream.Open(PhysicalDrive: Byte; Access, Creation, Share,
  Attributes: Cardinal): Boolean;
begin
  PhysicalDrivePath[PhysicalDriveLetter] := CoreChar(PhysicalDrive + Byte('0'));
  Result := Open(PhysicalDrivePath, Access, Creation, Share, Attributes);
end;

function THandleStream.Read(var Data; Count: Integer): Integer;
var
  Rslt: Cardinal absolute Result;
begin
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FHandle, Data, Count, Rslt, nil);
end;

function THandleStream.Seek(Offset: Int64; Origin: Cardinal): Int64;
begin
{$IFDEF Unicode} // since Windows 2000
  if not SetFilePointerEx(FHandle, Offset, @Result, Origin) then
    Result := -1;
{$ELSE}
  Result := Offset;
  with Int64Rec(Result) do
    Lo := SetFilePointer(Handle, Lo, @Hi, Origin);
{$ENDIF}
end;

procedure THandleStream.SetPosition(Value: Int64);
begin
{$IFDEF Unicode} // since Windows 2000
  SetFilePointerEx(FHandle, Value, nil, FILE_BEGIN);
{$ELSE}
  Seek(Value, FILE_BEGIN);
{$ENDIF}
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

function THandleStream.Write(const Data; Count: Integer): Integer;
var
  Rslt: Cardinal absolute Result;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FHandle, Data, Count, Rslt, nil);
end;

end.

