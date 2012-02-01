(*
    The Unified Environment Core Library

    OOP-style platform-dependent wrappers

    Copyright (c) 2007-2012 The Unified Environment Laboratory
*)

unit CoreWrappers;

interface

uses
  Windows, CoreUtils;

{ Streams }

type
  TReadableStream = class(TObject)
  protected
    function GetPosition: Int64; virtual; abstract;
    function GetSize: Int64; virtual; abstract;
    procedure SetPosition(Value: Int64); virtual; abstract;
  public
    function Read(var Data; Count: LongWord): LongWord; virtual; abstract;
    procedure ReadBuffer(var Data; Count: LongWord);
  // properties
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
  end;

  TWriteableStream = class(TReadableStream)
  protected
    procedure SetSize(Value: Int64); virtual; abstract;
  public
    function Write(const Buf; Count: LongWord): LongWord; virtual; abstract;
    procedure WriteBuffer(const Data; Count: LongWord);
  // properties
    property Size write SetSize;
  end;

  TStream = TWriteableStream;

  THandleStream = class(TStream)
  private
    FHandle: THandle;
  protected
    function GetPosition: Int64; override;
    function GetSize: Int64; override;
    procedure SetPosition(Value: Int64); override;
    procedure SetSize(Value: Int64); override;
  public
    constructor Create(FileName: PWideChar; Access, Creation: LongWord;
      Share: LongWord = FILE_SHARE_READ; Attributes: LongWord = FILE_ATTRIBUTE_NORMAL); overload;
    constructor Create(StdHandleType: LongWord); overload;
    destructor Destroy; override;
    function Lock(Offset, Count: Int64): Boolean;
    function Open(FileName: PWideChar; Access, Creation: LongWord;
      Share: LongWord = FILE_SHARE_READ; Attributes: LongWord = FILE_ATTRIBUTE_NORMAL): Boolean;
    function Read(var Data; Count: LongWord): Cardinal; override;
    function Seek(Offset: Int64; Origin: LongWord): Int64;
    function Unlock(Offset, Count: Int64): Boolean;
    function Write(const Data; Count: LongWord): LongWord; override;
  // properties
    property Handle: THandle read FHandle;
  end;

  TFileStream = THandleStream;
  TStdStream = THandleStream;

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
  public
    procedure ReadLn(Prompt: PWideChar; LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: PWideChar; Count: LongWord; LineBreaks: Integer); overload;

    procedure SetTextAttribute(Value: Word);

    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PWideChar; Count: LongWord; LineBreaks: Integer); overload;
  end;

  TPerformanceCounter = class
  private
    FFrequency: Int64;
    function GetValue: Int64;
  public
    constructor Create;
    function ElapsedMilliseconds(StartValue: Int64): Double;
    function MillisecondsBetween(Value1, Value2: Int64): Double;
  // properties
    property Frequency: Int64 read FFrequency;
    property Value: Int64 read GetValue;
  end;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): LongBool; stdcall;
function SetFilePointerEx(hFile: THandle; liDistanceToMove: Int64;
  lpNewFilePointer: PInt64; dwMoveMethod: LongWord): LongBool; stdcall;

implementation

uses
  Exceptions;

{ Absent in Windows.pas }

function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): LongBool; stdcall;
  external kernel32 name 'GetFileSizeEx';
function SetFilePointerEx(hFile: THandle; liDistanceToMove: Int64;
  lpNewFilePointer: PInt64; dwMoveMethod: LongWord): LongBool; stdcall;
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

{ TWriteableStream }

procedure TWriteableStream.WriteBuffer(const Data; Count: LongWord);
var
  Bytes: LongWord;
begin
  Bytes := Write(Data, Count);
  if Bytes <> Count then
    RaiseLastPlatformError; // TODO: exception
end;

{ THandleStream }

constructor THandleStream.Create(FileName: PWideChar; Access, Creation, 
  Share, Attributes: LongWord);
begin
  if not Open(FileName, Access, Creation, Share, Attributes) then
    RaiseLastPlatformError;
end;

constructor THandleStream.Create(StdHandleType: LongWord);
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
  // GetFileSizeEx available since Windows 2000
  if not SetFilePointerEx(FHandle, 0, @Result, FILE_CURRENT) then
    Result := -1;
end;

function THandleStream.GetSize: Int64;
begin
  if not GetFileSizeEx(FHandle, Result) then
    Result := -1;
end;

function THandleStream.Lock(Offset, Count: Int64): Boolean;
begin
  Result := LockFile(Handle, Int64Rec(Offset).Lo, Int64Rec(Offset).Hi,
    Int64Rec(Count).Lo, Int64Rec(Count).Hi);
end;

function THandleStream.Open(FileName: PWideChar; Access, Creation,
  Share, Attributes: LongWord): Boolean;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := CreateFileW(FileName, Access, Share, nil, Creation, Attributes, 0);
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

function THandleStream.Read(var Data; Count: LongWord): Cardinal;
begin
{$IFDEF Tricks} System. {$ENDIF}
  ReadFile(FHandle, Data, Count, Result, nil);
end;

function THandleStream.Seek(Offset: Int64; Origin: LongWord): Int64;
begin
  // SetFilePointerEx available since Windows 2000
  if not SetFilePointerEx(FHandle, Offset, @Result, Origin) then
    Result := -1;
end;

procedure THandleStream.SetPosition(Value: Int64);
begin
  SetFilePointerEx(FHandle, Value, nil, FILE_BEGIN);
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

function THandleStream.Unlock(Offset, Count: Int64): Boolean;
begin
  Result := UnlockFile(Handle, Int64Rec(Offset).Lo, Int64Rec(Offset).Hi,
    Int64Rec(Count).Lo, Int64Rec(Count).Hi);
end;

function THandleStream.Write(const Data; Count: LongWord): Cardinal;
begin
{$IFDEF Tricks} System. {$ENDIF}
  WriteFile(FHandle, Data, Count, Result, nil);
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
const
  sElipsis: array[0..2] of LegacyChar = '...';
var
  Dummy: array[0..$FF] of LegacyChar; // preventing flood
  BytesRead: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(sElipsis, Length(sElipsis), 0);
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
    WriteFile(FOutput, CRLF, SizeOf(CRLF), BytesWritten, nil);
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
const
  sElipsis: array[0..2] of WideChar = ('.', '.', '.');
var
  Dummy: array[0..$FF] of WideChar; // preventing flood
  Read: LongWord;
begin
  WriteLn;
  WriteLn(Prompt, Count, 0);
  WriteLn(sElipsis, Length(sElipsis), 0);
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
    WriteConsoleW(FOutput, @WideCRLF, Length(WideCRLF), Written, nil);
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

function TPerformanceCounter.ElapsedMilliseconds(StartValue: Int64): Double;
begin
  Result := GetValue - StartValue / FFrequency;
end;

function TPerformanceCounter.GetValue: Int64;
begin
  if not QueryPerformanceCounter(Result) then
    RaiseLastPlatformError;
end;

function TPerformanceCounter.MillisecondsBetween(Value1, Value2: Int64): Double;
begin
  Result := (Value2 - Value1) / FFrequency;
end;

end.

