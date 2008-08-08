(*
    The Unified Environment, legacy Win32 core

    Legacy console input-output implementation

    Copyright (c) 2007-2009 The Unified Environment Laboratory
*)

unit ConsoleIO;

interface

uses
  Windows, Strings, Storage;

const
  STD_INPUT_HANDLE            = Windows.STD_INPUT_HANDLE;
  STD_OUTPUT_HANDLE           = Windows.STD_OUTPUT_HANDLE;
  STD_ERROR_HANDLE            = Windows.STD_ERROR_HANDLE;

type
  TConsoleStream = THandleStream;

  TConsole = class(TObject)
  private
    FInput, FOutput: THandle;
    function GetCodePage: Cardinal;
    procedure SetCodePage(Value: Cardinal);
  public
    constructor Create;
    procedure ReadLn(Prompt: PAnsiChar; ByteCount: Integer;
      LineBreaks: Integer = 1); overload;
    procedure ReadLn(Prompt: TUniString; LineBreaks: Integer = 1); overload;
    procedure WriteLn(LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: PAnsiChar; ByteCount: Integer;
      LineBreaks: Integer = 1); overload;
    procedure WriteLn(Text: TUniString; LineBreaks: Integer = 1); overload;
  // properties
    property CodePage: Cardinal read GetCodePage write SetCodePage;
    property Input: THandle read FInput;
    property Output: THandle read FOutput;
  end;

implementation

{ TConsole }

constructor TConsole.Create;
begin
  FInput := GetStdHandle(STD_INPUT_HANDLE);
  FOutput := GetStdHandle(STD_OUTPUT_HANDLE);
end;

function TConsole.GetCodePage: Cardinal;
begin
  Result := GetConsoleOutputCP;
end;

const
  sElipsis: array[0..2] of Char = '...';

procedure TConsole.ReadLn(Prompt: PAnsiChar; ByteCount, LineBreaks: Integer);
var
  Dummy: array[0..$FF] of AnsiChar; // preventing flood
  BytesRead: Cardinal;
begin
  WriteLn;
  WriteLn(Prompt, ByteCount, 0);
  WriteLn(sElipsis, Length(sElipsis), 0);
{$IFDEF Tricks} System. {$ENDIF}  
  ReadFile(FInput, Dummy, SizeOf(Dummy), BytesRead, nil);
  WriteLn(LineBreaks - 1);
end;

procedure TConsole.ReadLn(Prompt: TUniString; LineBreaks: Integer);
var
  CodePage: Cardinal;
  P: PAnsiChar;
  L: Integer;
begin
  if Prompt <> nil then
  begin
    CodePage := GetConsoleOutputCP;
    if Prompt.CodePage[True] = CodePage then
      ReadLn(Prompt.Data, Prompt.Length, LineBreaks)
    else
    begin
      L := Prompt.GetData(nil, 0, CodePage);
      if L <> 0 then
      begin
        GetMem(P, L);
        try
          Prompt.GetData(P, L, CodePage);
          ReadLn(P, L, LineBreaks);
        finally
          FreeMem(P);
        end;
      end;
    end;
  end;
end;

procedure TConsole.SetCodePage(Value: Cardinal);
begin
  SetConsoleCP(Value);
  SetConsoleOutputCP(Value);
end;

procedure TConsole.WriteLn(LineBreaks: Integer);
const
  CRLF: array[0..1] of Char = sLineBreak;
var
  I: Integer;
  BytesWritten: Cardinal;
begin
  for I := 0 to LineBreaks - 1 do
    WriteFile(FOutput, CRLF, Length(CRLF), BytesWritten, nil);
end;

procedure TConsole.WriteLn(Text: PAnsiChar; ByteCount, LineBreaks: Integer);
var
  BytesWritten: Cardinal;
begin
  WriteFile(FOutput, Text^, ByteCount, BytesWritten, nil);
  WriteLn(LineBreaks);
end;

procedure TConsole.WriteLn(Text: TUniString; LineBreaks: Integer);
begin
  if Text <> nil then
    WriteLn(Text.AsChar[GetConsoleOutputCP], Text.Length, LineBreaks)
  else
    WriteLn(LineBreaks);
end;

end.
