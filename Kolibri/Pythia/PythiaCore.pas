(*
    Pythia -- Delphi to FASM preprocessor for Kolibri OS

    Core and command line interface implementation

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit PythiaCore;

interface

uses
  CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

type
  TInputFile = (ifInt, ifInto, ifMap, ifSource);

  TApplication = class
  private
    FACP, FConsoleCP: TCodePage;
    FConsole: TStreamConsole;
    FAppFileName: TCoreString;
    FInputFileNames: array[TInputFile] of TCoreString;
    FPause: Boolean;
    procedure Help;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

  TMapFile = class;

  TMapFileItem = class(TRedBlackTreeItem)
  private
  { hold } FOwner: TMapFile;
  { hold } FParent, FLeft, FRight: TMapFileItem;
  { hold } FRed: Boolean;
    FFuncName: TLegacyString;
  public
    constructor Create(FuncName: PLegacyChar; Count: Integer);
    property FuncName: TLegacyString read FFuncName;
  end;

  TMapFile = class(TRedBlackTree)
  private
  { hold } FRoot: TMapFileItem;
  public
    constructor Create(FileName: PCoreChar);
    property Root: TMapFileItem read FRoot;
  end;

implementation

uses
  Windows, PythiaConsts;

{ TApplication }

constructor TApplication.Create(CommandLine: PCoreChar);

var
  InputFile: TInputFile;

procedure AssignFileName(Value: PCoreChar; Count: Integer);
begin
  if FInputFileNames[InputFile] <> nil then
  begin
    FInputFileNames[InputFile] := TCoreString.Create;
    FInputFileNames[InputFile].Insert(Value, Count, [soAttachBuffer]);
    InputFile := ifSource;
  end;
end;

function SameKey(Key, Param: PWideChar; Count: Integer): Boolean;
const
  CSTR_EQUAL = 2;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Key, WideStrLen(Key), Param + 1, Count - 1) = CSTR_EQUAL;
end;

var
  CP: Word;
begin
  FACP := PlatformCodePage(CP_ACP);

  FConsole := TStreamConsole.Create;
  CP := FConsole.CodePage;

  if CP <> FACP.Number then
    FConsoleCP := PlatformCodePage(CP)
  else
    FConsoleCP := FACP;

  FAppFileName := TCoreString.Create;
  with WideParamStr(CommandLine) do
  begin
    FAppFileName.Insert(Param, Length, [soAttachBuffer]);
    CommandLine := NextParam;
  end;

  InputFile := ifSource;
  repeat
    with WideParamStr(CommandLine) do
      if Length <> 0 then
      begin
        if Quoted then
          AssignFileName(Param, Length)
        else
          case Param^ of
            CoreChar('/'), CoreChar('-'):
              if SameKey(sInto, Param, Length) then
                InputFile := ifInto
              else if SameKey(sMap, Param, Length) then
                InputFile := ifMap
              else if SameKey(sSrc, Param, Length) then
                InputFile := ifSource
              else if SameKey(sPause, Param, Length) then
                FPause := True;
          else
            AssignFileName(Param, Length);
          end;
        CommandLine := NextParam;
      end
      else
        Break;
  until False;
end;

destructor TApplication.Destroy;
var
  I: TInputFile;
begin
  for I := Low(TInputFile) to High(TInputFile) do
    FInputFileNames[I].Free;

  FAppFileName.Free;

  if FConsoleCP <> FACP then
    FConsoleCP.Free;

  FACP.Free;
end;

procedure TApplication.Help;
var
  AppName, Dot: PCoreChar;
  Buf: array[0..MAX_PATH + Length(sUsage) - 1] of LegacyChar;
begin
  with FAppFileName do
  begin
    AppName := WideStrRScan(Data + Count, PathDelimiter, Count);
    if AppName <> nil then
      Inc(AppName)
    else
      AppName := Data;
    Dot := WideStrScan(AppName, '.', Count - (PLegacyChar(AppName) - PLegacyChar(Data)) div SizeOf(CoreChar));
  end;
  if Dot <> nil then
    Dot^ := CoreChar(0);
  FConsole.WriteLn(Buf, FormatBuf(sUsage, [AppName], Buf), 2);
end;

procedure TApplication.Run;
var
  Buf: array[Byte] of LegacyChar;
begin
  FConsole.WriteLn(@Buf, FormatBuf(sTitle, [sVersion], Buf), 2);
  try
    if (FInputFileNames[ifInt] <> nil) and (FInputFileNames[ifInto] <> nil) then
    else
      Help;
  finally
    if FPause then
      FConsole.ReadLn(sPressEnterToExit);
  end;
end;

{ TMapFileItem }

constructor TMapFileItem.Create(FuncName: PLegacyChar; Count: Integer);
begin
  FFuncName := TLegacyString.Create;
  FFuncName.Insert(FuncName, Count);
end;

{ TMapFile }

constructor TMapFile.Create(FileName: PCoreChar);
begin

end;

end.

