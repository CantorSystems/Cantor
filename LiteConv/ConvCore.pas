(*
    LiteConv core engine

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

unit ConvCore;

interface

uses
  CoreUtils, CoreWrappers, CoreClasses, CoreStrings, CoreExceptions;

type
  TCodePageArg = (cpAuto, cpOEM, cpCESU8, cpUTF16LE, cpUTF16BE, cpUTF32LE, cpUTF32BE);
const
  cpUTF16 = cpUTF16LE; // platform ??
  cpUTF32 = cpUTF32LE;

type
  TFileArgs = class;

  TFileArg = class(TListItem)
  private
  { hold } FOwner: TFileArgs;
  { hold } FPrior, FNext: TFileArg;
    FFileName: TCoreString;
    FCodePage: Word;
  public
    constructor Create(FileName: PCoreChar; Count: Integer); overload;
    destructor Destroy; override; 

    property CodePage: Word read FCodePage write FCodePage;
    property FileName: TCoreString read FFileName;
    property Next: TFileArg read FNext;
    property Owner: TFileArgs read FOwner;
    property Prior: TFileArg read FPrior;
  end;

  TFileArgs = class(TList)
  private
  { hold } FFirst, FLast: TFileArg;
  public
    property First: TFileArg read FFirst;
    property Last: TFileArg read FLast;
  end;

  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppName: PCoreChar;
    FFileArgs: TFileArgs;
    FInto: TFileArg;
    FPause: Boolean;
    procedure Warning(Msg: PLegacyChar); overload;
    procedure Warning(Msg: PLegacyChar; const Args: array of const); overload;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

  ECommandLine = class(Exception)
  private
    FInvalidArg: PCoreChar;
  public
    constructor Create(Msg: PLegacyChar; Arg: PCoreChar);
    property InvalidArg: PCoreChar read FInvalidArg;
  end;

  ECore = class(Exception);

implementation

uses
  Windows, ConvConsts;

type
  TLineBuf = array[0..80] of LegacyChar; // including #0

{ ECommandLine }

constructor ECommandLine.Create(Msg: PLegacyChar; Arg: PCoreChar);
begin
  inherited Create(Msg, CP_CORE, [Arg]);
end;

{ TFileArg }

constructor TFileArg.Create(FileName: PCoreChar; Count: Integer);
begin
  FFileName := TCoreString.Create;
  FFileName.Insert(FileName, Count, [soAttachBuffer]); // soAttachBuffer unsafe for other environment
end;

destructor TFileArg.Destroy;
begin
  FFileName.Free;
  inherited;
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PCoreChar);

function SameKey(const P: TWideParamRec; Key: PWideChar): Boolean;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Key, WideStrLen(Key),
    P.Param + 1, P.Length - 1) = CSTR_EQUAL;
end;

function ToCodePage(const P: TWideParamRec): Word;
var
  Limit, W: PCoreChar;
begin
  Result := 0;
  with P do
  begin
    Limit := Param + Length;
    W := Param;
  end;
  while (W < Limit) and (W^ >= CoreChar('0')) and (W^ <= CoreChar('9')) do
  begin
    Result := Result * 10 + Word(W^) - Word('0');
    Inc(W);
  end;
  if W <> Limit then
  begin
    Limit^ := CoreChar(0); // unsafe;
    raise ECommandLine.Create(sInvalidCodePage, P.Param);
  end;
end;

var
  Dot: PCoreChar;
  P: TWideParamRec;
  ExeName: array[0..MAX_PATH] of CoreChar;
  Arg: TFileArg;
  CP: Word;
begin
  FConsole := TStreamConsole.Create;
  with FConsole do
  begin
    CodePage := GetACP;
    WriteLn;
  end;

  if GetModuleFileNameW(0, ExeName, Length(ExeName)) = 0 then
    RaiseLastPlatformError;
  with TVersionInfo.Create(ExeName) do
  try
    if TranslationCount <> 0 then
      FConsole.WriteLn('%ws %ws  %ws', [StringInfo(0, 'ProductName'),
        StringInfo(0, 'ProductVersion'), StringInfo(0, 'LegalCopyright')], 2);
  finally
    Free;
  end;

  with WideParamStr(CommandLine) do
  begin
    FAppName := WideStrRScan(Param, PathDelimiter, Length);
    if FAppName <> nil then
      Inc(FAppName)
    else
      FAppName := Param;

    Dot := WideStrScan(FAppName, '.', Length - (PLegacyChar(FAppName) - PLegacyChar(Param)) div SizeOf(CoreChar));
    if Dot <> nil then
      Dot^ := CoreChar(0) // unsafe
    else
      Param[Length] := CoreChar(0); // unsafe

    CommandLine := NextParam;
  end;

  FFileArgs := TFileArgs.Create;

  repeat
    P := WideParamStr(CommandLine);
    if P.Length = 0 then
      Break;

    CP := 0;

    if (not P.Quoted) and ((P.Param^ = CoreChar('/')) or (P.Param^ = CoreChar('-'))) then
    begin
      if SameKey(P, sCodePage) then
      begin
        P := WideParamStr(P.NextParam);
        if P.Length = 0 then
          raise ECommandLine.Create(sMissingCodePage, nil);
        if FInto <> nil then
        begin
          FInto.CodePage := ToCodePage(P);
          P.Param := nil;
        end
        else
          CP := ToCodePage(P);
      end
      else if SameKey(P, sOEM) then
        CP := Word(cpOEM)
      else if SameKey(P, sInto) then
      begin
        FInto := TFileArg.Create;
        P.Param := nil;
      end
      else if SameKey(P, sPause) then
      begin
        FPause := True;
        P.Param := nil;
      end
      else
        if FInto <> nil then
        begin
          if SameKey(P, sCESU8) then
          begin
            FInto.CodePage := Word(cpCESU8);
            P.Param := nil;
          end
          else if SameKey(P, sUTF16) then
          begin
            FInto.CodePage := Word(cpUTF16);
            P.Param := nil;
          end
          else if SameKey(P, sUTF32) then
          begin
            FInto.CodePage := Word(cpUTF32);
            P.Param := nil;
          end
          else if SameKey(P, sBigEndian) and (TCodePageArg(FInto.CodePage) in [cpUTF16, cpUTF32]) then
          begin
            Inc(FInto.FCodePage);
            P.Param := nil;
          end;
        end;
    end;

    if CP <> 0 then
    begin
      Arg := FFileArgs.Last;
      while (Arg <> nil) and (Arg.CodePage = 0) do
      begin
        Arg.CodePage := CP;
        Arg := Arg.Prior;
      end;
    end
    else if P.Param <> nil then
    begin
      with P do
        Param[Length] := CoreChar(0); // unsafe;

      if FInto <> nil then
        if FInto.FFileName = nil then
          FInto := TFileArg.Create(P.Param, P.Length)
        else
          FInto.FileName.Insert(P.Param, P.Length, [soAttachBuffer])
      else
        FFileArgs.Append(TFileArg.Create(P.Param, P.Length));
    end;

    CommandLine := P.NextParam;
  until False;
end;

destructor TApplication.Destroy;
begin
  FInto.Free;
  FFileArgs.Free;
  if FPause then // placed here to show exceptions properly
    FConsole.ReadLn(sPressEnterToExit);
  FConsole.Free;
end;

procedure TApplication.Warning(Msg: PLegacyChar);
begin
  FConsole.WriteLn(sWarning, [Msg]);
end;

procedure TApplication.Warning(Msg: PLegacyChar; const Args: array of const);
var
  Buf: TLineBuf;
begin
  FormatBuf(Msg, Args, Buf);
  Warning(Buf);
end;

procedure TApplication.Run;

var
  CodePages: TCodePages;

procedure ConvertFile(Source: TFileArg);
var
  Text, DestFileName: TCoreString;
  CP: TCodePage;
begin
  CP := CodePages[Source.CodePage];

end;

var
  Arg: TFileArg;
begin
  CodePages := TCodePages.Create(@PlatformCodePage);
  try
    if FFileArgs.First <> nil then
    begin
      Arg := FFileArgs.First;
      while Arg <> nil do
      begin
        ConvertFile(Arg);
        Arg := Arg.Next;
      end;
    end
    else if (FInto <> nil) and (FInto.FFileName <> nil) then
      ConvertFile(FInto)
    else
      FConsole.WriteLn(sUsage, [FAppName, GetACP, GetOEMCP]);
  finally
    CodePages.Free;
  end;
end;

end.

