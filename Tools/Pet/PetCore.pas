(*
    PE Tool core

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit PetCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses;

type
  TResourceName = record
    Length: Integer;
    Value: PCoreChar;
  end;

  PResourceNameArray = ^TResourceNameArray;
  TResourceNameArray = array[0..MaxInt div SizeOf(TResourceName) - 1] of TResourceName;

  TResourceNames = class(TArray)
  private
  { hold } FItems: PResourceNameArray;
  public
    class function ItemSize: Integer; override;
    property Items: PResourceNameArray read FItems;
  end;

  TSectionName = class
  private
    FLength: Integer;
    FValue: PLegacyChar;
  public
    destructor Destroy; override;

    property Length: Integer read FLength;
    property Value: PLegacyChar read FValue;
  end;

  PSectionNameArray = ^TSectionNameArray;
  TSectionNameArray = array[0..MaxInt div SizeOf(TSectionName) - 1] of TSectionName;

  TSectionNames = class(TObjects)
  private
  { hold } FItems: PSectionNameArray;
  public
    property Items: PSectionNameArray read FItems;
  end;

  TFileKey = (fkInto, fkBackup, fkStub, fkExtract, fkDump);
  TFileKeys = array[TFileKey] of PCoreChar;

  TRunOption = (roStrip, roDeep, roClean, roMainIcon, ro3GB, roPause);
  TRunOptions = set of TRunOption;

  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppName: PLegacyChar;
    FSourceFileName: PCoreChar;
    FFileNames: TFileKeys;
    FSectionNames: TSectionNames;
    FResourceNames: TResourceNames;
    FMajorVersion, FMinorVersion: Word;
    FOptions: TRunOptions;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

{ Exceptions }

  ECommandLine = class(Exception)
  end;

  EFileKey = class(ECommandLine)
  private
    FFileKey: TFileKey;
  public
    constructor Create(MissingFileName: TFileKey); overload;
    property FileKey: TFileKey read FFileKey;
  end;

  ECore = class(Exception);

implementation

uses
  Windows, CoreStrings, PetConsts, ExeImages;

const
  FileKeys: TFileKeys = (sInto, sBackup, sStub, sExtract, sDump);

{ EFileKey }

constructor EFileKey.Create(MissingFileName: TFileKey);
begin
  inherited Create(sMissingFileName, CP_CORE, [FileKeys[MissingFileName]]);
end;

{ TResourceNames }

class function TResourceNames.ItemSize: Integer;
begin
  Result := SizeOf(TResourceName); 
end;

{ TSectionName }

destructor TSectionName.Destroy;
begin
  FreeMem(FValue);
  inherited;
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PCoreChar);

var
  P: TWideParamRec;

function SameKey(Key: PCoreChar): Boolean;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Key, WideStrLen(Key),
    P.Param + 1, P.Length - 1) = CSTR_EQUAL;
end;

const
  OptionKeys: array[TRunOption] of PCoreChar = (sStrip, sDeep, sCleanVer, sMainIcon, s3GB, sPause);
var
  Dot, Ver, CmdLineParams, StubFileName, Limit, Head: PCoreChar;
  ExeName: array[0..MAX_PATH] of CoreChar;
  K: TFileKey;
  R: TRunOption;
  S: TSectionName;
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
    begin
      if FixedInfo.Flags * [verDebug, verPreRelease] <> [] then
        Ver := StringInfo(0, 'FileVersion')
      else
        Ver := StringInfo(0, 'ProductVersion');
      FConsole.WriteLn('%ws %ws  %ws', [StringInfo(0, 'ProductName'),
        Ver, StringInfo(0, 'LegalCopyright')], 2);
    end;
  finally
    Free;
  end;

  with WideParamStr(CommandLine) do
  begin
    Param[Length] := CoreChar(0); // unsafe
    StubFileName := Param;

    Head := WideStrRScan(Param, PathDelimiter, Length);
    if Head <> nil then
      Inc(Head)
    else
      Head := Param;

    Dot := WideStrScan(Head, '.', Length - (Head - Param));
    if Dot <> nil then
      Length := Dot - Head;
    FAppName := EncodeLegacy(Head, Length, CP_ACP);

    CommandLine := NextParam;
  end;

  CmdLineParams := CommandLine;

  repeat
    P := WideParamStr(CommandLine);
    if P.Length = 0 then
    begin
      if CommandLine = CmdLineParams then // no params
        Include(FOptions, roPause);
      Break;
    end;

    if (not P.Quoted) and ((P.Param^ = CoreChar('/')) or (P.Param^ = CoreChar('-'))) then
    begin
      for K := Low(FFileNames) to High(FFileNames) do
        if SameKey(FileKeys[K]) then
        begin
          P := WideParamStr(P.NextParam);
          if P.Length <> 0 then
          begin
            with P do
            begin
              Param[Length] := CoreChar(0); // unsafe
              FFileNames[K] := Param;
              Param := nil;
            end;
          end
          else if (FFileNames[K] = nil) and not (K in [fkInto, fkStub]) then
            raise EFileKey.Create(K)
          else
            P.Param := nil;
          Break;
        end;

      if P.Param <> nil then
        for R := Low(R) to High(R) do
          if SameKey(OptionKeys[R]) then
          begin
            Include(FOptions, R);
            P.Param := nil;
            Break;
          end;

      if P.Param <> nil then
        if SameKey(sOSVer) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
            if Length <> 0 then
            begin
              Limit := Param + Length;
              while (Word(Param^) and $FF00 = 0) and (LegacyChar(Param^) in ['0'..'9']) and (Param < Limit) do
              begin
                FMajorVersion := FMajorVersion * 10 + Byte(Param^) - Byte('0');
                Inc(Param);
              end;
              if Param^ = '.' then
              begin
                Inc(Param);
                while (Param < Limit) and (Word(Param^) and $FF00 = 0) and (LegacyChar(Param^) in ['0'..'9']) do
                begin
                  FMinorVersion := FMinorVersion * 10 + Byte(Param^) - Byte('0');
                  Inc(Param);
                end;
              end;
              Param := nil;
            end
            else
              raise ECommandLine.Create(sMissingOSVer);
        end
        else if SameKey(sDropSections) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
          begin
            Limit := Param + Length;
            while Param < Limit do
            begin
              while (Word(Param^) and $FF00 = 0) and (LegacyChar(Param^) in [' ', ',', ';']) and (Param < Limit) do
                Inc(Param);
              Head := Param;
              while (Param < Limit) and (Word(Param^) and $FF00 = 0) and not (LegacyChar(Param^) in [' ', ',', ';']) do
                Inc(Param);
              if FSectionNames = nil then
                FSectionNames := TSectionNames.Create(16, -2, True);
              S := TSectionName.Create;
              with S do
              begin
                FLength := Param - Head;
                FValue := EncodeLegacy(Head, FLength, CP_ACP);
              end;
              FSectionNames.Append(S);
            end;
            if FSectionNames = nil then
              raise ECommandLine.Create(sMissingSectionNames);
            Param := nil;
          end;
        end
        else if SameKey(sDropRes) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
          begin
            Limit := Param + Length;
            while Param < Limit do
            begin
              while (Word(Param^) and $FF00 = 0) and (LegacyChar(Param^) in [' ', ',', ';']) and (Param < Limit) do
                Inc(Param);
              Head := Param;
              while (Param < Limit) and (Word(Param^) and $FF00 = 0) and not (LegacyChar(Param^) in [' ', ',', ';']) do
                Inc(Param);
              if FResourceNames = nil then
                FResourceNames := TResourceNames.Create(16, -2);
              with FResourceNames, FItems[Append] do
              begin
                Length := Param - Head;
                Value := Head;
              end;
            end;
            if FSectionNames = nil then
              raise ECommandLine.Create(sMissingSectionNames);
            Param := nil;
          end;
        end;
    end;

    if P.Param <> nil then
      with P do
      begin
        Param[Length] := CoreChar(0); // unsafe
        FSourceFileName := Param;
      end;

    CommandLine := P.NextParam;
  until False;

  if FFileNames[fkInto] = nil then
    FFileNames[fkInto] := FSourceFileName;
  if FFileNames[fkStub] = nil then
    FFileNames[fkStub] := StubFileName; 
end;

destructor TApplication.Destroy;
begin
  FResourceNames.Free;
  FSectionNames.Free;
  FreeMem(FAppName);
  FConsole.Free;

  if roPause in FOptions then // placed here to show exceptions properly
    with TStreamConsole.Create(True) do
    try
      ReadLn(sPressEnterToExit);
    finally
      Free;
    end;
end;

procedure TApplication.Run;
const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
var
  I, Idx: Integer;
  Image: TExeImage;
begin
  if FSourceFileName <> nil then
  begin
    Image := TExeImage.Create(IMAGE_NUMBEROF_DIRECTORY_ENTRIES, 0, True);
    try
      Image.Load(FSourceFileName);

      if FFileNames[fkExtract] <> nil then
      begin
        with TExeStub.Create do
        try
          Load(Image.Stub);
          Strip(roStrip in FOptions);
          Save(FFileNames[fkExtract]);
          FConsole.WriteLn(sExtractingStub, [FFileNames[fkExtract], Size]);
        finally
          Free;
        end;
      end;

      if FFileNames[fkStub] <> nil then
        Image.Stub.Load(FFileNames[fkStub]);

      if roStrip in FOptions then
        Image.Strip([soStub..soEmptySections] + Deep[roDeep in FOptions])
      else
        Image.Stub.Strip(False);

      if FSectionNames <> nil then
        for I := 0 to FSectionNames.Count - 1 do
        begin
          with FSectionNames.Items[I] do
            Idx := Image.IndexOfSection(Value, Length);
          if Idx >= 0 then
            Image.Extract(Idx).Free;
        end;

      if (FResourceNames <> nil) or (FOptions * [roClean, roMainIcon] <> []) then
      begin
        Idx := Image.IndexOfSection(IMAGE_DIRECTORY_ENTRY_RESOURCE);
        if Idx >= 0 then
          with TExeResources.Create(Image.Sections[Idx]) do
          try
            Load(Image.Sections[Idx]);
            Save(Image.Sections[Idx]);
          finally
            Free;
          end;

        if roClean in FOptions then
        begin
        end;

        if roMainIcon in FOptions then
        begin
        end;  
      end;

      if FFileNames[fkInto] <> nil then
      begin
        if (FFileNames[fkBackup] <> nil) and (not MoveFileExW(FSourceFileName,
          FFileNames[fkBackup], MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH))
        then
          RaiseLastPlatformError;
        with Image do
        begin
          if FMajorVersion <> 0 then
            with Image.Headers.OptionalHeader do
            begin
              MajorOSVersion := FMajorVersion;
              MinorOSVersion := FMinorVersion;
              MajorSubsystemVersion := FMajorVersion;
              MinorSubsystemVersion := FMinorVersion;
            end;
          if ro3GB in FOptions then
            with Image.Headers.FileHeader do
              Characteristics := Characteristics or IMAGE_FILE_LARGE_ADDRESS_AWARE;
          Build;
          Save(FFileNames[fkInto]);
          FConsole.WriteLn(sWritingInto, [FFileNames[fkInto], Size]);
        end;
      end;
    finally
      Image.Free;
    end;
  end
  else
    FConsole.WriteLn(sUsage, [FAppName]);
end;

end.

