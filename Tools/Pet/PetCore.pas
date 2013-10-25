(*
    PE Tool core

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit PetCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers;

type
  TFileKey = (fkInto, fkBackup, fkStub, fkExtract, fkDump);
  TFileKeys = array[TFileKey] of PCoreChar;

  TRunOption = (roStrip, ro3GB, roPause);
  TRunOptions = set of TRunOption;

  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppName, FSourceFileName: PCoreChar;
    FFileNames: TFileKeys;
    FMajorVersion, FMinorVersion: Word;
    FOptions: TRunOptions;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

{ Exceptions }

  ECommandLine = Exception;

  EFileName = class(ECommandLine)
  private
    FFileName: PCoreChar;
  public
    constructor Create(Value: PCoreChar); overload;
    property FileName: PCoreChar read FFileName;
  end;

  EFileKey = class(EFileName)
  private
    FFileKey: TFileKey;
  public
    constructor Create(MissingFileName: TFileKey); overload;
    constructor Create(DuplicateFileName: TFileKey; Value: PCoreChar); overload;
    property FileKey: TFileKey read FFileKey;
  end;

  ECore = class(Exception);

implementation

uses
  Windows, CoreClasses, CoreStrings, PetConsts, ExeImages;

const
  FileKeys: TFileKeys = (sInto, sBackup, sStub, sExtract, sDump);

{ EFileName }

constructor EFileName.Create(Value: PCoreChar);
begin
  inherited Create(sDuplicateSourceFileName, CP_CORE, [Value]);
end;

{ EFileKey }

constructor EFileKey.Create(MissingFileName: TFileKey);
begin
  inherited Create(sMissingFileName, CP_CORE, [FileKeys[MissingFileName]]);
end;

constructor EFileKey.Create(DuplicateFileName: TFileKey; Value: PCoreChar);
begin
  if Value <> nil then
    inherited Create(sDuplicateFileName, CP_CORE, [FileKeys[DuplicateFileName], Value])
  else
    inherited Create(sDuplicateFileKey, CP_CORE, [FileKeys[DuplicateFileName]])
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
  OptionKeys: array[TRunOption] of PCoreChar = (sStrip, s3GB, sPause);
var
  Dot, Ver, CmdLineParams, Limit: PCoreChar;
  ExeName: array[0..MAX_PATH] of CoreChar;
  K: TFileKey;
  R: TRunOption;
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
              Param[Length] := CoreChar(0); // unsafe
            if FFileNames[K] = nil then
            begin
              FFileNames[K] := P.Param;
              P.Param := nil;
              Break;
            end
            else
              raise EFileKey.Create(K, P.Param);
          end
          else if FFileNames[K] <> nil then
            raise EFileKey.Create(K, nil)
          else
            raise EFileKey.Create(K);
        end;

      if P.Param <> nil then
        for R := Low(R) to High(R) do
          if SameKey(OptionKeys[R]) then
          begin
            Include(FOptions, R);
            P.Param := nil;
            Break;
          end;

      if (P.Param <> nil) and SameKey(sOSVer) then
      begin
        P := WideParamStr(P.NextParam);
        with P do
          if Length <> 0 then
          begin
            Limit := Param + Length;
            while (LegacyChar(Param^) in ['0'..'9']) and (Param < Limit) do
            begin
              FMajorVersion := FMajorVersion * 10 + Byte(Param^) - Byte('0');
              Inc(Param);
            end;
            if Param^ = '.' then
            begin
              Inc(Param);
              while (Param < Limit) and (LegacyChar(Param^) in ['0'..'9']) do
              begin
                FMinorVersion := FMinorVersion * 10 + Byte(Param^) - Byte('0');
                Inc(Param);
              end;
            end;
            Param := nil;
          end
          else
            raise ECommandLine.Create(sMissingOSVer);
      end;
    end;

    if P.Param <> nil then
    begin
      with P do
        Param[Length] := CoreChar(0); // unsafe
      if FSourceFileName = nil then
        FSourceFileName := P.Param
      else
        raise EFileName.Create(P.Param);
    end;

    CommandLine := P.NextParam;
  until False;

  if (FFileNames[fkInto] = nil) and (FFileNames[fkBackup] <> nil) then
    FFileNames[fkInto] := FSourceFileName;
end;

destructor TApplication.Destroy;
begin
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
var
  Image: TExeImage;
begin
  if FSourceFileName <> nil then
  begin
    Image := TExeImage.Create(IMAGE_NUMBEROF_DIRECTORY_ENTRIES, 0);
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
        Image.Strip
      else
        Image.Stub.Strip(False);

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

