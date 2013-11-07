(*
    PE Tool core

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit PetCore;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses;

type
  TLocaleMapRec = packed record // satisfying TArray.ItemSize
    LocaleFrom, LocaleTo: Word;
  end;

  PLocaleMapArray = ^TLocaleMapArray;
  TLocaleMapArray = array[0..MaxInt div SizeOf(TLocaleMapRec) - 1] of TLocaleMapRec;

  TLocaleMap = class(TArray)
  private
  { hold } FItems: PLocaleMapArray;
  public
    property Items: PLocaleMapArray read FItems;
  end;

  TResourceNames = class(TWideStringArray)
  public
    procedure Append(Source: PCoreChar; Count: Integer);
  end;

  TSectionNames = class(TLegacyStringArray)
  public
    procedure Append(Source: PCoreChar; Count: Integer);
  end;

  TFileKey = (fkInto, fkBackup, fkStub, fkExtract, fkDump);
  TFileKeys = array[TFileKey] of PCoreChar;

  TRunOption = (roStrip, roDeep, roCleanVer, roMainIcon, ro3GB, roPause);
  TRunOptions = set of TRunOption;

  TMenuetKolibri = (mkNone, mkMenuet, mkKolibri);

  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppName: PLegacyChar;
    FSourceFileName: PCoreChar;
    FFileNames: TFileKeys;
    FDropSections: TSectionNames;
    FDropResources: TResourceNames;
    FLocaleMap: TLocaleMap;
    FImageBase: CoreInt;
    FMajorVersion, FMinorVersion: Word;
    FOptions: TRunOptions;
//    FMenuetKolibri: TMenuetKolibri;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

{ Exceptions }

  ECommandLine = class(Exception);

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
  Windows, PetConsts, ExeImages;

const
  FileKeys: TFileKeys = (sInto, sBackup, sStub, sExtract, sDump);

{ EFileKey }

constructor EFileKey.Create(MissingFileName: TFileKey);
begin
  inherited Create(sMissingFileName, CP_CORE, [FileKeys[MissingFileName]]);
end;

{ TResourceNames }

procedure TResourceNames.Append(Source: PCoreChar; Count: Integer);
begin
  with Items[inherited Append] do
  begin
    Value := Source;
    Length := Count;
  end;
end;

{ TSectionNames }

procedure TSectionNames.Append(Source: PCoreChar; Count: Integer);
begin
  Items[inherited Append] := EncodeLegacy(Source, Count, CP_ACP);
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
  HexBase: array[Boolean] of LegacyChar = 'A0';
var
  ExeName: array[0..MAX_PATH] of CoreChar;
  Ver: TVersionBuffer;
  Dot, CmdLineParams, StubFileName, Limit, Head, Current: PCoreChar;
  K: TFileKey;
  R: TRunOption;
  Value: Word;
//  Nibble: Byte;
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
    FormatVersion(Ver);
    if TranslationCount <> 0 then
      FConsole.WriteLn('%ws %s  %ws', [StringInfo(0, 'ProductName'), Ver,
        StringInfo(0, 'LegalCopyright')], 2);
  finally
    Free;
  end;

  FImageBase := -1;

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
    FAppName := EncodeLegacy(Head, Length, CP_ACP).Value;

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
          if P.Length <> 0 then
          begin
            with P do
            begin
              Current := Param;
              Limit := Current + Length;
            end;
            while (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in ['0'..'9']) and (Current < Limit) do
            begin
              FMajorVersion := FMajorVersion * 10 + PByte(Current)^ - Byte('0');
              Inc(Current);
            end;
            if Current^ = '.' then
            begin
              Inc(Current);
              while (Current < Limit) and (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in ['0'..'9']) do
              begin
                FMinorVersion := FMinorVersion * 10 + PByte(Current)^ - Byte('0');
                Inc(Current);
              end;
            end;
            P.Param := nil;
          end
          else
            raise ECommandLine.Create(sMissingParam, [sOSVersion]);
        end
        else if SameKey(sDropSections) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
          begin
            Current := Param;
            Limit := Current + Length;
          end;
          while Current < Limit do
          begin
            while (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in [' ', ',', ';']) and (Current < Limit) do
              Inc(Current);
            Head := Current;
            while (Current < Limit) and (PWord(Current)^ and $FF00 = 0) and not (PLegacyChar(Current)^ in [' ', ',', ';']) do
              Inc(Current);
            if FDropSections = nil then
              FDropSections := TSectionNames.Create(IMAGE_NUMBEROF_DIRECTORY_ENTRIES, -2, True);
            FDropSections.Append(Head, Current - Head);
          end;
          if FDropSections = nil then
            raise ECommandLine.Create(sMissingParam, [sSectionNames]);
          P.Param := nil;
        end
        else if SameKey(sDropRes) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
          begin
            Current := Param;
            Limit := Current + Length;
          end;
          while Current < Limit do
          begin
            while (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in [' ', ',', ';']) and (Current < Limit) do
              Inc(Current);
            Head := Current;
            while (Current < Limit) and (PWord(Current)^ and $FF00 = 0) and not (PLegacyChar(Current)^ in [' ', ',', ';']) do
              Inc(Current);
            if FDropResources = nil then
              FDropResources := TResourceNames.Create(16, -2); // why not 16?
            FDropResources.Append(Head, Current - Head);
          end;
          if FDropResources = nil then
            raise ECommandLine.Create(sMissingParam, [sResourceNames]);
          P.Param := nil;
        end
        else if SameKey(sLocale) then
        begin
          P := WideParamStr(P.NextParam);
          with P do
          begin
            Current := Param;
            Limit := Current + Length;
          end;
          while Current < Limit do
          begin
            while (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in [' ', ',', ';']) and (Current < Limit) do
              Inc(Current);
            Value := 0;
            while (Current < Limit) and (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in ['0'..'9']) do
            begin
              Value := Value * 10 + PByte(Current)^ - Byte('0');
              Inc(Current);
            end;
            if Value <> 0 then
            begin
              if FLocaleMap = nil then
                FLocaleMap := TLocaleMap.Create(8, -2); // why not 8?
              with FLocaleMap do
                LongWord(FItems[Append]) := Value; // Fast core
              if PLegacyChar(Current)^ = '=' then
              begin
                Inc(Current);
                Value := 0;
                while (Current < Limit) and (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in ['0'..'9']) do
                begin
                  Value := Value * 10 + PByte(Current)^ - Byte('0');
                  Inc(Current);
                end;
                with FLocaleMap do
                  FItems[Count - 1].LocaleTo := Value;
              end;
            end;
            if FLocaleMap = nil then
              raise ECommandLine.Create(sMissingParam, [sLocaleMap]);
            P.Param := nil;
          end;
        {end
        else if SameKey(sRebase) then
        begin
          P := WideParamStr(P.NextParam);
          if P.Length <> 0 then
          begin
            with P do
            begin
              Current := Param;
              Limit := Current + Length;
            end;
            FImageBase := 0;
            while (PWord(Current)^ and $FF00 = 0) and (PLegacyChar(Current)^ in ['0'..'9', 'A'..'F', 'a'..'f']) and
              (Current < Limit) do
            begin
              if PLegacyChar(Current)^ in ['0'..'9'] then
                Nibble := PByte(Current)^ - Byte('0')
              else
                Nibble := PByte(Current)^ and not $20 - Byte('A') + 10;
              FImageBase := FImageBase * 16 + Nibble;
              Inc(Current);
            end;
            P.Param := nil;
          end
          else
            raise ECommandLine.Create(sMissingParam, [sImageBase]);
        end
        else if SameKey(sMenuet) then
        begin
          FMenuetKolibri := mkMenuet;
          if FImageBase < 0 then
            FImageBase := 8;
          P.Param := nil;}
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
  FLocaleMap.Free;
  FDropResources.Free;
  FDropSections.Free;
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

      if FDropSections <> nil then
        for I := 0 to FDropSections.Count - 1 do
        begin
          with FDropSections.Items[I] do
            Idx := Image.IndexOfSection(Value, Length);
          if Idx >= 0 then
            Image.Extract(Idx).Free;
        end;

      if (FDropResources <> nil) or (FLocaleMap <> nil) or (FOptions * [roCleanVer, roMainIcon] <> []) then
      begin
        Idx := Image.IndexOfSection(IMAGE_DIRECTORY_ENTRY_RESOURCE);
        if Idx >= 0 then
        begin
          Image.Sections[Idx].Handler := TExeResources.Create(True);

          if FDropResources <> nil then
            for I := 0 to FDropResources.Count - 1 do
            begin
            end;

          if roCleanVer in FOptions then
          begin
          end;

          if roMainIcon in FOptions then
          begin
          end;

          if FLocaleMap <> nil then
            for I := 0 to FLocaleMap.Count - 1 do
            begin
            end;
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
          Save(FFileNames[fkInto], roStrip in FOptions);
          FConsole.WriteLn(sWritingInto, [FFileNames[fkInto], Size(roStrip in FOptions)]);
        end;
      end;
    finally
      Image.Free;
    end;
  end
  else
    with FConsole do
    begin
      WriteLn(sUsage, [FAppName], 0); // wvsprintf limits length to 1024 characters :-(
      WriteLn(sHelp);
    end;
end;

end.

