(*
    PE Tool's core

    Copyright (c) 2013-2015 Vladislav Javadov (aka Freeman)
*)

unit PetCore;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
//  TResourceNames = TWideStringArray;
  TSectionNames = TLegacyStrings;

  TFileKey = (fkInto, fkBackup, fkStub, fkExtract, fkDump);
  TFileKeys = array[TFileKey] of PCoreChar;

  TRunOption = (roStrip, roTrunc, roDeep, {roMiniRes, roCleanVer, roMainIcon,} ro3GB, roPause);
  TRunOptions = set of TRunOption;

  TMenuetKolibri = (mkNone, mkMenuet, mkKolibri);

  TApplication = object
  private
    FConsole: TStreamConsole;
    FAppName, FSourceFileName: TCoreString;
    FFileNames: TFileKeys;
    FDropSections: TSectionNames;
    FImageBase: CoreInt;
    FMajorVersion, FMinorVersion: Word;
    FOptions: TRunOptions;
  {$IFDEF Kolibri}
    FMenuetKolibri: TMenuetKolibri;
  {$ENDIF}
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; 
    procedure Pause;
    procedure Run;
    procedure Help;
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
  ExeImages, CoreConsts, PetConsts;

const
  FileKeys: TFileKeys = (sInto, sBackup, sStub, sExtract, sDump);

{ EFileKey }

constructor EFileKey.Create(MissingFileName: TFileKey);
begin
  inherited Create(sMissingFileName, CP_CORE, [FileKeys[MissingFileName]]);
end;

{ TApplication }

constructor TApplication.Create(CommandLine: PWideChar);

const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sStrip, sTrunc, sDeep, {sMiniRes, sCleanVer, sMainIcon,} s3GB, sPause);
  HexBase: array[Boolean] of LegacyChar = 'A0';
var
  ExeName: array[0..MAX_PATH] of CoreChar;
  VerInfo: TVersionInfo;
  Ver: TVersionBuffer;
  CmdLine, Key: TWideString;
  Param: TCommandLineParam;
  ExeNameLength, ParamCount: Integer;

  K: TFileKey;
  R: TRunOption;
  Value: Word;
//  Nibble: Byte;
begin
  with FConsole do
  begin
    Create;
    CodePage := CP_UTF8;
    WriteLn;
  end;

  ExeNameLength := GetModuleFileNameW(0, ExeName, Length(ExeName));
  if ExeNameLength = 0 then
    RaiseLastPlatformError {$IFDEF Debug} (sModuleFileName, Length(ExeName)) {$ENDIF} ;

  with VerInfo do
  begin
    Create(ExeName);
    try
      FormatVersion(Ver, sVersionAndRevision);
      if TranslationCount <> 0 then
        FConsole.WriteLn(sTitle, 0, [StringInfo(0, 'ProductName'), Ver,
          StringInfo(0, 'LegalCopyright')], 2);
    finally
      Destroy;
    end;
  end;

  with FAppName do
  begin
    Create;
    AsWideString(ExeName, ExeNameLength, soAttach);
    Skip(LastIndex(PathDelimiter) + 1);
    Truncate(Count - LastIndex(WideChar('.')));
    Detach;
  end;

  FImageBase := -1;

  CmdLine.Create;
  CmdLine.AsWideString(CommandLine, WideStrLen(CommandLine), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name

(*  repeat
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
            if K = fkBackup then
              FFileNames[fkInto] := FSourceFileName;
          end
          else if FFileNames[K] = nil then
          begin
            case K of
              fkInto:
                FFileNames[fkInto] := FSourceFileName;
              fkStub:
                FFileNames[fkStub] := FExeName;
            else
              raise EFileKey.Create(K);
            end;
            P.Param := nil;
          end;
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
            raise ECommandLine.Create(sMissingParam, [PLegacyChar(sOSVersion)]);
        end
        else if SameKey(sDropSect) then
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
            FDropSections.Append(EncodeLegacy(Head, Current - Head, CP_ACP));
          end;
          if FDropSections = nil then
            raise ECommandLine.Create(sMissingParam, [PLegacyChar(sSectionNames)]);
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
            raise ECommandLine.Create(sMissingParam, [PLegacyChar(sResourceNames)]);
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
              raise ECommandLine.Create(sMissingParam, [PLegacyChar(sLocaleMap)]);
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
            raise ECommandLine.Create(sMissingParam, [PLegacyChar(sImageBase)]);
        end
        else if SameKey(sMenuet) then
        begin
          FMenuetKolibri := mkMenuet;
          if FImageBase < 0 then
            FImageBase := 8;
          P.Param := nil;}
        end
      {$IFDEF Kolibri}
        else if SameKey(sKolibri) then
        begin
          FMenuetKolibri := mkKolibri;
          P.Param := nil;
        end;
      {$ENDIF}  
    end;

    if P.Param <> nil then
      with P do
      begin
        Param[Length] := CoreChar(0); // unsafe
        FSourceFileName := Param;
      end;

    CommandLine := P.NextParam;
  until False;*)
end;

destructor TApplication.Destroy;
begin
  FDropSections.Finalize;
  FSourceFileName.Finalize;
  FAppName.Finalize;
  FConsole.Destroy;

  if roPause in FOptions then // placed here to show exceptions properly
    with FConsole do
    begin
      Create(True);
      try
        ReadLn(sPressEnterToExit, StrLen(sPressEnterToExit));
      finally
        Destroy;
      end;
    end;
end;

procedure TApplication.Help;
begin
  with FConsole do
  begin
    WriteLn(sUsage, 0, [FAppName.RawData], 2);
    WriteLn(PLegacyChar(sHelp), StrLen(sHelp));
  end;
end;

procedure TApplication.Pause;
begin
  Include(FOptions, roPause);
end;

procedure TApplication.Run;

var
  Buf: string[6];

function Percentage(Ratio: Double): PLegacyChar;
begin
  Str((Ratio * 100):1:1, Buf);
  PWord(@Buf[Length(Buf) + 1])^ := Word('%');  // Fast core
  Result :=  @Buf[1];
end;

procedure Processing(Args: array of const; IsUnicode: Boolean = True);
const
  Prefixes: array[Boolean] of LegacyChar = 'hw';
var
  Format: string[Length(sProcessing) + 1];
begin
  Format := sProcessing + #0;
  Format[FileNameOffset] := Prefixes[IsUnicode];
  FConsole.WriteLn(@Format[1], FileNameWidth, Args);
end;

function TempFileName(Source: PCoreChar): PCoreChar;
var
  L: Integer;
begin
  L := WideStrLen(Source);
  GetMem(Result, (L + Length('.$$$') + 1) * SizeOf(CoreChar));
  Move(Source^, Result^, L * SizeOf(CoreChar));
  PLongWord(Result + L)^ := $0024002E;     // Fast core: '.$'
  PLongWord(Result + L + 2)^ := $00240024; // '$$'
  Result[L + 4] := CoreChar(0);
end;

const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
var
  I, Idx: Integer;
  OldSize, NewSize, RawSize, FixedSize: LongWord;
  Image: TExeImage;
  FileInfo: TWin32FindDataW;
  hInfo: THandle;
  TmpFileName: PCoreChar;
begin
(*  if FSourceFileName <> nil then
  begin
    Image := TExeImage.Create(IMAGE_NUMBEROF_DIRECTORY_ENTRIES, 0, True);
    try
      hInfo := FindFirstFileW(FSourceFileName, FileInfo);
      if hInfo = INVALID_HANDLE_VALUE then
        RaiseLastPlatformError(FSourceFileName);
      FindClose(hInfo);
      OldSize := FileInfo.nFileSizeLow;
      Processing([sLoadingSource, FSourceFileName, OldSize]);

      with Image do
      begin
        Load(FSourceFileName);
        NewSize := Size(False);
        Processing([sImageData, Percentage(NewSize / OldSize), NewSize], False);
      end;

      if FFileNames[fkExtract] <> nil then
      begin
        with TExeStub.Create do
        try
          Load(Image.Stub);
          Strip(roStrip in FOptions);
          Processing([sExtractingStub, FFileNames[fkExtract], Size]);
          Save(FFileNames[fkExtract]);
        finally
          Free;
        end;
      end;

      if FFileNames[fkStub] <> nil then
        with Image.Stub do
        begin
          Load(FFileNames[fkStub]);
          Processing([sInsertingStub, FFileNames[fkStub], Size]);
        end;

      if roStrip in FOptions then
        with Image do
        begin
          Strip([soStub..soEmptySections] + Deep[roDeep in FOptions]);
          NewSize := OldSize - Size;
          Processing([sStripping, Percentage(NewSize / OldSize), NewSize], False);
        end
      else
        with Image.Stub do
        begin
          RawSize := Size;
          Strip(False);
          FixedSize := Size;
          Processing([sFixingStub, Percentage(FixedSize / RawSize), FixedSize], False);
        end;

      if FDropSections <> nil then
        for I := 0 to FDropSections.Count - 1 do
        begin
          with FDropSections.Items[I] do
            Idx := Image.IndexOfSection(Value, Length);
          if Idx >= 0 then
          begin
            with Image.Sections[Idx].Header do
              Processing([sDroppingSection, Name, RawDataSize], False);
            Image.Extract(Idx).Free;
          end;
        end;

      {if (FDropResources <> nil) or (FLocaleMap <> nil) or
        (FOptions * [roMiniRes, roCleanVer, roMainIcon] <> []) then
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
      end;}

      if FFileNames[fkInto] <> nil then
      begin
        if FFileNames[fkBackup] <> nil then
        begin
          FConsole.WriteLn(sBackuping, 0, [FFileNames[fkBackup]]);
          if not MoveFileExW(FSourceFileName, FFileNames[fkBackup],
            MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH)
          then
            RaiseLastPlatformError(FFileNames[fkBackup]);
        end;

      {$IFDEF Kolibri}
        case FMenuetKolibri of
          mkKolibri:
            with TKolibriImage.Create do
            try
              Load(Image);
              Build(Byte(roStrip in FOptions) * 8);
              NewSize := Size;
              Processing([sSavingInto, FFileNames[fkInto], NewSize]);
              if FFileNames[fkBackup] <> nil then
                Save(FFileNames[fkInto], roTrunc in FOptions)
              else
              begin
                TmpFileName := TempFileName(FFileNames[fkInto]);
                try
                  Save(TmpFileName, roTrunc in FOptions);
                  if not MoveFileExW(TmpFileName, FFileNames[fkInto],
                    MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH or MOVEFILE_REPLACE_EXISTING)
                  then
                    RaiseLastPlatformError(FFileNames[fkInto]);
                finally
                  FreeMem(TmpFileName);
                end;
              end;
            finally
              Free;
            end;
        else
      {$ENDIF}
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
            Build(Byte(roStrip in FOptions) * 512);
            NewSize := Size(roTrunc in FOptions);
          end;

          Processing([sSavingInto, FFileNames[fkInto], NewSize]);
          if FFileNames[fkBackup] <> nil then
            Image.Save(FFileNames[fkInto], roTrunc in FOptions)
          else
          begin
            TmpFileName := TempFileName(FFileNames[fkInto]);
            try
              Image.Save(TmpFileName, roTrunc in FOptions);
              if not MoveFileExW(TmpFileName, FFileNames[fkInto],
                MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH or MOVEFILE_REPLACE_EXISTING)
              then
                RaiseLastPlatformError(FFileNames[fkInto]);
            finally
              FreeMem(TmpFileName);
            end;
          end;
      {$IFDEF Kolibri}
        end;
      {$ENDIF}

        FConsole.WriteLn;
        Processing([sTotal, Percentage(NewSize / OldSize), OldSize - NewSize], False);
      end;
    finally
      Image.Free;
    end;
  end
  else
    with FConsole do
    begin
      WriteLn(sUsage, 0, [FAppName], 0); // wvsprintf limits length to 1024 characters :-(
      WriteLn(sHelp);
    end; *)
end;

end.             

