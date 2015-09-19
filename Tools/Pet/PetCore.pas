(*
    PE Tool's core

    Copyright (c) 2013-2015 Vladislav Javadov (aka Freeman)
*)

unit PetCore;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreStrings, CoreApp;

type
//  TResourceNames = TWideStringArray;
  TSectionNames = TLegacyStrings;

  PFileName = ^TFileName;
  TFileName = object(TCoreString)
  public
    function IsDotOrNull: Boolean;
  end;

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TRunOption = (roPause, roNoLogo, roAuto, roStrip, roTrunc, roKeep, roSafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon,} ro3GB);
  TRunOptions = set of TRunOption;

  TMenuetKolibri = (mkNone, mkMenuet, mkKolibri);

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FSourceFileName: TCoreString;
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
//    FImageBase: CoreInt;
    FMajorVersion, FMinorVersion: Word;
  {$IFDEF Kolibri}
    FMenuetKolibri: TMenuetKolibri;
  {$ENDIF}
    procedure Parse(CommandLine: PCoreChar);
  public
//    constructor Create;
    destructor Destroy;
    procedure Run(CommandLine: PCoreChar);
  end;

{ Exceptions }

  TCmdLineFileKind = fkSource..High(TFileKind);

  ECommandLine = class(Exception)
  private
    FDuplicateParam: PCoreString;
    FFileKind: TFileKind;
  public
    constructor Create(MissingKind: TCmdLineFileKind); overload;
    constructor Create(MissingParam: PLegacyChar); overload;

    constructor Create(DuplicateKind: TCmdLineFileKind; FileName: PCoreString); overload;
    constructor Create(DuplicateParam: PLegacyChar; ParamValue: PCoreString); overload;

    property DuplicateParam: PCoreString read FDuplicateParam;
    property FileKind: TFileKind read FFileKind;
  end;

  ECore = class(Exception);

implementation

uses
  ExeImages, CoreConsts, PetConsts;

const
  FileKeys: array[fkInto..High(TFileKind)] of PCoreChar = (sInto, sStub, sExtract, sBackup{, sDump});

{ EFileKey }

//  inherited Create(sMissingFileName, LocalizationCP, [FileKeys[MissingFileName]]);
//

{ ECommandLine }

type
  TFileKindBuf = array[0..Length(sFileName) + 10] of LegacyChar;

function FormatFileKind(FileKind: TCmdLineFileKind): TFileKindBuf;
var
  KindName: PCoreChar;
begin
  if FileKind = fkSource then
    KindName := sSource
  else
    KindName := FileKeys[FileKind] + 1;
  FormatBuf(sFileName, [KindName], Result);
end;

constructor ECommandLine.Create(MissingKind: TCmdLineFileKind);
var
  Kind: TFileKindBuf;
begin
  Kind := FormatFileKind(MissingKind);
  inherited Create(sMissingParam, [Kind]);
  FFileKind := MissingKind;
end;

constructor ECommandLine.Create(MissingParam: PLegacyChar);
begin
  inherited Create(sMissingParam, [MissingParam]);
end;

constructor ECommandLine.Create(DuplicateKind: TCmdLineFileKind; FileName: PCoreString);
var
  Kind: TFileKindBuf;
begin
  Kind := FormatFileKind(DuplicateKind);
  inherited Create(sDuplicateParam, LocalizationCP, [Kind, FileName.Data]);
  FFileKind := DuplicateKind;
  FDuplicateParam := FileName;
end;

constructor ECommandLine.Create(DuplicateParam: PLegacyChar; ParamValue: PCoreString);
begin
  inherited Create(sDuplicateParam, LocalizationCP, [DuplicateParam, ParamValue.Data]);
  FDuplicateParam := ParamValue;
end;

{ TFileName }

function TFileName.IsDotOrNull: Boolean;
begin
  Result := (TypeOf(Self) <> nil) and ((Count = 0) or (Count = 1) and (RawData^ = '.'));
end;

{ TApplication }

{constructor TApplication.Create;
begin
  inherited;
  FImageBase := -1;
end;}

destructor TApplication.Destroy;
var
  K: TFileKind;
begin
  FDropSections.Finalize;
  for K := Low(FFileNames) to High(FFileNames) do
    FFileNames[K].Finalize;
  FSourceFileName.Finalize;
  inherited;
end;

procedure TApplication.Parse(CommandLine: PCoreChar);
const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sPause, sNoLogo, sAuto, sStrip, sTrunc, sKeep, sSafe, sDeep,
     {sMiniRes, sCleanVer, sMainIcon,} s3GB);
  HexBase: array[Boolean] of LegacyChar = 'A0';
var
  CmdLine, Key: TCoreString;
  Param: TCommandLineParam;
  ParamCount, Dot: Integer;
  FileName: PFileName;
  W: PCoreChar;
  K: TFileKind;
  R: TRunOption;
  Value: Word;
begin
  CmdLine.Create;
  CmdLine.AsWideString(CommandLine, WideStrLen(CommandLine), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name

  Key.Create;
  ParamCount := 0;
  repeat
    CmdLine := Param.AsNextParam(@CmdLine);
    if Param.Count = 0 then
      Break;

    Inc(ParamCount);
    Key.AsRange(@Param, 1);
    if not Param.Quoted and (Param.RawData^ = '-') then
    begin
      for R := Low(OptionKeys) to High(OptionKeys) do
      begin
        W := OptionKeys[R];
        if Key.Compare(W + 1, PWord(W)^, True) = 0 then
        begin
          Include(FOptions, R);
          Param.Clear;
          Break;
        end;
      end;

      if Param.Count <> 0 then
        for K := Low(FFileNames) to High(FFileNames) do
        begin
          W := FileKeys[K];
          if Key.Compare(W + 1, PWord(W)^, True) = 0 then
          begin
            CmdLine := Param.AsNextParam(@CmdLine);
            if (Param.Count = 0) and not (K in [fkInto, fkStub]) then
              raise ECommandLine.Create(K);
            with FFileNames[K] do
            begin
              if Count <> 0 then
                raise ECommandLine.Create(K, @Param);
              Create;
              AsRange(@Param, 0);
              Detach;
            end;
            Param.Clear;
            Break;
          end;
        end;

      if Param.Count <> 0 then
      begin
        W := sOSVer;
        if Key.Compare(W + 1, PWord(W)^, True) = 0 then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sOSVersion);
          if FMajorVersion <> 0 then
            raise ECommandLine.Create(sOSVersion, @Param);
          with Param do
          begin
            Dot := NextIndex('.');
            if Dot >= 0 then
            begin
              FMajorVersion := AsRange(0, Dot - 1).AsInteger;
              FMinorVersion := AsRange(Dot + 1).AsInteger;
            end
            else
            begin
              FMajorVersion := AsInteger;
              FMinorVersion := 0;
            end;
            Clear;
          end;
        end
        else
        begin
          W := sSectionNames;
          if Key.Compare(W + 1, PWord(W)^, True) = 0 then
          begin
            CmdLine := Param.AsNextParam(@CmdLine);
            if Param.Count = 0 then
              raise ECommandLine.Create(sSectionNames);
            with Param do
            begin
              {Dot := NextIndex('.');
              if Dot >= 0 then
              begin
                FMajorVersion := AsRange(0, Dot - 1).AsInteger;
                FMinorVersion := AsRange(Dot + 1).AsInteger;
              end
              else
              begin
                FMajorVersion := AsInteger;
                FMinorVersion := 0;
              end;}
              Clear;
            end;
          end
        end;
      end;
    end;

    if Param.Count <> 0 then
    begin
      if FSourceFileName.Count <> 0 then
        raise ECommandLine.Create(fkSource, @Param);
      with FSourceFileName do
      begin
        Create;
        AsRange(@Param, 0);
      end;
    end;
  until False;

  if ParamCount = 0 then
    Include(FOptions, roPause)
  else
  begin
    if FSourceFileName.Count = 0 then
      raise ECommandLine.Create(fkSource);

    FileName := @FFileNames[fkInto];
    if FileName.IsDotOrNull then
      FileName.AsRange(@FSourceFileName, 0);

    FileName := @FFileNames[fkStub];
    if FileName.IsDotOrNull then
      FileName.AsRange(@ExeName, 0);
  end;
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
  Console.WriteLn(@Format[1], FileNameWidth, Args);
end;

function TempFileName(Source: PCoreChar): PCoreChar;
type
  PDotDollarDollarDollar = ^TDotDollarDollarDollar;
  TDotDollarDollarDollar = array[0..3] of CoreChar;
const
  DotDollarDollarDollar: TDotDollarDollarDollar = ('.', '$', '$', '$');
var
  L: Integer;
begin
  L := WideStrLen(Source);
  GetMem(Result, (L + Length(DotDollarDollarDollar) + 1) * SizeOf(CoreChar));
  Move(Source^, Result^, L * SizeOf(CoreChar));
  PDotDollarDollarDollar(Result + L)^ := DotDollarDollarDollar;
  Result[L + Length(DotDollarDollarDollar)] := CoreChar(0);
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
  inherited Run(sLogo);
  Parse(CommandLine);

  if FSourceFileName.Count <> 0 then
  begin
(*    Image := TExeImage.Create(IMAGE_NUMBEROF_DIRECTORY_ENTRIES, 0, True);
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
    end; *)
  end
  else
    Help(sUsage, sHelp);
end;

end.

