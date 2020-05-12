(*
    PE Tool core

    Copyright (c) 2013-2018, 2020 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Locale -- additional locale-dependent translations for console output
*)

unit PetCore;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings,
  CoreApp, ExeImages;

type
  TSectionNames = TLegacyTextList;

  TOutput = object(TCoreObject)
  private
    FConsole: PStreamConsole;
    FActionFormat, FActionEllipsisFormat, FActionBuf,
    FStatsFormat, FStatsBuf: PWideChar;
    FPercentage: TLegacyString;
    FPercentageBuf: array[0..$F] of LegacyChar;
    FFileNameWidth: Integer;
  public
    constructor Create(Dest: PStreamConsole; PromptWidth, FileNameWidth: Integer;
      FileSizeLimit: QuadWord);
    destructor Destroy; virtual;
    procedure Action(Prompt: PLegacyChar; FileName: PFileName);
    procedure StripStats(SourceSize, StrippedSize: LongWord);
    procedure TotalStats(FileCount, TotalBytes, TotalWritten: LongWord);
    procedure TransferStats(SourceSize, ActualSize: LongWord);
  end;

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TLogStyle = (lsAuto, lsBrief, lsDetail);

  TRunOption = (roPause, roNoLogo, roVersion, ro3GB, roASLR, roDEP, // ordered
    {roAuto,} roStrip, roTrunc, roTouch, roUnsafe, roDeep, roDir, roRaw,
    {roMiniRes, roVerInfo, roMainIcon, roVerbose} roListSections, roOSVersion, roRebase);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication{<TRunOptions>})
  private
    FOptions: TRunOptions; // specialize <F>
    FSourceFileNames: TFileNameList;
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
    FMajorVersion, FMinorVersion: Word;
    FLogStyle: TLogStyle;
    FRebaseAddress: CoreWord;
    FFoundFiles: TFileNameList;
    FImage: TExeImage;
    FCurrentPath: TFileName;
    FMaxWidth: Integer;
    FMaxFileSize: QuadWord;
    FACP: TCodePage;
    procedure AddFile(const Data: TWin32FindDataW; var Found: Boolean);
    function MaxFileNameWidth(MaxWidth: Integer): Integer;
    function MaxPromptWidth: Integer;
    procedure ParseCommandLine(Source: PCoreChar);
    function PrepareFileName(Kind: TFileKind; FileName: PFileName): PFileName;
    procedure SaveImage(Dest: PWritableStream);
    function TotalsMaxPromptWidth: Integer;
  public
    destructor Destroy;
    procedure Run(CommandLine: PCoreChar);
    property Options: TRunOptions read FOptions;
  end;

{ Exceptions }

  TCmdLineFileKind = fkSource..High(TFileKind);

  ECommandLine = class(Exception)
  private
    FInvalidParam: PCoreString;
    FFileKind: TFileKind;
  public
    constructor CreateMissing(MissingKind: TCmdLineFileKind); overload;
    constructor CreateMissing(MissingParam: PLegacyChar); overload;

    constructor CreateDuplicate(DuplicateKind: TCmdLineFileKind; FileName: PCoreString); overload;
    constructor CreateDuplicate(DuplicateParam: PLegacyChar; ParamValue: PCoreString); overload;

    constructor CreateInvalid(Option: PCoreString); overload;
    constructor CreateInvalid(Option: PLegacyChar; InvalidValue: PCoreString); overload;

    property InvalidParam: PCoreString read FInvalidParam;
    property FileKind: TFileKind read FFileKind;
  end;

  EImageBase = class(Exception)
  private
    FValue: CoreWord;
  public
    constructor Create(Value: CoreWord);
    property Value: CoreWord read FValue;
  end;

implementation

uses
  CoreConsts, PetConsts;

const
  IgnoreFileNameCase = True; // platform

  FileKeys: array[fkInto..High(TFileKind)] of PCoreChar = (sInto, sStub, sExtract, sBackup{, sDump});
  Processing: array[Boolean] of PLegacyChar = (sEstimating, sStripping);

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

constructor ECommandLine.CreateMissing(MissingKind: TCmdLineFileKind);
var
  Kind: TFileKindBuf;
begin
  Kind := FormatFileKind(MissingKind);
  inherited Create(sMissingParam, [Kind]);
  FFileKind := MissingKind;
end;

constructor ECommandLine.CreateMissing(MissingParam: PLegacyChar);
begin
  inherited Create(sMissingParam, [MissingParam]);
end;

constructor ECommandLine.CreateDuplicate(DuplicateKind: TCmdLineFileKind; FileName: PCoreString);
var
  Kind: TFileKindBuf;
begin
  Kind := FormatFileKind(DuplicateKind);
  inherited Create(sDuplicateParam, DefaultSystemCodePage, [Kind, FileName.Data]);
  FFileKind := DuplicateKind;
  FInvalidParam := FileName;
end;

constructor ECommandLine.CreateDuplicate(DuplicateParam: PLegacyChar; ParamValue: PCoreString);
begin
  inherited Create(sDuplicateParam, DefaultSystemCodePage, [DuplicateParam, ParamValue.Data]);
  FInvalidParam := ParamValue;
end;

constructor ECommandLine.CreateInvalid(Option: PCoreString);
begin
  inherited Create(sInvalidOption, DefaultSystemCodePage, [Option.Data]);
  FInvalidParam := Option;
end;

constructor ECommandLine.CreateInvalid(Option: PLegacyChar; InvalidValue: PCoreString);
begin
  inherited Create(sInvalidOptionValue, DefaultSystemCodePage, [Option, InvalidValue.Data]);
  FInvalidParam := InvalidValue;
end;

{ EImageBase }

constructor EImageBase.Create(Value: CoreWord);
begin
  inherited Create(sImageBaseUnaligned, DefaultSystemCodePage, [Value]);
  FValue := Value;
end;

{ TOutput }

constructor TOutput.Create(Dest: PStreamConsole;
  PromptWidth, FileNameWidth: Integer; FileSizeLimit: QuadWord);
var
  StatBytesWidth: Integer;
begin
  InitInstance;
  FConsole := Dest;
  FPercentage.Create;
  FPercentage.ExternalBuffer(@FPercentageBuf, Length(FPercentageBuf));

  FActionFormat := Format(sActionFmt, DefaultSystemCodePage, 0,
    [PromptWidth, -FileNameWidth - Length(sPathEllipsis)]).Value;
  FActionEllipsisFormat := Format(sActionFmt, DefaultSystemCodePage, 0,
    [PromptWidth, -FileNameWidth]).Value;
  FFileNameWidth := FileNameWidth;
  GetMem(FActionBuf, (PromptWidth + FileNameWidth + Length(sPathEllipsis) +
    Length(sActionFmt)) * SizeOf(WideChar));

  if FileSizeLimit = 0 then
    StatBytesWidth := 1
  else                                             // ,-- sign
    StatBytesWidth := Ceil32(Log10(FileSizeLimit)) +  1;
  FStatsFormat := Format(sStatsFmt, DefaultSystemCodePage, 0, [StatBytesWidth]).Value;
  GetMem(FStatsBuf, (StatBytesWidth + PercentageWidth + Length(sStatsFmt)) * SizeOf(WideChar));
end;

destructor TOutput.Destroy;
begin
  FreeMem(FStatsBuf);
  FreeMem(FStatsFormat);

  FreeMem(FActionBuf);
  FreeMem(FActionEllipsisFormat);
  FreeMem(FActionFormat);

  inherited;
end;

procedure TOutput.Action(Prompt: PLegacyChar; FileName: PFileName);
var
  W: PWideChar;
begin
  if FileName <> nil then
    if FileName.Count > FFileNameWidth then
    begin
      FConsole.WriteLn(FActionBuf, WideFormatBuf(FActionEllipsisFormat, [Prompt,
        PLegacyChar(sPathEllipsis), FileName.RawData + FileName.NameIndex], FActionBuf), 0);
      Exit;
    end
    else
      W := FileName.RawData
  else
    W := nil;
  FConsole.WriteLn(FActionBuf, WideFormatBuf(FActionFormat, [Prompt, nil, W], FActionBuf), 0);
end;

procedure TOutput.TransferStats(SourceSize, ActualSize: LongWord);
var
  P: PLegacyChar;
begin
  if SourceSize <> 0 then
  begin
    FPercentage.AsPercentage(ActualSize / SourceSize);
    P := FPercentage.RawData;
  end
  else
    P := nil;
    FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [ActualSize, P], FStatsBuf));
end;

procedure TOutput.StripStats(SourceSize, StrippedSize: LongWord);
var
  DiffBytes: LongInt;
begin
  DiffBytes := StrippedSize - SourceSize;
  FPercentage.AsPercentage(DiffBytes / SourceSize{, 1, True});
  FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [DiffBytes, FPercentage.RawData], FStatsBuf));
end;

procedure TOutput.TotalStats(FileCount, TotalBytes, TotalWritten: LongWord);
var
  DiffBytes: LongInt;
begin
  DiffBytes := TotalWritten - TotalBytes;
  FPercentage.AsPercentage(DiffBytes / TotalBytes{, 1, True});
  FConsole.WriteLn(sTotalsMessage, 0, [FileCount, DiffBytes, FPercentage.RawData]);
end;

{ TApplication }

destructor TApplication.Destroy;
var
  K: TFileKind;
begin
  FImage.Finalize;
  FCurrentPath.Finalize;
  FFoundFiles.Finalize;

  FDropSections.Finalize;
  for K := Low(FFileNames) to High(FFileNames) do
    FFileNames[K].Finalize;
  FSourceFileNames.Finalize;
  inherited;
end;

procedure TApplication.AddFile(const Data: TWin32FindDataW; var Found: Boolean);
var
  Item: PFileNameListItem;
  Width: Integer;
  FileSize: QuadWord;
begin
  if Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
  begin
    New(Item, Create);
    Item.AsRange(@FCurrentPath, 0);
    with Data do
    begin
      Item.ChangeFileName(cFileName, WideStrLen(cFileName, Length(cFileName)));
      FileSize := nFileSizeHigh shr 32 or nFileSizeLow;
      if FileSize > FMaxFileSize then
        FMaxFileSize := FileSize;
    end;
    Width := Item.Width(FMaxWidth);
    if Width > FMaxWidth then
      FMaxWidth := Width;
    FFoundFiles.Append(Item);
  end;
end;

function TApplication.MaxFileNameWidth(MaxWidth: Integer): Integer;
var
  K: TFileKind;
  Width: Integer;
begin                                       
  Result := Byte(roRebase in FOptions) * 8;
  for K := Low(FFileNames) to High(FFileNames) do
  begin
    Width := FFileNames[K].Width(MaxWidth);
    if Width > Result then
      Result := Width;
  end;
end;

function TApplication.MaxPromptWidth: Integer;
const
  Prompts: array[fkInto..High(TFileKind)] of PLegacyChar =
    (sSaving, sReplacingStub, sExtractingStub, sBackuping);
var
  K: TFileKind;
  Width: Integer;
begin
  if FLogStyle <> lsBrief then
  begin
    Result := StrLen(DefaultMaxWidth);
    if roASLR in FOptions then
    begin
      Width := StrLen(sKeepingRelocations);
      if Width > Result then
        Result := Width;
    end;
    if roRebase in FOptions then
    begin
      Width := StrLen(sRebasingTo);
      if Width > Result then
        Result := Width;
    end;
    if FDropSections.Count <> 0 then
    begin
      Width := StrLen(sDroppingSection);
      if Width > Result then
        Result := Width;
    end;
    for K := Low(Prompts) to High(Prompts) do
      if TypeOf(FFileNames[K]) <> nil then
      begin
        Width := StrLen(Prompts[K]);
        if Width > Result then
          Result := Width;
      end;
    if TypeOf(FFileNames[fkInto]) <> nil then
    begin
      Width := StrLen(sDestFile);
      if Width > Result then
        Result := Width;
    end
    else
    begin
      Width := StrLen(sEstimated);
      if Width > Result then
        Result := Width;
    end;
  end
  else
    Result := TotalsMaxPromptWidth;
end;

procedure TApplication.ParseCommandLine(Source: PCoreChar);

const
  OptionKeys: array[roPause..roListSections] of PCoreChar = (
    sPause, sNoLogo, sVersion, s3GB, sASLR, sDEP,
    sStrip, sTrunc, sTouch, sUnsafe, sDeep, sDir, sRaw,
    {sMiniRes, sVerInfo, sMainIcon, sVerbose,} sLS
  );

procedure AppendFileName(Source: PCoreString);
var
  FileName: PFileNameListItem;
begin
  New(FileName, Create);
  FileName.AsRange(Source, 0);
  FileName.Detach;
  FSourceFileNames.Append(FileName);
end;

var
  CmdLine: TCoreString;
  Key, Param: TCommandLineParam;
  ParamCount, Dot: Integer;
  FileName: PFileName;
  K: TFileKind;
  R: TRunOption;
begin
  CmdLine.Create;
  CmdLine.AsWideString(Source, WideStrLen(Source), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name
  Param.Clear;

  ParamCount := 0;
  Key.Create;
  repeat
    if Param.Count = 0 then
      CmdLine := Param.AsNextParam(@CmdLine);
    if Param.Count = 0 then
      Break;

    Inc(ParamCount);
    if Param.IsKey then
    begin
      Key.AsRange(@Param, 1);
      if Key.Count <> 0 then
      begin
        for R := Low(OptionKeys) to High(OptionKeys) do
          if Key.Equals(OptionKeys[R]) then
          begin
            Include(FOptions, R);
            Param.Clear;
            Break;
          end;

        if Param.Count <> 0 then
        begin
          for K := Low(FFileNames) to High(FFileNames) do
            if Key.Equals(FileKeys[K]) then
            begin
              CmdLine := Param.AsNextParam(@CmdLine);
              if (Param.Count = 0) and not (K in [fkInto, fkStub]) then
                raise ECommandLine.CreateMissing(K);
              with FFileNames[K] do
              begin
                if Count <> 0 then
                  raise ECommandLine.CreateDuplicate(K, @Param);
                PPointer(@FFileNames[K])^ := TypeOf(TFileName); // Fast core
                if not Param.IsKey then
                begin
                  AsRange(@Param, 0);
                  Detach;
                  Param.Clear;
                end;
              end;
              Break;
            end;
          if Param.RawData > Key.RawData then
            Continue;
        end;
      end;

      if Param.Count <> 0 then
      begin
        if Key.Equals(sOSVer) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.CreateMissing(sOSVersion);
          if roOSVersion in FOptions then
            raise ECommandLine.CreateDuplicate(sOSVersion, @Param);
          with Param do
          begin
            Dot := RawNextIndex(WideChar('.'));
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
          end;
          Include(FOptions, roOSVersion);
        end
        else if Key.Equals(sRebase) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sRebaseAddress);
          if roRebase in FOptions then
            raise ECommandLine.CreateDuplicate(sRebaseAddress, @Param);
          if Param.RawData[0] = '$' then
          begin
            Param.Skip;
            FRebaseAddress := Param.AsHexadecimal;
          end
          else if Word(Param.RawData[Param.Count - 1]) or $20 = Word('h') then
          begin
            Param.Truncate(1);
            FRebaseAddress := Param.AsHexadecimal;
          end
          else
          begin
            FRebaseAddress := Param.AsInteger;
            if (FRebaseAddress < 32) and (FRebaseAddress <> 0) then
              FRebaseAddress := 1 shl FRebaseAddress;
          end;
          Include(FOptions, roRebase);
        end
        else if Key.Equals(sDropSect) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sSectionNames);
          if TypeOf(FDropSections) = nil then
            FDropSections.Create;
          FACP.Create;
          LoadText(FDropSections.Append, @Param, CoreChar(','), @FACP);
        end
        else if Key.Equals(sLog) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.CreateMissing(sLogStyle);
          if FLogStyle <> lsAuto then
            raise ECommandLine.CreateDuplicate(sLogStyle, @Param);
          if Param.Equals(sDetail) then
            FLogStyle := lsDetail
          else if Param.Equals(sBrief) then
            FLogStyle := lsBrief
          else
            raise ECommandLine.CreateInvalid(sLogStyle, @Param);
        end
        else
          raise ECommandLine.CreateInvalid(@Param);
        Param.Clear;
      end;
    end;

    if Param.Count <> 0 then
    begin
      if TypeOf(FSourceFileNames) = nil then
        FSourceFileNames.Create;
      AppendFileName(@Param);
      Param.Clear;
    end;
  until False;

  if ParamCount <> 0 then
  begin
    if (TypeOf(FSourceFileNames) = nil) and not (roVersion in FOptions) then
    begin
      FileName := @FFileNames[fkInto];
      if FileName.Count <> 0 then
      begin
        FSourceFileNames.Create;
        AppendFileName(FileName);
      end
      else if FileName.IsDotOrNull then
      begin
        FSourceFileNames.Create;
        AppendFileName(@ExeName);
      end
      else
        raise ECommandLine.CreateMissing(fkSource);
    end;
    FileName := @FFileNames[fkStub];
    if FileName.IsDotOrNull then
      FileName.AsRange(@ExeName, 0);
  end
  else if Console.Redirection = [] then
    Include(FOptions, roPause);
end;

function TApplication.PrepareFileName(Kind: TFileKind; FileName: PFileName): PFileName;
begin
  Result := @FFileNames[Kind];
  if Result.IsPath or
    not Result.IsDot and (FileAttributes(Result.RawData) and FILE_ATTRIBUTE_DIRECTORY <> 0) then
  begin
    with FileName^ do
    begin
      AsRange(Result, 0);
      MakePath;
    end;
    Result := FileName;
  end;
end;

procedure TApplication.Run(CommandLine: PCoreChar);

var
  TmpFileName: TFileName;
  HexString: TCoreString;
  DestFileName, ImageFileName: PFileName;
  FileName: PFileNameListItem;
  Loaded: TLoadFileResult;
  Output: TOutput;

function SaveFile(SwapFileName: PCoreChar): TSaveFileResult;
const
  Backup: array[Boolean] of TSaveOptions = ([], [soBackup]);
  Touch: array[Boolean] of TSaveOptions = ([], [soTouch]);
begin
  Result := CoreWrappers.SaveFile(
    SaveImage, FileName.RawData, SwapFileName, DestFileName.RawData,
    FImage.Size(roTrunc in FOptions), faRandomWrite,
    Touch[roTouch in FOptions] + Backup[FFileNames[fkBackup].Count <> 0]
  );
end;

procedure ShowImageException(Prompt: PLegacyChar; E: Exception);
begin
  Console.EndOfLine;
  Console.WriteLn(Prompt, 0, [E.Message.AsString, ImageFileName.RawData]);
end;

procedure ShowSections;
type
  KnownSubsystem = IMAGE_SUBSYSTEM_NATIVE..IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION;
const
  SubsystemName: array[KnownSubsystem] of PLegacyChar =
    (sNative, sGUI, sConsole, nil, sOS2, nil, sPOSIX, s9xDrv, sWindowsCE,
     sEFIApp, sEFIBootDrv, sEFIRuntimeDrv, sEFIROM, sXbox, nil, sWindowsBootApp);
var
  Opt: array[0..3] of PCoreChar;
  P: PPCoreChar;
  I, L, R, V: Integer;
  TempSectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of LegacyChar;
  S: PLegacyChar;
begin
  if FLogStyle <> lsBrief then
  begin
    Console.WriteLn;
    Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
      PLegacyChar(sSectionList), StrLen(sSectionList));
  end;

  with FImage.SectionsMax do
  begin
    R := -Ceil(Log2(RawDataOffset) / 4);
    V := -Ceil(Log2(VirtualAddress) / 4);
  end;

  TmpFileName.AsHexadecimal(0, R, False, CoreChar('0'));
  HexString.AsHexadecimal(0, V, False, CoreChar('0'));
  Console.WriteLn(sSectionFmt, 0, [PLegacyChar(sStubSection), TmpFileName.RawData, HexString.RawData], 0);
  Output.TransferStats(Loaded.FileSize, FImage.Stub.Size);

  with FImage.Stub.Header.Ext do
  begin
    TmpFileName.AsHexadecimal(NewHeaderOffset, R, False, CoreChar('0'));
    HexString.AsHexadecimal(NewHeaderOffset, V, False, CoreChar('0'));
  end;
  Console.WriteLn(sSectionFmt, 0, [PLegacyChar(sHeadersSection), TmpFileName.RawData, HexString.RawData], 0);
  Output.TransferStats(Loaded.FileSize, FImage.HeadersSize +
    Cardinal(FImage.Count * SizeOf(TImageSectionHeader)));

  for I := 0 to FImage.Count - 1 do
    with FImage.Sections[I] do
    begin
      L := StrLen(Header.Name, IMAGE_SIZEOF_SHORT_NAME);
      Move(Header.Name[0], TempSectionName, L);
      TempSectionName[L] := #0;
      TmpFileName.AsHexadecimal(Header.RawDataOffset, R, False, CoreChar('0'));
      HexString.AsHexadecimal(Header.VirtualAddress, V, False, CoreChar('0'));
      Console.WriteLn(sSectionFmt, 0, [@TempSectionName, TmpFileName.RawData, HexString.RawData], 0);
      Output.TransferStats(Loaded.FileSize, Size);
    end;

  if FLogStyle <> lsBrief then
    Console.WriteLn;
  with FImage.Headers.OptionalHeader do
  begin
    Console.WriteLn(sOSVersionFmt, 0, [PLegacyChar(sRequiredOSVersion),
      MajorOSVersion, MinorOSVersion, MajorSubsystemVersion, MinorSubsystemVersion], 0);
    if Subsystem in [Low(KnownSubsystem)..High(KnownSubsystem)] then
      S := SubsystemName[Subsystem]
    else
      S := nil;
    if S <> nil then
      Console.WriteLn(sSubsystemFmt, 0, [S])
    else
      Console.WriteLn;
    TmpFileName.AsHexadecimal(ImageBase, -8, False, CoreChar('0'));
    Console.WriteLn(sSectionAlignmentFmt, 0, [PLegacyChar(sSectionAlignment),
      SectionAlignment, FileAlignment]);
    Console.WriteLn(sImageOptionsFmt, 0, [PLegacyChar(sImageBaseTitle),
      TmpFileName.RawData, nil, nil, nil]);
  end;

  FillChar(Opt, SizeOf(Opt), 0);
  P := @Opt[0];
  if FImage.IsLargeAddressAware then
  begin
    P^ := s3GB;
    Inc(P^);
    Inc(P);
  end;
  if FImage.IsASLRAware then
  begin
    P^ := sASLR;
    Inc(P^);
    Inc(P);
  end;
  if FImage.IsDEPAware then
  begin
    P^ := sDEP;
    Inc(P^);
  end;
  if FImage.IsDotNETAware then
  begin
    P^ := sDotNET;
    Inc(P^);
  end;
  if Opt[0] <> nil then
    Console.WriteLn(sImageOptionsFmt, 0, [sImageOptions, Opt[0], Opt[1], Opt[2], Opt[3]]);
end;

const
  DataDirectory: array[Boolean] of TStripOptions = ([], [soDataDirectory]);
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
  Relocations: array[Boolean] of TStripOptions = ([], [soRelocations]);
var
  Stub: TExeStub;
  Saved: TSaveFileResult;
  TotalBytes, TotalWritten: QuadWord;
  ImageSize, OldSize, SectionBytes: LongWord;
  ExtractFileName: PFileName;
  Section: PLegacyTextListItem;
  RawData: TExeSectionData;
  Relocs: PExeSection;
begin
  try
    ParseCommandLine(CommandLine);
  except
    Logo(sLogo);
    raise;
  end;

  if Logo(sLogo) then
    Exit;

  if FSourceFileNames.Count = 0 then
  begin
    Help(sUsage, sHelp);
    Exit;
  end;

  PPointer(@FCurrentPath)^ := TypeOf(TFileName); // Fast core
  FFoundFiles.Create;
  FMaxWidth := MaxFileNameWidth(40); // magic
  if FLogStyle = lsBrief then
    Inc(FMaxWidth, StrLen(DefaultMaxWidth) - TotalsMaxPromptWidth);

  FileName := FSourceFileNames.First;
  while FileName <> nil do
  begin
    if FileName.IsDot then
      FileName.AsRange(@ExeName, 0);
    with FCurrentPath do
    begin
      AsRange(FileName, 0, FileName.NameIndex);
      Detach;
    end;
    FindFiles(AddFile, FileName.RawData);
    FileName := FileName.Next;
  end;

  Console.EndOfLine;
  case FFoundFiles.Count of
    0:
      begin
        Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
          PLegacyChar(sNoFilesFound), StrLen(sNoFilesFound));
        Exit;
      end;
    1:
      if FLogStyle = lsAuto then
        Inc(FLogStyle, 2);
  else
    if FLogStyle = lsAuto then
      Inc(FLogStyle);
  end;

  DestFileName := PrepareFileName(fkInto, @FCurrentPath);
  FImage.Create;

  TmpFileName.Create;
  HexString.Create;
  Output.Create(@Console, MaxPromptWidth, FMaxWidth, FMaxFileSize);
  try
    TotalBytes := 0;
    TotalWritten := 0;
    FileName := FFoundFiles.First;
    while FileName <> nil do
    begin
      try
        ImageFileName := FileName;
        Loaded := LoadFile(FImage.Load, FileName.RawData, faRandomRead);

        if FLogStyle <> lsBrief then
        begin
          Output.Action(sLoading, FileName);
          Output.TransferStats(Loaded.FileSize, Loaded.BytesRead);
        end
        else
          Output.Action(Processing[TypeOf(DestFileName^) <> nil], FileName);

        ImageSize := FImage.Size(False);
        if FLogStyle <> lsBrief then
        begin
          Output.Action(sImageData, nil);
          Output.TransferStats(Loaded.BytesRead, ImageSize);
        end;

        if Loaded.FileSize <> Loaded.BytesRead then // chained data found
        begin
          if FLogStyle <> lsBrief then
          begin
            Output.Action(sChainedData, nil);
            if roUnsafe in FOptions then
              Output.StripStats(Loaded.FileSize, Loaded.BytesRead)
            else
              Output.TransferStats(Loaded.FileSize, Loaded.FileSize - Loaded.BytesRead);
          end;
          if not (roUnsafe in FOptions) then
          begin
            Console.EndOfLine;
            Console.WriteLn(sUnsafeOperation, 0, [PLegacyChar(sChainedDataFound), PLegacyChar(sSafeStripping)]);
            if roListSections in FOptions then
            begin
              ShowSections;
              if FileName.Next <> nil then
                Console.WriteLn;
            end;
            if (FLogStyle <> lsBrief) and (FileName.Next <> nil) then
              Console.WriteLn;
            Inc(TotalWritten, Loaded.FileSize);
            FileName := FileName.Next;
            Continue;
          end;
        end;

        if FFileNames[fkExtract].Count <> 0 then
        begin
          Stub.Create;
          try
            ExtractFileName := PrepareFileName(fkExtract, @TmpFileName);
            if ExtractFileName = @TmpFileName then
              TmpFileName.ChangeFileName(FileName);
            if FLogStyle <> lsBrief then
              Output.Action(sExtractingStub, ExtractFileName);
            with Stub do
            begin
              Load(@FImage.Stub);
              Strip(roStrip in FOptions);
              Saved := CoreWrappers.SaveFile(Save, ExtractFileName.RawData, Size);
            end;
            if FLogStyle <> lsBrief then
              Output.TransferStats(0, Saved.BytesWritten);
          finally
            Stub.Destroy;
          end;
        end;

        if FFileNames[fkStub].Count <> 0 then
          with FImage.Stub do
          begin
            if FLogStyle <> lsBrief then
              Output.Action(sReplacingStub, @FFileNames[fkStub]);
            OldSize := Size;
            ImageFileName := @FFileNames[fkStub];
            LoadFile(Load, ImageFileName.RawData);
            if FLogStyle <> lsBrief then
              Output.StripStats(OldSize, Size);
          end;

        if roRebase in FOptions then
        begin
          if FLogStyle <> lsBrief then
          begin
            TmpFileName.AsHexadecimal(FRebaseAddress, -8, False, CoreChar('0'));
            Output.Action(sRebasingTo, @TmpFileName);
          end;
          if (FRebaseAddress mod $10000 <> 0) and (FRebaseAddress <> 0) and not (roUnsafe in FOptions) then
          begin
            Console.EndOfLine;
            Console.WriteLn(sUnsafeOperation, 0, [PLegacyChar(sNonStandardRebase), PLegacyChar(sSafeRebasing)]);
          end
          else if FImage.Rebase(FRebaseAddress) = 0 then
            raise EBadImage.Create(sCannotRebaseImage)
          else if FLogStyle <> lsBrief then
            Console.WriteLn;
        end;

        if FDropSections.Count <> 0 then
        begin
          Section := FDropSections.First;
          while Section <> nil do
          begin
            with Section^ do
              SectionBytes := FImage.Delete(RawData, Count);
            if FLogStyle <> lsBrief then
            begin
              TmpFileName.AsString(Section);
              if SectionBytes <> 0 then
              begin
                Output.Action(sDroppingSection, @TmpFileName);
                Output.StripStats(ImageSize, ImageSize - SectionBytes);
              end
              else
                Console.WriteLn(sSectionNotFound, 0, [TmpFileName.RawData]);
            end;
            Section := Section.Next;
          end;
          ImageSize := FImage.Size(False);
        end;

        if roStrip in FOptions then
        begin
          if FLogStyle <> lsBrief then
            Output.Action(sStripping, nil);
          FImage.Strip([soStub..soEmptySections] - Relocations[roASLR in FOptions] +
            DataDirectory[roDir in FOptions] + Deep[roDeep in FOptions]);
          if FLogStyle <> lsBrief then
          begin
            Output.StripStats(ImageSize, FImage.Size(roTrunc in FOptions));
            if (roASLR in FOptions) and FImage.CanStripRelocations then
            begin
              Relocs := FImage.SectionOf(stRelocations);
              if Relocs <> nil then
              begin
                Output.Action(sKeepingRelocations, nil);
                Output.TransferStats(ImageSize, Relocs.Size + SizeOf(TImageSectionHeader));
              end;
            end;
          end;
        end
        else 
          with FImage.Stub do
          begin
            if FLogStyle <> lsBrief then
              Output.Action(sFixingStub, nil);
            OldSize := Size;
            Strip(False);
            if FLogStyle <> lsBrief then
              Output.StripStats(OldSize, Size);
          end;

        if roRaw in FOptions then
          RawData := sdRaw
        else
          RawData := TExeSectionData(Byte(roTrunc in FOptions) + 1);
        FImage.Build(Byte(roStrip in FOptions) * 512, RawData);

        if roOSVersion in FOptions then
          FImage.OSVersion(FMajorVersion, FMinorVersion);
        if ro3GB in FOptions then
          FImage.LargeAddressAware;
        if (roASLR in FOptions) and not FImage.ASLRAware then
          raise EBadImage.Create(sNoRelocationsForASLR);
        if roDEP in FOptions then
          FImage.DEPAware;

        if DestFileName.IsDotOrNull then
          DestFileName := FileName
        else if DestFileName = @FCurrentPath then
          FCurrentPath.ChangeFileName(FileName);

        Saved.FileSize := Loaded.FileSize;
        Saved.BytesWritten := FImage.Size(roTrunc in FOptions);
        try
          if TypeOf(DestFileName^) <> nil then
            try
              if FFileNames[fkBackup].Count <> 0 then
              begin
                if FLogStyle <> lsBrief then
                begin
                  Output.Action(sBackuping, @FFileNames[fkBackup]);
                  Console.WriteLn;
                end;
                Saved := SaveFile(FFileNames[fkBackup].RawData);
              end
              else
                Saved := SaveFile(nil);

              if FLogStyle <> lsBrief then
              begin
                if Saved.FileSize <> 0 then
                begin
                  Output.Action(sDestFile, DestFileName);
                  Output.TransferStats(Loaded.FileSize, Saved.FileSize);
                end;
                Output.Action(sSaving, DestFileName);
              end;
            finally
              if Saved.FileSize = 0 then
                Saved.FileSize := Loaded.FileSize;
              if DestFileName = FileName then
                DestFileName := @FFileNames[fkInto];
            end
          else
            if FLogStyle <> lsBrief then
              Output.Action(sEstimated, FileName);
          Inc(TotalBytes, Saved.FileSize);

          if FLogStyle <> lsBrief then
          begin
            Output.TransferStats(Saved.FileSize, Saved.BytesWritten);
            Output.Action(sTotal, nil);
          end;
          Output.StripStats(Saved.FileSize, Saved.BytesWritten);
        finally
          Inc(TotalWritten, Saved.BytesWritten);
        end;

        if roListSections in FOptions then
        begin
          ShowSections;
          if (FLogStyle <> lsBrief) and ((FileName.Prev <> nil) or (FileName.Next <> nil)) then
            Console.WriteLn;
        end;
      except
        on E: EBadImage do
          ShowImageException('%hs: %s', E);
        on E: EStream do
          ShowImageException(sUnexpectedEndOfStream, E);
        on E: EPlatform do
          ShowException(E);
      end;

      FileName := FileName.Next;
      if (FileName <> nil) and ((FLogStyle <> lsBrief) or (roListSections in FOptions)) then
        Console.WriteLn;
    end;

    if (FFoundFiles.Count > 1) and (TotalBytes <> 0) then
    begin
      Console.WriteLn;
      Output.TotalStats(FFoundFiles.Count, TotalBytes, TotalWritten);
    end;
  finally
    Output.Destroy;
    HexString.Destroy;
    TmpFileName.Destroy;
  end;
end;

procedure TApplication.SaveImage(Dest: PWritableStream);
begin
  FImage.Save(Dest, roTrunc in FOptions);
end;

function TApplication.TotalsMaxPromptWidth: Integer;
begin
  Result := StrLen(Processing[TypeOf(FFileNames[fkInto]) <> nil]);
end;

end.
