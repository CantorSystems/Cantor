(*
    PE Tool core

    Copyright (c) 2013-2016 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Lite -- avoid redundant bloatness of code, without functionality loss 
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
    FPercentage: TLegacyString;
    FPercentageBuf: array[0..$F] of LegacyChar;
  public
    constructor Create(Dest: PStreamConsole);
    procedure Action(Prompt: PLegacyChar; FileName: PFileName); virtual; abstract;
    procedure StripStats(SourceSize, StrippedSize: LongWord); virtual; abstract;
    procedure TotalStats(FileCount, TotalBytes, BytesSaved: LongWord); virtual; abstract;
    procedure TransferStats(SourceSize, ActualSize: LongWord); virtual; abstract;
  end;

  TDefaultOutput = object(TOutput)
  private
    FActionFormat, FActionEllipsisFormat, FStatsFormat,
    FActionBuf, FStatsBuf: PWideChar;
    FFileNameWidth: Integer;
  public
    constructor Create(Dest: PStreamConsole; PromptWidth, FileNameWidth, StatBytesWidth: Integer);
    destructor Destroy; virtual;
    procedure Action(Prompt: PLegacyChar; FileName: PFileName); virtual;
    procedure StripStats(SourceSize, StrippedSize: LongWord); virtual;
    procedure TotalStats(FileCount, TotalBytes, BytesSaved: LongWord); virtual;
    procedure TransferStats(SourceSize, ActualSize: LongWord); virtual;
  end;

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TLogStyle = (lsAuto, lsTotals, lsActions);

  TRunOption = (roPause, roNoLogo, roVersion, // ordered
    {roAuto,} roStrip, roTrunc, roTouch, roUnsafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon, roVerbose} ro3GB, roListSections, roMenuet);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
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
    FMaxSize: QuadWord;
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
  end;

{ Exceptions }

  TCmdLineFileKind = fkSource..High(TFileKind);

  ECommandLine = class(Exception)
  private
    FInvalidParam: PCoreString;
    FFileKind: TFileKind;
  public
    constructor Create(MissingKind: TCmdLineFileKind); overload;
    constructor Create(MissingParam: PLegacyChar); overload;

    constructor Create(DuplicateKind: TCmdLineFileKind; FileName: PCoreString); overload;
    constructor Create(DuplicateParam: PLegacyChar; ParamValue: PCoreString); overload;

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

  EMenuet = class(Exception)
  public
    constructor Create;
  end;

implementation

uses
  CoreConsts, PetConsts;

const
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
  inherited Create(sDuplicateParam, DefaultSystemCodePage, [Kind, FileName.Data]);
  FFileKind := DuplicateKind;
  FInvalidParam := FileName;
end;

constructor ECommandLine.Create(DuplicateParam: PLegacyChar; ParamValue: PCoreString);
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

constructor TOutput.Create(Dest: PStreamConsole);
begin
  InitInstance;
  FConsole := Dest;
  FPercentage.Create;
  FPercentage.ExternalBuffer(@FPercentageBuf, Length(FPercentageBuf));
end;

{ TDefaultOutput }

constructor TDefaultOutput.Create(Dest: PStreamConsole;
  PromptWidth, FileNameWidth, StatBytesWidth: Integer);
begin
  inherited Create(Dest);

  FActionFormat := Format(sDefaultActionFmt, DefaultSystemCodePage, 0,
    [PromptWidth, -FileNameWidth - Length(sPathEllipsis)]).Value;
  FActionEllipsisFormat := Format(sDefaultActionFmt, DefaultSystemCodePage, 0,
    [PromptWidth, -FileNameWidth]).Value;
  FFileNameWidth := FileNameWidth;
  GetMem(FActionBuf, (PromptWidth + FileNameWidth + Length(sPathEllipsis) +
    Length(sDefaultActionFmt)) * SizeOf(WideChar));

  FStatsFormat := Format(sDefaultStatsFmt, DefaultSystemCodePage, 0, [StatBytesWidth]).Value;
  GetMem(FStatsBuf, (StatBytesWidth + PercentageWidth + Length(sDefaultStatsFmt)) * SizeOf(WideChar));
end;

destructor TDefaultOutput.Destroy;
begin
  FreeMem(FStatsBuf);
  FreeMem(FActionBuf);

  FreeMem(FStatsFormat);
  FreeMem(FActionEllipsisFormat);
  FreeMem(FActionFormat);

  inherited;
end;

procedure TDefaultOutput.Action(Prompt: PLegacyChar; FileName: PFileName);
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

procedure TDefaultOutput.TransferStats(SourceSize, ActualSize: LongWord);
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

procedure TDefaultOutput.StripStats(SourceSize, StrippedSize: LongWord);
var
  DiffBytes: LongInt;
begin
  DiffBytes := StrippedSize - SourceSize;
  FPercentage.AsPercentage(DiffBytes / SourceSize);
  FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [DiffBytes, FPercentage.RawData], FStatsBuf));
end;

procedure TDefaultOutput.TotalStats(FileCount, TotalBytes, BytesSaved: LongWord);
var
  DiffBytes: LongInt;
begin
  DiffBytes := BytesSaved - TotalBytes;
  FPercentage.AsPercentage(DiffBytes / TotalBytes);
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
  New(Item, Create);
  Item.AsRange(@FCurrentPath, 0);
  with Data do
  begin
    Item.ChangeFileName(cFileName, WideStrLen(cFileName, Length(cFileName)));
    FileSize := nFileSizeHigh shr 32 or nFileSizeLow;
    if FileSize > FMaxSize then
      FMaxSize := FileSize;
  end;
  Width := Item.Width(FMaxWidth);
  if Width > FMaxWidth then
    FMaxWidth := Width;
  FFoundFiles.Append(Item);
end;

function TApplication.MaxFileNameWidth(MaxWidth: Integer): Integer;
var
  K: TFileKind;
  Width: Integer;
begin
  Result := Byte((FRebaseAddress <> 0) or (roMenuet in FOptions)) * 8;
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
  if FLogStyle <> lsTotals then
  begin
    Result := StrLen(DefaultMaxWidth);
    if (FRebaseAddress <> 0) or (roMenuet in FOptions) then
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
  end
  else
    Result := TotalsMaxPromptWidth;
end;

procedure TApplication.ParseCommandLine(Source: PCoreChar);
const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sPause, sNoLogo, sVersion, sStrip, sTrunc, sTouch, sUnsafe, sDeep,
     {sMiniRes, sCleanVer, sMainIcon, sVerbose,} s3GB, sListSect, sMenuet);
var
  CmdLine: TCoreString;
  Key, Param: TCommandLineParam;
  ParamCount, Dot: Integer;
  FileName: PFileName;
  K: TFileKind;
  R: TRunOption;
  SourceFileName: PFileNameListItem;
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

      for R := Low(OptionKeys) to High(OptionKeys) do
        if Key.Equals(OptionKeys[R]) then
        begin
          if (R = roMenuet) and (FRebaseAddress <> 0) then
            raise EMenuet.Create;
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
              raise ECommandLine.Create(K);
            with FFileNames[K] do
            begin
              if Count <> 0 then
                raise ECommandLine.Create(K, @Param);
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
        if Param.IsKey then
          Continue;
      end;

      if Param.Count <> 0 then
      begin
        if Key.Equals(sOSVer) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sOSVersion);
          if FMajorVersion <> 0 then
            raise ECommandLine.Create(sOSVersion, @Param);
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
        end
        else if Key.Equals(sRebase) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sRebaseAddress);
          if FRebaseAddress <> 0 then
            raise ECommandLine.Create(sRebaseAddress, @Param)
          else if roMenuet in FOptions then
            raise EMenuet.Create;
          FRebaseAddress := Param.AsHexadecimal;
          if FRebaseAddress mod $10000 <> 0 then
            raise EImageBase.Create(FRebaseAddress);
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
            raise ECommandLine.Create(sLogStyle);
          if FLogStyle <> lsAuto then
            raise ECommandLine.Create(sLogStyle, @Param);
          if Param.Equals(sActions) then
            FLogStyle := lsActions
          else if Param.Equals(sTotals) then
            FLogStyle := lsTotals
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
      if FSourceFileNames.Count = 0 then
        FSourceFileNames.Create;
      New(SourceFileName, Create);
      with SourceFileName^ do
      begin
        AsRange(@Param, 0);
        Detach;
      end;
      FSourceFileNames.Append(SourceFileName);
      Param.Clear;
    end;
  until False;

  if ParamCount <> 0 then
  begin
    if (FSourceFileNames.Count = 0) and not (roVersion in FOptions) then
      raise ECommandLine.Create(fkSource);
    FileName := @FFileNames[fkStub];
    if FileName.IsDotOrNull then
      FileName.AsRange(@ExeName, 0);
  end
  else
    Include(FOptions, roPause)
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
  DestFileName: PFileName;
  FileName: PFileNameListItem;
  Output: TDefaultOutput;

function SaveFile(SwapFileName: PFileName): QuadWord;
const
  Backup: array[Boolean] of TSaveOptions = ([], [soBackup]);
  Touch: array[Boolean] of TSaveOptions = ([], [soTouch]);
var
  MenuetImage: TMenuetImage;
begin
  if roMenuet in FOptions then
    with MenuetImage do
    begin
      Create;
      try
        Load(@FImage);
        Build;
        Result := CoreWrappers.SaveFile(
          Save, FileName.RawData, SwapFileName.RawData, DestFileName.RawData,
          Header.ImageSize, faRandomRewrite,
          Touch[roTouch in FOptions] + Backup[FFileNames[fkBackup].Count <> 0]
        );
      finally
        Destroy;
      end;
    end
  else
    Result := CoreWrappers.SaveFile(
      SaveImage, FileName.RawData, SwapFileName.RawData, DestFileName.RawData,
      FImage.Size(roTrunc in FOptions), faRandomRewrite,
      Touch[roTouch in FOptions] + Backup[FFileNames[fkBackup].Count <> 0]
    );
end;

procedure RebaseImage(MenuetStyle: Boolean);
begin
  if FLogStyle <> lsTotals then
  begin
    TmpFileName.AsHexadecimal(FRebaseAddress, -8, False, CoreChar('0'));
    Output.Action(sRebasingTo, @TmpFileName);
  end;
  if FImage.Rebase(FRebaseAddress div $10000, MenuetStyle) = 0 then
    Console.WriteLn(sCannotRebaseImage, 0, [FileName.RawData])
  else if FLogStyle <> lsTotals then
    Console.WriteLn;
end;

const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
  Relocations: array[Boolean] of TStripOptions = ([], [soRelocations]);
var
  Stub: TExeStub;
  Loaded: TLoadFileResult;
  BytesSaved, TotalBytes, TotalSaved: QuadWord;
  ImageSize, OldSize, SectionBytes: LongWord;
  ExtractFileName: PFileName;
  Section: PLegacyTextListItem;
  I: Integer;
begin
  ParseCommandLine(CommandLine);

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
  if FLogStyle = lsTotals then
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
        Console.WriteLn(PLegacyChar(sNoFilesFound), StrLen(sNoFilesFound));
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

  TmpFileName.Create;                                                      // ,-- sign
  Output.Create(@Console, MaxPromptWidth, FMaxWidth, Ceil(Log10(FMaxSize)) +  1);
  try
    TotalBytes := 0;
    TotalSaved := 0;
    FileName := FFoundFiles.First;
    while FileName <> nil do
    begin
      try
        Loaded := LoadFile(FImage.Load, FileName.RawData, faRandomRead);

        if roListSections in FOptions then
        begin
          Console.WriteLn(sSectionList, 0, [FileName.RawData]);
          for I := 0 to FImage.Count - 1 do
            with FImage.Sections[I] do
            begin
              Console.WriteLn('  %8hs', 0, [Header.Name], 0);
              Output.TransferStats(Loaded.FileSize, Size);
            end;
          FileName := FileName.Next;
          if FileName <> nil then
            Console.WriteLn;
          Continue;
        end;

        if FLogStyle <> lsTotals then
        begin
          Output.Action(sLoading, FileName);
          Output.TransferStats(Loaded.FileSize, Loaded.BytesRead);
        end
        else
          Output.Action(Processing[TypeOf(DestFileName^) <> nil], FileName);
        Inc(TotalBytes, Loaded.FileSize);

        ImageSize := FImage.Size(False);
        if FLogStyle <> lsTotals then
        begin
          Output.Action(sImageData, nil);
          Output.TransferStats(Loaded.BytesRead, ImageSize);
        end;

        if Loaded.FileSize <> Loaded.BytesRead then // chained data found
        begin
          if FLogStyle <> lsTotals then
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
            Console.WriteLn(PLegacyChar(sChainedDataFound), StrLen(sChainedDataFound),
              1 + Byte(FileName.Next <> nil));
            Inc(TotalSaved, Loaded.FileSize);
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
            if FLogStyle <> lsTotals then
              Output.Action(sExtractingStub, ExtractFileName);
            with Stub do
            begin
              Load(@FImage.Stub);
              Strip(roStrip in FOptions);
              BytesSaved := CoreWrappers.SaveFile(Save, ExtractFileName.RawData, Size);
            end;
            if FLogStyle <> lsTotals then
              Output.TransferStats(0, BytesSaved);
          finally
            Stub.Destroy;
          end;
        end;

        if FFileNames[fkStub].Count <> 0 then
          with FImage.Stub do
          begin
            if FLogStyle <> lsTotals then
              Output.Action(sReplacingStub, @FFileNames[fkStub]);
            OldSize := Size;
            LoadFile(Load, FFileNames[fkStub].RawData);
            if FLogStyle <> lsTotals then
              Output.StripStats(OldSize, Size);
          end;

        if (FRebaseAddress <> 0) and not (roMenuet in FOptions) then
          RebaseImage(False);

        if FDropSections.Count <> 0 then
        begin
          Section := FDropSections.First;
          while Section <> nil do
          begin
            with Section^ do
              SectionBytes := FImage.Delete(RawData, Count);
            if FLogStyle <> lsTotals then
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
          if FLogStyle <> lsTotals then
            Output.Action(sStripping, nil);
          FImage.Strip([soStub..soEmptySections] - Relocations[roMenuet in FOptions] + Deep[roDeep in FOptions]);
          if FLogStyle <> lsTotals then
            Output.StripStats(ImageSize, FImage.Size(roTrunc in FOptions));
        end
        else if not (roMenuet in FOptions) then
          with FImage.Stub do
          begin
            if FLogStyle <> lsTotals then
              Output.Action(sFixingStub, nil);
            OldSize := Size;
            Strip(False);
            if FLogStyle <> lsTotals then
              Output.StripStats(OldSize, Size);
          end;

        if roMenuet in FOptions then
        begin
          RebaseImage(True);
          FImage.Strip([soRelocations]);
        end
        else
          FImage.Build(Byte(roStrip in FOptions) * 512);

        if DestFileName.IsDotOrNull then
          DestFileName := FileName
        else if DestFileName = @FCurrentPath then
          FCurrentPath.ChangeFileName(FileName);

        BytesSaved := FImage.Size(roTrunc in FOptions);
        try
          if TypeOf(DestFileName^) <> nil then
          try
            with FImage do
            begin
              if FMajorVersion <> 0 then
                OSVersion(FMajorVersion, FMinorVersion);
              if ro3GB in FOptions then
                LargeAddressAware;
            end;

            if FFileNames[fkBackup].Count <> 0 then
            begin
              if FLogStyle <> lsTotals then
              begin
                Output.Action(sBackuping, @FFileNames[fkBackup]);
                Console.WriteLn;
                Output.Action(sSaving, DestFileName);
              end;
              BytesSaved := SaveFile(@FFileNames[fkBackup]);
            end
            else
            begin
              TmpFileName.AsTempName(DestFileName);
              if FLogStyle <> lsTotals then
                Output.Action(sSaving, DestFileName);
              BytesSaved := SaveFile(@TmpFileName);
            end;

            if FLogStyle <> lsTotals then
              Output.TransferStats(Loaded.FileSize, BytesSaved);
          finally
            if DestFileName = FileName then
              DestFileName := @FFileNames[fkInto];
          end;

          if FLogStyle <> lsTotals then
            Output.Action(sTotal, nil);
          Output.StripStats(Loaded.FileSize, BytesSaved);
        finally
          Inc(TotalSaved, BytesSaved);
        end;
      except
        on E: EBadImage do
          Console.WriteLn('%hs: %s', 0, [E.Message.AsString, FileName.RawData]);
        on E: EPlatform do
          ShowException(E);
      end;

      FileName := FileName.Next;
      if (FileName <> nil) and (FLogStyle <> lsTotals) then
        Console.WriteLn;
    end;

    if (FFoundFiles.Count > 1) and (TotalBytes <> 0) then
      Output.TotalStats(FFoundFiles.Count, TotalBytes, TotalSaved);
  finally
    Output.Destroy;
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

{ EMenuet }

constructor EMenuet.Create;
begin
  inherited Create(sMenuetAt0);
end;

end.
