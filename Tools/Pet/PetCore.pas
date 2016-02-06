(*
    PE Tool's core

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
//  TResourceNames = TWideStringArray;
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

  TLogStyle = (lsTotals, lsActions{, lsDetails});

  TRunOption = (roPause, roNoLogo, roVersion, // ordered
    roAuto, roStrip, roTrunc, roTouch, roUnsafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon, roVerbose} ro3GB);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FSourceFileNames: TFileNameList;
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
    FMajorVersion, FMinorVersion: Word;
    FLogStyle: TLogStyle;
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
  public
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

implementation

uses
  CoreConsts, PetConsts;

const
  FileKeys: array[fkInto..High(TFileKind)] of PCoreChar = (sInto, sStub, sExtract, sBackup{, sDump});

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
  FDuplicateParam := FileName;
end;

constructor ECommandLine.Create(DuplicateParam: PLegacyChar; ParamValue: PCoreString);
begin
  inherited Create(sDuplicateParam, DefaultSystemCodePage, [DuplicateParam, ParamValue.Data]);
  FDuplicateParam := ParamValue;
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
  FConsole.WriteLn(sTotals, 0, [FileCount, DiffBytes, FPercentage.RawData]);
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
  Result := 0;
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
    Result := StrLen(TotalsMaxWidth);
end;

procedure TApplication.ParseCommandLine(Source: PCoreChar);
const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sPause, sNoLogo, sVersion, sAuto, sStrip, sTrunc, sTouch, sUnsafe, sDeep,
     {sMiniRes, sCleanVer, sMainIcon, sVerbose,} s3GB);
var
  CmdLine, Key: TCoreString;
  Param: TCommandLineParam;
  ParamCount, Dot: Integer;
  FileName: PFileName;
  W: PCoreChar;
  K: TFileKind;
  R: TRunOption;
  SourceFileName: PFileNameListItem;
begin
  CmdLine.Create;
  CmdLine.AsWideString(Source, WideStrLen(Source), soAttach);
  Param.Create;
  CmdLine := Param.AsNextParam(@CmdLine); // skip own file name

  ParamCount := 0;
  Key.Create;
  repeat
    CmdLine := Param.AsNextParam(@CmdLine);
    if Param.Count = 0 then
      Break;

    Inc(ParamCount);
    if Param.IsKey then
    begin
      Key.AsRange(@Param, 1);
      
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
              PPointer(@FFileNames[K])^ := TypeOf(TFileName); // Fast core
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
            Clear;
          end;
        end
        else
        begin
          W := sDropSect;
          if Key.Compare(W + 1, PWord(W)^, True) = 0 then
          begin
            CmdLine := Param.AsNextParam(@CmdLine);
            if Param.Count = 0 then
              raise ECommandLine.Create(sSectionNames);
            if TypeOf(FDropSections) = nil then
              FDropSections.Create;
            FACP.Create;
            LoadText(FDropSections.Append, @Param, CoreChar(','), @FACP); 
            Param.Clear;
          end
        end;
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
const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
  Touch: array[Boolean] of TSaveOptions = ([], [soTouch]);
  Processing: array[Boolean] of PLegacyChar = (sEstimating, sStripping);
var
  Stub: TExeStub;
  TmpFileName: TFileName;
  Output: TDefaultOutput;
  Loaded: TLoadFileResult;
  BytesSaved, TotalBytes, TotalSaved: QuadWord;
  ImageSize, OldSize, SectionBytes: LongWord;
  FileName: PFileNameListItem;
  DestFileName, ExtractFileName: PFileName;
  Section: PLegacyTextListItem;
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
    Inc(FMaxWidth, StrLen(DefaultMaxWidth) - StrLen(TotalsMaxWidth));

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
      if FLogStyle = lsTotals then
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
        if FLogStyle <> lsTotals then
        begin
          Output.Action(sLoading, FileName);
          Output.TransferStats(Loaded.FileSize, Loaded.BytesRead);
        end
        else
          Output.Action(Processing[DestFileName.Count <> 0], FileName);
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
            Console.WriteLn(PLegacyChar(sChainedDataFound), StrLen(sChainedDataFound));
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
              BytesSaved := SaveFile(Save, ExtractFileName.RawData, Size);
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
          FImage.Strip([soStub..soEmptySections] + Deep[roDeep in FOptions]);
          if FLogStyle <> lsTotals then
            Output.StripStats(ImageSize, FImage.Size(roTrunc in FOptions));
        end
        else
          with FImage.Stub do
          begin
            if FLogStyle <> lsTotals then
              Output.Action(sFixingStub, nil);
            OldSize := Size;
            Strip(False);
            if FLogStyle <> lsTotals then
              Output.StripStats(OldSize, Size);
          end;

        FImage.Build(Byte(roStrip in FOptions) * 512);

        if DestFileName.IsDotOrNull then
          DestFileName := FileName
        else if DestFileName = @FCurrentPath then
          FCurrentPath.ChangeFileName(FileName);

        BytesSaved := FImage.Size(roTrunc in FOptions);
        try
          if DestFileName.Count <> 0 then
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
              BytesSaved := SaveFile(
                SaveImage, FileName.RawData, FFileNames[fkBackup].RawData,
                DestFileName.RawData, FImage.Size(roTrunc in FOptions),
                faRandomRewrite, Touch[roTouch in FOptions] + [soBackup]
              );
            end
            else
            begin
              TmpFileName.AsTempName(DestFileName);
              if FLogStyle <> lsTotals then
                Output.Action(sSaving, DestFileName);
              BytesSaved := SaveFile(
                SaveImage, FileName.RawData, TmpFileName.RawData,
                DestFileName.RawData, FImage.Size(roTrunc in FOptions),
                faRandomRewrite, Touch[roTouch in FOptions]
              );
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

end.
