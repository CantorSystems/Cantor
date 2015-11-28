(*
    PE Tool's core

    Copyright (c) 2013-2015 Vladislav Javadov (aka Freeman)

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
  TSectionNames = TLegacyStrings;

  TOutput = object(TCoreObject)
  private
    FConsole: PStreamConsole;
    FPercentage: TLegacyString;
    FPercentageBuf: array[0..$F] of LegacyChar;
  public
    constructor Create(Dest: PStreamConsole);
    procedure Action(Prompt: PLegacyChar; FileName: PFileName); virtual; abstract;
    procedure StripStats(SourceSize, StrippedSize: LongWord); virtual; abstract;
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
    procedure TransferStats(SourceSize, ActualSize: LongWord); virtual;
  end;

{  TVerboseOutput = object(TOutput)
  private
    FActionFormat, FStatsFormat: PWideChar;
    FActionFixedWidth, FStatsFixedWidth: Integer;
    FPercentage: TLegacyString;
  public
    constructor Create(Dest: PStreamConsole; PromptWidth, FileNameWidth, StatBytesWidth: Word);
    destructor Destroy; virtual;
    procedure Action(Prompt: PLegacyChar; FileName: PCoreString); virtual;
    procedure Line(Double: Boolean); 
    procedure Stats(OldSize, NewSize: LongWord); virtual;

    property PromptFixedWidth: Integer read FActionFixedWidth;
    property PromptFormat: PWideChar read FActionFormat;
    property StatsFixedWidth: Integer read FStatsFixedWidth;
    property StatsFormat: PWideChar read FStatsFormat;
  end;}

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TRunOption = (roPause, roNoLogo, roVersion, // ordered
    roAuto, roStrip, roTrunc, roTouch, roUnsafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon, roVerbose} ro3GB);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FSourceFileName: TFileName; // TODO
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
    FMajorVersion, FMinorVersion: Word;
    FFileNameList: TFileNameList;
    FImage: TExeImage;
    FCurrentPath: TFileName;
    procedure AddFile(const Data: TWin32FindDataW; var Found: Boolean);
    function MaxFileNameWidth(MaxWidth: Integer): Integer;
    procedure ParseCommandLine(Source: PCoreChar);
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

  ECore = class(Exception);

implementation

uses
  CoreConsts, PetConsts;

const
  FileKeys: array[fkInto..High(TFileKind)] of PCoreChar = (sInto, sStub, sExtract, sBackup{, sDump});

{ EFileKey }

//  inherited Create(sMissingFileName, DefaultSystemCodePage, [FileKeys[MissingFileName]]);
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
{
  with FFormatter do
  begin
    Create;
    with DecimalFormats do
    begin
//      Append(0);
      Capacity := 2;
      Append(LOCALE_USER_DEFAULT);
      Append((SORT_DEFAULT shl 16) or LANG_ARABIC, True);
    end;
  end;
}

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
      FConsole.WriteLn(FActionBuf, WideFormatBuf(FActionEllipsisFormat, [Prompt, PLegacyChar(sPathEllipsis),
        FileName.RawData + FileName.PathDelimiterIndex + 1], FActionBuf), 0);
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

{ TApplication }

destructor TApplication.Destroy;
var
  K: TFileKind;
begin
  FImage.Finalize;
  FCurrentPath.Finalize;
  FFileNameList.Finalize;

  FDropSections.Finalize;
  for K := Low(FFileNames) to High(FFileNames) do
    FFileNames[K].Finalize;
  FSourceFileName.Finalize;
  inherited;
end;

procedure TApplication.AddFile(const Data: TWin32FindDataW; var Found: Boolean);
var
  Item: PFileNameListItem;
begin
  New(Item, Create);
  Item.AsRange(@FCurrentPath, 0);
  with Data do
    Item.ChangeFileName(cFileName, WideStrLen(cFileName, Length(cFileName)));
  FFileNameList.Append(Item);
end;

function TApplication.MaxFileNameWidth(MaxWidth: Integer): Integer;
var
  K: TFileKind;
  Width: Integer;
begin
  Result := FSourceFileName.Width(MaxWidth);
  for K := Low(FFileNames) to High(FFileNames) do
  begin
    Width := FFileNames[K].Width(MaxWidth);
    if Width > Result then
      Result := Width;
  end;
end;

procedure TApplication.ParseCommandLine(Source: PCoreChar);
const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sPause, sNoLogo, sVersion, sAuto, sStrip, sTrunc, sTouch, sUnsafe, sDeep,
     {sMiniRes, sCleanVer, sMainIcon, sVerbose,} s3GB);
  HexBase: array[Boolean] of LegacyChar = 'A0';
var
  CmdLine, Key: TCoreString;
  Param: TCommandLineParam;
  ParamCount, Dot: Integer;
  FileName: PFileName;
  W: PCoreChar;
  K: TFileKind;
  R: TRunOption;
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
          W := sSectionNames;
          if Key.Compare(W + 1, PWord(W)^, True) = 0 then
          begin
            CmdLine := Param.AsNextParam(@CmdLine);
            if Param.Count = 0 then
              raise ECommandLine.Create(sSectionNames);
            with Param do
            begin
              // FSectionNames bla-bla-bla
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
      PPointer(@FSourceFileName)^ := TypeOf(TFileName); // Fast core
      with FSourceFileName do
      begin
        AsRange(@Param, 0);
        Detach;
      end;
    end;
  until False;

  if ParamCount = 0 then
    Include(FOptions, roPause)
  else
  begin
    with FSourceFileName do
    begin
      if (Count = 0) and not (roVersion in FOptions) then
        raise ECommandLine.Create(fkSource);
      if IsDotOrNull then
      begin
        AsRange(@ExeName, 0);
        Detach;
      end;
    end;

    FileName := @FFileNames[fkInto];
    if FileName.IsDotOrNull then
      FileName.AsRange(@FSourceFileName, 0);

    FileName := @FFileNames[fkStub];
    if FileName.IsDotOrNull then
      FileName.AsRange(@ExeName, 0);
  end;
end;

procedure TApplication.Run(CommandLine: PCoreChar);
const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
  Touch: array[Boolean] of TSaveOptions = ([], [soTouch]);
var
  Stub: TExeStub;
  TmpFileName: TFileName;
  Output: TDefaultOutput;
  Loaded: TLoadFileResult;
  BytesSaved: QuadWord;
  ImageSize, OldSize: LongWord;
  FileName: PFileNameListItem;
begin
  ParseCommandLine(CommandLine);

  if Logo(sLogo) then
    Exit;

  if FSourceFileName.Count = 0 then
  begin
    Help(sUsage, sHelp);
    Exit;
  end;

  PPointer(@FCurrentPath)^ := TypeOf(TFileName); // Fast core
  with FCurrentPath do
  begin
    AsRange(@FSourceFileName, 0, FSourceFileName.PathDelimiterIndex + 1);
    Detach;
  end;

  FFileNameList.Create;
  if FindFiles(AddFile, FSourceFileName.RawData) = 0 then
  begin
    Console.WriteLn(sNoFilesFound, 0, [FSourceFileName.RawData]);
    Exit;
  end;

  FImage.Create;
  Output.Create(@Console, PromptMaxWidth, MaxFileNameWidth(40), Ceil(Log10(Loaded.FileSize)) + 1); // TODO
  try
    FileName := FFileNameList.First;
    while FileName <> nil do
    begin
      Console.WriteLn;
      try
        Loaded := LoadFile(FImage.Load, FileName.RawData, faRandomRead);
        Output.Action(sLoading, FileName);
        Output.TransferStats(Loaded.FileSize, Loaded.BytesRead);

        ImageSize := FImage.Size(False);
        Output.Action(sImageData, nil);
        Output.TransferStats(Loaded.BytesRead, ImageSize);

        if Loaded.FileSize <> Loaded.BytesRead then // chained data found
        begin
          Output.Action(sChainedData, nil);
          Output.StripStats(Loaded.FileSize, Loaded.BytesRead);
          if not (roUnsafe in FOptions) then
          begin
            Console.WriteLn(PLegacyChar(sChainedDataFound), StrLen(sChainedDataFound)); // TODO?
            Exit;
          end;
        end;

        if FFileNames[fkExtract].Count <> 0 then
          with Stub do
          begin
            Create;
            try
              Output.Action(sExtractingStub, @FFileNames[fkExtract]);
              Load(@FImage.Stub);
              Strip(roStrip in FOptions);
              Output.TransferStats(0, SaveFile(Save, FFileNames[fkExtract].RawData, Size));
            finally
              Destroy;
            end;
          end;

        if FFileNames[fkStub].Count <> 0 then
          with FImage.Stub do
          begin
            Output.Action(sReplacingStub, @FFileNames[fkStub]);
            OldSize := Size;
            LoadFile(Load, FFileNames[fkStub].RawData);
            Output.TransferStats(OldSize, Size);
          end;

    {    if FDropSections <> nil then
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
        end;}

        if roStrip in FOptions then
        begin
          Output.Action(sStripping, nil);
          FImage.Strip([soStub..soEmptySections] + Deep[roDeep in FOptions]);
          Output.StripStats(ImageSize, FImage.Size(roTrunc in FOptions));
        end
        else
          with FImage.Stub do
          begin
            Output.Action(sFixingStub, nil);
            OldSize := Size;
            Strip(False);
            Output.StripStats(OldSize, Size);
          end;

        FImage.Build(Byte(roStrip in FOptions) * 512);

        if FFileNames[fkInto].Count <> 0 then
        begin
          Output.Action(sSaving, @FFileNames[fkInto]);

          with FImage do
          begin
            if FMajorVersion <> 0 then
              OSVersion(FMajorVersion, FMinorVersion);
            if ro3GB in FOptions then
              LargeAddressAware;
          end;

          if FFileNames[fkBackup].Count <> 0 then
            BytesSaved := SaveFile(
              SaveImage, FSourceFileName.RawData, FFileNames[fkBackup].RawData,
              FFileNames[fkInto].RawData, FImage.Size(roTrunc in FOptions),
              faRandomRewrite, Touch[roTouch in FOptions] + [soBackup]
            )
          else
          begin
            TmpFileName.Create;
            try
              TmpFileName.AsTempName(@FFileNames[fkInto]);
              BytesSaved := SaveFile(
                SaveImage, FileName.RawData, TmpFileName.RawData,
                FFileNames[fkInto].RawData, FImage.Size(roTrunc in FOptions),
                faRandomRewrite, Touch[roTouch in FOptions]
              );
            finally
              TmpFileName.Destroy;
            end;
          end;

          Output.TransferStats(0, BytesSaved);
        end
        else
          BytesSaved := FImage.Size(roTrunc in FOptions);

        Output.Action(sTotal, nil);
        Output.StripStats(Loaded.FileSize, BytesSaved);
      except
        on E: EPlatform do
          ShowException(E);
      end;
      FileName := FileName.Next;
    end;
  finally
    Output.Destroy;
  end;

  Console.WriteLn(sTotalFiles, 0, [FFileNameList.Count])
end;

procedure TApplication.SaveImage(Dest: PWritableStream);
begin
  FImage.Save(Dest, roTrunc in FOptions);
end;

end.
