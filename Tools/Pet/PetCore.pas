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
    procedure LoadStats(SourceSize, ActualSize: LongWord); virtual; abstract;
    procedure StripStats(SourceSize, StrippedSize: LongWord); virtual; abstract;
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
    procedure LoadStats(SourceSize, ActualSize: LongWord); virtual;
    procedure StripStats(SourceSize, StrippedSize: LongWord); virtual;
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
    roAuto, roStrip, roTrunc, roKeep, roUnsafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon,} ro3GB, roVerbose);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FSourceFileName: TFileName;
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
    FMajorVersion, FMinorVersion: Word;
    FImage: TExeImage;
    function MaxFileNameWidth(MaxWidth: Integer): Integer;
    procedure Parse(CommandLine: PCoreChar);
    procedure ProcessFile(FileName: PCoreString);
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
    [PromptWidth, -FileNameWidth]).Value;
  FActionEllipsisFormat := Format(sDefaultActionFmt, DefaultSystemCodePage, 0,
    [PromptWidth, -(FileNameWidth - Length(sPathEllipsis))]).Value;
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
      FConsole.WriteLn(FActionBuf, WideFormatBuf(FActionFormat, [Prompt, PLegacyChar(sPathEllipsis),
        FileName.RawData + FileName.PathDelimiterIndex], FActionBuf), 0);
      Exit;
    end
    else
      W := FileName.RawData
  else
    W := nil;
  FConsole.WriteLn(FActionBuf, WideFormatBuf(FActionFormat, [Prompt, nil, W], FActionBuf), 0);
end;

procedure TDefaultOutput.LoadStats(SourceSize, ActualSize: LongWord);
begin
  FPercentage.AsPercentage(ActualSize / SourceSize);
  FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [SourceSize, FPercentage.RawData], FStatsBuf));
end;

procedure TDefaultOutput.StripStats(SourceSize, StrippedSize: LongWord);
var
  DiffBytes: LongWord;
begin
  DiffBytes := SourceSize - StrippedSize;
  FPercentage.AsPercentage(DiffBytes / SourceSize);
  FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [DiffBytes, FPercentage.RawData], FStatsBuf));
end;

{ TApplication }

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

procedure TApplication.Parse(CommandLine: PCoreChar);
const
  OptionKeys: array[TRunOption] of PCoreChar =
    (sPause, sNoLogo, sVersion, sAuto, sStrip, sTrunc, sKeep, sUnsafe, sDeep,
     {sMiniRes, sCleanVer, sMainIcon,} s3GB, sVerbose);
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
      with FSourceFileName do
      begin
        Create;
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

procedure TApplication.ProcessFile(FileName: PCoreString);
const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
var
  Stub: TExeStub;
  TmpFileName: TFileName;
  Output: TDefaultOutput;
  Loaded: TLoadFileResult;
begin
  FImage.Create;
  try
    Loaded := LoadFile(FImage.Load, FileName.Data, faRandomRead);
    Output.Create(@Console, PromptMaxWidth, MaxFileNameWidth(40), Ceil(Log10(Loaded.FileSize)));
    try
      with Output do
      begin
        Action(sLoading, @FSourceFileName);
        LoadStats(Loaded.BytesRead, FImage.Size(False));
        if Loaded.FileSize <> Loaded.BytesRead then
        begin
          Action(sOverlayData, nil);
          StripStats(Loaded.FileSize, Loaded.BytesRead);
          if not (roUnsafe in FOptions) then
          begin
            FConsole.WriteLn(PLegacyChar(sOverlayDataFound), StrLen(sOverlayDataFound));
            Exit;
          end;
          // TODO: overlay data found
        end;
      end;
    finally
      Output.Destroy;
    end;
(*    Processing(sLoadingSource, @FSourceFileName);
//      LoadFile(nil, FSourceFileName.RawData, faRead, @Data, LoadImage); SysUtils
    with FImage do
    begin
      ActualSize := Size(False);
      StrippedSize := Size(roTrunc in FOptions);
    end;
//      Stats(sImageData, Data.FileSize, ActualSize, StrippedSize);

    if FFileNames[fkExtract].Count <> 0 then
    begin
      Stub.Create;
      try
//          Stub.Load(@Data.Image.Stub);
        Stub.Strip(roStrip in FOptions);
        Processing(sExtractingStub, @FFileNames[fkExtract]);
        SaveFile(Stub.Save, FFileNames[fkExtract].RawData, Stub.Size);
      finally
        Stub.Destroy;
      end;
    end;

    if FFileNames[fkStub].Count <> 0 then
    begin
//        LoadFile(Data.Image.Stub.Load, FFileNames[fkStub].RawData);
//      Processing([sInsertingStub, FFileNames[fkStub], Size]);
    end;

    if roStrip in FOptions then
    begin
//        Data.Image.Strip([soStub..soEmptySections] + Deep[roDeep in FOptions]);
//      NewSize := OldSize - Data.Image.Size(roTrunc in FOptions);
//        Processing([sStripping, Percentage(NewSize / OldSize), NewSize], False);
    end
    else
//        with Data.Image.Stub do
      begin
        RawSize := Size;
//          Strip(False);
        FixedSize := Size;
//        Processing([sFixingStub, Percentage(FixedSize / RawSize), FixedSize], False);
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

    if FFileNames[fkInto].Count <> 0 then
    begin
      if FFileNames[fkBackup].Count <> 0 then
      begin
        Console.WriteLn(sBackuping, 0, [FFileNames[fkBackup].RawData]);
        if not MoveFileExW(FSourceFileName.RawData, FFileNames[fkBackup].RawData,
          MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH)
        then
          RaiseLastPlatformError(FFileNames[fkBackup].RawData);
      end;

      with FImage do
      begin
        if FMajorVersion <> 0 then
          OSVersion(FMajorVersion, FMinorVersion);
        if ro3GB in FOptions then
          LargeAddressAware;
        Build(Byte(roStrip in FOptions) * 512);
        NewSize := Size(roTrunc in FOptions);
      end;

//      Processing([sSavingInto, FFileNames[fkInto], NewSize]);
      if FFileNames[fkBackup].Count <> 0 then
//          SaveFile(nil, FFileNames[fkInto].RawData, NewSize, faRewrite, @Data.Image, nil, SaveImage)
      else
      begin
        TmpFileName.Create;
        try
          TmpFileName.AsTempName(@FFileNames[fkInto]);
//            SaveFile(nil, TmpFileName.RawData, NewSize, faRewrite, @Data.Image, nil, SaveImage);
          if not MoveFileExW(TmpFileName.RawData, FFileNames[fkInto].RawData,
            MOVEFILE_COPY_ALLOWED or MOVEFILE_WRITE_THROUGH or MOVEFILE_REPLACE_EXISTING)
          then
            RaiseLastPlatformError(TmpFileName.RawData);
        finally
          TmpFileName.Destroy;
        end;
      end;

      Console.WriteLn;
//      Processing([sTotal, Percentage(NewSize / OldSize), OldSize - NewSize], False);
    end;*)
  finally
  {$IFDEF Lite}
    FImage.Destroy;
  {$ELSE}
    FreeAndNil(FImage);
  {$ENDIF}
  end;
end;

procedure TApplication.Run(CommandLine: PCoreChar);
begin
  Parse(CommandLine);
  if Logo(sLogo) then
    Exit;
  if FSourceFileName.Count <> 0 then
    ProcessFile(@FSourceFileName)
  else
    Help(sUsage, sHelp);
end;

procedure TApplication.SaveImage(Dest: PWritableStream);
begin
  FImage.Save(Dest, roTrunc in FOptions);
end;

end.

