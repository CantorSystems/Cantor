(*
    PE Tool's core

    Copyright (c) 2013-2015 Vladislav Javadov (aka Freeman)
*)

unit PetCore;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreStrings, CoreApp, ExeImages;

type
//  TResourceNames = TWideStringArray;
  TSectionNames = TLegacyStrings;

  PFileName = ^TFileName;
  TFileName = object(TCoreString)
  public
    procedure AsTempName(Source: PCoreString);
    function IsDotOrNull: Boolean;
  end;

  PImageData = ^TImageData;
  TImageData = record
    Image: TExeImage;
    FileSize: LongWord;
  end;

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TRunOption = (roPause, roNoLogo, roVersion, // ordered
    roAuto, roStrip, roTrunc, roKeep, roUnsafe, roDeep,
    {roMiniRes, roCleanVer, roMainIcon,} ro3GB);
  TRunOptions = set of TRunOption;

  TApplication = object(TConsoleApplication)
  private
  { hold } FOptions: TRunOptions;
    FSourceFileName: TCoreString;
    FFileNames: TFileNames;
    FDropSections: TSectionNames;
//    FImageBase: CoreInt;
    FMajorVersion, FMinorVersion: Word;
    procedure Parse(CommandLine: PCoreChar);
    procedure ProcessFile(FileName: PCoreString);
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
  CoreConsts, PetConsts, CoreClasses;

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

{ TFileName }

procedure TFileName.AsTempName(Source: PCoreString);
type
  PPostfix = ^TPostfix;
  TPostfix = array[0..3] of CoreChar;
const
  Postfix: TPostfix = ('.', '$', '$', '$');
begin
  AsRange(Source, 0);
  Capacity := Count + Length(Postfix);
  PPostfix(RawData + Count)^ := Postfix;
  Append(Length(Postfix));
  RawData[Count] := #0;
end;

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
    (sPause, sNoLogo, sVersion, sAuto, sStrip, sTrunc, sKeep, sUnsafe, sDeep,
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
    if (FSourceFileName.Count = 0) and not (roVersion in FOptions) then
      raise ECommandLine.Create(fkSource);

    FileName := @FFileNames[fkInto];
    if FileName.IsDotOrNull then
      FileName.AsRange(@FSourceFileName, 0);

    FileName := @FFileNames[fkStub];
    if FileName.IsDotOrNull then
      FileName.AsRange(@ExeName, 0);
  end;
end;

procedure TApplication.ProcessFile(FileName: PCoreString);

var
  Ratio: TLegacyString;

procedure Processing(Source: PLegacyChar; FileName: PCoreString);
begin
  Console.WriteLn('%hs: %s', 0, [Source, FileName.RawData]);
end;

procedure Stats(Source: PLegacyChar; Read, Actual, Stripped: LongWord);
begin
  Ratio.AsPercentage(Stripped / Read);
  Console.WriteLn(sDataFmt, DataFixedWidth, [Read, Actual, Stripped, Ratio.RawData]);
end;

const
  Deep: array[Boolean] of TStripOptions = ([], [soOrphanedSections]);
var
  Data: TImageData;
  Stub: TExeStub;
  I, Idx: Integer;
  OldSize, NewSize, RawSize, FixedSize: LongWord;
  ActualSize, StrippedSize, Size: LongWord;
{  FileInfo: TWin32FindDataW;
  hInfo: THandle;}
  TmpFileName: TFileName;
begin
  Data.Image.Create;
  try
    Ratio.Create;
    try
{    hInfo := FindFirstFileW(FSourceFileName, FileInfo);
    if hInfo = INVALID_HANDLE_VALUE then
      RaiseLastPlatformError(FSourceFileName);
    FindClose(hInfo);}

      Processing(sLoadingSource, @FSourceFileName);
//      LoadFile(nil, FSourceFileName.RawData, faRead, @Data, LoadImage); SysUtils
      with Data.Image do
      begin
        ActualSize := Size(False);
        StrippedSize := Size(roTrunc in FOptions);
      end;
      Stats(sImageData, Data.FileSize, ActualSize, StrippedSize);

      if FFileNames[fkExtract].Count <> 0 then
      begin
        Stub.Create;
        try
          Stub.Load(@Data.Image.Stub);
          Stub.Strip(roStrip in FOptions);
          Processing(sExtractingStub, @FFileNames[fkExtract]);
          SaveFile(Stub.Save, FFileNames[fkExtract].RawData, Stub.Size);
        finally
          Stub.Destroy;
        end;
      end;

      if FFileNames[fkStub].Count <> 0 then
      begin
        LoadFile(Data.Image.Stub.Load, FFileNames[fkStub].RawData);
  //      Processing([sInsertingStub, FFileNames[fkStub], Size]);
      end;

      if roStrip in FOptions then
      begin
        Data.Image.Strip([soStub..soEmptySections] + Deep[roDeep in FOptions]);
  //      NewSize := OldSize - Data.Image.Size(roTrunc in FOptions);
  //        Processing([sStripping, Percentage(NewSize / OldSize), NewSize], False);
      end
      else
        with Data.Image.Stub do
        begin
          RawSize := Size;
          Strip(False);
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

        with Data.Image do
        begin
          if FMajorVersion <> 0 then
            with (@Headers.OptionalHeader)^ do // WTF?
            begin
              MajorOSVersion := FMajorVersion;
              MinorOSVersion := FMinorVersion;
              MajorSubsystemVersion := FMajorVersion;
              MinorSubsystemVersion := FMinorVersion;
            end;
          if ro3GB in FOptions then
            with (@Headers.FileHeader)^ do // WTF?
              Characteristics := Characteristics or IMAGE_FILE_LARGE_ADDRESS_AWARE;
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
      end;
    finally
      Ratio.Destroy;
    end;
  finally
    Data.Image.Destroy;
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

{procedure TApplication.LoadImage(Source: PReadableStream; CustomData: Pointer);
begin
  with PImageData(CustomData)^ do
  begin
    Image.Load(Source);
    FileSize := Source.Size;
  end;
end;

procedure TApplication.SaveImage(Dest: PWritableStream; CustomData: Pointer);
begin
  PExeImage(CustomData).Save(Dest, roTrunc in FOptions);
end;}

end.

