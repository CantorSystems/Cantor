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
    procedure TransferStats(SourceSize, ActualSize: LongWord; LineBreaks: Integer = 1);
  end;

  TFileKind = (fkNone, fkSource, fkInto, fkStub, fkExtract, fkBackup{, fkDump});
  TFileNames = array[fkInto..High(TFileKind)] of TFileName;

  TLogStyle = (lsAuto, lsBrief, lsDetail);

  TRunOption = (roPause, roNoLogo, roVersion, ro3GB, roASLR, roDEP, // ordered
    {roAuto,} roStrip, roTrunc, roTouch, roUnsafe, roDeep, roDir, roRaw,
    {roMiniRes, roVerInfo, roMainIcon, roVerbose} roShowInfo, roOSVersion, roRebase);
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

procedure TOutput.TransferStats(SourceSize, ActualSize: LongWord; LineBreaks: Integer);
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
  FConsole.WriteLn(FStatsBuf, WideFormatBuf(FStatsFormat, [ActualSize, P], FStatsBuf), LineBreaks);
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
  OptionKeys: array[roPause..roShowInfo] of PCoreChar = (
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
            FRebaseAddress := Param.AsInteger;
          Include(FOptions, roRebase);
        end
        else if Key.Equals(sDropSect) then
        begin
          CmdLine := Param.AsNextParam(@CmdLine);
          if Param.Count = 0 then
            raise ECommandLine.Create(sSectionNames);
          if TypeOf(FDropSections) = nil then
            FDropSections.Create;
          LoadText(FDropSections.Append, @Param, CoreChar(','));
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

procedure ShowInfo;
type
  KnownSubsystem = IMAGE_SUBSYSTEM_NATIVE..IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION;
const
  SubsystemNames: array[KnownSubsystem] of PLegacyChar =
    (sNative, sGUI, sConsole, nil, sOS2, nil, sPOSIX, s9xDrv, sWindowsCE,
     sEFIApp, sEFIBootDrv, sEFIRuntimeDrv, sEFIROM, sXbox, sUnknown, sWindowsBootApp);
  TwoSpaces: array[0..1] of WideChar = (WideChar(' '), WideChar(' '));
var
  P: PCoreChar;
  I, L, R, V: Integer;
  TmpSectionName: array[0..IMAGE_SIZEOF_SHORT_NAME] of LegacyChar;
  MachineName, SubsystemName: PLegacyChar;
  W: WideChar;
begin
  if FLogStyle <> lsBrief then
    Console.WriteLn;
    
  case FImage.Headers.FileHeader.Machine of
    IMAGE_FILE_MACHINE_I386:      MachineName := sX86;
    IMAGE_FILE_MACHINE_AMD64:     MachineName := sX64;
    IMAGE_FILE_MACHINE_IA64:      MachineName := sItanium;
    IMAGE_FILE_MACHINE_EBC:       MachineName := sEFI;
    IMAGE_FILE_MACHINE_POWERPC:   MachineName := sPowerPC;
    IMAGE_FILE_MACHINE_POWERPCFP: MachineName := sPowerPCFPU;
    IMAGE_FILE_MACHINE_THUMB:     MachineName := sTumb;
    IMAGE_FILE_MACHINE_ARM:       MachineName := sARM;
    IMAGE_FILE_MACHINE_ARM64:     MachineName := sARM64;
    IMAGE_FILE_MACHINE_ALPHA64:   MachineName := sAlpha64;
    IMAGE_FILE_MACHINE_R3000:     MachineName := sR3000;
    IMAGE_FILE_MACHINE_R4000:     MachineName := sR4000;
    IMAGE_FILE_MACHINE_R10000:    MachineName := sR10000;
    IMAGE_FILE_MACHINE_WCEMIPSV2: MachineName := sMIPSWCE2;
    IMAGE_FILE_MACHINE_MIPSFPU:   MachineName := sMIPSFPU;
    IMAGE_FILE_MACHINE_MIPSFPU16: MachineName := sMIPS16FPU;
    IMAGE_FILE_MACHINE_MIPS16:    MachineName := sMIPS16;
    IMAGE_FILE_MACHINE_RISCV32:   MachineName := sRISCV32;
    IMAGE_FILE_MACHINE_RISCV64:   MachineName := sRISCV64;
    IMAGE_FILE_MACHINE_RISCV128:  MachineName := sRISCV128;
    IMAGE_FILE_MACHINE_SH3:       MachineName := sSH3;
    IMAGE_FILE_MACHINE_SH3DSP:    MachineName := sSH3DSP;
    IMAGE_FILE_MACHINE_SH4:       MachineName := sSH4;
    IMAGE_FILE_MACHINE_SH5:       MachineName := sSH5;
    IMAGE_FILE_MACHINE_AM33:      MachineName := sAM33;
    IMAGE_FILE_MACHINE_M32R:      MachineName := sM32R;
  else
    MachineName := sUnknown;
  end;

  with FImage.Headers.OptionalHeader do
  begin
    if Subsystem in [Low(KnownSubsystem)..High(KnownSubsystem)] then
      SubsystemName := SubsystemNames[Subsystem]
    else
      SubsystemName := sUnknown;
    Console.WriteLn(sMachineTypeFmt, 0, [PLegacyChar(sMachineType), PLegacyChar(sPE), MachineName, SubsystemName]);
    Console.WriteLn(sOSVersionFmt, 0, [PLegacyChar(sRequiredOSVersion),
      MajorOSVersion, MinorOSVersion, MajorSubsystemVersion, MinorSubsystemVersion]);
    HexString.AsHexadecimal(ImageBase, -8, False, CoreChar('0'));
    Console.WriteLn(sImageOptionsFmt, 0, [PLegacyChar(sImageBaseTitle), HexString.RawData]);
    Console.WriteLn(sHexDataFmt, 0, [PLegacyChar(sStackInfoTitle), StackCommitSize, StackReserveSize]);
    Console.WriteLn(sHexDataFmt, 0, [PLegacyChar(sHeapInfoTitle), HeapCommitSize, HeapReserveSize]);
  end;

  TmpFileName.Clear;
  W := ' ';
  if FImage.IsLargeAddressAware then
  begin
    P := s3GB;
    TmpFileName.Append(P + 1, PWord(P)^);
  end;
  if FImage.IsASLRAware then
  begin
    if TmpFileName.Count <> 0 then
      TmpFileName.Append(W);
    P := sASLR;
    TmpFileName.Append(P + 1, PWord(P)^);
  end;
  if FImage.IsDEPAware then
  begin
    if TmpFileName.Count <> 0 then
      TmpFileName.Append(W);
    P := sDEP;
    TmpFileName.Append(P + 1, PWord(P)^);
  end;
  if FImage.IsDotNETAware then
  begin
    if TmpFileName.Count <> 0 then
      TmpFileName.Append(W);
    P := sDotNET;
    TmpFileName.Append(P + 1, PWord(P)^);
  end;
  if TmpFileName.Count <> 0 then
    Console.WriteLn(sImageOptionsFmt, 0, [sImageOptions, TmpFileName.RawData]);

  with FImage.Headers.OptionalHeader do
    Console.WriteLn(sHexDataFmt, 0, [PLegacyChar(sSectionAlignment), FileAlignment, SectionAlignment]);

  if FLogStyle <> lsBrief then
  begin
    Console.WriteLn;
    Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF}
      PLegacyChar(sSectionList), StrLen(sSectionList));
  end;

  with FImage.SectionsMax do
  begin
    R := -Ceil32(Log2(RawDataOffset) / 3.99); // 3.99 bits per character
    V := -Ceil32(Log2(VirtualAddress) / 3.99);
  end;

  TmpFileName.AsHexadecimal(0, R, False, CoreChar('0'));
  HexString.AsHexadecimal(0, V, False, CoreChar('0'));
  Console.WriteLn(sSectionFmt, 0, [PLegacyChar(sStubSection), TmpFileName.RawData, HexString.RawData], 0);
  Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF} '  ', 2, 0);
  Output.TransferStats(Loaded.FileSize, FImage.Stub.Size);

  with FImage.Stub.Header.Ext do
  begin
    TmpFileName.AsHexadecimal(NewHeaderOffset, R, False, CoreChar('0'));
    HexString.AsHexadecimal(NewHeaderOffset, V, False, CoreChar('0'));
  end;
  Console.WriteLn(sSectionFmt, 0, [PLegacyChar(sHeadersSection), TmpFileName.RawData, HexString.RawData], 0);
  Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF} '  ', 2, 0);
  Output.TransferStats(Loaded.FileSize, FImage.HeadersSize + Cardinal(FImage.Count * SizeOf(TImageSectionHeader)));

  for I := 0 to FImage.Count - 1 do
    with FImage.Sections[I] do
    begin
      L := StrLen(Header.Name, IMAGE_SIZEOF_SHORT_NAME);
      Move(Header.Name[0], TmpSectionName, L);
      TmpSectionName[L] := #0;
      TmpFileName.AsHexadecimal(Header.RawDataOffset, R, False, CoreChar('0'));
      HexString.AsHexadecimal(Header.VirtualAddress, V, False, CoreChar('0'));
      Console.WriteLn(sSectionFmt, 0, [@TmpSectionName, TmpFileName.RawData, HexString.RawData], 0);
      Console.WriteLn({$IFDEF Locale} CP_LOCALIZATION, {$ENDIF} '  ', 2, 0);
      Output.TransferStats(Loaded.FileSize, Size, 0);
      TmpFileName.Clear;
      TmpFileName.Append(TwoSpaces, Length(TwoSpaces));
      if Header.Characteristics and IMAGE_SCN_TYPE_NO_PAD <> 0 then
        W := 'N'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then
        W := 'C'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA <> 0 then
        W := 'D'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA <> 0 then
        W := 'U'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_NO_DEFER_SPEC_EXC <> 0 then
        W := 'E'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_GPREL <> 0 then
        W := 'G'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_LNK_NRELOC_OVFL <> 0 then
        W := 'L'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_DISCARDABLE <> 0 then
        W := 'A'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_NOT_CACHED <> 0 then
        W := 'H'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_NOT_PAGED  <> 0 then
        W := 'P'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_SHARED <> 0 then
        W := 'S'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_EXECUTE <> 0 then
        W := 'X'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_READ <> 0 then
        W := 'R'
      else
        W := MidDot;
      TmpFileName.Append(W);
      if Header.Characteristics and IMAGE_SCN_MEM_WRITE <> 0 then
        W := 'W'
      else
        W := MidDot;
      TmpFileName.Append(W);
      Console.WriteLn(TmpFileName.RawData, TmpFileName.Count);
  end;
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
            if roShowInfo in FOptions then
            begin
              ShowInfo;
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

        if roShowInfo in FOptions then
        begin
          ShowInfo;
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
      if (FileName <> nil) and ((FLogStyle <> lsBrief) or (roShowInfo in FOptions)) then
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
