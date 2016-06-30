(*
    Executable images object model

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses;
                                       
{$I ImageHelper.inc}
{$I MenuetKolibri.inc}

type
  PExeStub = ^TExeStub;
  TExeStub = object(TCoreObject)
  private
    FHeader: TImageLegacyHeader;
    FData: Pointer;
    function GetDataSize: LongWord;
    procedure SetDataSize(Value: LongWord);
  public
    destructor Destroy; virtual;
    procedure Expand;
    function HeaderSize: LongWord;
    procedure Load(Source: PExeStub); overload;
    procedure Load(Source: PReadableStream); overload;
    procedure Save(Dest: PWritableStream);
    function Size: LongWord;
    procedure Strip(Heuristics: Boolean = True);

    property Data: Pointer read FData;
    property DataSize: LongWord read GetDataSize write SetDataSize;
    property Header: TImageLegacyHeader read FHeader;
  end;

  PExeSection = ^TExeSection;

{  TExeSectionHandler = class(TObjects)
  public
    procedure Build; virtual; abstract;
    function DataSize: LongWord; virtual; abstract;
    procedure Load(Source: PExeSection); virtual; abstract;
    procedure Save(Dest: PWritableStream); virtual; abstract;
  end;}

  TExeSection = object(TCoreObject)
  private
    FHeader: TImageSectionHeader;
    FData: Pointer;
//    FHandler: TExeSectionHandler;
//    procedure SetHandler(Value: TExeSectionHandler);
  public
    destructor Destroy; virtual;
    procedure Build;
    function DirectoryIndex(const OptionalHeader: TImageOptionalHeader): Integer;
    function IsOrphaned(const OptionalHeader: TImageOptionalHeader): Boolean;
    procedure Load(Source: PReadableStream);
    function RawData(VirtualAddress: LongWord): Pointer;
    procedure Save(Dest: PWritableStream);
    function Size: LongWord;
    procedure Strip(StripPadding: Boolean = True);

    property Data: Pointer read FData;
//    property Handler: TExeSectionHandler read FHandler write SetHandler;
    property Header: TImageSectionHeader read FHeader;
  end;

  TStripOptions = set of (soStub, soDebug, soRelocations, soExports, soSlashes, soVersionInfo,
    soSectionData, soPadding, soEmptySections, soOrphanedSections, soDataDirectory);

  PExeSectionArray = ^TExeSectionArray;
  TExeSectionArray = array[0..MaxInt div SizeOf(TExeSection) - 1] of TExeSection;

  TExeHeaderSection = (hsCode, hsData);
  TSectionRawData = (rdRaw, rdAlign, rdTruncLast);

  PExeImage = ^TExeImage;
  TExeImage = object(TCollection)
  private     
  { hold } FSections: PExeSectionArray;
    FHeaders: TImageNewHeaders; // TODO: x64
    FStub: TExeStub;
    function IndexOfAddress(Address: LongWord): Integer;
  protected
    class function CollectionInfo: TCollectionInfo; virtual;
    procedure Cut(Index: Integer; ItemCount: Integer = 1); virtual;
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Build(FileAlignment: LongWord = 512;
      SectionRawData: TSectionRawData = rdTruncLast);
    function Delete(Name: PLegacyChar; Length: Integer): Integer; overload;
    function FileAlignBytes(Source: LongWord): LongWord;
    function HeadersSize: LongWord;
    function IndexOf(HeaderSection: TExeHeaderSection): Integer; overload;
    function IndexOf(DirectoryIndex: Byte): Integer; overload;
    function IndexOf(Name: PLegacyChar; Length: Integer): Integer; overload;
    procedure LargeAddressAware(Value: Boolean = True);
    procedure Load(Source: PReadableStream);
    procedure OSVersion(MajorVersion: Word; MinorVersion: Word = 0);
    function RawData(VirtualAddress: LongWord): Pointer;
    function Rebase(Segment: Word; MenuetStyle: Boolean = False): LongWord;
    procedure Save(Dest: PWritableStream; TruncLastSection: Boolean = True);
    function SectionAlignBytes(Source: LongWord): LongWord;
    function Size(TruncLastSection: Boolean = True): LongWord;
    procedure Strip(Options: TStripOptions = [soStub..soEmptySections]);

    property Headers: TImageNewHeaders read FHeaders write FHeaders;
    property Sections: PExeSectionArray read FSections;
    property Stub: TExeStub read FStub;
  end;

  PMenuetImage = ^TMenuetImage;
  TMenuetImage = object(TCoreObject)
  private
    FHeader: TMenuetHeader;
    FData: Pointer;
  public
    destructor Destroy; virtual;
    procedure Build;
    function HeaderSize: Byte;
    procedure Load(Image: PExeImage);
    procedure Save(Dest: PWritableStream);

    property Data: Pointer read FData;
    property Header: TMenuetHeader read FHeader;
  end;

{ Exceptions }

  EBadImage = class(Exception);

  EUnknownImage = class(EBadImage)
  private
    FHeaders: TImageNewHeaders;
  public
    constructor Create(const Headers: TImageNewHeaders);
    property Headers: TImageNewHeaders read FHeaders;
  end;

{ Service functions }

function AlignToLongWord(Source: LongWord): LongWord;

implementation

uses
  CoreConsts;

const
  LegacyPageBytes       = 512; // DOS page
  LegacyParagraphBytes  = 16;  // DOS paragraph
  MinParagraphs         = SizeOf(TImageLegacyHeader) div LegacyParagraphBytes;

{ Service functions }

function AlignToLongWord(Source: LongWord): LongWord;
begin
  Result := Source + SizeOf(LongWord) - Source mod SizeOf(LongWord);
end;

{ EUnknownImage }

constructor EUnknownImage.Create(const Headers: TImageNewHeaders);
begin
  with Headers do
    inherited Create(sUnknownExeImage, [Magic[0], Magic[1]]);
  FHeaders := Headers;
end;

{ TExeStub }

destructor TExeStub.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

procedure TExeStub.Expand;
const
  ExtBytes = SizeOf(TImageLegacyHeaderExt);
var
  L: Integer;
begin
  if FHeader.HeaderParagraphs < MinParagraphs then
  begin
    L := Size;
    if L > SizeOf(FHeader) then
    begin
      L := DataSize;
      FHeader.HeaderParagraphs := MinParagraphs;
      SetDataSize(AlignToLongWord(L));
      Move(FData^, PLegacyChar(FData)[ExtBytes], L);
      Move(FHeader.Ext, FData^, ExtBytes);
      FillChar(FHeader.Ext, ExtBytes, 0); // goodbye, ð*ùRich<ð*ù
    end
    else
    begin
      SetDataSize(SizeOf(FHeader.Ext));
      FillChar(PLegacyChar(@FHeader)[L], SizeOf(FHeader) - L, 0); // also ð*ùRich<ð*ù
    end;
  end
  else
    SetDataSize(AlignToLongWord(DataSize));
  FHeader.Checksum := 0;
end;

function TExeStub.GetDataSize: LongWord;
begin
  Result := Size - HeaderSize;
end;

function TExeStub.HeaderSize: LongWord;
begin
  Result := FHeader.HeaderParagraphs * LegacyParagraphBytes;
  if Result > SizeOf(FHeader) then
    Result := SizeOf(FHeader);
end;

procedure TExeStub.Load(Source: PExeStub);
var
  L: LongWord;
begin
  FHeader := Source.FHeader;
  L := DataSize;
  if L >= SizeOf(FHeader.Ext) then
    Dec(L, SizeOf(FHeader.Ext));
  ReallocMem(FData, L);
  Move(Source.FData^, FData^, L);
end;

procedure TExeStub.Load(Source: PReadableStream);
const
  MinHeaderBytes = SizeOf(TImageLegacyHeader) - SizeOf(TImageLegacyHeaderExt);
var
  L: LongWord;
begin
  Source.ReadBuffer(FHeader, MinHeaderBytes);
  if FHeader.Magic <> 'MZ' then
    raise EBadImage.Create(sNotExecutableImage);
  L := Size;
  if L > 0 then
  begin
    Dec(L, MinHeaderBytes);
    if L > SizeOf(FHeader.Ext) then
    begin
      Source.ReadBuffer(FHeader.Ext, SizeOf(FHeader.Ext));
      Dec(L, SizeOf(FHeader.Ext));
      ReallocMem(FData, L);
      Source.ReadBuffer(FData^, L);
      Exit;
    end
    else
      Source.ReadBuffer(FHeader.Ext, L);
  end;
  FreeMemAndNil(FData);
end;

procedure TExeStub.Save(Dest: PWritableStream);
begin
  if FData <> nil then
  begin
    Dest.WriteBuffer(FHeader, SizeOf(FHeader));
    Dest.WriteBuffer(FData^, Size - SizeOf(FHeader));
  end
  else
    Dest.WriteBuffer(FHeader, Size);
end;

procedure TExeStub.SetDataSize(Value: LongWord);
begin
  if FHeader.HeaderParagraphs >= MinParagraphs then
    ReallocMem(FData, Value)
  else
    FreeMemAndNil(FData);
  Inc(Value, HeaderSize);
  with FHeader do
  begin
    FilePages := Value div LegacyPageBytes + 1;
    LastPageBytes := Value mod LegacyPageBytes;
  end;
end;

function TExeStub.Size: LongWord;
begin
  with FHeader do
    if LastPageBytes <> 0 then
      Result := (FilePages - 1) * LegacyPageBytes + LastPageBytes
    else
      Result := FilePages * LegacyPageBytes;
end;

procedure TExeStub.Strip(Heuristics: Boolean);
var
  NewSize, L: LongWord;
  P, Limit: PLegacyChar;
begin
  if FHeader.HeaderParagraphs >= MinParagraphs then
  begin
    NewSize := FHeader.Ext.NewHeaderOffset;
    if NewSize <> 0 then
    begin
      Dec(NewSize, HeaderSize);
      L := DataSize;
      if L > NewSize then
      begin
        if Heuristics then
        begin
          Limit := PLegacyChar(FData) + L;
          P := PLegacyChar(FData) + FHeader.HeaderParagraphs * LegacyParagraphBytes - SizeOf(FHeader);
          P := StrScan(P, Limit - P, #$4C);               // MOV AX, 4C01h
          if (P <> nil) and (PWord(P + 1)^ = $21CD) then  // INT 21h
          begin
            Inc(P, 3);
            P := StrScan(P, Limit - P, '$');
            if P <> nil then
              NewSize := P - PLegacyChar(FData) + 1;
          end;
        end;
        SetDataSize(NewSize);
        FHeader.Checksum := 0;
      end;
    end;
  end
  else if Heuristics and (Size = SizeOf(FHeader)) and (FHeader.Ext.NewHeaderOffset = SizeOf(FHeader)) then
  begin
    SetDataSize(SizeOf(FHeader.Ext) - SizeOf(FHeader.Ext.NewHeaderOffset));
    FHeader.Checksum := 0;
  end;
end;

{ TExeSection }

destructor TExeSection.Destroy;
begin
//  FHandler.Free;
  FreeMem(FData);
  inherited;
end;

procedure TExeSection.Build;
begin
//  if FHandler <> nil then
//    FHeader.RawDataSize := FHandler.DataSize;
end;

function TExeSection.DirectoryIndex(const OptionalHeader: TImageOptionalHeader): Integer;
begin
  if FHeader.VirtualAddress <> 0 then
    for Result := 0 to OptionalHeader.DirectoryEntryCount - 1 do
      if FHeader.VirtualAddress = OptionalHeader.DataDirectory[Result].VirtualAddress then
        Exit;
  Result := -1;
end;

function TExeSection.IsOrphaned(const OptionalHeader: TImageOptionalHeader): Boolean;
begin
  with FHeader, OptionalHeader do
    Result := (VirtualAddress <> 0) and (VirtualAddress <> CodeBase) and
      (VirtualAddress <> DataBase) and (DirectoryIndex(OptionalHeader) < 0);
end;

procedure TExeSection.Load(Source: PReadableStream);
var
  Pos: QuadWord;
  BytesRead: LongWord;
begin
  Source.ReadBuffer(FHeader, SizeOf(FHeader));
  ReallocMem(FData, FHeader.RawDataSize);
  with Source^ do
  begin
    Pos := Position;
    try
      Position := FHeader.RawDataOffset;
      BytesRead := Read(FData^, FHeader.RawDataSize);
      if BytesRead < FHeader.RawDataSize then // Watcom linker workaround
        FillChar(PLegacyChar(FData)[BytesRead], FHeader.RawDataSize - BytesRead, 0);
    finally
      Position := Pos;
    end;
  end;
///  if FHandler <> nil then
//    FHandler.Load(Self);
end;

function TExeSection.RawData(VirtualAddress: LongWord): Pointer;
begin
  if (VirtualAddress >= FHeader.VirtualAddress) and
    (VirtualAddress < FHeader.VirtualAddress + FHeader.VirtualSize)
  then
    Result := PAddress(FData) + VirtualAddress - FHeader.VirtualAddress
  else
    Result := nil;
end;

procedure TExeSection.Save(Dest: PWritableStream);
begin
///  if FHandler <> nil then
//    FHandler.Save(Dest)
//  else
    Dest.WriteBuffer(FData^, FHeader.RawDataSize);
end;

(*procedure TExeSection.SetHandler(Value: TExeSectionHandler);
begin
{$IFNDEF Lite}
  if FHandler = Value then
    Exit;
{$ENDIF}
  FHandler.Free;
  FHandler := Value;
  if FHandler <> nil then
    FHandler.Load(Self);
end;*)

function TExeSection.Size: LongWord;
begin
//  if FHandler <> nil then
//    Result := FHandler.DataSize
//  else
    Result := FHeader.RawDataSize;
end;

procedure TExeSection.Strip(StripPadding: Boolean);
type
  PPadding = ^TPadding;
  TPadding = array[0..3] of LongWord; // 'PADDINGXXPADDING'
const
  PADDINGXXPADDING: array[$0..$F] of LegacyChar = 'PADDINGXXPADDING';
var
  Padding: PPadding;
  P, Limit: PLegacyChar;
begin
  if {(FHandler = nil) and} (FData <> nil) then
  begin
    while (FHeader.RawDataSize <> 0) and (PLegacyChar(FData)[FHeader.RawDataSize - 1] = #0) do
      Dec(FHeader.RawDataSize);

    if StripPadding then
    begin
      P := PLegacyChar(FData) + FHeader.RawDataSize;
      Limit := P - SizeOf(PADDINGXXPADDING);
      Padding := Pointer(Limit);
      while (P > PLegacyChar(FData)) and (P > Limit) do
      begin
        Dec(P);
        if CompareMem(P, @PADDINGXXPADDING, Limit + SizeOf(PADDINGXXPADDING) - P) then
          Padding := Pointer(P - SizeOf(PADDINGXXPADDING));
      end;

      P := nil;
      while PLegacyChar(Padding) > PLegacyChar(FData) do
        if (Padding[0] = $44444150) and (Padding[1] = $58474E49) and // 'PADDINGXXPADDING'
          (Padding[2] = $44415058) and (Padding[3] = $474E4944) then
        begin
          Dec(Padding);
          P := PLegacyChar(Padding);
        end
        else
        begin
          if P <> nil then
          begin
            Limit := P;
            Inc(P, SizeOf(PADDINGXXPADDING));
            Padding := nil;
            while (P > PLegacyChar(FData)) and (P > Limit) do
            begin
              Dec(P);
              if CompareMem(P, @PADDINGXXPADDING, Limit + SizeOf(PADDINGXXPADDING) - P) then
                Padding := Pointer(P);
            end;
            if Padding <> nil then
              FHeader.RawDataSize := PLegacyChar(Padding) - PLegacyChar(FData);
          end;
          Break;
        end;
    end;
  end;
end;

{ TExeImage }

constructor TExeImage.Create;
begin
  inherited Create(imFinalize);
end;

destructor TExeImage.Destroy;
begin
  FStub.Finalize;
  inherited;
end;

procedure TExeImage.Build(FileAlignment: LongWord; SectionRawData: TSectionRawData);
const
  FixedOptHeaderSize = SizeOf(TImageOptionalHeader) -
    IMAGE_NUMBEROF_DIRECTORY_ENTRIES * SizeOf(TImageDataDirectory);
var
  Offset: LongWord;
  I: Integer;
begin
  if FileAlignment <> 0 then
    FHeaders.OptionalHeader.FileAlignment := FileAlignment;

  if FHeaders.OptionalHeader.MajorOSVersion >= 5 then
    with FHeaders.FileHeader do
      Characteristics := Characteristics and not IMAGE_FILE_BYTES_REVERSED_HI;

  Offset := 0;

  with FStub do
  begin
    Expand;
    Inc(Offset, Size);
    if LongWord(FHeaders.Magic) = IMAGE_NT_SIGNATURE then
      FHeader.Ext.NewHeaderOffset := Offset
    else if LongWord(FHeaders.Magic) <> 0 then
      raise EUnknownImage.Create(FHeaders);
  end;

  Inc(Offset, HeadersSize + SizeOf(TImageSectionHeader) * LongWord(Count));
  Inc(Offset, FileAlignBytes(Offset));
  with FHeaders.OptionalHeader do
  begin
    HeadersSize := Offset;
    for I := 0 to Count - 1 do
      with FSections[I] do
      begin
        Build;
        if CodeBase = FHeader.RawDataOffset then
          CodeBase := Offset
        else if DataBase = FHeader.RawDataOffset then
          DataBase := Offset;
        FHeader.RawDataOffset := Offset;
        if SectionRawData = rdRaw then
        begin
          Inc(Offset, FHeader.RawDataSize);
          Inc(Offset, FileAlignBytes(Offset));
        end
        else if (SectionRawData = rdAlign) or (I < Count - 1) then
        begin
          Inc(FHeader.RawDataSize, FileAlignBytes(FHeader.RawDataSize));
          Inc(Offset, FHeader.RawDataSize);
        end;
      end;
    Checksum := 0;
  end;
  FHeaders.FileHeader.SectionCount := Count;
{
  // not valid Win32 image
  FHeaders.FileHeader.OptionalHeaderSize := FixedOptHeaderSize + Count * SizeOf(TImageDataDirectory);
  Dec(FHeaders.OptionalHeader.HeadersSize, SizeOf(TImageOptionalHeader) - OptionalHeaderSize);
}
end;

class function TExeImage.CollectionInfo: TCollectionInfo;
begin
  with Result do
  begin
    ClassName := sExeImage;
    ItemSize := SizeOf(TExeSection); 
  end;
end;

procedure TExeImage.Cut(Index, ItemCount: Integer);
type
  TDirectoryEntry = (deExports, deImports, deResources, deExceptions, deCertificates,
    deRelocations, deDebug, deCopyrights, deGlobalPtr, deTLS, deLoadConfig, deBoundImports,
    deIAT, deDelayImport, deCOMDescriptor);
  TDirectoryEntries = set of TDirectoryEntry;
const
  deSecurity = deCertificates;
var
  Increment: LongWord;
  I, Idx: Integer;
  Entries: TDirectoryEntries;
begin
  if Index <> 0 then
  begin
    Increment := 0;
    for I := Index to Index + ItemCount - 1 do
      with FSections[I].FHeader do
        if VirtualSize <> 0 then
          Inc(Increment, VirtualSize + SectionAlignBytes(VirtualSize));
    with FSections[Index - 1].FHeader do
      Inc(VirtualSize, SectionAlignBytes(VirtualSize) + Increment);
  end;

  Entries := [];
  for I := Index to Index + ItemCount - 1 do
  begin
    Idx := FSections[I].DirectoryIndex(FHeaders.OptionalHeader);
    if Idx >= 0 then
      Include(Entries, TDirectoryEntry(Idx));
  end;

  inherited;
  Dec(FHeaders.FileHeader.SectionCount, ItemCount);

  if Entries <> [] then
  begin
    if deDebug in Entries then
      with FHeaders.FileHeader do
        Characteristics := Characteristics or IMAGE_FILE_DEBUG_STRIPPED;

    if deRelocations in Entries then
    begin
      for I := 0 to Count - 1 do
        with FSections[I].FHeader do
        begin
          RelocationsOffset := 0;
          RelocationCount := 0;
        end;
      QuadWord(FHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC]) := 0; // Fast core
      with FHeaders.FileHeader do
        Characteristics := Characteristics or IMAGE_FILE_RELOCS_STRIPPED;
    end;
  end;
end;

function TExeImage.Delete(Name: PLegacyChar; Length: Integer): Integer;
var
  Index: Integer;
begin
  Index := IndexOf(Name, Length);
  if Index >= 0 then
  begin
    with FSections[Index] do
      Result := Size + SizeOf(FHeader);
    Cut(Index);
  end
  else
    Result := 0;
end;

function TExeImage.FileAlignBytes(Source: LongWord): LongWord;
begin
  Result := FHeaders.OptionalHeader.FileAlignment;
  if Result <> 0 then
  begin
    Result := Source mod Result;
    if Result <> 0 then
      Result := FHeaders.OptionalHeader.FileAlignment - Result;
  end;
end;

function TExeImage.HeadersSize: LongWord;
begin
  if LongWord(FHeaders.Magic) = IMAGE_NT_SIGNATURE then
    Result := SizeOf(TImageNewHeaders) - SizeOf(TImageOptionalHeader) +
      FHeaders.FileHeader.OptionalHeaderSize
  else
    Result := 0;
end;

function TExeImage.IndexOfAddress(Address: LongWord): Integer;
begin
  if Address <> 0 then
    for Result := 0 to Count - 1 do
      if FSections[Result].Header.VirtualAddress = Address then
        Exit;
  Result := -1;
end;

function TExeImage.IndexOf(HeaderSection: TExeHeaderSection): Integer;
begin
  with FHeaders.OptionalHeader do
    if HeaderSection = hsCode then
      Result := IndexOfAddress(CodeBase)
    else
      Result := IndexOfAddress(DataBase);
end;

function TExeImage.IndexOf(DirectoryIndex: Byte): Integer;
begin
  Result := IndexOfAddress(FHeaders.OptionalHeader.DataDirectory[DirectoryIndex].VirtualAddress);
end;

function TExeImage.IndexOf(Name: PLegacyChar; Length: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    with FSections[Result] do
      if StrComp(Name, Length, FHeader.Name, StrLen(FHeader.Name, IMAGE_SIZEOF_SHORT_NAME)) = 0 then
        Exit;
  Result := -1;
end;

procedure TExeImage.LargeAddressAware(Value: Boolean);
begin
  with FHeaders.FileHeader do
    Characteristics := Characteristics or (IMAGE_FILE_LARGE_ADDRESS_AWARE and (LongWord(not Value) - 1));
end;

procedure TExeImage.Load(Source: PReadableStream);
var
  I: Integer;
begin
  Clear;
 
  FStub.Load(Source);

  if (FStub.Size >= SizeOf(TImageLegacyHeader)) and
    (FStub.Header.Ext.NewHeaderOffset <> 0) then
  begin
    with Source^ do
    begin
      Position := FStub.Header.Ext.NewHeaderOffset;
      ReadBuffer(FHeaders, SizeOf(FHeaders) - SizeOf(FHeaders.OptionalHeader));
    end;
    if LongWord(FHeaders.Magic) <> IMAGE_NT_SIGNATURE then
      raise EUnknownImage.Create(FHeaders);
    if FHeaders.FileHeader.OptionalHeaderSize <> SizeOf(FHeaders.OptionalHeader) then
      raise EBadImage.Create(sNotValidWin32Image);
    Source.ReadBuffer(FHeaders.OptionalHeader, FHeaders.FileHeader.OptionalHeaderSize);
    with FHeaders.OptionalHeader do
    begin
      if (DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR) and
        (QuadWord(DataDirectory[IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR]) <> 0) // Fast core
      then
        raise EBadImage.Create(sDotNETAssembly);
      FillChar(DataDirectory[DirectoryEntryCount], SizeOf(DataDirectory) -
        DirectoryEntryCount * SizeOf(TImageDataDirectory), 0);
    end;
    Capacity := FHeaders.FileHeader.SectionCount;
    for I := 0 to Capacity - 1 do
      with FSections[Append] do
      begin
        Create;
        Load(Source);
      end;
    if Capacity <> 0 then
      with FSections[Count - 1].Header do
        Source.Position := RawDataOffset + RawDataSize; // for LoadFile result
  end;
end;

procedure TExeImage.OSVersion(MajorVersion, MinorVersion: Word);
begin
  with FHeaders.OptionalHeader do
  begin
    MajorOSVersion := MajorVersion;
    MinorOSVersion := MinorVersion;
    MajorSubsystemVersion := MajorVersion;
    MinorSubsystemVersion := MinorVersion;
  end;
end;

function TExeImage.RawData(VirtualAddress: LongWord): Pointer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := FSections[I].RawData(VirtualAddress);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TExeImage.Rebase(Segment: Word; MenuetStyle: Boolean): LongWord;
var
  DeltaOffset: Integer;
  MenuetImageBase: Byte;
  Relocs: PImageBaseRelocation;
  Idx, I: Integer;
  Chunk: PAddress;
  Offset: Word;
begin
  Result := 0;

  if MenuetStyle then
  begin
    MenuetImageBase := SizeOf(TMenuetHeader);
    Idx := IndexOf(IMAGE_DIRECTORY_ENTRY_TLS);
    if Idx < 0 then
      Dec(MenuetImageBase, SizeOf(LongWord));
  end
  else
    MenuetImageBase := 0;

  Idx := IndexOf(IMAGE_DIRECTORY_ENTRY_BASERELOC);
  if Idx >= 0 then
  begin
    Relocs := PImageBaseRelocation(FSections[Idx].Data);
    if Relocs <> nil then
    begin
      while Relocs.VirtualAddress <> 0 do
      begin
        Chunk := RawData(Relocs.VirtualAddress);
        if Chunk = nil then
          raise EBadImage.Create('Invalid address'); // TODO: invalid address

        DeltaOffset := Segment shl 16 + MenuetImageBase;
        if MenuetStyle then
          for I := 0 to Count - 1 do
            with FHeaders.OptionalHeader, FSections[I].FHeader do
            begin
              if (VirtualAddress > EntryPoint) and (FSections[I - 1].FHeader.VirtualAddress <= EntryPoint) then
                Dec(EntryPoint, DeltaOffset);
              Inc(DeltaOffset, RawDataSize - VirtualAddress);
              if VirtualAddress = CodeBase then
                Dec(CodeBase, DeltaOffset);
              if VirtualAddress = DataBase then
                Dec(DataBase, DeltaOffset);
            end;
        Dec(DeltaOffset, FHeaders.OptionalHeader.ImageBase);

        for I := 0 to (Relocs.BlockSize - SizeOf(TImageBaseRelocation)) div SizeOf(Word) - 1 do
        begin
          Offset := PWordArray(PAddress(Relocs) + SizeOf(TImageBaseRelocation))[I];
          case Offset shr 12 of
            0:
              Break;
            IMAGE_REL_BASED_HIGHLOW:
              begin
                Inc(PLongWord(Chunk + Offset and $FFF)^, DeltaOffset);
                Inc(Result);
              end;
          else // TODO
            raise EBadImage.Create('Unsupported relocation format, code %2u', DefaultSystemCodePage, [Offset shr 12]);
          end;
        end;
        Inc(PAddress(Relocs), Relocs.BlockSize);
      end;
    end;
  end;

  FHeaders.OptionalHeader.ImageBase := Segment shl 16 + MenuetImageBase;
end;

procedure TExeImage.Save(Dest: PWritableStream; TruncLastSection: Boolean);
var
  Dummy: array[0..4095] of Byte;
  Offset, H: LongWord;
  I: Integer;
begin
  FillChar(Dummy, SizeOf(Dummy), 0);

  Offset := 0;
  with FStub do
  begin
    Save(Dest);
    Inc(Offset, Size);
  end;

  H := HeadersSize;
  Dest.WriteBuffer(FHeaders, H);
  Inc(Offset, H);

  for I := 0 to Count - 1 do
    Dest.WriteBuffer(FSections[I].FHeader, SizeOf(TImageSectionHeader));
  Dest.WriteBuffer(Dummy, FileAlignBytes(Offset + SizeOf(TImageSectionHeader) * LongWord(Count)));

  for I := 0 to Count - 1 do
  begin
    FSections[I].Save(Dest);
    if (I < Count - 1) or not TruncLastSection then
      Dest.WriteBuffer(Dummy, FileAlignBytes(Sections[I].FHeader.RawDataSize));
  end;
end;

function TExeImage.SectionAlignBytes(Source: LongWord): LongWord;
begin
  Result := FHeaders.OptionalHeader.SectionAlignment;
  if Result <> 0 then
  begin
    Result := Source mod Result;
    if Result <> 0 then
      Result := FHeaders.OptionalHeader.SectionAlignment - Result;
  end;
end;

function TExeImage.Size(TruncLastSection: Boolean): LongWord;
var
  I: Integer;
begin
  Result := FStub.Size;
  Inc(Result, HeadersSize);
  Inc(Result, SizeOf(TImageSectionHeader) * Count);
  Inc(Result, FileAlignBytes(Result));
  for I := 0 to Count - 1 do
  begin
    Inc(Result, FSections[I].Size);;
    Inc(Result, FileAlignBytes(Result));
  end;
  if TruncLastSection and (Count <> 0) then
    Dec(Result, FileAlignBytes(FSections[Count - 1].Size));
end;

procedure TExeImage.Strip(Options: TStripOptions);
var
  Cnt, I: Integer;
begin
  if soStub in Options then
    FStub.Strip;

  if soDebug in Options then
    DeleteExisting(IndexOf(IMAGE_DIRECTORY_ENTRY_DEBUG));

  if (FHeaders.FileHeader.Characteristics and IMAGE_FILE_DLL = 0) and
    (FHeaders.OptionalHeader.Subsystem in [IMAGE_SUBSYSTEM_WINDOWS_GUI, IMAGE_SUBSYSTEM_WINDOWS_CUI]) then
  begin
    if (soRelocations in Options) and (FHeaders.OptionalHeader.DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_BASERELOC) then
      DeleteExisting(IndexOf(IMAGE_DIRECTORY_ENTRY_BASERELOC));
    if soExports in Options then
      DeleteExisting(IndexOf(IMAGE_DIRECTORY_ENTRY_EXPORT));
  end;

  if Options * [soSectionData..soOrphanedSections] <> [] then
  begin
    for I := Count - 1 downto 0 do
      with Sections[I] do
      begin
        if soSectionData in Options then
          Strip(soPadding in Options);
        if ((soEmptySections in Options) and (Header.RawDataSize = 0)) or
          ((soOrphanedSections in Options) and IsOrphaned(FHeaders.OptionalHeader)) or
          ((soSlashes in Options) and (Header.Name[0] = '/'))
        then
          Cut(I);
      end;
  end;

  with FHeaders.OptionalHeader do
  begin
    Cnt := 0;
    for I := DirectoryEntryCount - 1 downto 0 do
      if QuadWord(DataDirectory[I]) <> 0 then // Fast core
      begin
        Cnt := I + 1;
        Break;
      end;
    if soDataDirectory in Options then
      DirectoryEntryCount := Cnt
    else if DirectoryEntryCount < IMAGE_NUMBEROF_DIRECTORY_ENTRIES then
    begin
      FillChar(DataDirectory[Cnt], (IMAGE_NUMBEROF_DIRECTORY_ENTRIES - Cnt) * SizeOf(TImageDataDirectory), 0);
      DirectoryEntryCount := IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
    end;
  end;
end;

{ TMenuetImage }

destructor TMenuetImage.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

procedure TMenuetImage.Build;
asm
        MOV dword [EAX+FHeader.Magic], $554E454D  // 'MENU'
        CMP dword [EAX.FHeader.TLS], 0
        SETNZ DL
        SHL EDX, 24
        ADD EDX, $31305445                        // 'ET01'
        MOV dword [EAX+FHeader.Magic+4], EDX
        SHR EDX, 29 // hack
        MOV dword [EAX+FHeader.HeaderVersion], EDX
end;
{begin
  with QuadRec(FHeader.Magic) do
  begin
    Lo := $554E454D;                                  // 'MENU'
    Hi := $31305445 + Byte(FHeader.TLS <> 0) shl 24;  // 'ET01'
  end;
end;}

function TMenuetImage.HeaderSize: Byte;
begin
  Result := SizeOf(FHeader) - Byte(FHeader.TLS = 0) * SizeOf(FHeader.TLS);
end;

procedure TMenuetImage.Load(Image: PExeImage);
var
  I, Delta: Integer;
  Dest: PAddress;
begin
  FHeader.ImageSize := Image.FHeaders.OptionalHeader.ImageBase;
  for I := 0 to Image.Count - 1 do
    with FHeader, Image.Sections[I].FHeader do
    begin
      if VirtualAddress = Image.FHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_TLS].VirtualAddress then
        TLS := ImageSize;
      Inc(ImageSize, VirtualSize); // TODO: handlers
    end;

  ReallocMem(FData, FHeader.ImageSize);
  Dest := FData;
  with Image.FHeaders.OptionalHeader do
  begin
    FillChar(Dest^, ImageBase, 0);
    Inc(Dest, ImageBase);
  end;
  for I := 0 to Image.Count - 1 do
    with Image.Sections[I], FHeader do
    begin
      Move(Data^, Dest^, RawDataSize); // TODO: handlers
      Inc(Dest, RawDataSize);
      Delta := RawDataSize - VirtualSize;
      FillChar(Dest^, Delta, 0);
      Inc(Dest, Delta);
    end;

  with FHeader, Image.Headers do
  begin
    Inc(ImageSize, HeaderSize);
    EntryPoint := OptionalHeader.EntryPoint;
    HeapSize := OptionalHeader.HeapReserveSize;
    ESP := ImageSize + OptionalHeader.StackReserveSize;
  end;
end;

procedure TMenuetImage.Save(Dest: PWritableStream);
var
  L: LongWord;
begin
  with Dest^ do
  begin
    L := HeaderSize;
    WriteBuffer(FHeader, L);
    WriteBuffer(FData^, FHeader.ImageSize - L);
  end;
end;

end.

