(*
    Executable images object model

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
  TImageLegacyHeaderExt = packed record
    Reserved: array[0..3] of Word;
    OEMId, OEMInfo: Word;
    Reserved2: array[0..9] of Word;
    NewHeaderOffset: LongWord;
  end;

  PImageLegacyHeader = ^TImageLegacyHeader;
  TImageLegacyHeader = packed record
    Magic: array[0..1] of Char;
    LastPageBytes, FilePages, RelocationCount,
    HeaderParagraphs, MinAlloc, MaxAlloc,
    InitialSS, InitialSP,
    Checksum,
    InitialIP, InitialCS,
    RelocationsOffset, OverlayNumber: Word;
    Ext: TImageLegacyHeaderExt;
  end;

  PImageFileHeader = ^TImageFileHeader;
  TImageFileHeader = packed record
    Machine, SectionCount: Word;
    TimeDateStamp,
    SymbolsOffset, SymbolCount: LongWord;
    OptionalHeaderSize, Characteristics: Word;
  end;

  PImageOptionalHeader = ^TImageOptionalHeader;
  TImageOptionalHeader = packed record
    Magic: Word;
    MajorLinkerVersion, MinorLinkerVersion: Byte;
    CodeSize, InitializedDataSize, UninitializedDataSize,
    EntryPoint, CodeBase, DataBase, ImageBase,
    SectionAlignment, FileAlignment: LongWord;
    MajorOSVersion, MinorOSVersion,
    MajorImageVersion, MinorImageVersion,
    MajorSubsystemVersion, MinorSubsystemVersion: Word;
    Win32Version,
    ImageSize, HeadersSize,
    Checksum: LongWord;
    Subsystem, DLLCharacteristics: Word;
    StackReserveSize, StackCommitSize,
    HeapReserveSize, HeapCommitSize,
    LoaderFlags: LongWord;
    DirectoryEntryCount: LongInt;
    DataDirectory: array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;

  PImageNewHeaders = ^TImageNewHeaders;
  TImageNewHeaders = packed record
    Magic: array[0..3] of Char;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader;
  end;

  TImageSectionMisc = packed record
    case Byte of
      0: (PhysicalAddress: LongWord);
      1: (VirtualSize: LongWord);
  end;

  PImageSectionHeader = ^TImageSectionHeader;
  TImageSectionHeader = packed record
    Name: array[0..IMAGE_SIZEOF_SHORT_NAME - 1] of Char;
    Misc: TImageSectionMisc;
    VirtualAddress, RawDataSize,
    RawDataOffset, RelocationsOffset, LineNumbersOffset: LongWord;
    RelocationCount, LineNumberCount: Word;
    Characteristics: LongWord;
  end;

const // for Delphi 6
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT    = 13;
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR  = 14;

  IMAGE_FILE_LARGE_ADDRESS_AWARE        = $0020; 

type
  TExeStub = class
  private
    FHeader: TImageLegacyHeader;
    FData: Pointer;
    function GetDataSize: LongWord;
    procedure SetDataSize(Value: LongWord);
  public
    destructor Destroy; override;
    procedure Expand;
    function HeaderSize: LongWord;
    procedure Load(Source: TExeStub); overload;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    function Size: LongWord;
    procedure Strip(Heuristics: Boolean = True);

    property Data: Pointer read FData;
    property DataSize: LongWord read GetDataSize write SetDataSize;
    property Header: TImageLegacyHeader read FHeader;
  end;

  TExeSection = class
  private
    FHeader: TImageSectionHeader;
    FData: Pointer;
  public
    destructor Destroy; override;
    function DirectoryIndex(const OptionalHeader: TImageOptionalHeader): Integer;
    function IsOrphaned(const OptionalHeader: TImageOptionalHeader): Boolean;
    procedure Load(Source: TReadableStream);
    procedure Save(Dest: TWritableStream);

    property Data: Pointer read FData;
    property Header: TImageSectionHeader read FHeader;
  end;

  TStripOptions = set of (soDirectory, soStub, soEmptySections, soOrphanedSections,
     soRelocations{, soRedundantResources});

  PExeSectionArray = ^TExeSectionArray;
  TExeSectionArray = array[0..MaxInt div SizeOf(TExeSection) - 1] of TExeSection;

  TExeImage = class(TObjects)
  private
  { hold } FSections: PExeSectionArray;
  { hold } FOwnsSections: Boolean;
    FHeaders: TImageNewHeaders;
    FStub: TExeStub;
  public
    destructor Destroy; override;
    procedure Build;
    function Extract(Index: Integer): TObject; {$IFNDEF Lite} override; {$ENDIF}
    function FileAlignBytes(Source: LongWord): LongWord;
    function HeadersSize: LongWord;
    function IndexOfSection(DirectoryIndex: Byte): Integer;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    function SectionAlignBytes(Source: LongWord): LongWord;
    function Size: LongWord;
    procedure Strip(Options: TStripOptions = [soStub..soRelocations]);

    property Headers: TImageNewHeaders read FHeaders;
    property OwnsSections: Boolean read FOwnsSections;
    property Sections: PExeSectionArray read FSections;
    property Stub: TExeStub read FStub;
  end;

{ Exceptions }

  EUnknownImage = class(Exception)
  private
    FHeaders: PImageNewHeaders;
  public
    constructor Create(const Headers: TImageNewHeaders);
    property Headers: PImageNewHeaders read FHeaders;
  end;

implementation

uses
  CoreConsts;

const
  LegacyPageBytes       = 512; // DOS page
  LegacyParagraphBytes  = 16;  // DOS paragraph
  MinParagraphs         = SizeOf(TImageLegacyHeader) div LegacyParagraphBytes;

{ EUnknownImage }

constructor EUnknownImage.Create(const Headers: TImageNewHeaders);
begin
  with Headers do
    inherited Create(sUnknownExeImage, [Magic[0], Magic[1]]);
  FHeaders := @Headers;
end;

{ TExeStub }

destructor TExeStub.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

procedure TExeStub.Expand;

function Aligned(Source: LongWord): LongWord;
begin
  Result := Source + SizeOf(LongWord) - Source mod SizeOf(LongWord);
end;

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
      SetDataSize(Aligned(DataSize + ExtBytes));
      Move(FData^, PLegacyChar(FData)[ExtBytes], ExtBytes);
      Move(FHeader.Ext, FData^, ExtBytes);
      FillChar(FHeader.Ext, ExtBytes, 0); // goodbye, �*�Rich<�*�
      FHeader.HeaderParagraphs := MinParagraphs;
    end
    else
    begin
      SetDataSize(SizeOf(LongWord)); // aligning to 32-bit boundary
      PLongWord(FData)^ := $E2EB;    // JMP -28
      FillChar(PLegacyChar(@FHeader)[L], SizeOf(FHeader) - L, 0); // also �*�Rich<�*�
    end;
  end
  else
    SetDataSize(Aligned(DataSize));
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

procedure TExeStub.Load(Source: TExeStub);
var
  L: LongWord;
begin
  FHeader := Source.FHeader;
  L := Source.DataSize;
  ReallocMem(FData, L);
  Move(Source.FData^, FData^, L);
end;

procedure TExeStub.Load(Source: TReadableStream);
const
  MinHeaderBytes = SizeOf(TImageLegacyHeader) - SizeOf(TImageLegacyHeaderExt);
var
  L: LongWord;
begin
  Source.ReadBuffer(FHeader, MinHeaderBytes);
  L := Size - MinHeaderBytes;
  if L > SizeOf(FHeader.Ext) then
  begin
    Source.ReadBuffer(FHeader.Ext, SizeOf(FHeader.Ext));
    Dec(L, SizeOf(FHeader.Ext));
    ReallocMem(FData, L);
    Source.ReadBuffer(FData^, L);
  end
  else
  begin
    Source.ReadBuffer(FHeader.Ext, L);
    FreeMemAndNil(FData);
  end;
end;

procedure TExeStub.Load(FileName: PCoreChar);
var
  Source: TReadableStream;
begin
  Source := TFileStream.Create(FileName, faRead + [faSequential]);
  try
    Load(Source);
  finally
    Source.Free;
  end;
end;

procedure TExeStub.Save(Dest: TWritableStream);
var
  L, SaveSize: LongWord;
begin
  L := Size;
  if L > SizeOf(FHeader) then
  begin
    SaveSize := FHeader.FilePages;
    try
      with Dest do
      begin
        WriteBuffer(FHeader, SizeOf(FHeader));
        WriteBuffer(FData^, L - SizeOf(FHeader));
      end
    finally
      FHeader.FilePages := SaveSize;
    end;
  end
  else
    Dest.WriteBuffer(FHeader, L);
end;

procedure TExeStub.Save(FileName: PCoreChar);
var
  Dest: TWritableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faSequential]);
  try
    Dest.Size := Size;
    Save(Dest);
  finally
    Dest.Free;
  end;
end;

procedure TExeStub.SetDataSize(Value: LongWord);
begin
  ReallocMem(FData, Value);
  Inc(Value, SizeOf(FHeader));
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
        P := StrScan(P, #$4C, Limit - P);               // MOV AX, 4C01h
        if (P <> nil) and (PWord(P + 1)^ = $21CD) then  // INT 21h
        begin
          Inc(P, 3);
          P := StrScan(P, '$', Limit - P);
          if P <> nil then
            NewSize := P - PLegacyChar(FData) + 1;
        end;
      end;
      SetDataSize(NewSize);
    end;
    with FHeader do
    begin
      Checksum := 0;
      Ext.NewHeaderOffset := 0;
    end;
  end;
end;

{ TExeSection }

destructor TExeSection.Destroy;
begin
  FreeMem(FData);
  inherited;
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

procedure TExeSection.Load(Source: TReadableStream);
var
  Pos: QuadWord;
begin
  Source.ReadBuffer(FHeader, SizeOf(FHeader));
  ReallocMem(FData, FHeader.RawDataSize);
  with Source do
  begin
    Pos := Position;
    try
      Position := FHeader.RawDataOffset;
      ReadBuffer(FData^, FHeader.RawDataSize);
    finally
      Position := Pos;
    end;
  end;
end;

procedure TExeSection.Save(Dest: TWritableStream);
var
  Pos: QuadWord;
begin
  with Dest do
  begin
    WriteBuffer(FHeader, SizeOf(FHeader));
    Pos := Position;
    try
      Position := FHeader.RawDataOffset;
      WriteBuffer(FData^, FHeader.RawDataSize);
    finally
      Position := Pos;
    end;
  end;
end;

{ TExeImage }

destructor TExeImage.Destroy;
begin
  FStub.Free;
  inherited;
end;

procedure TExeImage.Build;
var
  Offset: LongWord;
  I: Integer;
begin
  Offset := 0;

  if FStub <> nil then
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
        if CodeBase = FHeader.RawDataOffset then
          CodeBase := Offset
        else if DataBase = FHeader.RawDataOffset then
          DataBase := Offset;
        FHeader.RawDataOffset := Offset;
        Inc(Offset, FHeader.RawDataSize);
        Inc(Offset, FileAlignBytes(Offset));
      end;
    Checksum := 0;
  end;
  FHeaders.FileHeader.SectionCount := Count;
end;

function TExeImage.Extract(Index: Integer): TObject;
begin
  Result := inherited Extract(Index);
  Dec(FHeaders.FileHeader.SectionCount);
end;

function TExeImage.FileAlignBytes(Source: LongWord): LongWord;
begin
  Result := Source mod FHeaders.OptionalHeader.FileAlignment;
  if Result <> 0 then
    Result := FHeaders.OptionalHeader.FileAlignment - Result;
end;

function TExeImage.HeadersSize: LongWord;
begin
  if LongWord(FHeaders.Magic) = IMAGE_NT_SIGNATURE then
    Result := SizeOf(TImageNewHeaders) - SizeOf(FHeaders.OptionalHeader) +
      FHeaders.FileHeader.OptionalHeaderSize
  else
    Result := 0;
end;

function TExeImage.IndexOfSection(DirectoryIndex: Byte): Integer;
var
  A: LongWord;
begin
  A := FHeaders.OptionalHeader.DataDirectory[DirectoryIndex].VirtualAddress;
  for Result := 0 to Count - 1 do
    if FSections[Result].Header.VirtualAddress = A then
      Exit;
  Result := -1;
end;

procedure TExeImage.Load(Source: TReadableStream);
var
  I: Integer;
  Section: TExeSection;
begin
  if FStub = nil then
    FStub := TExeStub.Create;
  FStub.Load(Source);

  if (FStub.Size > SizeOf(TImageLegacyHeader)) and
    (FStub.Header.Ext.NewHeaderOffset <> 0) then
  begin
    with Source do
    begin
      Position := FStub.Header.Ext.NewHeaderOffset;
      ReadBuffer(FHeaders, SizeOf(FHeaders) - SizeOf(FHeaders.OptionalHeader));
      ReadBuffer(FHeaders.OptionalHeader, FHeaders.FileHeader.OptionalHeaderSize);
    end;
    with FHeaders.OptionalHeader do
      FillChar(DataDirectory[DirectoryEntryCount], SizeOf(DataDirectory) -
        DirectoryEntryCount * SizeOf(TImageDataDirectory), 0);
    for I := 0 to FHeaders.FileHeader.SectionCount - 1 do
    begin
      Section := TExeSection.Create;
      Section.Load(Source);
      Append(Section);
    end;
  end;
end;

procedure TExeImage.Load(FileName: PCoreChar);
var
  Source: TReadableStream;
begin
  Source := TFileStream.Create(FileName, faRead + [faRandom]);
  try
    Load(Source);
  finally
    Source.Free;
  end;
end;

procedure TExeImage.Save(Dest: TWritableStream);
var
  Dummy: array[0..4095] of Byte;
  Offset, H: LongWord;
  I: Integer;
begin
  FillChar(Dummy, SizeOf(Dummy), 0);

  Offset := 0;
  if FStub <> nil then
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
    with FSections[I], Dest do
    begin
      WriteBuffer(FData^, FHeader.RawDataSize);
      WriteBuffer(Dummy, FileAlignBytes(FHeader.RawDataSize));
    end;
end;

procedure TExeImage.Save(FileName: PCoreChar);
var
  Dest: TReadableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faSequential]);
  try
    Dest.Size := Size;
    Save(Dest);
  finally
    Dest.Free;
  end;
end;

function TExeImage.SectionAlignBytes(Source: LongWord): LongWord;
begin
  Result := Source mod FHeaders.OptionalHeader.SectionAlignment;
  if Result <> 0 then
    Result := FHeaders.OptionalHeader.SectionAlignment - Result;
end;

function TExeImage.Size: LongWord;
var
  I: Integer;
begin
  Result := 0;
  if FStub <> nil then
    Inc(Result, FStub.Size);

  Inc(Result, HeadersSize);
  Inc(Result, SizeOf(TImageSectionHeader) * Count);
  Inc(Result, FileAlignBytes(Result));
  for I := 0 to Count - 1 do
  begin
    Inc(Result, FSections[I].Header.RawDataSize);;
    Inc(Result, FileAlignBytes(Result));
  end;
end;

procedure TExeImage.Strip(Options: TStripOptions);

procedure DropSection(Index: Integer);
begin
  if Index <> 0 then
  begin
    with FSections[Index - 1].FHeader.Misc do
      Inc(VirtualSize, SectionAlignBytes(VirtualSize) + FSections[Index].FHeader.Misc.VirtualSize);
    Extract(Index).Free;
  end;
end;

var
  I: Integer;
begin
  if (soStub in Options) and (FStub <> nil) then
    FStub.Strip;

  if (soRelocations in Options) and (FHeaders.FileHeader.Characteristics and IMAGE_FILE_DLL = 0) and
    (FHeaders.OptionalHeader.DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_BASERELOC) then
  begin
    I := IndexOfSection(IMAGE_DIRECTORY_ENTRY_BASERELOC);
    if I >= 0 then
      DropSection(I);
    QuadWord(FHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC]) := 0;
    with FHeaders.FileHeader do
      Characteristics := Characteristics or IMAGE_FILE_RELOCS_STRIPPED;
  end;

  if Options * [soEmptySections, soOrphanedSections] <> [] then
    for I := Count - 1 downto 0 do
      with Sections[I] do
        if ((soEmptySections in Options) and (Header.RawDataSize = 0)) or
          ((soOrphanedSections in Options) and IsOrphaned(FHeaders.OptionalHeader))
        then
          DropSection(I);

  if soDirectory in Options then
    for I := FHeaders.OptionalHeader.DirectoryEntryCount - 1 downto IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG do
      if QuadWord(FHeaders.OptionalHeader.DataDirectory[I]) <> 0 then
      begin
        with FHeaders do
        begin
          OptionalHeader.DirectoryEntryCount := I + 1;
          FileHeader.OptionalHeaderSize := SizeOf(OptionalHeader) -
            SizeOf(OptionalHeader.DataDirectory) + (I + 1) * SizeOf(TImageDataDirectory);
        end;
        Break;
      end;
end;

end.

