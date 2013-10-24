(*
    Executable images object model

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

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
    CheckSum: LongWord;
    Subsystem, DLLCharacteristics: Word;
    StackReserveSize, StackCommitSize,
    HeapReserveSize, HeapCommitSize,
    LoaderFlags,
    DirectoryEntryCount: LongWord;
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

const  
  IMAGE_DIRECTORY_ENTRY_EXPORT             = 0; 
  IMAGE_DIRECTORY_ENTRY_IMPORT             = 1; 
  IMAGE_DIRECTORY_ENTRY_RESOURCE           = 2; 
  IMAGE_DIRECTORY_ENTRY_EXCEPTION          = 3; 
  IMAGE_DIRECTORY_ENTRY_SECURITY           = 4; 
  IMAGE_DIRECTORY_ENTRY_BASERELOC          = 5; 
  IMAGE_DIRECTORY_ENTRY_DEBUG              = 6; 
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT          = 7; 
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR          = 8; 
  IMAGE_DIRECTORY_ENTRY_TLS                = 9; 
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG       = 10; 
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT      = 11; 
  IMAGE_DIRECTORY_ENTRY_IAT               = 12;

const
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT      = 13;
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR    = 14; 

type
  TExeStub = class
  private
    FHeader: PImageLegacyHeader;
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
  public
    destructor Destroy; override;
    procedure Expand;
    procedure Load(Source: TExeStub); overload;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    procedure Strip(Heuristics: Boolean = True);

    property Header: PImageLegacyHeader read FHeader;
    property Size: LongWord read GetSize write SetSize;
  end;

  TExeImage = class;

  TExeSection = class(TListItem)
  private
  { hold } FOwner: TExeImage;
  { hold } FPrior, FNext: TExeSection;
    FHeader: PImageSectionHeader;
  public
    destructor Destroy; override;
    function DirectoryIndex: Integer;
//    procedure Extract; override;
    procedure Load(Source: TReadableStream);
    procedure Save(Dest: TWritableStream);

    property Header: PImageSectionHeader read FHeader;
    property Next: TExeSection read FNext;
    property Owner: TExeImage read FOwner;
    property Prior: TExeSection read FPrior;
  end;

  TStripOptions = set of (soStub, soDirectory, soEmptySections, soOrphanedSections,
     soRelocations{, soRedundantResources});

  TExeImage = class(TList)
  private
  { hold } FFirst, FLast: TExeSection;
    FStub: TExeStub;
    FHeaders: PImageNewHeaders;
  public
    destructor Destroy; override;
    procedure Build;
    function FindSection(DirectoryIndex: Byte): TExeSection;
    function HeadersSize: LongWord;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    procedure Strip(Options: TStripOptions = [soStub..soRelocations]);

    property First: TExeSection read FFirst;
    property Headers: PImageNewHeaders read FHeaders;
    property Last: TExeSection read FLast;
    property Stub: TExeStub read FStub;
  end;

implementation

const
  LegacyPageBytes       = 512; // DOS page
  LegacyParagraphBytes  = 16;  // DOS paragraph
  MinParagraphs         = SizeOf(TImageLegacyHeader) div LegacyParagraphBytes;

{ TExeStub }

destructor TExeStub.Destroy;
begin
  FreeMem(FHeader);
  inherited;
end;

procedure TExeStub.Expand;
var
  Old, New, L, Tail: Integer;
begin
  if FHeader <> nil then
  begin
    if FHeader.HeaderParagraphs < MinParagraphs then
    begin
      New := MinParagraphs * LegacyParagraphBytes;
      L := GetSize;
      if L > SizeOf(TImageLegacyHeader) then
      begin
        Old := FHeader^.HeaderParagraphs * LegacyParagraphBytes;
        Tail := L - Old;
        SetSize(New + Tail);
        Move(PLegacyChar(FHeader)[Old], PLegacyChar(FHeader)[New], Tail);
        FHeader.HeaderParagraphs := MinParagraphs;
      end
      else
      begin
        Old := L;
        SetSize(New + 2);
        PWord(PLegacyChar(FHeader) + SizeOf(TImageLegacyHeader))^ := $E2EB; // JMP -28
      end;
      FillChar(PLegacyChar(FHeader)[Old], New - Old, 0); // instead of ð*ùRich<ð*ù
    end;
    FHeader.Ext.NewHeaderOffset := GetSize;
  end;
end;

function TExeStub.GetSize: LongWord;
begin
  if FHeader <> nil then
    with FHeader^ do
      if LastPageBytes <> 0 then
        Result := (FilePages - 1) * LegacyPageBytes + LastPageBytes
      else
        Result := FilePages * LegacyPageBytes
  else
    Result := 0;
end;

procedure TExeStub.Load(Source: TExeStub);
var
  L: LongWord;
begin
  L := Source.Size;
  ReallocMem(FHeader, L);
  Move(Source.FHeader^, FHeader^, L);
end;

procedure TExeStub.Load(Source: TReadableStream);
const
  HeaderBytes = SizeOf(TImageLegacyHeader) - SizeOf(TImageLegacyHeaderExt);
var
  L: LongInt;
begin
  ReallocMem(FHeader, HeaderBytes);
  Source.ReadBuffer(FHeader^, HeaderBytes);
  L := GetSize;
  if L > HeaderBytes then
  begin
    ReallocMem(FHeader, L);
    Source.ReadBuffer(PLegacyChar(FHeader)[HeaderBytes], L - HeaderBytes);
  end;
end;

procedure TExeStub.Save(Dest: TWritableStream);
begin
  Dest.WriteBuffer(FHeader^, GetSize);
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

procedure TExeStub.Save(FileName: PCoreChar);
var
  Dest: TWritableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faSequential]);
  try
    Dest.Size := GetSize;
    Save(Dest);
  finally
    Dest.Free;
  end;
end;

procedure TExeStub.SetSize(Value: LongWord);
begin
  ReallocMem(FHeader, Value);
  if FHeader <> nil then
    with FHeader^ do
    begin
      FilePages := (Value + LegacyPageBytes - 1) div LegacyPageBytes;
      LastPageBytes := Value mod LegacyPageBytes;
    end;
end;

procedure TExeStub.Strip(Heuristics: Boolean);
var
  NewSize, L: LongWord;
  P, Limit: PLegacyChar;
begin
  if FHeader <> nil then
  begin
    NewSize := FHeader.Ext.NewHeaderOffset;
    L := GetSize;
    if (NewSize <> 0) and (L > NewSize) then
    begin
      if Heuristics then
      begin
        Limit := PLegacyChar(FHeader) + L;
        P := PLegacyChar(FHeader) + FHeader.HeaderParagraphs * LegacyParagraphBytes;
        P := StrScan(P, #$4C, Limit - P);               // MOV AX, 4C01h
        if (P <> nil) and (PWord(P + 1)^ = $21CD) then  // INT 21h
        begin
          Inc(P, 3);
          P := StrScan(P, '$', Limit - P);
          if P <> nil then
            NewSize := P - PLegacyChar(FHeader) + 1;
        end;
      end;
      SetSize(NewSize);
      FHeader.Ext.NewHeaderOffset := 0;
    end;
  end;
end;

{ TExeSection }

destructor TExeSection.Destroy;
begin
  FreeMem(FHeader);
  inherited;
end;

function TExeSection.DirectoryIndex: Integer;
var
  I: Integer;
begin
  if FOwner <> nil then
    with FOwner.Headers.OptionalHeader do
      for I := 0 to DirectoryEntryCount - 1 do
        if FHeader.VirtualAddress = DataDirectory[I].VirtualAddress then
        begin
          Result := I;
          Exit;
        end;
  Result := -1;
end;

{procedure TExeSection.Extract;
var
  Idx: Integer;
  Section: TExeSection;
begin
  if FHeader <> nil then
  begin
    Idx := DirectoryIndex;
    if Idx >= 0 then
      with FOwner.FHeaders.OptionalHeader.DataDirectory[Idx] do
      begin
        VirtualAddress := 0;
        Size := 0;
      end;
    if (FOwner <> nil) and (FOwner.FHeaders <> nil) then
      Dec(FOwner.FHeaders.FileHeader.SectionCount);

    Section := FNext;
    while Section <> nil do
    begin
      if Section.FHeader <> nil then
        with Section.FHeader^ do
        begin
          Dec(RawDataOffset, FHeader.RawDataSize);
          Dec(VirtualAddress, FHeader.Misc.VirtualSize);
        end;
      Section := Section.FNext;
    end;
  end;
  inherited;
end;}

procedure TExeSection.Load(Source: TReadableStream);
var
  Pos: QuadWord;
begin
  ReallocMem(FHeader, SizeOf(TImageSectionHeader));
  Source.ReadBuffer(FHeader^, SizeOf(TImageSectionHeader));
  ReallocMem(FHeader, SizeOf(TImageSectionHeader) + FHeader.RawDataSize);
  with Source do
  begin
    Pos := Position;
    try
      Position := FHeader.RawDataOffset;
      ReadBuffer(PLegacyChar(FHeader)[SizeOf(TImageSectionHeader)], FHeader.RawDataSize);
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
    WriteBuffer(FHeader^, SizeOf(TImageSectionHeader));
    Pos := Position;
    try
      Position := FHeader.RawDataOffset;
      WriteBuffer(PLegacyChar(FHeader)[SizeOf(TImageSectionHeader)], FHeader.RawDataSize);
    finally
      Position := Pos;
    end;
  end;
end;

{ TExeImage }

destructor TExeImage.Destroy;
begin
  FreeMem(FHeaders);
  FStub.Free;
  inherited;
end;

procedure TExeImage.Build;
begin

end;

function TExeImage.FindSection(DirectoryIndex: Byte): TExeSection;
begin
  Result := First;
  while Result <> nil do
    with Result, FHeaders.OptionalHeader.DataDirectory[DirectoryIndex] do
    begin
      if (Header <> nil) and (Header.VirtualAddress = VirtualAddress) then
        Break;
      Result := Next;
    end;
end;

function TExeImage.HeadersSize: LongWord;
begin
  if FHeaders <> nil then
    Result := SizeOf(TImageNewHeaders) - SizeOf(FHeaders.OptionalHeader) +
      FHeaders.FileHeader.OptionalHeaderSize
  else
    Result := 0;
end;

procedure TExeImage.Load(Source: TReadableStream);
var
  I: Integer;
  Section: TExeSection;
begin
  if FStub = nil then
    FStub := TExeStub.Create;
  FStub.Load(Source);

  if (FStub.Header <> nil) and (FStub.Header.HeaderParagraphs >= MinParagraphs) and
    (FStub.Header.Ext.NewHeaderOffset <> 0) then
  begin
    Source.Position := FStub.Header.Ext.NewHeaderOffset;
    ReallocMem(FHeaders, SizeOf(TImageNewHeaders));
    Source.ReadBuffer(FHeaders^, SizeOf(TImageNewHeaders));
    ReallocMem(FHeaders, HeadersSize);

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
  Section: TExeSection;
begin
  Build;
  if FStub <> nil then
    FStub.Save(Dest);
  Dest.WriteBuffer(FHeaders^, HeadersSize);
  Section := First;
  while Section <> nil do
  begin
    Section.Save(Dest);
    Section := Section.Next;
  end;
end;

procedure TExeImage.Save(FileName: PCoreChar);
var
  Dest: TReadableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faRandom]);
  try
    if FHeaders <> nil then
      Dest.Size := FHeaders.OptionalHeader.ImageSize;
    Save(Dest);
  finally
    Dest.Free;
  end;
end;

procedure TExeImage.Strip(Options: TStripOptions = [soStub..soRelocations]);
var
  A: LongWord;
  I: Integer;
  Section, T: TExeSection;
begin
  if (soStub in Options) and (FStub <> nil) then
    FStub.Strip;

  if Options * [soEmptySections, soOrphanedSections] <> [] then
  begin
    Section := Last;
    while Section <> nil do
      if (Section.Header <> nil) and (((soEmptySections in Options) and (Section.Header.RawDataSize = 0)) or
        ((soOrphanedSections in Options) and (Section.DirectoryIndex < 0))) then
      begin
        T := Section.Prior;
        Section.Free;
        Section := T;
      end
      else
        Section := Section.Prior;
  end;

  if FHeaders <> nil then
  begin
    if (soRelocations in Options) and (FHeaders.FileHeader.Characteristics and IMAGE_FILE_DLL = 0) and
      (FHeaders.OptionalHeader.DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_BASERELOC) then
    begin
      with FHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC] do
      begin
        A := VirtualAddress;
        VirtualAddress := 0;
        Size := 0;
      end;

      Section := First;
      while Section <> nil do
      begin
        if Section.Header <> nil then
        begin
          if (Section.Header.VirtualAddress = A) and (Section.Header.RawDataSize <> 0) then
          begin
            T := Section.Next;
            Section.Free;
            Section := T;
            Continue;
          end;
          with Section.Header^ do
          begin
            RelocationsOffset := 0;
            RelocationCount := 0;
          end;
        end;
        Section := Section.Next;
      end;

      with FHeaders.FileHeader do
        Characteristics := Characteristics or IMAGE_FILE_RELOCS_STRIPPED;
    end;

    if soDirectory in Options then
      for I := FHeaders.OptionalHeader.DirectoryEntryCount - 1 downto 0 do
        if QuadWord(FHeaders.OptionalHeader.DataDirectory[I]) <> 0 then
        begin
          with FHeaders^ do
          begin
           OptionalHeader.DirectoryEntryCount := I + 1;
           FileHeader.OptionalHeaderSize := SizeOf(OptionalHeader) -
             SizeOf(OptionalHeader.DataDirectory) + I * SizeOf(TImageDataDirectory);
          end;
          ReallocMem(FHeaders, HeadersSize);
          Break;
        end;
  end;
end;

end.

