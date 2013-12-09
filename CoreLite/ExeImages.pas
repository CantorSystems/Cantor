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

  TExeSection = class;               

  TExeSectionHandler = class(TObjects)
  public
    procedure Build; virtual; abstract;
    function DataSize: LongWord; virtual; abstract;
    procedure Load(Source: TExeSection); virtual; abstract;
    procedure Save(Dest: TWritableStream); virtual; abstract;
  end;

  TExeSection = class
  private
    FHeader: TImageSectionHeader;
    FData: Pointer;
    FHandler: TExeSectionHandler;
    procedure SetHandler(Value: TExeSectionHandler);
  public
    destructor Destroy; override;
    procedure Build;
    function DirectoryIndex(const OptionalHeader: TImageOptionalHeader): Integer;
    function IsOrphaned(const OptionalHeader: TImageOptionalHeader): Boolean;
    procedure Load(Source: TReadableStream);
    procedure Save(Dest: TWritableStream);
    function Size: LongWord;
    procedure Strip;

    property Data: Pointer read FData;
    property Handler: TExeSectionHandler read FHandler write SetHandler;
    property Header: TImageSectionHeader read FHeader;
  end;

  TStripOptions = set of (soStub, soDataDirectory, soRelocations, soCleanVersionInfo,
    soSectionData, soEmptySections, soOrphanedSections);

  PExeSectionArray = ^TExeSectionArray;
  TExeSectionArray = array[0..MaxInt div SizeOf(TExeSection) - 1] of TExeSection;

  TExeHeaderSection = (hsCode, hsData);

  TExeImage = class(TObjects)
  private
  { hold } FSections: PExeSectionArray;
  { hold } FOwnsSections: Boolean;
    FHeaders: TImageNewHeaders;
    FStub: TExeStub;
    function IndexOfAddress(Address: LongWord): Integer;
  public
    destructor Destroy; override;
    procedure Build(FileAlignment: LongWord = 512);
    function Extract(Index: Integer): TObject; {$IFNDEF Lite} override; {$ENDIF}
    function FileAlignBytes(Source: LongWord): LongWord;
    function HeadersSize: LongWord;
    function IndexOfSection(HeaderSection: TExeHeaderSection): Integer; overload;
    function IndexOfSection(DirectoryIndex: Byte): Integer; overload;
    function IndexOfSection(Name: PLegacyChar; Length: Integer): Integer; overload;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream; TruncLastSection: Boolean = True); overload;
    procedure Save(FileName: PCoreChar; TruncLastSection: Boolean = True); overload;
    function SectionAlignBytes(Source: LongWord): LongWord;
    function Size(TruncLastSection: Boolean = True): LongWord;
    procedure Strip(Options: TStripOptions = [soStub..soEmptySections]);

    property Headers: TImageNewHeaders read FHeaders;
    property OwnsSections: Boolean read FOwnsSections;
    property Sections: PExeSectionArray read FSections;
    property Stub: TExeStub read FStub;
  end;

  TExeResourceHandler = class
  public
    function DataSize: LongWord; virtual; abstract;
    procedure Load(Source: Pointer); virtual; abstract;
    procedure Save(Dest: TWritableStream); virtual; abstract;
  end;

  TExeResource = class
  private
    FHeader: TImageResourceDataEntry;
    FData: Pointer;
    FHandler: TExeResourceHandler;
    procedure SetHandler(Value: TExeResourceHandler);
  public
    procedure Build;
    procedure Load(Source: Pointer; const Header: TImageResourceDataEntry);
    procedure Save(Dest: TWritableStream);
    function Size: LongWord;

    property Data: Pointer read FData;
    property Handler: TExeResourceHandler read FHandler write SetHandler;
    property Header: TImageResourceDataEntry read FHeader;
  end;

  TExeResources = class;

  PExeResourceArray = ^TExeResourceArray;
  TExeResourceArray = array[0..MaxInt div SizeOf(TExeResource) - 1] of
    packed record
      case Byte of
        0: (Value: TObject);
        1: (AsResource: TExeResource);
        2: (AsResources: TExeResources);
    end;

  TExeResources = class(TExeSectionHandler)
  private
  { hold } FItems: PExeResourceArray;
  { hold } FOwnsItems: Boolean;
    FHeader: TImageResourceDirectory;
    FName: PWideChar;
    FId: Word;
    procedure Load(Source: Pointer; Header: PImageResourceDirectory); reintroduce; overload;
    function SaveNames(Dest: TWritableStream): LongWord;
    procedure SetId(Value: Word);
    procedure SetName(Value: PWideChar);
  public
    procedure Build; override;
    function DataSize: LongWord; override;
    function Extract(Index: Integer): TObject; {$IFNDEF Lite} override; {$ENDIF}
    function HeadersSize(Minimize: Boolean = True): LongWord;
    procedure Load(Source: TExeSection); overload; override;
    procedure Save(Dest: TWritableStream); override;

    property Header: TImageResourceDirectory read FHeader;
    property Id: Word read FId write SetId;
    property Items: PExeResourceArray read FItems;
    property Name: PWideChar read FName write SetName;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  PExeResourceNameArray = ^TExeResourceNameArray;
  TExeResourceNameArray = array[0..MaxInt div SizeOf(PImageResourceName) - 1] of PImageResourceName;

  TExeResourceNames = class(TArray)
  { hold } FItems: PExeResourceNameArray;
  public
    function Append(Item: PImageResourceName): Integer;
    function Extract(Index: Integer): PImageResourceName;
    procedure Insert(Index: Integer; Item: PImageResourceName);
  end;

  TExeVersionInfo = class(TExeResourceHandler)
  public
    function DataSize: LongWord; override;
    procedure Load(Source: Pointer); override;
    procedure Save(Dest: TWritableStream); override;
  end;

  TKolibriImage = class
  private
    FHeader: TKolibriHeader;
    FCode, FData, FImports, FExports: Pointer;
  public
    function AlignBytes(Source: LongWord): LongWord;
    procedure Build(Alignment: Byte = 3);
    procedure Clear;
    procedure Load(Image: TExeImage);
    procedure Save(Dest: TWritableStream; TruncLastSection: Boolean = True); overload;
    procedure Save(FileName: PCoreChar; TruncLastSection: Boolean = True); overload;
    function Size(TruncLastSection: Boolean = True): LongWord;

    property Code: Pointer read FCode;
    property Data: Pointer read FData;
    property Exports_: Pointer read FExports;
    property Header: TKolibriHeader read FHeader;
    property Imports: Pointer read FImports;
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

procedure TExeStub.Load(Source: TExeStub);               
var
  L: LongWord;
begin
  FHeader := Source.FHeader;
  L := DataSize;
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
begin
  if FData <> nil then
  begin
    Dest.WriteBuffer(FHeader, SizeOf(FHeader));
    Dest.WriteBuffer(FData^, Size - SizeOf(FHeader));
  end
  else
    Dest.WriteBuffer(FHeader, Size);
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
  FHandler.Free;
  FreeMem(FData);
  inherited;
end;

procedure TExeSection.Build;
begin
  if FHandler <> nil then
    FHeader.RawDataSize := FHandler.DataSize;
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
  if FHandler <> nil then
    FHandler.Load(Self);
end;

procedure TExeSection.Save(Dest: TWritableStream);
begin
  if FHandler <> nil then
    FHandler.Save(Dest)
  else
    Dest.WriteBuffer(FData^, FHeader.RawDataSize);
end;

procedure TExeSection.SetHandler(Value: TExeSectionHandler);
begin
{$IFNDEF Lite}
  if FHandler = Value then
    Exit;
{$ENDIF}
  FHandler.Free;
  FHandler := Value;
  if FHandler <> nil then
    FHandler.Load(Self);
end;

function TExeSection.Size: LongWord;
begin
  if FHandler <> nil then
    Result := FHandler.DataSize
  else
    Result := FHeader.RawDataSize;
end;

procedure TExeSection.Strip;
begin
  if (FHandler = nil) and (FData <> nil) then
    with FHeader do
      while (RawDataSize <> 0) and (PLegacyChar(FData)[RawDataSize - 1] = #0) do
        Dec(RawDataSize);
end;

{ TExeImage }

destructor TExeImage.Destroy;
begin
  FStub.Free;
  inherited;
end;

procedure TExeImage.Build(FileAlignment: LongWord);
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
        Build;
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
  if (Index <> 0) and (FSections[Index].FHeader.VirtualSize <> 0) then
    with FSections[Index - 1].FHeader do
      Inc(VirtualSize, SectionAlignBytes(VirtualSize) + FSections[Index].FHeader.VirtualSize);
  Result := inherited Extract(Index);
  Dec(FHeaders.FileHeader.SectionCount);
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
    Result := SizeOf(TImageNewHeaders) - SizeOf(FHeaders.OptionalHeader) +
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

function TExeImage.IndexOfSection(HeaderSection: TExeHeaderSection): Integer;
begin
  with FHeaders.OptionalHeader do
    if HeaderSection = hsCode then
      Result := IndexOfAddress(CodeBase)
    else
      Result := IndexOfAddress(DataBase);
end;

function TExeImage.IndexOfSection(DirectoryIndex: Byte): Integer;
begin
  Result := IndexOfAddress(FHeaders.OptionalHeader.DataDirectory[DirectoryIndex].VirtualAddress);
end;

function TExeImage.IndexOfSection(Name: PLegacyChar; Length: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    with FSections[Result] do
      if CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Name, Length,
        FHeader.Name, StrLen(FHeader.Name, IMAGE_SIZEOF_SHORT_NAME)) = CSTR_EQUAL
      then
        Exit;
  Result := -1;
end;

procedure TExeImage.Load(Source: TReadableStream);
var
  I: Integer;
  Section: TExeSection;
begin
  Clear;
  
  if FStub = nil then
    FStub := TExeStub.Create;
  FStub.Load(Source);

  if (FStub.Size >= SizeOf(TImageLegacyHeader)) and
    (FStub.Header.Ext.NewHeaderOffset <> 0) then
  begin
    with Source do
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

procedure TExeImage.Save(Dest: TWritableStream; TruncLastSection: Boolean);
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
  begin
    FSections[I].Save(Dest);
    if (I < Count - 1) or not TruncLastSection then
      Dest.WriteBuffer(Dummy, FileAlignBytes(Sections[I].FHeader.RawDataSize));
  end;
end;

procedure TExeImage.Save(FileName: PCoreChar; TruncLastSection: Boolean);
var
  Dest: TReadableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faSequential]);
  try
    Dest.Size := Size(TruncLastSection);
    Save(Dest, TruncLastSection);
  finally
    Dest.Free;
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
  Result := 0;
  if FStub <> nil then
    Inc(Result, FStub.Size);

  Inc(Result, HeadersSize);
  Inc(Result, SizeOf(TImageSectionHeader) * Count);
  Inc(Result, FileAlignBytes(Result));
  for I := 0 to Count - 1 do
  begin
    Inc(Result, FSections[I].Size);;
    if (I < Count - 1) or not TruncLastSection then
      Inc(Result, FileAlignBytes(Result));
  end;
end;

procedure TExeImage.Strip(Options: TStripOptions);
var
  I: Integer;
begin
  if (soStub in Options) and (FStub <> nil) then
    FStub.Strip;

  if (soRelocations in Options) and (FHeaders.FileHeader.Characteristics and IMAGE_FILE_DLL = 0) and
    (FHeaders.OptionalHeader.Subsystem in [IMAGE_SUBSYSTEM_WINDOWS_GUI, IMAGE_SUBSYSTEM_WINDOWS_CUI]) and
    (FHeaders.OptionalHeader.DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_BASERELOC) then
  begin
    I := IndexOfSection(IMAGE_DIRECTORY_ENTRY_BASERELOC);
    if I >= 0 then
      Extract(I).Free;
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

  if Options * [soSectionData..soOrphanedSections] <> [] then
  begin
    for I := Count - 1 downto 0 do
      with Sections[I] do
      begin
        if soSectionData in Options then
          Strip;
        if ((soEmptySections in Options) and (Header.RawDataSize = 0)) or
          ((soOrphanedSections in Options) and IsOrphaned(FHeaders.OptionalHeader))
        then
          Extract(I).Free;
      end;
  end;

  if soDataDirectory in Options then
    with FHeaders.OptionalHeader do
    begin
      for I := DirectoryEntryCount - 1 downto 0 do
        if QuadWord(DataDirectory[I]) <> 0 then // Fast core
        begin
          DirectoryEntryCount := I + 1;
          Exit;
        end;
      DirectoryEntryCount := 0;
    end;
end;

{ TExeResource }

procedure TExeResource.Build;
begin
  if FHandler <> nil then
    FHeader.DataSize := FHandler.DataSize;
end;

procedure TExeResource.Load(Source: Pointer; const Header: TImageResourceDataEntry);
begin
  FHeader := Header;
  FData := PLegacyChar(Source) + FHeader.DataOffset;
  if FHandler <> nil then
    FHandler.Load(FData);
end;

procedure TExeResource.Save(Dest: TWritableStream);
begin
  if FHandler <> nil then
    FHandler.Save(Dest)
  else
    Dest.WriteBuffer(FData^, FHeader.DataSize);
end;

procedure TExeResource.SetHandler(Value: TExeResourceHandler);
begin
{$IFNDEF Lite}
  if FHandler = Value then
    Exit;
{$ENDIF}
  FHandler.Free;
  FHandler := Value;
  if FHandler <> nil then
    FHandler.Load(FData);
end;

function TExeResource.Size: LongWord;
begin
  if FHandler <> nil then
    Result := FHandler.DataSize
  else
    Result := FHeader.DataSize;
end;

{ TExeResources }

procedure TExeResources.Build;
begin

end;

function TExeResources.DataSize: LongWord;
var
  I: Integer;
  ResSize: LongWord;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if FItems[I].Value is TExeResources then
      with FItems[I].AsResources do
      begin
        if Count <> 0 then
        begin
          if Name <> nil then
            Inc(Result, SizeOf(Id) + Id);
          Inc(Result, SizeOf(TImageResourceDirectoryEntry) * Count);
          Inc(Result, DataSize);
          if Count > 1 then
            Inc(Result, SizeOf(TImageResourceDirectory));
        end
      end
    else {$IFNDEF Lite} if FItems[I].Value is TExeResource then {$ENDIF}
    begin
      ResSize := FItems[I].AsResource.Size;
      if ResSize <> 0 then
        Inc(Result, SizeOf(TImageResourceDataEntry) + ResSize);
    end;
end;

function TExeResources.Extract(Index: Integer): TObject;
begin
  Result := inherited Extract(Index);
  if Result is TExeResources then
    if TExeResources(Result).Name <> nil then
      Dec(FHeader.NamedEntryCount)
    else
      Dec(FHeader.IdEntryCount);
end;

procedure TExeResources.Load(Source: Pointer; Header: PImageResourceDirectory);
var
  I: Integer;
  Res: PImageResourceDirectoryEntry;
  Children: TExeResources;
  Child: TExeResource;
  ResName: PImageResourceName;
begin
  Clear;
  FHeader := Header^;
  with FHeader do
    SetCapacity(NamedEntryCount + IdEntryCount);

  Res := Pointer(PLegacyChar(Header) + SizeOf(FHeader));
  for I := 0 to Capacity - 1 do
  begin
    Children := TExeResources.Create(True);

    if Res.Name and IMAGE_RESOURCE_NAME_IS_STRING <> 0 then
    begin
      ResName := Pointer(PLegacyChar(Source) + Res.Name and not IMAGE_RESOURCE_NAME_IS_STRING);
      with Children do
      begin
        FId := ResName.Length;
        FName := @ResName.UnicodeData;
      end;
    end
    else
      Children.FId := Res.Id;

    if Res.DataOffset and IMAGE_RESOURCE_DATA_IS_DIRECTORY <> 0 then
      Children.Load(Source, Pointer(PLegacyChar(Source) +
        Res.DataOffset and not IMAGE_RESOURCE_DATA_IS_DIRECTORY))
    else
    begin
      Children.Capacity := 1;
      Child := TExeResource.Create;
      Child.Load(Source, PImageResourceDataEntry(PLegacyChar(Source) + Res.DataOffset)^);
      Children.Append(Child);
    end;

    Append(Children);
    Inc(Res);
  end;
end;

function TExeResources.HeadersSize(Minimize: Boolean): LongWord;
var
  I: Integer;
begin
  Result := 0;
  if Count > 0 then
    Inc(Result, SizeOf(FHeader));
  Inc(Result, SizeOf(TImageResourceDirectoryEntry) * Count);
  for I := 0 to Count - 1 do
    with FItems[I] do
      if Value is TExeResources then
        Inc(Result, AsResources.HeadersSize(Minimize))
      else {$IFNDEF Lite} if FItems[I].Value is TExeResource then {$ENDIF}
        Inc(Result, SizeOf(AsResource.FHeader));
end;

procedure TExeResources.Load(Source: TExeSection);
begin
  Load(Source.Data, Source.Data);
end;

procedure TExeResources.Save(Dest: TWritableStream);

var
  Offset: LongWord;

begin
  if Count <> 0 then
  begin
//    Dest.WriteBuffer(FDirectory^, SizeOf(FDirectory^));
//    Offset := SizeOf(FDirectory^);

    if FId = 0 then
  end;
end;

function TExeResources.SaveNames(Dest: TWritableStream): LongWord;
var
  ResName: TImageResourceName;
begin
  Result := SizeOf(FHeader);

end;

procedure TExeResources.SetId(Value: Word);
begin
  FId := Value;
  FName := nil;
end;

procedure TExeResources.SetName(Value: PWideChar);
begin
  FName := Value;
  FId := WideStrLen(FName);
end;

{ TExeResourceNames }

function TExeResourceNames.Append(Item: PImageResourceName): Integer;
begin
  Result := inherited Append;
  FItems[Result] := Item;
end;

function TExeResourceNames.Extract(Index: Integer): PImageResourceName;
begin
  CheckIndex(Index);
  Result := FItems[Index];
  inherited Extract(Index);
end;

procedure TExeResourceNames.Insert(Index: Integer; Item: PImageResourceName);
begin
  inherited Insert(Index);
  FItems[Index] := Item;
end;

{ TExeVersionInfo }

function TExeVersionInfo.DataSize: LongWord;
begin
  Result := 0;
end;

procedure TExeVersionInfo.Load(Source: Pointer);
begin

end;

procedure TExeVersionInfo.Save(Dest: TWritableStream);
begin

end;

{ TKolibriImage }

function TKolibriImage.AlignBytes(Source: LongWord): LongWord;
begin
 Result := Source mod (1 shl FHeader.Alignment);
 if Result <> 0 then
   Result := 1 shl FHeader.Alignment - Result;
end;

procedure TKolibriImage.Build(Alignment: Byte);
begin
  FHeader.Magic.AsQuadWord := $00495242494C4F4B; // Fast core: 'KOLIBRI'#0
  FHeader.Alignment := Alignment;
end;

procedure TKolibriImage.Clear;
begin
  FillChar(FHeader, SizeOf(FHeader), 0);
  FCode := nil;
  FData := nil;
  FExports := nil;
  FImports := nil;
end;

procedure TKolibriImage.Load(Image: TExeImage);

procedure LoadEntry(Entry: TExeHeaderSection; var Data: Pointer; var Size: LongWord); overload;
var
  Idx: Integer;
begin
  Idx := Image.IndexOfSection(Entry);
  if Idx >= 0 then
  begin
    Size := Image.Sections[Idx].Size;
    Data := Image.Sections[Idx].Data;
  end;
end;

procedure LoadEntry(Entry: Byte; var Data: Pointer; var Size: LongWord); overload;
var
  Idx: Integer;
begin
  Idx := Image.IndexOfSection(Entry);
  if Idx >= 0 then
  begin
    Size := Image.Sections[Idx].Size;
    Data := Image.Sections[Idx].Data;
  end;
end;

begin
{$IFNDEF Lite}
  Clear;
{$ENDIF}
  with Image.Headers.OptionalHeader do
  begin
    FHeader.ImageBase := ImageBase;
    FHeader.EntryPoint := EntryPoint;
    FHeader.UninitDataSize :=  UninitializedDataSize;
    if Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI then
      FHeader.CommandLine := Pointer(-1);
  end;
  LoadEntry(hsCode, FCode, FHeader.CodeSize);
  LoadEntry(hsData, FData, FHeader.InitDataSize);
  LoadEntry(IMAGE_DIRECTORY_ENTRY_IMPORT, FImports, FHeader.ImportsSize);
  LoadEntry(IMAGE_DIRECTORY_ENTRY_EXPORT, FExports, FHeader.ExportsSize);
end;

procedure TKolibriImage.Save(Dest: TWritableStream; TruncLastSection: Boolean);
var
  Dummy: array[0..4095] of Byte;
begin
  FillChar(Dummy, SizeOf(Dummy), 0);
  with Dest do
  begin
    WriteBuffer(FHeader, SizeOf(FHeader));
    WriteBuffer(Dummy, AlignBytes(SizeOf(FHeader)));
    WriteBuffer(FCode^, FHeader.CodeSize);
    WriteBuffer(Dummy, AlignBytes(FHeader.CodeSize));
    WriteBuffer(FImports^, FHeader.ImportsSize);
    WriteBuffer(Dummy, AlignBytes(FHeader.ImportsSize));
    WriteBuffer(FExports^, FHeader.ExportsSize);
    WriteBuffer(Dummy, AlignBytes(FHeader.ExportsSize));
    WriteBuffer(FData^, FHeader.InitDataSize);
    if not TruncLastSection then
      WriteBuffer(Dummy, AlignBytes(FHeader.InitDataSize));
  end;
end;

procedure TKolibriImage.Save(FileName: PCoreChar; TruncLastSection: Boolean);
var
  Dest: TWritableStream;
begin
  Dest := TFileStream.Create(FileName, faRewrite + [faSequential]);
  try
    Save(Dest, TruncLastSection);
  finally
    Dest.Free;
  end;
end;

function TKolibriImage.Size(TruncLastSection: Boolean): LongWord;
begin
  Result := SizeOf(FHeader);
  Inc(Result, AlignBytes(Result));
  Inc(Result, FHeader.CodeSize);
  Inc(Result, AlignBytes(Result));
  Inc(Result, FHeader.ImportsSize);
  Inc(Result, AlignBytes(Result));
  Inc(Result, FHeader.ExportsSize);
  Inc(Result, AlignBytes(Result));
  Inc(Result, FHeader.InitDataSize);
  if not TruncLastSection then
    Inc(Result, AlignBytes(Result));
end;

end.

