(*
    Executable images object model

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;
                                       
{$I ImageHelper.inc}

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

    property Data: Pointer read FData;
    property Handler: TExeSectionHandler read FHandler write SetHandler;
    property Header: TImageSectionHeader read FHeader;
  end;

  TStripOptions = set of (soStub, soDataDirectory, soRelocations, soCleanResources,
    soEmptySections, soOrphanedSections);

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
    function IndexOfSection(DirectoryIndex: Byte): Integer; overload;
    function IndexOfSection(Name: PLegacyChar; Length: Integer): Integer; overload;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    function SectionAlignBytes(Source: LongWord): LongWord;
    function Size: LongWord;
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

  TExeResourceData = record
    Data: Pointer;
    Size, Locale: LongWord;
    Handler: TExeResourceHandler;
  end;

  PExeResourceDataArray = ^TExeResourceDataArray;
  TExeResourceDataArray = array[0..MaxInt div SizeOf(TExeResourceData) - 1] of TExeResourceData;

  TExeResource = class(TArray)
  private
  { hold } FItems: PExeResourceDataArray;
    FDirectory: PImageResourceDirectory;
    FId: Word;
    FName: PWideChar;
  public
    class function ItemSize: Integer; override;
    procedure Load(Source: Pointer; const Res: TImageResourceDirectoryEntry);
    function Size: LongWord;

    property Directory: PImageResourceDirectory read FDirectory;
    property Id: Word read FId;
    property Items: PExeResourceDataArray read FItems;
    property Name: PWideChar read FName;
  end;

  PExeResourceArray = ^TExeResourceArray;
  TExeResourceArray = array[0..MaxInt div SizeOf(TExeResource) - 1] of TExeResource;

  TExeResources = class(TExeSectionHandler)
  private
  { hold } FItems: PExeResourceArray;
    FDirectory: PImageResourceDirectory;
  public
    constructor Create(Section: TExeSection; Delta: Integer = 0); overload;
    function DataSize: LongWord; override;
    procedure Load(Source: TExeSection); override;
    procedure Save(Dest: TWritableStream); override;

    property Directory: PImageResourceDirectory read FDirectory;
    property Items: PExeResourceArray read FItems;
  end;

  TExeVersionInfo = class(TExeResourceHandler)
  public
    function DataSize: LongWord; override;
    procedure Load(Source: Pointer); override;
    procedure Save(Dest: TWritableStream); override;
  end;

{ Exceptions }

  EBadImage = class(Exception);

  EUnknownImage = class(EBadImage)
  private
    FHeaders: PImageNewHeaders;
  public
    constructor Create(const Headers: TImageNewHeaders);
    property Headers: PImageNewHeaders read FHeaders;
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
  FHeaders := @Headers;
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
  if L >= SizeOf(FHeader.Ext) then
    Dec(L, SizeOf(FHeader.Ext));
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
begin
  if FData <> nil then
    with Dest do
    begin
      WriteBuffer(FHeader, SizeOf(FHeader));
      WriteBuffer(FData^, DataSize);
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
      if FHandler <> nil then
        FHandler.Save(Dest)
      else
        WriteBuffer(FData^, FHeader.RawDataSize);
    finally
      Position := Pos;
    end;
  end;
end;

procedure TExeSection.SetHandler(Value: TExeSectionHandler);
begin
{$IFNDEF Lite}
  if FHandler = Value then
    Exit;
{$ENDIF}
  FHandler.Free;
  FHandler := Value;
  FHandler.Load(Self);
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

function TExeImage.IndexOfSection(DirectoryIndex: Byte): Integer;
var
  A: LongWord;
begin
  A := FHeaders.OptionalHeader.DataDirectory[DirectoryIndex].VirtualAddress;
  if A <> 0 then
    for Result := 0 to Count - 1 do
      if FSections[Result].Header.VirtualAddress = A then
        Exit;
  Result := -1;
end;

function TExeImage.IndexOfSection(Name: PLegacyChar; Length: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    with FSections[Result] do
      if CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Name, Count,
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
  begin
    with FSections[I] do
    begin
      if FHandler <> nil then
        FHandler.Save(Dest)
      else
        Dest.WriteBuffer(FData^, FHeader.RawDataSize);
    end;
    if I < Count - 1 then
      Dest.WriteBuffer(Dummy, FileAlignBytes(Sections[I].FHeader.RawDataSize));
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
  Result := FHeaders.OptionalHeader.SectionAlignment;
  if Result <> 0 then
  begin
    Result := Source mod Result;
    if Result <> 0 then
      Result := FHeaders.OptionalHeader.SectionAlignment - Result;
  end;
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
    if I < Count - 1 then
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
    (FHeaders.OptionalHeader.DirectoryEntryCount >= IMAGE_DIRECTORY_ENTRY_BASERELOC) then
  begin
    I := IndexOfSection(IMAGE_DIRECTORY_ENTRY_BASERELOC);
    if I >= 0 then
      Extract(I).Free;
    QuadWord(FHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC]) := 0; // Fast core
    with FHeaders.FileHeader do
      Characteristics := Characteristics or IMAGE_FILE_RELOCS_STRIPPED;
  end;

  if Options * [soEmptySections, soOrphanedSections] <> [] then
  begin
    for I := Count - 1 downto 0 do
      with Sections[I] do
        if ((soEmptySections in Options) and (Header.RawDataSize = 0)) or
          ((soOrphanedSections in Options) and IsOrphaned(FHeaders.OptionalHeader))
        then
          Extract(I).Free;
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

class function TExeResource.ItemSize: Integer;
begin
  Result := SizeOf(TExeResourceData);
end;

procedure TExeResource.Load(Source: Pointer; const Res: TImageResourceDirectoryEntry);
begin
//  if Res.DataOffset and IMAGE_RESOURCE_DATA_IS_DIRECTORY <> 0 then
//    raise EBadImage.Create(sBadExeImage, [sResDirAtSingleResLevel]);

  if Res.Name and IMAGE_RESOURCE_NAME_IS_STRING <> 0 then
    with PImageResourceName((PLegacyChar(Source) + Res.Name and not IMAGE_RESOURCE_NAME_IS_STRING))^ do
    begin
      FId := Length;
      FName := @UnicodeData;
    end
  else
    FId := Res.Name;

//  LoadData(Source, PImageResourceDataEntry(PLegacyChar(Source) + Res.DataOffset and not IMAGE_RESOURCE_DATA_IS_DIRECTORY)^);
end;

function TExeResource.Size: LongWord;
begin
  Result := 0;
end;

{ TExeResources }

constructor TExeResources.Create(Section: TExeSection; Delta: Integer);
begin
  with PImageResourceDirectory(Section.Data)^ do
    inherited Create(NamedEntryCount + IdEntryCount, Delta, True);
  Load(Section);
end;

function TExeResources.DataSize: LongWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
//    Inc(Result, FItems[I].Size);
end;

procedure TExeResources.Load(Source: TExeSection);
var
  ResType, ChildRes: PImageResourceDirectoryEntry;
  ResDir: PImageResourceDirectory;
  I, J, H: Integer;
begin
  Clear;
  FDirectory := PImageResourceDirectory(Source.Data);
  ResType := Pointer(PLegacyChar(Source.Data) + SizeOf(FDirectory));
  for I := 0 to FDirectory.NamedEntryCount + FDirectory.IdEntryCount - 1 do
  begin
    if ResType.Name and IMAGE_RESOURCE_NAME_IS_STRING <> 0 then
      raise EBadImage.Create(sNotImplemented, [sNamedResourceTypes]);

{    ResClass := TExeResource;
    for H := 0 to HandlerCount - 1 do
      if Handlers[H].ResourceType = ResType.Name then
      begin
        ResClass := Handlers[H].HandlerClass;
        Break;
      end;}

    if ResType.DataOffset and IMAGE_RESOURCE_DATA_IS_DIRECTORY <> 0 then
    begin
      ResDir := Pointer(PLegacyChar(Source.Data) + ResType.DataOffset and not IMAGE_RESOURCE_DATA_IS_DIRECTORY);
      ChildRes := Pointer(PLegacyChar(ResDir) + SizeOf(ResDir^));
      for J := 0 to ResDir.NamedEntryCount + ResDir.IdEntryCount - 1 do
      begin
//        AppendRes(ChildRes);
        Inc(ChildRes);
      end;
    end
    else
//      AppendRes(Pointer(PLegacyChar(ResType) + ResType.DataOffset));

    Inc(ResType);
  end;
end;

procedure TExeResources.Save(Dest: TWritableStream);
begin

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

end.

