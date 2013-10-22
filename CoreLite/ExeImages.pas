(*
    Executable images object model

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

type
  TImageLegacyHeaderExtension = packed record
    Reserved: array[0..3] of Word;
    OEMId, OEMInfo: Word;
    Reserved2: array[0..9] of Word;
    NewHeaderOffset: LongWord;
  end;

  PImageLegacyHeader = ^TImageLegacyHeader;
  TImageLegacyHeader = packed record
    Magic,
    LastPageBytes, FilePages, RelocationCount,
    HeaderParagraphs, MinAlloc, MaxAlloc,
    InitialSS, InitialSP,
    Checksum,
    InitialIP, InitialCS,
    RelocationsOffset, OverlayNumber: Word;
    Extension: TImageLegacyHeaderExtension;
  end;

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
    procedure Load(Source: TReadableStream);
    procedure Save(Dest: TWritableStream);

    property Header: PImageSectionHeader read FHeader;
    property Next: TExeSection read FNext;
    property Owner: TExeImage read FOwner;
    property Prior: TExeSection read FPrior;
  end;

  TStripOptions = set of (soStub, soDirectory, soRelocations);

  TExeImage = class(TList)
  private
  { hold } FFirst, FLast: TExeSection;
    FStub: TExeStub;
    FHeaders: PImageNtHeaders;
  public
    destructor Destroy; override;
    procedure Build;
    function HeadersSize: LongWord;
    procedure Load(Source: TReadableStream); overload;
    procedure Load(FileName: PCoreChar); overload;
    procedure Save(Dest: TWritableStream); overload;
    procedure Save(FileName: PCoreChar); overload;
    procedure Strip(Options: TStripOptions = [soStub..soRelocations]);

    property First: TExeSection read FFirst;
    property Headers: PImageNtHeaders read FHeaders;
    property Last: TExeSection read FLast;
    property Stub: TExeStub read FStub;
  end;

implementation

const
  LegacyPageBytes       = 512; // DOS page
  LegacyParagraphBytes  = 16;  // DOS paragraph

{ TExeStub }

destructor TExeStub.Destroy;
begin
  FreeMem(FHeader);
  inherited;
end;

procedure TExeStub.Expand;
const
  MinParagraphs = SizeOf(TImageLegacyHeader) div LegacyParagraphBytes;
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
    FHeader.Extension.NewHeaderOffset := GetSize;
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
  HeaderBytes = SizeOf(TImageLegacyHeader) - SizeOf(TImageLegacyHeaderExtension);
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
    NewSize := FHeader.Extension.NewHeaderOffset;
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
      FHeader.Extension.NewHeaderOffset := 0;
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
    with FOwner.FHeaders.OptionalHeader do
      for I := 0 to NumberOfRvaAndSizes - 1 do
        if FHeader.VirtualAddress = DataDirectory[I].VirtualAddress then
        begin
          Result := I;
          Exit;
        end;
  Result := -1;
end;

procedure TExeSection.Load(Source: TReadableStream);
var
  Pos: QuadWord;
begin
  ReallocMem(FHeader, SizeOf(TImageSectionHeader));
  Source.ReadBuffer(FHeader^, SizeOf(TImageSectionHeader));
  ReallocMem(FHeader, SizeOf(TImageSectionHeader) + FHeader.SizeOfRawData);
  with Source do
  begin
    Pos := Position;
    try
      Position := FHeader.PointerToRawData;
      ReadBuffer(PLegacyChar(FHeader)[SizeOf(TImageSectionHeader)], FHeader.SizeOfRawData);
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
      Position := FHeader.PointerToRawData;
      WriteBuffer(PLegacyChar(FHeader)[SizeOf(TImageSectionHeader)], FHeader.SizeOfRawData);
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

function TExeImage.HeadersSize: LongWord;
begin
  if FHeaders <> nil then
    Result := SizeOf(TImageNtHeaders) - SizeOf(FHeaders.OptionalHeader) +
      FHeaders.FileHeader.SizeOfOptionalHeader
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

  if (FStub.Size > SizeOf(TImageLegacyHeader)) and (FStub.Header.Extension.NewHeaderOffset <> 0) then
  begin
    Source.Position := FStub.Header.Extension.NewHeaderOffset;
    ReallocMem(FHeaders, SizeOf(TImageNtHeaders));
    Source.ReadBuffer(FHeaders^, SizeOf(TImageNtHeaders));
    ReallocMem(FHeaders, HeadersSize);

    for I := 0 to FHeaders.FileHeader.NumberOfSections - 1 do
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
  if FStub <> nil then
  begin
    FStub.Expand;
    FStub.Save(Dest);
  end;
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
      Dest.Size := FHeaders.OptionalHeader.SizeOfImage;
    Save(Dest);
  finally
    Dest.Free;
  end;
end;

procedure TExeImage.Strip(Options: TStripOptions = [soStub..soRelocations]);
var
  I: Integer;
begin
  if soStub in Options then
    FStub.Strip;

  if soDirectory in Options then
    for I := FHeaders.OptionalHeader.NumberOfRvaAndSizes - 1 downto 0 do
      with FHeaders.OptionalHeader.DataDirectory[I] do
       if (VirtualAddress <> 0) or (Size <> 0) then
       begin
         with FHeaders^ do
         begin
           OptionalHeader.NumberOfRvaAndSizes := I + 1;
           FileHeader.SizeOfOptionalHeader := SizeOf(OptionalHeader) -
             SizeOf(OptionalHeader.DataDirectory) + I * SizeOf(TImageDataDirectory);
         end;
         ReallocMem(FHeaders, HeadersSize);
         Break;
       end;

  if soRelocations in Options then
  begin
  end;
end;

end.

