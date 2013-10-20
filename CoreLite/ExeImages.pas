(*
    Executable images object model

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

unit ExeImages;

interface

uses
  Windows, CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

type
  PImageLegacyHeader = ^TImageLegacyHeader;
  TImageLegacyHeader = packed record
    Magic,
    LastPageBytes, FilePages, RelocationCount,
    HeaderParagraphs, MinAlloc, MaxAlloc,
    InitialSS, InitialSP,
    Checksum,
    InitialIP, InitialCS,
    RelocationsOffset, OverlayNumber: Word;
    Reserved: array[0..3] of Word;
    OEMId, OEMInfo: Word;
    Reserved2: array[0..9] of Word;
    NewHeaderOffset: LongWord;
  end;

  TExeStub = class
  private
    FHeader: PImageLegacyHeader;
  public
    constructor Create(Source: TReadableStream);
    destructor Destroy; override;
    procedure Save(Dest: TWritableStream);
    function Size: LongInt;

    property Header: PImageLegacyHeader read FHeader;
  end;

  TExeSections = class;

  TExeSection = class(TRedBlackTreeItem)
  private
    { hold } FOwner: TExeSections;
    { hold } FLeft, FRight, FParent: TExeSection;
    { hold } FRed: Boolean;
    FHeader: PImageSectionHeader;
  public
    function Compare(Item: TBalancedTreeItem): Integer; override;

    property Header: PImageSectionHeader read FHeader;
    property Left: TExeSection read FLeft;
    property Owner: TExeSections read FOwner;
    property Parent: TExeSection read FParent;
    property Red: Boolean read FRed;
    property Right: TExeSection read FRight;
  end;

  TExeSections = class(TRedBlackTree)
  private
    { hold } FRoot: TExeSection;
  public
    property Root: TExeSection read FRoot;
  end;

  TExeImage = class
  private
    PDosHeader: PImageDosHeader;
    PNtHeaders: PImageNtHeaders;
    
  public
    procedure Load(Source: Pointer);
    procedure Save(Dest: Pointer);
  end;


implementation

{ TExeStub }

constructor TExeStub.Create(Source: TReadableStream);
var
  L: LongInt;
begin
  GetMem(FHeader, SizeOf(TImageLegacyHeader));
  Source.ReadBuffer(FHeader^, SizeOf(TImageLegacyHeader));
  L := Size;
  if L > SizeOf(TImageLegacyHeader) then
  begin
    ReallocMem(FHeader, L);
    Source.ReadBuffer(PLegacyChar(FHeader)[SizeOf(TImageLegacyHeader)],
      L - SizeOf(TImageLegacyHeader));
  end;
end;

destructor TExeStub.Destroy;
begin
  FreeMem(FHeader);
  inherited;
end;

procedure TExeStub.Save(Dest: TWritableStream);
begin
  Dest.WriteBuffer(FHeader^, Size);
end;

function TExeStub.Size: LongInt;
begin
  Result := (FHeader.FilePages - 1) * 512{bytes per DOS page} + FHeader.LastPageBytes;
end;

{ TExeSection }

function TExeSection.Compare(Item: TBalancedTreeItem): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, @FHeader.Name, StrLen(@FHeader.Name, Length(FHeader.Name)),
    @TExeSection(Item).FHeader.Name, StrLen(@TExeSection(Item).FHeader.Name, Length(TExeSection(Item).FHeader.Name))) - 2;
end;

{ TExeImage }

procedure TExeImage.Load(Source: Pointer);
begin

end;

procedure TExeImage.Save(Dest: Pointer);
begin

end;

end.

