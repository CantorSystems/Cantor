(*
    Image helper structures for CoreLite

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

const // for Delphi 6
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT    = 13;
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR  = 14;

  IMAGE_FILE_LARGE_ADDRESS_AWARE        = $0020;

  IMAGE_RESOURCE_NAME_IS_STRING         = $80000000;
  IMAGE_RESOURCE_DATA_IS_DIRECTORY      = $80000000;

  IMAGE_REL_BASED_HIGHLOW = 3;

{
  RT_NEW            = $2000;
  RT_ERROR          = $7FFF;

  RT_CURSOR         = 1;
  RT_BITMAP         = 2;
  RT_ICON           = 3;
  RT_MENU           = 4;
  RT_DIALOG         = 5;
  RT_STRING         = 6;
  RT_FONTDIR        = 7;
  RT_FONT           = 8;
  RT_ACCELERATORS   = 9;
  RT_RCDATA         = 10;
  RT_MESSAGETABLE   = 11;
  RT_GROUP_CURSOR   = 12;
  RT_GROUP_ICON     = 14;
  RT_VERSION        = 16;
  RT_NEWBITMAP      = RT_BITMAP or RT_NEW;
  RT_NEWMENU        = RT_MENU or RT_NEW;
  RT_NEWDIALOG      = RT_DIALOG or RT_NEW;

  StringsPerBlock = 16;}

type
  TImageLegacyHeaderExt = packed record
    Reserved: array[0..1] of Word;
    OEMId, OEMInfo: Word;
    Reserved2: array[0..9] of Word;
    NewHeaderOffset: LongWord;
  end;

  PImageLegacyHeader = ^TImageLegacyHeader;
  TImageLegacyHeader = packed record
    Magic: array[0..1] of LegacyChar;
    LastPageBytes, FilePages, RelocationCount,
    HeaderParagraphs, MinAlloc, MaxAlloc,
    InitialSS, InitialSP,
    Checksum,
    InitialIP, InitialCS,
    RelocationsOffset, OverlayNumber: Word;
    Reserved: array[0..1] of Word;
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
    Magic: array[0..3] of LegacyChar;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader;
  end;

  PImageSectionHeader = ^TImageSectionHeader;
  TImageSectionHeader = packed record
    Name: array[0..IMAGE_SIZEOF_SHORT_NAME - 1] of LegacyChar;
    case Byte of
      0: (VirtualSize, VirtualAddress,
          RawDataSize, RawDataOffset,
          RelocationsOffset, LineNumbersOffset: LongWord;
          RelocationCount, LineNumberCount: Word;
          Characteristics: LongWord);
      1: (PhysicalAddress: LongWord);
  end;

  PImageBaseRelocation = ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress, BlockSize: LongWord;
  end;

  PImageResourceDirectory = ^TImageResourceDirectory;
  TImageResourceDirectory = packed record
    Characteristics, TimeDateStamp: LongWord;
    MajorVersion, MinorVersion,
    NamedEntryCount, IdEntryCount: Word;
  end;

  PImageResourceDirectoryEntry = ^TImageResourceDirectoryEntry;
  TImageResourceDirectoryEntry = packed record
    case Byte of
      0: (Name, DataOffset: LongWord);
      1: (Id: Word);
  end;

  PImageResourceDataEntry = ^TImageResourceDataEntry;
  TImageResourceDataEntry = packed record
    DataOffset, DataSize, Locale, Reserved: LongWord;
  end;

  PImageResourceName = ^TImageResourceName;
  TImageResourceName = packed record
    Length: Word;
    case Byte of
      0: (LegacyData: array [0..0] of LegacyChar);
      1: (UnicodeData: array [0..0] of WideChar);
  end;

  PIconHeader = ^TIconHeader;
  TIconHeader = packed record
    Reserved, ResType, ComponentCount: Word;
  end;

  PIconResInfo = ^TIconResInfo;
  TIconResInfo = packed record
    Width, Height, Colors, Reserved: Byte;
    Planes, BitsPerPixel: Word;
    DataSize: LongWord;
    NameOrdinal: Word;
  end;

  PCursorResInfo = ^PIconResInfo;
  TCursorResInfo = TIconResInfo;

{  PVarFileInfo = ^TVarFileInfo;
  TVarFileInfo = packed record
    Length, ValueLength, DataType: Word;
    Key: array[0..10] of WideChar;
    Padding: array[0..1] of Word;
  end;

  PStringFileInfo = ^TStringFileInfo;
  TStringFileInfo = packed record
    Length, ValueLength, DataType: Word;
    Key: array[0..12] of WideChar;
    Padding: array[0..1] of Word;
  end;}
