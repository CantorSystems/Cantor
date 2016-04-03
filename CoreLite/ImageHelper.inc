(*
    Image helper structures for CoreLite

    Copyright (c) 2013, 2016 Vladislav Javadov (aka Freeman)
*)

const // for Delphi 6
  IMAGE_FILE_LARGE_ADDRESS_AWARE  = $0020;

  IMAGE_FILE_MACHINE_WCEMIPSV2    = $0169;  // MIPS little-endian WCE v2
  IMAGE_FILE_MACHINE_SH3          = $01A2;  // SH3 little-endian
  IMAGE_FILE_MACHINE_SH3DSP       = $01A3;
  IMAGE_FILE_MACHINE_SH3E         = $01A4;  // SH3E little-endian
  IMAGE_FILE_MACHINE_SH4          = $01A6;  // SH4 little-endian
  IMAGE_FILE_MACHINE_SH5          = $01A8;  // SH5
  IMAGE_FILE_MACHINE_ARM          = $01C0;  // ARM Little-Endian
  IMAGE_FILE_MACHINE_THUMB        = $01C2;
  IMAGE_FILE_MACHINE_AM33         = $01D3;
  IMAGE_FILE_MACHINE_POWERPCFP    = $01F1;
  IMAGE_FILE_MACHINE_IA64         = $0200;  // Intel 64
  IMAGE_FILE_MACHINE_MIPS16       = $0266;  // MIPS
  IMAGE_FILE_MACHINE_ALPHA64      = $0284;  // ALPHA64
  IMAGE_FILE_MACHINE_MIPSFPU      = $0366;  // MIPS
  IMAGE_FILE_MACHINE_MIPSFPU16    = $0466;  // MIPS
  IMAGE_FILE_MACHINE_AXP64        = IMAGE_FILE_MACHINE_ALPHA64;
  IMAGE_FILE_MACHINE_TRICORE      = $0520;  // Infineon
  IMAGE_FILE_MACHINE_CEF          = $0CEF;
  IMAGE_FILE_MACHINE_EBC          = $0EBC;  // EFI Byte Code
  IMAGE_FILE_MACHINE_AMD64        = $8664;  // AMD64 (K8)
  IMAGE_FILE_MACHINE_M32R         = $9041;  // M32R little-endian
  IMAGE_FILE_MACHINE_CEE          = $C0EE;

  IMAGE_SUBSYSTEM_NATIVE_WINDOWS           = 8;
  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI           = 9;
  IMAGE_SUBSYSTEM_EFI_APPLICATION          = 10;
  IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER  = 11;
  IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER       = 12;
  IMAGE_SUBSYSTEM_EFI_ROM                  = 13;
  IMAGE_SUBSYSTEM_XBOX                     = 14;
  IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION = 16;

  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE    = $0040; // ASLR-aware
  IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY = $0080;
  IMAGE_DLLCHARACTERISTICS_NX_COMPAT       = $0100;
  IMAGE_DLLCHARACTERISTICS_NO_ISOLATION    = $0200;
  IMAGE_DLLCHARACTERISTICS_NO_SEH          = $0400;
  IMAGE_DLLCHARACTERISTICS_NO_BIND         = $0800;
  IMAGE_DLLCHARACTERISTICS_WDM_DRIVER      = $2000;
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;

  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT       = 13;
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR     = 14;

  IMAGE_RESOURCE_NAME_IS_STRING            = $80000000;
  IMAGE_RESOURCE_DATA_IS_DIRECTORY         = $80000000;

  IMAGE_REL_BASED_ABSOLUTE        = 0;
  IMAGE_REL_BASED_HIGH            = 1;
  IMAGE_REL_BASED_LOW             = 2;
  IMAGE_REL_BASED_HIGHLOW         = 3;  // PE x86 (and x64?)
  IMAGE_REL_BASED_HIGHADJ         = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR    = 5;
  IMAGE_REL_BASED_MIPS_JMPADDR16  = 9;
  IMAGE_REL_BASED_IA64_IMM64      = 9;
  IMAGE_REL_BASED_DIR64           = 10;

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

  PImageOptionalHeader32 = ^TImageOptionalHeader32;
  TImageOptionalHeader32 = packed record
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

  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  TImageOptionalHeader64 = packed record
    Magic: Word;
    MajorLinkerVersion, MinorLinkerVersion: Byte;
    CodeSize, InitializedDataSize, UninitializedDataSize,
    EntryPoint, CodeBase, DataBase: LongWord;
    ImageBase: QuadWord;
    SectionAlignment, FileAlignment: LongWord;
    MajorOSVersion, MinorOSVersion,
    MajorImageVersion, MinorImageVersion,
    MajorSubsystemVersion, MinorSubsystemVersion: Word;
    Win32Version,
    ImageSize, HeadersSize,
    Checksum: LongWord;
    Subsystem, DLLCharacteristics: Word;
    StackReserveSize, StackCommitSize,
    HeapReserveSize, HeapCommitSize: QuadWord;
    LoaderFlags: LongWord;
    DirectoryEntryCount: LongInt;
    DataDirectory: array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;

{$IFDEF CPUX64}
  PImageOptionalHeader = PImageOptionalHeader64;
  TImageOptionalHeader = TImageOptionalHeader64;
{$ELSE}
  PImageOptionalHeader = PImageOptionalHeader32;
  TImageOptionalHeader = TImageOptionalHeader32;
{$ENDIF}

  PImageNewHeaders32 = ^TImageNewHeaders32;
  TImageNewHeaders32 = packed record
    Magic: array[0..3] of LegacyChar;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader32;
  end;

  PImageNewHeaders64 = ^TImageNewHeaders64;
  TImageNewHeaders64 = packed record
    Magic: array[0..3] of LegacyChar;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;

{$IFDEF CPUX64}
  PImageNewHeaders = PImageNewHeaders64;
  TImageNewHeaders = TImageNewHeaders64;
{$ELSE}
  PImageNewHeaders = PImageNewHeaders32;
  TImageNewHeaders = TImageNewHeaders32;
{$ENDIF}

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
