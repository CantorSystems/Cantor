(*
    Lite Core Library (CoreLite)

    Core library string messages

    Copyright (c) 2008-2017 Vladislav Javadov (aka Freeman)
*)

unit CoreConsts;

interface

const
  CP_LOCALIZATION = 1252; // we're using ��, but only in exception messages

  sDivByZero          = 'Division by zero';
  sRangeError         = 'Range check error';
  sIntOverflow        = 'Integer overflow';
  sInvalidOp          = 'Invalid floating point operation';
  sZeroDivide         = 'Floating point division by zero';
  sOverflow           = 'Floating point overflow';
  sUnderflow          = 'Floating point underflow';
  sInvalidCast        = 'Invalid class typecast';
  sPrivilege          = 'Privileged instruction';
  sControlC           = '^C';
  sStackOverflow      = 'Stack overflow';

  sIntfNotSupported   = 'Interface not supported';
  sSafecallException  = 'Exception in safecall method';

  sOperationAborted   = 'Operation aborted';
  sOutOfMemory        = 'Out of memory';
  sInvalidPointer     = 'Invalid pointer operation';

  sReadAccess         = 'read';
  sWriteAccess        = 'write';

  sAccessViolation = 'Access violation at address %p:%hc%s of address %p';
  sModuleAccessViolation = 'Access violation at address %p in module �%s�:%hc%hs of address %p';

  sAbstractInstance = '%s instance abstract method call';
  sAbstractClass    = '%s abstract class method call';
  sInlineObject     = 'Inline object';
  
  sNotImplemented     = '%s support is not yet implemented';
  sGeneralFault       = 'General fault %#02X';  // e. g. 0x0F for 15
  sDelphiRuntimeIO    = 'Delphi runtime I/O';

  sAssertError = '%s (%s, line %d)';
  sAssertionFailed = 'Assertion failed';

  sMMX = 'This program requires MMX';

  sPlatformError  = '%s: %s';
  sPlatformError2 = '%s: %hs %d';

  sStreamRead = 'read';
  sStreamWrote = 'wrote';
  sStreamReadError = 'Unexpected end of stream';
  sStreamWriteError = 'Not enough space';
  sStreamError = '%hs: %hs only %u bytes of %u required';

  sVS_VERSION_INFO = 'VS_VERSION_INFO';
  sFixedVersionInfo = 'fixed version info';
  sVersionAndBuild = '%s build %u';
  sVersionAndRevision = '%s revision %u';

  sPerformanceCounter = 'performance counter';
  sPerformanceFrequency = 'performance frequency';

  sCastNull = 'Cannot cast null to %s';
  sCastToNull = 'Cannot cast object to null %s';
  sCastToNull2 = 'Cannot cast %s to null %s';
  sCastMistmatch = 'Cannot cast mismatch object to %s';
  sCastMistmatch2 = 'Cannot cast %s to %s';
  sCastUntyped = 'Cannot cast uninitialized object to %s';

  sIndexOutOfBounds = 'Index %d out of bounds %d..%d of %s';
  sIndexOutOfEmpty  = 'Index %d out of empty %s';
  sIndexOutOfNull   = 'Index %d out of null container';
  sRangeOutOfBounds = 'Range %d..%d out of bounds %d..%d of %s';
  sRangeOutOfEmpty  = 'Range %d..%d out of empty %s';
  sRangeOutOfNull   = 'Range %d..%d out of null container';
  sOutOfCapacity    = 'This %s can contain only %d items, cannot add %d more.';
  sNullCapacity     = 'Cannot add %d item into null container';

  sPosition = 'position';
  sSize = 'size';
  sCodePage = 'code page';
  sConsoleCodePage = 'console code page';

  sLegacyString = 'string';
  sWideString = 'UTF-16 string';
  sLegacyText = 'text';
  sWideText = 'UTF-16 text';

  sDecimalFormats = 'decimal formats';
  sHexadecimalFormats = 'hexadecimal formats';
  sLocaleInfo = 'locale info';

  sInteger = 'integer';
  sHexadecimal = 'hexadecimal';
  sInvalidInteger = '�%s� is not a valid %hs value';
  sNullInteger = 'Null string is not a valid %s value';

  sNotNativeUTF16BE = 'UTF-16 Big-Endian is not native for Windows API';
  sUTF32notSupported = 'UTF-32 not supported';

  sSystemCPtoUTF16 = 'System error �%s� while converting code page %u (%s) to UTF-16';
  sSystemUTF16toCP = 'System error �%s� while converting UTF-16 to code page %u (%s)';
  sUnicodeToCP = 'Cannot convert %hs string to code page %u (%s)';
  sCPtoCP = 'Cannot convert string between code page %u (%s) and code page %u (%s)';

  sLatin1 = 'ISO 8859-1 (Latin-1)';
  sUTF8 = 'UTF-8';
  sCESU8 = 'CESU-8';
  sUTF16 = 'UTF-16';
  sUTF32 = 'UTF-32';

  sInvalidString = '%s string contains %s';
  sInvalidChar = 'invalid character (U+%04X)';
  sNonLatin1 = 'a character outside of Latin-1 range (U+%04X)';
  sNonUnicode = 'a character outside of Unicode range (U+%04X)';
  sNonBMP = 'a character outside of Basic Multilingual Plane (U+%04X),%hcbut surrogate pairs are not allowed here';

  sBadUTF8 = 'Bad UTF-8 sequence starting with byte $%02X';
  sBrokenUTF8 = 'Broken %u-byte UTF-8 sequence or unexpected end of string';

  sBadSurrogatePair = 'Bad %s surrogate pair starting with a low surrogate (U+%04X)'; // UTF-16 or CESU-8
  sBrokenSurrogatePair = 'Broken %s surrogate pair or unexpected end of string:'#10 + // UTF-16 or CESU-8
    'high surrogate (U+%04X) is not complemented by a low surrogate';

  sFileNameList = 'file name list';

  sExeImage = 'executable image';
  sNotExecutableImage = 'Not an executable image';
  sInvalidImageStub = 'Invalid image stub � %u bytes on last page (max 512 allowed)';
  sInvalidWin32Image = 'Invalid Win32 image';
  sDotNETAssembly = '.NET assemblies cannot be handled';
  sUnknownExeImage = 'Unknown image signature �%c%c�';
  sLegacyExeImage = 'Image extension not found, DOS/DPMI image';
  sInvalidRVA = 'Invalid RVA %08X';
  sUnsupportedRelocationFormat = 'Unsupported relocation format, code %2u';

  sAvoidCharCorruption = 'This program uses UTF-8 console. To avoid on-screen Unicode characters'#10 +
    'corruption, please set a TrueType/OpenType font for console window.';
  sPressEnterToExit = 'Press ENTER to exit';
  sModuleFileName = 'module file name, length';

implementation

end.
