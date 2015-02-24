(*
    Lite Core Library (CoreLite mini)

    Core library string messages

    Copyright (c) 2008-2015 Vladislav Javadov (aka Freeman)
*)

unit CoreConsts;

interface

const
  CP_LEGACY = 1252; // we're using “”, but only in exception messages

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

  sReadAccess         = 'Read';
  sWriteAccess        = 'Write';

  sAccessViolation = 'Access violation at address %p. %hs of address %p';
  sModuleAccessViolation = 'Access violation at address %p in module “%ws”.'#10'%hs of address %p';

  sAbstractInstance = '%s instance abstract method call';
  sAbstractClass    = '%s abstract class method call';
  sInlineObject     = 'Inline object';
  
  sNotImplemented     = '%s support is not yet implemented';
  sGeneralFault       = 'General fault %#02X';  // e. g. 0x0F for 15

  sAssertError = '%s (%s, line %d)';
  sAssertionFailed = 'Assertion failed';

  sMMX = 'This program requires MMX';

  sPlatformError  = '%s: “%s”';
  sPlatformError2 = '%s: %hs %d';

  sStreamRead = 'read';
  sStreamWrote = 'wrote';
  sStreamReadError = 'Unexpected end of stream';
  sStreamWriteError = 'Not enough space';
  sStreamError = '%hs: %hs only %u bytes of %u required';

  sVS_VERSION_INFO = 'VS_VERSION_INFO';
  sFixedVersionInfo = 'fixed version info';
  sVersionAndBuild = '%s build %u';

  sPerformanceCounter = 'performance counter';
  sPerformanceFrequency = 'performance frequency';

  sCastNull = 'Cannot cast null to %s';
  sCastToNull = 'Cannot cast object to null %s';
  sCastToNull2 = 'Cannot cast %s to null %s';
  sCastMistmatch = 'Cannot cast mismatch object to %s';
  sCastMistmatch2 = 'Cannot cast %s to %s';
  sCastUntyped = 'Cannot cast uninitialized object to %s';

  sPosition = 'position';
  sCodePage = 'code page';
  sConsoleCodePage = 'console code page';

  sIndexOutOfBounds = 'Index %d out of bounds %d..%d of %s';
  sIndexOutOfEmpty  = 'Index %d out of empty %s';
  sIndexOutOfNull   = 'Index %d out of null container';
  sRangeOutOfBounds = 'Range %d..%d out of bounds %d..%d of %s';
  sRangeOutOfEmpty  = 'Range %d..%d out of empty %s';
  sRangeOutOfNull   = 'Range %d..%d out of null container';
  sFixedCapacity    = 'This %s can contain only %d items';
  sNullCapacity     = 'Cannot add an item into null container';

  sLegacyString = 'code paged or UTF-8 string';
  sWideString = 'UTF-16 string';

  sUnsupportedCodePage = 'Code page %u (%s) is not yet supported';

  sWhitespaceNo = ' no';
  sInvalidCodePageClass = 'Code page %u (%s) has%hs lead bytes'#10'that is not valid for %hs';

  sLatin1 = 'ISO 8859-1 (Latin-1)';

  sUTF8 = 'UTF-8';
  sUTF16 = 'UTF-16';
  sUTF32 = 'UTF-32';
  sCESU8 = 'CESU-8';

//  sBigEndian = '(Big-endian)';
//  sLittleEndian = '(Little-endian)';

  sInvalidCharSetToCharSet = 'Cannot convert %s string'#10'into %s character set';
  sInvalidCharSetToCodePage = 'Cannot convert %hs string'#10'into code page %u (%s)';
  sInvalidCodePageToCharSet = 'Cannot convert string between code page %u (%s)'#10'and %hs character set';
  sInvalidCodePageToCodePage = 'Cannot convert string between code page %u (%s)'#10'and code page %u (%s)';

  sInvalidChar = '%scontains invalid character (U+%04X)';

  sCannotMixCharSetAndCharSet = 'Cannot mix %s and %s characters at the same target';
  sCannotMixCharSetAndCodePage = 'Cannot mix %hs and %u (%s) characters at the same target';
  sCannotMixCodePageAndCharSet = 'Cannot mix %u (%s) and %hs characters at the same target';
  sCannotMixCodePageAndCodePage = 'Cannot mix %u (%s) and %u (%s) characters at the same target';

  sInvalidCharSetChar = 'Cannot convert %hs character (U+%04X)'#10'into %hs character set';
  sInvalidCodePageChar = 'Cannot convert %hs character (U+%04X)'#10'into code page %u (%s)';

  sCharSetSource = 'Source %s string ';
  sCodePageSource = 'Source string encoded with code page %u (%s)'#10;

  sNonUnicode = '%scontains characters outside of Unicode ranges';
  sDisallowedSurrogates = '%scontains characters outside of Basic Multilingual Plane,'#10 +
    'but surrogate pairs are not allowed here';

  sBadUTF8 = 'Bad UTF-8 sequence starting with byte $%02X';
  sBrokenUTF8 = 'Broken %u-byte UTF-8 sequence or unexpected end of string';

  sBadSurrogate = 'Bad %s surrogate pair starting with a low surrogate (U+%04X)'; // UTF-16 or CESU-8
  sBrokenSurrogate = 'Broken %s surrogate pair or unexpected end of string:'#10 + // UTF-16 or CESU-8
    'high surrogate (U+%04X) is not complemented by a low surrogate';

implementation

end.
