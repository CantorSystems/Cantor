(*
    Lite Core Library (CoreLite)

    Core library string messages

    Copyright (c) 2008-2013 Vladislav Javadov (Freeman)
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

  sAbstractMethodCall = '%hs class abstract method call';
  sNotImplemented     = '%s support is not yet implemented';
  sGeneralFault       = 'General fault %#02X';  // e. g. 0x0F for 15

  sAssertError = '%s (%s, line %d)';
  sAssertionFailed = 'Assertion failed';

  sMMX = 'This program requires MMX';

  sPlatformError = '%s: “%s”';
  sVS_VERSION_INFO = 'VS_VERSION_INFO';
  sVersionAndBuild = '%s build %u';

  sStreamRead = 'read';
  sStreamWrote = 'wrote';
  sStreamReadError = 'Unexpected end of stream';
  sStreamWriteError = 'Not enough space';
  sStreamError = '%hs: %hs only %u bytes of %u required';

  sIndexOutOfBounds = 'Index %hs[%d] out of bounds %d..%d';
  sIndexOfNull      = 'Index %hs[%d] where container is null';
  sRangeOutOfBounds = 'Range %hs[%d..%d] out of bounds %d..%d';
  sFixedCapacity    = 'This %hs object can contain only %d items';

  sNotExecutableImage = 'Not an executable image';
  sNotValidWin32Image = 'Not valid Win32 image';
  sDotNETAssembly = '.NET assemblies cannot be handled';
  sUnknownExeImage = 'Unknown image signature: %c%c';
  sBadExeImage = 'Bad image: %s';

  sUnsupportedCodePage = 'Code page %u (%s) is not yet supported';

  sWhitespaceNo = ' no';
  sInvalidCodePageClass = 'Code page %u (%s) has%hs lead bytes'#10'that is not valid for %hs';

//  sLatin = '7-bit ASCII';
  sLatin1 = 'Latin-1 (ISO 8859-1)';

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

  sInvalidCharSetChar = 'Cannot convert %hs character “%lc” (U+%04X)'#10'into %hs character set';
  sInvalidCodePageChar = 'Cannot convert %hs character “%lc” (U+%04X)'#10'into code page %u (%s)';

  sNonUnicodeCharSet = '%s string contains characters outside of Unicode range';
  sNonUnicodeCodePage = 'Source string encoded with code page %u (%s)'#10'contains characters outside of Unicode range';

  sOutsideOfBMP = 'outside of Basic Multilingual Plane (BMP)'#10'but surrogate pairs are not allowed here';
  sSurrogates = '%s string contains characters %s';
  sSurrogateChar = '%s character (U+%05X) lies %s';

  sSecond = '2nd';
  sThird  = '3rd';
  sFourth = '4th';
  sFifth  = '5th';

  sBadUTF8 = 'Bad UTF-8 sequence starting with byte $%02X';
  sBrokenUTF8 = 'Broken %u-byte UTF-8 sequence or unexpected end of string';

  sBadSurrogate = 'Bad %s surrogate pair starting with low surrogate (U+%04X)'; // UTF-16 or CESU-8
  sBrokenSurrogate = 'Broken %s surrogate pair or unexpected end of string:'#10 +
    'high surrogate (U+%04X) is not continued by a low surrogate'; // --"--

  sBasicLatin                      = 'Basic Latin';
  sLatin1Supplement                = 'Latin-1 Supplement';
  sLatinExtA                       = 'Latin Extended-A';
  sLatinExtB                       = 'Latin Extended-B';
  sIPAExtensions                   = 'IPA Extensions';
  sSpacingModLetters               = 'Spacing Modifier Letters';
  sCombiDiacriticalMarks           = 'Combining Diacritical Marks';
  sGreekAndCoptic                  = 'Greek and Coptic';
  sCyrillic                        = 'Cyrillic';
  sCyrillicSupplement              = 'Cyrillic Supplement';
  sArmenian                        = 'Armenian';
  sHebrew                          = 'Hebrew';
  sArabic                          = 'Arabic';
  sSyriac                          = 'Syriac';
  sArabicSupplement                = 'Arabic Supplement';
  sThaana                          = 'Thaana';
  sNKo                             = 'N’Ko';
  sSamaritan                       = 'Samaritan';
  sMandaic                         = 'Mandaic';
  sArabicExtA                      = 'Arabic Extended-A';
  sDevanagari                      = 'Devanagari';
  sBengali                         = 'Bengali';
  sGurmukhi                        = 'Gurmukhi';
  sGujarati                        = 'Gujarati';
  sOriya                           = 'Oriya';
  sTamil                           = 'Tamil';
  sTelugu                          = 'Telugu';
  sKannada                         = 'Kannada';
  sMalayalam                       = 'Malayalam';
  sSinhala                         = 'Sinhala';
  sThai                            = 'Thai';
  sLao                             = 'Lao';
  sTibetan                         = 'Tibetan';
  sMyanmar                         = 'Myanmar';
  sGeorgian                        = 'Georgian';
  sHangulJamo                      = 'Hangul Jamo';
  sEthiopic                        = 'Ethiopic';
  sEthiopicSupplement              = 'Ethiopic Supplement';
  sCherokee                        = 'Cherokee';
  sUniCanadianSyllabics            = 'Unified Canadian Aboriginal Syllabics';
  sOgham                           = 'Ogham';
  sRunic                           = 'Runic';
  sTagalog                         = 'Tagalog';
  sHanunoo                         = 'Hanunoo';
  sBuhid                           = 'Buhid';
  sTagbanwa                        = 'Tagbanwa';
  sKhmer                           = 'Khmer';
  sMongolian                       = 'Mongolian';
  sUniCanadianSyllabicsExt         = 'Unified Canadian Aboriginal Syllabics Extended';
  sLimbu                           = 'Limbu';
  sTaiLe                           = 'Tai Le';
  sNewTaiLue                       = 'New Tai Lue';
  sKhmerSymbols                    = 'Khmer Symbols';
  sBuginese                        = 'Buginese';
  sTaiTham                         = 'Tai Tham';
  sBalinese                        = 'Balinese';
  sSundanese                       = 'Sundanese';
  sBatak                           = 'Batak';
  sLepcha                          = 'Lepcha';
  sOlChiki                         = 'Ol Chiki';
  sSundaneseSupplement             = 'Sundanese Supplement';
  sVedicExtensions                 = 'Vedic Extensions';
  sPhoneticExt                     = 'Phonetic Extensions';
  sPhoneticExtSupplement           = 'Phonetic Extensions Supplement';
  sCombiDiacriticalMarksSupplement = 'Combining Diacritical Marks Supplement';
  sLatinExtAdditional              = 'Latin Extended Additional';
  sGreekExt                        = 'Greek Extended';
  sGeneralPunctuation              = 'General Punctuation';
  sSuperscriptsAndSubscripts       = 'Superscripts and Subscripts';
  sCurrencySymbols                 = 'Currency Symbols';
  sCombiDiacriticalMarksForSymbols = 'Combining Diacritical Marks for Symbols';
  sLetterlikeSymbols               = 'Letterlike Symbols';
  sNumForms                        = 'Number Forms';
  sArrows                          = 'Arrows';
  sMathOperators                   = 'Mathematical Operators';
  sMiscTechnical                   = 'Miscellaneous Technical';
  sControlPictures                 = 'Control Pictures';
  sOpticalCharRecognition          = 'Optical Character Recognition';
  sEnclosedAlphanums               = 'Enclosed Alphanumerics';
  sBoxDrawing                      = 'Box Drawing';
  sBlockElements                   = 'Block Elements';
  sGeometricShapes                 = 'Geometric Shapes';
  sMiscSymbols                     = 'Miscellaneous Symbols';
  sDingbats                        = 'Dingbats';
  sMiscMathSymbolsA                = 'Miscellaneous Mathematical Symbols-A';
  sSupplementalArrowsA             = 'Supplemental Arrows-A';
  sBraillePatterns                 = 'Braille Patterns';
  sSupplementalArrowsB             = 'Supplemental Arrows-B';
  sMiscMathSymbolsB                = 'Miscellaneous Mathematical Symbols-B';
  sSupplementalMathOperators       = 'Supplemental Mathematical Operators';
  sMiscSymbolsAndArrows            = 'Miscellaneous Symbols and Arrows';
  sGlagolitic                      = 'Glagolitic';
  sLatinExtC                       = 'Latin Extended-C';
  sCoptic                          = 'Coptic';
  sGeorgianSupplement              = 'Georgian Supplement';
  sTifinagh                        = 'Tifinagh';
  sEthiopicExt                     = 'Ethiopic Extended';
  sCyrillicExtA                    = 'Cyrillic Extended-A';
  sSupplementalPunctuation         = 'Supplemental Punctuation';
  sCJKRadicalsSupplement           = 'CJK Radicals Supplement';
  sKangxiRadicals                  = 'Kangxi Radicals';
  sIdeographicDescChars            = 'Ideographic Description';// Characters';
  sCJKSymbolsAndPunctuation        = 'CJK Symbols and Punctuation';
  sHiragana                        = 'Hiragana';
  sKatakana                        = 'Katakana';
  sBopomofo                        = 'Bopomofo';
  sHangulCompatJamo                = 'Hangul Compatibility Jamo';
  sKanbun                          = 'Kanbun';
  sBopomofoExt                     = 'Bopomofo Extended';
  sCJKStrokes                      = 'CJK Strokes';
  sKatakanaPhoneticExt             = 'Katakana Phonetic Extensions';
  sEnclosedCJKLettersAndMonths     = 'Enclosed CJK Letters and Months';
  sCJKCompat                       = 'CJK Compatibility';
  sCJKUniIdeographsExtA            = 'CJK Unified Ideographs Extension A';
  sYijingHexagramSymbols           = 'Yijing Hexagram Symbols';
  sCJKUniIdeographs                = 'CJK Unified Ideographs';
  sYiSyllables                     = 'Yi Syllables';
  sYiRadicals                      = 'Yi Radicals';
  sLisu                            = 'Lisu';
  sVai                             = 'Vai';
  sCyrillicExtB                    = 'Cyrillic Extended-B';
  sBamum                           = 'Bamum';
  sModToneLetters                  = 'Modifier Tone Letters';
  sLatinExtD                       = 'Latin Extended-D';
  sSylotiNagri                     = 'Syloti Nagri';
  sCommonIndicNumForms             = 'Common Indic Number Forms';
  sPhagsPa                         = 'Phags-pa';
  sSaurashtra                      = 'Saurashtra';
  sDevanagariExt                   = 'Devanagari Extended';
  sKayahLi                         = 'Kayah Li';
  sRejang                          = 'Rejang';
  sHangulJamoExtA                  = 'Hangul Jamo Extended-A';
  sJavanese                        = 'Javanese';
  sCham                            = 'Cham';
  sMyanmarExtA                     = 'Myanmar Extended-A';
  sTaiViet                         = 'Tai Viet';
  sMeeteiMayekExt                  = 'Meetei Mayek Extensions';
  sEthiopicExtA                    = 'Ethiopic Extended-A';
  sMeeteiMayek                     = 'Meetei Mayek';
  sHangulSyllables                 = 'Hangul Syllables';
  sHangulJamoExtB                  = 'Hangul Jamo Extended-B';
  sHighSurrogates                  = 'High Surrogates';
  sHighPrivateSurrogates           = 'High Private Use Surrogates';
  sLowSurrogates                   = 'Low Surrogates';
  sPrivateArea                     = 'Private Use Area';
  sCJKCompatIdeographs             = 'CJK Compatibility Ideographs';
  sAlphabeticPresentForms          = 'Alphabetic Presentation Forms';
  sArabicPresentFormsA             = 'Arabic Presentation Forms-A';
  sVarSelectors                    = 'Variation Selectors';
  sVerticalForms                   = 'Vertical Forms';
  sCombiHalfMarks                  = 'Combining Half Marks';
  sCJKCompatForms                  = 'CJK Compatibility Forms';
  sSmallFormVariants               = 'Small Form Variants';
  sArabicPresentFormsB             = 'Arabic Presentation Forms-B';
  sHalfwidthAndFullwidthForms      = 'Halfwidth and Fullwidth Forms';
  sSpecials                        = 'Specials';
  sLinearBSyllabary                = 'Linear B Syllabary';
  sLinearBIdeograms                = 'Linear B Ideograms';
  sAegeanNums                      = 'Aegean Numbers';
  sAncientGreekNums                = 'Ancient Greek Numbers';
  sAncientSymbols                  = 'Ancient Symbols';
  sPhaistosDisc                    = 'Phaistos Disc';
  sLycian                          = 'Lycian';
  sCarian                          = 'Carian';
  sOldItalic                       = 'Old Italic';
  sGothic                          = 'Gothic';
  sUgaritic                        = 'Ugaritic';
  sOldPersian                      = 'Old Persian';
  sDeseret                         = 'Deseret';
  sShavian                         = 'Shavian';
  sOsmanya                         = 'Osmanya';
  sCypriotSyllabary                = 'Cypriot Syllabary';
  sImperialAramaic                 = 'Imperial Aramaic';
  sPhoenician                      = 'Phoenician';
  sLydian                          = 'Lydian';
  sMeroiticHieroglyphs             = 'Meroitic Hieroglyphs';
  sMeroiticCursive                 = 'Meroitic Cursive';
  sKharoshthi                      = 'Kharoshthi';
  sOldSouthArabian                 = 'Old South Arabian';
  sAvestan                         = 'Avestan';
  sInscriptionalParthian           = 'Inscriptional Parthian';
  sInscriptionalPahlavi            = 'Inscriptional Pahlavi';
  sOldTurkic                       = 'Old Turkic';
  sRumiNumeralSymbols              = 'Rumi Numeral Symbols';
  sBrahmi                          = 'Brahmi';
  sKaithi                          = 'Kaithi';
  sSoraSompeng                     = 'Sora Sompeng';
  sChakma                          = 'Chakma';
  sSharada                         = 'Sharada';
  sTakri                           = 'Takri';
  sCuneiform                       = 'Cuneiform';
  sCuneiformNumsAndPunctuation     = 'Cuneiform Numbers and Punctuation';
  sEgyptianHieroglyphs             = 'Egyptian Hieroglyphs';
  sBamumSupplement                 = 'Bamum Supplement';
  sMiao                            = 'Miao';
  sKanaSupplement                  = 'Kana Supplement';
  sByzantineMusicalSymbols         = 'Byzantine Musical Symbols';
  sMusicalSymbols                  = 'Musical Symbols';
  sAncientGreekMusicalNotation     = 'Ancient Greek Musical Notation';
  sTaiXuanJingSymbols              = 'Tai Xuan Jing Symbols';
  sCountingRodNumerals             = 'Counting Rod Numerals';
  sMathAlphanumSymbols             = 'Mathematical Alphanumeric Symbols';
  sArabicMathAlphabeticSymbols     = 'Arabic Mathematical Alphabetic Symbols';
  sMahjongTiles                    = 'Mahjong Tiles';
  sDominoTiles                     = 'Domino Tiles';
  sPlayingCards                    = 'Playing Cards';
  sMiscSymbolsAndPictographs       = 'Miscellaneous Symbols And Pictographs';
  sEmoticons                       = 'Emoticons';
  sTransportAndMapSymbols          = 'Transport And Map Symbols';
  sAlchemicalSymbols               = 'Alchemical Symbols';
  sEnclosedAlphanumSupplement      = 'Enclosed Alphanumeric Supplement';
  sEnclosedIdeographicSupplement   = 'Enclosed Ideographic Supplement';
  sCJKUniIdeographsExtB            = 'CJK Unified Ideographs Extension B';
  sCJKUniIdeographsExtC            = 'CJK Unified Ideographs Extension C';
  sCJKUniIdeographsExtD            = 'CJK Unified Ideographs Extension D';
  sCJKCompatIdeographsSupplement   = 'CJK Compatibility Ideographs Supplement';
  sTags                            = 'Tags';
  sVarSelectorsSupplement          = 'Variation Selectors Supplement';
  sPrivateAreaA                    = 'Supplementary Private Use Area-A';
  sPrivateAreaB                    = 'Supplementary Private Use Area-B';

  sStrong       = 'strong';
  sEmphasis     = 'emphasis';
  sInsert       = 'insert';
  sDelete       = 'delete';
  sSmall        = 'small';
  sMark         = 'mark';
  sSubscript    = 'subscript';
  sSuperscript  = 'superscript';
  sNoWrap       = 'nowrap';
  sKeyboard     = 'keyboard';
  sSample       = 'sample';
  sVariable     = 'variable';
  sDefinition   = 'definition';
  sCite         = 'cite';
  sQuote        = 'quote';
  sEmotion      = 'emotion';
  sSpelling     = 'spelling';

implementation

end.

