(*
    The Unified Environment Core Library

    The Unified Environment Core Library

    Core library string messages

    Copyright (c) 2008-2012 The Unified Environment Laboratory

    Conditional defines:
      * Interfaces -- interface support
*)

unit CoreConsts;

interface

const
  CP_LEGACY = 1252; // we're using “”

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
{$IFDEF Interfaces}
  sIntfNotSupported   = 'Interface not supported';
  sSafecallException  = 'Exception in safecall method';
{$ENDIF}
  sOperationAborted   = 'Operation aborted';
  sOutOfMemory        = 'Out of memory';
  sInvalidPointer     = 'Invalid pointer operation';

  sReadAccess         = 'Read';
  sWriteAccess        = 'Write';

  sAccessViolation = 'Access violation at address %p. %hs of address %p';
  sModuleAccessViolation = 'Access violation at address %p in module “%ws”.'#10'%hs of address %p';

  sAbstractError    = 'Abstract method call'; // instead of meaningless "Abstract Error"
  sGeneralFault     = 'General fault %#02X';  // e. g. 0x0F for 15

{$IFOPT C+}
  sAssertError = '%s (%s, line %i)';
  sAssertionFailed = 'Assertion failed';
{$ENDIF}

  sConsistentRead = 'consistent read';
  sSyncUpdate = 'syncronized update';
  sExclusiveLock = 'exclusive lock';
  sDestroy = 'destroy';
  sSharingViolation = 'Sharing violation while trying to %s of %s object';

  sIndexOutOfBounds = 'Index out of bounds %i..%i on %hs[%i]';

  sLatin = '7-bit ASCII';
  sLatin1 = 'Latin-1 (ISO 8859-1)';

  sUTF8 = 'UTF-8';
  sUTF16 = 'UTF-16';
  sUTF32 = 'UTF-32';

//  sBigEndian = '(Big-endian)';
//  sLittleEndian = '(Little-endian)';

{$IFDEF Lite}
  sInvalidLatin = 'Cannot convert %s string using %s character set';
  sInvalidCodePage = 'Cannot convert %hs string using code page %s';
{$ELSE}
  sInvalidLatin = 'Cannot convert %s character using %s character set';
  sInvalidCodePage = 'Cannot convert %hs character using code page %s';
{$ENDIF}

  sNoCodePage = '%s has no assigned code page';
  sNonUnicode = '%s string contains characters outside of Unicode range'; // UTF-8, UTF-32
  sSurrogates = '%s string contains characters outside of Basic Multilingual Plane'#10 +
    'but surrogate pairs are not allowed here'; // UTF-8, UTF-16, UTF-32

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
  sKharoshthi                      = 'Kharoshthi';
  sOldSouthArabian                 = 'Old South Arabian';
  sAvestan                         = 'Avestan';
  sInscriptionalParthian           = 'Inscriptional Parthian';
  sInscriptionalPahlavi            = 'Inscriptional Pahlavi';
  sOldTurkic                       = 'Old Turkic';
  sRumiNumeralSymbols              = 'Rumi Numeral Symbols';
  sBrahmi                          = 'Brahmi';
  sKaithi                          = 'Kaithi';
  sCuneiform                       = 'Cuneiform';
  sCuneiformNumsAndPunctuation     = 'Cuneiform Numbers and Punctuation';
  sEgyptianHieroglyphs             = 'Egyptian Hieroglyphs';
  sBamumSupplement                 = 'Bamum Supplement';
  sKanaSupplement                  = 'Kana Supplement';
  sByzantineMusicalSymbols         = 'Byzantine Musical Symbols';
  sMusicalSymbols                  = 'Musical Symbols';
  sAncientGreekMusicalNotation     = 'Ancient Greek Musical Notation';
  sTaiXuanJingSymbols              = 'Tai Xuan Jing Symbols';
  sCountingRodNumerals             = 'Counting Rod Numerals';
  sMathAlphanumSymbols             = 'Mathematical Alphanumeric Symbols';
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

implementation

end.

