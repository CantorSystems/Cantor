(*
    Lite Core Library (CoreLite)

    Unicode 6.1 code blocks

    Copyright © 2009-2013 Vladislav Javadov (Freeman)

    Used abbreviations:
      * Aboriginal -> (removed)
      * Alphanumeric -> Alphanum
      * Character -> Char
      * Combining -> Combi
      * Compatibility -> Compat
      * Description -> Desc
      * Extended -> Ext
      * Extension -> Ext
      * ticExtensions -> ticExt
      * Mathematical -> Math
      * Miscellaneous -> Misc
      * Modifier -> Mod
      * Number(s) -> Num(s)
      * Presentation -> Present
      * Supplementary -> (removed)
      * Variation -> Var
      * Unified -> Uni
      * Use -> (removed)
*)

type
  PSurrogates = ^TSurrogates;
  TSurrogates = packed record
    Hi, Lo: WideChar; // fixed word order, always big-endian
  end;

  PUTF16Char = ^UTF16Char;
  UTF16Char = packed record
    case Word of
      0: (BasicChar: WideChar);
      1: (Surrogates: TSurrogates);
  end;

const
  BOM_UTF7            = $762F2B;      // byte sequence is platform-dependent
  BOM_UTF7_LastBytes  = [$38, $39, $2B, $2F];

  BOM_UTF8            = $BFBBEF;      // byte sequence is platform-dependent

  BOM_UTF16           = $FEFF;        // platform-dependent

  BOM_UTF16_BE        = WideChar($FFFE);
  BOM_UTF16_LE        = WideChar($FEFF);

  BOM_UTF32_BE        = QuadChar($FFFE0000);
  BOM_UTF32_LE        = QuadChar($0000FEFF);

  Unknown_Latin       = #$1A;

  Unknown_UTF7        = '+//0-';
  Unknown_UTF7_Bytes  = Cardinal(Length(Unknown_UTF7));
  UnknownUTF7: array[0..Unknown_UTF7_Bytes - 1] of LegacyChar = Unknown_UTF7;

  Unknown_UTF8        = $BDBFEF;  // byte sequence is platform-dependent
  Unknown_UTF8_Bytes  = 3;
  UnknownUTF8: array[0..Unknown_UTF8_Bytes - 1] of Byte = ($EF, $BF, $BD);

  Unknown_UTF16_LE    = WideChar($FFFD);
  Unknown_UTF16_BE    = WideChar($FDFF);

  Unknown_UTF16       = Unknown_UTF16_LE;  // platform-dependent

  UnknownUTF16: array[Boolean] of WideChar = (Unknown_UTF16_LE, Unknown_UTF16_BE);

  Unknown_UTF32_LE    = QuadChar($0000FFFD);
  Unknown_UTF32_BE    = QuadChar($FDFF0000);

  Unknown_UTF32       = Unknown_UTF32_LE;  // platform-dependent

  UnknownUTF32: array[Boolean] of QuadChar = (Unknown_UTF32_LE, Unknown_UTF32_BE);

  UTF7_MaxCharBytes   = 7;
  UTF8_MaxCharBytes   = 4;
  UTF16_MaxCharBytes  = SizeOf(UTF16Char);
  UTF32_MaxCharBytes  = SizeOf(QuadChar);

type
  TCharBlock = (
    cbNonUnicode,
    cbBasicLatin,
    cbLatin1Supplement,
    cbLatinExtA,
    cbLatinExtB,
    cbIPAExtensions,
    cbSpacingModLetters,
    cbCombiDiacriticalMarks,
    cbGreekAndCoptic,
    cbCyrillic,
    cbCyrillicSupplement,
    cbArmenian,
    cbHebrew,
    cbArabic,
    cbSyriac,
    cbArabicSupplement,
    cbThaana,
    cbNKo,
    cbSamaritan,
    cbMandaic,
    cbArabicExtA,
    cbDevanagari,
    cbBengali,
    cbGurmukhi,
    cbGujarati,
    cbOriya,
    cbTamil,
    cbTelugu,
    cbKannada,
    cbMalayalam,
    cbSinhala,
    cbThai,
    cbLao,
    cbTibetan,
    cbMyanmar,
    cbGeorgian,
    cbHangulJamo,
    cbEthiopic,
    cbEthiopicSupplement,
    cbCherokee,
    cbUniCanadianSyllabics,
    cbOgham,
    cbRunic,
    cbTagalog,
    cbHanunoo,
    cbBuhid,
    cbTagbanwa,
    cbKhmer,
    cbMongolian,
    cbUniCanadianSyllabicsExt,
    cbLimbu,
    cbTaiLe,
    cbNewTaiLue,
    cbKhmerSymbols,
    cbBuginese,
    cbTaiTham,
    cbBalinese,
    cbSundanese,
    cbBatak,
    cbLepcha,
    cbOlChiki,
    cbSundaneseSupplement,
    cbVedicExtensions,
    cbPhoneticExt,
    cbPhoneticExtSupplement,
    cbCombiDiacriticalMarksSupplement,
    cbLatinExtAdditional,
    cbGreekExt,
    cbGeneralPunctuation,
    cbSuperscriptsAndSubscripts,
    cbCurrencySymbols,
    cbCombiDiacriticalMarksForSymbols,
    cbLetterlikeSymbols,
    cbNumForms,
    cbArrows,
    cbMathOperators,
    cbMiscTechnical,
    cbControlPictures,
    cbOpticalCharRecognition,
    cbEnclosedAlphanums,
    cbBoxDrawing,
    cbBlockElements,
    cbGeometricShapes,
    cbMiscSymbols,
    cbDingbats,
    cbMiscMathSymbolsA,
    cbSupplementalArrowsA,
    cbBraillePatterns,
    cbSupplementalArrowsB,
    cbMiscMathSymbolsB,
    cbSupplementalMathOperators,
    cbMiscSymbolsAndArrows,
    cbGlagolitic,
    cbLatinExtC,
    cbCoptic,
    cbGeorgianSupplement,
    cbTifinagh,
    cbEthiopicExt,
    cbCyrillicExtA,
    cbSupplementalPunctuation,
    cbCJKRadicalsSupplement,
    cbKangxiRadicals,
    cbIdeographicDescChars,
    cbCJKSymbolsAndPunctuation,
    cbHiragana,
    cbKatakana,
    cbBopomofo,
    cbHangulCompatJamo,
    cbKanbun,
    cbBopomofoExt,
    cbCJKStrokes,
    cbKatakanaPhoneticExt,
    cbEnclosedCJKLettersAndMonths,
    cbCJKCompat,
    cbCJKUniIdeographsExtA,
    cbYijingHexagramSymbols,
    cbCJKUniIdeographs,
    cbYiSyllables,
    cbYiRadicals,
    cbLisu,
    cbVai,
    cbCyrillicExtB,
    cbBamum,
    cbModToneLetters,
    cbLatinExtD,
    cbSylotiNagri,
    cbCommonIndicNumForms,
    cbPhagsPa,
    cbSaurashtra,
    cbDevanagariExt,
    cbKayahLi,
    cbRejang,
    cbHangulJamoExtA,
    cbJavanese,
    cbCham,
    cbMyanmarExtA,
    cbTaiViet,
    cbMeeteiMayekExt,
    cbEthiopicExtA,
    cbMeeteiMayek,
    cbHangulSyllables,
    cbHangulJamoExtB,
    cbHighSurrogates,
    cbHighPrivateSurrogates,
    cbLowSurrogates,
    cbPrivateArea,
    cbCJKCompatIdeographs,
    cbAlphabeticPresentForms,
    cbArabicPresentFormsA,
    cbVarSelectors,
    cbVerticalForms,
    cbCombiHalfMarks,
    cbCJKCompatForms,
    cbSmallFormVariants,
    cbArabicPresentFormsB,
    cbHalfwidthAndFullwidthForms,
    cbSpecials,
    cbLinearBSyllabary,
    cbLinearBIdeograms,
    cbAegeanNums,
    cbAncientGreekNums,
    cbAncientSymbols,
    cbPhaistosDisc,
    cbLycian,
    cbCarian,
    cbOldItalic,
    cbGothic,
    cbUgaritic,
    cbOldPersian,
    cbDeseret,
    cbShavian,
    cbOsmanya,
    cbCypriotSyllabary,
    cbImperialAramaic,
    cbPhoenician,
    cbLydian,
    cbMeroiticHieroglyphs,
    cbMeroiticCursive,
    cbKharoshthi,
    cbOldSouthArabian,
    cbAvestan,
    cbInscriptionalParthian,
    cbInscriptionalPahlavi,
    cbOldTurkic,
    cbRumiNumeralSymbols,
    cbBrahmi,
    cbKaithi,
    cbSoraSompeng,
    cbChakma,
    cbSharada,
    cbTakri,
    cbCuneiform,
    cbCuneiformNumsAndPunctuation,
    cbEgyptianHieroglyphs,
    cbBamumSupplement,
    cbMiao,
    cbKanaSupplement,
    cbByzantineMusicalSymbols,
    cbMusicalSymbols,
    cbAncientGreekMusicalNotation,
    cbTaiXuanJingSymbols,
    cbCountingRodNumerals,
    cbMathAlphanumSymbols,
    cbArabicMathAlphabeticSymbols,
    cbMahjongTiles,
    cbDominoTiles,
    cbPlayingCards,
    cbEnclosedAlphanumSupplement,
    cbEnclosedIdeographicSupplement,
    cbMiscSymbolsAndPictographs,
    cbEmoticons,
    cbTransportAndMapSymbols,
    cbAlchemicalSymbols,
    cbCJKUniIdeographsExtB,
    cbCJKUniIdeographsExtC,
    cbCJKUniIdeographsExtD,
    cbCJKCompatIdeographsSupplement,
    cbTags,
    cbVarSelectorsSupplement,
    cbPrivateAreaA,
    cbPrivateAreaB
  );

  TCharBlocks = set of TCharBlock;

  TUnicodeBlock = cbBasicLatin..cbPrivateAreaB;
  TUnicodeBlocks = set of TUnicodeBlock;

  THighSurrogates = $D800..$DBFF;
  TLowSurrogates  = $DC00..$DFFF;

  TCharPlanes = (chBMP, chSMP, chSIP, chSSP, chPUA, chNonUnicode);
  TUnicodePlanes = chBMP..chPUA;

  TUnicodeBMP = $000000..$00FFFF;  // Basic Multilingual Plane
  TUnicodeSMP = $010000..$01FFFF;  // Supplementary Multilingual Plane
  TUnicodeSIP = $020000..$02FFFF;  // Supplementary Ideographic Plane
  TUnicodeSSP = $0E0000..$0EFFFF;  // Supplementary Special-purpose Plane
  TUnicodePUA = $0F0000..$10FFFF;  // Private Use Area

  TNonUnicode = $110000..$FFFFFFFF;

  TUnicodeBMPBlockRange = cbBasicLatin..cbSpecials;
  TUnicodeSMPBlockRange = cbLinearBSyllabary..cbEnclosedIdeographicSupplement;
  TUnicodeSIPBlockRange = cbCJKUniIdeographsExtB..cbCJKCompatIdeographsSupplement;
  TUnicodeSSPBlockRange = cbTags..cbVarSelectorsSupplement;
  TUnicodePUABlockRange = cbPrivateAreaA..cbPrivateAreaB;

  TCharRange = packed record
     Lo, Hi: QuadChar;
  end;

const
  UnicodeBlocks = [Low(TUnicodeBlock)..High(TUnicodeBlock)];

  UnicodeBMPBlocks = [Low(TUnicodeBMPBlockRange)..High(TUnicodeBMPBlockRange)];
  UnicodeSMPBlocks = [Low(TUnicodeSMPBlockRange)..High(TUnicodeSMPBlockRange)];
  UnicodeSIPBlocks = [Low(TUnicodeSIPBlockRange)..High(TUnicodeSIPBlockRange)];
  UnicodeSSPBlocks = [Low(TUnicodeSSPBlockRange)..High(TUnicodeSSPBlockRange)];
  UnicodePUABlocks = [Low(TUnicodePUABlockRange)..High(TUnicodePUABlockRange)];

  UnicodeBlockRanges: array[TUnicodeBlock] of TCharRange  = ( // sorted!
    (Lo: $0000; Hi: $007F), // Basic Latin
    (Lo: $0080; Hi: $00FF), // Latin-1 Supplement
    (Lo: $0100; Hi: $017F), // Latin Extended-A
    (Lo: $0180; Hi: $024F), // Latin Extended-B
    (Lo: $0250; Hi: $02AF), // IPA Extensions
    (Lo: $02B0; Hi: $02FF), // Spacing Modifier Letters
    (Lo: $0300; Hi: $036F), // Combining Diacritical Marks
    (Lo: $0370; Hi: $03FF), // Greek and Coptic
    (Lo: $0400; Hi: $04FF), // Cyrillic
    (Lo: $0500; Hi: $052F), // Cyrillic Supplement
    (Lo: $0530; Hi: $058F), // Armenian
    (Lo: $0590; Hi: $05FF), // Hebrew
    (Lo: $0600; Hi: $06FF), // Arabic
    (Lo: $0700; Hi: $074F), // Syriac
    (Lo: $0750; Hi: $077F), // Arabic Supplement
    (Lo: $0780; Hi: $07BF), // Thaana
    (Lo: $07C0; Hi: $07FF), // N’Ko
    (Lo: $0800; Hi: $083F), // Samaritan
    (Lo: $0840; Hi: $085F), // Mandaic
    (Lo: $08A0; Hi: $08FF), // Arabic Extended-A
    (Lo: $0900; Hi: $097F), // Devanagari
    (Lo: $0980; Hi: $09FF), // Bengali
    (Lo: $0A00; Hi: $0A7F), // Gurmukhi
    (Lo: $0A80; Hi: $0AFF), // Gujarati
    (Lo: $0B00; Hi: $0B7F), // Oriya
    (Lo: $0B80; Hi: $0BFF), // Tamil
    (Lo: $0C00; Hi: $0C7F), // Telugu
    (Lo: $0C80; Hi: $0CFF), // Kannada
    (Lo: $0D00; Hi: $0D7F), // Malayalam
    (Lo: $0D80; Hi: $0DFF), // Sinhala
    (Lo: $0E00; Hi: $0E7F), // Thai
    (Lo: $0E80; Hi: $0EFF), // Lao
    (Lo: $0F00; Hi: $0FFF), // Tibetan
    (Lo: $1000; Hi: $109F), // Myanmar
    (Lo: $10A0; Hi: $10FF), // Georgian
    (Lo: $1100; Hi: $11FF), // Hangul Jamo
    (Lo: $1200; Hi: $137F), // Ethiopic
    (Lo: $1380; Hi: $139F), // Ethiopic Supplement
    (Lo: $13A0; Hi: $13FF), // Cherokee
    (Lo: $1400; Hi: $167F), // Unified Canadian Aboriginal Syllabics
    (Lo: $1680; Hi: $169F), // Ogham
    (Lo: $16A0; Hi: $16FF), // Runic
    (Lo: $1700; Hi: $171F), // Tagalog
    (Lo: $1720; Hi: $173F), // Hanunoo
    (Lo: $1740; Hi: $175F), // Buhid
    (Lo: $1760; Hi: $177F), // Tagbanwa
    (Lo: $1780; Hi: $17FF), // Khmer
    (Lo: $1800; Hi: $18AF), // Mongolian
    (Lo: $18B0; Hi: $18FF), // Unified Canadian Aboriginal Syllabics Extended
    (Lo: $1900; Hi: $194F), // Limbu
    (Lo: $1950; Hi: $197F), // Tai Le
    (Lo: $1980; Hi: $19DF), // New Tai Lue
    (Lo: $19E0; Hi: $19FF), // Khmer Symbols
    (Lo: $1A00; Hi: $1A1F), // Buginese
    (Lo: $1A20; Hi: $1AAF), // Tai Tham
    (Lo: $1B00; Hi: $1B7F), // Balinese
    (Lo: $1B80; Hi: $1BBF), // Sundanese
    (Lo: $1BC0; Hi: $1BFF), // Batak
    (Lo: $1C00; Hi: $1C4F), // Lepcha
    (Lo: $1C50; Hi: $1C7F), // Ol Chiki
    (Lo: $1CC0; Hi: $1CCF), // Sundanese Supplement
    (Lo: $1CD0; Hi: $1CFF), // Vedic Extensions
    (Lo: $1D00; Hi: $1D7F), // Phonetic Extensions
    (Lo: $1D80; Hi: $1DBF), // Phonetic Extensions Supplement
    (Lo: $1DC0; Hi: $1DFF), // Combining Diacritical Marks Supplement
    (Lo: $1E00; Hi: $1EFF), // Latin Extended Additional
    (Lo: $1F00; Hi: $1FFF), // Greek Extended
    (Lo: $2000; Hi: $206F), // General Punctuation
    (Lo: $2070; Hi: $209F), // Superscripts and Subscripts
    (Lo: $20A0; Hi: $20CF), // Currency Symbols
    (Lo: $20D0; Hi: $20FF), // Combining Diacritical Marks for Symbols
    (Lo: $2100; Hi: $214F), // Letterlike Symbols
    (Lo: $2150; Hi: $218F), // Number Forms
    (Lo: $2190; Hi: $21FF), // Arrows
    (Lo: $2200; Hi: $22FF), // Mathematical Operators
    (Lo: $2300; Hi: $23FF), // Miscellaneous Technical
    (Lo: $2400; Hi: $243F), // Control Pictures
    (Lo: $2440; Hi: $245F), // Optical Character Recognition
    (Lo: $2460; Hi: $24FF), // Enclosed Alphanumerics
    (Lo: $2500; Hi: $257F), // Box Drawing
    (Lo: $2580; Hi: $259F), // Block Elements
    (Lo: $25A0; Hi: $25FF), // Geometric Shapes
    (Lo: $2600; Hi: $26FF), // Miscellaneous Symbols
    (Lo: $2700; Hi: $27BF), // Dingbats
    (Lo: $27C0; Hi: $27EF), // Miscellaneous Mathematical Symbols-A
    (Lo: $27F0; Hi: $27FF), // Supplemental Arrows-A
    (Lo: $2800; Hi: $28FF), // Braille Patterns
    (Lo: $2900; Hi: $297F), // Supplemental Arrows-B
    (Lo: $2980; Hi: $29FF), // Miscellaneous Mathematical Symbols-B
    (Lo: $2A00; Hi: $2AFF), // Supplemental Mathematical Operators
    (Lo: $2B00; Hi: $2BFF), // Miscellaneous Symbols and Arrows
    (Lo: $2C00; Hi: $2C5F), // Glagolitic
    (Lo: $2C60; Hi: $2C7F), // Latin Extended-C
    (Lo: $2C80; Hi: $2CFF), // Coptic
    (Lo: $2D00; Hi: $2D2F), // Georgian Supplement
    (Lo: $2D30; Hi: $2D7F), // Tifinagh
    (Lo: $2D80; Hi: $2DDF), // Ethiopic Extended
    (Lo: $2DE0; Hi: $2DFF), // Cyrillic Extended-A
    (Lo: $2E00; Hi: $2E7F), // Supplemental Punctuation
    (Lo: $2E80; Hi: $2EFF), // CJK Radicals Supplement
    (Lo: $2F00; Hi: $2FDF), // Kangxi Radicals
    (Lo: $2FF0; Hi: $2FFF), // Ideographic Description Characters
    (Lo: $3000; Hi: $303F), // CJK Symbols and Punctuation
    (Lo: $3040; Hi: $309F), // Hiragana
    (Lo: $30A0; Hi: $30FF), // Katakana
    (Lo: $3100; Hi: $312F), // Bopomofo
    (Lo: $3130; Hi: $318F), // Hangul Compatibility Jamo
    (Lo: $3190; Hi: $319F), // Kanbun
    (Lo: $31A0; Hi: $31BF), // Bopomofo Extended
    (Lo: $31C0; Hi: $31EF), // CJK Strokes
    (Lo: $31F0; Hi: $31FF), // Katakana Phonetic Extensions
    (Lo: $3200; Hi: $32FF), // Enclosed CJK Letters and Months
    (Lo: $3300; Hi: $33FF), // CJK Compatibility
    (Lo: $3400; Hi: $4DBF), // CJK Unified Ideographs Extension A
    (Lo: $4DC0; Hi: $4DFF), // Yijing Hexagram Symbols
    (Lo: $4E00; Hi: $9FFF), // CJK Unified Ideographs
    (Lo: $A000; Hi: $A48F), // Yi Syllables
    (Lo: $A490; Hi: $A4CF), // Yi Radicals
    (Lo: $A4D0; Hi: $A4FF), // Lisu
    (Lo: $A500; Hi: $A63F), // Vai
    (Lo: $A640; Hi: $A69F), // Cyrillic Extended-B
    (Lo: $A6A0; Hi: $A6FF), // Bamum
    (Lo: $A700; Hi: $A71F), // Modifier Tone Letters
    (Lo: $A720; Hi: $A7FF), // Latin Extended-D
    (Lo: $A800; Hi: $A82F), // Syloti Nagri
    (Lo: $A830; Hi: $A83F), // Common Indic Number Forms
    (Lo: $A840; Hi: $A87F), // Phags-pa
    (Lo: $A880; Hi: $A8DF), // Saurashtra
    (Lo: $A8E0; Hi: $A8FF), // Devanagari Extended
    (Lo: $A900; Hi: $A92F), // Kayah Li
    (Lo: $A930; Hi: $A95F), // Rejang
    (Lo: $A960; Hi: $A97F), // Hangul Jamo Extended-A
    (Lo: $A980; Hi: $A9DF), // Javanese
    (Lo: $AA00; Hi: $AA5F), // Cham
    (Lo: $AA60; Hi: $AA7F), // Myanmar Extended-A
    (Lo: $AA80; Hi: $AADF), // Tai Viet
    (Lo: $AAE0; Hi: $AAFF), // Meetei Mayek Extensions
    (Lo: $AB00; Hi: $AB2F), // Ethiopic Extended-A
    (Lo: $ABC0; Hi: $ABFF), // Meetei Mayek
    (Lo: $AC00; Hi: $D7AF), // Hangul Syllables
    (Lo: $D7B0; Hi: $D7FF), // Hangul Jamo Extended-B
    (Lo: $D800; Hi: $DB7F), // High Surrogates
    (Lo: $DB80; Hi: $DBFF), // High Private Use Surrogates
    (Lo: $DC00; Hi: $DFFF), // Low Surrogates
    (Lo: $E000; Hi: $F8FF), // Private Use Area
    (Lo: $F900; Hi: $FAFF), // CJK Compatibility Ideographs
    (Lo: $FB00; Hi: $FB4F), // Alphabetic Presentation Forms
    (Lo: $FB50; Hi: $FDFF), // Arabic Presentation Forms-A
    (Lo: $FE00; Hi: $FE0F), // Variation Selectors
    (Lo: $FE10; Hi: $FE1F), // Vertical Forms
    (Lo: $FE20; Hi: $FE2F), // Combining Half Marks
    (Lo: $FE30; Hi: $FE4F), // CJK Compatibility Forms
    (Lo: $FE50; Hi: $FE6F), // Small Form Variants
    (Lo: $FE70; Hi: $FEFF), // Arabic Presentation Forms-B
    (Lo: $FF00; Hi: $FFEF), // Halfwidth and Fullwidth Forms
    (Lo: $FFF0; Hi: $FFFF), // Specials
    (Lo: $10000; Hi: $1007F), // Linear B Syllabary
    (Lo: $10080; Hi: $100FF), // Linear B Ideograms
    (Lo: $10100; Hi: $1013F), // Aegean Numbers
    (Lo: $10140; Hi: $1018F), // Ancient Greek Numbers
    (Lo: $10190; Hi: $101CF), // Ancient Symbols
    (Lo: $101D0; Hi: $101FF), // Phaistos Disc
    (Lo: $10280; Hi: $1029F), // Lycian
    (Lo: $102A0; Hi: $102DF), // Carian
    (Lo: $10300; Hi: $1032F), // Old Italic
    (Lo: $10330; Hi: $1034F), // Gothic
    (Lo: $10380; Hi: $1039F), // Ugaritic
    (Lo: $103A0; Hi: $103DF), // Old Persian
    (Lo: $10400; Hi: $1044F), // Deseret
    (Lo: $10450; Hi: $1047F), // Shavian
    (Lo: $10480; Hi: $104AF), // Osmanya
    (Lo: $10800; Hi: $1083F), // Cypriot Syllabary
    (Lo: $10840; Hi: $1085F), // Imperial Aramaic
    (Lo: $10900; Hi: $1091F), // Phoenician
    (Lo: $10920; Hi: $1093F), // Lydian
    (Lo: $10980; Hi: $1099F), // Meroitic Hieroglyphs
    (Lo: $109A0; Hi: $109FF), // Meroitic Cursive
    (Lo: $10A00; Hi: $10A5F), // Kharoshthi
    (Lo: $10A60; Hi: $10A7F), // Old South Arabian
    (Lo: $10B00; Hi: $10B3F), // Avestan
    (Lo: $10B40; Hi: $10B5F), // Inscriptional Parthian
    (Lo: $10B60; Hi: $10B7F), // Inscriptional Pahlavi
    (Lo: $10C00; Hi: $10C4F), // Old Turkic
    (Lo: $10E60; Hi: $10E7F), // Rumi Numeral Symbols
    (Lo: $11000; Hi: $1107F), // Brahmi
    (Lo: $11080; Hi: $110CF), // Kaithi
    (Lo: $110D0; Hi: $110FF), // Sora Sompeng
    (Lo: $11100; Hi: $1114F), // Chakma
    (Lo: $11180; Hi: $111DF), // Sharada
    (Lo: $11680; Hi: $116CF), // Takri
    (Lo: $12000; Hi: $123FF), // Cuneiform
    (Lo: $12400; Hi: $1247F), // Cuneiform Numbers and Punctuation
    (Lo: $13000; Hi: $1342F), // Egyptian Hieroglyphs
    (Lo: $16800; Hi: $16A3F), // Bamum Supplement
    (Lo: $16F00; Hi: $16F9F), // Miao
    (Lo: $1B000; Hi: $1B0FF), // Kana Supplement
    (Lo: $1D000; Hi: $1D0FF), // Byzantine Musical Symbols
    (Lo: $1D100; Hi: $1D1FF), // Musical Symbols
    (Lo: $1D200; Hi: $1D24F), // Ancient Greek Musical Notation
    (Lo: $1D300; Hi: $1D35F), // Tai Xuan Jing Symbols
    (Lo: $1D360; Hi: $1D37F), // Counting Rod Numerals
    (Lo: $1D400; Hi: $1D7FF), // Mathematical Alphanumeric Symbols
    (Lo: $1EE00; Hi: $1EEFF), // Arabic Mathematical Alphabetic Symbols
    (Lo: $1F000; Hi: $1F02F), // Mahjong Tiles
    (Lo: $1F030; Hi: $1F09F), // Domino Tiles
    (Lo: $1F0A0; Hi: $1F0FF), // Playing Cards
    (Lo: $1F100; Hi: $1F1FF), // Enclosed Alphanumeric Supplement
    (Lo: $1F200; Hi: $1F2FF), // Enclosed Ideographic Supplement
    (Lo: $1F300; Hi: $1F5FF), // Miscellaneous Symbols And Pictographs
    (Lo: $1F600; Hi: $1F64F), // Emoticons
    (Lo: $1F680; Hi: $1F6FF), // Transport And Map Symbols
    (Lo: $1F700; Hi: $1F77F), // Alchemical Symbols
    (Lo: $20000; Hi: $2A6DF), // CJK Unified Ideographs Extension B
    (Lo: $2A700; Hi: $2B73F), // CJK Unified Ideographs Extension C
    (Lo: $2B740; Hi: $2B81F), // CJK Unified Ideographs Extension D
    (Lo: $2F800; Hi: $2FA1F), // CJK Compatibility Ideographs Supplement
    (Lo: $E0000; Hi: $E007F), // Tags
    (Lo: $E0100; Hi: $E01EF), // Variation Selectors Supplement
    (Lo: $F0000; Hi: $FFFFF),  // Supplementary Private Use Area-A
    (Lo: $100000; Hi: $10FFFF) // Supplementary Private Use Area-B
  );

  UnicodeBlockNames: array[TUnicodeBlock] of PLegacyChar = (
    sBasicLatin,
    sLatin1Supplement,
    sLatinExtA,
    sLatinExtB,
    sIPAExtensions,
    sSpacingModLetters,
    sCombiDiacriticalMarks,
    sGreekAndCoptic,
    sCyrillic,
    sCyrillicSupplement,
    sArmenian,
    sHebrew,
    sArabic,
    sSyriac,
    sArabicSupplement,
    sThaana,
    sNKo,
    sSamaritan,
    sMandaic,
    sArabicExtA,
    sDevanagari,
    sBengali,
    sGurmukhi,
    sGujarati,
    sOriya,
    sTamil,
    sTelugu,
    sKannada,
    sMalayalam,
    sSinhala,
    sThai,
    sLao,
    sTibetan,
    sMyanmar,
    sGeorgian,
    sHangulJamo,
    sEthiopic,
    sEthiopicSupplement,
    sCherokee,
    sUniCanadianSyllabics,
    sOgham,
    sRunic,
    sTagalog,
    sHanunoo,
    sBuhid,
    sTagbanwa,
    sKhmer,
    sMongolian,
    sUniCanadianSyllabicsExt,
    sLimbu,
    sTaiLe,
    sNewTaiLue,
    sKhmerSymbols,
    sBuginese,
    sTaiTham,
    sBalinese,
    sSundanese,
    sBatak,
    sLepcha,
    sOlChiki,
    sSundaneseSupplement,
    sVedicExtensions,
    sPhoneticExt,
    sPhoneticExtSupplement,
    sCombiDiacriticalMarksSupplement,
    sLatinExtAdditional,
    sGreekExt,
    sGeneralPunctuation,
    sSuperscriptsAndSubscripts,
    sCurrencySymbols,
    sCombiDiacriticalMarksForSymbols,
    sLetterlikeSymbols,
    sNumForms,
    sArrows,
    sMathOperators,
    sMiscTechnical,
    sControlPictures,
    sOpticalCharRecognition,
    sEnclosedAlphanums,
    sBoxDrawing,
    sBlockElements,
    sGeometricShapes,
    sMiscSymbols,
    sDingbats,
    sMiscMathSymbolsA,
    sSupplementalArrowsA,
    sBraillePatterns,
    sSupplementalArrowsB,
    sMiscMathSymbolsB,
    sSupplementalMathOperators,
    sMiscSymbolsAndArrows,
    sGlagolitic,
    sLatinExtC,
    sCoptic,
    sGeorgianSupplement,
    sTifinagh,
    sEthiopicExt,
    sCyrillicExtA,
    sSupplementalPunctuation,
    sCJKRadicalsSupplement,
    sKangxiRadicals,
    sIdeographicDescChars,
    sCJKSymbolsAndPunctuation,
    sHiragana,
    sKatakana,
    sBopomofo,
    sHangulCompatJamo,
    sKanbun,
    sBopomofoExt,
    sCJKStrokes,
    sKatakanaPhoneticExt,
    sEnclosedCJKLettersAndMonths,
    sCJKCompat,
    sCJKUniIdeographsExtA,
    sYijingHexagramSymbols,
    sCJKUniIdeographs,
    sYiSyllables,
    sYiRadicals,
    sLisu,
    sVai,
    sCyrillicExtB,
    sBamum,
    sModToneLetters,
    sLatinExtD,
    sSylotiNagri,
    sCommonIndicNumForms,
    sPhagsPa,
    sSaurashtra,
    sDevanagariExt,
    sKayahLi,
    sRejang,
    sHangulJamoExtA,
    sJavanese,
    sCham,
    sMyanmarExtA,
    sTaiViet,
    sMeeteiMayekExt,
    sEthiopicExtA,
    sMeeteiMayek,
    sHangulSyllables,
    sHangulJamoExtB,
    sHighSurrogates,
    sHighPrivateSurrogates,
    sLowSurrogates,
    sPrivateArea,
    sCJKCompatIdeographs,
    sAlphabeticPresentForms,
    sArabicPresentFormsA,
    sVarSelectors,
    sVerticalForms,
    sCombiHalfMarks,
    sCJKCompatForms,
    sSmallFormVariants,
    sArabicPresentFormsB,
    sHalfwidthAndFullwidthForms,
    sSpecials,
    sLinearBSyllabary,
    sLinearBIdeograms,
    sAegeanNums,
    sAncientGreekNums,
    sAncientSymbols,
    sPhaistosDisc,
    sLycian,
    sCarian,
    sOldItalic,
    sGothic,
    sUgaritic,
    sOldPersian,
    sDeseret,
    sShavian,
    sOsmanya,
    sCypriotSyllabary,
    sImperialAramaic,
    sPhoenician,
    sLydian,
    sMeroiticHieroglyphs,
    sMeroiticCursive,
    sKharoshthi,
    sOldSouthArabian,
    sAvestan,
    sInscriptionalParthian,
    sInscriptionalPahlavi,
    sOldTurkic,
    sRumiNumeralSymbols,
    sBrahmi,
    sKaithi,
    sSoraSompeng,
    sChakma,
    sSharada,
    sTakri,
    sCuneiform,
    sCuneiformNumsAndPunctuation,
    sEgyptianHieroglyphs,
    sBamumSupplement,
    sMiao,
    sKanaSupplement,
    sByzantineMusicalSymbols,
    sMusicalSymbols,
    sAncientGreekMusicalNotation,
    sTaiXuanJingSymbols,
    sCountingRodNumerals,
    sMathAlphanumSymbols,
    sArabicMathAlphabeticSymbols,
    sMahjongTiles,
    sDominoTiles,
    sPlayingCards,
    sEnclosedAlphanumSupplement,
    sEnclosedIdeographicSupplement,
    sMiscSymbolsAndPictographs,
    sEmoticons,
    sTransportAndMapSymbols,
    sAlchemicalSymbols,
    sCJKUniIdeographsExtB,
    sCJKUniIdeographsExtC,
    sCJKUniIdeographsExtD,
    sCJKCompatIdeographsSupplement,
    sTags,
    sVarSelectorsSupplement,
    sPrivateAreaA,
    sPrivateAreaB
  );

