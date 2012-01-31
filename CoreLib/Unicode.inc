(*
    Unicode 5.2 code blocks

    Copyright (c) 2009-2012 The Unified Environment Laboratory

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

  BOM_UTF16_BE        = $FFFE;
  BOM_UTF16_LE        = $FEFF;
  BOM_UTF32_BE        = $FFFE0000;
  BOM_UTF32_LE        = $0000FEFF;

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
    cbUnknown, cbBasicLatin, cbLatin1Supplement, cbLatinExtA, cbLatinExtB,
    cbIPAExtensions, cbSpacingModLetters, cbCombiDiacriticalMarks,
    cbGreekAndCoptic, cbCyrillic, cbCyrillicSupplement, cbArmenian, cbHebrew,
    cbArabic, cbSyriac, cbArabicSupplement, cbThaana, cbNKo, cbSamaritan,
    cbDevanagari, cbBengali, cbGurmukhi, cbGujarati, cbOriya, cbTamil, cbTelugu,
    cbKannada, cbMalayalam, cbSinhala, cbThai, cbLao, cbTibetan, cbMyanmar,
    cbGeorgian, cbHangulJamo, cbEthiopic, cbEthiopicSupplement, cbCherokee,
    cbUniCanadianSyllabics, cbOgham, cbRunic, cbTagalog, cbHanunoo, cbBuhid,
    cbTagbanwa, cbKhmer, cbMongolian, cbUniCanadianSyllabicsExt, cbLimbu,
    cbTaiLe, cbNewTaiLue, cbKhmerSymbols, cbBuginese, cbTaiTham, cbBalinese,
    cbSundanese, cbLepcha, cbOlChiki, cbVedicExtensions, cbPhoneticExt,
    cbPhoneticExtSupplement, cbCombiDiacriticalMarksSupplement,
    cbLatinExtAdditional, cbGreekExt, cbGeneralPunctuation,
    cbSuperscriptsAndSubscripts, cbCurrencySymbols,
    cbCombiDiacriticalMarksForSymbols, cbLetterlikeSymbols, cbNumForms,
    cbArrows, cbMathOperators, cbMiscTechnical, cbControlPictures,
    cbOpticalCharRecognition, cbEnclosedAlphanums, cbBoxDrawing,
    cbBlockElements, cbGeometricShapes, cbMiscSymbols, cbDingbats,
    cbMiscMathSymbolsA, cbSupplementalArrowsA, cbBraillePatterns,
    cbSupplementalArrowsB, cbMiscMathSymbolsB, cbSupplementalMathOperators,
    cbMiscSymbolsAndArrows, cbGlagolitic, cbLatinExtC, cbCoptic,
    cbGeorgianSupplement, cbTifinagh, cbEthiopicExt, cbCyrillicExtA,
    cbSupplementalPunctuation, cbCJKRadicalsSupplement, cbKangxiRadicals,
    cbIdeographicDescChars, cbCJKSymbolsAndPunctuation, cbHiragana, cbKatakana,
    cbBopomofo, cbHangulCompatJamo, cbKanbun, cbBopomofoExt, cbCJKStrokes,
    cbKatakanaPhoneticExt, cbEnclosedCJKLettersAndMonths, cbCJKCompat,
    cbCJKUniIdeographsExtA, cbYijingHexagramSymbols, cbCJKUniIdeographs,
    cbYiSyllables, cbYiRadicals, cbLisu, cbVai, cbCyrillicExtB, cbBamum,
    cbModToneLetters, cbLatinExtD, cbSylotiNagri, cbCommonIndicNumForms,
    cbPhagsPa, cbSaurashtra, cbDevanagariExt, cbKayahLi, cbRejang,
    cbHangulJamoExtA, cbJavanese, cbCham, cbMyanmarExtA, cbTaiViet,
    cbMeeteiMayek, cbHangulSyllables, cbHangulJamoExtB, cbHighSurrogates,
    cbHighPrivateSurrogates, cbLowSurrogates, cbPrivateArea,
    cbCJKCompatIdeographs, cbAlphabeticPresentForms, cbArabicPresentFormsA,
    cbVarSelectors, cbVerticalForms, cbCombiHalfMarks, cbCJKCompatForms,
    cbSmallFormVariants, cbArabicPresentFormsB, cbHalfwidthAndFullwidthForms,
    cbSpecials, cbLinearBSyllabary, cbLinearBIdeograms, cbAegeanNums,
    cbAncientGreekNums, cbAncientSymbols, cbPhaistosDisc, cbLycian, cbCarian,
    cbOldItalic, cbGothic, cbUgaritic, cbOldPersian, cbDeseret, cbShavian,
    cbOsmanya, cbCypriotSyllabary, cbImperialAramaic, cbPhoenician, cbLydian,
    cbKharoshthi, cbOldSouthArabian, cbAvestan,ubInscriptionalParthian,
    cbInscriptionalPahlavi, cbOldTurkic, cbRumiNumeralSymbols, cbKaithi,
    cbCuneiform, cbCuneiformNumsAndPunctuation, cbEgyptianHieroglyphs,
    cbByzantineMusicalSymbols, cbMusicalSymbols, cbAncientGreekMusicalNotation,
    cbTaiXuanJingSymbols, cbCountingRodNumerals, cbMathAlphanumSymbols,
    cbMahjongTiles, cbDominoTiles, cbEnclosedAlphanumSupplement,
    cbEnclosedIdeographicSupplement, cbCJKUniIdeographsExtB,
    cbCJKUniIdeographsExtC, cbCJKCompatIdeographsSupplement, cbTags,
    cbVarSelectorsSupplement, cbPrivateAreaA, cbPrivateAreaB, cbNonUnicode
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
     Min, Max: QuadChar;
  end;
  TUnicodeBlockRanges = array[TUnicodeBlock] of TCharRange;

const
  TUnicodeBMPBlocks = [Low(TUnicodeBMPBlockRange)..High(TUnicodeBMPBlockRange)];
  TUnicodeSMPBlocks = [Low(TUnicodeSMPBlockRange)..High(TUnicodeSMPBlockRange)];
  TUnicodeSIPBlocks = [Low(TUnicodeSIPBlockRange)..High(TUnicodeSIPBlockRange)];
  TUnicodeSSPBlocks = [Low(TUnicodeSSPBlockRange)..High(TUnicodeSSPBlockRange)];
  TUnicodePUABlocks = [Low(TUnicodePUABlockRange)..High(TUnicodePUABlockRange)];

  UnicodeBlockRanges: TUnicodeBlockRanges = ( // sorted!
    (Min: $0000; Max: $007F), // Basic Latin
    (Min: $0080; Max: $00FF), // Latin-1 Supplement
    (Min: $0100; Max: $017F), // Latin Extended-A
    (Min: $0180; Max: $024F), // Latin Extended-B
    (Min: $0250; Max: $02AF), // IPA Extensions
    (Min: $02B0; Max: $02FF), // Spacing Modifier Letters
    (Min: $0300; Max: $036F), // Combining Diacritical Marks
    (Min: $0370; Max: $03FF), // Greek and Coptic
    (Min: $0400; Max: $04FF), // Cyrillic
    (Min: $0500; Max: $052F), // Cyrillic Supplement
    (Min: $0530; Max: $058F), // Armenian
    (Min: $0590; Max: $05FF), // Hebrew
    (Min: $0600; Max: $06FF), // Arabic
    (Min: $0700; Max: $074F), // Syriac
    (Min: $0750; Max: $077F), // Arabic Supplement
    (Min: $0780; Max: $07BF), // Thaana
    (Min: $07C0; Max: $07FF), // NKo
    (Min: $0800; Max: $083F), // Samaritan
    (Min: $0900; Max: $097F), // Devanagari
    (Min: $0980; Max: $09FF), // Bengali
    (Min: $0A00; Max: $0A7F), // Gurmukhi
    (Min: $0A80; Max: $0AFF), // Gujarati
    (Min: $0B00; Max: $0B7F), // Oriya
    (Min: $0B80; Max: $0BFF), // Tamil
    (Min: $0C00; Max: $0C7F), // Telugu
    (Min: $0C80; Max: $0CFF), // Kannada
    (Min: $0D00; Max: $0D7F), // Malayalam
    (Min: $0D80; Max: $0DFF), // Sinhala
    (Min: $0E00; Max: $0E7F), // Thai
    (Min: $0E80; Max: $0EFF), // Lao
    (Min: $0F00; Max: $0FFF), // Tibetan
    (Min: $1000; Max: $109F), // Myanmar
    (Min: $10A0; Max: $10FF), // Georgian
    (Min: $1100; Max: $11FF), // Hangul Jamo
    (Min: $1200; Max: $137F), // Ethiopic
    (Min: $1380; Max: $139F), // Ethiopic Supplement
    (Min: $13A0; Max: $13FF), // Cherokee
    (Min: $1400; Max: $167F), // Unified Canadian Aboriginal Syllabics
    (Min: $1680; Max: $169F), // Ogham
    (Min: $16A0; Max: $16FF), // Runic
    (Min: $1700; Max: $171F), // Tagalog
    (Min: $1720; Max: $173F), // Hanunoo
    (Min: $1740; Max: $175F), // Buhid
    (Min: $1760; Max: $177F), // Tagbanwa
    (Min: $1780; Max: $17FF), // Khmer
    (Min: $1800; Max: $18AF), // Mongolian
    (Min: $18B0; Max: $18FF), // Unified Canadian Aboriginal Syllabics Extended
    (Min: $1900; Max: $194F), // Limbu
    (Min: $1950; Max: $197F), // Tai Le
    (Min: $1980; Max: $19DF), // New Tai Lue
    (Min: $19E0; Max: $19FF), // Khmer Symbols
    (Min: $1A00; Max: $1A1F), // Buginese
    (Min: $1A20; Max: $1AAF), // Tai Tham
    (Min: $1B00; Max: $1B7F), // Balinese
    (Min: $1B80; Max: $1BBF), // Sundanese
    (Min: $1C00; Max: $1C4F), // Lepcha
    (Min: $1C50; Max: $1C7F), // Ol Chiki
    (Min: $1CD0; Max: $1CFF), // Vedic Extensions
    (Min: $1D00; Max: $1D7F), // Phonetic Extensions                                  
    (Min: $1D80; Max: $1DBF), // Phonetic Extensions Supplement                       
    (Min: $1DC0; Max: $1DFF), // Combining Diacritical Marks Supplement               
    (Min: $1E00; Max: $1EFF), // Latin Extended Additional                            
    (Min: $1F00; Max: $1FFF), // Greek Extended                                       
    (Min: $2000; Max: $206F), // General Punctuation                                  
    (Min: $2070; Max: $209F), // Superscripts and Subscripts                          
    (Min: $20A0; Max: $20CF), // Currency Symbols                                     
    (Min: $20D0; Max: $20FF), // Combining Diacritical Marks for Symbols              
    (Min: $2100; Max: $214F), // Letterlike Symbols                                   
    (Min: $2150; Max: $218F), // Number Forms                                         
    (Min: $2190; Max: $21FF), // Arrows                                               
    (Min: $2200; Max: $22FF), // Mathematical Operators                               
    (Min: $2300; Max: $23FF), // Miscellaneous Technical                              
    (Min: $2400; Max: $243F), // Control Pictures
    (Min: $2440; Max: $245F), // Optical Character Recognition                        
    (Min: $2460; Max: $24FF), // Enclosed Alphanumerics                               
    (Min: $2500; Max: $257F), // Box Drawing                                          
    (Min: $2580; Max: $259F), // Block Elements                                       
    (Min: $25A0; Max: $25FF), // Geometric Shapes                                     
    (Min: $2600; Max: $26FF), // Miscellaneous Symbols                                
    (Min: $2700; Max: $27BF), // Dingbats                                             
    (Min: $27C0; Max: $27EF), // Miscellaneous Mathematical Symbols-A                 
    (Min: $27F0; Max: $27FF), // Supplemental Arrows-A                                
    (Min: $2800; Max: $28FF), // Braille Patterns                                     
    (Min: $2900; Max: $297F), // Supplemental Arrows-B                                
    (Min: $2980; Max: $29FF), // Miscellaneous Mathematical Symbols-B                 
    (Min: $2A00; Max: $2AFF), // Supplemental Mathematical Operators                  
    (Min: $2B00; Max: $2BFF), // Miscellaneous Symbols and Arrows                     
    (Min: $2C00; Max: $2C5F), // Glagolitic                                           
    (Min: $2C60; Max: $2C7F), // Latin Extended-C                                     
    (Min: $2C80; Max: $2CFF), // Coptic
    (Min: $2D00; Max: $2D2F), // Georgian Supplement                                  
    (Min: $2D30; Max: $2D7F), // Tifinagh                                             
    (Min: $2D80; Max: $2DDF), // Ethiopic Extended                                    
    (Min: $2DE0; Max: $2DFF), // Cyrillic Extended-A                                  
    (Min: $2E00; Max: $2E7F), // Supplemental Punctuation                             
    (Min: $2E80; Max: $2EFF), // CJK Radicals Supplement                              
    (Min: $2F00; Max: $2FDF), // Kangxi Radicals                                      
    (Min: $2FF0; Max: $2FFF), // Ideographic Description Characters                   
    (Min: $3000; Max: $303F), // CJK Symbols and Punctuation                          
    (Min: $3040; Max: $309F), // Hiragana                                             
    (Min: $30A0; Max: $30FF), // Katakana                                             
    (Min: $3100; Max: $312F), // Bopomofo                                             
    (Min: $3130; Max: $318F), // Hangul Compatibility Jamo
    (Min: $3190; Max: $319F), // Kanbun                                               
    (Min: $31A0; Max: $31BF), // Bopomofo Extended                                    
    (Min: $31C0; Max: $31EF), // CJK Strokes                                          
    (Min: $31F0; Max: $31FF), // Katakana Phonetic Extensions                         
    (Min: $3200; Max: $32FF), // Enclosed CJK Letters and Months                      
    (Min: $3300; Max: $33FF), // CJK Compatibility                                    
    (Min: $3400; Max: $4DBF), // CJK Unified Ideographs Extension A                   
    (Min: $4DC0; Max: $4DFF), // Yijing Hexagram Symbols                              
    (Min: $4E00; Max: $9FFF), // CJK Unified Ideographs                               
    (Min: $A000; Max: $A48F), // Yi Syllables                                         
    (Min: $A490; Max: $A4CF), // Yi Radicals                                          
    (Min: $A4D0; Max: $A4FF), // Lisu
    (Min: $A500; Max: $A63F), // Vai                                                  
    (Min: $A640; Max: $A69F), // Cyrillic Extended-B                                  
    (Min: $A6A0; Max: $A6FF), // Bamum                                                
    (Min: $A700; Max: $A71F), // Modifier Tone Letters                                
    (Min: $A720; Max: $A7FF), // Latin Extended-D                                     
    (Min: $A800; Max: $A82F), // Syloti Nagri                                         
    (Min: $A830; Max: $A83F), // Common Indic Number Forms                            
    (Min: $A840; Max: $A87F), // Phags-pa                                             
    (Min: $A880; Max: $A8DF), // Saurashtra                                           
    (Min: $A8E0; Max: $A8FF), // Devanagari Extended                                  
    (Min: $A900; Max: $A92F), // Kayah Li                                             
    (Min: $A930; Max: $A95F), // Rejang                                               
    (Min: $A960; Max: $A97F), // Hangul Jamo Extended-A                               
    (Min: $A980; Max: $A9DF), // Javanese                                             
    (Min: $AA00; Max: $AA5F), // Cham                                                 
    (Min: $AA60; Max: $AA7F), // Myanmar Extended-A                                   
    (Min: $AA80; Max: $AADF), // Tai Viet                                             
    (Min: $ABC0; Max: $ABFF), // Meetei Mayek                                         
    (Min: $AC00; Max: $D7AF), // Hangul Syllables                                     
    (Min: $D7B0; Max: $D7FF), // Hangul Jamo Extended-B
    (Min: $D800; Max: $DB7F), // High Surrogates                                      
    (Min: $DB80; Max: $DBFF), // High Private Use Surrogates
    (Min: $DC00; Max: $DFFF), // Low Surrogates                                       
    (Min: $E000; Max: $F8FF), // Private Use Area                                     
    (Min: $F900; Max: $FAFF), // CJK Compatibility Ideographs                         
    (Min: $FB00; Max: $FB4F), // Alphabetic Presentation Forms                        
    (Min: $FB50; Max: $FDFF), // Arabic Presentation Forms-A                          
    (Min: $FE00; Max: $FE0F), // Variation Selectors                                  
    (Min: $FE10; Max: $FE1F), // Vertical Forms                                       
    (Min: $FE20; Max: $FE2F), // Combining Half Marks                                 
    (Min: $FE30; Max: $FE4F), // CJK Compatibility Forms                              
    (Min: $FE50; Max: $FE6F), // Small Form Variants                                  
    (Min: $FE70; Max: $FEFF), // Arabic Presentation Forms-B
    (Min: $FF00; Max: $FFEF), // Halfwidth and Fullwidth Forms                        
    (Min: $FFF0; Max: $FFFF), // Specials
    (Min: $10000; Max: $1007F), // Linear B Syllabary
    (Min: $10080; Max: $100FF), // Linear B Ideograms                                 
    (Min: $10100; Max: $1013F), // Aegean Numbers                                     
    (Min: $10140; Max: $1018F), // Ancient Greek Numbers                              
    (Min: $10190; Max: $101CF), // Ancient Symbols                                    
    (Min: $101D0; Max: $101FF), // Phaistos Disc                                      
    (Min: $10280; Max: $1029F), // Lycian
    (Min: $102A0; Max: $102DF), // Carian                                             
    (Min: $10300; Max: $1032F), // Old Italic                                         
    (Min: $10330; Max: $1034F), // Gothic                                             
    (Min: $10380; Max: $1039F), // Ugaritic                                           
    (Min: $103A0; Max: $103DF), // Old Persian                                        
    (Min: $10400; Max: $1044F), // Deseret                                            
    (Min: $10450; Max: $1047F), // Shavian                                            
    (Min: $10480; Max: $104AF), // Osmanya                                            
    (Min: $10800; Max: $1083F), // Cypriot Syllabary                                  
    (Min: $10840; Max: $1085F), // Imperial Aramaic                                   
    (Min: $10900; Max: $1091F), // Phoenician                                         
    (Min: $10920; Max: $1093F), // Lydian                                             
    (Min: $10A00; Max: $10A5F), // Kharoshthi                                         
    (Min: $10A60; Max: $10A7F), // Old South Arabian                                  
    (Min: $10B00; Max: $10B3F), // Avestan
    (Min: $10B40; Max: $10B5F), // Inscriptional Parthian
    (Min: $10B60; Max: $10B7F), // Inscriptional Pahlavi
    (Min: $10C00; Max: $10C4F), // Old Turkic
    (Min: $10E60; Max: $10E7F), // Rumi Numeral Symbols
    (Min: $11080; Max: $110CF), // Kaithi
    (Min: $12000; Max: $123FF), // Cuneiform
    (Min: $12400; Max: $1247F), // Cuneiform Numbers and Punctuation
    (Min: $13000; Max: $1342F), // Egyptian Hieroglyphs
    (Min: $1D000; Max: $1D0FF), // Byzantine Musical Symbols
    (Min: $1D100; Max: $1D1FF), // Musical Symbols
    (Min: $1D200; Max: $1D24F), // Ancient Greek Musical Notation
    (Min: $1D300; Max: $1D35F), // Tai Xuan Jing Symbols
    (Min: $1D360; Max: $1D37F), // Counting Rod Numerals
    (Min: $1D400; Max: $1D7FF), // Mathematical Alphanumeric Symbols
    (Min: $1F000; Max: $1F02F), // Mahjong Tiles
    (Min: $1F030; Max: $1F09F), // Domino Tiles
    (Min: $1F100; Max: $1F1FF), // Enclosed Alphanumeric Supplement
    (Min: $1F200; Max: $1F2FF), // Enclosed Ideographic Supplement
    (Min: $20000; Max: $2A6DF), // CJK Unified Ideographs Extension B
    (Min: $2A700; Max: $2B73F), // CJK Unified Ideographs Extension C
    (Min: $2F800; Max: $2FA1F), // CJK Compatibility Ideographs Supplement
    (Min: $E0000; Max: $E007F), // Tags
    (Min: $E0100; Max: $E01EF), // Variation Selectors Supplement
    (Min: $F0000; Max: $FFFFF), // Supplementary Private Use Area-A
    (Min: $100000; Max: $10FFFF) // Supplementary Private Use Area-B
  );

