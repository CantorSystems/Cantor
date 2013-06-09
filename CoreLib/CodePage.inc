(*
    Windows native engine code page codes for
    MultiByteToWideChar() and WideCharToMultiByte() functions

    Copyright © 2004 Kenta Yoshimura
    Copyright © 2008-2009 The Unified Environment Laboratory
*)

const
  CP_EBCDIC_US                  = 037;          // IBM EBCDIC - U.S./Canada

  CP_OEM_US                     = 437;          // OEM - United States

  CP_EBCDIC_International       = 500;          // IBM EBCDIC - International

  CP_ASMO708                    = 708;          // Arabic - ASMO 708
  CP_ASMO449                    = 709;          // Arabic - ASMO 449+, BCON V4

  CP_Transparent_Arabic         = 710;          // Arabic - Transparent Arabic
  CP_Transparent_ASMO           = 720;          // Arabic - Transparent ASMO

  CP_OEM_Greek                  = 737;          // OEM - Greek (formerly 437G)
  CP_OEM_Baltic                 = 775;          // OEM - Baltic
  CP_OEM_Latin1                 = 850;          // OEM - Multilingual Latin I
  CP_OEM_Latin2                 = 852;          // OEM - Latin II
  CP_OEM_Cyrillic               = 855;          // OEM - Cyrillic (primarily Russian)
  CP_OEM_Turkish                = 857;          // OEM - Turkish
  CP_OEM_Latin1_Euro            = 858;          // OEM - Multlingual Latin I + Euro symbol
  CP_OEM_Portuguese             = 860;          // OEM - Portuguese
  CP_OEM_Icelandic              = 861;          // OEM - Icelandic
  CP_OEM_Hebrew                 = 862;          // OEM - Hebrew
  CP_OEM_CanadianFrench         = 863;          // OEM - Canadian-French
  CP_OEM_Arabic                 = 864;          // OEM - Arabic
  CP_OEM_Nordic                 = 865;          // OEM - Nordic
  CP_OEM_Russian                = 866;          // OEM - Russian
  CP_OEM_ModernGreek            = 867;          // OEM - ModernGreek

  CP_EBCDIC_Multilingual        = 870;          // IBM EBCDIC - Multilingual/ROECE (Latin-2)

  CP_ANSI_Thai                  = 874;          // ANSI/OEM - Thai (same as 28605, ISO 8859-15)

  CP_EBCDIC_Greek_Modern        = 875;          // IBM EBCDIC - Greek Modern

  CP_ANSI_Japanese              = 932;          // ANSI/OEM - Japanese, Shift-JIS
  CP_ANSI_SimplifiedChinese     = 936;          // ANSI/OEM - Simplified Chinese (PRC, Singapore)
  CP_ANSI_Korean                = 949;          // ANSI/OEM - Korean (Unified Hangeul Code)
  CP_ANSI_TraditionalChinese    = 950;          // ANSI/OEM - Traditional Chinese (Taiwan; Hong Kong SAR, PRC)

  CP_EBCDIC_Turkish_Latin5      = 1026;         // IBM EBCDIC - Turkish (Latin-5)
  CP_EBCDIC_Latin1              = 1047;         // IBM EBCDIC - Latin 1/Open System
  CP_EBCDIC_US_Euro             = 1140;         // IBM EBCDIC - U.S./Canada (037 + Euro symbol)
  CP_EBCDIC_Germany_Euro        = 1141;         // IBM EBCDIC - Germany (20273 + Euro symbol)
  CP_EBCDIC_Denmark_Euro        = 1142;         // IBM EBCDIC - Denmark/Norway (20277 + Euro symbol)
  CP_EBCDIC_Finland_Euro        = 1143;         // IBM EBCDIC - Finland/Sweden (20278 + Euro symbol)
  CP_EBCDIC_Italy_Euro          = 1144;         // IBM EBCDIC - Italy (20280 + Euro symbol)
  CP_EBCDIC_America_Euro        = 1145;         // IBM EBCDIC - Latin America/Spain (20284 + Euro symbol)
  CP_EBCDIC_UK_Euro             = 1146;         // IBM EBCDIC - United Kingdom (20285 + Euro symbol)
  CP_EBCDIC_France_Euro         = 1147;         // IBM EBCDIC - France (20297 + Euro symbol)
  CP_EBCDIC_International_Euro  = 1148;         // IBM EBCDIC - International (500 + Euro symbol)
  CP_EBCDIC_Icelandic_Euro      = 1149;         // IBM EBCDIC - Icelandic (20871 + Euro symbol)

  CP_UCS2_LE                    = 1200;         // Unicode UCS-2 Little-Endian (BMP of ISO 10646)
  CP_UCS2_BE                    = 1201;         // Unicode UCS-2 Big-Endian
  CP_Unicode                    = CP_UCS2_LE;
  CP_UnicodeFFFE                = CP_UCS2_BE;

  CP_ANSI_CentralEuropean       = 1250;         // ANSI - Central European
  CP_ANSI_Cyrillic              = 1251;         // ANSI - Cyrillic
  CP_ANSI_Latin1                = 1252;         // ANSI - Latin I
  CP_ANSI_Greek                 = 1253;         // ANSI - Greek
  CP_ANSI_Turkish               = 1254;         // ANSI - Turkish
  CP_ANSI_Hebrew                = 1255;         // ANSI - Hebrew
  CP_ANSI_Arabic                = 1256;         // ANSI - Arabic
  CP_ANSI_Baltic                = 1257;         // ANSI - Baltic
  CP_ANSI_Vietnamese            = 1258;         // ANSI/OEM - Vietnamese

  CP_Korean                     = 1363;         // Korean (Johab)

  CP_MAC_Roman                  = 10000;        // MAC - Roman
  CP_MAC_Japanese               = 10001;        // MAC - Japanese
  CP_MAC_TraditionalChinese     = 10002;        // MAC - Traditional Chinese (Big5)
  CP_MAC_Korean                 = 10003;        // MAC - Korean
  CP_MAC_Arabic                 = 10004;        // MAC - Arabic
  CP_MAC_Hebrew                 = 10005;        // MAC - Hebrew
  CP_MAC_Greek1                 = 10006;        // MAC - Greek I
  CP_MAC_Cyrillic               = 10007;        // MAC - Cyrillic
  CP_MAC_SimplifiedChinese      = 10008;        // MAC - Simplified Chinese (GB 2312)
  CP_MAC_Romanian               = 10010;        // MAC - Romanian
  CP_MAC_Ukrainian              = 10017;        // MAC - Ukrainian
  CP_MAC_Thai                   = 10021;        // MAC - Thai
  CP_MAC_Latin2                 = 10029;        // MAC - Latin II
  CP_MAC_Icelandic              = 10079;        // MAC - Icelandic
  CP_MAC_Turkish                = 10081;        // MAC - Turkish
  CP_MAC_Croatian               = 10082;        // MAC - Croatian

  CP_UCS4_LE                    = 12000;        // Unicode UCS-4 Little-Endian
  CP_UCS4_BE                    = 12001;        // Unicode UCS-4 Big-Endian

  CP_CNS                        = 20000;        // CNS - Taiwan
  CP_TCA                        = 20001;        // TCA - Taiwan
  CP_Eten                       = 20002;        // Eten - Taiwan
  CP_IBM5550                    = 20003;        // IBM5550 - Taiwan
  CP_TeleText                   = 20004;        // TeleText - Taiwan
  CP_Wang                       = 20005;        // Wang - Taiwan
  CP_IA5_IRV                    = 20105;        // IA5 IRV International Alphabet No. 5 (7-bit)
  CP_IA5_German                 = 20106;        // IA5 German (7-bit)
  CP_IA5_Swedish                = 20107;        // IA5 Swedish (7-bit)
  CP_IA5_Norwegian              = 20108;        // IA5 Norwegian (7-bit)
  CP_US_ASCII                   = 20127;        // US-ASCII (7-bit)
  CP_T_61                       = 20261;        // T.61
  CP_ISO6937                    = 20269;        // ISO 6937 Non-Spacing Accent

  CP_EBCDIC_Germany             = 20237;        // IBM EBCDIC - Germany
  CP_EBCDIC_Denmark             = 20277;        // IBM EBCDIC - Denmark/Norway
  CP_EBCDIC_Finland             = 20278;        // IBM EBCDIC - Finland/Sweden
  CP_EBCDIC_Italy               = 20280;        // IBM EBCDIC - Italy
  CP_EBCDIC_America             = 20284;        // IBM EBCDIC - Latin America/Spain
  CP_EBCDIC_UK                  = 20285;        // IBM EBCDIC - United Kingdom
  CP_EBCDIC_Japanese            = 20290;        // IBM EBCDIC - Japanese Katakana Extended
  CP_EBCDIC_France              = 20297;        // IBM EBCDIC - France
  CP_EBCDIC_Arabic              = 20420;        // IBM EBCDIC - Arabic
  CP_EBCDIC_Greek               = 20423;        // IBM EBCDIC - Greek
  CP_EBCDIC_Hebrew              = 20424;        // IBM EBCDIC - Hebrew
  CP_EBCDIC_Korean              = 20833;        // IBM EBCDIC - Korean Extended
  CP_EBCDIC_Thai                = 20838;        // IBM EBCDIC - Thai

  CP_Russian                    = 20866;        // Russian - KOI8-R
  CP_Ukrainian                  = 21866;        // Ukrainian (KOI8-U)

  CP_EBCDIC_Icelandic           = 20871;        // IBM EBCDIC - Icelandic
  CP_EBCDIC_Cyrillic_Russian    = 20880;        // IBM EBCDIC - Cyrillic (Russian)
  CP_EBCDIC_Turkish             = 20905;        // IBM EBCDIC - Turkish
  CP_EBCDIC_Latin1_Euro         = 20924;        // IBM EBCDIC - Latin-1/Open System (1047 + Euro symbol)

  CP_JISX0208_1990_0121_1990    = 20932;        // JIS X 0208-1990 & 0121-1990
  CP_GB2312                     = 20936;        // Simplified Chinese (GB2312)

  CP_EBCDIC_Cyrillic_Serbian    = 21025;        // IBM EBCDIC - Cyrillic (Serbian, Bulgarian)

  CP_ExtendedAlphaLowercase     = 21027;        // Extended Alpha Lowercase

  CP_ISO8859_1                  = 28591;        // ISO 8859-1 Latin I
  CP_ISO8859_2                  = 28592;        // ISO 8859-2 Central Europe
  CP_ISO8859_3                  = 28593;        // ISO 8859-3 Latin 3
  CP_ISO8859_4                  = 28594;        // ISO 8859-4 Baltic
  CP_ISO8859_5                  = 28595;        // ISO 8859-5 Cyrillic
  CP_ISO8859_6                  = 28596;        // ISO 8859-6 Arabic
  CP_ISO8859_7                  = 28597;        // ISO 8859-7 Greek
  CP_ISO8859_8                  = 28598;        // ISO 8859-8 Hebrew
  CP_ISO8859_9                  = 28599;        // ISO 8859-9 Latin 5
  CP_ISO8859_15                 = 28605;        // ISO 8859-15 Latin 9
  CP_Europa3                    = 29001;        // Europa 3
  CP_ISO8859_8_2                = 38598;        // ISO 8859-8 Hebrew
  CP_ISO2022_JP                 = 50220;        // ISO 2022 Japanese with no halfwidth Katakana
  CP_ISO2022_JP_Kana            = 50221;        // ISO 2022 Japanese with halfwidth Katakana
  CP_ISO2022_JP_0201_1989       = 50222;        // ISO 2022 Japanese JIS X 0201-1989
  CP_ISO2022_KR                 = 50225;        // ISO 2022 Korean
  CP_ISO2022_SC                 = 50227;        // ISO 2022 Simplified Chinese
  CP_ISO2022_TC                 = 50229;        // ISO 2022 Traditional Chinese

  CP_ECMA_114                   = CP_ISO8859_6; // ECMA-114
  CP_ECMA_118                   = CP_ISO8859_7; // ECMA-118
  CP_ELOT_928                   = CP_ISO8859_7; // ELOT_928

  CP_Cyrillic                   = CP_ISO8859_5;
  CP_Greek                      = CP_ISO8859_7;
  CP_Greek8                     = CP_ISO8859_7;

  CP_JapaneseKatakanaExt        = 50930;        // Japanese (Katakana) Extended
  CP_US_Japanese                = 50931;        // US/Canada and Japanese
  CP_KoreanExtended             = 50933;        // Korean Extended and Korean
  CP_SimplifiedChineseExt       = 50935;        // Simplified Chinese Extended and Simplified Chinese
  CP_SimplifiedChinese          = 50936;        // Simplified Chinese
  CP_US_TraditionalChinese      = 50937;        // US/Canada and Traditional Chinese
  CP_JapaneseLatinExt           = 50939;        // Japanese (Latin) Extended and Japanese

  CP_EUC_JP                     = 51932;        // EUC - Japanese
  CP_EUC_SC                     = 51936;        // EUC - Simplified Chinese
  CP_EUC_KR                     = 50949;        // EUC - Korean
  CP_EUC_TC                     = 51950;        // EUC - Traditional Chinese

  CP_HZ_GB2312                  = 52936;        // HZ-GB2312 Simplified Chinese
  CP_GB18030                    = 54936;        // Windows XP: GB18030 Simplified Chinese (4 Byte)

  CP_ISCII_Devanagari           = 57002;        // ISCII Devanagari
  CP_ISCII_Bengali              = 57003;        // ISCII Bengali
  CP_ISCII_Tamil                = 57004;        // ISCII Tamil
  CP_ISCII_Telugu               = 57005;        // ISCII Telugu
  CP_ISCII_Assamese             = 57006;        // ISCII Assamese
  CP_ISCII_Oriya                = 57007;        // ISCII Oriya
  CP_ISCII_Kannada              = 57008;        // ISCII Kannada
  CP_ISCII_Malayalam            = 57009;        // ISCII Malayalam
  CP_ISCII_Gujarati             = 57010;        // ISCII Gujarati
  CP_ISCII_Punjabi              = 57011;        // ISCII Punjabi

  CODE_JPN_JIS                  = 50220;        // JIS
  CODE_JPN_EUC                  = 51932;        // EUC-JP

  CP_ANSI_x3_4_1968             = 1252;         // Western                     // ANSI_x3.4-1968
  CP_ANSI_x3_4_1986             = 1252;         // Western                     // ANSI_x3.4-1986
  CP_ASCII                      = 1252;         // Western
  CP_367                        = 1252;         // Western
  CP_819                        = 1252;         // Western

  CP_Big5                       = 950;          // Traditional Chinese (BIG5)
  CP_Chinese                    = 936;          // Chinese Simplified
  CP_GBK                        = CP_GB2312;

  CP_CSASCII                    = 1252;         // Western
  CP_CSBig5                     = 950;          // Traditional Chinese (BIG5)
  CP_CSEUCKr                    = 949;          // Korean
  CP_CSEUCPkdFmtJapanese        = CODE_JPN_EUC; // Japanese (EUC)
  CP_CSGB2312                   = 936;          // Chinese Simplified (GB2312)
  CP_CSISO2022Jp                = CODE_JPN_JIS; // Japanese (JIS-Allow 1 byte Kana)
  CP_CSISO2022Kr                = 50225;        // Korean (ISO)
  CP_CSISO58GB231280            = 936;          // Chinese Simplified (GB2312)
  CP_CSISOLatin2                = 28592;        // Central European (ISO)
  CP_CSISOLatinHebrew           = 1255;         // Hebrew (ISO-Visual)
  CP_CSKOI8R                    = 20866;        // Cyrillic (KOI8-R)
  CP_CSKSC56011987              = 949;          // Korean
  CP_CSShiftJIS                 = 932;          // Shift-JIS

  CP_GB_2312_80                 = 936;          // Chinese Simplified (GB2312) // GB_2318-80
  CP_Hebrew                     = 1255;         // Hebrew
  CP_Visual                     = CP_Hebrew;
  CP_HZ_GB_2312                 = 936;          // Chinese Simplified (HZ)     // HZ-GB-2312

  CP_IBM367                     = 1252;         // Western
  CP_IBM819                     = 1252;         // Western
  CP_IBM852                     = 852;          // Central European (DOS)
  CP_IBM866                     = 866;          // Cyrillic (DOS)

  CP_ISO_2022_Jp                = CODE_JPN_JIS; // Japanese (JIS)              // ISO-2022-Jp
  CP_ISO_2022_Kr                = 50225;        // Korean (ISO)                // ISO-2022-Kr
  CP_ISO_8859_1                 = 1252;         // Western                     // ISO-8859_1
  CP_ISO_8859_2                 = 28592;        // Central European (ISO)      // ISO-8859_2
  CP_ISO_8859_8                 = 1255;         // Hebrew (ISO-Visual)         // ISO-8859_8
  CP_ISO_IR_100                 = 1252;         // Western                     // ISO-ir-100
  CP_ISO_IR_101                 = 28592;        // Central European (ISO)      // ISO-ir-101
  CP_ISO_IR_138                 = 1255;         // Hebrew (ISO-Visual)         // ISO-ir-138
  CP_ISO_IR_149                 = 949;          // Korean                      // ISO-ir-149
  CP_ISO_IR_58                  = 936;          // Chinese Simplified (GB2312) // ISO-ir-58
  CP_ISO_IR_6                   = 1252;         // Western                     // ISO-ir-6
  CP_ISO646_us                  = 1252;         // Western                     // ISO646-us
  CP_ISO_646_irv_1991           = 1252;         // Western                     // ISO_646.irv:1991
  CP_ISO_8859_1_1987            = 1252;         // Western                     // ISO_8859-1:1987
  CP_ISO_8859_2_1987            = 28592;        // Central European (ISO)      // ISO_8859-2:1987
  CP_ISO_8859_8_1988            = 1255;         // Hebrew (ISO-Visual)         // ISO_8859-8:1988
  CP_KOI8_r                     = 20866;        // Cyrillic (KOI8-R)           // KOI8-R
  CP_KS_C_5601                  = 949;          // Korean                      // KS_C_5601
  CP_KS_C_5601_1987             = 949;          // Korean                      // KS_C_5601-1987
  CP_KS_C_5601_1989             = 949;          // Korean                      // KS_C_5601-1989
  CP_KSC5601                    = 949;          // Korean                      // KSC5601
  CP_KSC_5601                   = 949;          // Korean                      // KSC_5601

  CP_l2                         = 28592;        // Central European (ISO)
  CP_Latin1                     = 1252;         // Western
  CP_Latin2                     = 28592;        // Central European (ISO)
  CP_MS_Kanji                   = 932;          // Shift-JIS
  CP_Shift_JIS                  = 932;          // Shift-JIS
  CP_US                         = 1252;         // Western

  CP_Windows_1250               = 1250;         // Central European (Windows)  // Windows-1250
  CP_Windows_1251               = 1251;         // Cyrillic (Windows)          // Windows-1251
  CP_Windows_1252               = 1252;         // Western                     // Windows-1252
  CP_Windows_1253               = 1253;         // Greek (Windows)             // Windows-1253
  CP_Windows_1254               = 1254;         // Turkish (Windows)           // Windows-1254
  CP_Windows_1255               = 1255;         // Hebrew                      // Windows-1255
  CP_Windows_1256               = 1256;         // Arabic                      // Windows-1256
  CP_Windows_1257               = 1257;         // Baltic (Windows)            // Windows-1257
  CP_Windows_1258               = 1258;         // Vietnamese                  // Windows-1258
  CP_Windows_874                = 874;          // Thai                        // Windows-874

  CP_X_CP1250                   = 1250;         // Central European (Windows)  // X-CP1250
  CP_X_CP1251                   = 1251;         // Cyrillic (Windows)          // X-CP1251
  CP_X_EUC                      = CODE_JPN_EUC; // Japanese (EUC)              // X-EUC
  CP_X_EUC_Jp                   = CODE_JPN_EUC; // Japanese (EUC)              // X-EUC-Jp
  CP_X_SJIS                     = 932;          // Shift-JIS                   // X-SJIS
  CP_X_X_Big5                   = 950;          // Traditional Chinese (BIG5)  // X-X-Big5

  CP_ISO_Latin1                 = CP_ISO8859_1;

  CP_ACP                        = Windows.CP_ACP;
  CP_OEMCP                      = Windows.CP_OEMCP;
  CP_UTF7                       = Windows.CP_UTF7;
  CP_UTF8                       = Windows.CP_UTF8;

  CP_UTF16_LE                   = CP_UCS2_LE;
  CP_UTF16_BE                   = CP_UCS2_BE;

  CP_UTF16                      = CP_UTF16_LE;  // platform-dependent

  CP_UTF32_LE                   = CP_UCS4_LE;
  CP_UTF32_BE                   = CP_UCS4_BE;

  CP_UTF32                      = CP_UTF32_LE;  // platform-dependent


