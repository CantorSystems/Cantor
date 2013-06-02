(*
    Lite Core Library (CoreLite)

    HTML entities definition

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

const
  Chars: packed array[0..3] of TCharCode = (
    (Source: '"'; Code: 'quot'),
    (Source: '&'; Code: 'amp'),
    (Source: '<'; Code: 'lt'),
    (Source: '>'; Code: 'gt')
  );

  ExtChars: packed array[0..247] of TCharCode = (
    (Source: WideChar(160); Code: 'nbsp'),
    (Source: WideChar(161); Code: 'iexcl'),
    (Source: WideChar(162); Code: 'cent'),
    (Source: WideChar(163); Code: 'pound'),
    (Source: WideChar(164); Code: 'curren'),
    (Source: WideChar(165); Code: 'yen'),
    (Source: WideChar(166); Code: 'brvbar'),
    (Source: WideChar(167); Code: 'sect'),
    (Source: WideChar(168); Code: 'uml'),
    (Source: WideChar(169); Code: 'copy'),
    (Source: WideChar(170); Code: 'ordf'),
    (Source: WideChar(171); Code: 'laquo'),
    (Source: WideChar(172); Code: 'not'),
    (Source: WideChar(173); Code: 'shy'),
    (Source: WideChar(174); Code: 'reg'),
    (Source: WideChar(175); Code: 'macr'),
    (Source: WideChar(176); Code: 'deg'),
    (Source: WideChar(177); Code: 'plusmn'),
    (Source: WideChar(178); Code: 'sup2'),
    (Source: WideChar(179); Code: 'sup3'),
    (Source: WideChar(180); Code: 'acute'),
    (Source: WideChar(181); Code: 'micro'),
    (Source: WideChar(182); Code: 'para'),
    (Source: WideChar(183); Code: 'middot'),
    (Source: WideChar(184); Code: 'cedil'),
    (Source: WideChar(185); Code: 'sup1'),
    (Source: WideChar(186); Code: 'ordm'),
    (Source: WideChar(187); Code: 'raquo'),
    (Source: WideChar(188); Code: 'frac14'),
    (Source: WideChar(189); Code: 'frac12'),
    (Source: WideChar(190); Code: 'frac34'),
    (Source: WideChar(191); Code: 'iquest'),
    (Source: WideChar(192); Code: 'Agrave'),
    (Source: WideChar(193); Code: 'Aacute'),
    (Source: WideChar(194); Code: 'Acirc'),
    (Source: WideChar(195); Code: 'Atilde'),
    (Source: WideChar(196); Code: 'Auml'),
    (Source: WideChar(197); Code: 'Aring'),
    (Source: WideChar(198); Code: 'AElig'),
    (Source: WideChar(199); Code: 'Ccedil'),
    (Source: WideChar(200); Code: 'Egrave'),
    (Source: WideChar(201); Code: 'Eacute'),
    (Source: WideChar(202); Code: 'Ecirc'),
    (Source: WideChar(203); Code: 'Euml'),
    (Source: WideChar(204); Code: 'Igrave'),
    (Source: WideChar(205); Code: 'Iacute'),
    (Source: WideChar(206); Code: 'Icirc'),
    (Source: WideChar(207); Code: 'Iuml'),
    (Source: WideChar(208); Code: 'ETH'),
    (Source: WideChar(209); Code: 'Ntilde'),
    (Source: WideChar(210); Code: 'Ograve'),
    (Source: WideChar(211); Code: 'Oacute'),
    (Source: WideChar(212); Code: 'Ocirc'),
    (Source: WideChar(213); Code: 'Otilde'),
    (Source: WideChar(214); Code: 'Ouml'),
    (Source: WideChar(215); Code: 'times'),
    (Source: WideChar(216); Code: 'Oslash'),
    (Source: WideChar(217); Code: 'Ugrave'),
    (Source: WideChar(218); Code: 'Uacute'),
    (Source: WideChar(219); Code: 'Ucirc'),
    (Source: WideChar(220); Code: 'Uuml'),
    (Source: WideChar(221); Code: 'Yacute'),
    (Source: WideChar(222); Code: 'THORN'),
    (Source: WideChar(223); Code: 'szlig'),
    (Source: WideChar(224); Code: 'agrave'),
    (Source: WideChar(225); Code: 'aacute'),
    (Source: WideChar(226); Code: 'acirc'),
    (Source: WideChar(227); Code: 'atilde'),
    (Source: WideChar(228); Code: 'auml'),
    (Source: WideChar(229); Code: 'aring'),
    (Source: WideChar(230); Code: 'aelig'),
    (Source: WideChar(231); Code: 'ccedil'),
    (Source: WideChar(232); Code: 'egrave'),
    (Source: WideChar(233); Code: 'eacute'),
    (Source: WideChar(234); Code: 'ecirc'),
    (Source: WideChar(235); Code: 'euml'),
    (Source: WideChar(236); Code: 'igrave'),
    (Source: WideChar(237); Code: 'iacute'),
    (Source: WideChar(238); Code: 'icirc'),
    (Source: WideChar(239); Code: 'iuml'),
    (Source: WideChar(240); Code: 'eth'),
    (Source: WideChar(241); Code: 'ntilde'),
    (Source: WideChar(242); Code: 'ograve'),
    (Source: WideChar(243); Code: 'oacute'),
    (Source: WideChar(244); Code: 'ocirc'),
    (Source: WideChar(245); Code: 'otilde'),
    (Source: WideChar(246); Code: 'ouml'),
    (Source: WideChar(247); Code: 'divide'),
    (Source: WideChar(248); Code: 'oslash'),
    (Source: WideChar(249); Code: 'ugrave'),
    (Source: WideChar(250); Code: 'uacute'),
    (Source: WideChar(251); Code: 'ucirc'),
    (Source: WideChar(252); Code: 'uuml'),
    (Source: WideChar(253); Code: 'yacute'),
    (Source: WideChar(254); Code: 'thorn'),
    (Source: WideChar(255); Code: 'yuml'),

  // Latin Extended-A                      
    (Source: WideChar(338); Code: 'OElig'),
    (Source: WideChar(339); Code: 'oelig'),

  // ligature is a misnomer, this is a separate character in some languages
    (Source: WideChar(352); Code: 'Scaron'),
    (Source: WideChar(353); Code: 'scaron'),
    (Source: WideChar(376); Code: 'Yuml'),

  // Latin Extended-B
    (Source: WideChar(402); Code: 'fnof'),     // Latin small f with hook, =function, =florin, U0192 ISOtech

  //Spacing Modifier Letters
    (Source: WideChar(710); Code: 'circ'),
    (Source: WideChar(732); Code: 'tilde'),

  // Greek
    (Source: WideChar(913); Code: 'Alpha'),    // Greek capital letter alpha, U0391
    (Source: WideChar(914); Code: 'Beta'),     // Greek capital letter beta, U0392
    (Source: WideChar(915); Code: 'Gamma'),    // Greek capital letter gamma, U0393 ISOgrk3
    (Source: WideChar(916); Code: 'Delta'),    // Greek capital letter delta, U0394 ISOgrk3
    (Source: WideChar(917); Code: 'Epsilon'),  // Greek capital letter epsilon, U0395
    (Source: WideChar(918); Code: 'Zeta'),     // Greek capital letter zeta, U0396
    (Source: WideChar(919); Code: 'Eta'),      // Greek capital letter eta, U0397
    (Source: WideChar(920); Code: 'Theta'),    // Greek capital letter theta, U0398 ISOgrk3
    (Source: WideChar(921); Code: 'Iota'),     // Greek capital letter iota, U0399
    (Source: WideChar(922); Code: 'Kappa'),    // Greek capital letter kappa, U039A
    (Source: WideChar(923); Code: 'Lambda'),   // Greek capital letter lambda, U039B ISOgrk3
    (Source: WideChar(924); Code: 'Mu'),       // Greek capital letter mu, U039C
    (Source: WideChar(925); Code: 'Nu'),       // Greek capital letter nu, U039D
    (Source: WideChar(926); Code: 'Xi'),       // Greek capital letter xi, U039E ISOgrk3
    (Source: WideChar(927); Code: 'Omicron'),  // Greek capital letter omicron, U039F
    (Source: WideChar(928); Code: 'Pi'),       // Greek capital letter pi, U03A0 ISOgrk3
    (Source: WideChar(929); Code: 'Rho'),      // Greek capital letter rho, U03A1
    (Source: WideChar(931); Code: 'Sigma'),    // Greek capital letter sigma, U03A3 ISOgrk3
    (Source: WideChar(932); Code: 'Tau'),      // Greek capital letter tau, U03A4
    (Source: WideChar(933); Code: 'Upsilon'),  // Greek capital letter upsilon, U03A5 ISOgrk3
    (Source: WideChar(934); Code: 'Phi'),      // Greek capital letter phi, U03A6 ISOgrk3
    (Source: WideChar(935); Code: 'Chi'),      // Greek capital letter chi, U03A7
    (Source: WideChar(936); Code: 'Psi'),      // Greek capital letter psi, U03A8 ISOgrk3
    (Source: WideChar(937); Code: 'Omega'),    // Greek capital letter omega, U03A9 ISOgrk3
    (Source: WideChar(945); Code: 'alpha'),    // Greek small letter alpha, U03B1 ISOgrk3
    (Source: WideChar(946); Code: 'beta'),     // Greek small letter beta, U03B2 ISOgrk3
    (Source: WideChar(947); Code: 'gamma'),    // Greek small letter gamma, U03B3 ISOgrk3
    (Source: WideChar(948); Code: 'delta'),    // Greek small letter delta, U03B4 ISOgrk3
    (Source: WideChar(949); Code: 'epsilon'),  // Greek small letter epsilon, U03B5 ISOgrk3
    (Source: WideChar(950); Code: 'zeta'),     // Greek small letter zeta, U03B6 ISOgrk3
    (Source: WideChar(951); Code: 'eta'),      // Greek small letter eta, U03B7 ISOgrk3
    (Source: WideChar(952); Code: 'theta'),    // Greek small letter theta, U03B8 ISOgrk3
    (Source: WideChar(953); Code: 'iota'),     // Greek small letter iota, U03B9 ISOgrk3
    (Source: WideChar(954); Code: 'kappa'),    // Greek small letter kappa, U03BA ISOgrk3
    (Source: WideChar(955); Code: 'lambda'),   // Greek small letter lambda, U03BB ISOgrk3
    (Source: WideChar(956); Code: 'mu'),       // Greek small letter mu, U03BC ISOgrk3
    (Source: WideChar(957); Code: 'nu'),       // Greek small letter nu, U03BD ISOgrk3
    (Source: WideChar(958); Code: 'xi'),       // Greek small letter xi, U03BE ISOgrk3
    (Source: WideChar(959); Code: 'omicron'),  // Greek small letter omicron, U03BF NEW
    (Source: WideChar(960); Code: 'pi'),       // Greek small letter pi, U03C0 ISOgrk3
    (Source: WideChar(961); Code: 'rho'),      // Greek small letter rho, U03C1 ISOgrk3
    (Source: WideChar(962); Code: 'sigmaf'),   // Greek small letter final sigma, U03C2 ISOgrk3
    (Source: WideChar(963); Code: 'sigma'),    // Greek small letter sigma, U03C3 ISOgrk3
    (Source: WideChar(964); Code: 'tau'),      // Greek small letter tau, U03C4 ISOgrk3
    (Source: WideChar(965); Code: 'upsilon'),  // Greek small letter upsilon, U03C5 ISOgrk3
    (Source: WideChar(966); Code: 'phi'),      // Greek small letter phi, U03C6 ISOgrk3
    (Source: WideChar(967); Code: 'chi'),      // Greek small letter chi, U03C7 ISOgrk3
    (Source: WideChar(968); Code: 'psi'),      // Greek small letter psi, U03C8 ISOgrk3
    (Source: WideChar(969); Code: 'omega'),    // Greek small letter omega, U03C9 ISOgrk3
    (Source: WideChar(977); Code: 'thetasym'), // Greek small letter theta symbol, U03D1 NEW
    (Source: WideChar(978); Code: 'upsih'),    // Greek upsilon with hook symbol, U03D2 NEW
    (Source: WideChar(982); Code: 'piv'),      // Greek pi symbol, U03D6 ISOgrk3

  // General Punctuation
    (Source: WideChar(8194); Code: 'ensp'),
    (Source: WideChar(8195); Code: 'emsp'),
    (Source: WideChar(8201); Code: 'thinsp'),
    (Source: WideChar(8204); Code: 'zwnj'),
    (Source: WideChar(8205); Code: 'zwj'),
    (Source: WideChar(8206); Code: 'lrm'),
    (Source: WideChar(8207); Code: 'rlm'),
    (Source: WideChar(8211); Code: 'ndash'),
    (Source: WideChar(8212); Code: 'mdash'),
    (Source: WideChar(8216); Code: 'lsquo'),
    (Source: WideChar(8217); Code: 'rsquo'),
    (Source: WideChar(8218); Code: 'sbquo'),
    (Source: WideChar(8220); Code: 'ldquo'),
    (Source: WideChar(8221); Code: 'rdquo'),
    (Source: WideChar(8222); Code: 'bdquo'),
    (Source: WideChar(8224); Code: 'dagger'),
    (Source: WideChar(8225); Code: 'Dagger'),
    (Source: WideChar(8226); Code: 'bull'),    // bullet, =black small circle, U2022 ISOpub
    (Source: WideChar(8230); Code: 'hellip'),  // horizontal ellipsis, =three dot leader, U2026 ISOpub
    (Source: WideChar(8240); Code: 'permil'),
    (Source: WideChar(8242); Code: 'prime'),   // prime, =minutes, =feet, U2032 ISOtech
    (Source: WideChar(8243); Code: 'Prime'),   // double prime, =seconds, =inches, U2033 ISOtech
    (Source: WideChar(8249); Code: 'lsaquo'),  // lsaquo is proposed but not yet ISO standardized
    (Source: WideChar(8250); Code: 'rsaquo'),  // rsaquo is proposed but not yet ISO standardized
    (Source: WideChar(8254); Code: 'oline'),   // overline, =spacing overscore, U203E NEW
    (Source: WideChar(8260); Code: 'frasl'),   // fraction slash, U2044 NEW
    (Source: WideChar(8364); Code: 'euro'),

  // Letterlike Symbols
    (Source: WideChar(8472); Code: 'weierp'),  // script capital P, =power set, =Weierstrass p, U2118 ISOamso
    (Source: WideChar(8465); Code: 'image'),   // blackletter capital I, =imaginary part, U2111 ISOamso
    (Source: WideChar(8476); Code: 'real'),    // blackletter capital R, =real part symbol, U211C ISOamso
    (Source: WideChar(8482); Code: 'trade'),   // trade mark sign, U2122 ISOnum (#153?)
    (Source: WideChar(8501); Code: 'alefsym'), // alef symbol, =first transfinite cardinal, U2135 NEW

  // Arrows
    (Source: WideChar(8592); Code: 'larr'),    // leftward arrow, U2190 ISOnum
    (Source: WideChar(8593); Code: 'uarr'),    // upward arrow, U2191 ISOnum
    (Source: WideChar(8594); Code: 'rarr'),    // rightward arrow, U2192 ISOnum
    (Source: WideChar(8595); Code: 'darr'),    // downward arrow, U2193 ISOnum
    (Source: WideChar(8596); Code: 'harr'),    // left right arrow, U2194 ISOamsa
    (Source: WideChar(8629); Code: 'crarr'),   // downward arrow with corner leftward, =carriage return, U21B5 NEW
    (Source: WideChar(8656); Code: 'lArr'),    // leftward double arrow, U21D0 ISOtech
    (Source: WideChar(8657); Code: 'uArr'),    // upward double arrow, U21D1 ISOamsa
    (Source: WideChar(8658); Code: 'rArr'),    // rightward double arrow, U21D2 ISOtech
    (Source: WideChar(8659); Code: 'dArr'),    // downward double arrow, U21D3 ISOamsa
    (Source: WideChar(8660); Code: 'hArr'),    // left right double arrow, U21D4 ISOamsa

  // Mathematical Operators
    (Source: WideChar(8704); Code: 'forall'),  // for all, U2200 ISOtech
    (Source: WideChar(8706); Code: 'part'),    // partial differential, U2202 ISOtech
    (Source: WideChar(8707); Code: 'exist'),   // there exists, U2203 ISOtech
    (Source: WideChar(8709); Code: 'empty'),   // empty set, =null set, =diameter, U2205 ISOamso
    (Source: WideChar(8711); Code: 'nabla'),   // nabla, =backward difference, U2207 ISOtech
    (Source: WideChar(8712); Code: 'isin'),    // element of, U2208 ISOtech
    (Source: WideChar(8713); Code: 'notin'),   // not an element of, U2209 ISOtech
    (Source: WideChar(8715); Code: 'ni'),      // contains as member, U220B ISOtech
    (Source: WideChar(8719); Code: 'prod'),    // n-ary product, =product sign, U220F ISOamsb
    (Source: WideChar(8722); Code: 'sum'),     // n-ary sumation, U2211 ISOamsb
    (Source: WideChar(8722); Code: 'minus'),   // minus sign, U2212 ISOtech
    (Source: WideChar(8727); Code: 'lowast'),  // asterisk operator, U2217 ISOtech
    (Source: WideChar(8730); Code: 'radic'),   // square root, =radical sign, U221A ISOtech
    (Source: WideChar(8733); Code: 'prop'),    // proportional to, U221D ISOtech
    (Source: WideChar(8734); Code: 'infin'),   // infinity, U221E ISOtech
    (Source: WideChar(8736); Code: 'ang'),     // angle, U2220 ISOamso
    (Source: WideChar(8869); Code: 'and'),     // logical and, =wedge, U2227 ISOtech
    (Source: WideChar(8870); Code: 'or'),      // logical or, =vee, U2228 ISOtech
    (Source: WideChar(8745); Code: 'cap'),     // intersection, =cap, U2229 ISOtech
    (Source: WideChar(8746); Code: 'cup'),     // union, =cup, U222A ISOtech
    (Source: WideChar(8747); Code: 'int'),     // integral, U222B ISOtech
    (Source: WideChar(8756); Code: 'there4'),  // therefore, U2234 ISOtech
    (Source: WideChar(8764); Code: 'sim'),     // tilde operator, =varies with, =similar to, U223C ISOtech
    (Source: WideChar(8773); Code: 'cong'),    // approximately equal to, U2245 ISOtech
    (Source: WideChar(8773); Code: 'asymp'),   // almost equal to, =asymptotic to, U2248 ISOamsr
    (Source: WideChar(8800); Code: 'ne'),      // not equal to, U2260 ISOtech
    (Source: WideChar(8801); Code: 'equiv'),   // identical to, U2261 ISOtech
    (Source: WideChar(8804); Code: 'le'),      // less-than or equal to, U2264 ISOtech
    (Source: WideChar(8805); Code: 'ge'),      // greater-than or equal to, U2265 ISOtech
    (Source: WideChar(8834); Code: 'sub'),     // subset of, U2282 ISOtech
    (Source: WideChar(8835); Code: 'sup'),     // superset of, U2283 ISOtech
    (Source: WideChar(8836); Code: 'nsub'),    // not a subset of, U2284 ISOamsn
    (Source: WideChar(8838); Code: 'sube'),    // subset of or equal to, U2286 ISOtech
    (Source: WideChar(8839); Code: 'supe'),    // superset of or equal to, U2287 ISOtech
    (Source: WideChar(8853); Code: 'oplus'),   // circled plus, =direct sum, U2295 ISOamsb
    (Source: WideChar(8855); Code: 'otimes'),  // circled times, =vector product, U2297 ISOamsb
    (Source: WideChar(8869); Code: 'perp'),    // up tack, =orthogonal to, =perpendicular, U22A5 ISOtech
    (Source: WideChar(8901); Code: 'sdot'),    // dot operator, U22C5 ISOamsb

  // Miscellaneous Technical
    (Source: WideChar(8968); Code: 'lceil'),   // left ceiling, =apl upstile, U2308, ISOamsc
    (Source: WideChar(8969); Code: 'rceil'),   // right ceiling, U2309, ISOamsc
    (Source: WideChar(8970); Code: 'lfloor'),  // left floor, =apl downstile, U230A, ISOamsc
    (Source: WideChar(8971); Code: 'rfloor'),  // right floor, U230B, ISOamsc
    (Source: WideChar(9001); Code: 'lang'),    // left-pointing angle bracket, =bra, U2329 ISOtech
    (Source: WideChar(9002); Code: 'rang'),    // right-pointing angle bracket, =ket, U232A ISOtech

  // Geometric Shapes
    (Source: WideChar(9674); Code: 'loz'),     // lozenge, U25CA ISOpub

  // Miscellaneous Symbols
    (Source: WideChar(9824); Code: 'spades'),  // black spade suit, U2660 ISOpub
    (Source: WideChar(9827); Code: 'clubs'),   // black club suit, =shamrock, U2663 ISOpub
    (Source: WideChar(9829); Code: 'hearts'),  // black heart suit, =valentine, U2665 ISOpub
    (Source: WideChar(9830); Code: 'diams')    // black diamond suit, U2666 ISOpub
  );
