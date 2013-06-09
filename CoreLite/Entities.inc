(*
    Lite Core Library (CoreLite)

    HTML entities definition

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

const
  SafeEntities: packed array[0..3] of TEntity = (
    (Code: '"'; Value: 'quot'),
    (Code: '&'; Value: 'amp'),
    (Code: '<'; Value: 'lt'),
    (Code: '>'; Value: 'gt')
  );

  Entities: packed array[0..247] of TEntity = (
    (Code: WideChar(160); Value: 'nbsp'),
    (Code: WideChar(161); Value: 'iexcl'),
    (Code: WideChar(162); Value: 'cent'),
    (Code: WideChar(163); Value: 'pound'),
    (Code: WideChar(164); Value: 'curren'),
    (Code: WideChar(165); Value: 'yen'),
    (Code: WideChar(166); Value: 'brvbar'),
    (Code: WideChar(167); Value: 'sect'),
    (Code: WideChar(168); Value: 'uml'),
    (Code: WideChar(169); Value: 'copy'),
    (Code: WideChar(170); Value: 'ordf'),
    (Code: WideChar(171); Value: 'laquo'),
    (Code: WideChar(172); Value: 'not'),
    (Code: WideChar(173); Value: 'shy'),
    (Code: WideChar(174); Value: 'reg'),
    (Code: WideChar(175); Value: 'macr'),
    (Code: WideChar(176); Value: 'deg'),
    (Code: WideChar(177); Value: 'plusmn'),
    (Code: WideChar(178); Value: 'sup2'),
    (Code: WideChar(179); Value: 'sup3'),
    (Code: WideChar(180); Value: 'acute'),
    (Code: WideChar(181); Value: 'micro'),
    (Code: WideChar(182); Value: 'para'),
    (Code: WideChar(183); Value: 'middot'),
    (Code: WideChar(184); Value: 'cedil'),
    (Code: WideChar(185); Value: 'sup1'),
    (Code: WideChar(186); Value: 'ordm'),
    (Code: WideChar(187); Value: 'raquo'),
    (Code: WideChar(188); Value: 'frac14'),
    (Code: WideChar(189); Value: 'frac12'),
    (Code: WideChar(190); Value: 'frac34'),
    (Code: WideChar(191); Value: 'iquest'),
    (Code: WideChar(192); Value: 'Agrave'),
    (Code: WideChar(193); Value: 'Aacute'),
    (Code: WideChar(194); Value: 'Acirc'),
    (Code: WideChar(195); Value: 'Atilde'),
    (Code: WideChar(196); Value: 'Auml'),
    (Code: WideChar(197); Value: 'Aring'),
    (Code: WideChar(198); Value: 'AElig'),
    (Code: WideChar(199); Value: 'Ccedil'),
    (Code: WideChar(200); Value: 'Egrave'),
    (Code: WideChar(201); Value: 'Eacute'),
    (Code: WideChar(202); Value: 'Ecirc'),
    (Code: WideChar(203); Value: 'Euml'),
    (Code: WideChar(204); Value: 'Igrave'),
    (Code: WideChar(205); Value: 'Iacute'),
    (Code: WideChar(206); Value: 'Icirc'),
    (Code: WideChar(207); Value: 'Iuml'),
    (Code: WideChar(208); Value: 'ETH'),
    (Code: WideChar(209); Value: 'Ntilde'),
    (Code: WideChar(210); Value: 'Ograve'),
    (Code: WideChar(211); Value: 'Oacute'),
    (Code: WideChar(212); Value: 'Ocirc'),
    (Code: WideChar(213); Value: 'Otilde'),
    (Code: WideChar(214); Value: 'Ouml'),
    (Code: WideChar(215); Value: 'times'),
    (Code: WideChar(216); Value: 'Oslash'),
    (Code: WideChar(217); Value: 'Ugrave'),
    (Code: WideChar(218); Value: 'Uacute'),
    (Code: WideChar(219); Value: 'Ucirc'),
    (Code: WideChar(220); Value: 'Uuml'),
    (Code: WideChar(221); Value: 'Yacute'),
    (Code: WideChar(222); Value: 'THORN'),
    (Code: WideChar(223); Value: 'szlig'),
    (Code: WideChar(224); Value: 'agrave'),
    (Code: WideChar(225); Value: 'aacute'),
    (Code: WideChar(226); Value: 'acirc'),
    (Code: WideChar(227); Value: 'atilde'),
    (Code: WideChar(228); Value: 'auml'),
    (Code: WideChar(229); Value: 'aring'),
    (Code: WideChar(230); Value: 'aelig'),
    (Code: WideChar(231); Value: 'ccedil'),
    (Code: WideChar(232); Value: 'egrave'),
    (Code: WideChar(233); Value: 'eacute'),
    (Code: WideChar(234); Value: 'ecirc'),
    (Code: WideChar(235); Value: 'euml'),
    (Code: WideChar(236); Value: 'igrave'),
    (Code: WideChar(237); Value: 'iacute'),
    (Code: WideChar(238); Value: 'icirc'),
    (Code: WideChar(239); Value: 'iuml'),
    (Code: WideChar(240); Value: 'eth'),
    (Code: WideChar(241); Value: 'ntilde'),
    (Code: WideChar(242); Value: 'ograve'),
    (Code: WideChar(243); Value: 'oacute'),
    (Code: WideChar(244); Value: 'ocirc'),
    (Code: WideChar(245); Value: 'otilde'),
    (Code: WideChar(246); Value: 'ouml'),
    (Code: WideChar(247); Value: 'divide'),
    (Code: WideChar(248); Value: 'oslash'),
    (Code: WideChar(249); Value: 'ugrave'),
    (Code: WideChar(250); Value: 'uacute'),
    (Code: WideChar(251); Value: 'ucirc'),
    (Code: WideChar(252); Value: 'uuml'),
    (Code: WideChar(253); Value: 'yacute'),
    (Code: WideChar(254); Value: 'thorn'),
    (Code: WideChar(255); Value: 'yuml'),

  // Latin Extended-A                      
    (Code: WideChar(338); Value: 'OElig'),
    (Code: WideChar(339); Value: 'oelig'),

  // ligature is a misnomer, this is a separate character in some languages
    (Code: WideChar(352); Value: 'Scaron'),
    (Code: WideChar(353); Value: 'scaron'),
    (Code: WideChar(376); Value: 'Yuml'),

  // Latin Extended-B
    (Code: WideChar(402); Value: 'fnof'),     // Latin small f with hook, =function, =florin, U0192 ISOtech

  //Spacing Modifier Letters
    (Code: WideChar(710); Value: 'circ'),
    (Code: WideChar(732); Value: 'tilde'),

  // Greek
    (Code: WideChar(913); Value: 'Alpha'),    // Greek capital letter alpha, U0391
    (Code: WideChar(914); Value: 'Beta'),     // Greek capital letter beta, U0392
    (Code: WideChar(915); Value: 'Gamma'),    // Greek capital letter gamma, U0393 ISOgrk3
    (Code: WideChar(916); Value: 'Delta'),    // Greek capital letter delta, U0394 ISOgrk3
    (Code: WideChar(917); Value: 'Epsilon'),  // Greek capital letter epsilon, U0395
    (Code: WideChar(918); Value: 'Zeta'),     // Greek capital letter zeta, U0396
    (Code: WideChar(919); Value: 'Eta'),      // Greek capital letter eta, U0397
    (Code: WideChar(920); Value: 'Theta'),    // Greek capital letter theta, U0398 ISOgrk3
    (Code: WideChar(921); Value: 'Iota'),     // Greek capital letter iota, U0399
    (Code: WideChar(922); Value: 'Kappa'),    // Greek capital letter kappa, U039A
    (Code: WideChar(923); Value: 'Lambda'),   // Greek capital letter lambda, U039B ISOgrk3
    (Code: WideChar(924); Value: 'Mu'),       // Greek capital letter mu, U039C
    (Code: WideChar(925); Value: 'Nu'),       // Greek capital letter nu, U039D
    (Code: WideChar(926); Value: 'Xi'),       // Greek capital letter xi, U039E ISOgrk3
    (Code: WideChar(927); Value: 'Omicron'),  // Greek capital letter omicron, U039F
    (Code: WideChar(928); Value: 'Pi'),       // Greek capital letter pi, U03A0 ISOgrk3
    (Code: WideChar(929); Value: 'Rho'),      // Greek capital letter rho, U03A1
    (Code: WideChar(931); Value: 'Sigma'),    // Greek capital letter sigma, U03A3 ISOgrk3
    (Code: WideChar(932); Value: 'Tau'),      // Greek capital letter tau, U03A4
    (Code: WideChar(933); Value: 'Upsilon'),  // Greek capital letter upsilon, U03A5 ISOgrk3
    (Code: WideChar(934); Value: 'Phi'),      // Greek capital letter phi, U03A6 ISOgrk3
    (Code: WideChar(935); Value: 'Chi'),      // Greek capital letter chi, U03A7
    (Code: WideChar(936); Value: 'Psi'),      // Greek capital letter psi, U03A8 ISOgrk3
    (Code: WideChar(937); Value: 'Omega'),    // Greek capital letter omega, U03A9 ISOgrk3
    (Code: WideChar(945); Value: 'alpha'),    // Greek small letter alpha, U03B1 ISOgrk3
    (Code: WideChar(946); Value: 'beta'),     // Greek small letter beta, U03B2 ISOgrk3
    (Code: WideChar(947); Value: 'gamma'),    // Greek small letter gamma, U03B3 ISOgrk3
    (Code: WideChar(948); Value: 'delta'),    // Greek small letter delta, U03B4 ISOgrk3
    (Code: WideChar(949); Value: 'epsilon'),  // Greek small letter epsilon, U03B5 ISOgrk3
    (Code: WideChar(950); Value: 'zeta'),     // Greek small letter zeta, U03B6 ISOgrk3
    (Code: WideChar(951); Value: 'eta'),      // Greek small letter eta, U03B7 ISOgrk3
    (Code: WideChar(952); Value: 'theta'),    // Greek small letter theta, U03B8 ISOgrk3
    (Code: WideChar(953); Value: 'iota'),     // Greek small letter iota, U03B9 ISOgrk3
    (Code: WideChar(954); Value: 'kappa'),    // Greek small letter kappa, U03BA ISOgrk3
    (Code: WideChar(955); Value: 'lambda'),   // Greek small letter lambda, U03BB ISOgrk3
    (Code: WideChar(956); Value: 'mu'),       // Greek small letter mu, U03BC ISOgrk3
    (Code: WideChar(957); Value: 'nu'),       // Greek small letter nu, U03BD ISOgrk3
    (Code: WideChar(958); Value: 'xi'),       // Greek small letter xi, U03BE ISOgrk3
    (Code: WideChar(959); Value: 'omicron'),  // Greek small letter omicron, U03BF NEW
    (Code: WideChar(960); Value: 'pi'),       // Greek small letter pi, U03C0 ISOgrk3
    (Code: WideChar(961); Value: 'rho'),      // Greek small letter rho, U03C1 ISOgrk3
    (Code: WideChar(962); Value: 'sigmaf'),   // Greek small letter final sigma, U03C2 ISOgrk3
    (Code: WideChar(963); Value: 'sigma'),    // Greek small letter sigma, U03C3 ISOgrk3
    (Code: WideChar(964); Value: 'tau'),      // Greek small letter tau, U03C4 ISOgrk3
    (Code: WideChar(965); Value: 'upsilon'),  // Greek small letter upsilon, U03C5 ISOgrk3
    (Code: WideChar(966); Value: 'phi'),      // Greek small letter phi, U03C6 ISOgrk3
    (Code: WideChar(967); Value: 'chi'),      // Greek small letter chi, U03C7 ISOgrk3
    (Code: WideChar(968); Value: 'psi'),      // Greek small letter psi, U03C8 ISOgrk3
    (Code: WideChar(969); Value: 'omega'),    // Greek small letter omega, U03C9 ISOgrk3
    (Code: WideChar(977); Value: 'thetasym'), // Greek small letter theta symbol, U03D1 NEW
    (Code: WideChar(978); Value: 'upsih'),    // Greek upsilon with hook symbol, U03D2 NEW
    (Code: WideChar(982); Value: 'piv'),      // Greek pi symbol, U03D6 ISOgrk3

  // General Punctuation
    (Code: WideChar(8194); Value: 'ensp'),
    (Code: WideChar(8195); Value: 'emsp'),
    (Code: WideChar(8201); Value: 'thinsp'),
    (Code: WideChar(8204); Value: 'zwnj'),
    (Code: WideChar(8205); Value: 'zwj'),
    (Code: WideChar(8206); Value: 'lrm'),
    (Code: WideChar(8207); Value: 'rlm'),
    (Code: WideChar(8211); Value: 'ndash'),
    (Code: WideChar(8212); Value: 'mdash'),
    (Code: WideChar(8216); Value: 'lsquo'),
    (Code: WideChar(8217); Value: 'rsquo'),
    (Code: WideChar(8218); Value: 'sbquo'),
    (Code: WideChar(8220); Value: 'ldquo'),
    (Code: WideChar(8221); Value: 'rdquo'),
    (Code: WideChar(8222); Value: 'bdquo'),
    (Code: WideChar(8224); Value: 'dagger'),
    (Code: WideChar(8225); Value: 'Dagger'),
    (Code: WideChar(8226); Value: 'bull'),    // bullet, =black small circle, U2022 ISOpub
    (Code: WideChar(8230); Value: 'hellip'),  // horizontal ellipsis, =three dot leader, U2026 ISOpub
    (Code: WideChar(8240); Value: 'permil'),
    (Code: WideChar(8242); Value: 'prime'),   // prime, =minutes, =feet, U2032 ISOtech
    (Code: WideChar(8243); Value: 'Prime'),   // double prime, =seconds, =inches, U2033 ISOtech
    (Code: WideChar(8249); Value: 'lsaquo'),  // lsaquo is proposed but not yet ISO standardized
    (Code: WideChar(8250); Value: 'rsaquo'),  // rsaquo is proposed but not yet ISO standardized
    (Code: WideChar(8254); Value: 'oline'),   // overline, =spacing overscore, U203E NEW
    (Code: WideChar(8260); Value: 'frasl'),   // fraction slash, U2044 NEW
    (Code: WideChar(8364); Value: 'euro'),

  // Letterlike Symbols
    (Code: WideChar(8472); Value: 'weierp'),  // script capital P, =power set, =Weierstrass p, U2118 ISOamso
    (Code: WideChar(8465); Value: 'image'),   // blackletter capital I, =imaginary part, U2111 ISOamso
    (Code: WideChar(8476); Value: 'real'),    // blackletter capital R, =real part symbol, U211C ISOamso
    (Code: WideChar(8482); Value: 'trade'),   // trade mark sign, U2122 ISOnum (#153?)
    (Code: WideChar(8501); Value: 'alefsym'), // alef symbol, =first transfinite cardinal, U2135 NEW

  // Arrows
    (Code: WideChar(8592); Value: 'larr'),    // leftward arrow, U2190 ISOnum
    (Code: WideChar(8593); Value: 'uarr'),    // upward arrow, U2191 ISOnum
    (Code: WideChar(8594); Value: 'rarr'),    // rightward arrow, U2192 ISOnum
    (Code: WideChar(8595); Value: 'darr'),    // downward arrow, U2193 ISOnum
    (Code: WideChar(8596); Value: 'harr'),    // left right arrow, U2194 ISOamsa
    (Code: WideChar(8629); Value: 'crarr'),   // downward arrow with corner leftward, =carriage return, U21B5 NEW
    (Code: WideChar(8656); Value: 'lArr'),    // leftward double arrow, U21D0 ISOtech
    (Code: WideChar(8657); Value: 'uArr'),    // upward double arrow, U21D1 ISOamsa
    (Code: WideChar(8658); Value: 'rArr'),    // rightward double arrow, U21D2 ISOtech
    (Code: WideChar(8659); Value: 'dArr'),    // downward double arrow, U21D3 ISOamsa
    (Code: WideChar(8660); Value: 'hArr'),    // left right double arrow, U21D4 ISOamsa

  // Mathematical Operators
    (Code: WideChar(8704); Value: 'forall'),  // for all, U2200 ISOtech
    (Code: WideChar(8706); Value: 'part'),    // partial differential, U2202 ISOtech
    (Code: WideChar(8707); Value: 'exist'),   // there exists, U2203 ISOtech
    (Code: WideChar(8709); Value: 'empty'),   // empty set, =null set, =diameter, U2205 ISOamso
    (Code: WideChar(8711); Value: 'nabla'),   // nabla, =backward difference, U2207 ISOtech
    (Code: WideChar(8712); Value: 'isin'),    // element of, U2208 ISOtech
    (Code: WideChar(8713); Value: 'notin'),   // not an element of, U2209 ISOtech
    (Code: WideChar(8715); Value: 'ni'),      // contains as member, U220B ISOtech
    (Code: WideChar(8719); Value: 'prod'),    // n-ary product, =product sign, U220F ISOamsb
    (Code: WideChar(8722); Value: 'sum'),     // n-ary sumation, U2211 ISOamsb
    (Code: WideChar(8722); Value: 'minus'),   // minus sign, U2212 ISOtech
    (Code: WideChar(8727); Value: 'lowast'),  // asterisk operator, U2217 ISOtech
    (Code: WideChar(8730); Value: 'radic'),   // square root, =radical sign, U221A ISOtech
    (Code: WideChar(8733); Value: 'prop'),    // proportional to, U221D ISOtech
    (Code: WideChar(8734); Value: 'infin'),   // infinity, U221E ISOtech
    (Code: WideChar(8736); Value: 'ang'),     // angle, U2220 ISOamso
    (Code: WideChar(8869); Value: 'and'),     // logical and, =wedge, U2227 ISOtech
    (Code: WideChar(8870); Value: 'or'),      // logical or, =vee, U2228 ISOtech
    (Code: WideChar(8745); Value: 'cap'),     // intersection, =cap, U2229 ISOtech
    (Code: WideChar(8746); Value: 'cup'),     // union, =cup, U222A ISOtech
    (Code: WideChar(8747); Value: 'int'),     // integral, U222B ISOtech
    (Code: WideChar(8756); Value: 'there4'),  // therefore, U2234 ISOtech
    (Code: WideChar(8764); Value: 'sim'),     // tilde operator, =varies with, =similar to, U223C ISOtech
    (Code: WideChar(8773); Value: 'cong'),    // approximately equal to, U2245 ISOtech
    (Code: WideChar(8773); Value: 'asymp'),   // almost equal to, =asymptotic to, U2248 ISOamsr
    (Code: WideChar(8800); Value: 'ne'),      // not equal to, U2260 ISOtech
    (Code: WideChar(8801); Value: 'equiv'),   // identical to, U2261 ISOtech
    (Code: WideChar(8804); Value: 'le'),      // less-than or equal to, U2264 ISOtech
    (Code: WideChar(8805); Value: 'ge'),      // greater-than or equal to, U2265 ISOtech
    (Code: WideChar(8834); Value: 'sub'),     // subset of, U2282 ISOtech
    (Code: WideChar(8835); Value: 'sup'),     // superset of, U2283 ISOtech
    (Code: WideChar(8836); Value: 'nsub'),    // not a subset of, U2284 ISOamsn
    (Code: WideChar(8838); Value: 'sube'),    // subset of or equal to, U2286 ISOtech
    (Code: WideChar(8839); Value: 'supe'),    // superset of or equal to, U2287 ISOtech
    (Code: WideChar(8853); Value: 'oplus'),   // circled plus, =direct sum, U2295 ISOamsb
    (Code: WideChar(8855); Value: 'otimes'),  // circled times, =vector product, U2297 ISOamsb
    (Code: WideChar(8869); Value: 'perp'),    // up tack, =orthogonal to, =perpendicular, U22A5 ISOtech
    (Code: WideChar(8901); Value: 'sdot'),    // dot operator, U22C5 ISOamsb

  // Miscellaneous Technical
    (Code: WideChar(8968); Value: 'lceil'),   // left ceiling, =apl upstile, U2308, ISOamsc
    (Code: WideChar(8969); Value: 'rceil'),   // right ceiling, U2309, ISOamsc
    (Code: WideChar(8970); Value: 'lfloor'),  // left floor, =apl downstile, U230A, ISOamsc
    (Code: WideChar(8971); Value: 'rfloor'),  // right floor, U230B, ISOamsc
    (Code: WideChar(9001); Value: 'lang'),    // left-pointing angle bracket, =bra, U2329 ISOtech
    (Code: WideChar(9002); Value: 'rang'),    // right-pointing angle bracket, =ket, U232A ISOtech

  // Geometric Shapes
    (Code: WideChar(9674); Value: 'loz'),     // lozenge, U25CA ISOpub

  // Miscellaneous Symbols
    (Code: WideChar(9824); Value: 'spades'),  // black spade suit, U2660 ISOpub
    (Code: WideChar(9827); Value: 'clubs'),   // black club suit, =shamrock, U2663 ISOpub
    (Code: WideChar(9829); Value: 'hearts'),  // black heart suit, =valentine, U2665 ISOpub
    (Code: WideChar(9830); Value: 'diams')    // black diamond suit, U2666 ISOpub
  );
