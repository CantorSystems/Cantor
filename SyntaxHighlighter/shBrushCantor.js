/**
 * SyntaxHighlighter brush for Cantor
 *
 * @copyright
 * Copyright © 2014 Vladislav Javadov
 *
 * @license
 * Licensed under the MIT license
 */
;(function()
{
	// CommonJS
	typeof(require) != 'undefined' ? SyntaxHighlighter = require('shCore').SyntaxHighlighter : null;

	function Brush()
	{
		var keywords =
			'align all alter and any as by case class const do else elsif end except external ' +
			'final finally for from group handle if in inherited inner into is level ' +
			'memorysize new node not null of on or order out outer outside override '+
			'partial path protected public raise ref return root row select static ' +
			'then this union var when where with xor ' +
			'cdecl fastcall mcall optlink pascal register safecall stdcall syscall thiscall';

		var entities =
			'ACK BEL BS CAN CR DC1 DC2 DC3 DC4 DEL DLE EM ENQ EOF EOT ESC ETB ETH ETX FF FS ' +
			'GS HT LF NL NP NUL NAK RS SI SO SOH STX SUB SYN US VT XOFF XON ' +
			'acute alefsym amp and ang asymp bdquo brvbar bull cap cedil cent circ clubs cong ' +
			'copy crarr cup curren deg diams divide empty emsp ensp equiv eth euro exist fnof ' +
			'forall frac12 frac14 frac34 frasl ge gt hearts hellip iexcl image infin int iquest ' +
			'isin lang laquo lceil ldquo le lfloor lowast loz lrm lsaquo lsquo lt macr mdash ' +
			'micro middot minus nabla nbsp ndash ne ni not notin nsub oline oplus or ordf ordm ' +
			'otimes para part permil perp piv plusmn pound prod prop quot radic rang raquo rceil ' +
			'rdquo real reg rfloor rlm rsaquo rsquo sbquo sdot sect shy sigmaf sim spades sub sube ' +
			'sum sup sup1 sup2 sup3 supe szlig tab tau there4 thetasym thinsp tilde times trade ' +
			'uml upsih weierp yen zwj zwnj';

		var cs_entities =
			'aacute Aacute acirc Acirc aelig AElig agrave Agrave alpha Alpha aring Aring ' +
			'atilde Atilde auml Auml beta Beta ccedil Ccedil chi Chi dagger Dagger darr dArr ' +
			'delta Delta eacute Eacute ecirc Ecirc egrave Egrave epsilon Epsilon eta Eta ' +
			'euml Euml gamma Gamma harr hArr iacute Iacute icirc Icirc igrave Igrave iota Iota ' +
			'iuml Iuml kappa Kappa lambda Lambda larr lArr mu Mu ntilde Ntilde nu Nu ' +
			'oacute Oacute ocirc Ocirc oelig OElig ograve Ograve omega Omega omicron Omicron ' +
			'oslash Oslash otilde Otilde ouml Ouml phi Phi pi Pi prime Prime psi Psi rarr rArr ' +
			'rho Rho scaron Scaron sigma Sigma Tau theta Theta thorn THORN uacute Uacute ' +
			'uarr uArr ucirc Ucirc ugrave Ugrave upsilon Upsilon uuml Uuml xi Xi yacute Yacute ' +
			'yuml Yuml zeta Zeta';

		this.regexList = [
			{ regex: SyntaxHighlighter.regexLib.multiLineCComments,   css: 'comments' },  // multiline comments /* */
			{ regex: SyntaxHighlighter.regexLib.singleLineCComments,  css: 'comments' },  // one line comments
			{ regex: SyntaxHighlighter.regexLib.singleQuotedString,   css: 'string' },    // strings

			{ regex: /#[01]+[Bb]\b/g,                                 css: 'value' },     // chars #10011b
			{ regex: /#\d+((\.\d+)?[KkMmGgEePpZzYy])?\b/g,            css: 'string' },    // chars #12345.25KMGEPZY
			{ regex: /#\$[a-fA-F\d]+\b/g,                             css: 'string' },    // chars #$F5D3
			{ regex: /#\b[0-9]+[a-fA-F0-9]*h\b/g,                     css: 'string' },    // chars #0F5D3h

			{ regex: new RegExp('#' + this.getKeywords(entities), 'gmi'),   css: 'string' },  // entities #CR #euro
			{ regex: new RegExp('#' + this.getKeywords(cs_entities), 'gm'), css: 'string' },  // entities #Oslash #Psi

			{ regex: /\{'\s*[\S\s]*'\}/gm,                            css: 'string' },    // text
			{ regex: /\{=[\S\s]*=\}/gm,                               css: 'string' },    // wiki
			{ regex: /\{(<|&lt;)[\S\s]*(>|&gt;)\}/gm,                 css: 'string' },    // HTML/XML
			{ regex: /\{\\[\S\s]*\\\}/gm,                             css: 'string' },    // TeX
			{ regex: /\{\{[\S\s]*\}\}/gm,                             css: 'string' },    // RTF

			{ regex: /(\b|[\s\+-])\d+(\.\d+)?([Ee][\+-]?\d+)?\b/g,    css: 'value' },     // numbers -123.45e-67
			{ regex: /(\b|[\s\+])[01]+[Bb]\b/g,                       css: 'value' },     // numbers +10011b
			{ regex: /(\b|[\s\+])\d+(\.\d+)?[KkMmGgEePpZzYy]?\b/g,    css: 'value' },     // numbers +12345.25KMGEPZY
			{ regex: /[\s\+]\$[a-fA-F\d]+\b/g,                        css: 'value' },     // numbers +$F5D3
			{ regex: /(\b|[\s\+])[0-9]+[a-fA-F0-9]*[Hh]\b/g,          css: 'value' },     // numbers 0F5D3h

			{ regex: /"(("")|.)+"/g,                                  css: 'plain' },     // "escaped identifier"

			{ regex: new RegExp(this.getKeywords(keywords), 'gm'),    css: 'keyword' }    // keywords
		];
	};

	Brush.prototype	= new SyntaxHighlighter.Highlighter();
	Brush.aliases = ['Cantor'];

	SyntaxHighlighter.brushes.Cantor = Brush;

	// CommonJS
	typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();
