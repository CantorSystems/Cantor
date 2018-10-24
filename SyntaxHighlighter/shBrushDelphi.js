/**
 * SyntaxHighlighter brush for Delphi
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
			'absolute abstract and array as asm assembler at automated begin case cdecl class const ' +
			'constructor contains default deprecated destructor dispid dispinterface div do downto dynamic ' +
			'else end except export exports external experimental far file final finalization finally for ' +
			'forward function generic goto helper if implementation implements in index inherited initialization ' +
			'inline interface is label library local message mod name near nil nodefault not of on operator or out ' +
			'overload override package packed pascal platform private procedure program property protected ' +
			'public published raise read readonly record reference register reintroduce remove repeat requires ' +
			'resident resourcestring safecall set shl shr specialize static stdcall stored strict string then ' +
			'threadvar to try type uint64 unit unsafe until uses var varargs virtual while with write writeonly xor';

		this.regexList = [
			{ regex: /\(\*[\s\S]*?\*\)/gm,                              css: 'comments' },  // multiline comments (* *)
			{ regex: /{(?!\$)[\s\S]*?}/gm,                              css: 'comments' },  // multiline comments { }
			{ regex: SyntaxHighlighter.regexLib.singleLineCComments,    css: 'comments' },  // one line
			{ regex: SyntaxHighlighter.regexLib.singleQuotedString,     css: 'string' },    // strings
			{ regex: /\^[@a-zA-Z]\b/g,                                  css: 'string' },    // chars ^A
			{ regex: /#\d+\b/g,                                         css: 'string' },    // chars #12345
			{ regex: /#\$[a-fA-F\d]+\b/g,                               css: 'string' },    // chars #$FA89
			{ regex: /\{\$[a-zA-Z]+ .+\}/g,                             css: 'preprocessor' },  // compiler directives
			{ regex: /(\b|[\s\+-])\d+(\.\d+)?([Ee][\+-]?\d+)?\b/g,      css: 'value' },     // numbers +123.45e-67
			{ regex: /\$[a-fA-F\d]+\b/g,                                css: 'value' },     // numbers $F5D3
			{ regex: new RegExp(this.getKeywords(keywords), 'gmi'),     css: 'keyword' }    // keyword
		];
	};

	Brush.prototype = new SyntaxHighlighter.Highlighter();
	Brush.aliases = ['Delphi', 'Pascal'];

	SyntaxHighlighter.brushes.Delphi = Brush;

	// CommonJS
	typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();
