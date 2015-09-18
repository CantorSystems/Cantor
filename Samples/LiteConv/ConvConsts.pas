(*
    LiteConv string constants and messages
*)

unit ConvConsts;

interface

{$I Revision.inc} // use Revision.bat to generate

{ Do not localize }

const         // short Unicode strings
  sInto     = #4'into';
  sPause    = #5'pause';

  sBE       = #2'BE';
  sBig5     = #4'Big5';
//  sCP       = #2'CP';
  sGBK      = #3'GBK';
  sOEM      = #3'OEM';
  sShiftJIS = #9'Shift_JIS';
  sSJIS     = #4'SJIS';
  sUTF      = #3'UTF';
//  sWindows  = #8'Windows-';

{ Localize }

const
  sTitle = 'LiteConv ' + Revision;

  sUsage  =
    'Usage:'#10 +
            #9'%ws source-file [<in-charset>] [-into <dest-file> [<out-charset>]]'#10 +
            #9#9'[-pause]'#10#10 +
            #9'%ws -rename source-file [-pause]';
  sHelp =
    'Common options'#10 +
            #9'-into'#9#9'Replace source file with result'#10 +
            #9'-into <dest-file>'#9'Write result to <dest-file>'#10 +
            #9'-pause'#9#9'Wait for ENTER key to exit'#10#10 {+
    'Options for both <in-charset> and <out-charset>'#10 +
            #9'-cp N'#9'Use code page N to decode/encode file'#10 +
            #9'-oem'#9'Use current OEM code page to decode file'#10#10 +
    'Options for <out-charset> only'#10 +
            #9'-8'#9'Encode to CESU-8'#10 +
            #9'-16'#9'Encode to UTF-16'#10 +
            #9'-32'#9'Encode to UTF-32'#10 +
            #9'-BE'#9'Encode UTF-16 or UTF-32 as big-endian'};
  sEnvironment =
    'Current environment'#10 +
            #9'ANSI code page (ACP)'#9'%u (%s)'#10+
            #9'OEM code page (OEMCP)'#9'%u (%s)';

  sMissing = 'Missing %s';
  sDuplicate = 'Duplicate %hs: “%s”';

  sSourceFileNameParam = 'source file name';
  sIntoFileNameParam = '-into file name';
  sCallPropNameParam = 'property name to -call';
  sCodePageParam = 'code page number';
//  sIsNotACodePage = 'UTF-%u is not a valid code page';

  sSourceFileCP = 'Source file code page: %u (%s)';
  sSourceFileName = 'Source file name: %s';

implementation

end.



