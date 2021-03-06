(*
    LiteConv string constants and messages
*)

unit ConvConsts;

interface

{$I build.inc}

{ Do not localize }

const         // short Unicode strings
  sInto     = #4'into';
  sNoLogo   = #6'nologo';
  sPause    = #5'pause';
  sRen      = #3'ren';
  sVersion  = #7'version';

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
  sTitle = 'LiteConv build ' + Build;

  sUsage  =
    'Usage:' + sLineBreak +
            {#9'%ws source-file [<in-charset>] [-into <dest-file> [<out-charset>]]' + sLineBreak +
            #9#9'[-pause]' + sLineBreak + sLineBreak +
            #9'%ws -ren source-file [-oem [-pause]]'}
            #9'%s [commands [options]]';
  sHelp =
    'Commands' + sLineBreak +
            #9'-ren <file>'#9#9'Auto detect broken UTF-8 and rename <file> when' + sLineBreak +
            #9#9#9#9'needed' + sLineBreak +
            #9'-version'#9#9'Show version and exit' + sLineBreak + sLineBreak +
    'Options' + sLineBreak +
            #9'-nologo'#9#9#9'Don''t display program title' + sLineBreak +
            #9'-oem'#9#9#9'Use current OEM code page' + sLineBreak + 
            #9'-pause'#9#9#9'Wait for ENTER key to exit';
    {'Options for both <in-charset> and <out-charset>' + sLineBreak +
            #9'-cp N'#9'Use code page N to decode/encode file' + sLineBreak +
            #9'-oem'#9'Use current OEM code page to decode file' + sLineBreak + sLineBreak +
    'Options for <out-charset> only' + sLineBreak +
            #9'-8'#9'Encode to CESU-8' + sLineBreak +
            #9'-16'#9'Encode to UTF-16' + sLineBreak +
            #9'-32'#9'Encode to UTF-32' + sLineBreak +
            #9'-BE'#9'Encode UTF-16 or UTF-32 as big-endian'}

  sEnvironment =
    'Current environment' + sLineBreak +
            #9'ANSI code page (ACP)'#9'%u (%s)' + sLineBreak +
            #9'OEM code page (OEMCP)'#9'%u (%s)';

  sMissing = 'Missing %s';
  sDuplicate = 'Duplicate %hs: �%s�';

  sSourceFileNameParam = 'source file name';
  sIntoFileNameParam = '-into file name';
  sCallPropNameParam = 'property name to -call';
  sCodePageParam = 'code page number';
  sRenameParam = 'file name to rename';
//  sIsNotACodePage = 'UTF-%u is not a valid code page';

  sSourceFileCP = 'Source file code page: %u (%s)';
  sRenameUsingCP = 'Rename using code page: %u (%s)';
  sSourceFileName = 'Source file name: %s';

  sRenamed = 'Renamed �%s� to �%s�';
  sNoRenameNeeded = 'File name �%s� is valid, no rename needed';

implementation

end.
