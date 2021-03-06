(*
    Textest string constants and messages
*)

unit TestConsts;

interface

{$I build.inc}

{ Do not localize }

const       // short Unicode strings
  sCP       = #2'CP';
  sInto     = #4'into';
  sOEM      = #3'OEM';
  sPause    = #5'pause';

{ Localize }

const
  sTitle = 'Textest ' + Build;

  sUsage =
    'Usage:'#9'%s <source> [commands [options]]';

  sHelp =
    'Commands' + sLineBreak +
            #9'-cp <####>'+ #9'Set fallback code page to <####>' + sLineBreak +
            #9'-into <dest>'#9'Save result to <dest> file' + sLineBreak + sLineBreak +
    'Options' + sLineBreak +
            #9'-oem' +    #9#9'Set fallback code page to current OEM code page' + sLineBreak +
            #9'-pause' +  #9#9'Wait for ENTER key to exit';

  sMissing = 'Missing %s';
  sDuplicate = 'Duplicate %hs: �%s�';

  sSourceFileNameParam = 'source file name';
  sIntoFileNameParam = '-into file name';
  sFallbackCodePageParam = 'code page number for fallback -cp';
  sIsNotAFallbackCP = 'UTF-%u is not a fallback code page';

  sFileNameFmt = '%hs: �%s�';
  sSourceFile = 'Source file';
  sSavingInto = 'Saving into';
  sFallbackCP = 'Fallback code page: %u (%s)';

  sBytesLinesSecondsGBs = '%hs: %d bytes, %d lines, %hs seconds, %hs GB/s';
  sAverageLineLength = 'Average line length: %d';
  sEstimationVariance = 'Estimation variance: %hs%% (%d characters '#215' %d lines)';
  sCapacityOverhead = 'Capacity overhead: %hs%% (%d bytes)';
  sTotalOverhead = 'Total overhead: %hs%% (%d bytes)';

  sCoreLiteTestFailed = 'CoreLite test failed for average string length %d';
  sDelphiTestFailed = 'Delphi test failed';

implementation

end.
