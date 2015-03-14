(*
    Textest string constants and messages
*)

unit TestConsts;

interface

{$I Revision.inc} // use Revision.bat to generate

{ Do not localize }

const       // short Unicode strings
  sCP       = #2'CP';
  sInto     = #4'into';
  sOEM      = #3'OEM';
  sPause    = #5'pause';

{ Localize }

const
  sTextest = '%s ' + Revision;

  sUsage =
    'Usage:'#9'%s <source> [commands [options]]';

  sHelp =
    'Commands'#10 +
            #9'-cp <####>'+ #9'Set fallback code page to <####>'#10 +
            #9'-into <dest>'#9'Save result to <dest> file'#10#10 +
    'Options'#10 +
            #9'-oem' +    #9#9'Set fallback code page to current OEM code page'#10 +
            #9'-pause' +  #9#9'Wait for ENTER key to exit';

  sPressEnterToExit = 'Press ENTER to exit';

  sModuleFileName = 'module file name, length';

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

implementation

end.
