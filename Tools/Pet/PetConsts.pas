(*
    PE Tool's strings and messages

    Copyright (c) 2012-2013 Vladislav Javadov (Freeman)
*)

unit PetConsts;

interface

{ Do not localize }

const
  CP_CORE = 1252; // we're using ��, but only in exception messages

  sInto     = 'into';
  sBackup   = 'backup';
  sStub     = 'stub';
  sExtract  = 'extract';
  sDump     = 'dump';

  s3GB          = '3GB';
  sCleanVer     = 'cleanver';
  sDeep         = 'deep';
  sDropRes      = 'dropres';
  sDropSections = 'dropsect';
  sLocale       = 'locale';
  sMainIcon     = 'mainicon';
  sMenuet       = 'menuet';
  sPause        = 'pause';
  sRebase       = 'rebase';
  sOSVer        = 'osver';
  sStrip        = 'strip';

{ Localize }

const
  sUsage =
    'Usage:'#9'%s <source> [commands [options]]';
  sHelp =
                                                                     #9#9#9#9'/\_/\'#10 +
                                                     #9#9#9#9#9#9#9#9'   ____/ o o \'#10 +
    'Commands' +                                       #9#9#9#9#9#9#9' /~____ ==t==/'#10 +
            #9'-backup <bak>'#9'Backup <source> file to <bak> file'#9'(_______)_m_m)'#10 +
          //  #9'-dump <dump>'#9'Dump <source> info to <dump> file'#10 +
            #9'-extract <xtub>'#9'Extract <source> stub to <xtub> file'#10 +
            #9'-into <dest>'#9'Save result to <dest> file'+      #9#9'    PET :-)'#10#10 +
    'Options for <dest>'#10 +
            #9'-3GB'#9#9#9'Set large address aware application flag'#10 +
            #9'-cleanver'#9#9'Clean version info resource generated by Delphi'#10 +
            #9'-deep'#9#9#9'Strip orphaned sections too (unsafe!)'#10 +
            #9'-dropres <r1,r2,...>'#9'Drop resources <r1>, <r2>, etc.'#10 +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.'#10 +
            #9'-locale <a1,a2,...>'#9'Set locales <a1>, <a2>, etc. Each <a> has form'#10 +
            #9#9#9#9'of <#=#> or <#>'#10 +
            #9'-mainicon'#9#9'Strip the name of MAINICON resource (not for'#10 +
            #9#9#9#9'VCL applications!)'#10 +
          //  #9'-menuet'#9#9#9'Save to MENUET01 format'#10 +
          //  #9'-rebase <#>'#9#9'Change image base to <#> (hexadecimal value)'#10 +
            #9'-osver <#[.#]>'#9#9'Set required OS version to <#.#> or <#> only'#10 +
            #9'-strip'#9#9#9'Strip headers, relocations and empty sections'#10 +
            #9#9#9#9'when possible'#10 +
            #9'-stub <stub>'#9#9'Replace stub with one from <stub> file'#10#10 +
    'Other option'#10 +
            #9'-pause'#9#9#9'Wait for ENTER key to exit';

  sPressEnterToExit = 'Press ENTER to exit';

  sMissingFileName = 'Missing file name for �%s� key';
  sMissingParam = 'Missing %s';
  sOSVersion = 'OS version number';
  sSectionNames = 'section names';
  sResourceNames = 'resource names';
  sLocaleMap = 'locale map';
  sImageBase = 'image base value';

  sCannotRebaseImage = 'Cannot rebase image: %s';

  sDone = #9'done';
  sExtractingStub = 'Extracting stub'#9'%ws (%d bytes)';
  sWritingInto = 'Writing into'#9'%ws (%d bytes)';

implementation

end.

