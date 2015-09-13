(*
    PE Tool's strings and messages

    Copyright (c) 2012-2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Kolibri -- proposed KOLIBRI format support
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

  s3GB        = '3GB';
  sCleanVer   = 'cleanver';
  sDeep       = 'deep';
  sDropRes    = 'dropres';
  sDropSect   = 'dropsect';
  sKolibri    = 'kolibri';
  sLocale     = 'locale';
  sMainIcon   = 'mainicon';
  sMenuet     = 'menuet';
  sMiniRes    = 'minires';
  sPause      = 'pause';
  sRebase     = 'rebase';
  sOSVer      = 'osver';
  sStrip      = 'strip';
  sTrunc      = 'trunc';

{ Localize }

const
  sTitle = '%s %hs  %s';
  sUsage =
    'Usage:'#9'%s <source> [commands [options]]';
  sHelp =
                                                                     #9#9#9#9'/\_/\'#10 +
                                                     #9#9#9#9#9#9#9#9'   ____/ o o \'#10 +
    'Commands' +                                       #9#9#9#9#9#9#9' /~____ ==t==/'#10 +
            #9'-backup <bak>'#9'Backup <source> file to <bak> file'#9'(_______)_m_m)'#10 +
          //  #9'-dump <dump>'#9'Dump <source> info to <dump> file'#10 +
            #9'-extract <stub>'#9'Extract <source> stub to <stub> file'#10 +
            #9'-into [dest]'#9'Save result to [dest] file'+      #9#9'    PET :-)'#10#10 +
    'Option for -extract and -into commands'#10 +
            #9'-strip'#9#9#9'Strip headers, relocations and empty sections'#10 +
            #9#9#9#9'when possible'#10#10 +
    'Options for -into command'#10 +
            #9'-3GB'#9#9#9'Set large address aware application flag'#10 +
//            #9'-cleanver'#9#9'Clean version info resource generated by Delphi'#10 +
            #9'-deep'#9#9#9'Strip orphaned sections too (unsafe!)'#10 +
//            #9'-dropres <r1,r2,...>'#9'Drop resources <r1>, <r2>, etc.'#10 +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.'#10 +
          {$IFDEF Kolibri}
            #9'-kolibri'#9#9'Save to KOLIBRI format'#10 +
          {$ENDIF}
//            #9'-locale <a1,a2,...>'#9'Set resource locales to <a1>, <a2>, etc.'#10 +
//            #9#9#9#9'Each <a> has form of <#=#> or <#>'#10 +
//            #9'-mainicon'#9#9'Strip the name of MAINICON resource (not for'#10 +
//            #9#9#9#9'VCL applications!)'#10 +
          //  #9'-menuet'#9#9#9'Save to MENUET01 format'#10 +
          //  #9'-rebase <#>'#9#9'Change image base to <#> (hexadecimal value)'#10 +
            #9'-osver <#[.#]>'#9#9'Set required OS version to <#.#> or only <#>'#10 +
            #9'-stub [stub]'#9#9'Replace stub with one from [stub] file'#10 +
            #9'-trunc'#9#9#9'Don''t align last section, truncate file'#10 +
            #9#9#9#9'immediately after data end'#10#10 +
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

  FileNameWidth = 48;
  FileNameOffset = 12;

  sProcessing     = '%-16hs %-32@s %8u bytes';

  sLoadingSource    = 'Loading source';
  sImageData        = 'Image data';
  sExtractingStub   = 'Extracting stub';
  sFixingStub       = 'Fixing stub';
  sInsertingStub    = 'Inserting stub';
  sSavingInto       = 'Saving into';
  sDroppingSection  = 'Dropping section';
  sDroppingResource = 'Dropping resource';
  sStripping        = 'Stripping';
  sTotal            = 'Total';

  sBackuping      = 'Backuping'#9' %s';

implementation

end.

