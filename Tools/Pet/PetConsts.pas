(*
    PE Tool's strings and messages

    Copyright (c) 2012-2016 Vladislav Javadov (aka Freeman)
*)

unit PetConsts;

interface

{ Do not localize }

const         // short Unicode strings
  sInto       = #4'into';
  sBackup     = #6'backup';
  sStub       = #4'stub';
  sExtract    = #7'extract';
  sDump       = #4'dump';
  sVersion    = #7'version';

  s3GB        = #3'3GB';
  sAuto       = #4'auto';
  sCleanVer   = #8'cleanver';
  sDeep       = #4'deep';
  sDropSect   = #8'dropsect';
  sListSect   = #8'listsect';
  sLog        = #3'log';
  sMenuet     = #7'menuet';
  sNoLogo     = #6'nologo';
  sPause      = #5'pause';
  sOSVer      = #5'osver';
  sStrip      = #5'strip';
  sTouch      = #5'touch';
  sTrunc      = #5'trunc';
  sUnsafe     = #6'unsafe';
  sVerbose    = #7'verbose';

  sActions    = #7'actions';
  sTotals     = #6'totals';

  sLogo =
          #9'/\_/\'#10 +
    '    .__/ o o \    %s %hs'#10 +
    '   /~__ ==t==/    %s'#10 +
    '  (_____)_m_m)';

{ Localize }

const
  sUsage =
    'Usage:'#9'%s <source> [[<source>...] [commands [options]]]';
  sHelp =
    'Commands'#10 +
            #9'-backup <bak>' +    #9#9'Backup <source> file to <bak> file'#10 +
            #9'-extract <stub>' +  #9#9'Extract <source> stub to <stub> file'#10 +
            #9'-into [dest]|[.]' +   #9'Save result to [dest] file or replace source'#10 +
            #9'-version' +         #9#9'Show version and exit'#10#10 +
    'Option for -extract and -into commands'#10 +
            #9'-strip' +         #9#9#9'Strip headers, exports, debug information,'#10 +
                               #9#9#9#9'relocations and empty sections when possible'#10#10 +
    'Options for -into command'#10 +
            #9'-3GB' +           #9#9#9'Set large address aware application flag'#10 +
            {#9'-auto [#[%]]|[.]'    +#9'Using '#$E2#$80#$9C'-keep -strip -trunc -stub'#$E2#$80#$9D' mode, replace'#10 +
                               #9#9#9#9'original file when stripped size is equal or'#10 +
                               #9#9#9#9'greater given number of bytes, percent or'#10 +
                               #9#9#9#9'volume cluster size'#10 +}
            #9'-deep' +          #9#9#9'Strip orphaned sections (unsafe!)'#10 +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.'#10 +
            #9'-log <actions|totals>'#9'Output log style'#10 +
            //#9'-menuet' +        #9#9#9'Save to MENUET01/MENUET02 format'#10 +
            #9'-osver <#[.#]>' +   #9#9'Set required OS version to <#> or <#.#>'#10 +
            //#9'-rebase <########>' + #9'Rebase image to hexadecimal address <########>'#10 +
            #9'-stub [stub]|[.]' +   #9'Replace stub with one from [stub] file or take'#10 +
                               #9#9#9#9'stub from PE Tool itself'#10 +
            #9'-touch' +         #9#9#9'Don''t keep original file timestamp'#10 +
            #9'-trunc' +         #9#9#9'Don''t align last section, truncate file'#10 +
                               #9#9#9#9'immediately after data end'#10 +
            #9'-unsafe' +        #9#9#9'Strip chained certificate and other data'#10#10 +
    'Miscellaneous options'#10 +
            #9'-listsect' +        #9#9'Display section list and exit'#10 +
            #9'-nologo' +        #9#9#9'Don''t display PET logo and copyrights'#10 +
            #9'-pause' +         #9#9#9'Wait for ENTER key to exit';

  sMissingParam = 'Missing %s';
  sDuplicateParam = 'Duplicate %hs: %s';
  sInvalidOption = 'Invalid option: %s';
  sInvalidOptionValue = 'Invalid %hs option value: %s';
  sFileName = '%ws file name'; // CoreChar
  sSource = 'source';
  sOSVersion = 'OS version number';
  sSectionNames = 'section names';
  sResourceNames = 'resource names';
  sLogStyle = 'log style';
  sLocaleMap = 'locale map';
  sImageBase = 'image base value';

  sCannotRebaseImage = 'Cannot rebase image: %s';

  sPathEllipsis = '[...]';

  sLoading          = 'Loading';
  sChainedData      = 'Chained data';
  sImageData        = 'Image data';
  sExtractingStub   = 'Extracting stub';
  sFixingStub       = 'Fixing stub';
  sReplacingStub    = 'Replacing stub';
  sSaving           = 'Saving';
  sDroppingSection  = 'Dropping section';
  sStripping        = 'Stripping';
  sEstimating       = 'Estimating';
  sBackuping        = 'Backuping';
  sTotal            = 'Total';
  sSectionList      = 'Section list of';

  DefaultMaxWidth = sChainedData; // max of {sLoading, sChainedData, sStrippng,
                                  //         sEstimating, sFixingStub}
  TotalsMaxWidth  = sEstimating;  // max of {sStripping, sEstimating}
  
  PercentageWidth = 6;

  sDefaultActionFmt = '%%%dhs  %%hs%%%ds';   // '{*s}  {*s?{s1}}'
  sDefaultStatsFmt = '  %%%di bytes  %%6hs'; // '  {*u?{u'#160'bytes}}  {6p}'

  sChainedDataFound = 'Chained data found, safe stripping stopped';
  sTotalsMessage = #10'%u files %d bytes (%hs)';
  sNoFilesFound = 'No files found';
  sSectionNotFound = 'Section “%s” not found';

implementation

end.

