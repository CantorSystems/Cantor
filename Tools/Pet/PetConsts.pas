(*
    PE Tool strings and messages

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

  s3GB        = #3'3gb';
  sASLR       = #4'aslr';
  sAuto       = #4'auto';
  sDir        = #3'dir';
  sDeep       = #4'deep';
  sDropSect   = #8'dropsect';
  sLS         = #2'ls';
  sLog        = #3'log';
  sMenuet     = #7'menuet';
  sNoLogo     = #6'nologo';
  sNX         = #2'nx';
  sPause      = #5'pause';
  sRaw        = #3'raw';
  sRebase     = #6'rebase';
  sOSVer      = #5'osver';
  sStrip      = #5'strip';
  sTouch      = #5'touch';
  sTrunc      = #5'trunc';
  sUnsafe     = #6'unsafe';
  sVerbose    = #7'verbose';
  sVerInfo    = #8'verinfo';

  sActions    = #7'actions';
  sTotals     = #6'totals';

  sLogo =
          #9'/\_/\'#10 +
    '    .__/ o o \    %s %hs'#10 +
    '   /~__;==t==/    %s'#10 +
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
    'Options for estimation and -into command'#10 +
            #9'-3gb' +           #9#9#9'Set large address aware flag'#10 +
            #9'-aslr' +          #9#9#9'Set dynamic image base flag (ASLR)'#10 +
            {#9'-auto [#[%]]|[.]'    +#9'Using '#$E2#$80#$9C'-keep -strip -trunc -stub'#$E2#$80#$9D' mode, replace'#10 +
                               #9#9#9#9'original file when stripped size is equal or'#10 +
                               #9#9#9#9'greater given number of bytes, percent or'#10 +
                               #9#9#9#9'volume cluster size'#10 +}
            #9'-deep' +          #9#9#9'Strip orphaned sections (unsafe!)'#10 +
            #9'-dir' +           #9#9#9'Strip data directory (lesser compatibility)'#10 +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.'#10 +
            #9'-log <actions|totals>'#9'Output log style'#10 +
            #9'-menuet' +        #9#9#9'Save to MENUET01/MENUET02 format (alpha stage!)'#10 +
            #9'-nx' +            #9#9#9'Set DEP compatibility flag (NX)'#10 +
            #9'-osver <#[.#]>' +   #9#9'Set required OS version to <#> or <#.#>'#10 +
            #9'-raw' +           #9#9#9'Don''t align raw data size values at section'#10 +
                               #9#9#9#9'headers to avoid antivirus false positives'#10 +
            #9'-rebase <########>' + #9'Rebase image to hexadecimal address <########>'#10 +
            #9'-stub [stub]|[.]' +   #9'Replace stub with one from [stub] file or take'#10 +
                               #9#9#9#9'stub from PE Tool itself'#10 +
            #9'-touch' +         #9#9#9'Don''t keep original file timestamp'#10 +
            #9'-trunc' +         #9#9#9'Don''t align last section, truncate file'#10 +
                               #9#9#9#9'immediately after data end'#10 +
            #9'-unsafe' +        #9#9#9'Strip chained certificate and other data'#10#10 +
    'Miscellaneous options'#10 +
            #9'-ls' +            #9#9#9'Display section list and exit'#10 +
            #9'-nologo' +        #9#9#9'Don''t display PET logo and copyrights'#10 +
            #9'-pause' +         #9#9#9'Wait for ENTER key to exit';

  sMissingParam = 'Missing %s';
  sDuplicateParam = 'Duplicate %hs: %s';
  sInvalidOption = 'Invalid option: %s';
  sInvalidOptionValue = 'Invalid %hs option value: %s';
  sFileName = '%ws file name'; // CoreChar
  sSource = 'source';
  sOSVersion = 'OS version number';
  sRebaseAddress = 'rebase address';
  sSectionNames = 'section names';
  sResourceNames = 'resource names';
  sLogStyle = 'log style';
  sLocaleMap = 'locale map';
  sImageBase = 'image base value';

  sCannotRebaseImage = #10'Cannot rebase image: %s';

  sPathEllipsis = '[...]';

  sLoading            = 'Loading';
  sChainedData        = 'Chained data';
  sImageData          = 'Image data';
  sExtractingStub     = 'Extracting stub';
  sFixingStub         = 'Fixing stub';
  sReplacingStub      = 'Replacing stub';
  sSaving             = 'Saving';
  sDroppingSection    = 'Dropping section';
  sKeepingRelocations = 'Keeping relocations';
  sStripping          = 'Stripping';
  sEstimating         = 'Estimating';
  sBackuping          = 'Backuping';
  sTotal              = 'Total';
  sSectionList        = 'Section list of %s';
  sRebasingTo         = 'Rebasing image to';

  DefaultMaxWidth = sChainedData; // max of {sLoading, sChainedData, sStrippng,
                                  //         sEstimating, sFixingStub}
  PercentageWidth = 6;

  sDefaultActionFmt = '%%%dhs  %%hs%%%ds';   // '{*s}  {*s?{s1}}'
  sDefaultStatsFmt = '  %%%di bytes  %%6hs'; // '  {*u?{u'#160'bytes}}  {6p}'

  sChainedDataFound = 'Chained data found, safe stripping stopped';
  sTotalsMessage = #10'%u files %d bytes (%hs)';
  sNoFilesFound = 'No files found';
  sSectionNotFound = 'Section “%s” not found';
  sImageBaseUnaligned = 'Image base %08X is not on 64 KB boundary';
  sMenuetAt0 = 'MENUET01/02 images should start at image base 0';
  sNoRelocationsForASLR = 'Dynamic image base feature (ASLR) is not available because relocations were'#10 +
    'stripped from this file';

implementation

end.
