(*
    PE Tool strings and messages

    Copyright (c) 2012-2018, 2020 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Locale -- use &times; sign in locale-dependent encoding or raw UTF-8
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
  sASLR       = #4'ASLR';
  sAuto       = #4'auto';
  sDir        = #3'dir';
  sDeep       = #4'deep';
  sDEP        = #3'DEP';
  sDotNet     = #4'.NET';
  sDropSect   = #8'dropsect';
  sLS         = #2'ls';
  sLog        = #3'log';
  sNoLogo     = #6'nologo';
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

  sBrief      = #5'brief';
  sDetail     = #6'detail';

  sLogo =
    '        /\_/\' + sLineBreak +
    '    .__/ o o \    %s %hs' + sLineBreak +
    '   /~__;==t==/    %s' + sLineBreak +
    '  (_____)_m_m)';

{ Localize }

const
  TimesSign = {$IFDEF Locale} #215 {$ELSE} #$C3#$97 {$ENDIF};

  sUsage =
    'Usage:'#9'%s <source> [[<source>...] [commands [options]]]';

  sHelp =
    'Commands' + sLineBreak +
            #9'-backup <bak>' +    #9#9'Backup <source> file to <bak> file' + sLineBreak +
            #9'-extract <stub>' +  #9#9'Extract <source> stub to <stub> file' + sLineBreak +
            #9'-into [dest]|[.]' +   #9'Save result to [dest] file or replace source' + sLineBreak +
            #9'-version' +         #9#9'Show version and exit' + sLineBreak + sLineBreak +
    'Option for -extract and -into commands' + sLineBreak +
            #9'-strip' +         #9#9#9'Strip headers, exports, debug information,' + sLineBreak +
                               #9#9#9#9'relocations and empty sections when possible' + sLineBreak + sLineBreak +
    'Options for estimation and -into command' + sLineBreak +
            #9'-3gb' +           #9#9#9'Enable large address' + sLineBreak +
            #9'-aslr' +          #9#9#9'Enable dynamic image base (ASLR)' + sLineBreak +
            {#9'-auto [#[%]]|[.]'    +#9'Using '#$E2#$80#$9C'-keep -strip -trunc -stub'#$E2#$80#$9D' mode, replace' + sLineBreak +
                               #9#9#9#9'original file when stripped size is equal or' + sLineBreak +
                               #9#9#9#9'greater given number of bytes, percent or' + sLineBreak +
                               #9#9#9#9'volume cluster size' + sLineBreak +}
            #9'-deep' +          #9#9#9'Strip orphaned sections (unsafe!)' + sLineBreak +
            #9'-dep' +           #9#9#9'Enable data execution prevention (DEP)' + sLineBreak +
            #9'-dir' +           #9#9#9'Strip data directory (lesser compatibility)' + sLineBreak +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.' + sLineBreak +
            #9'-log <brief|detail>' +#9'Output log style' + sLineBreak +
            #9'-ls' +            #9#9#9'Display section list and image options' + sLineBreak +
            #9'-osver <#[.#]>' +   #9#9'Set required OS version to <#> or <#.#>' + sLineBreak +
            #9'-raw' +           #9#9#9'Don''t align raw data size values at section' + sLineBreak +
                               #9#9#9#9'headers to avoid antivirus false positives' + sLineBreak +
            #9'-rebase <[$]######[h]>'#9'Rebase image to <##########> decimal address,' + sLineBreak +
                               #9#9#9#9'to 2 power <##> when <##> in 1..31, or to' + sLineBreak +
                               #9#9#9#9'hexadecimal address <$########> or <########h>,' + sLineBreak +
            #9'-stub [stub]|[.]' +   #9'Replace stub with one from [stub] file or take' + sLineBreak +
                               #9#9#9#9'stub from PE Tool itself' + sLineBreak +
            #9'-touch' +         #9#9#9'Don''t keep original file timestamp' + sLineBreak +
            #9'-trunc' +         #9#9#9'When possible, don''t align last section,' + sLineBreak +
                               #9#9#9#9'truncate file immediately after data end' + sLineBreak +
            #9'-unsafe' +        #9#9#9'Strip chained certificate and other data' + sLineBreak + sLineBreak +
    'Miscellaneous options' + sLineBreak +
            #9'-nologo' +        #9#9#9'Don''t display logo and copyright' + sLineBreak +
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

  sCannotRebaseImage = 'Cannot rebase image without relocations';

  sPathEllipsis = '[...]';

  sOSVersionFmt         = '%17hs  %d.%d/%d.%d';
  sSubsystemFmt         = '  %hs';
  sImageOptionsFmt      = '%17hs  %s %s %s %s';
  sSectionAlignmentFmt  = '%17hs  %X/%X';

  sRequiredOSVersion  = 'OS Version';
  sSectionAlignment   = 'Section alignment';
  sImageBaseTitle     = 'Image base';
  sImageOptions       = 'Image options';

  sSectionFmt = '  %9hs  %s/%s';

  sStubSection        = '{stub}';
  sHeadersSection     = '{headers}';

  sNative         = 'Native';
  sGUI            = 'GUI';
  sConsole        = 'Console';
  sOS2            = 'OS/2';
  sPOSIX          = 'POSIX';
  s9xDrv          = '9x driver';
  sWindowsCE      = 'Windows CE';
  sEFIApp         = 'EFI application';
  sEFIBootDrv     = 'EFI boot driver';
  sEFIRuntimeDrv  = 'EFI runtime driver';
  sEFIROM         = 'EFI ROM';
  sXbox           = 'Xbox';
  sWindowsBootApp = 'Windows boot';

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
  sEstimated          = 'Estimated';
  sBackuping          = 'Backuping';
  sDestFile           = 'Destination file';
  sTotal              = 'Total';
  sRebasingTo         = 'Rebasing image to';

  DefaultMaxWidth = sChainedData; // max of {sLoading, sChainedData, sStrippng,
                                  //         sEstimating, sFixingStub}
  PercentageWidth = 6;

  sActionFmt = '%%%dhs  %%hs%%%ds';
  sStatsFmt = '  %%%di bytes  %%6hs';
                                                        
  sUnsafeOperation = '%hs, safe %hs stopped';
  sChainedDataFound = 'Chained data found';
  sNonStandardRebase = 'Rebase address is non-standard';
  sSafeStripping = 'stripping';
  sSafeRebasing = 'rebasing';

  sUnexpectedEndOfStream = '%hs from %s'; // Unexpected end of stream: read xx bytes instead of yy from <file>
  sSectionList = 'Headers and sections';
  sTotalsMessage = '%u files, %d bytes (%hs)';
  sNoFilesFound = 'No files found';
  sSectionNotFound = 'Section �%s� not found';
  sImageBaseUnaligned = 'Image base %08X is not on 64 KB boundary';
  sNoRelocationsForASLR = 'Dynamic image base feature (ASLR) is not available because relocations were' + sLineBreak +
    'stripped from this file';

implementation

end.
