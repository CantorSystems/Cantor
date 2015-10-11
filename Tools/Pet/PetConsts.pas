(*
    PE Tool's strings and messages

    Copyright (c) 2012-2015 Vladislav Javadov (aka Freeman)
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
  sKeep       = #4'keep';
  sNoLogo     = #6'nologo';
  sPause      = #5'pause';
  sOSVer      = #5'osver';
  sStrip      = #5'strip';
  sTrunc      = #5'trunc';
  sUnsafe     = #6'unsafe';
  sVerbose    = #7'verbose';

  sLogo =
          #9'  /\_/\'#10 +
    '     ____/ o o \    %s %hs #ПокажиКота'#10 +
    '   /~____ ==t==/'#10 +
    '  (_______)_m_m)    %s'#10 +
    #10 +
    '      PET :-)';

{ Localize }

const
  sUsage =
    'Usage:'#9'%s <source> [commands [options]]';
  sHelp =
    'Commands'#10 +
            #9'-backup <bak>' +    #9#9'Backup <source> file to <bak> file'#10#10 +
            #9'-extract <stub>' +  #9#9'Extract <source> stub to <stub> file'#10 +
            #9'-into [dest]|[.]' +   #9'Save result to [dest] file or replace source'#10 +
            #9'-version' +         #9#9'Show version and exit'#10#10 +
    'Option for -extract and -into commands'#10 +
            #9'-strip' +         #9#9#9'Strip headers, relocations and empty sections'#10 +
                               #9#9#9#9'when possible'#10#10 +
    'Options for -into command'#10 +
            #9'-3GB' +           #9#9#9'Set large address aware application flag'#10 +
            #9'-auto [#[%]]|[.]'    +#9'Using '#$E2#$80#$9C'-keep -strip -trunc -stub'#$E2#$80#$9D' mode, replace'#10 +
                               #9#9#9#9'original file when stripped size is equal or'#10 +
                               #9#9#9#9'greater given number of bytes, percent or'#10 +
                               #9#9#9#9'volume cluster size'#10 +
            #9'-deep' +          #9#9#9'Strip orphaned sections too (unsafe!)'#10 +
            #9'-dropsect <s1,s2,...>'#9'Drop sections <s1>, <s2>, etc.'#10 +
            #9'-keep' +          #9#9#9'Keep original timestamp and attributes'#10 +
            #9'-osver <#[.#]>'+    #9#9'Set required OS version to <#> or <#.#>'#10 +
            #9'-stub [stub]|[.]' +   #9'Replace stub with one from [stub] file or take'#10 +
                               #9#9#9#9'stub from PE Tool itself'#10 +
            #9'-trunc' +         #9#9#9'Don''t align last section, truncate file'#10 +
                               #9#9#9#9'immediately after data end'#10 +
            #9'-unsafe' +        #9#9#9'Strip chained overlay or certificate data'#10 +
            #9'-verbose' +         #9#9'Output detailed stripping information'#10#10 +
    'Miscellaneous options'#10 +
            #9'-nologo' +        #9#9#9'Don''t display PET logo'#10 +
            #9'-pause' +         #9#9#9'Wait for ENTER key to exit';

  sMissingParam = 'Missing %s';
  sDuplicateParam = 'Duplicate %hs: %s';
  sFileName = '%ws file name'; // CoreChar
  sSource = 'source';
  sOSVersion = 'OS version number';
  sSectionNames = 'section names';
  sResourceNames = 'resource names';
  sLocaleMap = 'locale map';
  sImageBase = 'image base value';

  sCannotRebaseImage = 'Cannot rebase image: %s';

  FileNameWidth = 48;
  FileNameOffset = 12;

  sProcessing     = '%-16hs %-32@s %8u bytes';

  sRead = 'Read';
  sActual = 'Actual';
  sStripped = 'Stripped';
  sRatio = 'Ratio';

  HeaderFixedWidth = 18 + 12 * 3;
  DataFixedWidth = 15 + 12 * 3 + 6;

  sHeaderFmt = '%-18s%-12s%-12s%-12s';
  sDataFmt = #$E2#$80#$A2' %15s%-12d%-12d%-12d%-6s';

  sDoubleLine = #$E2#$95#$90;
  sSingleLine = #$E2#$94#$80;

  sPathEllipsis = '[...]';

  sLoading          = 'Loading';
  sOverlayData      = 'Overlay data';
  sImageData        = 'Image data';
  sExtractingStub   = 'Extracting stub';
  sFixingStub       = 'Fixing stub';
  sReplacingStub    = 'Replacing stub';
  sSaving           = 'Saving';
  sDroppingSection  = 'Dropping section';
  sStripping        = 'Stripping';
  sTotal            = 'Total';

  PromptMaxWidth = Length(sDroppingSection);

  sDefaultActionFmt = '%%%dhs  %%hs%%%ds';
  sDefaultStatsFmt = '  %%%du bytes  %%6hs';

  sBackuping      = 'Backuping'#9' %s';

  sOverlayDataFound = 'Overlay data found, safe stripping stopped';

implementation

uses
  CoreConsts;

initialization
  DefaultSystemCodePage := 1251; // #ПокажиКота

end.

