(*
    PE Tool's string messages

    Copyright (c) 2012-2013 Vladislav Javadov (Freeman)
*)

unit PetConsts;

interface

{ Do not localize }

const
  CP_CORE = 1252; // we're using “”, but only in exception messages

  sInto     = 'into';
  sBackup   = 'backup';
  sStub     = 'stub';
  sExtract  = 'extract';
  sDump     = 'dump';

  sStrip    = 'strip';
  sRes      = 'res';
  sPause    = 'pause';
  sOSVer    = 'osver';
  s3GB      = '3GB';

{ Localize }

const
  sUsage  =
    'Usage:'#9'%ws <source> [commands [options]]' +                  #9#9#9#9'/\_/\'#10 +
                                                     #9#9#9#9#9#9#9#9'   ____/ o o \'#10 +
    'Commands' +                                       #9#9#9#9#9#9#9' /~____ ==t==/'#10 +
            #9'-backup <bak>'#9'Backup <source> file to <bak> file'#9'(_______)_m_m)'#10 +
            #9'-dump <dump>'#9'Dump <source> info to <dump> file'#10 +
            #9'-extract <xtub>'#9'Extract <source> stub to <xtub> file'#9'    PET :-)'#10 +
            #9'-into <dest>'#9'Save result to <dest> file'#10#10 +
    'Options for <dest>'#10 +
            #9'-3GB'#9#9'Set large address aware application flag'#10 +
            #9'-osver <#.#>'#9'Change required OS version to <#.#>'#10 +
            #9'-strip'#9#9'Strip headers, relocations, empty and orphaned sections'#10 +
            #9#9#9'when possible'#10 +
            #9'-stub <stub>'#9'Replace stub with one from <stub> file'#10#10 +
    'Other option'#10 +
            #9'-pause'#9#9'Wait for ENTER key to exit';

  sPressEnterToExit = 'Press ENTER to exit';

  sDuplicateSourceFileName = 'Duplicate source file name: %s';

  sMissingFileName = 'Missing file name for “%s” key';
  sDuplicateFileKey = 'Duplicate “%s” key';
  sDuplicateFileName = 'Duplicate file name for “%s” key: %s';

  sMissingOSVer = 'Missing OS version number';

  sDone = #9'done';
  sExtractingStub = 'Extracting stub'#9'%ws (%d bytes)';
  sWritingInto = 'Writing into'#9'%ws (%d bytes)';

implementation

end.

