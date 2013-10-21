(*
    PE Tool's string messages

    Copyright © 2012-2013 Vladislav Javadov (Freeman)
*)

unit PetConsts;

interface

{ Do not localize }

const
  CP_CORE = 1252;

  sInto     = 'into';
  sBackup   = 'backup';
  sStub     = 'stub';
  sExtract  = 'extract';
  sDump     = 'dump';

  sStrip    = 'strip';
  sRes      = 'res';
  sPause    = 'pause';

const
  sPressEnterToExit = 'Press ENTER to exit';
  sCat =
          #9'/\_/\'#10 +
    '   ____/ o o \'#10 +
    ' /~____ ==;==/'#10 +
    '(______)__m_m)';

  sDuplicateSourceFileName = 'Duplicate source file name: %s';

  sMissingFileName = 'Missing file name for “%s” key';
  sDuplicateFileKey = 'Duplicate “%s” key';
  sDuplicateFileName = 'Duplicate file name for “%s” key: %s';

  sDone = #9'done';
  sExtractingStub = 'Extracting stub: %ws (%d bytes)';

implementation

end.

