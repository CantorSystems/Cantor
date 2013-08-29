(*
    LiteConv string constants and messages

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

unit ConvConsts;

interface

{ Do not localize }

const
  CP_CORE = 1252;

  sInto     = 'into';
  sCodePage = 'cp';
  sCharSet  = 'cs';
  sPause    = 'pause';

{ Localize }

const
  sUsage  = 'Usage:'#9'%ws source-file [<charset>] [-into dest-file [<charset>]] [-pause]'#10#10 +
            'Common options'#10 +
                    #9'-into dest-file'#9'Write result to the dest-file'#10 +
                    #9'-pause'#9#9'Wait for ENTER key to exit'#10#10 +
            'Character set options for <charset>'#10 +
                    #9'-cp N'#9'Use code page N to decode/encode file'#10 +
                  //  #9'-cs S'#9'Use character set S to decode/encode file'#10 +
                    #9'-oem'#9'Use OEM code page to decode/encode file'#10 +
                    #9'-8'#9'Encode to CESU-8'#10 +
                    #9'-16'#9'Encode to UTF-16'#10 +
                    #9'-32'#9'Encode to UTF-32'#10 +
                    #9'-BE'#9'Encode UTF-16 or UTF-32 as big-endian'#10#10 +
            'Current environment'#10 +
                    #9'ANSI code page (ACP)'#9'%u'#10+
                    #9'OEM code page (OEMCP)'#9'%u';

  sEnvCodePage = '%u (%u bytes per character)';

  sWarning = 'Warning';
  sNothingToSave = 'Nothing to save';

  sInvalidCodePage = 'Invalid code page number “%s”';
  sMissingCodePage = 'Missing code page number';

  sPressEnterToExit = 'Press ENTER to exit';

implementation

end.



