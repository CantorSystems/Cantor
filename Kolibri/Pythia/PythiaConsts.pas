(*
    Pythia -- Delphi to FASM preprocessor for Kolibri OS

    String messages

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit PythiaConsts;

interface

const
  sVersion  = '0.0';
  sTitle    = 'Pythia %s  Copyright (c) 2013 Vladislav Javadov';
  sUsage    = 'Usage:'#9'%ws int-file -into dest-file [-map map-file] [-src source-file]'#10 +
                      #9'[-pause]';

  sInto  = 'into';
  sMap   = 'map';
  sSrc   = 'src';
  sPause = 'pause';

  KeysTotal = Length(sInto) + Length(sMap) + Length(sSrc) + Length(sPause);

  sPressEnterToExit = 'Press ENTER to exit';

implementation

end.

