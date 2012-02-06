unit Model;

interface

uses
  Core, Containers, Storage;

type
  TStatement = object(TObject)
  public
    procedure Execute; virtual; abstract;
//    procedure Read(S: PStream); virtual; abstract;
//    procedure Write(S: PStream); virtual; abstract;
  end;

  TStatementList = object(TObjectList)
  end;

  PFork = ^TFork;
  TFork = object(TStatement)
  end;

  PSwitch = ^TSwitch;
  TSwitch = object(TStatement)
  end;

implementation

end.
