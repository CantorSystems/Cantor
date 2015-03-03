(*
    Sample application with text encodings using CoreLite and VCL

    Conditional defines (in Project Options -> Directories/Conditionals):
      * CoreLiteVCL -- in order to compile CoreLite for VCL application
      * Debug -- for Delphi IDE friendly exceptions

    Search path:  ..\CoreLite
*)

program Encodings;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
