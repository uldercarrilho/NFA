program NFA;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  uNFA in 'uNFA.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
