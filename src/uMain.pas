unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uNFA, Vcl.StdCtrls;

type
  TfrmMain = class(TForm)
    btnLoad: TButton;
    dlgOpen: TOpenDialog;
    btnExecute: TButton;
    mmoResults: TMemo;
    lblAutomataDefinition: TLabel;
    edtAutomataDefinition: TEdit;
    lblInput: TLabel;
    edtInput: TEdit;
    lblResults: TLabel;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  private
    FAutomata: TAutomata;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnExecuteClick(Sender: TObject);
begin
  if FAutomata.Execute(edtInput.Text) then
    mmoResults.Lines.Add('[Accept] ' + edtInput.Text)
  else
    mmoResults.Lines.Add('[Reject] ' + edtInput.Text);

  edtInput.SetFocus;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  if not dlgOpen.Execute then
    Exit;

  FAutomata.Load(dlgOpen.FileName);

  edtAutomataDefinition.Text := dlgOpen.FileName;
  edtInput.Enabled := True;
  edtInput.Text := '';
  edtInput.SetFocus;
  btnExecute.Enabled := True;
  mmoResults.Lines.Clear;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FAutomata := TAutomata.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAutomata);
end;

end.
