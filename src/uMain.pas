unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uNFA, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnLoad: TButton;
    dlgOpen: TOpenDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FNFA: TNFA;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    FNFA.Load(dlgOpen.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FNFA := TNFA.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNFA);
end;

end.
