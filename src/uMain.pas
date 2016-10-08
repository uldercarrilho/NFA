unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uNFA, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnLoad: TButton;
    dlgOpen: TOpenDialog;
    btnExecute: TButton;
    Memo: TMemo;
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

uses
  System.IniFiles;

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  IniFile: TIniFile;
  Inputs: TStringList;
  i: Integer;
begin
  if not dlgOpen.Execute then
    Exit;

  FNFA.Load(dlgOpen.FileName);

  Inputs := TStringList.Create;
  IniFile := TIniFile.Create(dlgOpen.FileName);
  try
    IniFile.ReadSectionValues('Strings', Inputs);

    for i := 0 to Inputs.Count - 1 do
    begin
      // remove the key of indetifier. Only value is important.
      Inputs.Strings[i] := Copy(Inputs.Strings[i], Pos('=', Inputs.Strings[i]) + 1, MaxInt);
      if FNFA.Run(Inputs.Strings[i]) then
        Memo.Lines.Add('[ACEITA] ' + Inputs.Strings[i])
      else
        Memo.Lines.Add('[REJEITA] ' + Inputs.Strings[i]);
    end;
  finally
    FreeAndNil(IniFile);
  end;
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
