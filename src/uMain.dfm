object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'NFA (Nondeterministic Finite Automaton)'
  ClientHeight = 309
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnLoad: TButton
    Left = 416
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object btnExecute: TButton
    Left = 416
    Top = 168
    Width = 75
    Height = 25
    Caption = 'btnExecute'
    TabOrder = 1
  end
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 393
    Height = 293
    Lines.Strings = (
      'Memo')
    TabOrder = 2
  end
  object dlgOpen: TOpenDialog
    Left = 432
    Top = 72
  end
end
