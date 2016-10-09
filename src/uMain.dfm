object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'NFA (Nondeterministic Finite Automata)'
  ClientHeight = 374
  ClientWidth = 473
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
  object lblAutomataDefinition: TLabel
    Left = 8
    Top = 8
    Width = 98
    Height = 13
    Caption = 'Automata definition:'
  end
  object lblInput: TLabel
    Left = 8
    Top = 54
    Width = 30
    Height = 13
    Caption = 'Input:'
  end
  object lblResults: TLabel
    Left = 8
    Top = 100
    Width = 39
    Height = 13
    Caption = 'Results:'
  end
  object btnLoad: TButton
    Left = 391
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object btnExecute: TButton
    Left = 391
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Execute'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = btnExecuteClick
  end
  object mmoResults: TMemo
    Left = 8
    Top = 119
    Width = 377
    Height = 247
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object edtAutomataDefinition: TEdit
    Left = 8
    Top = 27
    Width = 377
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object edtInput: TEdit
    Left = 8
    Top = 73
    Width = 377
    Height = 21
    Enabled = False
    TabOrder = 2
  end
  object dlgOpen: TOpenDialog
    Left = 400
    Top = 120
  end
end
