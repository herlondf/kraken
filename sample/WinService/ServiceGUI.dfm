object frmServiceGUI: TfrmServiceGUI
  Left = 0
  Top = 0
  Caption = 'frmServiceGUI'
  ClientHeight = 242
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnInstall: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnInstall'
    TabOrder = 0
    OnClick = btnInstallClick
  end
  object btnUninstall: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnUninstall'
    TabOrder = 1
    OnClick = btnUninstallClick
  end
  object btnStart: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btnStart'
    TabOrder = 2
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 89
    Top = 39
    Width = 75
    Height = 25
    Caption = 'btnStop'
    TabOrder = 3
    OnClick = btnStopClick
  end
end
