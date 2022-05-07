object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 495
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnAnimar: TButton
    Left = 167
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnAnimar'
    TabOrder = 0
    OnClick = btnAnimarClick
  end
  object btnReverter: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btnReverter'
    TabOrder = 1
    OnClick = btnReverterClick
  end
  object cmbModo: TComboBox
    Left = 16
    Top = 8
    Width = 145
    Height = 23
    TabOrder = 2
    Items.Strings = (
      'afLinear'
      'afQuadratic'
      'afCubic'
      'afQuartic'
      'afQuintic'
      'afBack')
  end
  object pnlAnimacao: TPanel
    Left = 16
    Top = 39
    Width = 185
    Height = 106
    Caption = 'pnlAnimacao'
    TabOrder = 3
  end
end
