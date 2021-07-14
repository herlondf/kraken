object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 458
  ClientWidth = 873
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 16
    Caption = 'Consulta'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 229
    Width = 35
    Height = 16
    Caption = 'Sa'#237'da'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object memConsulta: TMemo
    Left = 8
    Top = 30
    Width = 625
    Height = 193
    TabOrder = 0
  end
  object memSaida: TMemo
    Left = 8
    Top = 251
    Width = 625
    Height = 193
    TabOrder = 1
  end
  object Button1: TButton
    Left = 639
    Top = 30
    Width = 122
    Height = 25
    Caption = 'OPEN RequestHTTP'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 639
    Top = 61
    Width = 122
    Height = 25
    Caption = 'EXECSQL RequestHTTP'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 639
    Top = 92
    Width = 122
    Height = 25
    Caption = 'OPEN+Start Request'
    TabOrder = 4
    OnClick = Button3Click
  end
end
