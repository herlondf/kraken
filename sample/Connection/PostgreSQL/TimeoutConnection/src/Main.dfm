object frmSampleTimeOut: TfrmSampleTimeOut
  Left = 0
  Top = 0
  Caption = 'frmSampleTimeOut'
  ClientHeight = 243
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblHost: TLabel
    Left = 10
    Top = 32
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object lblPort: TLabel
    Left = 8
    Top = 59
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object lblDatabase: TLabel
    Left = 8
    Top = 86
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object lblUser: TLabel
    Left = 8
    Top = 113
    Width = 22
    Height = 13
    Caption = 'User'
  end
  object lblPassword: TLabel
    Left = 8
    Top = 140
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object lblTimeout: TLabel
    Left = 8
    Top = 167
    Width = 38
    Height = 13
    Caption = 'Timeout'
  end
  object edtHost: TEdit
    Left = 80
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtPort: TEdit
    Left = 80
    Top = 51
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 1
  end
  object edtDatabase: TEdit
    Left = 80
    Top = 78
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edtUser: TEdit
    Left = 80
    Top = 105
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object edtPassword: TEdit
    Left = 80
    Top = 132
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object edtTimeout: TEdit
    Left = 80
    Top = 159
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 5
  end
  object btnConnect: TButton
    Left = 80
    Top = 200
    Width = 121
    Height = 25
    Caption = 'Connect'
    TabOrder = 6
    OnClick = btnConnectClick
  end
  object mmoQuery: TMemo
    Left = 223
    Top = 24
    Width = 226
    Height = 156
    Lines.Strings = (
      'mmoQuery')
    TabOrder = 7
  end
  object btnOpen: TButton
    Left = 374
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 8
    OnClick = btnOpenClick
  end
  object btnExecSQL: TButton
    Left = 293
    Top = 200
    Width = 75
    Height = 25
    Caption = 'ExecSQL'
    TabOrder = 9
    OnClick = btnExecSQLClick
  end
end
