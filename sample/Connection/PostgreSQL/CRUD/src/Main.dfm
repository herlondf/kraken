object frmSampleTimeOut: TfrmSampleTimeOut
  Left = 0
  Top = 0
  Caption = 'frmSampleTimeOut'
  ClientHeight = 308
  ClientWidth = 831
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
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
  object lblParam: TLabel
    Left = 458
    Top = 24
    Width = 30
    Height = 13
    Caption = 'Param'
  end
  object lblParamValue: TLabel
    Left = 585
    Top = 24
    Width = 59
    Height = 13
    Caption = 'Param Value'
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
    TabOrder = 4
  end
  object edtDatabase: TEdit
    Left = 80
    Top = 78
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object edtUser: TEdit
    Left = 80
    Top = 105
    Width = 121
    Height = 21
    TabOrder = 9
  end
  object edtPassword: TEdit
    Left = 80
    Top = 132
    Width = 121
    Height = 21
    TabOrder = 10
  end
  object edtTimeout: TEdit
    Left = 80
    Top = 159
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 11
  end
  object btnConnect: TButton
    Left = 80
    Top = 200
    Width = 121
    Height = 25
    Caption = 'Connect'
    TabOrder = 12
  end
  object mmoQuery: TMemo
    Left = 223
    Top = 24
    Width = 226
    Height = 156
    TabOrder = 1
  end
  object btnOpen: TButton
    Left = 374
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 14
  end
  object btnExecSQL: TButton
    Left = 293
    Top = 200
    Width = 75
    Height = 25
    Caption = 'ExecSQL'
    TabOrder = 13
    OnClick = btnExecSQLClick
  end
  object edtParam: TEdit
    Left = 455
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edtParamValue: TEdit
    Left = 582
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object lstParams: TListBox
    Left = 455
    Top = 70
    Width = 248
    Height = 110
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 5
  end
  object btnParamAdd: TButton
    Left = 709
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 6
  end
  object btnParamRemove: TButton
    Left = 709
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Rem'
    TabOrder = 8
  end
end
