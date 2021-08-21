object FAddAccountDlg: TFAddAccountDlg
  Left = 659
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Add Account'
  ClientHeight = 260
  ClientWidth = 196
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 196
    Height = 225
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 42
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 18
      Top = 66
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label3: TLabel
      Left = 33
      Top = 90
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object Label4: TLabel
      Left = 45
      Top = 114
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label5: TLabel
      Left = 24
      Top = 18
      Width = 40
      Height = 13
      Caption = 'Account'
    end
    object Label6: TLabel
      Left = 10
      Top = 192
      Width = 62
      Height = 13
      Caption = 'Spam handle'
    end
    object Edit1: TEdit
      Left = 72
      Top = 40
      Width = 113
      Height = 21
      TabOrder = 1
    end
    object Edit2: TEdit
      Left = 72
      Top = 64
      Width = 49
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object Edit3: TEdit
      Left = 72
      Top = 88
      Width = 113
      Height = 21
      TabOrder = 4
    end
    object Edit4: TEdit
      Left = 72
      Top = 112
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '143'
    end
    object Edit5: TEdit
      Left = 72
      Top = 16
      Width = 113
      Height = 21
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 144
      Width = 169
      Height = 17
      Caption = 'Use Secure Connection (SSL)'
      TabOrder = 6
      OnClick = CheckBox1Click
    end
    object ComboBox1: TComboBox
      Left = 80
      Top = 192
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 168
      Width = 89
      Height = 17
      Caption = 'Smart INBOX'
      TabOrder = 7
    end
    object CBSavePassword: TCheckBox
      Left = 128
      Top = 66
      Width = 57
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Save it'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object Button1: TButton
    Left = 16
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
