object FPasswordDlg: TFPasswordDlg
  Left = 204
  Top = 222
  BorderStyle = bsDialog
  Caption = 'Password'
  ClientHeight = 143
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 102
    Width = 240
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Button1: TButton
      Left = 40
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 120
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 240
    Height = 102
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 185
      Height = 13
      Caption = 'Please enter the password for account:'
    end
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object CBSavePassword: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Save Password'
      TabOrder = 0
    end
    object EditPassword: TEdit
      Left = 56
      Top = 48
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
  end
end
