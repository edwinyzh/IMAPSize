object FAddSpamHandleDlg: TFAddSpamHandleDlg
  Left = 617
  Top = 302
  BorderStyle = bsDialog
  Caption = 'Add Spam Handle'
  ClientHeight = 246
  ClientWidth = 184
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 184
    Height = 209
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 63
      Height = 13
      Caption = 'Spam header'
    end
    object Label3: TLabel
      Left = 8
      Top = 176
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object Label10: TLabel
      Left = 8
      Top = 16
      Width = 63
      Height = 13
      Caption = 'Handle name'
    end
    object Edit2: TEdit
      Left = 80
      Top = 46
      Width = 89
      Height = 21
      TabOrder = 1
    end
    object Edit3: TEdit
      Left = 40
      Top = 170
      Width = 129
      Height = 21
      TabOrder = 3
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 80
      Width = 161
      Height = 73
      Caption = 'Spam Recognized by '
      Items.Strings = (
        'Value greater than'
        'String in spam header')
      TabOrder = 2
    end
    object Edit1: TEdit
      Left = 80
      Top = 16
      Width = 89
      Height = 21
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 16
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
