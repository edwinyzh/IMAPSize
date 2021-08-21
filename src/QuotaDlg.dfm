object FQuotaDlg: TFQuotaDlg
  Left = 337
  Top = 223
  BorderStyle = bsDialog
  Caption = 'Storage Quota'
  ClientHeight = 131
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 335
    Height = 131
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 34
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object ProgressBar1: TALProgressBar
      Left = 32
      Top = 48
      Width = 273
      Height = 16
      BackgroundColor = clLime
      BarColor1 = clRed
      BarColorStyle = cs1Color
      BorderColor1 = clBackground
      Percentage = True
      PosTextSuffix = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
    end
    object Button1: TButton
      Left = 128
      Top = 88
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
