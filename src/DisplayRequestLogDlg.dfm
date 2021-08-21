object FDisplayRequestLogDlg: TFDisplayRequestLogDlg
  Left = 276
  Top = 264
  BorderStyle = bsDialog
  Caption = 'Request Details'
  ClientHeight = 306
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 267
    Width = 587
    Height = 39
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Button1: TButton
      Left = 268
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 587
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object CBErrorsOnly: TCheckBox
      Left = 8
      Top = 8
      Width = 193
      Height = 17
      Caption = 'Show only errors and warnings'
      TabOrder = 0
      OnClick = CBErrorsOnlyClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 33
    Width = 587
    Height = 234
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 583
      Height = 230
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
end
