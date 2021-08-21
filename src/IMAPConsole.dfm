object FIMAPConsole: TFIMAPConsole
  Left = 193
  Top = 151
  Width = 563
  Height = 442
  Caption = 'IMAP Console'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 57
    Width = 555
    Height = 358
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 551
      Height = 335
      Align = alClient
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object StatusBar1: TStatusBar
      Left = 2
      Top = 337
      Width = 551
      Height = 19
      Panels = <
        item
          Text = 'Account:'
          Width = 120
        end
        item
          Text = 'Server:'
          Width = 120
        end
        item
          Text = 'Port:'
          Width = 60
        end
        item
          Text = 'SSL:'
          Width = 60
        end
        item
          Text = 'Status:'
          Width = 50
        end>
      SimplePanel = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 33
    Width = 555
    Height = 24
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 416
      Top = 2
      Width = 21
      Height = 21
      Flat = True
    end
    object Edit1: TEdit
      Left = 2
      Top = 2
      Width = 409
      Height = 21
      TabOrder = 0
    end
  end
end
