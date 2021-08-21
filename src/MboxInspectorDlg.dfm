object FMboxInspectorDlg: TFMboxInspectorDlg
  Left = 286
  Top = 208
  Width = 696
  Height = 480
  Caption = 'mbox Inspector'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 48
      Height = 13
      Caption = 'Mbox File:'
    end
    object SpeedButton1: TSpeedButton
      Left = 408
      Top = 8
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object EditMboxFile: TEdit
      Left = 64
      Top = 8
      Width = 345
      Height = 21
      TabOrder = 0
    end
    object Button2: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Inspect'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 392
    Width = 688
    Height = 61
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 78
      Height = 13
      Caption = 'Total Messages:'
    end
    object LblMsgCount: TLabel
      Left = 96
      Top = 16
      Width = 3
      Height = 13
    end
    object Button1: TButton
      Left = 600
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
    end
    object StatusBar1: TStatusBar
      Left = 2
      Top = 40
      Width = 684
      Height = 19
      Panels = <>
      SimplePanel = False
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 688
    Height = 351
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 2
      Top = 153
      Width = 684
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object MessageList: TVirtualStringTree
      Left = 2
      Top = 2
      Width = 684
      Height = 151
      Align = alTop
      BorderStyle = bsSingle
      Header.AutoSizeIndex = 0
      Header.Columns = <
        item
          Alignment = taLeftJustify
          ImageIndex = -1
          Layout = blGlyphLeft
          Position = 0
          Width = 50
          WideText = 'Line'
        end
        item
          Alignment = taLeftJustify
          ImageIndex = -1
          Layout = blGlyphLeft
          Position = 1
          Width = 250
          WideText = 'From Line'
        end
        item
          Alignment = taLeftJustify
          ImageIndex = -1
          Layout = blGlyphLeft
          Position = 2
          Width = 250
          WideText = 'Subject'
        end>
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoVisible]
      Header.Style = hsThickButtons
      HintAnimation = hatNone
      HintMode = hmDefault
      IncrementalSearchDirection = sdForward
      TabOrder = 0
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
      OnFreeNode = MessageListFreeNode
      OnGetText = MessageListGetText
      OnInitNode = MessageListInitNode
      WideDefaultText = 'Node'
    end
    object Memo1: TMemo
      Left = 2
      Top = 156
      Width = 684
      Height = 193
      Align = alClient
      TabOrder = 1
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mbox'
    Title = 'Specify a mbox file to inspect'
    Left = 576
    Top = 8
  end
end
