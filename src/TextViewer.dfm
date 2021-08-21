object FTextViewer: TFTextViewer
  Left = 206
  Top = 161
  Width = 665
  Height = 410
  Caption = 'Text Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 657
    Height = 383
    Align = alClient
    TabOrder = 0
    object TabText: TTabSheet
      Caption = 'TabText'
      TabVisible = False
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 649
        Height = 373
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabHTML: TTabSheet
      Caption = 'TabHTML'
      ImageIndex = 1
      TabVisible = False
      object htmlLite1: ThtmlLite
        Left = 0
        Top = 0
        Width = 649
        Height = 373
        ViewImages = False
        TabOrder = 0
        Align = alClient
        DefBackground = clWindow
        BorderStyle = htFocused
        HistoryMaxCount = 0
        DefFontName = 'Times New Roman'
        DefPreFontName = 'Courier New'
        VisitedMaxCount = 0
        ImageCacheCount = 0
        NoSelect = False
        CharSet = DEFAULT_CHARSET
        htOptions = []
      end
    end
  end
end
