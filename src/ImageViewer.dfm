object FImageViewer: TFImageViewer
  Left = 295
  Top = 267
  Width = 427
  Height = 366
  Caption = 'Image Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 419
    Height = 339
    Align = alClient
    TabOrder = 0
    object TabJPG: TTabSheet
      Caption = 'TabJPG'
      TabVisible = False
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 411
        Height = 329
        Align = alClient
        TabOrder = 0
        object Image1: TImage
          Left = 0
          Top = 0
          Width = 407
          Height = 307
          AutoSize = True
        end
      end
    end
    object TabGIF: TTabSheet
      Caption = 'TabGIF'
      ImageIndex = 1
      TabVisible = False
      object ScrollBox2: TScrollBox
        Left = 0
        Top = 0
        Width = 411
        Height = 329
        Align = alClient
        TabOrder = 0
        object GIFImage1: TGIFImage
          Left = 0
          Top = 0
          Width = 407
          Height = 307
          Animate = False
          AutoSize = True
          Center = False
          FirstImageOnly = False
          Graphic = GIFImage1
          Loop = False
          Opaque = False
          Stretch = False
          StretchRatio = False
          StretchBigOnly = False
          Tile = False
          Threaded = False
        end
      end
    end
  end
end
