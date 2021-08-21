object FMsgPeeker: TFMsgPeeker
  Left = 205
  Top = 195
  Width = 740
  Height = 496
  Caption = 'MessagePeeker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 732
    Height = 39
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object LabelFrom: TLabel
      Left = 8
      Top = 4
      Width = 26
      Height = 13
      Caption = 'From:'
      Constraints.MaxWidth = 260
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelTo: TLabel
      Left = 8
      Top = 20
      Width = 16
      Height = 13
      Caption = 'To:'
      Constraints.MaxWidth = 260
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelDate: TLabel
      Left = 280
      Top = 4
      Width = 29
      Height = 13
      Caption = 'Date: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelSubject: TLabel
      Left = 280
      Top = 20
      Width = 42
      Height = 13
      Caption = 'Subject: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 431
    Width = 732
    Height = 19
    Panels = <
      item
        Width = 100
      end>
    SimplePanel = False
  end
  object PanelBodyParts: TPanel
    Left = 0
    Top = 39
    Width = 732
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object PanelBodyDisplay: TPanel
      Left = 0
      Top = 0
      Width = 732
      Height = 392
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object PageControl1: TPageControl
        Left = 2
        Top = 2
        Width = 728
        Height = 388
        ActivePage = TSAttachments
        Align = alClient
        TabOrder = 0
        OnChange = PageControl1Change
        object TSText: TTabSheet
          Caption = 'Text'
          object RichEditBody: TRichEdit
            Left = 0
            Top = 0
            Width = 720
            Height = 360
            Align = alClient
            PlainText = True
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
        object TSHtml: TTabSheet
          Caption = 'HTML'
          ImageIndex = 1
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 720
            Height = 379
            Align = alClient
            BevelInner = bvRaised
            BevelOuter = bvLowered
            TabOrder = 0
            object htmlLite1: ThtmlLite
              Left = 2
              Top = 2
              Width = 716
              Height = 375
              OnHotSpotClick = htmlLite1HotSpotClick
              TabOrder = 0
              Align = alClient
              BorderStyle = htFocused
              HistoryMaxCount = 0
              DefFontName = 'Times New Roman'
              DefPreFontName = 'Courier New'
              NoSelect = False
              CharSet = DEFAULT_CHARSET
              htOptions = []
            end
          end
        end
        object TSAttachments: TTabSheet
          Caption = 'Attachments'
          ImageIndex = 2
          object PanelParts: TPanel
            Left = 0
            Top = 0
            Width = 720
            Height = 360
            Align = alClient
            BevelInner = bvRaised
            BevelOuter = bvLowered
            TabOrder = 0
            object Panel5: TPanel
              Left = 2
              Top = 2
              Width = 716
              Height = 63
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Label1: TLabel
                Left = 8
                Top = 42
                Width = 335
                Height = 13
                Caption = 
                  'Hint: To save/view an attachment right click on it and select th' +
                  'e action'
              end
              object SpeedButton2: TSpeedButton
                Left = 8
                Top = 10
                Width = 23
                Height = 22
                Hint = 'Check all for deletion'
                Flat = True
                Glyph.Data = {
                  F6000000424DF600000000000000760000002800000010000000100000000100
                  04000000000080000000C30E0000C30E00001000000010000000000000000000
                  80000080000000808000800000008000800080800000C0C0C000808080000000
                  FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
                  7777777700000000000777770FFFFFFFFF0777770FFFFFFFFF0700000000FFFF
                  FF0707777770FFFFFF0707177170FFFFFF0707711770FFFFFF0707711770FFFF
                  FF0707177170FFFFFF0707777770FFFFFF0700000000FFF0000777770FFFFFF0
                  F07777770FFFFFF0077777770000000077777777777777777777}
                ParentShowHint = False
                ShowHint = True
                OnClick = SpeedButton2Click
              end
              object SpeedButton3: TSpeedButton
                Left = 32
                Top = 10
                Width = 23
                Height = 22
                Hint = 'Uncheck All'
                Flat = True
                Glyph.Data = {
                  36030000424D3603000000000000360000002800000010000000100000000100
                  18000000000000030000A0605900A06059000000000000000000C0C0C0C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000000000000000000000
                  0000000000000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0
                  C0C0C0C0C0C0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000C0C0C0000000000000
                  000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFF000000C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000
                  0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000C0C0C0000000C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFF000000C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000
                  0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000C0C0C0000000C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFF000000C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000
                  0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000C0C0C0000000C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFF000000C0C0C000000000000000000000000000000000000000000000
                  0000FFFFFFFFFFFFFFFFFF000000000000000000000000C0C0C0C0C0C0C0C0C0
                  C0C0C0C0C0C0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
                  FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFF000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                  C0C0C0C0C0C0000000000000000000000000000000000000000000000000C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                  C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0}
                ParentShowHint = False
                ShowHint = True
                OnClick = SpeedButton3Click
              end
              object SpeedButton4: TSpeedButton
                Left = 56
                Top = 10
                Width = 23
                Height = 22
                Hint = 'Toggle Simple/Advanced'
                AllowAllUp = True
                GroupIndex = 1
                Flat = True
                Glyph.Data = {
                  36080000424D3608000000000000360400002800000040000000100000000100
                  08000000000000040000C40E0000C40E00000001000000000000000000005500
                  0000AA000000FF0000000024000055240000AA240000FF240000004900005549
                  0000AA490000FF490000006D0000556D0000AA6D0000FF6D0000009200005592
                  0000AA920000FF92000000B6000055B60000AAB60000FFB6000000DB000055DB
                  0000AADB0000FFDB000000FF000055FF0000AAFF0000FFFF0000000024005500
                  2400AA002400FF0024000024240055242400AA242400FF242400004924005549
                  2400AA492400FF492400006D2400556D2400AA6D2400FF6D2400009224005592
                  2400AA922400FF92240000B6240055B62400AAB62400FFB6240000DB240055DB
                  2400AADB2400FFDB240000FF240055FF2400AAFF2400FFFF2400000049005500
                  4900AA004900FF0049000024490055244900AA244900FF244900004949005549
                  4900AA494900FF494900006D4900556D4900AA6D4900FF6D4900009249005592
                  4900AA924900FF92490000B6490055B64900AAB64900FFB6490000DB490055DB
                  4900AADB4900FFDB490000FF490055FF4900AAFF4900FFFF490000006D005500
                  6D00AA006D00FF006D0000246D0055246D00AA246D00FF246D0000496D005549
                  6D00AA496D00FF496D00006D6D00556D6D00AA6D6D00FF6D6D0000926D005592
                  6D00AA926D00FF926D0000B66D0055B66D00AAB66D00FFB66D0000DB6D0055DB
                  6D00AADB6D00FFDB6D0000FF6D0055FF6D00AAFF6D00FFFF6D00000092005500
                  9200AA009200FF0092000024920055249200AA249200FF249200004992005549
                  9200AA499200FF499200006D9200556D9200AA6D9200FF6D9200009292005592
                  9200AA929200FF92920000B6920055B69200AAB69200FFB6920000DB920055DB
                  9200AADB9200FFDB920000FF920055FF9200AAFF9200FFFF92000000B6005500
                  B600AA00B600FF00B6000024B6005524B600AA24B600FF24B6000049B6005549
                  B600AA49B600FF49B600006DB600556DB600AA6DB600FF6DB6000092B6005592
                  B600AA92B600FF92B60000B6B60055B6B600AAB6B600FFB6B60000DBB60055DB
                  B600AADBB600FFDBB60000FFB60055FFB600AAFFB600FFFFB6000000DB005500
                  DB00AA00DB00FF00DB000024DB005524DB00AA24DB00FF24DB000049DB005549
                  DB00AA49DB00FF49DB00006DDB00556DDB00AA6DDB00FF6DDB000092DB005592
                  DB00AA92DB00FF92DB0000B6DB0055B6DB00AAB6DB00FFB6DB0000DBDB0055DB
                  DB00AADBDB00FFDBDB0000FFDB0055FFDB00AAFFDB00FFFFDB000000FF005500
                  FF00AA00FF00FF00FF000024FF005524FF00AA24FF00FF24FF000049FF005549
                  FF00AA49FF00FF49FF00006DFF00556DFF00AA6DFF00FF6DFF000092FF005592
                  FF00AA92FF00FF92FF0000B6FF0055B6FF00AAB6FF00FFB6FF0000DBFF0055DB
                  FF00AADBFF00FFDBFF0000FFFF0055FFFF00AAFFFF00FFFFFF00B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6020202B6B6B6B6B6B6B6B6B6B6B6B600000000
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B602FF02B6000000B6B6B6B6B6B6B6B600FFFF00
                  B6B60000000000000000B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B60202B6B6B6B6B6B6B6B6B6B6B6B6B600FFFF00
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B600FF0000
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6000000B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6020202B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B602FF02B6000000B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B60202B6B6B6B6B6B6B6B600000000
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B600FFFF00
                  B6B60000000000000000B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B600FFFF00
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6020202B6B6B6B6B6B6B6B6B6B6B6B6B6B600FF0000
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B602FF02B6000000B6B6B6B6B6B6B6B6B6B6000000B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B60202B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6
                  B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6B6}
                NumGlyphs = 4
                ParentShowHint = False
                ShowHint = True
                OnClick = SpeedButton4Click
              end
            end
            object PartsTree: TVirtualStringTree
              Left = 2
              Top = 65
              Width = 716
              Height = 293
              Align = alClient
              BorderStyle = bsSingle
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Columns = <
                item
                  Alignment = taLeftJustify
                  ImageIndex = -1
                  Layout = blGlyphLeft
                  Position = 0
                  Width = 200
                  WideText = 'Name'
                end
                item
                  Alignment = taLeftJustify
                  ImageIndex = -1
                  Layout = blGlyphLeft
                  Position = 1
                  Width = 50
                  WideText = 'Size'
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
              ParentFont = False
              TabOrder = 1
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
              OnFreeNode = PartsTreeFreeNode
              OnGetText = PartsTreeGetText
              OnInitNode = PartsTreeInitNode
              OnMouseDown = PartsTreeMouseDown
              WideDefaultText = 'Node'
            end
          end
        end
        object TSHeader: TTabSheet
          Caption = 'Header'
          ImageIndex = 3
          object RichEditHeader: TRichEdit
            Left = 0
            Top = 0
            Width = 720
            Height = 360
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            PlainText = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            OnKeyDown = FormKeyDown
          end
        end
        object TSRaw: TTabSheet
          Caption = 'Raw'
          ImageIndex = 4
          object RichEditRaw: TRichEdit
            Left = 0
            Top = 0
            Width = 720
            Height = 360
            Align = alClient
            PlainText = True
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 640
  end
  object SaveDialog: TSaveDialog
    Left = 672
  end
  object PopupMenu1: TPopupMenu
    OwnerDraw = True
    Left = 598
    Top = 9
    object SaveAttachment1: TMenuItem
      Caption = '&Save Attachment...'
      OnClick = SaveAttachment1Click
    end
    object ViewAttachment1: TMenuItem
      Caption = '&View Attachment'
      OnClick = ViewAttachment1Click
    end
  end
  object SaveDialogAttach: TSaveDialog
    Title = 'Save Attachment As'
    Left = 672
    Top = 32
  end
  object MainMenu1: TMainMenu
    Images = FMain.ImageList6
    OwnerDraw = True
    Left = 528
    Top = 49
    object Manage1: TMenuItem
      Caption = '&Message'
      object MenuItemDeleteAttachments: TMenuItem
        Caption = '&Delete all attachments from the message'
        ImageIndex = 0
        OnClick = MenuItemDeleteAttachmentsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemSaveRemote: TMenuItem
        Caption = 'Save modified message to the &server'
        ImageIndex = 1
        OnClick = MenuItemSaveRemoteClick
      end
      object MenuItemSaveLocal: TMenuItem
        Caption = 'Save modified message &locally'
        ImageIndex = 2
        OnClick = MenuItemSaveLocalClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Closeviewer1: TMenuItem
        Caption = '&Close viewer'
        ShortCut = 27
        OnClick = Closeviewer1Click
      end
    end
  end
  object XPMenu1: TXPMenu
    Font.Charset = ANSI_CHARSET
    Font.Color = clMenuText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Color = clBtnFace
    IconBackColor = clBtnFace
    MenuBarColor = clBtnFace
    SelectColor = clHighlight
    SelectBorderColor = clHighlight
    SelectFontColor = clMenuText
    DisabledColor = clInactiveCaption
    SeparatorColor = clBtnFace
    CheckedColor = clHighlight
    IconWidth = 24
    DrawSelect = True
    UseSystemColors = True
    OverrideOwnerDraw = False
    Gradient = False
    FlatMenu = False
    AutoDetect = False
    Active = True
    Left = 440
    Top = 49
  end
end
