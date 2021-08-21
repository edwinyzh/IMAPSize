object FGlobalSearch: TFGlobalSearch
  Left = 187
  Top = 234
  BorderStyle = bsDialog
  Caption = 'Global Search'
  ClientHeight = 375
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 263
    Height = 375
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Query'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 255
        Height = 347
        Align = alClient
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Panel3: TPanel
          Left = 2
          Top = 2
          Width = 251
          Height = 272
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object CheckListBox1: TCheckListBox
            Left = 0
            Top = 62
            Width = 251
            Height = 210
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
          end
          object Panel4: TPanel
            Left = 0
            Top = 21
            Width = 251
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Button1: TButton
              Left = 8
              Top = 8
              Width = 75
              Height = 25
              Caption = 'Select All'
              TabOrder = 0
              OnClick = Button1Click
            end
            object Button2: TButton
              Left = 88
              Top = 8
              Width = 75
              Height = 25
              Caption = 'Deselect All'
              TabOrder = 1
              OnClick = Button2Click
            end
            object Button3: TButton
              Left = 168
              Top = 8
              Width = 75
              Height = 25
              Caption = 'Refresh List'
              TabOrder = 2
              OnClick = Button3Click
            end
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 251
            Height = 21
            Align = alTop
            BevelOuter = bvNone
            Caption = 'Folders to search:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
          end
        end
        object Panel6: TPanel
          Left = 2
          Top = 274
          Width = 251
          Height = 71
          Align = alBottom
          BevelInner = bvRaised
          BevelOuter = bvLowered
          TabOrder = 1
          object Label2: TLabel
            Left = 8
            Top = 10
            Width = 52
            Height = 13
            Caption = 'Search for '
          end
          object Bevel1: TBevel
            Left = 8
            Top = 32
            Width = 235
            Height = 2
          end
          object EditSearchString: TEdit
            Left = 64
            Top = 8
            Width = 97
            Height = 21
            Hint = 'Search string (text to find or IMAP SEARCH command)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object BitBtn1: TBitBtn
            Left = 168
            Top = 8
            Width = 75
            Height = 22
            Caption = 'Search'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = BitBtn1Click
            Glyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              0800000000000001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A60004040400080808000C0C0C0011111100161616001C1C1C00222222002929
              2900555555004D4D4D004242420039393900807CFF005050FF009300D600FFEC
              CC00C6D6EF00D6E7E70090A9AD000000330000006600000099000000CC000033
              00000033330000336600003399000033CC000033FF0000660000006633000066
              6600006699000066CC000066FF00009900000099330000996600009999000099
              CC000099FF0000CC000000CC330000CC660000CC990000CCCC0000CCFF0000FF
              660000FF990000FFCC00330000003300330033006600330099003300CC003300
              FF00333300003333330033336600333399003333CC003333FF00336600003366
              330033666600336699003366CC003366FF003399000033993300339966003399
              99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
              FF0033FF330033FF660033FF990033FFCC0033FFFF0066000000660033006600
              6600660099006600CC006600FF00663300006633330066336600663399006633
              CC006633FF00666600006666330066666600666699006666CC00669900006699
              330066996600669999006699CC006699FF0066CC000066CC330066CC990066CC
              CC0066CCFF0066FF000066FF330066FF990066FFCC00CC00FF00FF00CC009999
              000099339900990099009900CC009900000099333300990066009933CC009900
              FF00996600009966330099336600996699009966CC009933FF00999933009999
              6600999999009999CC009999FF0099CC000099CC330066CC660099CC990099CC
              CC0099CCFF0099FF000099FF330099CC660099FF990099FFCC0099FFFF00CC00
              000099003300CC006600CC009900CC00CC0099330000CC333300CC336600CC33
              9900CC33CC00CC33FF00CC660000CC66330099666600CC669900CC66CC009966
              FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
              3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF330099FF6600CCFF
              9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC330000FF333300FF33
              6600FF339900FF33CC00FF33FF00FF660000FF663300CC666600FF669900FF66
              CC00CC66FF00FF990000FF993300FF996600FF999900FF99CC00FF99FF00FFCC
              0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF3300CCFF6600FFFF
              9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66FF00FFFF66002100
              A5005F5F5F00777777008686860096969600CBCBCB00B2B2B200D7D7D700DDDD
              DD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFC8C8FFFFFFFFFFFFFFFFFFFFFFFFFFC8FED35FFFFFFFFFFFEB13
              131313130FFED35F1313FFFFFFFFEBBCBCBCBCC8FED35FEDBC13FFFFFFFFEBF6
              F6F6C8FED35FED1BBC13FFFFFFFFEB1B1BC8D3D35FEAEDF6BC13FFFFEB130F10
              EDF6EA5FED1B1BF6BC13FFEBE207E2E210ED5FEAEDEDEDF6BC13EBE2F607E2ED
              ED0FED1B1B1B1BF6BC13EBE2F207E2DFDF0FEAEDEDEDEDF6BC13EBE2F607E2ED
              ED0FED1B1B1B1B1BBC13EBE2F207E2DFDF0FEAEDEDF61BBCBC13FFEBE207E2E2
              0FEDBC1B1BF613131313FFFFEBEB0F10EDBC1BF6F6F6EDF6BCEAFFFFFFFFEB1B
              1BF6F6F6F6F6EDBCEAFFFFFFFFFFEBEBEBEBEBEBEBEBEBEAFFFF}
          end
          object BitBtn2: TBitBtn
            Left = 56
            Top = 40
            Width = 137
            Height = 25
            Caption = 'Advanced Search'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = BitBtn2Click
            Glyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              0800000000000001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A60004040400080808000C0C0C0011111100161616001C1C1C00222222002929
              2900555555004D4D4D004242420039393900807CFF005050FF009300D600FFEC
              CC00C6D6EF00D6E7E70090A9AD000000330000006600000099000000CC000033
              00000033330000336600003399000033CC000033FF0000660000006633000066
              6600006699000066CC000066FF00009900000099330000996600009999000099
              CC000099FF0000CC000000CC330000CC660000CC990000CCCC0000CCFF0000FF
              660000FF990000FFCC00330000003300330033006600330099003300CC003300
              FF00333300003333330033336600333399003333CC003333FF00336600003366
              330033666600336699003366CC003366FF003399000033993300339966003399
              99003399CC003399FF0033CC000033CC330033CC660033CC990033CCCC0033CC
              FF0033FF330033FF660033FF990033FFCC0033FFFF0066000000660033006600
              6600660099006600CC006600FF00663300006633330066336600663399006633
              CC006633FF00666600006666330066666600666699006666CC00669900006699
              330066996600669999006699CC006699FF0066CC000066CC330066CC990066CC
              CC0066CCFF0066FF000066FF330066FF990066FFCC00CC00FF00FF00CC009999
              000099339900990099009900CC009900000099333300990066009933CC009900
              FF00996600009966330099336600996699009966CC009933FF00999933009999
              6600999999009999CC009999FF0099CC000099CC330066CC660099CC990099CC
              CC0099CCFF0099FF000099FF330099CC660099FF990099FFCC0099FFFF00CC00
              000099003300CC006600CC009900CC00CC0099330000CC333300CC336600CC33
              9900CC33CC00CC33FF00CC660000CC66330099666600CC669900CC66CC009966
              FF00CC990000CC993300CC996600CC999900CC99CC00CC99FF00CCCC0000CCCC
              3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF330099FF6600CCFF
              9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC330000FF333300FF33
              6600FF339900FF33CC00FF33FF00FF660000FF663300CC666600FF669900FF66
              CC00CC66FF00FF990000FF993300FF996600FF999900FF99CC00FF99FF00FFCC
              0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF3300CCFF6600FFFF
              9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66FF00FFFF66002100
              A5005F5F5F00777777008686860096969600CBCBCB00B2B2B200D7D7D700DDDD
              DD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFC8C8FFFFFFFFFFFFFFFFFFFFFFFFFFC8FED35FFFFFFFFFFFEB13
              131313130FFED35F1313FFFFFFFFEBBCBCBCBCC8FED35FEDBC13FFFFFFFFEBF6
              F6F6C8FED35FED1BBC13FFFFFFFFEB1B1BC8D3D35FEAEDF6BC13FFFFEB130F10
              EDF6EA5FED1B1BF6BC13FFEBE207E2E210ED5FEAEDEDEDF6BC13EBE2F607E2ED
              ED0FED001B1B1BF6BC13EBE2F207E2DFDF0FEA030300EDF6BC13EBE2F607E2ED
              ED0FED03FB03001BBC13EBE2F207E2DFDF0FEAED03FB0300BC13FFEBE207E2E2
              0FEDBC1B1B03FB030013FFFFEBEB0F10EDBC1BF6F6F603FB0300FFFFFFFFEB1B
              1BF6F6F6F6F6ED03F901FFFFFFFFEBEBEBEBEBEBEBEBEBEA0101}
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Results'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 255
        Height = 347
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ListView1: TListView
          Left = 0
          Top = 0
          Width = 255
          Height = 328
          Align = alClient
          Columns = <
            item
              Caption = 'Mailbox'
              Width = 150
            end
            item
              Caption = 'Found'
            end>
          ColumnClick = False
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = ListView1DblClick
        end
        object StatusBar1: TStatusBar
          Left = 0
          Top = 328
          Width = 255
          Height = 19
          Panels = <>
          SimplePanel = True
        end
      end
    end
  end
end
