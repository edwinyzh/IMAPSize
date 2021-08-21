object FOptions: TFOptions
  Left = 426
  Top = 215
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 334
  ClientWidth = 357
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
  object PanelPodloga: TPanel
    Left = 0
    Top = 0
    Width = 357
    Height = 334
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 293
      Width = 357
      Height = 41
      Align = alBottom
      TabOrder = 0
      object Label18: TLabel
        Left = 8
        Top = 12
        Width = 116
        Height = 13
        Caption = '* Change requires restart'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Button1: TButton
        Left = 192
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 272
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        OnClick = Button2Click
      end
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 357
      Height = 293
      ActivePage = TabSheet7
      Align = alClient
      MultiLine = True
      TabOrder = 1
      object TabSheet3: TTabSheet
        Caption = 'Accounts'
        ImageIndex = 2
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 349
          Height = 247
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object ListBoxAccounts: TListBox
            Left = 8
            Top = 8
            Width = 137
            Height = 185
            ItemHeight = 13
            TabOrder = 0
            OnClick = ListBoxAccountsClick
          end
          object Panel3: TPanel
            Left = 152
            Top = 8
            Width = 193
            Height = 233
            BevelInner = bvRaised
            BevelOuter = bvLowered
            TabOrder = 1
            object Label5: TLabel
              Left = 8
              Top = 34
              Width = 48
              Height = 13
              Caption = 'Username'
            end
            object Label6: TLabel
              Left = 10
              Top = 58
              Width = 46
              Height = 13
              Caption = 'Password'
            end
            object Label7: TLabel
              Left = 25
              Top = 82
              Width = 31
              Height = 13
              Caption = 'Server'
            end
            object Label8: TLabel
              Left = 109
              Top = 108
              Width = 19
              Height = 13
              Caption = 'Port'
            end
            object Label9: TLabel
              Left = 16
              Top = 10
              Width = 40
              Height = 13
              Caption = 'Account'
            end
            object Label4: TLabel
              Left = 8
              Top = 202
              Width = 62
              Height = 13
              Caption = 'Spam handle'
            end
            object Label17: TLabel
              Left = 8
              Top = 152
              Width = 55
              Height = 13
              Caption = 'Root Folder'
            end
            object Bevel1: TBevel
              Left = 72
              Top = 160
              Width = 113
              Height = 2
            end
            object Bevel2: TBevel
              Left = 8
              Top = 192
              Width = 177
              Height = 2
            end
            object EditAcctUser: TEdit
              Left = 64
              Top = 32
              Width = 121
              Height = 21
              TabOrder = 1
            end
            object EditAcctPass: TEdit
              Left = 64
              Top = 56
              Width = 57
              Height = 21
              PasswordChar = '*'
              TabOrder = 2
            end
            object EditAcctServer: TEdit
              Left = 64
              Top = 80
              Width = 121
              Height = 21
              TabOrder = 4
            end
            object EditAcctPort: TEdit
              Left = 136
              Top = 104
              Width = 33
              Height = 21
              TabOrder = 5
              Text = '143'
            end
            object EditAcctName: TEdit
              Left = 64
              Top = 8
              Width = 121
              Height = 21
              TabOrder = 0
            end
            object CBSSL: TCheckBox
              Left = 8
              Top = 128
              Width = 153
              Height = 17
              Caption = 'Secure Connection (SSL)'
              TabOrder = 6
              OnClick = CBSSLClick
            end
            object ComboBoxAcctSpamHandle: TComboBox
              Left = 80
              Top = 200
              Width = 97
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 7
            end
            object SmartInboxCB: TCheckBox
              Left = 8
              Top = 108
              Width = 89
              Height = 17
              Caption = 'Smart INBOX'
              TabOrder = 8
            end
            object CBSavePassword: TCheckBox
              Left = 128
              Top = 56
              Width = 57
              Height = 17
              Alignment = taLeftJustify
              Caption = 'Save it'
              TabOrder = 3
            end
            object EditRootFolder: TEdit
              Left = 8
              Top = 168
              Width = 57
              Height = 21
              TabOrder = 9
            end
            object CBExcludeRoot: TCheckBox
              Left = 72
              Top = 168
              Width = 105
              Height = 17
              Caption = 'Exclude from tree'
              TabOrder = 10
            end
          end
          object ButtonRemoveAcct: TButton
            Left = 88
            Top = 200
            Width = 57
            Height = 25
            Caption = 'Remove'
            TabOrder = 2
            OnClick = ButtonRemoveAcctClick
          end
          object ButtonAddAcct: TButton
            Left = 8
            Top = 200
            Width = 73
            Height = 25
            Caption = 'New Account'
            ParentShowHint = False
            ShowHint = False
            TabOrder = 3
            OnClick = ButtonAddAcctClick
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Spam Handles'
        ImageIndex = 1
        object GroupBox2: TGroupBox
          Left = 160
          Top = 8
          Width = 185
          Height = 209
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
            Top = 184
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
          object EditSpamHeader: TEdit
            Left = 80
            Top = 46
            Width = 89
            Height = 21
            TabOrder = 1
          end
          object EditCompareValue: TEdit
            Left = 40
            Top = 178
            Width = 129
            Height = 21
            TabOrder = 3
          end
          object RadioGroupSpamRecognizedBy: TRadioGroup
            Left = 8
            Top = 88
            Width = 161
            Height = 73
            Caption = 'Spam Recognized by '
            Items.Strings = (
              'Value greater than'
              'String in spam header')
            TabOrder = 2
          end
          object EditHandleName: TEdit
            Left = 80
            Top = 16
            Width = 89
            Height = 21
            TabOrder = 0
          end
        end
        object ListBoxSpamHandles: TListBox
          Left = 8
          Top = 8
          Width = 137
          Height = 177
          ItemHeight = 13
          TabOrder = 3
          OnClick = ListBoxSpamHandlesClick
        end
        object ButtonAddSH: TButton
          Left = 8
          Top = 192
          Width = 73
          Height = 25
          Caption = 'New Handle'
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          OnClick = ButtonAddSHClick
        end
        object ButtonRemoveSH: TButton
          Left = 88
          Top = 192
          Width = 57
          Height = 25
          Caption = 'Remove'
          TabOrder = 2
          OnClick = ButtonRemoveSHClick
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Mailbox Tree'
        object GroupBox1: TGroupBox
          Left = 8
          Top = 8
          Width = 257
          Height = 65
          Caption = 'Define color areas (% of parent) '
          TabOrder = 0
          object Image2: TImage
            Left = 116
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              000000000000000080000080000000808000800000008000800080800000C0C0
              C000C0DCC000F0CAA60004040400080808000C0C0C0011111100161616001C1C
              1C002222220029292900555555004D4D4D004242420039393900807CFF005050
              FF009300D600FFECCC00C6D6EF00D6E7E70090A9AD0000003300000066000000
              99000000CC00003300000033330000336600003399000033CC000033FF000066
              00000066330000666600006699000066CC000066FF0000990000009933000099
              6600009999000099CC000099FF0000CC000000CC330000CC660000CC990000CC
              CC0000CCFF0000FF660000FF990000FFCC003300000033003300330066003300
              99003300CC003300FF00333300003333330033336600333399003333CC002500
              D800336600003366330033666600336699003366CC003366FF00339900003399
              330033996600339999003399CC003399FF0033CC000033CC330033CC660033CC
              990033CCCC0033CCFF0033FF330033FF660033FF990033FFCC0033FFFF006600
              00006600330066006600660099006600CC006600FF0066330000663333006633
              6600663399006633CC006633FF00666600006666330066666600666699006666
              CC00669900006699330066996600669999006699CC006699FF0066CC000066CC
              330066CC990066CCCC0066CCFF0066FF000066FF330066FF990066FFCC00CC00
              FF00FF00CC009999000099339900990099009900CC0099000000993333009900
              66009933CC009900FF00996600009966330099336600996699009966CC009933
              FF009999330099996600999999009999CC009999FF0099CC000099CC330066CC
              660099CC990099CCCC0099CCFF0099FF000099FF330099CC660099FF990099FF
              CC0099FFFF00CC00000099003300CC006600CC009900CC00CC0099330000CC33
              3300CC336600CC339900CC33CC00CC33FF00CC660000CC66330099666600CC66
              9900CC66CC009966FF00CC990000CC993300CC996600CC999900CC99CC00CC99
              FF00CCCC0000CCCC3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF
              330099FF6600CCFF9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC33
              0000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF663300CC66
              6600FF669900FF66CC00CC66FF00FF990000FF993300FF996600FF999900FF99
              CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF
              3300CCFF6600FFFF9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66
              FF00FFFF66002100A5005F5F5F00777777008686860096969600CBCBCB00B2B2
              B200D7D7D700DDDDDD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0
              A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF0000000000000000000000000000FF4C4C4C4C4C4C4C4C4C4C4C4C4C4C
              00FF4C7A32323232323232323232324C00FF4C7A32323232323232323232324C
              00FF4C7A32323232323232323232324C00FF4C7A32323232323232323232324C
              00FF4C7A32323232323232323232324C00FF4C7A32323232323232323232324C
              00FF4C7A32323232323232323232324C00FF4C7A7A7A7A7A7A7A7A7A7A7A324C
              00FF4C4C4C4C4C4C4C4C4C4C4C4C4C4CFFFFFF4C7A7A7A7A7A4CFFFFFFFFFFFF
              FFFFFFFF4C4C4C4C4CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object Image3: TImage
            Left = 216
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              000000000000000080000080000000808000800000008000800080800000C0C0
              C000C0DCC000F0CAA60004040400080808000C0C0C0011111100161616001C1C
              1C002222220029292900555555004D4D4D004242420039393900807CFF005050
              FF009300D600FFECCC00C6D6EF00D6E7E70090A9AD0000003300000066000000
              99000000CC00003300000033330000336600003399000033CC000033FF000066
              00000066330000666600006699000066CC000066FF0000990000009933000099
              6600009999000099CC000099FF0000CC000000CC330000CC660000CC990000CC
              CC0000CCFF0000FF660000FF990000FFCC003300000033003300330066003300
              99003300CC003300FF00333300003333330033336600333399003333CC002500
              D800336600003366330033666600336699003366CC003366FF00339900003399
              330033996600339999003399CC003399FF0033CC000033CC330033CC660033CC
              990033CCCC0033CCFF0033FF330033FF660033FF990033FFCC0033FFFF006600
              00006600330066006600660099006600CC006600FF0066330000663333006633
              6600663399006633CC006633FF00666600006666330066666600666699006666
              CC00669900006699330066996600669999006699CC006699FF0066CC000066CC
              330066CC990066CCCC0066CCFF0066FF000066FF330066FF990066FFCC00CC00
              FF00FF00CC009999000099339900990099009900CC0099000000993333009900
              66009933CC009900FF00996600009966330099336600996699009966CC009933
              FF009999330099996600999999009999CC009999FF0099CC000099CC330066CC
              660099CC990099CCCC0099CCFF0099FF000099FF330099CC660099FF990099FF
              CC0099FFFF00CC00000099003300CC006600CC009900CC00CC0099330000CC33
              3300CC336600CC339900CC33CC00CC33FF00CC660000CC66330099666600CC66
              9900CC66CC009966FF00CC990000CC993300CC996600CC999900CC99CC00CC99
              FF00CCCC0000CCCC3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF
              330099FF6600CCFF9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC33
              0000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF663300CC66
              6600FF669900FF66CC00CC66FF00FF990000FF993300FF996600FF999900FF99
              CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF
              3300CCFF6600FFFF9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66
              FF00FFFF66002100A5005F5F5F00777777008686860096969600CBCBCB00B2B2
              B200D7D7D700DDDDDD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0
              A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF0000000000000000000000000000FF0120202020202020202020202020
              00FF0116F9F9F9F9F9F9F9F9F9F9F92000FF0116F9F9F9F9F9F9F9F9F9F9F920
              00FF0116F9F9F9F9F9F9F9F9F9F9F92000FF0116F9F9F9F9F9F9F9F9F9F9F920
              00FF0116F9F9F9F9F9F9F9F9F9F9F92000FF0116F9F9F9F9F9F9F9F9F9F9F920
              00FF0116F9F9F9F9F9F9F9F9F9F9F92000FF011616161616161616161616F920
              00FF0101010101010101010101010101FFFFFF01161616161601FFFFFFFFFFFF
              FFFFFFFF0101010101FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object Image1: TImage
            Left = 16
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              000000000000000080000080000000808000800000008000800080800000C0C0
              C000C0DCC000F0CAA60004040400080808000C0C0C0011111100161616001C1C
              1C002222220029292900555555004D4D4D004242420039393900807CFF005050
              FF009300D600FFECCC00C6D6EF00D6E7E70090A9AD0000003300000066000000
              99000000CC00003300000033330000336600003399000033CC000033FF000066
              00000066330000666600006699000066CC000066FF0000990000009933000099
              6600009999000099CC000099FF0000CC000000CC330000CC660000CC990000CC
              CC0000CCFF0000FF660000FF990000FFCC003300000033003300330066003300
              99003300CC003300FF00333300003333330033336600333399003333CC003333
              FF00336600003366330033666600336699003366CC003366FF00339900003399
              330033996600339999003399CC003399FF0033CC000033CC330033CC660033CC
              990033CCCC0033CCFF0033FF330033FF660033FF990033FFCC0033FFFF006600
              00006600330066006600660099006600CC006600FF0066330000663333006633
              6600663399006633CC006633FF00666600006666330066666600666699006666
              CC00669900006699330066996600669999006699CC006699FF0066CC000066CC
              330066CC990066CCCC0066CCFF0066FF000066FF330066FF990066FFCC00CC00
              FF00FF00CC009999000099339900990099009900CC0099000000993333009900
              66009933CC009900FF00996600009966330099336600996699009966CC009933
              FF009999330099996600999999009999CC009999FF0099CC000099CC330066CC
              660099CC990099CCCC0099CCFF0099FF000099FF330099CC660099FF990099FF
              CC0099FFFF00CC00000099003300CC006600CC009900CC00CC0099330000CC33
              3300CC336600CC339900CC33CC00CC33FF00CC660000CC66330099666600CC66
              9900CC66CC009966FF00CC990000CC993300CC996600CC999900CC99CC00CC99
              FF00CCCC0000CCCC3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF
              330099FF6600CCFF9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC33
              0000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF663300CC66
              6600FF669900FF66CC00CC66FF00FF990000FF993300FF996600FF999900FF99
              CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF
              3300CCFF6600FFFF9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66
              FF00FFFF66002100A5005F5F5F00777777008686860096969600CBCBCB00B2B2
              B200D7D7D700DDDDDD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0
              A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF0000000000000000000000000000FF3079797979797979797979797979
              00FF30C3A0A0A0A0A0A0A0A0A0A0A07900FF30C3A0A0A0A0A0A0A0A0A0A0A079
              00FF30C3A0A0A0A0A0A0A0A0A0A0A07900FF30C3A0A0A0A0A0A0A0A0A0A0A079
              00FF30C3A0A0A0A0A0A0A0A0A0A0A07900FF30C3A0A0A0A0A0A0A0A0A0A0A079
              00FF30C3A0A0A0A0A0A0A0A0A0A0A07900FF30C3C3C3C3C3C3C3C3C3C3C3A079
              00FF3079797979797979303030303030FFFFFF30F4C3C3A0A030FFFFFFFFFFFF
              FFFFFFFF3030303030FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object SpinEdit1: TSpinEdit
            Left = 48
            Top = 26
            Width = 49
            Height = 22
            MaxValue = 99
            MinValue = 0
            TabOrder = 0
            Value = 10
            OnChange = SpinEdit1Change
          end
          object SpinEdit2: TSpinEdit
            Left = 152
            Top = 26
            Width = 49
            Height = 22
            MaxValue = 100
            MinValue = 1
            TabOrder = 1
            Value = 25
            OnChange = SpinEdit1Change
          end
        end
        object GroupBox3: TGroupBox
          Left = 8
          Top = 80
          Width = 209
          Height = 81
          Caption = 'Few children coloring '
          TabOrder = 1
          object Label2: TLabel
            Left = 112
            Top = 54
            Width = 37
            Height = 13
            Caption = 'children'
          end
          object Label16: TLabel
            Left = 8
            Top = 54
            Width = 42
            Height = 13
            Caption = 'less than'
          end
          object CheckBox2: TCheckBox
            Left = 8
            Top = 24
            Width = 185
            Height = 17
            Caption = 'Leave children yellow if parent has'
            TabOrder = 0
            OnClick = CheckBox2Click
          end
          object SpinEdit3: TSpinEdit
            Left = 56
            Top = 48
            Width = 49
            Height = 22
            MaxValue = 10
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnChange = SpinEdit1Change
          end
        end
        object RadioGroup1: TRadioGroup
          Left = 224
          Top = 80
          Width = 113
          Height = 81
          Caption = 'Size display '
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Byte'
            'KB'
            'MB'
            'Smart')
          TabOrder = 2
          OnClick = RadioGroup1Click
        end
        object CBUpdateOnSizeCheck: TCheckBox
          Left = 8
          Top = 176
          Width = 249
          Height = 17
          Caption = 'Update on every mailbox when checking size'
          TabOrder = 3
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Message List'
        ImageIndex = 3
        object GroupBox4: TGroupBox
          Left = 8
          Top = 8
          Width = 257
          Height = 57
          Caption = 'Define size ranges for icon coloring (KB) '
          TabOrder = 0
          object Image4: TImage
            Left = 16
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              000000000000706C700000FFFF00A8A0A8000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000071FFFF0000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000000000000000000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFF0101010101010101FFFFFFFFFFFFFF111111111111111101FF
              FFFFFFFFFFFF111111111111111101FFFFFFFFFFFFFF110303030303031101FF
              FFFFFFFFFFFF111111111111111101FFFFFFFFFFFFFF111111111111111101FF
              FFFFFFFFFFFF110303030303031101FFFFFFFFFFFFFF111111111111111101FF
              FFFFFFFFFFFF111111111111111101FFFFFFFFFFFFFF110303030303031101FF
              FFFFFFFFFFFF111111111111111101FFFFFFFFFFFFFF1111111111111111FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object Image5: TImage
            Left = 116
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              00000000000040004000FF00FF0068376800FF5CFF00397EFF000051FF000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000000000000000000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFF0303030303030303FFFFFFFFFFFFFF050505050505050503FF
              FFFFFFFFFFFF050505050505050503FFFFFFFFFFFFFF050606060606060503FF
              FFFFFFFFFFFF050505050505050503FFFFFFFFFFFFFF050505050505050503FF
              FFFFFFFFFFFF050606060606060503FFFFFFFFFFFFFF050505050505050503FF
              FFFFFFFFFFFF050505050505050503FFFFFFFFFFFFFF050606060606060503FF
              FFFFFFFFFFFF050505050505050503FFFFFFFFFFFFFF0505050505050505FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object Image6: TImage
            Left = 216
            Top = 28
            Width = 16
            Height = 16
            AutoSize = True
            Picture.Data = {
              07544269746D617036050000424D360500000000000036040000280000001000
              0000100000000100080000000000000100000000000000000000000100000000
              000000000000000050000000FF00000078003737FF000000B300000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000000000000000000FFFF
              FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFF0101010101010101FFFFFFFFFFFFFF040404040404040401FF
              FFFFFFFFFFFF040404040404040401FFFFFFFFFFFFFF040505050505050401FF
              FFFFFFFFFFFF040404040404040401FFFFFFFFFFFFFF040404040404040401FF
              FFFFFFFFFFFF040505050505050401FFFFFFFFFFFFFF040404040404040401FF
              FFFFFFFFFFFF040404040404040401FFFFFFFFFFFFFF040505050505050401FF
              FFFFFFFFFFFF040404040404040401FFFFFFFFFFFFFF0404040404040404FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF}
          end
          object SpinEdit4: TSpinEdit
            Left = 48
            Top = 26
            Width = 57
            Height = 22
            Increment = 10
            MaxValue = 10000
            MinValue = 1
            TabOrder = 0
            Value = 100
            OnChange = SpinEdit4Change
          end
          object SpinEdit5: TSpinEdit
            Left = 144
            Top = 26
            Width = 57
            Height = 22
            Increment = 100
            MaxValue = 10000
            MinValue = 1
            TabOrder = 1
            Value = 1000
            OnChange = SpinEdit4Change
          end
        end
        object RadioGroup2: TRadioGroup
          Left = 8
          Top = 72
          Width = 129
          Height = 73
          Caption = 'Size display '
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Byte'
            'KB')
          TabOrder = 1
          OnClick = RadioGroup2Click
        end
        object GroupBox5: TGroupBox
          Left = 144
          Top = 72
          Width = 121
          Height = 73
          Caption = 'Message colors '
          TabOrder = 2
          object Label11: TLabel
            Left = 16
            Top = 48
            Width = 27
            Height = 13
            Caption = 'Spam'
          end
          object Label12: TLabel
            Left = 16
            Top = 24
            Width = 38
            Height = 13
            Caption = 'Flagged'
          end
          object STColorsCombo1: TSTColorsCombo
            Left = 60
            Top = 22
            Width = 45
            Height = 22
            ColorHeight = 16
            ColorSelected = clGreen
            DisplayNames = False
            SystemColors = False
            Items.Strings = (
              'clBlack'
              'clMaroon'
              'clGreen'
              'clOlive'
              'clNavy'
              'clPurple'
              'clTeal'
              'clGray'
              'clSilver'
              'clRed'
              'clLime'
              'clYellow'
              'clBlue'
              'clFuchsia'
              'clAqua'
              'clWhite')
            TabOrder = 0
          end
          object STColorsCombo2: TSTColorsCombo
            Left = 60
            Top = 46
            Width = 45
            Height = 22
            ColorHeight = 16
            ColorSelected = clRed
            DisplayNames = False
            SystemColors = False
            Items.Strings = (
              'clBlack'
              'clMaroon'
              'clGreen'
              'clOlive'
              'clNavy'
              'clPurple'
              'clTeal'
              'clGray'
              'clSilver'
              'clRed'
              'clLime'
              'clYellow'
              'clBlue'
              'clFuchsia'
              'clAqua'
              'clWhite')
            TabOrder = 1
          end
        end
        object RGAddressStructure: TRadioGroup
          Left = 8
          Top = 152
          Width = 153
          Height = 73
          Caption = 'Address Structure '
          ItemIndex = 0
          Items.Strings = (
            'Name and email address'
            'Name only (if possible)'
            'Address only')
          TabOrder = 3
          OnClick = SpinEdit4Change
        end
        object CBAnsweredItalics: TCheckBox
          Left = 168
          Top = 160
          Width = 169
          Height = 17
          Caption = 'Answered messages in Italics'
          TabOrder = 4
          OnClick = CBAnsweredItalicsClick
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Message Viewer'
        ImageIndex = 4
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 349
          Height = 247
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label13: TLabel
            Left = 34
            Top = 42
            Width = 123
            Height = 13
            Caption = 'Warning threshold (bytes):'
          end
          object CBMsgPeekerLimitSize: TCheckBox
            Left = 16
            Top = 16
            Width = 289
            Height = 17
            Caption = 'Warn when fetching large body parts (recommended)'
            TabOrder = 0
            OnClick = CBMsgPeekerLimitSizeClick
          end
          object SESizeLimit: TSpinEdit
            Left = 168
            Top = 38
            Width = 65
            Height = 22
            Increment = 1000
            MaxValue = 500000
            MinValue = 0
            TabOrder = 1
            Value = 10000
          end
          object CBEnableHeaderEdit: TCheckBox
            Left = 16
            Top = 96
            Width = 217
            Height = 17
            Caption = 'Enable header editing (experts only!)'
            TabOrder = 2
          end
          object CBAutoCloseMsgPeeker: TCheckBox
            Left = 16
            Top = 72
            Width = 297
            Height = 17
            Caption = 'Auto-close Message Viewer when append is invoked'
            TabOrder = 3
          end
          object Button4: TButton
            Left = 16
            Top = 124
            Width = 75
            Height = 25
            Caption = '&Font...'
            TabOrder = 4
            OnClick = Button4Click
          end
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Logging'
        ImageIndex = 5
        object Label14: TLabel
          Left = 152
          Top = 20
          Width = 96
          Height = 13
          Caption = 'Max logfile size (MB)'
        end
        object Label15: TLabel
          Left = 152
          Top = 44
          Width = 98
          Height = 13
          Caption = 'Backup logs to keep'
        end
        object RGIMAPTrafficLog: TRadioGroup
          Left = 32
          Top = 8
          Width = 97
          Height = 121
          Caption = 'IMAP Traffic * '
          ItemIndex = 1
          Items.Strings = (
            'Off'
            'Basic'
            'Detailed')
          TabOrder = 0
          OnClick = RGIMAPTrafficLogClick
        end
        object SEMaxLogSize: TSpinEdit
          Left = 264
          Top = 16
          Width = 41
          Height = 22
          MaxValue = 50
          MinValue = 1
          TabOrder = 1
          Value = 5
          OnChange = RGIMAPTrafficLogClick
        end
        object SEFilesToKeep: TSpinEdit
          Left = 264
          Top = 40
          Width = 41
          Height = 22
          MaxValue = 10
          MinValue = 0
          TabOrder = 2
          Value = 3
          OnChange = RGIMAPTrafficLogClick
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Misc'
        ImageIndex = 6
        object LblServerTimeout: TLabel
          Left = 240
          Top = 16
          Width = 89
          Height = 13
          Caption = 'Server timeout (s) *'
        end
        object SpeedButton1: TSpeedButton
          Left = 280
          Top = 144
          Width = 23
          Height = 22
          Caption = '...'
          OnClick = SpeedButton1Click
        end
        object Label19: TLabel
          Left = 8
          Top = 128
          Width = 111
          Height = 13
          Caption = 'Root Backup Directory:'
        end
        object Label20: TLabel
          Left = 8
          Top = 176
          Width = 117
          Height = 13
          Caption = 'Backup Filename Format'
        end
        object Label21: TLabel
          Left = 8
          Top = 216
          Width = 303
          Height = 13
          Caption = 'Can use: %FROM %SUBJECT %DATE %MSGNUM %CURTIME'
        end
        object Label22: TLabel
          Left = 8
          Top = 232
          Width = 282
          Height = 13
          Caption = 'Please include %MSGNUM and %CURTIME for uniqueness'
        end
        object CBEnableCache: TCheckBox
          Left = 8
          Top = 40
          Width = 105
          Height = 17
          Caption = 'Enable caching'
          TabOrder = 0
        end
        object Button3: TButton
          Left = 120
          Top = 40
          Width = 81
          Height = 21
          Caption = 'Clear Cache'
          TabOrder = 1
          OnClick = Button3Click
        end
        object CBXPMenus: TCheckBox
          Left = 8
          Top = 16
          Width = 121
          Height = 17
          Caption = 'Use XP style menus'
          TabOrder = 2
        end
        object SpinEditServerTimeout: TSpinEdit
          Left = 240
          Top = 36
          Width = 41
          Height = 22
          MaxValue = 60
          MinValue = 1
          TabOrder = 3
          Value = 5
        end
        object CBMsgDelAddFilenamesToBody: TCheckBox
          Left = 8
          Top = 64
          Width = 281
          Height = 17
          Caption = 'Add names of deleted attachments to message body'
          TabOrder = 4
        end
        object CBLoadFoldersStartup: TCheckBox
          Left = 8
          Top = 88
          Width = 257
          Height = 17
          Caption = 'Load folders of the default account on startup'
          TabOrder = 5
        end
        object EdBackupDir: TEdit
          Left = 8
          Top = 144
          Width = 265
          Height = 21
          ReadOnly = True
          TabOrder = 6
        end
        object EditBackupFilenameFormat: TEdit
          Left = 8
          Top = 192
          Width = 265
          Height = 21
          TabOrder = 7
          Text = '%SUBJECT_%CURTIME_%MSGNUM.eml'
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
    Left = 140
    Top = 298
  end
  object DirDialog1: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 316
    Top = 74
  end
end
