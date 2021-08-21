object FAdvSearchDlg: TFAdvSearchDlg
  Left = 520
  Top = 266
  Width = 486
  Height = 333
  Caption = 'Advanced Search'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 105
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object ButtonSearch: TButton
      Left = 392
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Search'
      Default = True
      TabOrder = 0
      OnClick = ButtonSearchClick
    end
    object ButtonClear: TButton
      Left = 392
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = ButtonClearClick
    end
    object RadioButtonOR: TRadioButton
      Left = 200
      Top = 72
      Width = 177
      Height = 17
      Caption = 'Match ANY of the following (OR)'
      TabOrder = 2
    end
    object RadioButtonAND: TRadioButton
      Left = 8
      Top = 72
      Width = 185
      Height = 17
      Caption = 'Match ALL of the following (AND)'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 369
      Height = 49
      Caption = 'Search Set '
      TabOrder = 4
      object CBSearchSets: TComboBox
        Left = 8
        Top = 16
        Width = 169
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = CBSearchSetsChange
      end
      object ButtonAddSearch: TButton
        Left = 192
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Add'
        TabOrder = 1
        OnClick = ButtonAddSearchClick
      end
      object ButtonDelSearch: TButton
        Left = 272
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Delete'
        TabOrder = 2
        OnClick = ButtonDelSearchClick
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 267
    Width = 478
    Height = 39
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object ButtonMore: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'More'
      TabOrder = 0
      OnClick = ButtonMoreClick
    end
    object ButtonLess: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Less'
      Enabled = False
      TabOrder = 1
      OnClick = ButtonLessClick
    end
    object ButtonClose: TButton
      Left = 392
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 2
      OnClick = ButtonCloseClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 105
    Width = 478
    Height = 162
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 2
      Width = 474
      Height = 158
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
      inline Frm0: TSearchKeyFrame
        Left = 5
        Top = 10
      end
    end
  end
end
