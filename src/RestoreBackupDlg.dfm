object FRestoreBackupDlg: TFRestoreBackupDlg
  Left = 285
  Top = 182
  Width = 616
  Height = 480
  Caption = 'Restore Backup'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 608
    Height = 57
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 280
      Top = 32
      Width = 91
      Height = 13
      Caption = 'Restore to account'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 88
      Height = 13
      Caption = 'Restore backup of'
    end
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 94
      Height = 13
      Caption = 'Root backup folder:'
    end
    object LblRootFolder: TLabel
      Left = 112
      Top = 8
      Width = 3
      Height = 13
    end
    object CBAccountDest: TComboBox
      Left = 384
      Top = 28
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = CBAccountDestChange
    end
    object CBAccountNameSrc: TComboBox
      Left = 112
      Top = 28
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = CBAccountNameSrcChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 400
    Width = 608
    Height = 53
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Button3: TButton
      Left = 384
      Top = 16
      Width = 99
      Height = 25
      Caption = 'Restore'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 488
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Cancel / Close'
      ModalResult = 2
      TabOrder = 1
    end
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Select All'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Deselect All'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object FolderTree: TVirtualStringTree
    Left = 0
    Top = 57
    Width = 608
    Height = 343
    Align = alClient
    BorderStyle = bsSingle
    Header.AutoSizeIndex = 0
    Header.Columns = <
      item
        Alignment = taLeftJustify
        ImageIndex = -1
        Layout = blGlyphLeft
        Position = 0
        Width = 200
        WideText = 'Remote Folder'
      end
      item
        Alignment = taLeftJustify
        ImageIndex = -1
        Layout = blGlyphLeft
        Position = 1
        Width = 150
        WideText = 'Relative Local Folder'
      end
      item
        Alignment = taLeftJustify
        ImageIndex = -1
        Layout = blGlyphLeft
        Position = 2
        Width = 100
        WideText = 'Last Backup'
      end
      item
        Alignment = taLeftJustify
        ImageIndex = -1
        Layout = blGlyphLeft
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
        Position = 3
        Width = 150
        WideText = 'New Remote Name'
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
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnFreeNode = FolderTreeFreeNode
    OnGetText = FolderTreeGetText
    OnInitNode = FolderTreeInitNode
    OnMouseDown = FolderTreeMouseDown
    WideDefaultText = 'Node'
  end
  object PopupMenu1: TPopupMenu
    Left = 472
    Top = 144
    object Renamedestinationfolder1: TMenuItem
      Caption = 'Change New Remote Folder Name'
      OnClick = Renamedestinationfolder1Click
    end
  end
end
