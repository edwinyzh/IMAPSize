object FSaveAttachmentsDlg: TFSaveAttachmentsDlg
  Left = 298
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Save Attachments'
  ClientHeight = 635
  ClientWidth = 674
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
    Top = 594
    Width = 674
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object BtnSaveAttachments: TButton
      Left = 416
      Top = 8
      Width = 161
      Height = 25
      Caption = 'Save Selected Attachments'
      TabOrder = 0
      OnClick = BtnSaveAttachmentsClick
    end
    object Button2: TButton
      Left = 584
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 674
    Height = 594
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object AttachTree: TVirtualStringTree
      Left = 0
      Top = 73
      Width = 674
      Height = 330
      Align = alClient
      BorderStyle = bsSingle
      CheckImageKind = ckDarkTick
      Header.AutoSizeIndex = 0
      Header.Columns = <
        item
          Alignment = taLeftJustify
          ImageIndex = -1
          Layout = blGlyphLeft
          Position = 0
          Width = 600
          WideText = 'Attachment'
        end
        item
          Alignment = taLeftJustify
          ImageIndex = -1
          Layout = blGlyphLeft
          Position = 1
          Width = 65
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
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      OnChecked = AttachTreeChecked
      OnFreeNode = AttachTreeFreeNode
      OnGetText = AttachTreeGetText
      OnInitNode = AttachTreeInitNode
      WideDefaultText = 'Node'
    end
    object Panel3: TPanel
      Left = 0
      Top = 403
      Width = 674
      Height = 191
      Align = alBottom
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 1
      object Label2: TLabel
        Left = 184
        Top = 32
        Width = 130
        Height = 13
        Caption = 'Save attachments to folder '
      end
      object SpeedButton1: TSpeedButton
        Left = 520
        Top = 28
        Width = 23
        Height = 22
        Hint = 'Browse for Folder'
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButton1Click
      end
      object Label3: TLabel
        Left = 400
        Top = 8
        Width = 84
        Height = 13
        Caption = 'Size to download:'
      end
      object LabelSize: TLabel
        Left = 496
        Top = 8
        Width = 46
        Height = 13
        Caption = 'LabelSize'
      end
      object Button3: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Select All'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Deselect All'
        TabOrder = 1
        OnClick = Button4Click
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 56
        Width = 537
        Height = 65
        Caption = 'Selection Fine Tuning '
        TabOrder = 2
        object CBFineTuneCompare: TComboBox
          Left = 136
          Top = 24
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object EditFineTune: TEdit
          Left = 296
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 1
        end
        object CBFineTuneType: TComboBox
          Left = 8
          Top = 24
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = CBFineTuneTypeChange
          Items.Strings = (
            'Attachment Type'
            'Extension'
            'Size'
            'Name')
        end
        object Button5: TButton
          Left = 376
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Select'
          TabOrder = 3
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 456
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Deselect'
          TabOrder = 4
          OnClick = Button6Click
        end
      end
      object EditDir: TEdit
        Left = 320
        Top = 28
        Width = 193
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 128
        Width = 537
        Height = 49
        Caption = 'Add to attachment filename '
        TabOrder = 4
        object CBPrefixSender: TCheckBox
          Left = 280
          Top = 24
          Width = 225
          Height = 17
          Caption = 'Prefix with message sender and timestamp'
          TabOrder = 0
        end
        object CBPrefixTime: TCheckBox
          Left = 8
          Top = 24
          Width = 257
          Height = 17
          Caption = 'Prefix with current time (for uniqueness)'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 674
      Height = 73
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 2
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 544
        Height = 26
        Caption = 
          'This dialog enables you to select the attachments you want to sa' +
          've locally. Select the attachments from the tree, or fine tune t' +
          'he selection in the options below.  You can perform multiple fin' +
          'e tuning operations.'
        WordWrap = True
      end
      object LabelStatus: TLabel
        Left = 8
        Top = 56
        Width = 68
        Height = 13
        Caption = 'LabelStatus'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object DirDialog1: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 520
    Top = 312
  end
end
