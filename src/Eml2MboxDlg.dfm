object FEml2MboxDlg: TFEml2MboxDlg
  Left = 453
  Top = 168
  BorderStyle = bsDialog
  Caption = 'eml2mbox'
  ClientHeight = 383
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 342
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 254
      Height = 13
      Caption = 'Choose .eml files you want to include in the mbox file'
    end
    object Label2: TLabel
      Left = 16
      Top = 272
      Width = 126
      Height = 13
      Caption = 'Specify the mbox filename'
    end
    object SpeedButton1: TSpeedButton
      Left = 280
      Top = 296
      Width = 23
      Height = 22
      Hint = 'Click here to specify the file through the Save Dialog'
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object Bevel1: TBevel
      Left = 16
      Top = 256
      Width = 457
      Height = 2
    end
    object Label3: TLabel
      Left = 200
      Top = 48
      Width = 129
      Height = 13
      Caption = 'Number of files to convert:'
    end
    object LblNumFiles: TLabel
      Left = 336
      Top = 48
      Width = 6
      Height = 13
      Caption = '0'
    end
    object EdMbox: TEdit
      Left = 16
      Top = 296
      Width = 257
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object CBUnix: TCheckBox
      Left = 320
      Top = 296
      Width = 153
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Save with Unix line ending'
      TabOrder = 1
    end
    object BtnAddFiles: TButton
      Left = 16
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Add files'
      TabOrder = 2
      OnClick = BtnAddFilesClick
    end
    object BtnRemove: TButton
      Left = 96
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Remove'
      TabOrder = 3
      OnClick = BtnRemoveClick
    end
    object ScrollBox1: TScrollBox
      Left = 16
      Top = 80
      Width = 457
      Height = 161
      BorderStyle = bsNone
      TabOrder = 4
      object ListBox1: TListBox
        Left = 0
        Top = 0
        Width = 457
        Height = 161
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 13
        MultiSelect = True
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 342
    Width = 490
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Button1: TButton
      Left = 176
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Convert'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 256
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
