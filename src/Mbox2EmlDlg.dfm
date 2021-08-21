object FMbox2EmlDlg: TFMbox2EmlDlg
  Left = 277
  Top = 176
  BorderStyle = bsDialog
  Caption = 'mbox2eml'
  ClientHeight = 364
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 309
    Height = 323
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
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 263
      Height = 52
      Caption = 
        'This dialog will help you convert mbox files to eml files. You c' +
        'an convert single or multiple mbox files at once. To select mult' +
        'iple files, hold down the CTRL key while selecting files in the ' +
        'file dialog. '
      WordWrap = True
    end
    object Label2: TLabel
      Left = 16
      Top = 240
      Width = 261
      Height = 26
      Caption = 
        'Select the destination folder for eml files. A  subfolder will b' +
        'e created for each mbox file specified above'
      WordWrap = True
    end
    object SpeedButton2: TSpeedButton
      Left = 264
      Top = 280
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton2Click
    end
    object EdEmlDir: TEdit
      Left = 16
      Top = 280
      Width = 241
      Height = 21
      TabOrder = 0
    end
    object Memo1: TMemo
      Left = 16
      Top = 112
      Width = 273
      Height = 89
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
    object Button4: TButton
      Left = 214
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Clear List'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button3: TButton
      Left = 16
      Top = 80
      Width = 177
      Height = 25
      Caption = 'Select mbox files to convert'
      TabOrder = 3
      OnClick = Button3Click
    end
    object CBDotsToFolders: TCheckBox
      Left = 16
      Top = 208
      Width = 257
      Height = 17
      Caption = 'Dots in mbox file represent folders separators'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 323
    Width = 309
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Button1: TButton
      Left = 80
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Convert'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 160
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object DirDialog1: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 272
    Top = 176
  end
end
