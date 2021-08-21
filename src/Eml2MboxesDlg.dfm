object FEml2MboxesDlg: TFEml2MboxesDlg
  Left = 214
  Top = 348
  BorderStyle = bsDialog
  Caption = 'eml2mboxes'
  ClientHeight = 279
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 238
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 319
      Height = 52
      Caption = 
        'This dialog enables you to convert a hieararchy of folders with ' +
        'eml files into mbox files. Only files with the eml extension wil' +
        'l be added to mbox files. One mbox file will be created for each' +
        ' folder with eml files.'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 16
      Top = 80
      Width = 323
      Height = 13
      Caption = 
        'Select the folder which contains the hierarchy of folders with e' +
        'ml files'
    end
    object Label3: TLabel
      Left = 16
      Top = 144
      Width = 279
      Height = 13
      Caption = 'Select the folder where generated mbox files will be placed:'
    end
    object SpeedButton1: TSpeedButton
      Left = 304
      Top = 104
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 304
      Top = 168
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButton2Click
    end
    object EditEmlDir: TEdit
      Left = 16
      Top = 104
      Width = 281
      Height = 21
      TabOrder = 0
    end
    object EditMboxDir: TEdit
      Left = 16
      Top = 168
      Width = 281
      Height = 21
      TabOrder = 1
    end
    object CBUnix: TCheckBox
      Left = 15
      Top = 206
      Width = 202
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Save mbox files with Unix line ending'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 238
    Width = 360
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Button1: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Convert'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 176
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object DirDialog1: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 320
    Top = 88
  end
  object DirDialog2: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 320
    Top = 144
  end
end
