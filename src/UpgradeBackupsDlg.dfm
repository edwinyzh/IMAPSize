object FUpgradeBackupsDlg: TFUpgradeBackupsDlg
  Left = 311
  Top = 176
  Width = 488
  Height = 497
  Caption = 'Backup migration from 0.2.x to 0.3.x'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 200
    Width = 95
    Height = 13
    Caption = 'Path to old backups'
  end
  object Label2: TLabel
    Left = 8
    Top = 232
    Width = 97
    Height = 13
    Caption = 'New Backup Folder:'
  end
  object LblFolder: TLabel
    Left = 112
    Top = 232
    Width = 27
    Height = 13
    Caption = 'Path'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SpeedButton1: TSpeedButton
    Left = 400
    Top = 200
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label3: TLabel
    Left = 8
    Top = 304
    Width = 41
    Height = 13
    Caption = 'Progress'
  end
  object LblOperation: TLabel
    Left = 8
    Top = 264
    Width = 83
    Height = 13
    Caption = 'Current Operation'
  end
  object Label4: TLabel
    Left = 8
    Top = 160
    Width = 436
    Height = 26
    Caption = 
      'IMPORTANT: Before proceeding, please ensure you have a backup co' +
      'py of your new backup.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 8
    Top = 16
    Width = 457
    Height = 137
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'If you have performed backups with versions 0.2.x, this tool wil' +
        'l let you import the meta data for '
      
        'those messages into the 0.3.x database, so you can use old messa' +
        'ges for incremental backups '
      'and restoring.'
      ''
      
        'This tool will not touch your existing old backups. Messages in ' +
        'your old backups which are not '
      
        'found on your IMAP server will be copied to the new backup and m' +
        'eta data about them added to '
      
        'the backup database so you can restore these messages at a later' +
        ' stage if needed.'
      ''
      
        'This operation might take some time, please be patient. In case ' +
        'you stop the migration, it can be '
      
        'safely restarted at a later stage, but the whole process will ha' +
        've to be repeated.')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 112
    Top = 200
    Width = 281
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 160
    Top = 440
    Width = 75
    Height = 25
    Caption = 'Import'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 240
    Top = 440
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 320
    Width = 457
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 4
  end
  object Memo2: TMemo
    Left = 8
    Top = 352
    Width = 457
    Height = 25
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'The migration process is logged in imapsize.log as "bak" events.')
    TabOrder = 5
  end
  object PBFolderDialog1: TPBFolderDialog
    Flags = [ShowPath, NewDialogStyle, ShowShared]
    Left = 440
    Top = 216
  end
end
