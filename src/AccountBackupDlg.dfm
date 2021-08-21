object FAccountBackupDlg: TFAccountBackupDlg
  Left = 185
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Account Backup'
  ClientHeight = 489
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 489
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel2: TPanel
      Left = 2
      Top = 446
      Width = 569
      Height = 41
      Align = alBottom
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object Button5: TButton
        Left = 480
        Top = 8
        Width = 83
        Height = 25
        Caption = '&Close / Cancel'
        ModalResult = 2
        TabOrder = 4
      end
      object BtnBackup: TButton
        Left = 248
        Top = 8
        Width = 75
        Height = 25
        Hint = 'Start the backup process'
        Caption = '&Backup'
        Default = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = BtnBackupClick
      end
      object Button1: TButton
        Left = 10
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Select All'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 90
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Deselect All'
        TabOrder = 1
        OnClick = Button2Click
      end
      object BtnSaveFolders: TButton
        Left = 170
        Top = 8
        Width = 75
        Height = 25
        Hint = 'Save the list of folders for future backups'
        Caption = 'Save &Folders'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = BtnSaveFoldersClick
      end
    end
    object Panel3: TPanel
      Left = 249
      Top = 41
      Width = 322
      Height = 405
      Align = alRight
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 27
        Height = 13
        Caption = 'Help'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Memo1: TMemo
        Left = 8
        Top = 32
        Width = 296
        Height = 353
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          'From this dialog you can invoke incremental backups on '
          'selected folders in your account.'
          ''
          'Only messages not already present locally will be backed up. '
          'Messages will be backed up into the folder specifed above the '
          
            'folder list on the left. You can change the root backup folder i' +
            'n '
          'Options/Misc.'
          ''
          'The folder hierarchy will be replicated locally if it doesn'#39't '
          'already exist. A local folder will be created for each server '
          'folder.'
          ''
          'Every time you perform a backup from this dialog, the folders '
          'you selected will be saved for future backups. In case you '
          'want to create a list of backup folders without performing an '
          'initial backup, select the folders and click the "Save Folders" '
          'button.'
          ''
          'You can also perform backups from the command line. Please '
          'check the FAQ on our website for more information.')
        TabOrder = 0
        WantReturns = False
      end
    end
    object Panel5: TPanel
      Left = 2
      Top = 41
      Width = 247
      Height = 405
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 2
      object CheckListBox1: TCheckListBox
        Left = 2
        Top = 2
        Width = 243
        Height = 401
        Align = alClient
        Flat = False
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object Panel6: TPanel
      Left = 2
      Top = 2
      Width = 569
      Height = 39
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 3
      object Label8: TLabel
        Left = 8
        Top = 4
        Width = 72
        Height = 13
        Caption = 'Backup Folder:'
      end
      object LblBackupDir: TLabel
        Left = 96
        Top = 4
        Width = 77
        Height = 13
        Caption = 'LblBackupDir'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LblStatus: TLabel
        Left = 10
        Top = 20
        Width = 54
        Height = 13
        Caption = 'LblStatus'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
  end
end
