object FFolderHierarchyReplicatorDlg: TFFolderHierarchyReplicatorDlg
  Left = 682
  Top = 413
  BorderStyle = bsDialog
  Caption = 'Folder Hierarchy Replicator'
  ClientHeight = 280
  ClientWidth = 315
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
    Top = 229
    Width = 315
    Height = 51
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Button1: TButton
      Left = 80
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Replicate'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 160
      Top = 16
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
    Width = 315
    Height = 229
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 262
      Height = 39
      Caption = 
        'The Folder Hierarchy Replicator replicates the folder hierarchy ' +
        'of your IMAP account onto your local drive. It does not copy any' +
        ' messages!'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 16
      Top = 136
      Width = 276
      Height = 52
      Caption = 
        'If you have selected eml files as a backup method in Options/Mis' +
        'c/Backup Method, a local folder will be created for each server ' +
        'folder. If mbox files are selected, a mbox file will be created ' +
        'for each server folder. '
      WordWrap = True
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 262
      Height = 13
      Caption = 'The hierarchy will be replicated in the following location:'
    end
    object Label4: TLabel
      Left = 16
      Top = 112
      Width = 280
      Height = 13
      Caption = 'Any existing files/folders in that location will not be touched.'
    end
    object LblLocation: TLabel
      Left = 16
      Top = 88
      Width = 67
      Height = 13
      Caption = 'LblLocation'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 16
      Top = 200
      Width = 198
      Height = 13
      Caption = 'The currently selected backup method is: '
      WordWrap = True
    end
    object LblMethod: TLabel
      Left = 216
      Top = 200
      Width = 60
      Height = 13
      Caption = 'LblMethod'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
