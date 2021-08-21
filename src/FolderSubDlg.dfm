object FFolderSubDlg: TFFolderSubDlg
  Left = 683
  Top = 235
  BorderStyle = bsDialog
  Caption = 'Folder Subscriptions'
  ClientHeight = 405
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 405
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel2: TPanel
      Left = 2
      Top = 362
      Width = 296
      Height = 41
      Align = alBottom
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object Button5: TButton
        Left = 184
        Top = 8
        Width = 99
        Height = 25
        Caption = 'Close / Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object BtnApply: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Apply!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = BtnApplyClick
      end
    end
    object Panel3: TPanel
      Left = 2
      Top = 2
      Width = 296
      Height = 95
      Align = alTop
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 271
        Height = 26
        Caption = 
          'Select the folders that you want to mark as Subscribed in this a' +
          'ccount.'
        WordWrap = True
      end
      object LblStatus: TLabel
        Left = 8
        Top = 76
        Width = 44
        Height = 13
        Caption = 'LblStatus'
      end
      object Button1: TButton
        Left = 8
        Top = 44
        Width = 75
        Height = 25
        Caption = 'Select All'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 88
        Top = 44
        Width = 75
        Height = 25
        Caption = 'Deselect All'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 208
        Top = 44
        Width = 75
        Height = 25
        Caption = 'Refresh List'
        TabOrder = 2
        OnClick = Button3Click
      end
    end
    object CheckListBox1: TCheckListBox
      Left = 2
      Top = 97
      Width = 296
      Height = 265
      OnClickCheck = CheckListBox1ClickCheck
      Align = alClient
      Flat = False
      ItemHeight = 13
      TabOrder = 2
    end
  end
end
