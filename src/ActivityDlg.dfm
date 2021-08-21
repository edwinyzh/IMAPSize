object FActivityDlg: TFActivityDlg
  Left = 529
  Top = 296
  BorderStyle = bsDialog
  Caption = 'Activity'
  ClientHeight = 107
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 349
    Height = 25
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 5
      Top = 8
      Width = 48
      Height = 13
      Caption = 'Account'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 81
      Top = 8
      Width = 65
      Height = 13
      Caption = 'Description'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 177
      Top = 8
      Width = 50
      Height = 13
      Caption = 'Progress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 281
      Top = 8
      Width = 17
      Height = 13
      Caption = 'St.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel1: TBevel
      Left = 76
      Top = 2
      Width = 2
      Height = 21
    end
    object Bevel2: TBevel
      Left = 172
      Top = 2
      Width = 2
      Height = 21
    end
    object Bevel3: TBevel
      Left = 278
      Top = 2
      Width = 2
      Height = 21
    end
    object Bevel4: TBevel
      Left = 310
      Top = 2
      Width = 2
      Height = 21
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 25
    Width = 349
    Height = 82
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 2
      Width = 345
      Height = 78
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
end
