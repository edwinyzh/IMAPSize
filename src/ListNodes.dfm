object FListNodes: TFListNodes
  Left = 282
  Top = 236
  Width = 345
  Height = 382
  Caption = 'Mailboxes sorted by size'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelPozadina: TPanel
    Left = 0
    Top = 0
    Width = 337
    Height = 355
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 302
      Width = 337
      Height = 53
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 160
        Top = 16
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = Button1Click
      end
    end
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 337
      Height = 302
      Align = alClient
      Columns = <
        item
          Caption = 'Mailbox'
          Width = 200
        end
        item
          Caption = 'Size'
          Width = 100
        end>
      ColumnClick = False
      ReadOnly = True
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
end
