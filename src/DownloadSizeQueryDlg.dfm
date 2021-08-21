object FDownloadSizeQueryDlg: TFDownloadSizeQueryDlg
  Left = 237
  Top = 253
  BorderStyle = bsDialog
  Caption = 'Download Size Query'
  ClientHeight = 250
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 250
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 80
      Top = 16
      Width = 236
      Height = 52
      Caption = 
        'The operation you have requested requires to download more data ' +
        'than what you have set as a warning threshold for downloadable m' +
        'essage size (Options/Message Viewer).'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 16
      Top = 112
      Width = 311
      Height = 26
      Caption = 
        'Press "Get Specified" to download the specified amount of bytes.' +
        ' Press "Get Full" to get the full amount available.'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 300
      Height = 26
      Caption = 
        'Use the slider or the edit box to specify the amount in bytes yo' +
        'u want to download.'
      WordWrap = True
    end
    object Button1: TButton
      Left = 16
      Top = 208
      Width = 115
      Height = 25
      Caption = 'Get Specified (Bytes)'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 256
      Top = 208
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button3: TButton
      Left = 136
      Top = 208
      Width = 113
      Height = 25
      Caption = 'Get Full Amount'
      TabOrder = 2
      OnClick = Button3Click
    end
    object SpinEdit1: TSpinEdit
      Left = 256
      Top = 160
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = SpinEdit1Change
    end
    object TrackBar1: TExTrackBar
      Left = 16
      Top = 152
      Width = 233
      Height = 45
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar1Change
      LabelsFactor = 1
      LabelsFactorOperation = foMul
      LabelsInterval = 2
      SelEnable = False
    end
  end
end
