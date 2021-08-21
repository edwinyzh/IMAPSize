object FAbout: TFAbout
  Left = 458
  Top = 217
  BorderStyle = bsDialog
  Caption = 'IMAPSize'
  ClientHeight = 325
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel
    Left = 24
    Top = 112
    Width = 42
    Height = 13
    Caption = 'Installer: '
  end
  object JDUrlLabel10: TJDUrlLabel
    Left = 128
    Top = 112
    Width = 53
    Height = 13
    Cursor = crHandPoint
    Hint = 'http://www.jrsoftware.org/isinfo.php'
    Caption = 'Inno Setup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    DetectMouse = True
    URLType = utCustom
    URL = 'http://www.jrsoftware.org/isinfo.php'
  end
  object Label16: TLabel
    Left = 24
    Top = 156
    Width = 26
    Height = 13
    Caption = 'Cyrus'
  end
  object JDUrlLabel13: TJDUrlLabel
    Left = 128
    Top = 156
    Width = 57
    Height = 13
    Cursor = crHandPoint
    Hint = 'http://www.fastmail.fm/mail/?STKI=67979'
    Caption = 'FastMail.FM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    DetectMouse = True
    URLType = utCustom
    URL = 'http://www.fastmail.fm/mail/?STKI=67979'
  end
  object Label17: TLabel
    Left = 32
    Top = 164
    Width = 26
    Height = 13
    Caption = 'Cyrus'
  end
  object JDUrlLabel14: TJDUrlLabel
    Left = 136
    Top = 164
    Width = 57
    Height = 13
    Cursor = crHandPoint
    Hint = 'http://www.fastmail.fm/mail/?STKI=67979'
    Caption = 'FastMail.FM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    DetectMouse = True
    URLType = utCustom
    URL = 'http://www.fastmail.fm/mail/?STKI=67979'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 258
    Height = 325
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'About'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 250
        Height = 297
        Align = alClient
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 78
          Height = 19
          Caption = 'IMAPSize'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object VersionLabel: TLabel
          Left = 15
          Top = 40
          Width = 64
          Height = 13
          Caption = 'Version 0.0.2'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 13
          Top = 96
          Width = 124
          Height = 13
          Caption = 'Author: Ivan Vecanski'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object JDUrlLabel2: TJDUrlLabel
          Left = 13
          Top = 152
          Width = 149
          Height = 13
          Cursor = crHandPoint
          Hint = 'Send an email to the author'
          Caption = 'e-mail: imapsize@broobles.com'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          ShowUrlHint = False
          URLType = utMail
          URL = 'imapsize@broobles.com'
        end
        object JDUrlLabel3: TJDUrlLabel
          Left = 13
          Top = 136
          Width = 51
          Height = 13
          Cursor = crHandPoint
          Hint = 'IMAPSize on the web'
          Caption = 'Homepage'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          ShowUrlHint = False
          URLType = utCustom
          URL = 'http://www.broobles.com/imapsize'
        end
        object Label4: TLabel
          Left = 16
          Top = 56
          Width = 88
          Height = 13
          Caption = 'Licence: Freeware'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object JDUrlLabel16: TJDUrlLabel
          Left = 13
          Top = 120
          Width = 71
          Height = 13
          Cursor = crHandPoint
          Hint = 'IMAPSize Support Forum'
          Caption = 'Support Forum'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          ShowUrlHint = False
          URLType = utCustom
          URL = 'http://www.broobles.com/forum/'
        end
        object Button1: TButton
          Left = 163
          Top = 264
          Width = 75
          Height = 25
          Caption = 'OK'
          Default = True
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Credits'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 250
        Height = 297
        Align = alClient
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Label6: TLabel
          Left = 24
          Top = 28
          Width = 39
          Height = 13
          Caption = 'TCP/IP:'
        end
        object JDUrlLabel1: TJDUrlLabel
          Left = 128
          Top = 28
          Width = 41
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.ararat.cz/synapse/'
          Caption = 'Synapse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.ararat.cz/synapse/'
        end
        object Label7: TLabel
          Left = 24
          Top = 44
          Width = 23
          Height = 13
          Caption = 'SSL:'
        end
        object JDUrlLabel4: TJDUrlLabel
          Left = 128
          Top = 44
          Width = 43
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.openssl.org'
          Caption = 'OpenSSL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.openssl.org'
        end
        object Label3: TLabel
          Left = 24
          Top = 168
          Width = 42
          Height = 13
          Caption = 'Installer: '
        end
        object JDUrlLabel6: TJDUrlLabel
          Left = 128
          Top = 168
          Width = 53
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.jrsoftware.org/isinfo.php'
          Caption = 'Inno Setup'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.jrsoftware.org/isinfo.php'
        end
        object Label8: TLabel
          Left = 24
          Top = 76
          Width = 41
          Height = 13
          Caption = 'Logging:'
        end
        object JDUrlLabel7: TJDUrlLabel
          Left = 128
          Top = 76
          Width = 37
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.vectrics.com/site/main/product/vcl_log/components.do'
          Caption = 'Vectrics'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.vectrics.com/site/main/product/vcl_log/components.do'
        end
        object Label9: TLabel
          Left = 24
          Top = 60
          Width = 99
          Height = 13
          Caption = 'Settings Persistance:'
        end
        object JDUrlLabel8: TJDUrlLabel
          Left = 128
          Top = 60
          Width = 49
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.bigattichouse.com/thoughtbrew.php?BREWID=QUICKRTTI'
          Caption = 'QuickRTTI'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.bigattichouse.com/thoughtbrew.php?BREWID=QUICKRTTI'
        end
        object Label10: TLabel
          Left = 8
          Top = 192
          Width = 87
          Height = 13
          Caption = 'Test Accounts:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label11: TLabel
          Left = 8
          Top = 8
          Width = 74
          Height = 13
          Caption = 'Components:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label12: TLabel
          Left = 24
          Top = 208
          Width = 26
          Height = 13
          Caption = 'Cyrus'
        end
        object JDUrlLabel9: TJDUrlLabel
          Left = 128
          Top = 208
          Width = 57
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.fastmail.fm/mail/?STKI=67979'
          Caption = 'FastMail.FM'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.fastmail.fm/mail/?STKI=67979'
        end
        object Label14: TLabel
          Left = 24
          Top = 224
          Width = 33
          Height = 13
          Caption = 'Courier'
        end
        object JDUrlLabel11: TJDUrlLabel
          Left = 128
          Top = 224
          Width = 36
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.vfemail.net'
          Caption = 'VFEmail'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.vfemail.net'
        end
        object Label15: TLabel
          Left = 24
          Top = 240
          Width = 48
          Height = 13
          Caption = 'UW IMAP'
        end
        object JDUrlLabel12: TJDUrlLabel
          Left = 128
          Top = 240
          Width = 36
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.f2o.org'
          Caption = 'f2o.org'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.f2o.org'
        end
        object Label18: TLabel
          Left = 24
          Top = 256
          Width = 67
          Height = 13
          Caption = 'Novel NetMail'
        end
        object JDUrlLabel15: TJDUrlLabel
          Left = 128
          Top = 256
          Width = 53
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.myrealbox.com/'
          Caption = 'MyRealBox'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.myrealbox.com/'
        end
        object Label19: TLabel
          Left = 8
          Top = 136
          Width = 36
          Height = 13
          Caption = 'Tools:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label21: TLabel
          Left = 24
          Top = 152
          Width = 98
          Height = 13
          Caption = 'Exception Handling: '
        end
        object JDUrlLabel17: TJDUrlLabel
          Left = 128
          Top = 152
          Width = 53
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://help.madshi.net/madExcept.htm'
          Caption = 'madExcept'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://help.madshi.net/madExcept.htm'
        end
        object Label2: TLabel
          Left = 24
          Top = 92
          Width = 68
          Height = 13
          Caption = 'HTML Viewer:'
        end
        object JDUrlLabel5: TJDUrlLabel
          Left = 128
          Top = 92
          Width = 37
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.pbear.com/'
          Caption = 'htmlLite'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.pbear.com/'
        end
        object Label20: TLabel
          Left = 24
          Top = 108
          Width = 49
          Height = 13
          Caption = 'Database:'
        end
        object JDUrlLabel18: TJDUrlLabel
          Left = 128
          Top = 108
          Width = 31
          Height = 13
          Cursor = crHandPoint
          Hint = 'http://www.sqlite.org/'
          Caption = 'SQLite'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          DetectMouse = True
          URLType = utCustom
          URL = 'http://www.sqlite.org/'
        end
      end
    end
  end
end
