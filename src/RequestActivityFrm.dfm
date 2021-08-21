object RequestActivityFrame: TRequestActivityFrame
  Left = 0
  Top = 0
  Width = 334
  Height = 21
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 21
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object RequestDescriptionLbl: TLabel
      Left = 80
      Top = 3
      Width = 54
      Height = 13
      Caption = 'RequestLbl'
      OnClick = RequestDescriptionLblClick
    end
    object ALProgressBar1: TALProgressBar
      Left = 176
      Top = 6
      Width = 97
      Height = 8
      BackgroundColor = clBtnFace
      BarColor1 = clBtnShadow
      BarColorStyle = cs1Color
      ShowPosText = False
    end
    object GIFImage1: TGIFImage
      Left = 280
      Top = 3
      Width = 24
      Height = 12
      Animate = False
      AutoSize = True
      Center = False
      FirstImageOnly = False
      Graphic = GIFImage1
      Loop = False
      Opaque = False
      Stretch = False
      StretchRatio = False
      StretchBigOnly = False
      Tile = False
      Threaded = False
      Data = {
        8603000047494638396118000C00F70000040204FC824464FE9C346634FC0204
        FCFE04FCFEFC2422249CFE9C64CE649200005A0000000C980040CE5F6FF71F81
        BF530000004040776F6F01818100940000B10077D300018700D44098B300B141
        00D35900870000360000CF0000F70000BFA450980040B1A56FD3008187AF0CCE
        5940FFCF6FF78281BFFFF0981575B1A4B8D300CF8777F40401E4E60176760000
        00F40E50B4A14009F76F52BF8100679401A5B100F7D300BF878F00F04B4075A4
        6FB80081CF7740040100E60000760000005FDF0E1F03A10000F70000BF370026
        7F00B3FF00F71500BF6F08000100406F0F6F01288143F7003ABF005CB6005051
        005250744FB500472800520000419F984D01B12000D346008749D7984C82B145
        00D35301875C00944900B1528FD3464B874100004E000056BF00496500450098
        5700CE5C5FF7691FBF5F8F0076B14069F96F65BF817774003300003200002E00
        006930986EE5B16976D30000877F243608DBCFFAF7F7BFBFBF0144980080B100
        C9D30087876E74CEE500FF7600F70000BF4C989850B1B154D3D3008787EC2098
        1300B15400D300408700738800B10200F9F800BFBF0100E20030130076F70000
        BFF0DF00750300B80000CF00005CF007E4750076B80000CF000E78DFA1E63EF7
        7600BF000067FD00A5F400F7F700BFBF000000004000006F0000810000407F00
        00080000FA0000BF00DEC00003DC00004F00000000007C4900E5020076F80000
        BF08563B00AB0000F80000BF00E230FF13BFFFF700FFBF80FF67583B015F0000
        000000800064B8E8CEA5F0F70076BF8000F78800418B00F70000BF80009800A0
        B101E6D30076870000FAD0FEDAC258F7004BBF800090EC809413E6FC5476BF00
        00DE7F5B03086200FA4E00BF0074FFB400FFE600FF7600FF00989C7FE4E60876
        76FA0000BF24EB57DBB6E7F7F976BFBF00EC6CC3136FFF544EFF00007F00EC1C
        0013E7005476000000CC7FECE4081376FA5400BF0020FF2C01FFF700FF7600FF
        00940067B1B71DD3F9FA87BFBF209C2400E500007600000000A03100A30000F7
        0000BF000000986E40B1FD6FD34681870021F90401000006002C000000001800
        0C00070863000D081C48B0A0C18308070C40C890E042010B0D1408D0B0E04204
        09221620002061440317331E101880E3C1010840AAC438602449930607244819
        B2254102300B1E909992A54B82003A1EDC3953A4C1920D7722B06910A750843B
        7F1A0C5AB16AC180003B}
    end
    object AccountLbl: TLabel
      Left = 4
      Top = 3
      Width = 54
      Height = 13
      Caption = 'AccountLbl'
    end
    object SpeedButton1: TSpeedButton
      Left = 312
      Top = 1
      Width = 18
      Height = 18
      Hint = 'Close request'
      Caption = 'x'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
      OnClick = SpeedButton1Click
    end
    object Bevel1: TBevel
      Left = 74
      Top = 0
      Width = 2
      Height = 18
    end
    object Bevel2: TBevel
      Left = 170
      Top = 0
      Width = 2
      Height = 18
    end
    object Bevel3: TBevel
      Left = 276
      Top = 0
      Width = 2
      Height = 18
    end
    object Bevel4: TBevel
      Left = 308
      Top = 0
      Width = 2
      Height = 18
    end
    object Bevel5: TBevel
      Left = 0
      Top = 20
      Width = 310
      Height = 2
      Shape = bsBottomLine
      Style = bsRaised
    end
  end
end
