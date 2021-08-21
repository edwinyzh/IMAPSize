object SearchKeyFrame: TSearchKeyFrame
  Left = 0
  Top = 0
  Width = 357
  Height = 40
  TabOrder = 0
  object ComboBoxSearchKeys: TComboBox
    Left = 8
    Top = 8
    Width = 97
    Height = 21
    Style = csDropDownList
    DropDownCount = 11
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxSearchKeysChange
  end
  object ComboBoxRelations: TComboBox
    Left = 112
    Top = 8
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object ComboBoxValues: TComboBox
    Left = 232
    Top = 8
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object EditBoxRelations: TEdit
    Left = 112
    Top = 8
    Width = 113
    Height = 21
    TabOrder = 3
  end
  object EditBoxValues: TEdit
    Left = 232
    Top = 8
    Width = 113
    Height = 21
    TabOrder = 4
  end
end
