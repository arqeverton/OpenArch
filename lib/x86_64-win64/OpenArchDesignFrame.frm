object ArchDesignFrame: TArchDesignFrame
  Left = 0
  Height = 595
  Top = 0
  Width = 977
  ClientHeight = 595
  ClientWidth = 977
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object FArchDesignCoolBar: TCoolBar
    Left = 0
    Height = 25
    Top = 570
    Width = 977
    Align = alBottom
    Bands = <>
  end
  object FArchTabControl: TTabControl
    Left = 0
    Height = 570
    Top = 0
    Width = 977
    OnChange = FArchTabControlChange
    Style = tsButtons
    TabIndex = 0
    Tabs.Strings = (
      'Pavimento[]'
      '3D []'
    )
    Align = alClient
    Options = [nboKeyboardTabSwitch]
    TabOrder = 1
    object FArchNoteBook: TNotebook
      Left = 2
      Height = 545
      Top = 23
      Width = 973
      PageIndex = 0
      Align = alClient
      TabOrder = 1
      object FArchStoriesPage: TPage
      end
      object FArch3DPage: TPage
      end
    end
  end
end
