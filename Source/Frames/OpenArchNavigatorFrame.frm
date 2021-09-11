object ArchNavigatorFrame: TArchNavigatorFrame
  Left = 0
  Height = 620
  Top = 0
  Width = 220
  ClientHeight = 620
  ClientWidth = 220
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object ToolBar1: TToolBar
    Left = 0
    Height = 72
    Top = 0
    Width = 220
    ButtonHeight = 72
    ButtonWidth = 72
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 0
    object tbtnProject: TToolButton
      Left = 1
      Hint = 'Visualizar e Gerenciar o Projeto'
      Top = 0
      Caption = 'PR'
      OnClick = tbtnProjectClick
      Style = tbsCheck
    end
    object tbtnView: TToolButton
      Left = 73
      Hint = 'Gerenciar Vistas'
      Top = 0
      Caption = 'VIEW'
      OnClick = tbtnViewClick
      Style = tbsCheck
    end
    object tbtnLayout: TToolButton
      Left = 145
      Hint = 'Gerenciar Pranchas'
      Top = 0
      Caption = 'LAY'
      OnClick = tbtnLayoutClick
      Style = tbsCheck
    end
  end
  object NoteBookNavigator: TNotebook
    Left = 0
    Height = 432
    Top = 72
    Width = 220
    PageIndex = 0
    Align = alTop
    TabOrder = 1
    object PageProject: TPage
    end
    object PageView: TPage
    end
    object PageLayout: TPage
    end
  end
end
