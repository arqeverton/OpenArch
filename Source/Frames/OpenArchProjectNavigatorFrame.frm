object ArchProjectNavigatorFrame: TArchProjectNavigatorFrame
  Left = 0
  Height = 559
  Top = 0
  Width = 220
  ClientHeight = 559
  ClientWidth = 220
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object ToolBar2: TToolBar
    Left = 0
    Height = 72
    Top = 0
    Width = 220
    ButtonHeight = 36
    ButtonWidth = 36
    Caption = 'ToolBar2'
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 1
      Hint = 'Pavimentos'
      Top = 2
      Caption = 'PVTO'
      Style = tbsCheck
    end
    object ToolButton5: TToolButton
      Left = 38
      Hint = 'Cortes'
      Top = 2
      Caption = 'COR'
      Style = tbsCheck
    end
    object ToolButton6: TToolButton
      Left = 74
      Hint = 'Elevações'
      Top = 2
      Caption = 'ELV'
      Style = tbsCheck
    end
    object ToolButton7: TToolButton
      Left = 110
      Hint = 'Elevações Interiores'
      Top = 2
      Caption = 'ELI'
    end
    object ToolButton8: TToolButton
      Left = 146
      Hint = 'Croquis em 2D'
      Top = 2
      Caption = 'C2D'
      Style = tbsCheck
    end
    object ToolButton9: TToolButton
      Left = 182
      Hint = 'Detalhes e Ampliações'
      Top = 2
      Caption = 'AMP'
      Style = tbsCheck
    end
    object ToolButton10: TToolButton
      Left = 1
      Hint = 'Documentar 3D'
      Top = 38
      Caption = 'D3D'
      Style = tbsCheck
    end
    object ToolButton11: TToolButton
      Left = 37
      Hint = 'Vistas 3D'
      Top = 38
      Caption = '3DV'
      Style = tbsCheck
    end
    object ToolButton12: TToolButton
      Left = 73
      Hint = 'Planilhas'
      Top = 38
      Caption = 'PLAN'
      Style = tbsCheck
    end
  end
  object ArchNotebookProject: TNotebook
    Left = 0
    Height = 487
    Top = 72
    Width = 220
    PageIndex = 0
    Align = alClient
    TabOrder = 1
    object ArchStoriesPage: TPage
      object StringGrid1: TStringGrid
        Left = 0
        Height = 487
        Top = 0
        Width = 220
        Align = alClient
        AutoEdit = False
        ColCount = 3
        Columns = <        
          item
            Title.Caption = 'Title'
            Width = 25
          end        
          item
            Title.Caption = 'Title'
            Width = 25
          end        
          item
            Title.Caption = 'Title'
            Width = 145
          end>
        DefaultRowHeight = 25
        FixedCols = 0
        FixedRows = 0
        Options = [goRangeSelect, goSmoothScroll, goRowHighlight]
        RowCount = 2
        ScrollBars = ssVertical
        TabOrder = 0
        OnDblClick = StringGrid1DblClick
        OnDrawCell = StringGrid1DrawCell
        OnMouseMove = StringGrid1MouseMove
      end
    end
    object ArchSectionsPage: TPage
    end
    object ArchElevationPage: TPage
    end
    object ArchInteriorElevationPage: TPage
    end
    object ArchSketch2DPage: TPage
    end
    object ArchDetailPage: TPage
    end
    object Arch3DDocumentPage: TPage
    end
    object Arch3DPage: TPage
    end
    object ArchSpreadSheetsPage: TPage
    end
  end
end
