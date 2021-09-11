object MainForm: TMainForm
  Left = 301
  Height = 675
  Top = 355
  Width = 1091
  Caption = 'OpenArch'
  ClientHeight = 655
  ClientWidth = 1091
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '7.5'
  WindowState = wsMaximized
  object ArchStatusBar: TStatusBar
    Left = 0
    Height = 23
    Top = 632
    Width = 1091
    Panels = <>
    ParentShowHint = False
    SimpleText = 'OpenArch...'
    ShowHint = True
  end
  object MainMenu1: TMainMenu
    Left = 104
    Top = 72
    object MenuFile: TMenuItem
      Caption = 'Start'
      object MenuProject: TMenuItem
        Caption = 'Novo Projeto'
      end
      object MenuProjectSave: TMenuItem
        Caption = 'Save...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuProjectClose: TMenuItem
        Caption = 'Close Project'
      end
      object MenuExit: TMenuItem
        Caption = 'Quit'
      end
    end
    object MenuEdit: TMenuItem
      Caption = 'Editar'
    end
    object MenuBuilding: TMenuItem
      Caption = 'Building'
      object MenuStorySettings: TMenuItem
        Caption = 'Story Settings'
        OnClick = MenuStorySettingsClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuArchitecture: TMenuItem
        Caption = 'Architecture'
        object MenuWall: TMenuItem
          Caption = 'Wall'
          OnClick = MenuWallClick
        end
      end
      object MenuMEP: TMenuItem
        Caption = 'MEP'
      end
      object N6: TMenuItem
        Caption = '-'
      end
    end
    object MenuItem1: TMenuItem
      Caption = 'Project'
      object MenuItem2: TMenuItem
        Caption = 'Project Information'
        OnClick = MenuItem2Click
      end
      object MenuItem3: TMenuItem
        Caption = 'Preferences'
      end
      object MenuItem4: TMenuItem
        Caption = 'Localization'
        object MenuItem5: TMenuItem
          Caption = 'SIG'
        end
        object MenuItem6: TMenuItem
          Caption = 'North Define'
        end
      end
    end
    object MenuItem7: TMenuItem
      Caption = 'Documentation'
      object MenuItem8: TMenuItem
        Caption = 'Tool Box'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuItem9: TMenuItem
        Caption = 'Layers'
        OnClick = MenuItem9Click
      end
      object MenuItem10: TMenuItem
        Caption = 'Scale'
      end
      object MenuItem11: TMenuItem
        Caption = 'Level of Detail'
      end
      object MenuItem12: TMenuItem
        Caption = 'Graphic Styles'
      end
      object MenuItem13: TMenuItem
        Caption = 'Project Phases'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItem14: TMenuItem
        Caption = 'Cut Plan in Floor Settings'
      end
      object N4: TMenuItem
        Caption = '-'
      end
    end
    object MenuItem15: TMenuItem
      Caption = 'Settings'
      object MenuItem16: TMenuItem
        Caption = 'Graphics Attributes'
        object MenuItem17: TMenuItem
          Caption = 'Line Types'
          OnClick = MenuItem17Click
        end
        object MenuItem18: TMenuItem
          Caption = 'Fill Types'
          OnClick = MenuItem18Click
        end
      end
    end
  end
end
