object LineTypesForm: TLineTypesForm
  Left = 430
  Height = 330
  Top = 256
  Width = 449
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Tipos de Linha'
  ClientHeight = 330
  ClientWidth = 449
  Position = poScreenCenter
  LCLVersion = '7.3'
  object Panel1: TPanel
    Left = 0
    Height = 50
    Top = 280
    Width = 449
    Align = alBottom
    ClientHeight = 50
    ClientWidth = 449
    TabOrder = 2
    object btnOK: TButton
      Left = 8
      Height = 25
      Top = 16
      Width = 75
      Caption = 'OK'
      OnClick = btnOKClick
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 10
    Height = 52
    Top = 106
    Width = 429
    Align = alTop
    BorderSpacing.Left = 10
    BorderSpacing.Right = 10
    BevelOuter = bvLowered
    ClientHeight = 52
    ClientWidth = 429
    TabOrder = 1
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Height = 50
      Top = 1
      Width = 427
      Camera = GLCamera1
      VSync = vsmSync
      Buffer.BackgroundColor = clWhite
      Buffer.AntiAliasing = aaNone
      FieldOfView = 136.397186279297
      Align = alClient
      TabOrder = 0
    end
  end
  object ComboBoxEx1: TComboBoxEx
    Left = 10
    Height = 56
    Top = 10
    Width = 429
    Align = alTop
    AutoDropDown = True
    BorderSpacing.Around = 10
    BorderStyle = bsSingle
    Images = ImageList1
    ItemHeight = 50
    ItemsEx = <>
    OnChange = ComboBoxEx1Change
    ParentBidiMode = False
    Style = csExDropDownList
    StyleEx = [csExNoEditImage, csExNoEditImageIndent]
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Height = 30
    Top = 76
    Width = 449
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 30
    ClientWidth = 449
    TabOrder = 3
    object btnAddLineType: TButton
      Left = 363
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Novo'
      OnClick = btnAddLineTypeClick
      TabOrder = 0
    end
    object btnDelete: TButton
      Left = 203
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Deletar'
      OnClick = btnDeleteClick
      TabOrder = 1
    end
    object btnRename: TButton
      Left = 283
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Renomear'
      OnClick = btnRenameClick
      TabOrder = 2
    end
  end
  object stgDash: TStringGrid
    Left = 10
    Height = 72
    Top = 168
    Width = 429
    Align = alTop
    BorderSpacing.Around = 10
    Color = clDefault
    ColCount = 1
    ColRowDragIndicatorColor = cl3DLight
    DefaultColWidth = 75
    FixedRows = 0
    HeaderHotZones = []
    Options = [goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
    RowCount = 2
    ScrollBars = ssHorizontal
    TabAdvance = aaDown
    TabOrder = 4
    OnMouseDown = stgDashMouseDown
    OnSelection = stgDashSelection
    OnValidateEntry = stgDashValidateEntry
  end
  object btnAddDash: TButton
    Left = 10
    Height = 25
    Top = 248
    Width = 100
    Caption = 'Novo Traçejado'
    OnClick = btnAddDashClick
    TabOrder = 5
  end
  object btnDelDash: TButton
    Left = 115
    Height = 25
    Top = 248
    Width = 100
    Caption = 'Deletar Traçejado'
    OnClick = btnDelDashClick
    TabOrder = 6
  end
  object GLScene1: TGLScene
    Left = 384
    Top = 152
    object GLCamera1: TGLCamera
      DepthOfView = 1
      FocalLength = 10
      SceneScale = 76
      CameraStyle = csOrthogonal
      KeepFOVMode = ckmVerticalFOV
      Position.Coordinates = {
        00000000000000000000803F0000803F
      }
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
      end
    end
    object GLLines1: TGLLines
      Visible = False
      LineColor.Color = {
        00000000000000000000000000000000
      }
      LineWidth = 0.100000001490116
      NodeColor.Color = {
        ACC8483E9A99193FCDCC4C3F0000003F
      }
      Nodes = <      
        item
          Y = -0.0500000007450581
        end      
        item
          Y = 0.0500000007450581
        end>
      NodesAspect = lnaInvisible
      SplineMode = lsmSegments
      Options = [loUseNodeColorForLines]
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Width = 250
    Height = 20
    Buffer.BackgroundColor = clWhite
    Left = 384
    Top = 280
  end
  object ImageList1: TImageList
    Height = 50
    ShareImages = True
    Width = 200
    Left = 384
    Top = 216
  end
end
