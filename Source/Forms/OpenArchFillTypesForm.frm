object FillTypesForm: TFillTypesForm
  Left = 430
  Height = 551
  Top = 256
  Width = 756
  Caption = 'Tipos de Preenchimento'
  ClientHeight = 551
  ClientWidth = 756
  Font.Name = 'Century Gothic'
  LCLVersion = '7.5'
  object ComboBoxEx1: TComboBoxEx
    Left = 10
    Height = 56
    Top = 10
    Width = 736
    Align = alTop
    AutoDropDown = True
    BorderSpacing.Around = 10
    BorderStyle = bsSingle
    ItemHeight = 50
    ItemsEx = <>
    ParentBidiMode = False
    Style = csExDropDownList
    StyleEx = [csExNoEditImage, csExNoEditImageIndent]
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Height = 30
    Top = 76
    Width = 756
    Align = alTop
    BevelOuter = bvNone
    ClientHeight = 30
    ClientWidth = 756
    TabOrder = 1
    object btnAddFillType: TButton
      Left = 363
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Novo'
      TabOrder = 0
    end
    object btnDelete: TButton
      Left = 203
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Deletar'
      TabOrder = 1
    end
    object btnRename: TButton
      Left = 283
      Height = 25
      Top = 0
      Width = 75
      Caption = 'Renomear'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 10
    Height = 326
    Top = 106
    Width = 736
    Align = alTop
    BorderSpacing.Left = 10
    BorderSpacing.Right = 10
    BevelInner = bvLowered
    BevelOuter = bvNone
    ClientHeight = 326
    ClientWidth = 736
    TabOrder = 2
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Height = 324
      Top = 1
      Width = 308
      Camera = GLCamera1
      VSync = vsmSync
      Buffer.BackgroundColor = clWhite
      Buffer.AntiAliasing = aaNone
      FieldOfView = 172.569427490234
      Align = alLeft
      TabOrder = 0
    end
    object GrBoxPatternSize: TGroupBox
      Left = 320
      Height = 80
      Top = 8
      Width = 400
      Caption = 'Padrão'
      ClientHeight = 60
      ClientWidth = 396
      TabOrder = 1
      object LabeledEdit4: TLabeledEdit
        Left = 104
        Height = 23
        Top = 0
        Width = 80
        EditLabel.Height = 15
        EditLabel.Width = 76
        EditLabel.Caption = 'Largura (mm)'
        LabelPosition = lpLeft
        LabelSpacing = 10
        TabOrder = 0
      end
      object LabeledEdit5: TLabeledEdit
        Left = 104
        Height = 23
        Top = 32
        Width = 80
        EditLabel.Height = 15
        EditLabel.Width = 64
        EditLabel.Caption = 'Altura (mm)'
        LabelPosition = lpLeft
        LabelSpacing = 10
        TabOrder = 1
      end
    end
    object GrBoxLevel: TGroupBox
      Left = 320
      Height = 232
      Top = 88
      Width = 416
      Caption = 'Nível'
      ClientHeight = 212
      ClientWidth = 412
      TabOrder = 2
      object ComboBox1: TComboBox
        Left = 8
        Height = 23
        Top = 0
        Width = 151
        ItemHeight = 15
        TabOrder = 1
        Text = 'ComboBox1'
      end
      object GrBoxStart: TGroupBox
        Left = 8
        Height = 81
        Top = 32
        Width = 208
        Caption = 'Origem'
        ClientHeight = 61
        ClientWidth = 204
        TabOrder = 2
        object LabeledEdit1: TLabeledEdit
          Left = 104
          Height = 23
          Top = 0
          Width = 80
          EditLabel.Height = 15
          EditLabel.Width = 7
          EditLabel.Caption = 'X'
          LabelPosition = lpLeft
          LabelSpacing = 10
          TabOrder = 0
        end
        object LabeledEdit2: TLabeledEdit
          Left = 104
          Height = 23
          Top = 32
          Width = 80
          EditLabel.Height = 15
          EditLabel.Width = 7
          EditLabel.Caption = 'Y'
          LabelPosition = lpLeft
          LabelSpacing = 10
          TabOrder = 1
        end
      end
      object GrBoxGeometric: TGroupBox
        Left = 8
        Height = 88
        Top = 120
        Width = 208
        Caption = 'Geometria'
        ClientHeight = 68
        ClientWidth = 204
        TabOrder = 0
        object LabeledEdit3: TLabeledEdit
          Left = 104
          Height = 23
          Top = 8
          Width = 80
          EditLabel.Height = 15
          EditLabel.Width = 85
          EditLabel.Caption = 'Tamanho (mm)'
          LabelPosition = lpLeft
          LabelSpacing = 10
          TabOrder = 0
        end
        object LabeledEdit6: TLabeledEdit
          Left = 104
          Height = 23
          Top = 40
          Width = 80
          EditLabel.Height = 15
          EditLabel.Width = 61
          EditLabel.Caption = 'Rotação (º)'
          LabelPosition = lpLeft
          LabelSpacing = 10
          TabOrder = 1
        end
      end
      object Button2: TButton
        Left = 163
        Height = 25
        Hint = 'Adicionar Nível'
        Top = 0
        Width = 25
        Caption = '+'
        Font.Height = -20
        Font.Name = 'Century Gothic'
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object Button3: TButton
        Left = 191
        Height = 25
        Hint = 'Remover Nível'
        Top = 0
        Width = 25
        Caption = '-'
        Font.Height = -20
        Font.Name = 'Century Gothic'
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object GrBoxRepetition: TGroupBox
        Left = 224
        Height = 176
        Top = 32
        Width = 168
        Caption = 'Repetições'
        TabOrder = 5
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Height = 50
    Top = 501
    Width = 756
    Align = alBottom
    ClientHeight = 50
    ClientWidth = 756
    TabOrder = 3
    object btnOK: TButton
      Left = 8
      Height = 25
      Top = 16
      Width = 75
      Caption = 'OK'
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 232
    Height = 25
    Top = 480
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 4
  end
  object GLScene1: TGLScene
    Left = 8
    Top = 72
    object GLCamera1: TGLCamera
      DepthOfView = 1
      FocalLength = 10
      KeepFOVMode = ckmVerticalFOV
      Position.Coordinates = {
        00000000000000000000803F0000803F
      }
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
      end
    end
    object GLPolygon1: TGLPolygon
      Material.BackProperties.Diffuse.Color = {
        DAD9593FDAD9593FD1D0D03D0000803F
      }
      Material.FrontProperties.Diffuse.Color = {
        0000000000000000000000000000803F
      }
      Material.BlendingMode = bmTransparency
      Material.MaterialOptions = [moNoLighting]
      Material.Texture.ImageClassName = 'TGLCubeMapImage'
      Material.Texture.MinFilter = miLinear
      Material.Texture.MappingMode = tmmEyeLinear
      Material.Texture.MappingSCoordinates.Coordinates = {
        00000040000000000000000000000000
      }
      Material.Texture.MappingTCoordinates.Coordinates = {
        00000000000000400000000000000040
      }
      Material.Texture.MappingRCoordinates.Coordinates = {
        00000000000000000000000000000000
      }
      Material.Texture.TextureCompareFunc = cfAlways
      Material.Texture.DepthTextureMode = dtmAlpha
      Material.Texture.KeepImageAfterTransfer = True
      Visible = False
      Nodes = <      
        item
          X = -0.0500000007450581
          Y = -0.0500000007450581
        end      
        item
          X = -0.0500000007450581
          Y = 0.0500000007450581
        end      
        item
          X = 0.0500000007450581
          Y = 0.0500000007450581
        end      
        item
          X = 0.0500000007450581
          Y = -0.0500000007450581
        end>
    end
    object GLDirectOpenGL1: TGLDirectOpenGL
      VisibilityCulling = vcNone
      UseBuildList = False
      OnRender = GLDirectOpenGL1Render
      Blend = False
    end
  end
  object ImageList1: TImageList
    Height = 50
    ShareImages = True
    Width = 200
    Left = 40
    Top = 72
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Width = 250
    Height = 20
    Buffer.BackgroundColor = clWhite
    Left = 72
    Top = 72
  end
  object GLSLShader1: TGLSLShader
    FragmentProgram.Code.Strings = (
      'uniform vec3 BrickColor, MortarColor;'
      'uniform vec2 BrickSize;'
      'uniform vec2 BrickPct;'
      'varying vec2 MCposition;'
      'varying float LightIntensity;'
      'void main()'
      '{'
      'vec3 color;'
      'vec2 position, useBrick;'
      'position = MCposition / BrickSize;'
      'if (fract(position.y * 0.5) > 0.5)'
      'position.x += 0.5;'
      'position = fract(position);'
      'useBrick = step(position, BrickPct);'
      'color = mix(MortarColor, BrickColor, useBrick.x * useBrick.y);'
      'color *= LightIntensity;'
      'gl_FragColor = vec4(color, 1.0);'
      '}'
    )
    FragmentProgram.Enabled = True
    VertexProgram.Code.Strings = (
      'uniform vec3 LightPosition;'
      'const float SpecularContribution = 0.3;'
      'const float DiffuseContribution = 1.0 - SpecularContribution;'
      'varying float LightIntensity;'
      'varying vec2 MCposition;'
      'void main()'
      '{'
      'vec3 ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);'
      'vec3 tnorm = normalize(gl_NormalMatrix * gl_Normal);'
      'vec3 lightVec = normalize(LightPosition - ecPosition);'
      'vec3 reflectVec = reflect(-lightVec, tnorm);'
      'vec3 viewVec = normalize(-ecPosition);'
      'float diffuse = max(dot(lightVec, tnorm), 0.0);'
      'float spec = 0.0;'
      'if (diffuse > 0.0)'
      '{'
      'spec = max(dot(reflectVec, viewVec), 0.0);'
      'spec = pow(spec, 16.0);'
      '}'
      'LightIntensity = DiffuseContribution * diffuse +'
      'SpecularContribution * spec;'
      'MCposition = gl_Vertex.xy;'
      'gl_Position = ftransform();'
      '}'
    )
    VertexProgram.Enabled = True
    GeometryProgram.Enabled = True
    GeometryProgram.InputPrimitiveType = gsInAdjTriangles
    GeometryProgram.OutputPrimitiveType = sOutTriangleStrip
    GeometryProgram.VerticesOut = 2
    ShaderStyle = ssHighLevel
    Left = 136
    Top = 440
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <    
      item
        Name = 'LibMaterial'
        Tag = 0
      end>
    Left = 40
    Top = 440
  end
end
