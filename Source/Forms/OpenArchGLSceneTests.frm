object FormTestes: TFormTestes
  Left = 430
  Height = 501
  Top = 256
  Width = 901
  Caption = 'FormTestes'
  ClientHeight = 501
  ClientWidth = 901
  LCLVersion = '7.3'
  object GLSceneViewer1: TGLSceneViewer
    Left = 374
    Height = 416
    Top = 32
    Width = 468
    Camera = GLCamera1
    Buffer.BackgroundColor = clWhite
    FieldOfView = 140
    TabOrder = 0
  end
  object GLScene1: TGLScene
    Left = 117
    Top = 122
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 75.705810546875
      TargetObject = GLPolygon1
      CameraStyle = csOrthogonal
      Position.Coordinates = {
        0000000000000000000040400000803F
      }
      object GLLightSource1: TGLLightSource
        ConstAttenuation = 1
        SpotCutOff = 180
      end
    end
    object GLPolygon1: TGLPolygon
      Material.Texture.ImageAlpha = tiaLuminance
      Material.Texture.MagFilter = maNearest
      Material.Texture.TextureWrap = twHorizontal
      Material.Texture.TextureWrapS = twClampToBorder
      Material.Texture.TextureWrapT = twClampToBorder
      Material.Texture.TextureWrapR = twClampToBorder
      Material.Texture.MappingMode = tmmCubeMapCamera
      Material.Texture.MappingQCoordinates.Coordinates = {
        0000003F0000003F000000000000803F
      }
      Material.Texture.EnvColor.Color = {
        6666263F6666263F6666263F0000803F
      }
      Material.Texture.Disabled = False
      Material.Texture.TextureCompareFunc = cfAlways
      Material.Texture.DepthTextureMode = dtmAlpha
      Material.TextureEx = <      
        item
          Texture.ImageClassName = 'TGLBlankImage'
          Texture.Image.ColorFormat = 6408
          Texture.ImageAlpha = tiaOpaque
          Texture.MagFilter = maNearest
          Texture.MinFilter = miLinear
          Texture.TextureMode = tmBlend
          Texture.TextureWrap = twNone
          Texture.FilteringQuality = tfAnisotropic
          TextureIndex = 1
          TextureOffset.Coordinates = {
            9A99D93F00000000000000000000803F
          }
          TextureScale.Coordinates = {
            0000803F00000000000000000000803F
          }
        end>
      Visible = False
      Nodes = <      
        item
        end      
        item
          X = 1
        end      
        item
          X = 1
          Y = 1
        end      
        item
          Y = 1
        end>
    end
    object GLLines1: TGLLines
      Visible = False
      AntiAliased = True
      LineColor.Color = {
        00000000000000000000000000000000
      }
      NodeColor.Color = {
        00000000000000000000000000000000
      }
      Nodes = <      
        item
          Color.Color = {
            14AE073F8FC2F53DD7A3F03E0000803F
          }
        end      
        item
          Y = 2
          Color.Color = {
            0000803FF8FEFE3E000000000000803F
          }
        end      
        item
          X = 2
          Y = 2
          Color.Color = {
            EBE0E03E9A93133FE4DB5B3F0000803F
          }
        end>
      NodesAspect = lnaInvisible
      Options = [loUseNodeColorForLines]
    end
    object GLDummyCube1: TGLDummyCube
      CubeSize = 1
      object GLDummyCube2: TGLDummyCube
        CubeSize = 1
      end
    end
    object GLLines2: TGLLines
      Position.Coordinates = {
        0000A0C000000000000000000000803F
      }
      LineColor.Color = {
        B1A8A83EB1A8A83EB1A8A83E0000803F
      }
      LinePattern = 3855
      Nodes = <      
        item
        end      
        item
          X = 10
        end>
      NodesAspect = lnaInvisible
      Options = []
    end
  end
  object GLMemoryViewer1: TGLMemoryViewer
    Camera = GLCamera1
    Left = 86
    Top = 310
  end
end
