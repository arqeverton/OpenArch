object ProjectInfoForm: TProjectInfoForm
  Left = 86
  Height = 492
  Top = 85
  Width = 586
  HorzScrollBar.Page = 576
  HorzScrollBar.Range = 576
  VertScrollBar.Page = 385
  VertScrollBar.Range = 385
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Informações do Projeto'
  ClientHeight = 492
  ClientWidth = 586
  FormStyle = fsMDIForm
  Position = poMainFormCenter
  ShowHint = True
  LCLVersion = '7.3'
  object pnBotton: TPanel
    Left = 0
    Height = 42
    Top = 450
    Width = 586
    Align = alBottom
    BevelOuter = bvLowered
    ClientHeight = 42
    ClientWidth = 586
    TabOrder = 0
    object btnOK: TButton
      Left = 8
      Height = 25
      Top = 8
      Width = 75
      Caption = 'OK'
      OnClick = btnOKClick
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 84
      Height = 25
      Top = 8
      Width = 77
      Caption = 'Cancelar'
      OnClick = btnCancelClick
      TabOrder = 1
    end
  end
  object TabInfoControl: TPageControl
    Left = 0
    Height = 450
    Top = 0
    Width = 586
    ActivePage = TabProjectInfo
    Align = alClient
    Font.Style = [fsBold]
    MultiLine = True
    ParentFont = False
    TabIndex = 0
    TabOrder = 1
    Options = [nboShowCloseButtons, nboMultiLine]
    object TabProjectInfo: TTabSheet
      Caption = 'Informações do Projeto'
      ClientHeight = 402
      ClientWidth = 578
      ParentFont = False
      object pnProjectInfo: TPanel
        Left = 0
        Height = 106
        Top = 296
        Width = 578
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 106
        ClientWidth = 578
        TabOrder = 0
        UseDockManager = False
        object BitBtnProjectInfoAdd: TBitBtn
          Left = 408
          Height = 25
          Top = 80
          Width = 80
          Align = alCustom
          Caption = 'Adicionar'
          OnClick = BitBtnProjectInfoAddClick
          TabOrder = 0
        end
        object BitBtnProjectInfoDel: TBitBtn
          Left = 488
          Height = 25
          Top = 80
          Width = 80
          Align = alCustom
          Caption = 'Remover'
          OnClick = BitBtnProjectInfoDelClick
          TabOrder = 1
        end
        object vleProjectInfo: TValueListEditor
          Left = 8
          Height = 72
          Top = 4
          Width = 562
          Align = alTop
          AlternateColor = clGradientInactiveCaption
          AutoAdvance = aaNone
          AutoEdit = False
          BorderSpacing.Left = 8
          BorderSpacing.Top = 4
          BorderSpacing.Right = 8
          DefaultColWidth = 255
          DefaultRowHeight = 17
          ExtendedSelect = False
          FixedCols = 0
          Flat = True
          RowCount = 1
          ScrollBars = ssVertical
          TabOrder = 2
          TitleStyle = tsNative
          OnSelectCell = vleProjectInfoSelectCell
          DisplayOptions = [doAutoColResize]
          DropDownRows = 20
          KeyOptions = [keyEdit]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          ColWidths = (
            272
            272
          )
        end
      end
      object lbProjectID: TLabeledEdit
        Left = 8
        Height = 23
        Top = 16
        Width = 104
        EditLabel.Height = 15
        EditLabel.Width = 104
        EditLabel.Caption = 'ID do Projeto'
        EditLabel.ParentColor = False
        TabOrder = 1
      end
      object lbProjectPhase: TLabeledEdit
        Left = 8
        Height = 23
        Top = 60
        Width = 208
        EditLabel.Height = 15
        EditLabel.Width = 208
        EditLabel.Caption = 'Fase do Projeto'
        EditLabel.ParentColor = False
        TabOrder = 2
      end
      object lbProjectName: TLabeledEdit
        Left = 120
        Height = 23
        Top = 16
        Width = 448
        EditLabel.Height = 15
        EditLabel.Width = 448
        EditLabel.Caption = 'Nome Projeto'
        EditLabel.ParentColor = False
        TabOrder = 3
      end
      object lbTemplateAuthor: TLabeledEdit
        Left = 224
        Height = 23
        Top = 60
        Width = 344
        EditLabel.Height = 15
        EditLabel.Width = 344
        EditLabel.Caption = 'Autor Template'
        EditLabel.ParentColor = False
        TabOrder = 4
      end
      object lbKeyWords: TLabeledEdit
        Left = 10
        Height = 23
        Top = 152
        Width = 558
        EditLabel.Height = 15
        EditLabel.Width = 558
        EditLabel.Caption = 'Palavras Chave'
        EditLabel.ParentColor = False
        TabOrder = 5
      end
      object labNotes: TLabel
        Left = 8
        Height = 15
        Top = 184
        Width = 56
        Caption = 'Anotações'
        ParentColor = False
      end
      object memNotes: TMemo
        Left = 8
        Height = 72
        Top = 200
        Width = 560
        ScrollBars = ssAutoVertical
        TabOrder = 6
      end
      object lbDescripition: TLabeledEdit
        Left = 8
        Height = 23
        Top = 104
        Width = 560
        EditLabel.Height = 15
        EditLabel.Width = 560
        EditLabel.Caption = 'Descrição'
        EditLabel.ParentColor = False
        TabOrder = 7
      end
      object labInfoAdd: TLabel
        Left = 8
        Height = 15
        Top = 280
        Width = 127
        Caption = 'Informações Adicionais:'
        ParentColor = False
      end
    end
    object TabSiteInfo: TTabSheet
      Caption = 'Informações do Terreno'
      ClientHeight = 402
      ClientWidth = 578
      object pnSiteInfo: TPanel
        Left = 0
        Height = 106
        Top = 296
        Width = 578
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 106
        ClientWidth = 578
        TabOrder = 0
        UseDockManager = False
        object vleSiteInfo: TValueListEditor
          Left = 8
          Height = 72
          Top = 4
          Width = 562
          Align = alTop
          AlternateColor = clGradientInactiveCaption
          AutoEdit = False
          BorderSpacing.Left = 8
          BorderSpacing.Top = 4
          BorderSpacing.Right = 8
          DefaultColWidth = 255
          DefaultRowHeight = 17
          FixedCols = 0
          Flat = True
          ParentFont = False
          RowCount = 1
          ScrollBars = ssVertical
          TabOrder = 0
          TitleStyle = tsNative
          OnSelectCell = vleSiteInfoSelectCell
          DisplayOptions = [doAutoColResize]
          KeyOptions = [keyEdit, keyAdd]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          Strings.Strings = (
            ''
          )
          ColWidths = (
            272
            272
          )
        end
        object BitBtnSiteInfoDel: TBitBtn
          Left = 487
          Height = 25
          Top = 80
          Width = 75
          Align = alCustom
          Caption = 'Remover'
          OnClick = BitBtnSiteInfoDelClick
          ParentFont = False
          TabOrder = 2
        end
        object BitBtnSiteInfoAdd: TBitBtn
          Left = 412
          Height = 25
          Top = 80
          Width = 75
          Align = alCustom
          Caption = 'Adicionar'
          OnClick = BitBtnSiteInfoAddClick
          ParentFont = False
          TabOrder = 1
        end
      end
      object lbSiteID: TLabeledEdit
        Left = 8
        Height = 23
        Top = 24
        Width = 96
        EditLabel.Height = 15
        EditLabel.Width = 96
        EditLabel.Caption = 'ID Terreno'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 1
      end
      object lbSiteName: TLabeledEdit
        Left = 112
        Height = 23
        Top = 24
        Width = 336
        EditLabel.Height = 15
        EditLabel.Width = 336
        EditLabel.Caption = 'Nome Terreno'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 2
      end
      object lbSiteArea: TLabeledEdit
        Left = 456
        Height = 23
        Top = 24
        Width = 112
        EditLabel.Height = 15
        EditLabel.Width = 112
        EditLabel.Caption = 'Área'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 3
        OnExit = lbSiteAreaExit
      end
      object Label3: TLabel
        Left = 8
        Height = 15
        Top = 56
        Width = 105
        Caption = 'Endereço Completo'
        ParentColor = False
        ParentFont = False
      end
      object memSiteFullAdress: TMemo
        Left = 8
        Height = 98
        Top = 72
        Width = 560
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object Label4: TLabel
        Left = 8
        Height = 15
        Top = 176
        Width = 51
        Caption = 'Descrição'
        ParentColor = False
        ParentFont = False
      end
      object memSiteDescripition: TMemo
        Left = 8
        Height = 88
        Top = 192
        Width = 560
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 5
      end
      object Label6: TLabel
        Left = 8
        Height = 15
        Top = 280
        Width = 127
        Caption = 'Informações Adicionais:'
        ParentColor = False
        ParentFont = False
      end
    end
    object TabBuildingInfo: TTabSheet
      Caption = 'Informações do Edifício'
      ClientHeight = 402
      ClientWidth = 578
      object pnBuildingInfo: TPanel
        Left = 0
        Height = 106
        Top = 296
        Width = 578
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 106
        ClientWidth = 578
        TabOrder = 0
        UseDockManager = False
        object vleBuildingInfo: TValueListEditor
          Left = 8
          Height = 72
          Top = 4
          Width = 562
          Align = alTop
          AlternateColor = clGradientInactiveCaption
          AutoEdit = False
          BorderSpacing.Left = 8
          BorderSpacing.Top = 4
          BorderSpacing.Right = 8
          DefaultColWidth = 255
          DefaultRowHeight = 17
          FixedCols = 0
          Flat = True
          RowCount = 1
          ScrollBars = ssVertical
          TabOrder = 0
          TitleStyle = tsNative
          OnSelectCell = vleBuildingInfoSelectCell
          DisplayOptions = [doAutoColResize]
          KeyOptions = [keyEdit]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          Strings.Strings = (
            ''
          )
          ColWidths = (
            272
            272
          )
        end
        object BitBtnBuildingInfoDel: TBitBtn
          Left = 491
          Height = 25
          Top = 80
          Width = 75
          Align = alCustom
          Caption = 'Remover'
          OnClick = BitBtnBuildingInfoDelClick
          TabOrder = 2
        end
        object BitBtnBuildingInfoAdd: TBitBtn
          Left = 416
          Height = 25
          Top = 80
          Width = 75
          Align = alCustom
          Caption = 'Adicionar'
          OnClick = BitBtnBuildingInfoAddClick
          TabOrder = 1
        end
      end
      object lbBuildingID: TLabeledEdit
        Left = 8
        Height = 23
        Top = 24
        Width = 80
        EditLabel.Height = 15
        EditLabel.Width = 80
        EditLabel.Caption = 'ID Edifício'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 1
      end
      object lbBuildingName: TLabeledEdit
        Left = 96
        Height = 23
        Top = 24
        Width = 472
        EditLabel.Height = 15
        EditLabel.Width = 472
        EditLabel.Caption = 'Nome do Edifício'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 2
      end
      object Label5: TLabel
        Left = 8
        Height = 15
        Top = 56
        Width = 51
        Caption = 'Descrição'
        ParentColor = False
        ParentFont = False
      end
      object memBuildingDescripition: TMemo
        Left = 8
        Height = 208
        Top = 72
        Width = 560
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object Label7: TLabel
        Left = 8
        Height = 15
        Top = 280
        Width = 127
        Caption = 'Informações Adicionais:'
        ParentColor = False
        ParentFont = False
      end
    end
    object TabDesignerInfo: TTabSheet
      Caption = 'Informações do Projetista'
      ClientHeight = 402
      ClientWidth = 578
      object pnDesignerInfo: TPanel
        Left = 0
        Height = 285
        Top = 117
        Width = 578
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 285
        ClientWidth = 578
        TabOrder = 0
        UseDockManager = False
        object vleDesignerInfo: TValueListEditor
          Left = 8
          Height = 250
          Top = 4
          Width = 562
          Align = alTop
          AlternateColor = clGradientInactiveCaption
          AutoEdit = False
          BorderSpacing.Left = 8
          BorderSpacing.Top = 4
          BorderSpacing.Right = 8
          DefaultColWidth = 255
          DefaultRowHeight = 17
          FixedCols = 0
          Flat = True
          ParentFont = False
          RowCount = 1
          ScrollBars = ssVertical
          TabOrder = 0
          TitleStyle = tsNative
          OnSelectCell = vleDesignerInfoSelectCell
          DisplayOptions = [doAutoColResize]
          KeyOptions = [keyEdit]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          Strings.Strings = (
            ''
          )
          ColWidths = (
            272
            272
          )
        end
        object BitBtnDesignerInfoDel: TBitBtn
          Left = 495
          Height = 25
          Top = 258
          Width = 75
          Align = alCustom
          Caption = 'Remover'
          OnClick = BitBtnDesignerInfoDelClick
          ParentFont = False
          TabOrder = 2
        end
        object BitBtnDesignerInfoAdd: TBitBtn
          Left = 420
          Height = 25
          Top = 258
          Width = 75
          Align = alCustom
          Caption = 'Adicionar'
          OnClick = BitBtnDesignerInfoAddClick
          ParentFont = False
          TabOrder = 1
        end
      end
      object lbDesignerID: TLabeledEdit
        Left = 8
        Height = 23
        Top = 24
        Width = 120
        EditLabel.Height = 15
        EditLabel.Width = 120
        EditLabel.Caption = 'ID Registro Projetista'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 1
      end
      object lbDesignerName: TLabeledEdit
        Left = 8
        Height = 23
        Top = 72
        Width = 560
        EditLabel.Height = 15
        EditLabel.Width = 560
        EditLabel.Caption = 'Nome Projetista'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 2
      end
      object Label8: TLabel
        Left = 8
        Height = 15
        Top = 100
        Width = 127
        Caption = 'Informações Adicionais:'
        ParentColor = False
        ParentFont = False
      end
    end
    object TabCustomerInfo: TTabSheet
      Caption = 'Informações do Cliente'
      ClientHeight = 402
      ClientWidth = 578
      object pnCustomerInfo: TPanel
        Left = 0
        Height = 282
        Top = 120
        Width = 578
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 282
        ClientWidth = 578
        TabOrder = 0
        UseDockManager = False
        object vleCustomerInfo: TValueListEditor
          Left = 8
          Height = 244
          Top = 4
          Width = 562
          Align = alTop
          AlternateColor = clGradientInactiveCaption
          AutoEdit = False
          BorderSpacing.Left = 8
          BorderSpacing.Top = 4
          BorderSpacing.Right = 8
          DefaultColWidth = 255
          DefaultRowHeight = 17
          FixedCols = 0
          Flat = True
          ParentFont = False
          RowCount = 1
          ScrollBars = ssVertical
          TabOrder = 0
          TitleStyle = tsNative
          OnSelectCell = vleCustomerInfoSelectCell
          DisplayOptions = [doAutoColResize]
          KeyOptions = [keyEdit]
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          Strings.Strings = (
            ''
          )
          ColWidths = (
            272
            272
          )
        end
        object BitBtnCustomerInfoDel: TBitBtn
          Left = 491
          Height = 25
          Top = 255
          Width = 75
          Align = alCustom
          Caption = 'Remover'
          OnClick = BitBtnCustomerInfoDelClick
          ParentFont = False
          TabOrder = 2
        end
        object BitBtnCustomerInfoAdd: TBitBtn
          Left = 416
          Height = 25
          Top = 255
          Width = 75
          Align = alCustom
          Caption = 'Adicionar'
          OnClick = BitBtnCustomerInfoAddClick
          ParentFont = False
          TabOrder = 1
        end
      end
      object lbCustomerID: TLabeledEdit
        Left = 8
        Height = 23
        Top = 24
        Width = 80
        EditLabel.Height = 15
        EditLabel.Width = 80
        EditLabel.Caption = 'ID Cliente'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 1
      end
      object lbCustomerName: TLabeledEdit
        Left = 104
        Height = 23
        Top = 24
        Width = 464
        EditLabel.Height = 15
        EditLabel.Width = 464
        EditLabel.Caption = 'Nome Cliente'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 2
      end
      object lbCustomerEmail: TLabeledEdit
        Left = 8
        Height = 23
        Top = 72
        Width = 560
        EditLabel.Height = 15
        EditLabel.Width = 560
        EditLabel.Caption = 'Email Cliente'
        EditLabel.ParentColor = False
        EditLabel.ParentFont = False
        ParentFont = False
        TabOrder = 3
      end
      object Label9: TLabel
        Left = 8
        Height = 15
        Top = 100
        Width = 124
        Caption = 'Informações Adicionais'
        ParentColor = False
        ParentFont = False
      end
    end
  end
end
