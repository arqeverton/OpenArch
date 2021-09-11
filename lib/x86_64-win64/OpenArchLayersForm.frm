object LayersForm: TLayersForm
  Left = 430
  Height = 548
  Top = 256
  Width = 550
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Camadas'
  ClientHeight = 548
  ClientWidth = 550
  FormStyle = fsMDIForm
  OnClose = FormClose
  Position = poMainFormCenter
  LCLVersion = '7.3'
  object pnBotton: TPanel
    Left = 0
    Height = 42
    Top = 506
    Width = 550
    Align = alBottom
    ClientHeight = 42
    ClientWidth = 550
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
  object pnLayersGroup: TPanel
    Left = 0
    Height = 58
    Top = 0
    Width = 550
    Align = alTop
    ClientHeight = 58
    ClientWidth = 550
    TabOrder = 1
    object labLayersGroup: TLabel
      Left = 8
      Height = 15
      Top = 3
      Width = 118
      Caption = 'Conjunto de Camadas'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object cbboxLayerSet: TComboBox
      Left = 8
      Height = 23
      Top = 25
      Width = 324
      ItemHeight = 15
      OnChange = cbboxLayerSetChange
      ReadOnly = True
      Style = csDropDownList
      TabOrder = 0
    end
    object btnNewLayersGroup: TButton
      Left = 339
      Height = 23
      Top = 2
      Width = 100
      Caption = 'Novo'
      OnClick = btnNewLayersGroupClick
      TabOrder = 1
    end
    object btnUpdateLayersGroup: TButton
      Left = 339
      Height = 23
      Top = 26
      Width = 100
      Caption = 'Atualizar'
      OnClick = btnUpdateLayersGroupClick
      TabOrder = 2
    end
    object btnDelLayersGroup: TButton
      Left = 441
      Height = 23
      Top = 2
      Width = 100
      Caption = 'Remover'
      OnClick = btnDelLayersGroupClick
      TabOrder = 3
    end
    object btnEditLayerGroup: TButton
      Left = 441
      Height = 23
      Top = 26
      Width = 99
      Caption = 'Editar'
      OnClick = btnEditLayerGroupClick
      TabOrder = 4
    end
  end
  object pnLayers: TPanel
    Left = 0
    Height = 448
    Top = 58
    Width = 550
    Align = alClient
    ClientHeight = 448
    ClientWidth = 550
    TabOrder = 2
    object btnAddLayer: TButton
      Left = 9
      Height = 32
      Top = 368
      Width = 75
      Caption = 'Novo'
      OnClick = btnAddLayerClick
      TabOrder = 0
    end
    object btnDelLayer: TButton
      Left = 9
      Height = 32
      Top = 405
      Width = 75
      Caption = 'Remover'
      OnClick = btnDelLayerClick
      TabOrder = 1
    end
    object btnSellectAll: TBitBtn
      Left = 128
      Height = 32
      Top = 368
      Width = 100
      OnClick = btnSellectAllClick
      TabOrder = 2
    end
    object btnLockedAll: TBitBtn
      Left = 232
      Height = 32
      Top = 368
      Width = 100
      OnClick = btnLockedAllClick
      TabOrder = 3
    end
    object btnVisibleSelect: TBitBtn
      Left = 336
      Height = 32
      Top = 368
      Width = 100
      OnClick = btnVisibleSelectClick
      TabOrder = 4
    end
    object btnPrintableSelect: TBitBtn
      Left = 440
      Height = 32
      Top = 368
      Width = 100
      OnClick = btnPrintableSelectClick
      TabOrder = 5
    end
    object btnCancelSellect: TBitBtn
      Left = 128
      Height = 32
      Top = 405
      Width = 100
      OnClick = btnCancelSellectClick
      TabOrder = 6
    end
    object btnCancelLockedSelect: TBitBtn
      Left = 232
      Height = 32
      Top = 405
      Width = 100
      OnClick = btnCancelLockedSelectClick
      TabOrder = 7
    end
    object btnCancelVisibleSelect: TBitBtn
      Left = 336
      Height = 32
      Top = 405
      Width = 100
      OnClick = btnCancelVisibleSelectClick
      TabOrder = 8
    end
    object btnCancelPrintableSelect: TBitBtn
      Left = 440
      Height = 32
      Top = 405
      Width = 100
      OnClick = btnCancelPrintableSelectClick
      TabOrder = 9
    end
    object stgLayers: TStringGrid
      Left = 9
      Height = 355
      Top = 5
      Width = 532
      Align = alTop
      BorderSpacing.Left = 8
      BorderSpacing.Top = 4
      BorderSpacing.Right = 8
      ColCount = 4
      ColumnClickSorts = True
      Columns = <      
        item
          Title.Font.Color = clBlack
          Title.Font.Style = [fsBold]
          Title.Caption = 'L'
          Width = 20
        end      
        item
          Title.Font.Style = [fsBold]
          Title.Caption = 'V'
          Width = 20
        end      
        item
          Title.Font.Style = [fsBold]
          Title.Caption = 'P'
          Width = 20
        end      
        item
          Title.Font.Style = [fsBold]
          Title.Caption = 'Nome da Camada'
          Width = 450
        end>
      FixedColor = clWindow
      FixedCols = 0
      Flat = True
      Font.Color = clBlack
      Options = [goFixedVertLine, goRangeSelect, goEditing, goRowSelect, goSmoothScroll]
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 10
      TitleStyle = tsNative
      OnDrawCell = stgLayersDrawCell
      OnMouseDown = stgLayersMouseDown
      OnSelectCell = stgLayersSelectCell
      OnValidateEntry = stgLayersValidateEntry
    end
  end
end
