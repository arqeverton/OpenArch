object StoriesForm: TStoriesForm
  Left = 430
  Height = 491
  Top = 256
  Width = 468
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Pavimentos'
  ClientHeight = 491
  ClientWidth = 468
  FormStyle = fsMDIForm
  OnClose = FormClose
  Position = poMainFormCenter
  LCLVersion = '7.3'
  object pnBotton: TPanel
    Left = 0
    Height = 42
    Top = 449
    Width = 468
    Align = alBottom
    BevelInner = bvLowered
    ClientHeight = 42
    ClientWidth = 468
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
  object stgStories: TStringGrid
    Left = 8
    Height = 404
    Top = 4
    Width = 452
    Align = alTop
    BorderSpacing.Left = 8
    BorderSpacing.Top = 4
    BorderSpacing.Right = 8
    ColRowDragIndicatorColor = 15850720
    Columns = <    
      item
        Alignment = taCenter
        Font.Color = clBlack
        Font.Style = [fsBold]
        ReadOnly = True
        Title.Color = clWindow
        Title.Font.Style = [fsBold]
        Title.Caption = 'Nº'
        Width = 20
      end    
      item
        Title.Font.Style = [fsBold]
        Title.Caption = 'Nome do Pavimento'
        Width = 250
      end    
      item
        Title.Font.Style = [fsBold]
        Title.Caption = 'Elevação'
        Width = 60
      end    
      item
        Title.Font.Color = clBlack
        Title.Font.Style = [fsBold]
        Title.Caption = 'Altura'
        Width = 60
      end    
      item
        ButtonStyle = cbsCheckboxColumn
        Title.Font.Color = clBlack
        Title.Font.Style = [fsBold]
        Title.Caption = '^---^'
        Width = 40
      end>
    FixedColor = clScrollBar
    FixedCols = 0
    Flat = True
    Font.Color = clBlack
    Options = [goFixedVertLine, goRangeSelect, goEditing, goRowSelect, goSmoothScroll]
    ParentFont = False
    RowCount = 1
    ScrollBars = ssVertical
    TabOrder = 1
    TitleStyle = tsNative
    OnCheckboxToggled = stgStoriesCheckboxToggled
    OnDrawCell = stgStoriesDrawCell
    OnMouseDown = stgStoriesMouseDown
    OnSelectCell = stgStoriesSelectCell
    OnValidateEntry = stgStoriesValidateEntry
  end
  object btnAddStoryDown: TButton
    Left = 152
    Height = 25
    Hint = 'Insere um Pavimento acima do selecionado'
    Top = 416
    Width = 100
    Align = alCustom
    Caption = 'Inserir Abaixo'
    OnClick = btnAddStoryDownClick
    TabOrder = 2
  end
  object btnAddStoryUp: TButton
    Left = 256
    Height = 25
    Hint = 'Insere um Pavimento Abaixo do Selecionado'
    Top = 416
    Width = 100
    Align = alCustom
    Caption = 'Inserir Acima'
    OnClick = btnAddStoryUpClick
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object btnDelStory: TButton
    Left = 360
    Height = 25
    Hint = 'Apaga o Pavimento Selecionado'
    Top = 416
    Width = 100
    Align = alCustom
    Caption = 'Apagar'
    OnClick = btnDelStoryClick
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
end
