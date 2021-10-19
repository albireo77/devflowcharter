object PageControlForm: TPageControlForm
  Left = 604
  Top = 348
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'PageControl'
  ClientHeight = 517
  ClientWidth = 407
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMode = pmExplicit
  OnDeactivate = FormDeactivate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object pgcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 407
    Height = 517
    Align = alClient
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    OwnerDraw = True
    ParentDoubleBuffered = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TabWidth = 65
    OnChange = pgcTabsChange
    OnDragDrop = pgcTabsDragDrop
    OnDragOver = pgcTabsDragOver
    OnDrawTab = pgcTabsDrawTab
    OnMouseDown = pgcTabsMouseDown
    OnMouseLeave = pgcTabsMouseLeave
    OnMouseMove = pgcTabsMouseMove
  end
  object MainMenu1: TMainMenu
    Left = 280
    object miAction: TMenuItem
      Tag = 2
      Caption = 'Action'
      OnClick = miActionClick
      object miAdd: TMenuItem
        Tag = 2
        Caption = 'Add'
        OnClick = miAddClick
      end
      object miRemove: TMenuItem
        Tag = 2
        Caption = 'Remove'
        OnClick = miRemoveClick
      end
      object miRemoveAll: TMenuItem
        Tag = 2
        Caption = 'Remove All'
        OnClick = miRemoveAllClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miImport: TMenuItem
        Tag = 2
        Caption = 'Import...'
        OnClick = miImportClick
      end
      object miExport: TMenuItem
        Tag = 2
        Caption = 'Export...'
        OnClick = miExportClick
      end
      object miExportAll: TMenuItem
        Tag = 2
        Caption = 'Export All...'
        OnClick = miExportAllClick
      end
    end
  end
end
