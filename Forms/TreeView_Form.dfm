object TreeViewForm: TTreeViewForm
  Left = 389
  Top = 133
  Width = 300
  Height = 577
  HorzScrollBar.Visible = False
  Caption = 'Project Explorer'
  Color = clBtnFace
  Constraints.MinWidth = 300
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  DesignSize = (
    284
    539)
  PixelsPerInch = 96
  TextHeight = 13
  object lblErrors: TLabel
    Tag = 6
    Left = 17
    Top = 514
    Width = 128
    Height = 21
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Errors'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblWarnings: TLabel
    Tag = 6
    Left = 152
    Top = 514
    Width = 145
    Height = 21
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Warnings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object tvExplorer: TTreeView
    Left = 1
    Top = 1
    Width = 283
    Height = 508
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinWidth = 283
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Default'
    Font.Style = []
    HideSelection = False
    Images = MainForm.ImageList1
    Indent = 19
    MultiSelectStyle = [msControlSelect, msShiftSelect, msVisibleOnly, msSiblingOnly]
    ParentFont = False
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 0
    OnChange = tvExplorerChange
    OnCustomDrawItem = tvExplorerCustomDrawItem
    OnGetImageIndex = tvExplorerGetImageIndex
    OnGetSelectedIndex = tvExplorerGetSelectedIndex
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 80
    Top = 120
    object miNextError: TMenuItem
      Tag = 2
      Caption = 'Next error/warning'
      ShortCut = 16472
      OnClick = miNextErrorClick
    end
    object miPrevError: TMenuItem
      Tag = 2
      Caption = 'Previous error/warning'
      ShortCut = 16474
      OnClick = miNextErrorClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miExpand: TMenuItem
      Tag = 2
      Caption = 'Expand recurrently'
      Enabled = False
      ShortCut = 16453
      OnClick = miExpandClick
    end
    object miCollapse: TMenuItem
      Tag = 2
      Caption = 'Collapse recurrently'
      Enabled = False
      ShortCut = 16451
      OnClick = miExpandClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miRemove: TMenuItem
      Tag = 2
      Caption = 'Remove'
      ShortCut = 16452
      OnClick = miRemoveClick
    end
    object miRefresh: TMenuItem
      Tag = 2
      Caption = 'Refresh'
      ShortCut = 16466
      OnClick = miRefreshClick
    end
  end
end
