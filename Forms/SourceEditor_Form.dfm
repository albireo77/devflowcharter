object EditorForm: TEditorForm
  Left = 544
  Top = 235
  Width = 425
  Height = 558
  Caption = 'Code Editor'
  Color = clBtnFace
  Constraints.MinHeight = 558
  Constraints.MinWidth = 225
  ParentFont = True
  FormStyle = fsStayOnTop
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    409
    500)
  PixelsPerInch = 96
  TextHeight = 13
  object memCodeEditor: TSynMemo
    Left = 0
    Top = 0
    Width = 418
    Height = 487
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    ParentCtl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentShowHint = False
    PopupMenu = pmPopMenu
    ShowHint = True
    TabOrder = 0
    TabStop = False
    OnDblClick = memCodeEditorDblClick
    OnDragDrop = memCodeEditorDragDrop
    OnDragOver = memCodeEditorDragOver
    OnMouseMove = memCodeEditorMouseMove
    BookMarkOptions.BookmarkImages = MainForm.ImageList1
    BookMarkOptions.EnableKeys = False
    BookMarkOptions.LeftMargin = -30
    BookMarkOptions.Xoffset = 30
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.RightOffset = 0
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    Lines.Strings = (
      'memCodeEditor')
    MaxUndo = 64
    Options = [eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoRightMouseMovesCursor, eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces]
    TabWidth = 3
    WantTabs = True
    OnChange = memCodeEditorChange
    OnGutterClick = memCodeEditorGutterClick
    OnStatusChange = memCodeEditorStatusChange
    OnPaintTransient = memCodeEditorPaintTransient
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object stbEditorBar: TStatusBar
    Left = 0
    Top = 481
    Width = 409
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
    UseSystemFont = False
  end
  object pmPopMenu: TPopupMenu
    OnPopup = pmPopMenuPopup
    Left = 72
    Top = 72
    object miUndo: TMenuItem
      Tag = 2
      Caption = 'Undo'
      OnClick = miUndoClick
    end
    object miRedo: TMenuItem
      Tag = 2
      Caption = 'Redo'
      OnClick = miUndoClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miCut: TMenuItem
      Tag = 2
      Caption = 'Cut'
      OnClick = miUndoClick
    end
    object miCopy: TMenuItem
      Tag = 2
      Caption = 'Copy'
      OnClick = miUndoClick
    end
    object miCopyRichText: TMenuItem
      Tag = 2
      Caption = 'Copy Special'
      OnClick = miUndoClick
    end
    object miPaste: TMenuItem
      Tag = 2
      Caption = 'Paste'
      OnClick = miUndoClick
    end
    object miPasteComment: TMenuItem
      Tag = 2
      Caption = 'Paste as Comment'
      OnClick = miUndoClick
    end
    object miRemove: TMenuItem
      Tag = 2
      Caption = 'Remove'
      OnClick = miUndoClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Tag = 2
      Caption = 'Select All'
      OnClick = miUndoClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miFindProj: TMenuItem
      Tag = 2
      Caption = 'Find in Project'
      OnClick = miFindProjClick
    end
  end
  object FindDialog: TFindDialog
    Tag = 3
    Options = [frDown, frHideWholeWord]
    OnFind = ReplaceDialogFind
    Left = 344
    Top = 280
  end
  object ReplaceDialog: TReplaceDialog
    Tag = 3
    Options = [frDown, frHideWholeWord, frDisableWholeWord]
    OnFind = ReplaceDialogFind
    OnReplace = ReplaceDialogReplace
    Left = 344
    Top = 240
  end
  object SynCppSyn1: TSynCppSyn
    DefaultFilter = 'C++ Files (*.c,*.cpp,*.h,*.hpp)|*.c;*.cpp;*.h;*.hpp'
    CommentAttri.Background = clWindow
    CommentAttri.Foreground = clGrayText
    NumberAttri.Background = clWindow
    NumberAttri.Foreground = clBackground
    FloatAttri.Background = clWindow
    FloatAttri.Foreground = clBackground
    StringAttri.Background = clWindow
    StringAttri.Foreground = clBackground
    CharAttri.Background = clWindow
    CharAttri.Foreground = clBackground
    Left = 320
    Top = 16
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Background = clWindow
    CommentAttri.Foreground = clGrayText
    DirectiveAttri.Background = clWindow
    DirectiveAttri.Foreground = clGrayText
    NumberAttri.Background = clWindow
    NumberAttri.Foreground = clBackground
    FloatAttri.Background = clWindow
    FloatAttri.Foreground = clBackground
    HexAttri.Background = clWindow
    HexAttri.Foreground = clBackground
    StringAttri.Background = clWindow
    StringAttri.Foreground = clBackground
    CharAttri.Background = clWindow
    CharAttri.Foreground = clBackground
    DelphiVersion = dvDelphi6
    Left = 352
    Top = 16
  end
  object SynEditPrint1: TSynEditPrint
    Tag = 3
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 15.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabWidth = 8
    Color = clWhite
    Left = 280
    Top = 240
  end
  object MainMenu1: TMainMenu
    Left = 104
    Top = 72
    object miProgram: TMenuItem
      Tag = 2
      Caption = 'Program'
      object miSave: TMenuItem
        Tag = 2
        Caption = 'Save...'
        ShortCut = 16467
        OnClick = miSaveClick
      end
      object miCompile: TMenuItem
        Tag = 2
        Caption = 'Compile...'
        OnClick = miCompileClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miPrint: TMenuItem
        Tag = 2
        Caption = 'Print...'
        ShortCut = 16464
        OnClick = miPrintClick
      end
    end
    object miEdit: TMenuItem
      Tag = 2
      Caption = 'Edit'
      object miFind: TMenuItem
        Tag = 2
        Caption = 'Find...'
        ShortCut = 16454
        OnClick = miFindClick
      end
      object miReplace: TMenuItem
        Tag = 2
        Caption = 'Replace...'
        ShortCut = 16466
        OnClick = miFindClick
      end
      object miGoto: TMenuItem
        Tag = 2
        Caption = 'Go To...'
        ShortCut = 16455
        OnClick = miGotoClick
      end
    end
    object miView: TMenuItem
      Tag = 2
      Caption = 'View'
      object miStatusBar: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Statusbar'
        OnClick = miRichTextClick
      end
      object miScrollbars: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Scrollbars'
        OnClick = miRichTextClick
      end
      object miGutter: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Gutter'
        OnClick = miRichTextClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miRichText: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Highlight Code'
        ShortCut = 16456
        OnClick = miRichTextClick
      end
      object miCodeFolding: TMenuItem
        Tag = 2
        Caption = 'Code Folding'
        object miCodeFoldingEnable: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Enabled'
          ShortCut = 16452
          OnClick = miRichTextClick
        end
        object miIndentGuides: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Indent Guides'
          ShortCut = 16457
          OnClick = miRichTextClick
        end
        object miUnCollapseAll: TMenuItem
          Tag = 2
          Caption = 'Expand All'
          ShortCut = 16453
          OnClick = miCollapseAllClick
        end
        object miCollapseAll: TMenuItem
          Tag = 2
          Caption = 'Collapse All'
          ShortCut = 16450
          OnClick = miCollapseAllClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miRegenerate: TMenuItem
        Tag = 2
        Caption = 'Regenerate'
        ShortCut = 116
        OnClick = miRegenerateClick
      end
    end
    object miHelp: TMenuItem
      Tag = 2
      Caption = 'Help'
      OnClick = miHelpClick
    end
  end
  object SaveDialog2: TSaveDialog
    Tag = 3
    DefaultExt = '*.pas'
    Filter = 'Plik Pascala (*.pas)|*.pas|Plik C (*.c)|*.c|Dokument RTF|*.rtf'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 312
    Top = 240
  end
  object SaveDialog1: TSaveDialog
    Tag = 3
    Left = 312
    Top = 280
  end
  object SynExporterRTF1: TSynExporterRTF
    Color = clWindow
    DefaultFilter = 'Rich Text Format Documents (*.rtf)|*.rtf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = False
    Left = 216
    Top = 16
  end
  object SynExporterHTML1: TSynExporterHTML
    Color = clWindow
    DefaultFilter = 'HTML Documents (*.htm;*.html)|*.htm;*.html'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = False
    Left = 248
    Top = 16
  end
end
