object SelectImportForm: TSelectImportForm
  Left = 0
  Top = 0
  Width = 279
  Height = 414
  HorzScrollBar.Visible = False
  VertScrollBar.Margin = 5
  VertScrollBar.Tracking = True
  AutoScroll = True
  BorderIcons = [biSystemMenu]
  Caption = 'Import'
  Color = clWindow
  Constraints.MaxWidth = 279
  Constraints.MinWidth = 279
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object pnlImports: TPanel
    Left = 24
    Top = 22
    Width = 217
    Height = 305
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
  end
  object btnOk: TButton
    Tag = 1
    Left = 145
    Top = 335
    Width = 41
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Tag = 1
    Left = 192
    Top = 335
    Width = 49
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object chkSelectAll: TCheckBox
    Tag = 8
    Left = 24
    Top = 335
    Width = 115
    Height = 17
    Caption = 'Select All'
    TabOrder = 3
    OnClick = chkSelectAllClick
  end
end
