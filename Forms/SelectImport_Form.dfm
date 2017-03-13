object SelectImportForm: TSelectImportForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Import'
  ClientHeight = 368
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlImports: TPanel
    Left = 24
    Top = 24
    Width = 217
    Height = 305
    BorderStyle = bsSingle
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
