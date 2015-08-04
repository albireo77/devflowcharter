object MemoEditorForm: TMemoEditorForm
  Left = 286
  Top = 218
  Width = 280
  Height = 182
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'MemoEditorForm'
  Color = clBtnFace
  Constraints.MinHeight = 182
  Constraints.MinWidth = 280
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    264
    144)
  PixelsPerInch = 96
  TextHeight = 13
  object memEditor: TMemo
    Left = 0
    Top = 0
    Width = 264
    Height = 144
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'memEditor')
    ParentFont = False
    TabOrder = 0
  end
  object btnOK: TButton
    Tag = 1
    Left = 105
    Top = 115
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Tag = 1
    Left = 185
    Top = 115
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnOKClick
  end
end
