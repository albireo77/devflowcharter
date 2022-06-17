object NavigatorForm: TNavigatorForm
  Left = 211
  Top = 230
  AlphaBlend = True
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'NavigatorForm'
  ClientHeight = 589
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    344
    589)
  PixelsPerInch = 96
  TextHeight = 13
  object scbAlphaVal: TScrollBar
    Left = 310
    Top = 9
    Width = 17
    Height = 121
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    Constraints.MaxWidth = 17
    Ctl3D = False
    Kind = sbVertical
    LargeChange = 5
    Max = 255
    Min = 50
    PageSize = 0
    ParentBiDiMode = False
    ParentCtl3D = False
    Position = 50
    TabOrder = 1
    TabStop = False
    OnChange = scbAlphaValChange
  end
  object chkAlphaVisible: TCheckBox
    Tag = 8
    Left = 312
    Top = 9
    Width = 12
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'chkAlphaVisible'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    OnClick = chkAlphaVisibleClick
  end
end
