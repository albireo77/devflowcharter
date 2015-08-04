object DataTypesForm: TDataTypesForm
  Left = 240
  Top = 229
  Width = 336
  Height = 323
  Caption = 'Data Types'
  Color = clBtnFace
  AutoScroll = False
  Constraints.MaxWidth = 326
  Constraints.MinWidth = 326
  Constraints.MinHeight = 323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnDeactivate = FormDeactivate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgcTabs: TPageControl
    Width = 330
    Height = 275
    OnChanging = pgcTabsChanging
  end
end
