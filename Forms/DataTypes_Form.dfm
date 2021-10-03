object DataTypesForm: TDataTypesForm
  Left = 240
  Top = 229
  Width = 330
  Height = 323
  Caption = 'Data Types'
  Color = clBtnFace
  AutoScroll = False
  Constraints.MaxWidth = 330
  Constraints.MinWidth = 330
  Constraints.MinHeight = 323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgcTabs: TPageControl
    Width = 330
    Height = 275
    OnChanging = pgcTabsChanging
  end
end
