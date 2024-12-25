object GotoForm: TGotoForm
  Left = 459
  Top = 288
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Go To'
  ClientHeight = 127
  ClientWidth = 192
  Color = clWindow
  Ctl3D = False
  ParentFont = True
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object pnlOptions: TPanel
    Left = 16
    Top = 8
    Width = 161
    Height = 81
    BorderStyle = bsSingle
    TabOrder = 0
    object rbLine: TRadioButton
      Tag = 7
      Left = 16
      Top = 8
      Width = 49
      Height = 17
      Caption = 'Line'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbNextBookmark: TRadioButton
      Tag = 7
      Left = 16
      Top = 32
      Width = 137
      Height = 17
      Caption = 'Next Bookmark'
      TabOrder = 2
      TabStop = True
    end
    object edtNumber: TEdit
      Tag = 5
      Left = 72
      Top = 7
      Width = 41
      Height = 23
      NumbersOnly = True
      TabOrder = 1
      Text = '0'
    end
    object rbPrevBookmark: TRadioButton
      Tag = 7
      Left = 16
      Top = 56
      Width = 137
      Height = 17
      Caption = 'Previous Bookmark'
      TabOrder = 3
      TabStop = True
    end
  end
  object btnGoto: TButton
    Tag = 1
    Left = 16
    Top = 96
    Width = 161
    Height = 25
    Caption = 'Go To'
    Default = True
    TabOrder = 1
    TabStop = False
    OnClick = btnGotoClick
  end
end
