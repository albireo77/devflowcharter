{
   Copyright (C) 2006 The devFlowcharter project.
   The initial author of this file is Michal Domagala.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}



unit Settings_Form;

interface

uses
   Windows, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SysUtils, Classes,
   Graphics, Base_Form;
   
type
  TSettingsForm = class(TBaseForm)
    btnSaveSettings: TButton;
    OpenDialog: TOpenDialog;
    gbMiscSettings: TGroupBox;
    gbProgLang: TGroupBox;
    btnDiscardSettings: TButton;
    gbParseSettings: TGroupBox;
    chkParseInput: TCheckBox;
    chkParseOutput: TCheckBox;
    chkParseAssign: TCheckBox;
    chkParseMAssign: TCheckBox;
    chkParseCondition: TCheckBox;
    ColorDialog: TColorDialog;
    chkParseRoutine: TCheckBox;
    gbEditorSettings: TGroupBox;
    chkParseFor: TCheckBox;
    btnDefaultSettings: TButton;
    gbEditorColors: TGroupBox;
    pnlEditorActiveLine: TPanel;
    lblEditorActiveLine: TLabel;
    pnlEditorGutter: TPanel;
    lblEditorGutter: TLabel;
    pnlEditorFont: TPanel;
    lblEditorFont: TLabel;
    pnlEditorBkg: TPanel;
    lblEditorBkg: TLabel;
    pnlEditorNumber: TPanel;
    lblEditorDigit: TLabel;
    lblEditorString: TLabel;
    pnlEditorString: TPanel;
    lblEditorSelect: TLabel;
    pnlEditorSelect: TPanel;
    lblEditorComment: TLabel;
    pnlEditorComment: TPanel;
    lblIndent: TLabel;
    edtEditorIndent: TEdit;
    edtCompiler: TEdit;
    lblCompiler: TLabel;
    btnBrowseCompilers: TButton;
    cbLanguage: TComboBox;
    lblEditorBracket: TLabel;
    pnlEditorBracket: TPanel;
    gbTranslation: TGroupBox;
    lblFile: TLabel;
    edtTranslateFile: TEdit;
    btnBrowseScripts: TButton;
    chkParseCase: TCheckBox;
    gbFlowchartSettings: TGroupBox;
    lblDesktop: TLabel;
    pnlDesktop: TPanel;
    lblBlockColor: TLabel;
    pnlFill: TPanel;
    imgColors: TImage;
    lblFontColor: TLabel;
    pnlFont: TPanel;
    gbPrintSettings: TGroupBox;
    chkMultiPrint: TCheckBox;
    chkMultiPrintHorz: TCheckBox;
    gbPrintMargins: TGroupBox;
    edtMarginLeft: TEdit;
    edtMarginRight: TEdit;
    edtMarginTop: TEdit;
    edtMarginBottom: TEdit;
    lblMarginLeft: TLabel;
    lblMarginRight: TLabel;
    lblMarginTop: TLabel;
    lblMarginBottom: TLabel;
    chkEnableDBuffer: TCheckBox;
    chkShowFuncLabels: TCheckBox;
    chkParseReturn: TCheckBox;
    chkConfirmRemove: TCheckBox;
    chkValidateConsts: TCheckBox;
    edtFontName: TEdit;
    FontDialog: TFontDialog;
    chkAutoSelectCode: TCheckBox;
    lblInterGen: TLabel;
    edtCompilerNoMain: TEdit;
    lblCompilerNoMain: TLabel;
    chkAutoUpdateCode: TCheckBox;
    lblFontSize: TLabel;
    cbFontSize: TComboBox;
    chkShowBlockLabels: TCheckBox;
    procedure btnBrowseCCompClick(Sender: TObject);
    procedure CloseFormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlFillClick(Sender: TObject);
    procedure btnDefaultSettingsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure btnBrowseScriptsClick(Sender: TObject);
    procedure imgColorsClick(Sender: TObject);
    procedure Localize(const AList: TStringList); override;
    procedure chkMultiPrintClick(Sender: TObject);
    procedure edtMarginLeftKeyPress(Sender: TObject; var Key: Char);
    procedure ResetForm; override;
    procedure edtFontNameClick(Sender: TObject);
    procedure SetCbFontSize(const AFontSize: integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  NONE_SELECTED    = 0;
  ELLIPSE_SELECTED = 1;
  INOUT_SELECTED   = 2;
  DIAMOND_SELECTED = 3;
  RECT_SELECTED    = 4;
  ROUTINE_SELECTED = 5;
  RSIGN_SELECTED   = 6;
  FOLDER_SELECTED  = 7;

var
  SettingsForm: TSettingsForm;

implementation

uses
   ApplicationCommon, StrUtils;

{$R *.dfm}


procedure TSettingsForm.Localize(const AList: TStringList);
var
   lval: integer;
begin
   lblInterGen.Left := cbLanguage.Left + cbLanguage.Width + 10;
   lblCompiler.Left := 7;
   edtCompiler.Left := lblCompiler.Width + lblCompiler.Left + 5;
   edtCompiler.Width := 449 - edtCompiler.Left;
   lblCompilerNoMain.Left := 7;
   edtCompilerNoMain.Left := lblCompilerNoMain.Width + lblCompilerNoMain.Left + 5;
   edtCompilerNoMain.Width := 449 - edtCompilerNoMain.Left;
   edtTranslateFile.Left := lblFile.Width + lblFile.Left + 5;;
   edtTranslateFile.Width := 449 - edtTranslateFile.Left;
   lval := lblDesktop.Width;
   if lval < lblBlockColor.Width then
      lval := lblBlockColor.Width;
   if lval < lblFontColor.Width then
      lval := lblFontColor.Width;
   Inc(lval, lblDesktop.Left+10);
   pnlDesktop.Left := lval;
   pnlFill.Left := lval;
   pnlFont.Left := lval;
   edtCompiler.Hint := AnsiReplaceStr(AList.Values['edtCompilerHint'], '##', CRLF);
   edtCompilerNoMain.Hint := AnsiReplaceStr(AList.Values['edtCompilerNoMainHint'], '##', CRLF);
   chkEnableDBuffer.Hint := AnsiReplaceStr(AList.Values['chkEnableDBufferHint'], '##', CRLF);
   inherited Localize(AList);
end;

procedure TSettingsForm.ResetForm;
begin
{}
end;

procedure TSettingsForm.btnBrowseCCompClick(Sender: TObject);
begin
    with OpenDialog do
    begin
       Filter := i18Manager.GetString('ExeFilesFilter');
       DefaultExt := '*.exe';
       FileName := '';
       if Execute then
          edtCompiler.Text := FileName;
    end;
end;

procedure TSettingsForm.CloseFormClick(Sender: TObject);
begin
    if Sender = btnSaveSettings then
       GSettings.LoadFromForm;
    Close;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
   GSettings.SetForm;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
   imgColors.Canvas.Brush.Color := gbFlowchartSettings.Color;
   imgColors.Canvas.FillRect(imgColors.Canvas.ClipRect);
   GInfra.GetLangNames(cbLanguage.Items);
   cbLanguage.ItemIndex := GInfra.GetLangIndex(GInfra.CurrentLang.Name);
end;

procedure TSettingsForm.pnlFillClick(Sender: TObject);
begin
   if ColorDialog.Execute then
      TPanel(Sender).Color := ColorDialog.Color;
end;

procedure TSettingsForm.btnDefaultSettingsClick(Sender: TObject);
begin
   GSettings.SetDefaultForm;
end;

procedure TSettingsForm.cbLanguageChange(Sender: TObject);
begin
   GSettings.ProtectFields;
end;

procedure TSettingsForm.btnBrowseScriptsClick(Sender: TObject);
begin
    with OpenDialog do
    begin
       Filter := i18Manager.GetString('LngFilesFilter');
       DefaultExt := '*.lng';
       FileName := '';
       if Execute then
          edtTranslateFile.Text := FileName;
    end;
end;

procedure TSettingsForm.imgColorsClick(Sender: TObject);
var
   lPoint: TPoint;
begin

   imgColors.Tag := NONE_SELECTED;
   lPoint := imgColors.ScreenToClient(Mouse.CursorPos);

   if PtInRect(Rect(10, 10, 60, 35), lPoint) then
      imgColors.Tag := ELLIPSE_SELECTED
   else if PtInRect(Rect(10, 45, 60, 65), lPoint) then
      imgColors.Tag := INOUT_SELECTED
   else if PtInRect(Rect(75, 13, 125, 63), lPoint) then
      imgColors.Tag := DIAMOND_SELECTED
   else if PtInRect(Rect(140, 10, 190, 35), lPoint) then
      imgColors.Tag := RECT_SELECTED
   else if PtInRect(Rect(140, 40, 190, 65), lPoint) then
      imgColors.Tag := ROUTINE_SELECTED
   else if PtInRect(Rect(205, 10, 252, 34), lPoint) then
      imgColors.Tag := RSIGN_SELECTED
   else if PtInRect(Rect(205, 40, 255, 65), lPoint) then
      imgColors.Tag := FOLDER_SELECTED;

   if (imgColors.Tag <> NONE_SELECTED) and ColorDialog.Execute then
   begin
      imgColors.Canvas.Brush.Color := ColorDialog.Color;
      case imgColors.Tag of
         ELLIPSE_SELECTED: lPoint := Point(35, 22);
         INOUT_SELECTED:   lPoint := Point(35, 55);
         DIAMOND_SELECTED: lPoint := Point(100, 38);
         RECT_SELECTED:    lPoint := Point(165, 22);
         RSIGN_SELECTED:   lPoint := Point(229, 22);
         ROUTINE_SELECTED:
         begin
            lPoint := Point(165, 52);
            imgColors.Canvas.FloodFill(143, 42, clBlack, fsBorder);
            imgColors.Canvas.FloodFill(187, 42, clBlack, fsBorder);
         end;
         FOLDER_SELECTED:
         begin
            lPoint := Point(230, 52);
            imgColors.Canvas.FloodFill(206, 41, clBlack, fsBorder);
         end;
      end;
      imgColors.Canvas.FloodFill(lPoint.X, lPoint.Y, clBlack, fsBorder);
   end;

end;

procedure TSettingsForm.chkMultiPrintClick(Sender: TObject);
begin
   chkMultiPrintHorz.Enabled := chkMultiPrint.Checked;
   if not chkMultiPrint.Checked then
      chkMultiPrintHorz.Checked := false;
end;

procedure TSettingsForm.edtMarginLeftKeyPress(Sender: TObject;
  var Key: Char);
begin
   if not (Key in [#8, '0'..'9']) then
      Key := #0;
end;

procedure TSettingsForm.edtFontNameClick(Sender: TObject);
begin
   if FontDialog.Execute then
      edtFontName.Text := FontDialog.Font.Name;
end;

procedure TSettingsForm.SetCbFontSize(const AFontSize: integer);
var
   i: integer;
begin
   i := cbFontSize.Items.IndexOf(IntToStr(AFontSize));
   if i = -1 then
      i := 0;
   cbFontSize.ItemIndex := i;
end;

end.
