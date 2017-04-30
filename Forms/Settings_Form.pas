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
  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, System.Classes, System.Types,
  Base_Form, Settings;

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
    imgShapes: TImage;
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
    edtCompilerNoMain: TEdit;
    lblCompilerNoMain: TLabel;
    chkAutoUpdateCode: TCheckBox;
    lblFontSize: TLabel;
    cbFontSize: TComboBox;
    chkShowBlockLabels: TCheckBox;
    cbFileEncoding: TComboBox;
    lblFileEncoding: TLabel;
    procedure btnBrowseCCompClick(Sender: TObject);
    procedure CloseFormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlFillClick(Sender: TObject);
    procedure btnDefaultSettingsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure btnBrowseScriptsClick(Sender: TObject);
    procedure imgShapesClick(Sender: TObject);
    procedure Localize(const AList: TStringList); override;
    procedure chkMultiPrintClick(Sender: TObject);
    procedure edtMarginLeftKeyPress(Sender: TObject; var Key: Char);
    procedure ResetForm; override;
    procedure edtFontNameClick(Sender: TObject);
  private
    procedure SetCbFontSize(const AFontSize: integer);
    procedure SetCbFileEncoding(const AFileEncoding: string);
    procedure FillShape(const idx: integer; const AColor: TColor);
    procedure DrawShapes(ASettings: TSettings);
    procedure FillAllShapes(const AColor: TColor);
  public
    procedure SetDefault;
    procedure ProtectFields;
    procedure SetSettings(ASettings: TSettings);
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
   System.StrUtils, System.SysUtils, ApplicationCommon, LangDefinition;

const
   SHAPE_BORDER_COLOR = clBlack;
   SHAPE_POINTS: array[0..6] of TPoint = ((X:35;  Y:22),    // ellipse
                                          (X:35;  Y:55),    // in out
                                          (X:100; Y:38),    // diamond
                                          (X:165; Y:22),    // rectangle
                                          (X:229; Y:22),    // roadsign
                                          (X:165; Y:52),    // routine
                                          (X:230; Y:52));   // folder

{$R *.dfm}

procedure TSettingsForm.Localize(const AList: TStringList);
var
   val: integer;
begin
   lblFileEncoding.Left := cbFileEncoding.Left - lblFileEncoding.Width - 5;
   lblCompiler.Left := 7;
   edtCompiler.Left := lblCompiler.Width + lblCompiler.Left + 5;
   edtCompiler.Width := 449 - edtCompiler.Left;
   lblCompilerNoMain.Left := 7;
   edtCompilerNoMain.Left := lblCompilerNoMain.Width + lblCompilerNoMain.Left + 5;
   edtCompilerNoMain.Width := 449 - edtCompilerNoMain.Left;
   edtTranslateFile.Left := lblFile.Width + lblFile.Left + 5;;
   edtTranslateFile.Width := 449 - edtTranslateFile.Left;
   val := lblDesktop.Width;
   if val < lblBlockColor.Width then
      val := lblBlockColor.Width;
   if val < lblFontColor.Width then
      val := lblFontColor.Width;
   Inc(val, lblDesktop.Left+10);
   pnlDesktop.Left := val;
   pnlFill.Left := val;
   pnlFont.Left := val;
   edtCompiler.Hint := ReplaceStr(AList.Values['edtCompilerHint'], '##', CRLF);
   edtCompilerNoMain.Hint := ReplaceStr(AList.Values['edtCompilerNoMainHint'], '##', CRLF);
   chkEnableDBuffer.Hint := ReplaceStr(AList.Values['chkEnableDBufferHint'], '##', CRLF);
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
   imgShapes.Canvas.Brush.Color := gbFlowchartSettings.Color;
   imgShapes.Canvas.FillRect(imgShapes.Canvas.ClipRect);
   GInfra.GetLangNames(cbLanguage.Items);
   cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(GInfra.CurrentLang.Name);
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

procedure TSettingsForm.imgShapesClick(Sender: TObject);
var
   idx: integer;
   pnt: TPoint;
begin
   idx := -1;
   pnt := imgShapes.ScreenToClient(Mouse.CursorPos);
   if PtInRect(Rect(10, 10, 60, 35), pnt) then
      idx := 0
   else if PtInRect(Rect(10, 45, 60, 65), pnt) then
      idx := 1
   else if PtInRect(Rect(75, 13, 125, 63), pnt) then
      idx := 2
   else if PtInRect(Rect(140, 10, 190, 35), pnt) then
      idx := 3
   else if PtInRect(Rect(205, 10, 252, 34), pnt) then
      idx := 4
   else if PtInRect(Rect(140, 40, 190, 65), pnt) then
      idx := 5
   else if PtInRect(Rect(205, 40, 255, 65), pnt) then
      idx := 6;
   if (idx <> -1) and ColorDialog.Execute then
      FillShape(idx, ColorDialog.Color);
end;

procedure TSettingsForm.FillShape(const idx: integer; const AColor: TColor);
begin
   if (idx >= 0) and (idx <= High(SHAPE_POINTS)) then
   begin
      imgShapes.Canvas.Brush.Color := AColor;
      imgShapes.Canvas.FloodFill(SHAPE_POINTS[idx].X, SHAPE_POINTS[idx].Y, SHAPE_BORDER_COLOR, fsBorder);
      if idx = 6 then
         imgShapes.Canvas.FloodFill(206, 41, SHAPE_BORDER_COLOR, fsBorder)
      else if idx = 5 then
      begin
         imgShapes.Canvas.FloodFill(143, 42, SHAPE_BORDER_COLOR, fsBorder);
         imgShapes.Canvas.FloodFill(187, 42, SHAPE_BORDER_COLOR, fsBorder);
      end;
   end;
end;

procedure TSettingsForm.FillAllShapes(const AColor: TColor);
var
   i: integer;
begin
   for i := 0 to High(SHAPE_POINTS) do
      FillShape(i, AColor);
end;

procedure TSettingsForm.DrawShapes(ASettings: TSettings);
begin
   with imgShapes.Canvas do
   begin
      Pen.Color := SHAPE_BORDER_COLOR;
      Brush.Color := ASettings.EllipseColor;
      Ellipse(10, 10, 60, 35);
      Brush.Color := ASettings.InOutColor;
      Polygon([Point(20, 45), Point(60, 45), Point(50, 65), Point(10, 65), Point(20, 45)]);
      Brush.Color := ASettings.DiamondColor;
      Polygon([Point(75, 38), Point(100, 13), Point(125, 38), Point(100, 63), Point(75, 38)]);
      Brush.Color := ASettings.RectColor;
      Rectangle(140, 10, 190, 35);
      Brush.Color := ASettings.RoutineColor;
      Rectangle(140, 40, 190, 65);
      Brush.Color := SHAPE_BORDER_COLOR;
      Rectangle(145, 40, 148, 65);
      Rectangle(182, 40, 185, 65);
      Brush.Color := ASettings.RoadSignColor;
      Polygon([Point(205, 10), Point(240, 10), Point(252, 22), Point(240, 34), Point(205, 34), Point(205, 10)]);
      Pen.Width := 2;
      Brush.Color := ASettings.FolderColor;
      Rectangle(205, 40, 255, 65);
      Pen.Width := 1;
      Polyline([Point(207, 42), Point(251, 42), Point(251, 61), Point(207, 61), Point(207, 42)]);
   end;
end;

procedure TSettingsForm.SetDefault;
var
   parserOn: boolean;
   langDef: TLangDefinition;
begin
   pnlFill.Color := clAqua;
   pnlDesktop.Color := clWhite;
   langDef := GInfra.GetLangDefinition(cbLanguage.Text);
   parserOn := (langDef <> nil) and (langDef.Parser <> nil);
   chkParseInput.Enabled := parserOn;
   chkParseInput.Checked := parserOn;
   chkParseOutput.Enabled := parserOn;
   chkParseOutput.Checked := parserOn;
   chkParseAssign.Enabled := parserOn;
   chkParseAssign.Checked := parserOn;
   chkParseMAssign.Enabled := parserOn;
   chkParseMAssign.Checked := parserOn;
   chkParseCondition.Enabled := parserOn;
   chkParseCondition.Checked := parserOn;
   chkParseRoutine.Enabled := parserOn;
   chkParseRoutine.Checked := parserOn;
   chkParseFor.Enabled := parserOn;
   chkParseFor.Checked := parserOn;
   chkParseCase.Enabled := parserOn;
   chkParseCase.Checked := parserOn;
   chkParseReturn.Enabled := parserOn;
   chkParseReturn.Checked := parserOn;
   chkConfirmRemove.Checked := true;
   chkMultiPrint.Checked := false;
   chkMultiPrintHorz.Checked := false;
   chkMultiPrintHorz.Enabled := false;
   edtMarginLeft.Text := '5';
   edtMarginRight.Text := '5';
   edtMarginTop.Text := '5';
   edtMarginBottom.Text := '5';
   chkEnableDBuffer.Checked := false;
   chkShowFuncLabels.Checked := true;
   chkShowBlockLabels.Checked := false;
   pnlEditorBkg.Color := clWindow;
   pnlEditorFont.Color := clWindowText;
   pnlEditorNumber.Color := clTeal;
   pnlEditorString.Color := clTeal;
   pnlEditorComment.Color := TEXT_COLOR;
   pnlEditorActiveLine.Color := clCream;
   pnlEditorSelect.Color := clHighlight;
   pnlEditorGutter.Color := clBtnFace;
   pnlEditorBracket.Color := clRed;
   pnlFont.Color := OK_COLOR;
   chkValidateConsts.Checked := true;
   chkAutoSelectCode.Checked := false;
   chkAutoUpdateCode.Checked := false;
   edtEditorIndent.Text := IntToStr(EDITOR_DEFAULT_INDENT_LENGTH);
   edtFontName.Text := FLOWCHART_DEFAULT_FONT_NAME;
   SetCbFontSize(EDITOR_DEFAULT_FONT_SIZE);
   FillAllShapes(clWhite);
end;

procedure TSettingsForm.ProtectFields;
var
   parserOn, compilerOn: boolean;
   langDef: TLangDefinition;
begin
   langDef := GInfra.GetLangDefinition(cbLanguage.Text);
   parserOn := (langDef <> nil) and (langDef.Parser <> nil);
   chkParseInput.Enabled := parserOn;
   chkParseOutput.Enabled := parserOn;
   chkParseAssign.Enabled := parserOn;
   chkParseMAssign.Enabled := parserOn;
   chkParseCondition.Enabled := parserOn;
   chkParseFor.Enabled := parserOn;
   chkParseCase.Enabled := parserOn;
   chkParseRoutine.Enabled := parserOn;
   chkParseReturn.Enabled := parserOn;
   if not parserOn then
   begin
      chkParseInput.Checked := false;
      chkParseOutput.Checked := false;
      chkParseAssign.Checked := false;
      chkParseMAssign.Checked := false;
      chkParseCondition.Checked := false;
      chkParseFor.Checked := false;
      chkParseCase.Checked := false;
      chkParseRoutine.Checked := false;
      chkParseReturn.Checked := false;
   end;
   compilerOn := (langDef <> nil) and langDef.EnabledCompiler;
   lblCompiler.Enabled := compilerOn;
   lblFileEncoding.Enabled := compilerOn;
   lblCompilerNoMain.Enabled := compilerOn;
   edtCompiler.Enabled := compilerOn;
   edtCompilerNoMain.Enabled := compilerOn;
   btnBrowseCompilers.Enabled := compilerOn;
   cbFileEncoding.Enabled := compilerOn;
   if compilerOn then
   begin
      edtCompiler.Text := langDef.CompilerCommand;
      edtCompilerNoMain.Text := langDef.CompilerCommandNoMain;
   end
   else
   begin
      edtCompiler.Text := '';
      edtCompilerNoMain.Text := '';
   end;
   SetCbFileEncoding(langDef.CompilerFileEncoding);
   chkMultiPrintHorz.Enabled := chkMultiPrint.Checked;
   if not chkMultiPrint.Checked then
      chkMultiPrintHorz.Checked := false;
end;

procedure TSettingsForm.SetSettings(ASettings: TSettings);
begin
   chkConfirmRemove.Checked := ASettings.ConfirmRemove;
   chkMultiPrint.Checked := ASettings.PrintMultPages;
   chkEnableDBuffer.Checked := ASettings.EnableDBuffering;
   chkShowFuncLabels.Checked := ASettings.ShowFuncLabels;
   chkShowBlockLabels.Checked := ASettings.ShowBlockLabels;
   chkMultiPrintHorz.Checked := ASettings.PrintMultPagesHorz;
   edtMarginLeft.Text := IntToStr(ASettings.PrintMargins.Left);
   edtMarginRight.Text := IntToStr(ASettings.PrintMargins.Right);
   edtMarginTop.Text := IntToStr(ASettings.PrintMargins.Top);
   edtMarginBottom.Text := IntToStr(ASettings.PrintMargins.Bottom);
   pnlFill.Color := ASettings.HighlightColor;
   pnlDesktop.Color := ASettings.DesktopColor;
   pnlEditorFont.Color := ASettings.EditorFontColor;
   pnlEditorBkg.Color := ASettings.EditorBkgColor;
   pnlEditorString.Color := ASettings.EditorStringColor;
   pnlEditorNumber.Color := ASettings.EditorNumberColor;
   pnlEditorComment.Color := ASettings.EditorCommentColor;
   pnlEditorActiveLine.Color := ASettings.EditorALineColor;
   pnlEditorSelect.Color := ASettings.EditorSelectColor;
   pnlEditorGutter.Color := ASettings.EditorGutterColor;
   pnlEditorBracket.Color := ASettings.EditorBracketColor;
   edtEditorIndent.Text := IntToStr(ASettings.IndentLength);
   pnlFont.Color := ASettings.FontColor;
   edtTranslateFile.Text := ASettings.TranslateFile;
   cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(GInfra.CurrentLang.Name);
   edtCompiler.Text := GInfra.CurrentLang.CompilerCommand;
   edtCompilerNoMain.Text := GInfra.CurrentLang.CompilerCommandNoMain;
   chkParseAssign.Checked := ASettings.ParseAssign;
   chkParseCondition.Checked := ASettings.ParseCondition;
   chkParseFor.Checked := ASettings.ParseFor;
   chkParseCase.Checked := ASettings.ParseCase;
   chkParseInput.Checked := ASettings.ParseInput;
   chkParseOutput.Checked := ASettings.ParseOutput;
   chkParseMAssign.Checked := ASettings.ParseAssignMult;
   chkParseRoutine.Checked := ASettings.ParseRoutineCall;
   chkParseReturn.Checked := ASettings.ParseReturn;
   chkValidateConsts.Checked := ASettings.ValidateDeclaration;
   chkAutoSelectCode.Checked := ASettings.EditorAutoSelectBlock;
   chkAutoUpdateCode.Checked := ASettings.EditorAutoUpdate;
   edtFontName.Text := ASettings.FlowchartFontName;
   SetCbFontSize(ASettings.EditorFontSize);
   SetCbFileEncoding(GInfra.CurrentLang.CompilerFileEncoding);
   DrawShapes(ASettings);
   ProtectFields;
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
   if not CharInSet(Key, [#8, '0'..'9']) then
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

procedure TSettingsForm.SetCbFileEncoding(const AFileEncoding: string);
var
   i: integer;
begin
   i := cbFileEncoding.Items.IndexOf(AFileEncoding);
   if i = -1 then
      i := 0;
   cbFileEncoding.ItemIndex := i;
end;

end.
