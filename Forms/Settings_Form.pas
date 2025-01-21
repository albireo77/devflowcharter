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
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Graphics, System.Types, Base_Form,
  Settings, Types, Vcl.Controls, Vcl.ExtCtrls, System.Classes;

type
  TSettingsForm = class(TBaseForm)
    btnSave: TButton;
    OpenDialog: TOpenDialog;
    gbMisc: TGroupBox;
    gbProgLang: TGroupBox;
    btnDiscard: TButton;
    gbParse: TGroupBox;
    chkParseInput: TCheckBox;
    chkParseOutput: TCheckBox;
    chkParseAssign: TCheckBox;
    chkParseMultiAssign: TCheckBox;
    chkParseCondition: TCheckBox;
    ColorDialog: TColorDialog;
    chkParseRoutine: TCheckBox;
    gbEditor: TGroupBox;
    chkParseFor: TCheckBox;
    btnDefault: TButton;
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
    lblEditorIdent: TLabel;
    pnlEditorIdent: TPanel;
    gbTranslation: TGroupBox;
    lblFile: TLabel;
    edtTranslationFile: TEdit;
    btnBrowseTranslations: TButton;
    chkParseCase: TCheckBox;
    gbFlowchart: TGroupBox;
    lblDesktop: TLabel;
    pnlDesktop: TPanel;
    lblSelectColor: TLabel;
    pnlSelect: TPanel;
    imgShapes: TImage;
    lblFontColor: TLabel;
    pnlFont: TPanel;
    gbPrint: TGroupBox;
    chkMultiPrint: TCheckBox;
    chkMultiPrintHorz: TCheckBox;
    gbPrintMargins: TGroupBox;
    edtPrintMarginLeft: TEdit;
    edtPrintMarginRight: TEdit;
    edtPrintMarginTop: TEdit;
    edtPrintMarginBottom: TEdit;
    lblMarginLeft: TLabel;
    lblMarginRight: TLabel;
    lblMarginTop: TLabel;
    lblMarginBottom: TLabel;
    chkEnableDBuffer: TCheckBox;
    chkShowFuncLabels: TCheckBox;
    chkParseReturn: TCheckBox;
    chkConfirmRemove: TCheckBox;
    chkValidateConsts: TCheckBox;
    edtFontNameSize: TEdit;
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
    pnlPen: TPanel;
    lblPenColor: TLabel;
    pnlEditorKeyword: TPanel;
    lblEditorKeyword: TLabel;
    cbIndentChar: TComboBox;
    procedure btnBrowseCCompClick(Sender: TObject);
    procedure CloseFormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlSelectClick(Sender: TObject);
    procedure btnDefaultSettingsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure btnBrowseTranslationsClick(Sender: TObject);
    procedure imgShapesClick(Sender: TObject);
    procedure AfterTranslation(AList: TStringList); override;
    procedure chkMultiPrintClick(Sender: TObject);
    procedure ResetForm; override;
    procedure edtFontNameSizeClick(Sender: TObject);
  private
    procedure SetComboBoxItem(AComboBox: TComboBox; const AText: string);
    procedure DrawShapes(ASettings: TSettings);
    procedure FillAllShapes(AColor: TColor);
    procedure SetFontNameSize(const AFontName: string; AFontSize: integer);
    procedure LoadFromSettings(ASettings: TSettings);
    procedure ProtectFields;
    procedure SetDefault;
    procedure SetPrintMargins(const APrintMargins: TRect);
    function GetPrintMargins: TRect;
  public
    property PrintMargins: TRect read GetPrintMargins write SetPrintMargins;
    function GetShapeColor(AShape: TColorShape): TColor;
  end;

  TColorShapeHelper = record helper for TColorShape
    class var image: TImage;
    procedure Draw(AColor: TColor);
    procedure Fill(AColor: TColor);
    function Rect: TRect;
    function Color: TColor;
    function Contains(const APoint: TPoint): boolean;
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
   System.StrUtils, System.SysUtils, LangDefinition, Constants, Infrastructure, Math;

const
   SHAPE_BORDER_COLOR = clBlack;

{$R *.dfm}

procedure TSettingsForm.AfterTranslation(AList: TStringList);
begin
   var w := TInfra.Scaled(Self, 449);
   lblFileEncoding.Left := cbFileEncoding.Left - lblFileEncoding.Width - 5;
   lblCompiler.Left := 7;
   edtCompiler.Left := lblCompiler.Width + lblCompiler.Left + 6;
   edtCompiler.Width := w - edtCompiler.Left;
   lblCompilerNoMain.Left := 7;
   edtCompilerNoMain.Left := lblCompilerNoMain.Width + lblCompilerNoMain.Left + 6;
   edtCompilerNoMain.Width := w - edtCompilerNoMain.Left;
   edtTranslationFile.Left := lblFile.Width + lblFile.Left + 6;
   edtTranslationFile.Width := w - edtTranslationFile.Left;
   var val := lblDesktop.Width;
   if val < lblSelectColor.Width then
      val := lblSelectColor.Width;
   if val < lblFontColor.Width then
      val := lblFontColor.Width;
   Inc(val, lblDesktop.Left + 10);
   pnlDesktop.Left := val;
   pnlSelect.Left := val;
   pnlFont.Left := val;
   pnlPen.Left := val;
   edtCompiler.Hint := ReplaceStr(AList.Values['edtCompilerHint'], LB_PHOLDER2, sLineBreak);
   edtCompilerNoMain.Hint := ReplaceStr(AList.Values['edtCompilerNoMainHint'], LB_PHOLDER2, sLineBreak);
   chkEnableDBuffer.Hint := ReplaceStr(AList.Values['chkEnableDBufferHint'], LB_PHOLDER2, sLineBreak);
   inherited AfterTranslation(AList);
end;

procedure TSettingsForm.ResetForm;
begin
{}
end;

procedure TSettingsForm.SetPrintMargins(const APrintMargins: TRect);
begin
   edtPrintMarginLeft.Text   := APrintMargins.Left.ToString;
   edtPrintMarginTop.Text    := APrintMargins.Top.ToString;
   edtPrintMarginRight.Text  := APrintMargins.Right.ToString;
   edtPrintMarginBottom.Text := APrintMargins.Bottom.ToString;
end;

function TSettingsForm.GetPrintMargins: TRect;
begin
   result.Left   := StrToIntDef(edtPrintMarginLeft.Text, DEFAULT_PRINT_MARGINS.Left);
   result.Top    := StrToIntDef(edtPrintMarginTop.Text, DEFAULT_PRINT_MARGINS.Top);
   result.Right  := StrToIntDef(edtPrintMarginRight.Text, DEFAULT_PRINT_MARGINS.Right);
   result.Bottom := StrToIntDef(edtPrintMarginBottom.Text, DEFAULT_PRINT_MARGINS.Bottom);
end;

procedure TSettingsForm.btnBrowseCCompClick(Sender: TObject);
begin
   with OpenDialog do
   begin
      Filter := trnsManager.GetString('ExeFilesFilter');
      DefaultExt := '*.exe';
      FileName := '';
      if Execute then
         edtCompiler.Text := FileName;
   end;
end;

procedure TSettingsForm.CloseFormClick(Sender: TObject);
begin
   if Sender = btnSave then
      GSettings.LoadFromForm;
   Close;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
   LoadFromSettings(GSettings);
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
   imgShapes.Canvas.Brush.Color := gbFlowchart.Color;
   imgShapes.Canvas.FillRect(imgShapes.Canvas.ClipRect);
   GInfra.GetLangNames(cbLanguage.Items);
   shpDiamond.Image := imgShapes;   // hack for TColorShapeHelper.Image
end;

procedure TSettingsForm.pnlSelectClick(Sender: TObject);
begin
   if ColorDialog.Execute then
      TPanel(Sender).Color := ColorDialog.Color;
end;

procedure TSettingsForm.btnDefaultSettingsClick(Sender: TObject);
begin
   SetDefault;
end;

procedure TSettingsForm.cbLanguageChange(Sender: TObject);
begin
   ProtectFields;
end;

procedure TSettingsForm.btnBrowseTranslationsClick(Sender: TObject);
begin
   var iDir := ExtractFileDir(edtTranslationFile.Text);
   with OpenDialog do
   begin
      if DirectoryExists(iDir) then
         InitialDir := iDir;
      Filter := trnsManager.GetString('LngFilesFilter');
      DefaultExt := '*.lng';
      FileName := '';
      if Execute then
         edtTranslationFile.Text := FileName;
   end;
end;

procedure TSettingsForm.imgShapesClick(Sender: TObject);
begin
   for var colorShape in COLOR_SHAPES do
   begin
      if colorShape.Contains(Mouse.CursorPos) then
      begin
         if ColorDialog.Execute then
            colorShape.Fill(ColorDialog.Color);
         break;
      end;
   end;
end;

function TSettingsForm.GetShapeColor(AShape: TColorShape): TColor;
begin
   result := AShape.Color;
end;

procedure TSettingsForm.FillAllShapes(AColor: TColor);
begin
   for var colorShape in COLOR_SHAPES do
      colorShape.Fill(AColor);
end;

procedure TSettingsForm.DrawShapes(ASettings: TSettings);
begin
   for var colorShape in COLOR_SHAPES do
      colorShape.Draw(ASettings.GetShapeColor(colorShape));
end;

procedure TSettingsForm.SetDefault;
begin
   var langDef := GInfra.GetLangDefinition(cbLanguage.Text);
   pnlSelect.Color := clAqua;
   pnlPen.Color := clBlack;
   pnlDesktop.Color := DEFAULT_DESKTOP_COLOR;
   var parserOn := langDef.Parser <> nil;
   chkParseInput.Enabled := parserOn;
   chkParseInput.Checked := parserOn;
   chkParseOutput.Enabled := parserOn;
   chkParseOutput.Checked := parserOn;
   chkParseAssign.Enabled := parserOn;
   chkParseAssign.Checked := parserOn;
   chkParseMultiAssign.Enabled := parserOn;
   chkParseMultiAssign.Checked := parserOn;
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
   chkConfirmRemove.Checked := True;
   chkMultiPrint.Checked := False;
   chkMultiPrintHorz.Checked := False;
   chkMultiPrintHorz.Enabled := False;
   PrintMargins := DEFAULT_PRINT_MARGINS;
   chkEnableDBuffer.Checked := False;
   chkShowFuncLabels.Checked := True;
   chkShowBlockLabels.Checked := False;
   pnlEditorBkg.Color := clWindow;
   pnlEditorFont.Color := clWindowText;
   pnlEditorNumber.Color := clTeal;
   pnlEditorString.Color := clTeal;
   pnlEditorKeyword.Color := clWindowText;
   pnlEditorComment.Color := TEXT_COLOR;
   pnlEditorActiveLine.Color := clCream;
   pnlEditorSelect.Color := clHighlight;
   pnlEditorGutter.Color := clBtnFace;
   pnlEditorIdent.Color := clWindowText;
   pnlFont.Color := OK_COLOR;
   chkValidateConsts.Checked := True;
   chkAutoSelectCode.Checked := False;
   chkAutoUpdateCode.Checked := False;
   edtEditorIndent.Text := IntToStr(EDITOR_DEFAULT_INDENT_LENGTH);
   SetComboBoxItem(cbFontSize, IntToStr(EDITOR_DEFAULT_FONT_SIZE));
   SetFontNameSize(FLOWCHART_DEFAULT_FONT_NAME, FLOWCHART_MIN_FONT_SIZE);
   cbIndentChar.ItemIndex := 0;
   FillAllShapes(DEFAULT_DESKTOP_COLOR);
end;

procedure TSettingsForm.ProtectFields;
begin
   var lang := GInfra.GetLangDefinition(cbLanguage.Text);
   var parserOn := lang.Parser <> nil;
   cbLanguage.Hint := lang.DefFile;
   chkParseInput.Enabled := parserOn;
   chkParseOutput.Enabled := parserOn;
   chkParseAssign.Enabled := parserOn;
   chkParseMultiAssign.Enabled := parserOn;
   chkParseCondition.Enabled := parserOn;
   chkParseFor.Enabled := parserOn;
   chkParseCase.Enabled := parserOn;
   chkParseRoutine.Enabled := parserOn;
   chkParseReturn.Enabled := parserOn;
   if not parserOn then
   begin
      chkParseInput.Checked := False;
      chkParseOutput.Checked := False;
      chkParseAssign.Checked := False;
      chkParseMultiAssign.Checked := False;
      chkParseCondition.Checked := False;
      chkParseFor.Checked := False;
      chkParseCase.Checked := False;
      chkParseRoutine.Checked := False;
      chkParseReturn.Checked := False;
   end;
   lblCompiler.Enabled := lang.EnabledCompiler;
   lblFileEncoding.Enabled := lang.EnabledCompiler;
   lblCompilerNoMain.Enabled := lang.EnabledCompiler;
   edtCompiler.Enabled := lang.EnabledCompiler;
   edtCompilerNoMain.Enabled := lang.EnabledCompiler;
   btnBrowseCompilers.Enabled := lang.EnabledCompiler;
   cbFileEncoding.Enabled := lang.EnabledCompiler;
   edtCompiler.Text := IfThen(lang.EnabledCompiler, lang.CompilerCommand);
   edtCompilerNoMain.Text := IfThen(lang.EnabledCompiler, lang.CompilerCommandNoMain);
   SetComboBoxItem(cbFileEncoding, lang.CompilerFileEncoding);
   chkMultiPrintHorz.Enabled := chkMultiPrint.Checked;
   if not chkMultiPrint.Checked then
      chkMultiPrintHorz.Checked := False;
end;

procedure TSettingsForm.LoadFromSettings(ASettings: TSettings);
begin
   chkConfirmRemove.Checked := ASettings.ConfirmRemove;
   chkMultiPrint.Checked := ASettings.PrintMultPages;
   chkEnableDBuffer.Checked := ASettings.EnableDBuffering;
   chkShowFuncLabels.Checked := ASettings.ShowFuncLabels;
   chkShowBlockLabels.Checked := ASettings.ShowBlockLabels;
   chkMultiPrintHorz.Checked := ASettings.PrintMultPagesHorz;
   PrintMargins := ASettings.PrintMargins;
   pnlSelect.Color := ASettings.SelectColor;
   pnlPen.Color := ASettings.PenColor;
   pnlDesktop.Color := ASettings.DesktopColor;
   pnlEditorFont.Color := ASettings.EditorFontColor;
   pnlEditorBkg.Color := ASettings.EditorBkgColor;
   pnlEditorString.Color := ASettings.EditorStringColor;
   pnlEditorKeyword.Color := ASettings.EditorKeywordColor;
   pnlEditorNumber.Color := ASettings.EditorNumberColor;
   pnlEditorComment.Color := ASettings.EditorCommentColor;
   pnlEditorActiveLine.Color := ASettings.EditorALineColor;
   pnlEditorSelect.Color := ASettings.EditorSelectColor;
   pnlEditorGutter.Color := ASettings.EditorGutterColor;
   pnlEditorIdent.Color := ASettings.EditorIdentColor;
   edtEditorIndent.Text := ASettings.IndentLength.ToString;
   pnlFont.Color := ASettings.FontColor;
   edtTranslationFile.Text := ASettings.TranslationFile;
   cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(ASettings.CurrentLangName);
   edtCompiler.Text := GInfra.CurrentLang.CompilerCommand;
   edtCompilerNoMain.Text := GInfra.CurrentLang.CompilerCommandNoMain;
   chkParseAssign.Checked := ASettings.ParseAssign;
   chkParseCondition.Checked := ASettings.ParseCondition;
   chkParseFor.Checked := ASettings.ParseFor;
   chkParseCase.Checked := ASettings.ParseCase;
   chkParseInput.Checked := ASettings.ParseInput;
   chkParseOutput.Checked := ASettings.ParseOutput;
   chkParseMultiAssign.Checked := ASettings.ParseMultiAssign;
   chkParseRoutine.Checked := ASettings.ParseRoutineCall;
   chkParseReturn.Checked := ASettings.ParseReturn;
   chkValidateConsts.Checked := ASettings.ValidateDeclaration;
   chkAutoSelectCode.Checked := ASettings.EditorAutoSelectBlock;
   chkAutoUpdateCode.Checked := ASettings.EditorAutoUpdate;
   cbIndentChar.ItemIndex := IfThen(ASettings.IndentChar = TAB_CHAR, 1);
   SetFontNameSize(ASettings.FlowchartFontName, ASettings.FlowchartFontSize);
   SetComboBoxItem(cbFontSize, ASettings.EditorFontSize.ToString);
   SetComboBoxItem(cbFileEncoding, GInfra.CurrentLang.CompilerFileEncoding);
   DrawShapes(ASettings);
   ProtectFields;
end;

procedure TSettingsForm.SetFontNameSize(const AFontName: string; AFontSize: integer);
begin
   edtFontNameSize.Text := AFontName + FLOWCHART_FONT_NAMESIZE_SEP + AFontSize.ToString;
end;

procedure TSettingsForm.chkMultiPrintClick(Sender: TObject);
begin
   chkMultiPrintHorz.Enabled := chkMultiPrint.Checked;
   if not chkMultiPrint.Checked then
      chkMultiPrintHorz.Checked := False;
end;

procedure TSettingsForm.edtFontNameSizeClick(Sender: TObject);
begin
   var fontNameSize := string(edtFontNameSize.Text);
   var tokens := fontNameSize.Split([FLOWCHART_FONT_NAMESIZE_SEP], 2);
   FontDialog.Font.Name := tokens[0];
   FontDialog.Font.Size := tokens[1].ToInteger;
   FontDialog.MinFontSize := FLOWCHART_MIN_FONT_SIZE;
   FontDialog.MaxFontSize := FLOWCHART_MAX_FONT_SIZE;
   if FontDialog.Execute then
      SetFontNameSize(FontDialog.Font.Name, FontDialog.Font.Size);
end;

procedure TSettingsForm.SetComboBoxItem(AComboBox: TComboBox; const AText: string);
begin
   var i := AComboBox.Items.IndexOf(AText);
   if i = -1 then
      i := 0;
   AComboBox.ItemIndex := i;
end;

function TColorShapeHelper.Rect: TRect;
const
   SHAPE_RECTS: array[TColorShape] of TRect = ((Left:10;  Top:10; Right:60;  Bottom:35),    // ellipse
                                               (Left:10;  Top:45; Right:60;  Bottom:65),    // parallelogram
                                               (Left:75;  Top:13; Right:125; Bottom:63),    // diamond
                                               (Left:140; Top:10; Right:190; Bottom:35),    // rectangle
                                               (Left:205; Top:10; Right:252; Bottom:34),    // roadsign
                                               (Left:140; Top:40; Right:190; Bottom:65),    // routine
                                               (Left:205; Top:40; Right:255; Bottom:65));   // folder
begin
   result := SHAPE_RECTS[Self];
end;

procedure TColorShapeHelper.Draw(AColor: TColor);
begin
   var r := Rect;
   var cnv := image.Canvas;
   cnv.Pen.Color := SHAPE_BORDER_COLOR;
   cnv.Brush.Color := AColor;
   case Self of
      shpEllipse:
         cnv.Ellipse(r);
      shpRectangle:
         cnv.Rectangle(r);
      shpParallel:
         cnv.Polygon([Point(r.Left+10, r.Top),
                      Point(r.Right, r.Top),
                      Point(r.Right-10, r.Bottom),
                      Point(r.Left, r.Bottom)]);
      shpRoadSign:
         cnv.Polygon([r.TopLeft,
                      Point(r.Left+35, r.Top),
                      Point(r.Right, r.CenterPoint.Y),
                      Point(r.Left+35, r.Bottom),
                      Point(r.Left, r.Bottom)]);
      shpDiamond:
      begin
         var p := r.CenterPoint;
         cnv.Polygon([Point(r.Left, p.Y),
                      Point(p.X, r.Top),
                      Point(r.Right, p.Y),
                      Point(p.X, r.Bottom)]);
      end;
      shpRoutine:
      begin
         cnv.Rectangle(r);
         cnv.Brush.Color := cnv.Pen.Color;
         var r1 := System.Types.Rect(r.Left+5, r.Top, r.Right-42, r.Bottom);
         cnv.Rectangle(r1);
         r1.Offset(37, 0);
         cnv.Rectangle(r1);
      end;
      shpFolder:
      begin
         cnv.Pen.Width := 2;
         cnv.Rectangle(r);
         var r1 := r;
         r1.Inflate(-2, -2, -3, -3);
         cnv.Pen.Width := 1;
         cnv.Rectangle(r1);
      end;
   end;
end;

procedure TColorShapeHelper.Fill(AColor: TColor);
begin
   var r := Rect;
   var pnt := r.CenterPoint;
   var cnv := image.Canvas;
   cnv.Brush.Color := AColor;
   cnv.FloodFill(pnt.X, pnt.Y, SHAPE_BORDER_COLOR, fsBorder);
   if Self = shpFolder then
      cnv.FloodFill(r.Left+1, r.Top+1, SHAPE_BORDER_COLOR, fsBorder)
   else if Self = shpRoutine then
   begin
      cnv.FloodFill(r.Left+3, r.Top+2, SHAPE_BORDER_COLOR, fsBorder);
      cnv.FloodFill(r.Right-3, r.Top+2, SHAPE_BORDER_COLOR, fsBorder);
   end;
end;

function TColorShapeHelper.Color: TColor;
begin
   var pnt := Rect.CenterPoint;
   result := image.Canvas.Pixels[pnt.X, pnt.Y];
end;

function TColorShapeHelper.Contains(const APoint: TPoint): boolean;
begin
   result := Rect.Contains(image.ScreenToClient(APoint));
end;

end.
