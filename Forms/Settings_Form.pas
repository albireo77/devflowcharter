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
  Base_Form, Settings, CommonTypes;

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
    procedure SetComboBoxItem(AComboBox: TComboBox; const AText: string);
    procedure FillShape(const shape: TColorShape; const AColor: TColor);
    procedure DrawShapes(ASettings: TSettings);
    procedure FillAllShapes(const AColor: TColor);
  public
    procedure SetDefault;
    procedure ProtectFields;
    procedure SetSettings(ASettings: TSettings);
    function GetShapeColor(const shape: TColorShape): TColor;
  end;

var
  SettingsForm: TSettingsForm;

implementation

uses
   System.StrUtils, System.SysUtils, ApplicationCommon, LangDefinition;

const
   SHAPE_BORDER_COLOR = clBlack;
   SHAPE_RECTS: array[TColorShape] of TRect = ((Left:0;   Top:0;  Right:0;   Bottom:0),     // none
                                               (Left:10;  Top:10; Right:60;  Bottom:35),    // ellipse
                                               (Left:10;  Top:45; Right:60;  Bottom:65),    // parallelogram
                                               (Left:75;  Top:13; Right:125; Bottom:63),    // diamond
                                               (Left:140; Top:10; Right:190; Bottom:35),    // rectangle
                                               (Left:205; Top:10; Right:252; Bottom:34),    // roadsign
                                               (Left:140; Top:40; Right:190; Bottom:65),    // routine
                                               (Left:205; Top:40; Right:255; Bottom:65));   // folder

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
   shape: TColorShape;
   pnt: TPoint;
begin
   pnt := imgShapes.ScreenToClient(Mouse.CursorPos);
   shape := High(TColorShape);
   repeat
      if SHAPE_RECTS[shape].Contains(pnt) then
         break;
      shape := Pred(shape);
   until shape = shpNone;
   if (shape <> shpNone) and ColorDialog.Execute then
      FillShape(shape, ColorDialog.Color);
end;

procedure TSettingsForm.FillShape(const shape: TColorShape; const AColor: TColor);
var
   pnt: TPoint;
   rect: TRect;
begin
   if shape <> shpNone then
   begin
      imgShapes.Canvas.Brush.Color := AColor;
      rect := SHAPE_RECTS[shape];
      pnt := rect.CenterPoint;
      imgShapes.Canvas.FloodFill(pnt.X, pnt.Y, SHAPE_BORDER_COLOR, fsBorder);
      if shape = shpFolder then
         imgShapes.Canvas.FloodFill(rect.Left+1, rect.Top+1, SHAPE_BORDER_COLOR, fsBorder)
      else if shape = shpRoutine then
      begin
         imgShapes.Canvas.FloodFill(rect.Left+3, rect.Top+2, SHAPE_BORDER_COLOR, fsBorder);
         imgShapes.Canvas.FloodFill(rect.Right-3, rect.Top+2, SHAPE_BORDER_COLOR, fsBorder);
      end;
   end;
end;

function TSettingsForm.GetShapeColor(const shape: TColorShape): TColor;
var
   pnt: TPoint;
begin
   result := clNone;
   if shape <> shpNone then
   begin
      pnt := SHAPE_RECTS[shape].CenterPoint;
      result := imgShapes.Canvas.Pixels[pnt.X, pnt.Y];
   end;
end;

procedure TSettingsForm.FillAllShapes(const AColor: TColor);
var
   shape: TColorShape;
begin
   for shape := Low(TColorShape) to High(TColorShape) do
      FillShape(shape, AColor);
end;

procedure TSettingsForm.DrawShapes(ASettings: TSettings);
var
   shape: TColorShape;
   rect, rect2: TRect;
   p: TPoint;
begin
   with imgShapes.Canvas do
   begin
      Pen.Color := SHAPE_BORDER_COLOR;
      for shape := Low(TColorShape) to High(TColorShape) do
      begin
         if shape <> shpNone then
         begin
            rect := SHAPE_RECTS[shape];
            Brush.Color := ASettings.GetShapeColor(shape);
            case shape of
               shpEllipse:
                  Ellipse(rect);
               shpRectangle:
                  Rectangle(rect);
               shpParallel:
               begin
                  p := Point(rect.Left+10, rect.Top);
                  Polygon([p,
                           Point(rect.Right, rect.Top),
                           Point(rect.Right-10, rect.Bottom),
                           Point(rect.Left, rect.Bottom),
                           p]);
               end;
               shpDiamond:
               begin
                  p := rect.CenterPoint;
                  Polygon([Point(rect.Left, p.Y),
                           Point(p.X, rect.Top),
                           Point(rect.Right, p.Y),
                           Point(p.X, rect.Bottom),
                           Point(rect.Left, p.Y)]);
               end;
               shpRoadSign:
                  Polygon([rect.TopLeft,
                           Point(rect.Left+35, rect.Top),
                           Point(rect.Right, rect.CenterPoint.Y),
                           Point(rect.Left+35, rect.Bottom),
                           Point(rect.Left, rect.Bottom),
                           rect.TopLeft]);
               shpRoutine:
               begin
                  Rectangle(rect);
                  Brush.Color := Pen.Color;
                  rect2 := System.Types.Rect(rect.Left+5, rect.Top, rect.Right-42, rect.Bottom);
                  Rectangle(rect2);
                  rect2 := System.Types.Rect(rect.Left+42, rect.Top, rect.Right-5, rect.Bottom);
                  Rectangle(rect2);
               end;
               shpFolder:
               begin
                  Pen.Width := 2;
                  Rectangle(rect);
                  rect.Inflate(-2, -2, -3, -3);
                  Pen.Width := 1;
                  Rectangle(rect);
               end;
            end;
         end;
      end;
   end;
end;

procedure TSettingsForm.SetDefault;
var
   parserOn: boolean;
   langDef: TLangDefinition;
begin
   pnlFill.Color := clAqua;
   pnlDesktop.Color := DEFAULT_DESKTOP_COLOR;
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
   SetComboBoxItem(cbFontSize, IntToStr(EDITOR_DEFAULT_FONT_SIZE));
   FillAllShapes(DEFAULT_DESKTOP_COLOR);
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
   SetComboBoxItem(cbFileEncoding, langDef.CompilerFileEncoding);
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
   SetComboBoxItem(cbFontSize, IntToStr(ASettings.EditorFontSize));
   SetComboBoxItem(cbFileEncoding, GInfra.CurrentLang.CompilerFileEncoding);
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

procedure TSettingsForm.SetComboBoxItem(AComboBox: TComboBox; const AText: string);
var
   i: integer;
begin
   i := AComboBox.Items.IndexOf(AText);
   if i = -1 then
      i := 0;
   AComboBox.ItemIndex := i;
end;

end.
