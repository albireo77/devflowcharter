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
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Graphics, System.Classes, System.Types, Base_Form,
  Settings, Types, Vcl.Controls, Vcl.ExtCtrls;

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
    chkParseMultiAssign: TCheckBox;
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
    lblEditorIdent: TLabel;
    pnlEditorIdent: TPanel;
    gbTranslation: TGroupBox;
    lblFile: TLabel;
    edtTranslateFile: TEdit;
    btnBrowseScripts: TButton;
    chkParseCase: TCheckBox;
    gbFlowchartSettings: TGroupBox;
    lblDesktop: TLabel;
    pnlDesktop: TPanel;
    lblSelectColor: TLabel;
    pnlSelect: TPanel;
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
    procedure btnBrowseScriptsClick(Sender: TObject);
    procedure imgShapesClick(Sender: TObject);
    procedure Localize(AList: TStringList); override;
    procedure chkMultiPrintClick(Sender: TObject);
    procedure ResetForm; override;
    procedure edtFontNameSizeClick(Sender: TObject);
  private
    procedure SetComboBoxItem(AComboBox: TComboBox; const AText: string);
    procedure FillShape(const shape: TColorShape; const AColor: TColor);
    procedure DrawShapes(ASettings: TSettings);
    procedure FillAllShapes(const AColor: TColor);
    procedure SetFontNameSize(const AFontName: string; AFontSize: integer);
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
   System.StrUtils, System.SysUtils, LangDefinition, Constants, Infrastructure, Math;

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

procedure TSettingsForm.Localize(AList: TStringList);
begin
   var w := TInfra.Scaled(Self, 449);
   lblFileEncoding.Left := cbFileEncoding.Left - lblFileEncoding.Width - 5;
   lblCompiler.Left := 7;
   edtCompiler.Left := lblCompiler.Width + lblCompiler.Left + 5;
   edtCompiler.Width := w - edtCompiler.Left;
   lblCompilerNoMain.Left := 7;
   edtCompilerNoMain.Left := lblCompilerNoMain.Width + lblCompilerNoMain.Left + 5;
   edtCompilerNoMain.Width := w - edtCompilerNoMain.Left;
   edtTranslateFile.Left := lblFile.Width + lblFile.Left + 5;;
   edtTranslateFile.Width := w - edtTranslateFile.Left;
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
begin
   var pnt := imgShapes.ScreenToClient(Mouse.CursorPos);
   var shape := High(TColorShape);
   repeat
      if SHAPE_RECTS[shape].Contains(pnt) then
         break;
      shape := Pred(shape);
   until shape = shpNone;
   if (shape <> shpNone) and ColorDialog.Execute then
      FillShape(shape, ColorDialog.Color);
end;

procedure TSettingsForm.FillShape(const shape: TColorShape; const AColor: TColor);
begin
   if shape <> shpNone then
   begin
      imgShapes.Canvas.Brush.Color := AColor;
      var rect := SHAPE_RECTS[shape];
      var pnt := rect.CenterPoint;
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
begin
   var pnt := SHAPE_RECTS[shape].CenterPoint;
   if pnt.IsZero then
      result := clNone
   else
      result := imgShapes.Canvas.Pixels[pnt.X, pnt.Y];
end;

procedure TSettingsForm.FillAllShapes(const AColor: TColor);
begin
   for var shape := Low(TColorShape) to High(TColorShape) do
      FillShape(shape, AColor);
end;

procedure TSettingsForm.DrawShapes(ASettings: TSettings);
begin
   with imgShapes.Canvas do
   begin
      Pen.Color := SHAPE_BORDER_COLOR;
      for var shape := Low(TColorShape) to High(TColorShape) do
      begin
         if shape <> shpNone then
         begin
            var rect := SHAPE_RECTS[shape];
            Brush.Color := ASettings.GetShapeColor(shape);
            case shape of
               shpEllipse:
                  Ellipse(rect);
               shpRectangle:
                  Rectangle(rect);
               shpParallel:
               begin
                  var p := Point(rect.Left+10, rect.Top);
                  Polygon([p,
                           Point(rect.Right, rect.Top),
                           Point(rect.Right-10, rect.Bottom),
                           Point(rect.Left, rect.Bottom),
                           p]);
               end;
               shpDiamond:
               begin
                  var p := rect.CenterPoint;
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
                  rect := System.Types.Rect(rect.Left+5, rect.Top, rect.Right-42, rect.Bottom);
                  Rectangle(rect);
                  rect.Offset(37, 0);
                  Rectangle(rect);
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
   var m := IntToStr(DEFAULT_PRINT_MARGIN);
   edtMarginLeft.Text := m;
   edtMarginRight.Text := m;
   edtMarginTop.Text := m;
   edtMarginBottom.Text := m;
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

procedure TSettingsForm.SetSettings(ASettings: TSettings);
begin
   chkConfirmRemove.Checked := ASettings.ConfirmRemove;
   chkMultiPrint.Checked := ASettings.PrintMultPages;
   chkEnableDBuffer.Checked := ASettings.EnableDBuffering;
   chkShowFuncLabels.Checked := ASettings.ShowFuncLabels;
   chkShowBlockLabels.Checked := ASettings.ShowBlockLabels;
   chkMultiPrintHorz.Checked := ASettings.PrintMultPagesHorz;
   edtMarginLeft.Text := ASettings.PrintRect.Left.ToString;
   edtMarginTop.Text := ASettings.PrintRect.Top.ToString;
   edtMarginRight.Text := (PRINT_SCALE_BASE - ASettings.PrintRect.Right).ToString;
   edtMarginBottom.Text := (PRINT_SCALE_BASE - ASettings.PrintRect.Bottom).ToString;
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
   edtTranslateFile.Text := ASettings.TranslateFile;
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

end.
