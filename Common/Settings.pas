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

unit Settings;

interface

uses
  WinApi.Windows, Vcl.Graphics, LangDefinition, CommonTypes, SynEditHighlighter;

type

  TSettings = class(TObject)
  private
    { Private declarations }

      FParseInput,
      FParseOutput,
      FParseAssign,
      FParseMultiAssign,
      FParseCondition,
      FParseFor,
      FParseCase,
      FParseRoutineCall,
      FParseReturn: boolean;

      FEditorShowGutter,
      FEditorShowScrollbars,
      FEditorShowRichText,
      FEditorShowStatusBar,
      FEditorCodeFolding,
      FEditorIndentGuides,
      FEditorAutoSelectBlock,
      FEditorAutoUpdate: boolean;
      FEditorFontSize,
      FFlowchartFontSize: integer;
      FEditorBkgColor,
      FEditorFontColor,
      FEditorNumberColor,
      FEditorStringColor,
      FEditorCommentColor,
      FEditorALineColor,
      FEditorSelectColor,
      FEditorGutterColor,
      FEditorBracketColor: TColor;

      FIndentLength: integer;
      FIndentString: string;

      FConfirmRemove: boolean;
      FPrintMultPages: boolean;
      FPrintMultPagesHorz: boolean;
      FHighlightColor: TColor;
      FDesktopColor: TColor;
      FTranslateFile: string;
      FFontColor: TColor;
      FPrintRect: TRect;
      FEnableDBuffering,
      FShowFuncLabels,
      FShowBlockLabels,
      FValidateDeclaration,
      FNavigatorAlphaVisible,
      FExplorerAutoNav: boolean;
      FNavigatorAlphaValue: integer;
      FFlowchartFontName: string;

      FShapeColors: array[TColorShape] of TColor;

      FColumnV1Width,
      FColumnV2Width,
      FColumnV3Width,
      FColumnV4Width,
      FColumnV5Width,
      FColumnC1Width,
      FColumnC2Width,
      FColumnC3Width: integer;

      procedure SetDefaultValues;
  public
    { Public declarations }
      constructor Create;
      procedure ReadFromRegistry;
      procedure WriteToRegistry;
      procedure LoadFromForm;
      procedure LoadFromEditor;
      procedure SetForm;
      procedure UpdateForLang(const ALang: TLangDefinition);
      procedure UpdateForHLighter(AHLighter: TSynCustomHighlighter);
      procedure ProtectFields;
      procedure SetDefaultForm;
      function GetShapeColor(const shape: TColorShape): TColor;
      function UpdateEditor: boolean;
      property ParseInput: boolean read FParseInput;
      property ParseOutput: boolean read FParseOutput;
      property ParseAssign: boolean read FParseAssign;
      property ParseMultiAssign: boolean read FParseMultiAssign;
      property ParseCondition: boolean read FParseCondition;
      property ParseFor: boolean read FParseFor;
      property ParseCase: boolean read FParseCase;
      property ParseRoutineCall: boolean read FParseRoutineCall;
      property ParseReturn: boolean read FParseReturn;
      property EditorShowGutter: boolean read FEditorShowGutter write FEditorShowGutter;
      property EditorCodeFolding: boolean read FEditorCodeFolding write FEditorCodeFolding;
      property EditorShowScrollbars: boolean read FEditorShowScrollbars write FEditorShowScrollbars;
      property EditorShowRichText: boolean read FEditorShowRichText write FEditorShowRichText;
      property EditorShowStatusBar: boolean read FEditorShowStatusBar write FEditorShowStatusBar;
      property EditorFontSize: integer read FEditorFontSize;
      property EditorBkgColor: TColor read FEditorBkgColor;
      property EditorFontColor: TColor read FEditorFontColor;
      property EditorNumberColor: TColor read FEditorNumberColor;
      property EditorStringColor: TColor read FEditorStringColor;
      property EditorCommentColor: TColor read FEditorCommentColor;
      property EditorALineColor: TColor read FEditorALineColor;
      property EditorSelectColor: TColor read FEditorSelectColor;
      property EditorGutterColor: TColor read FEditorGutterColor;
      property EditorBracketColor: TColor read FEditorBracketColor;
      property EditorIndentGuides: boolean read FEditorIndentGuides write FEditorIndentGuides;
      property EditorAutoSelectBlock: boolean read FEditorAutoSelectBlock;
      property EditorAutoUpdate: boolean read FEditorAutoUpdate write FEditorAutoUpdate;
      property IndentLength: integer read FIndentLength;
      property IndentString: string read FIndentString;
      property FlowchartFontName: string read FFlowchartFontName;
      property FlowchartFontSize: integer read FFlowchartFontSize;
      property ConfirmRemove: boolean read FConfirmRemove;
      property PrintMultPages: boolean read FPrintMultPages;
      property PrintMultPagesHorz: boolean read FPrintMultPagesHorz;
      property HighlightColor: TColor read FHighlightColor;
      property DesktopColor: TColor read FDesktopColor;
      property TranslateFile: string read FTranslateFile;
      property FontColor: TColor read FFontColor;
      property PrintRect: TRect read FPrintRect;
      property EnableDBuffering: boolean read FEnableDBuffering;
      property ShowFuncLabels: boolean read FShowFuncLabels;
      property ShowBlockLabels: boolean read FShowBlockLabels;
      property ValidateDeclaration: boolean read FValidateDeclaration;
      property NavigatorAlphaValue: integer read FNavigatorAlphaValue write FNavigatorAlphaValue;
      property NavigatorAlphaVisible: boolean read FNavigatorAlphaVisible write FNavigatorAlphaVisible;
      property ExplorerAutoNav: boolean read FExplorerAutoNav write FExplorerAutoNav;
      property ColumnV1Width: integer read FColumnV1Width write FColumnV1Width;
      property ColumnV2Width: integer read FColumnV2Width write FColumnV2Width;
      property ColumnV3Width: integer read FColumnV3Width write FColumnV3Width;
      property ColumnV4Width: integer read FColumnV4Width write FColumnV4Width;
      property ColumnV5Width: integer read FColumnV5Width write FColumnV5Width;
      property ColumnC1Width: integer read FColumnC1Width write FColumnC1Width;
      property ColumnC2Width: integer read FColumnC2Width write FColumnC2Width;
      property ColumnC3Width: integer read FColumnC3Width write FColumnC3Width;
  end;

implementation

uses
   System.SysUtils, System.Classes, System.Types, Vcl.Forms, System.Win.Registry,
   System.Math, ApplicationCommon, Main_Form, Navigator_Form;

const
   KEY_HIGHLIGHT_COLOR = 'HighlightColor';
   KEY_DESKTOP_COLOR = 'DesktopColor';
   KEY_ELLIPSE_COLOR = 'EllipseColor';
   KEY_DIAMOND_COLOR = 'DiamondColor';
   KEY_PARALLELOGRAM_COLOR = 'InOutColor';
   KEY_RECTANGLE_COLOR = 'RectColor';
   KEY_FOLDER_COLOR = 'FoldColor';
   KEY_ROADSIGN_COLOR = 'RoadSignColor';
   KEY_ROUTINE_COLOR = 'RoutineColor';
   KEY_FONT_COLOR = 'FontColor';

   KEY_PARSE_INPUT = 'ParseInput';
   KEY_PARSE_OUTPUT = 'ParseOutput';
   KEY_PARSE_ASSIGN = 'ParseAssign';
   KEY_PARSE_MULTI_ASSIGN = 'ParseMulAssign';
   KEY_PARSE_CONDITION = 'ParseCondition';
   KEY_PARSE_SUBROUTINE = 'ParseRoutineCall';
   KEY_PARSE_CASE = 'ParseCase';
   KEY_PARSE_FOR = 'ParseFor';
   KEY_PARSE_RETURN = 'ParseReturn';
   KEY_SHOW_EXPLORER = 'ShowExplorerOnError';
   KEY_SHOW_STATUSBAR = 'ShowStatusBar';
   KEY_CONFIRM_REMOVE = 'ConfirmRemove';

   KEY_EDITOR_SHOW_GUTTER = 'EditorShowGutter';
   KEY_EDITOR_SHOW_RICHTEXT = 'EditorShowRichtext';
   KEY_EDITOR_CODE_FOLDING = 'EditorCodeFolding';
   KEY_EDITOR_INDENT_GUIDES = 'EditorIndentGuides';
   KEY_EDITOR_FONT_COLOR = 'EditorFontColor';
   KEY_EDITOR_BKG_COLOR = 'EditorBkgColor';
   KEY_EDITOR_STRING_COLOR = 'EditorStringColor';
   KEY_EDITOR_NUMBER_COLOR = 'EditorNumberColor';
   KEY_EDITOR_COMMENT_COLOR = 'EditorCommentColor';
   KEY_EDITOR_ALINE_COLOR = 'EditorActiveLineColor';
   KEY_EDITOR_SELECT_COLOR = 'EditorSelectColor';
   KEY_EDITOR_GUTTER_COLOR = 'EditorGutterColor';
   KEY_EDITOR_INDENT = 'IndentLength';
   KEY_EDITOR_SHOW_SCROLLBARS = 'EditorScrollbars';
   KEY_EDITOR_BRACKET_COLOR = 'EditorBracketColor';
   KEY_EDITOR_FONT_SIZE = 'EditorFontSize';

   KEY_LOCALIZATION_FILE = 'LocalizationScript';
   KEY_PRINT_MULTI_PAGES = 'PrintMultiPages';
   KEY_PRINT_MULTI_PAGES_HORZ = 'PrintMultiPagesHorz';
   KEY_PRINT_MARGIN_LEFT = 'PrintMarginLeft';
   KEY_PRINT_MARGIN_RIGHT = 'PrintMarginRight';
   KEY_PRINT_MARGIN_TOP = 'PrintMarginTop';
   KEY_PRINT_MARGIN_BOTTOM = 'PrintMarginBottom';
   KEY_ENABLE_DBUFFERING = 'EnableDBuffering';
   KEY_SHOW_FUNC_LABELS = 'ShowFuncLabels';
   KEY_SHOW_BLOCK_LABELS = 'ShowBlockLabels';
   KEY_VALIDATE_DECLARATION = 'ValidateConsts';
   KEY_NAVIGATOR_ALPHA_VALUE = 'NavigatorAlphaValue';
   KEY_NAVIGATOR_ALPHA_VISIBLE = 'NavigatorAlphaVisible';
   KEY_EXPLORER_AUTO_NAV = 'ExplorerAutoNav';
   KEY_FLOWCHART_FONT_NAME = 'FlowchartFontName';
   KEY_FLOWCHART_FONT_SIZE = 'FlowchartFontSize';
   KEY_AUTOSELECT_CODE_BLOCK = 'AutoSelectCodeBlock';
   KEY_AUTOUPDATE_CODE = 'AutoUpdateCode';

   KEY_COLV1_WIDTH = 'ColumnVariable1Width';
   KEY_COLV2_WIDTH = 'ColumnVariable2Width';
   KEY_COLV3_WIDTH = 'ColumnVariable3Width';
   KEY_COLV4_WIDTH = 'ColumnVariable4Width';
   KEY_COLV5_WIDTH = 'ColumnVariable5Width';
   KEY_COLC1_WIDTH = 'ColumnConstant1Width';
   KEY_COLC2_WIDTH = 'ColumnConstant2Width';
   KEY_COLC3_WIDTH = 'ColumnConstant3Width';

constructor TSettings.Create;
begin
   inherited Create;
   SetDefaultValues;
end;

procedure TSettings.SetDefaultValues;
var
   shape: TColorShape;
begin

   FParseInput       := false;
   FParseOutput      := false;
   FParseAssign      := false;
   FParseMultiAssign := false;
   FParseCondition   := false;
   FParseFor         := false;
   FParseCase        := false;
   FParseRoutineCall := false;
   FParseReturn      := false;

   FEditorShowGutter      := true;
   FEditorShowScrollbars  := true;
   FEditorShowRichText    := false;
   FEditorShowStatusBar   := true;
   FEditorCodeFolding     := false;
   FEditorIndentGuides    := false;
   FEditorBkgColor        := clWindow;
   FEditorFontColor       := clWindowText;
   FEditorNumberColor     := clTeal;
   FEditorStringColor     := clTeal;
   FEditorCommentColor    := TEXT_COLOR;
   FEditorALineColor      := clCream;
   FEditorSelectColor     := clHighlight;
   FEditorGutterColor     := clBtnFace;
   FEditorBracketColor    := clRed;
   FEditorAutoUpdate      := false;
   FEditorAutoSelectBlock := false;
   FEditorFontSize        := EDITOR_DEFAULT_FONT_SIZE;
   FIndentLength          := EDITOR_DEFAULT_INDENT_LENGTH;
   FIndentString          := StringOfChar(INDENT_CHAR, FIndentLength);

   FHighlightColor := clAqua;
   FDesktopColor   := DEFAULT_DESKTOP_COLOR;
   FFontColor      := OK_COLOR;

   for shape := Low(TColorShape) to High(TColorShape) do
     FShapeColors[shape] := IfThen(shape = shpNone, clNone, DEFAULT_DESKTOP_COLOR);

   FColumnV1Width := 68;
   FColumnV2Width := 68;
   FColumnV3Width := 68;
   FColumnV4Width := 68;
   FColumnV5Width := 68;
   FColumnC1Width := 73;
   FColumnC2Width := 73;
   FColumnC3Width := 73;

   FConfirmRemove         := true;
   FPrintMultPages        := false;
   FPrintMultPagesHorz    := false;
   FEnableDBuffering      := false;
   FShowFuncLabels        := true;
   FShowBlockLabels       := false;
   FValidateDeclaration   := true;
   FTranslateFile         := '';
   FNavigatorAlphaValue   := 255;
   FNavigatorAlphaVisible := true;
   FExplorerAutoNav       := true;
   FFlowchartFontName     := FLOWCHART_DEFAULT_FONT_NAME;
   FFlowchartFontSize     := FLOWCHART_MIN_FONT_SIZE;
   FPrintRect             := Rect(DEFAULT_PRINT_MARGIN, DEFAULT_PRINT_MARGIN,
                                  PRINT_SCALE_BASE - DEFAULT_PRINT_MARGIN,
                                  PRINT_SCALE_BASE - DEFAULT_PRINT_MARGIN);
end;

procedure TSettings.ReadFromRegistry;
var
   reg: TRegistry;
   i : integer;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
         if reg.ValueExists(KEY_HIGHLIGHT_COLOR) then
            FHighlightColor := reg.ReadInteger(KEY_HIGHLIGHT_COLOR);
         if reg.ValueExists(KEY_ELLIPSE_COLOR) then
            FShapeColors[shpEllipse] := reg.ReadInteger(KEY_ELLIPSE_COLOR);
         if reg.ValueExists(KEY_DIAMOND_COLOR) then
            FShapeColors[shpDiamond] := reg.ReadInteger(KEY_DIAMOND_COLOR);
         if reg.ValueExists(KEY_PARALLELOGRAM_COLOR) then
            FShapeColors[shpParallel]:= reg.ReadInteger(KEY_PARALLELOGRAM_COLOR);
         if reg.ValueExists(KEY_RECTANGLE_COLOR) then
            FShapeColors[shpRectangle] := reg.ReadInteger(KEY_RECTANGLE_COLOR);
         if reg.ValueExists(KEY_FOLDER_COLOR) then
            FShapeColors[shpFolder] := reg.ReadInteger(KEY_FOLDER_COLOR);
         if reg.ValueExists(KEY_ROADSIGN_COLOR) then
            FShapeColors[shpRoadSign] := reg.ReadInteger(KEY_ROADSIGN_COLOR);
         if reg.ValueExists(KEY_ROUTINE_COLOR) then
            FShapeColors[shpRoutine] := reg.ReadInteger(KEY_ROUTINE_COLOR);
         if reg.ValueExists(KEY_FONT_COLOR) then
         begin
            FFontColor := reg.ReadInteger(KEY_FONT_COLOR);
            if TInfra.IsNOkColor(FFontColor) then
               FFontColor := OK_COLOR;
         end;
         if reg.ValueExists(KEY_NAVIGATOR_ALPHA_VALUE) then
            FNavigatorAlphaValue := reg.ReadInteger(KEY_NAVIGATOR_ALPHA_VALUE);
         if reg.ValueExists(KEY_NAVIGATOR_ALPHA_VISIBLE) then
            FNavigatorAlphaVisible := reg.ReadBool(KEY_NAVIGATOR_ALPHA_VISIBLE);
         if reg.ValueExists(KEY_EXPLORER_AUTO_NAV) then
            FExplorerAutoNav := reg.ReadBool(KEY_EXPLORER_AUTO_NAV);
         if reg.ValueExists(KEY_ENABLE_DBUFFERING) then
            FEnableDBuffering := reg.ReadBool(KEY_ENABLE_DBUFFERING);
         if reg.ValueExists(KEY_PARSE_INPUT) then
            FParseInput := reg.ReadBool(KEY_PARSE_INPUT);
         if reg.ValueExists(KEY_PARSE_OUTPUT) then
            FParseOutput := reg.ReadBool(KEY_PARSE_OUTPUT);
         if reg.ValueExists(KEY_PARSE_RETURN) then
            FParseReturn := reg.ReadBool(KEY_PARSE_RETURN);
         if reg.ValueExists(KEY_PARSE_ASSIGN) then
            FParseAssign := reg.ReadBool(KEY_PARSE_ASSIGN);
         if reg.ValueExists(KEY_PARSE_MULTI_ASSIGN) then
            FParseMultiAssign := reg.ReadBool(KEY_PARSE_MULTI_ASSIGN);
         if reg.ValueExists(KEY_PARSE_CONDITION) then
            FParseCondition := reg.ReadBool(KEY_PARSE_CONDITION);
         if reg.ValueExists(KEY_PARSE_SUBROUTINE) then
            FParseRoutineCall := reg.ReadBool(KEY_PARSE_SUBROUTINE);
         if reg.ValueExists(KEY_PARSE_FOR) then
            FParseFor := reg.ReadBool(KEY_PARSE_FOR);
         if reg.ValueExists(KEY_PARSE_CASE) then
            FParseCase := reg.ReadBool(KEY_PARSE_CASE);
         if reg.ValueExists(KEY_CONFIRM_REMOVE) then
            FConfirmRemove := reg.ReadBool(KEY_CONFIRM_REMOVE);
         if reg.ValueExists(KEY_PRINT_MULTI_PAGES) then
            FPrintMultPages := reg.ReadBool(KEY_PRINT_MULTI_PAGES);
         if reg.ValueExists(KEY_PRINT_MULTI_PAGES_HORZ) then
            FPrintMultPagesHorz := reg.ReadBool(KEY_PRINT_MULTI_PAGES_HORZ);
         if reg.ValueExists(KEY_PRINT_MARGIN_LEFT) then
            FPrintRect.Left := reg.ReadInteger(KEY_PRINT_MARGIN_LEFT);
         if reg.ValueExists(KEY_PRINT_MARGIN_TOP) then
            FPrintRect.Top := reg.ReadInteger(KEY_PRINT_MARGIN_TOP);
         if reg.ValueExists(KEY_PRINT_MARGIN_RIGHT) then
            FPrintRect.Right := PRINT_SCALE_BASE - reg.ReadInteger(KEY_PRINT_MARGIN_RIGHT);
         if reg.ValueExists(KEY_PRINT_MARGIN_BOTTOM) then
            FPrintRect.Bottom := PRINT_SCALE_BASE - reg.ReadInteger(KEY_PRINT_MARGIN_BOTTOM);
         if reg.ValueExists(KEY_EDITOR_SHOW_GUTTER) then
            FEditorShowGutter := reg.ReadBool(KEY_EDITOR_SHOW_GUTTER);
         if reg.ValueExists(KEY_EDITOR_INDENT_GUIDES) then
            FEditorIndentGuides := reg.ReadBool(KEY_EDITOR_INDENT_GUIDES);
         if reg.ValueExists(KEY_EDITOR_SHOW_RICHTEXT) then
            FEditorShowRichText := reg.ReadBool(KEY_EDITOR_SHOW_RICHTEXT);
         if reg.ValueExists(KEY_EDITOR_CODE_FOLDING) then
            FEditorCodeFolding := reg.ReadBool(KEY_EDITOR_CODE_FOLDING);
         if reg.ValueExists(KEY_EDITOR_SHOW_SCROLLBARS) then
            FEditorShowScrollbars := reg.ReadBool(KEY_EDITOR_SHOW_SCROLLBARS);
         if reg.ValueExists(KEY_SHOW_STATUSBAR) then
            FEditorShowStatusBar := reg.ReadBool(KEY_SHOW_STATUSBAR);
         if reg.ValueExists(KEY_EDITOR_BKG_COLOR) then
            FEditorBkgColor := reg.ReadInteger(KEY_EDITOR_BKG_COLOR);
         if reg.ValueExists(KEY_EDITOR_FONT_COLOR) then
            FEditorFontColor := reg.ReadInteger(KEY_EDITOR_FONT_COLOR);
         if reg.ValueExists(KEY_EDITOR_STRING_COLOR) then
            FEditorStringColor := reg.ReadInteger(KEY_EDITOR_STRING_COLOR);
         if reg.ValueExists(KEY_EDITOR_NUMBER_COLOR) then
            FEditorNumberColor := reg.ReadInteger(KEY_EDITOR_NUMBER_COLOR);
         if reg.ValueExists(KEY_EDITOR_COMMENT_COLOR) then
            FEditorCommentColor := reg.ReadInteger(KEY_EDITOR_COMMENT_COLOR);
         if reg.ValueExists(KEY_EDITOR_ALINE_COLOR) then
            FEditorALineColor := reg.ReadInteger(KEY_EDITOR_ALINE_COLOR);
         if reg.ValueExists(KEY_EDITOR_SELECT_COLOR) then
            FEditorSelectColor := reg.ReadInteger(KEY_EDITOR_SELECT_COLOR);
         if reg.ValueExists(KEY_EDITOR_GUTTER_COLOR) then
            FEditorGutterColor := reg.ReadInteger(KEY_EDITOR_GUTTER_COLOR);
         if reg.ValueExists(KEY_DESKTOP_COLOR) then
            FDesktopColor := reg.ReadInteger(KEY_DESKTOP_COLOR);
         if reg.ValueExists(KEY_EDITOR_BRACKET_COLOR) then
            FEditorBracketColor := reg.ReadInteger(KEY_EDITOR_BRACKET_COLOR);
         if reg.ValueExists(KEY_EDITOR_INDENT) then
         begin
            FIndentLength := reg.ReadInteger(KEY_EDITOR_INDENT);
            FIndentString := StringOfChar(INDENT_CHAR, FIndentLength);
         end;
         if reg.ValueExists(KEY_EDITOR_FONT_SIZE) then
            FEditorFontSize := reg.ReadInteger(KEY_EDITOR_FONT_SIZE);
         if reg.ValueExists(KEY_SHOW_FUNC_LABELS) then
            FShowFuncLabels := reg.ReadBool(KEY_SHOW_FUNC_LABELS);
         if reg.ValueExists(KEY_SHOW_BLOCK_LABELS) then
            FShowBlockLabels := reg.ReadBool(KEY_SHOW_BLOCK_LABELS);
         if reg.ValueExists(KEY_VALIDATE_DECLARATION) then
            FValidateDeclaration := reg.ReadBool(KEY_VALIDATE_DECLARATION);
         if reg.ValueExists(KEY_FLOWCHART_FONT_NAME) then
            FFlowchartFontName := reg.ReadString(KEY_FLOWCHART_FONT_NAME);
         if reg.ValueExists(KEY_FLOWCHART_FONT_SIZE) then
         begin
            i := reg.ReadInteger(KEY_FLOWCHART_FONT_SIZE);
            if i in FLOWCHART_VALID_FONT_SIZES then
               FFlowchartFontSize := i;
         end;
         if reg.ValueExists(KEY_LOCALIZATION_FILE) then
         begin
            FTranslateFile := reg.ReadString(KEY_LOCALIZATION_FILE);
            if not FileExists(FTranslateFile) then
               FTranslateFile := '';
         end;
         if reg.ValueExists(KEY_AUTOSELECT_CODE_BLOCK) then
            FEditorAutoSelectBlock := reg.ReadBool(KEY_AUTOSELECT_CODE_BLOCK);
         if reg.ValueExists(KEY_AUTOUPDATE_CODE) then
            FEditorAutoUpdate := reg.ReadBool(KEY_AUTOUPDATE_CODE);
         if reg.ValueExists(KEY_COLV1_WIDTH) then
            FColumnV1Width := reg.ReadInteger(KEY_COLV1_WIDTH);
         if reg.ValueExists(KEY_COLV2_WIDTH) then
            FColumnV2Width := reg.ReadInteger(KEY_COLV2_WIDTH);
         if reg.ValueExists(KEY_COLV3_WIDTH) then
            FColumnV3Width := reg.ReadInteger(KEY_COLV3_WIDTH);
         if reg.ValueExists(KEY_COLV4_WIDTH) then
            FColumnV4Width := reg.ReadInteger(KEY_COLV4_WIDTH);
         if reg.ValueExists(KEY_COLV5_WIDTH) then
            FColumnV5Width := reg.ReadInteger(KEY_COLV5_WIDTH);
         if reg.ValueExists(KEY_COLC1_WIDTH) then
            FColumnC1Width := reg.ReadInteger(KEY_COLC1_WIDTH);
         if reg.ValueExists(KEY_COLC2_WIDTH) then
            FColumnC2Width := reg.ReadInteger(KEY_COLC2_WIDTH);
         if reg.ValueExists(KEY_COLC3_WIDTH) then
            FColumnC3Width := reg.ReadInteger(KEY_COLC3_WIDTH);
      end;
   finally
      reg.Free;
   end;
end;

procedure TSettings.WriteToRegistry;
var
   reg: TRegistry;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKey(REGISTRY_KEY, true) then
      begin
         reg.WriteBool(KEY_PARSE_INPUT, FParseInput);
         reg.WriteBool(KEY_PARSE_OUTPUT, FParseOutput);
         reg.WriteBool(KEY_PARSE_ASSIGN, FParseAssign);
         reg.WriteBool(KEY_PARSE_MULTI_ASSIGN, FParseMultiAssign);
         reg.WriteBool(KEY_PARSE_CONDITION, FParseCondition);
         reg.WriteBool(KEY_PARSE_FOR, FParseFor);
         reg.WriteBool(KEY_PARSE_CASE, FParseCase);
         reg.WriteBool(KEY_PARSE_SUBROUTINE, FParseRoutineCall);
         reg.WriteBool(KEY_PARSE_RETURN, FParseReturn);
         reg.WriteBool(KEY_CONFIRM_REMOVE, FConfirmRemove);
         reg.WriteBool(KEY_PRINT_MULTI_PAGES, FPrintMultPages);
         reg.WriteBool(KEY_PRINT_MULTI_PAGES_HORZ, FPrintMultPagesHorz);
         reg.WriteBool(KEY_AUTOSELECT_CODE_BLOCK, FEditorAutoSelectBlock);
         reg.WriteBool(KEY_AUTOUPDATE_CODE, FEditorAutoUpdate);
         reg.WriteInteger(KEY_PRINT_MARGIN_LEFT, FPrintRect.Left);
         reg.WriteInteger(KEY_PRINT_MARGIN_TOP, FPrintRect.Top);
         reg.WriteInteger(KEY_PRINT_MARGIN_RIGHT, PRINT_SCALE_BASE - FPrintRect.Right);
         reg.WriteInteger(KEY_PRINT_MARGIN_BOTTOM, PRINT_SCALE_BASE - FPrintRect.Bottom);
         reg.WriteBool(KEY_EDITOR_SHOW_GUTTER, FEditorShowGutter);
         reg.WriteBool(KEY_EDITOR_CODE_FOLDING, FEditorCodeFolding);
         reg.WriteBool(KEY_EDITOR_SHOW_RICHTEXT, FEditorShowRichText);
         reg.WriteBool(KEY_EDITOR_SHOW_SCROLLBARS, FEditorShowScrollbars);
         reg.WriteBool(KEY_EDITOR_INDENT_GUIDES, FEditorIndentGuides);
         reg.WriteBool(KEY_SHOW_STATUSBAR, FEditorShowStatusBar);
         reg.WriteBool(KEY_ENABLE_DBUFFERING, FEnableDBuffering);
         reg.WriteBool(KEY_SHOW_FUNC_LABELS, FShowFuncLabels);
         reg.WriteBool(KEY_SHOW_BLOCK_LABELS, FShowBlockLabels);
         reg.WriteBool(KEY_VALIDATE_DECLARATION, FValidateDeclaration);
         reg.WriteInteger(KEY_NAVIGATOR_ALPHA_VALUE, FNavigatorAlphaValue);
         reg.WriteBool(KEY_NAVIGATOR_ALPHA_VISIBLE, FNavigatorAlphaVisible);
         reg.WriteBool(KEY_EXPLORER_AUTO_NAV, FExplorerAutoNav);
         reg.WriteInteger(KEY_HIGHLIGHT_COLOR, FHighlightColor);
         reg.WriteInteger(KEY_EDITOR_FONT_COLOR, FEditorFontColor);
         reg.WriteInteger(KEY_EDITOR_BKG_COLOR, FEditorBkgColor);
         reg.WriteInteger(KEY_EDITOR_STRING_COLOR, FEditorStringColor);
         reg.WriteInteger(KEY_EDITOR_NUMBER_COLOR, FEditorNumberColor);
         reg.WriteInteger(KEY_EDITOR_COMMENT_COLOR, FEditorCommentColor);
         reg.WriteInteger(KEY_EDITOR_ALINE_COLOR, FEditorALineColor);
         reg.WriteInteger(KEY_EDITOR_SELECT_COLOR, FEditorSelectColor);
         reg.WriteInteger(KEY_EDITOR_GUTTER_COLOR, FEditorGutterColor);
         reg.WriteInteger(KEY_EDITOR_BRACKET_COLOR, FEditorBracketColor);
         reg.WriteInteger(KEY_EDITOR_FONT_SIZE, FEditorFontSize);
         reg.WriteInteger(KEY_DESKTOP_COLOR, FDesktopColor);
         reg.WriteInteger(KEY_EDITOR_INDENT, FIndentLength);
         reg.WriteInteger(KEY_ELLIPSE_COLOR, FShapeColors[shpEllipse]);
         reg.WriteInteger(KEY_DIAMOND_COLOR, FShapeColors[shpDiamond]);
         reg.WriteInteger(KEY_PARALLELOGRAM_COLOR, FShapeColors[shpParallel]);
         reg.WriteInteger(KEY_RECTANGLE_COLOR, FShapeColors[shpRectangle]);
         reg.WriteInteger(KEY_FOLDER_COLOR, FShapeColors[shpFolder]);
         reg.WriteInteger(KEY_ROADSIGN_COLOR, FShapeColors[shpRoadSign]);
         reg.WriteInteger(KEY_ROUTINE_COLOR, FShapeColors[shpRoutine]);
         reg.WriteInteger(KEY_FONT_COLOR, FFontColor);
         reg.WriteInteger(KEY_COLV1_WIDTH, FColumnV1Width);
         reg.WriteInteger(KEY_COLV2_WIDTH, FColumnV2Width);
         reg.WriteInteger(KEY_COLV3_WIDTH, FColumnV3Width);
         reg.WriteInteger(KEY_COLV4_WIDTH, FColumnV4Width);
         reg.WriteInteger(KEY_COLV5_WIDTH, FColumnV5Width);
         reg.WriteInteger(KEY_COLC1_WIDTH, FColumnC1Width);
         reg.WriteInteger(KEY_COLC2_WIDTH, FColumnC2Width);
         reg.WriteInteger(KEY_COLC3_WIDTH, FColumnC3Width);
         reg.WriteString(KEY_LOCALIZATION_FILE, FTranslateFile);
         reg.WriteString(KEY_FLOWCHART_FONT_NAME, FFlowchartFontName);
         reg.WriteInteger(KEY_FLOWCHART_FONT_SIZE, FFlowchartFontSize);
       end
       else
          Application.MessageBox(PChar(i18Manager.GetString('RegErr')),
                                 PChar(i18Manager.GetString('Warning')),
                                 MB_OK+MB_ICONEXCLAMATION);
   finally
      reg.Free;
   end;
end;

procedure TSettings.UpdateForLang(const ALang: TLangDefinition);
begin
   if ALang.Parser = nil then
   begin
      FParseInput := false;
      FParseOutput := false;
      FParseAssign := false;
      FParseMultiAssign := false;
      FParseCondition := false;
      FParseFor := false;
      FParseCase := false;
      FParseRoutineCall := false;
      FParseReturn := false;
   end;
   FormatSettings.DecimalSeparator := ALang.DecimalSeparator;
end;

procedure TSettings.UpdateForHLighter(AHLighter: TSynCustomHighlighter);
begin
   FEditorShowRichText := FEditorShowRichText and (AHLighter <> nil);
end;

procedure TSettings.LoadFromEditor;
begin
   with TInfra.GetEditorForm do
   begin
      FEditorShowStatusBar := miStatusBar.Checked;
      FEditorShowScrollbars := miScrollBars.Checked;
      FEditorShowGutter := miGutter.Checked;
      FEditorIndentGuides := miIndentGuides.Checked;
      FEditorShowRichText := miRichText.Checked;
      FEditorCodeFolding := miCodeFoldingEnable.Checked;
   end;
end;

function TSettings.GetShapeColor(const shape: TColorShape): TColor;
begin
   result := FShapeColors[shape];
end;

procedure TSettings.LoadFromForm;
var
   redrawFlow, colorChanged, applyAll: boolean;
   langDef: TLangDefinition;
   lColor: TColor;
   shape: TColorShape;
   flowFontSize: integer;
   flowFontName: string;
   tokens: TArray<string>;
begin

   redrawFlow := false;
   colorChanged := false;
   applyAll := true;
   
   with TInfra.GetSettingsForm do
   begin
      FParseInput       := chkParseInput.Checked;
      FParseOutput      := chkParseOutput.Checked;
      FParseAssign      := chkParseAssign.Checked;
      FParseMultiAssign := chkParseMultiAssign.Checked;
      FParseCondition   := chkParseCondition.Checked;
      FParseFor         := chkParseFor.Checked;
      FParseCase        := chkParseCase.Checked;
      FParseRoutineCall := chkParseRoutine.Checked;
      FParseReturn      := chkParseReturn.Checked;

      FEditorBkgColor        := pnlEditorBkg.Color;
      FEditorFontColor       := pnlEditorFont.Color;
      FEditorNumberColor     := pnlEditorNumber.Color;
      FEditorStringColor     := pnlEditorString.Color;
      FEditorCommentColor    := pnlEditorComment.Color;
      FEditorALineColor      := pnlEditorActiveLine.Color;
      FEditorSelectColor     := pnlEditorSelect.Color;
      FEditorGutterColor     := pnlEditorGutter.Color;
      FEditorBracketColor    := pnlEditorBracket.Color;
      FEditorAutoSelectBlock := chkAutoSelectCode.Checked;
      FEditorAutoUpdate      := chkAutoUpdateCode.Checked;
      FValidateDeclaration   := chkValidateConsts.Checked;
      FEditorFontSize        := StrToIntDef(cbFontSize.Text, EDITOR_DEFAULT_FONT_SIZE);
      FIndentLength          := StrToIntDef(edtEditorIndent.Text, EDITOR_DEFAULT_INDENT_LENGTH);
      FIndentString          := StringOfChar(INDENT_CHAR, FIndentLength);

      for shape := Low(TColorShape) to High(TColorShape) do
      begin
         lColor := GetShapeColor(shape);
         if lColor <> FShapeColors[shape] then
         begin
           FShapeColors[shape] := lColor;
           colorChanged := true;
         end;
      end;

      if (FFontColor <> pnlFont.Color) and not TInfra.IsNOkColor(pnlFont.Color) then
      begin
         colorChanged := true;
         FFontColor := pnlFont.Color;
      end;
      if FDesktopColor <> pnlDesktop.Color then
      begin
         colorChanged := true;
         FDesktopColor := pnlDesktop.Color;
      end;
      FHighlightColor := pnlFill.Color;
      FConfirmRemove := chkConfirmRemove.Checked;
      FPrintMultPages := chkMultiPrint.Checked;
      FPrintMultPagesHorz := chkMultiPrintHorz.Checked;

      if edtTranslateFile.Text <> FTranslateFile then
      begin
         if edtTranslateFile.Text = '' then
         begin
            FTranslateFile := '';
            if i18Manager.LoadDefaultLabels = 0 then
               Application.Terminate;
         end
         else if i18Manager.LoadAllLabels(edtTranslateFile.Text) > 0 then
            FTranslateFile := edtTranslateFile.Text;
      end;

      langDef := GInfra.GetLangDefinition(cbLanguage.Text);
      if langDef <> nil then
      begin
         langDef.CompilerCommand := Trim(edtCompiler.Text);
         langDef.CompilerCommandNoMain := Trim(edtCompilerNoMain.Text);
         langDef.CompilerFileEncoding := cbFileEncoding.Text;
      end;

      if FShowFuncLabels <> chkShowFuncLabels.Checked then
      begin
         FShowFuncLabels := not FShowFuncLabels;
         redrawFlow := true;
      end;

      if FShowBlockLabels <> chkShowBlockLabels.Checked then
      begin
         FShowBlockLabels := not FShowBlockLabels;
         redrawFlow := true;
      end;

      flowFontName := edtFontNameSize.Text;
      tokens := flowFontName.Split([FLOWCHART_FONT_NAMESIZE_SEP], 2);
      flowFontName := tokens[0];
      flowFontSize := tokens[1].ToInteger;

      if (GProject <> nil) and ((FEnableDBuffering <> chkEnableDBuffer.Checked)
                                or (GInfra.CurrentLang.Name <> cbLanguage.Text)
                                or (FFlowchartFontName <> flowFontName)
                                or (FFlowchartFontSize <> flowFontSize)) then
      begin
         if TInfra.ShowFormattedQuestionBox('CloseProjectAsk', [sLineBreak]) = IDYES then
            TInfra.SetInitialSettings
         else
            applyAll := false;
      end;

      if applyAll then
      begin
         FEnableDBuffering := chkEnableDBuffer.Checked;
         FFlowchartFontName := flowFontName;
         FFlowchartFontSize := flowFontSize;
         if GInfra.CurrentLang.Name <> cbLanguage.Text then
         begin
            GInfra.SetCurrentLang(cbLanguage.Text);
{$IFDEF USE_CODEFOLDING}
            TInfra.GetEditorForm.ReloadFoldRegions;
{$ENDIF}
         end;
      end;

      if GProject <> nil then
         GProject.RefreshStatements;

      TInfra.GetEditorForm.SetFormAttributes;

      FPrintRect.Left   := StrToIntDef(edtMarginLeft.Text, DEFAULT_PRINT_MARGIN);
      FPrintRect.Top    := StrToIntDef(edtMarginTop.Text, DEFAULT_PRINT_MARGIN);
      FPrintRect.Right  := PRINT_SCALE_BASE - StrToIntDef(edtMarginRight.Text, DEFAULT_PRINT_MARGIN);
      FPrintRect.Bottom := PRINT_SCALE_BASE - StrToIntDef(edtMarginBottom.Text, DEFAULT_PRINT_MARGIN);
   end;

   if GProject <> nil then
   begin
     if colorChanged then
        GProject.ChangeDesktopColor(FDesktopColor);
     if colorChanged or redrawFlow then
     begin
        GProject.RepaintFlowcharts;
        NavigatorForm.Invalidate;
     end;
   end;
end;

procedure TSettings.SetForm;
begin
   TInfra.GetSettingsForm.SetSettings(Self);
end;

procedure TSettings.ProtectFields;
begin
   TInfra.GetSettingsForm.ProtectFields;
end;

procedure TSettings.SetDefaultForm;
begin
   TInfra.GetSettingsForm.SetDefault;
end;

function TSettings.UpdateEditor: boolean;
begin
   result := TInfra.GetEditorForm.Visible and FEditorAutoUpdate;
end;

end.
