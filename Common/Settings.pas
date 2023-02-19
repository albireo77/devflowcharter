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
  Vcl.Graphics, System.IniFiles, System.Types, LangDefinition, Types, YaccLib,
  SynEditHighlighter;

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
      FEditorRightEdgeColumn,
      FFlowchartFontSize: integer;
      FEditorBkgColor,
      FEditorFontColor,
      FEditorNumberColor,
      FEditorStringColor,
      FEditorKeywordColor,
      FEditorCommentColor,
      FEditorDocumentColor,
      FEditorALineColor,
      FEditorSelectColor,
      FEditorGutterColor,
      FEditorIdentColor,
      FEditorRightEdgeColor: TColor;

      FIndentLength: integer;
      FIndentSpaces: string;
      FIndentChar: char;

      FConfirmRemove: boolean;
      FPrintMultPages: boolean;
      FPrintMultPagesHorz: boolean;
      FSelectColor: TColor;
      FDesktopColor: TColor;
      FTranslateFile: string;
      FFontColor: TColor;
      FPenColor: TColor;
      FPrintRect: TRect;
      FEnableDBuffering,
      FShowFuncLabels,
      FShowBlockLabels,
      FValidateDeclaration,
      FNavigatorAlphaVisible,
      FExplorerAutoNav: boolean;
      FNavigatorAlphaValue: integer;
      FFlowchartFontName,
      FCurrentLangName,
      FLanguageDefinitionsDir: string;
      FSettingsFile: TCustomIniFile;

      FShapeColors: array[TColorShape] of TColor;
      procedure SetCurrentLangName(const ACurrentLangName: string);
      procedure Load;
  public
    { Public declarations }
      constructor Create;
      destructor Destroy; override;
      procedure Save;
      procedure LoadFromForm;
      procedure LoadFromEditor;
      procedure SetForm;
      procedure UpdateForHLighter(AHLighter: TSynCustomHighlighter);
      procedure ResetCurrentLangName;
      function GetShapeColor(const shape: TColorShape): TColor;
      function UpdateEditor: boolean;
      function IndentString(ATimes: integer = 1): string;
      function ExecuteParse(AParserMode: TYYMode): boolean;
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
      property EditorKeywordColor: TColor read FEditorKeywordColor;
      property EditorCommentColor: TColor read FEditorCommentColor;
      property EditorDocumentColor: TColor read FEditorDocumentColor;
      property EditorALineColor: TColor read FEditorALineColor;
      property EditorSelectColor: TColor read FEditorSelectColor;
      property EditorGutterColor: TColor read FEditorGutterColor;
      property EditorIdentColor: TColor read FEditorIdentColor;
      property EditorRightEdgeColor: TColor read FEditorRightEdgeColor;
      property EditorRightEdgeColumn: integer read FEditorRightEdgeColumn;
      property EditorIndentGuides: boolean read FEditorIndentGuides write FEditorIndentGuides;
      property EditorAutoSelectBlock: boolean read FEditorAutoSelectBlock;
      property EditorAutoUpdate: boolean read FEditorAutoUpdate write FEditorAutoUpdate;
      property IndentLength: integer read FIndentLength;
      property IndentChar: char read FIndentChar;
      property FlowchartFontName: string read FFlowchartFontName;
      property FlowchartFontSize: integer read FFlowchartFontSize;
      property ConfirmRemove: boolean read FConfirmRemove;
      property PrintMultPages: boolean read FPrintMultPages;
      property PrintMultPagesHorz: boolean read FPrintMultPagesHorz;
      property SelectColor: TColor read FSelectColor;
      property PenColor: TColor read FPenColor;
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
      property CurrentLangName: string read FCurrentLangName write SetCurrentLangName;
      property SettingsFile: TCustomIniFile read FSettingsFile;
      property LanguageDefinitionsDir: string read FLanguageDefinitionsDir;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
   System.Win.Registry,
{$ENDIF}
   System.SysUtils, Vcl.Forms, Vcl.Controls, System.Math, System.IOUtils,
   System.StrUtils, Infrastructure, Main_Form, Navigator_Form, Constants;

const
   KEY_SELECT_COLOR = 'HighlightColor';
   KEY_DESKTOP_COLOR = 'DesktopColor';
   KEY_ELLIPSE_COLOR = 'EllipseColor';
   KEY_DIAMOND_COLOR = 'DiamondColor';
   KEY_PARALLELOGRAM_COLOR = 'InOutColor';
   KEY_RECTANGLE_COLOR = 'RectColor';
   KEY_FOLDER_COLOR = 'FoldColor';
   KEY_ROADSIGN_COLOR = 'RoadSignColor';
   KEY_ROUTINE_COLOR = 'RoutineColor';
   KEY_FONT_COLOR = 'FontColor';
   KEY_PEN_COLOR = 'PenColor';

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
   KEY_CURRENT_LANGUAGE = 'CurrentLanguageName';

   KEY_EDITOR_SHOW_GUTTER = 'EditorShowGutter';
   KEY_EDITOR_SHOW_RICHTEXT = 'EditorShowRichtext';
   KEY_EDITOR_CODE_FOLDING = 'EditorCodeFolding';
   KEY_EDITOR_INDENT_GUIDES = 'EditorIndentGuides';
   KEY_EDITOR_FONT_COLOR = 'EditorFontColor';
   KEY_EDITOR_BKG_COLOR = 'EditorBkgColor';
   KEY_EDITOR_STRING_COLOR = 'EditorStringColor';
   KEY_EDITOR_KEYWORD_COLOR = 'EditorKeywordColor';
   KEY_EDITOR_NUMBER_COLOR = 'EditorNumberColor';
   KEY_EDITOR_COMMENT_COLOR = 'EditorCommentColor';
   KEY_EDITOR_DOCUMENT_COLOR = 'EditorDocumentColor';
   KEY_EDITOR_ALINE_COLOR = 'EditorActiveLineColor';
   KEY_EDITOR_SELECT_COLOR = 'EditorSelectColor';
   KEY_EDITOR_GUTTER_COLOR = 'EditorGutterColor';
   KEY_EDITOR_IDENT_COLOR = 'EditorIdentColor';
   KEY_EDITOR_RIGHT_EDGE_COLOR = 'EditorRightEdgeColor';
   KEY_EDITOR_RIGHT_EDGE_COLUMN = 'EditorRightEdgeColumn';
   KEY_EDITOR_INDENT = 'IndentLength';
   KEY_EDITOR_INDENT_CHAR = 'IndentChar';
   KEY_EDITOR_SHOW_SCROLLBARS = 'EditorScrollbars';
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

constructor TSettings.Create;
const
   SETTINGS_FILE_PARAM = '-settingsFile=';
   LANG_DEFS_DIR_PARAM = '-langDefinitionsDir=';
begin
   inherited Create;
   var sFile := '';
   for var i := 1 to ParamCount do
   begin
      var param := ParamStr(i);
      if param.StartsWith(SETTINGS_FILE_PARAM, True) then
      begin
         sFile := Copy(param, Length(SETTINGS_FILE_PARAM)+1);
         if not sFile.IsEmpty then
            sFile := TPath.GetFullPath(sFile);
      end
      else if param.StartsWith(LANG_DEFS_DIR_PARAM, True) then
         FLanguageDefinitionsDir := Copy(param, Length(LANG_DEFS_DIR_PARAM)+1);
   end;
   if sFile.IsEmpty then
{$IFDEF MSWINDOWS}
      FSettingsFile := TRegistryIniFile.Create('Software\' + PROGRAM_NAME)
{$ELSE}
      FSettingsFile := TIniFile.Create(TPath.GetFullPath(PROGRAM_NAME + '.ini'))
{$ENDIF}
   else
      FSettingsFile := TIniFile.Create(sFile);
   if not DirectoryExists(FLanguageDefinitionsDir) then
      FLanguageDefinitionsDir := 'LanguageDefinitions';
   FLanguageDefinitionsDir := IncludeTrailingPathDelimiter(FLanguageDefinitionsDir);
   Load;
end;

destructor TSettings.Destroy;
begin
   FSettingsFile.Free;
   inherited Destroy;
end;

procedure TSettings.Load;
begin
   FFontColor                 := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_FONT_COLOR, NOK_COLOR);
   FSelectColor               := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_SELECT_COLOR, clAqua);
   FPenColor                  := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PEN_COLOR, clBlack);
   FShapeColors[shpEllipse]   := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_ELLIPSE_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpDiamond]   := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_DIAMOND_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpParallel]  := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PARALLELOGRAM_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpRectangle] := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_RECTANGLE_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpFolder]    := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_FOLDER_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpRoadSign]  := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_ROADSIGN_COLOR, DEFAULT_DESKTOP_COLOR);
   FShapeColors[shpRoutine]   := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_ROUTINE_COLOR, DEFAULT_DESKTOP_COLOR);
   FNavigatorAlphaValue       := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_NAVIGATOR_ALPHA_VALUE, 255);
   FNavigatorAlphaVisible     := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_NAVIGATOR_ALPHA_VISIBLE, True);
   FExplorerAutoNav           := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EXPLORER_AUTO_NAV, True);
   FEnableDBuffering          := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_ENABLE_DBUFFERING, False);
   FParseInput                := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_INPUT, False);
   FParseOutput               := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_OUTPUT, False);
   FParseReturn               := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_RETURN, False);
   FParseAssign               := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_ASSIGN, False);
   FParseMultiAssign          := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_MULTI_ASSIGN, False);
   FParseCondition            := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_CONDITION, False);
   FParseRoutineCall          := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_SUBROUTINE, False);
   FParseFor                  := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_FOR, False);
   FParseCase                 := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PARSE_CASE, False);
   FConfirmRemove             := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_CONFIRM_REMOVE, True);
   FPrintMultPages            := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PRINT_MULTI_PAGES, False);
   FPrintMultPagesHorz        := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_PRINT_MULTI_PAGES_HORZ, False);
   FPrintRect.Left            := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_LEFT, DEFAULT_PRINT_MARGIN);
   FPrintRect.Top             := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_TOP, DEFAULT_PRINT_MARGIN);
   FPrintRect.Right           := PRINT_SCALE_BASE - FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_RIGHT, DEFAULT_PRINT_MARGIN);
   FPrintRect.Bottom          := PRINT_SCALE_BASE - FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_BOTTOM, DEFAULT_PRINT_MARGIN);
   FEditorShowGutter          := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_GUTTER, True);
   FEditorIndentGuides        := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EDITOR_INDENT_GUIDES, False);
   FEditorShowRichText        := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_RICHTEXT, False);
   FEditorCodeFolding         := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EDITOR_CODE_FOLDING, False);
   FEditorShowScrollbars      := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_SCROLLBARS, True);
   FEditorShowStatusBar       := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_SHOW_STATUSBAR, True);
   FEditorAutoSelectBlock     := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_AUTOSELECT_CODE_BLOCK, False);
   FEditorAutoUpdate          := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_AUTOUPDATE_CODE, False);
   FEditorBkgColor            := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_BKG_COLOR, clWindow);
   FEditorFontColor           := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_FONT_COLOR, clWindowText);
   FEditorStringColor         := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_STRING_COLOR, clTeal);
   FEditorKeywordColor        := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_KEYWORD_COLOR, clWindowText);
   FEditorNumberColor         := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_NUMBER_COLOR, clTeal);
   FEditorCommentColor        := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_COMMENT_COLOR, TEXT_COLOR);
   FEditorDocumentColor       := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_DOCUMENT_COLOR, clHotLight);
   FEditorALineColor          := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_ALINE_COLOR, clCream);
   FEditorSelectColor         := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_SELECT_COLOR, clHighlight);
   FEditorGutterColor         := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_GUTTER_COLOR, clBtnFace);
   FEditorIdentColor          := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_IDENT_COLOR, clWindowText);
   FEditorRightEdgeColor      := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_RIGHT_EDGE_COLOR, clSilver);
   FEditorRightEdgeColumn     := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_RIGHT_EDGE_COLUMN, 80);
   FDesktopColor              := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_DESKTOP_COLOR, DEFAULT_DESKTOP_COLOR);
   FIndentLength              := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_INDENT, EDITOR_DEFAULT_INDENT_LENGTH);
   FEditorFontSize            := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_FONT_SIZE, EDITOR_DEFAULT_FONT_SIZE);
   FShowFuncLabels            := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_SHOW_FUNC_LABELS, True);
   FShowBlockLabels           := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_SHOW_BLOCK_LABELS, False);
   FValidateDeclaration       := FSettingsFile.ReadBool(SETTINGS_SECTION, KEY_VALIDATE_DECLARATION, True);
   FFlowchartFontName         := FSettingsFile.ReadString(SETTINGS_SECTION, KEY_FLOWCHART_FONT_NAME, FLOWCHART_DEFAULT_FONT_NAME);
   FTranslateFile             := FSettingsFile.ReadString(SETTINGS_SECTION, KEY_LOCALIZATION_FILE, '');
   FCurrentLangName           := FSettingsFile.ReadString(SETTINGS_SECTION, KEY_CURRENT_LANGUAGE, '');
   FFlowchartFontSize         := FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_FLOWCHART_FONT_SIZE, FLOWCHART_MIN_FONT_SIZE);
   FIndentChar                := Char(FSettingsFile.ReadInteger(SETTINGS_SECTION, KEY_EDITOR_INDENT_CHAR, Integer(SPACE_CHAR)));
   if not (FFlowchartFontSize in FLOWCHART_VALID_FONT_SIZES) then
      FFlowchartFontSize := FLOWCHART_MIN_FONT_SIZE;
   if not FileExists(FTranslateFile) then
      FTranslateFile := '';
   if TInfra.IsNOkColor(FFontColor) then
      FFontColor := OK_COLOR;
   FIndentSpaces := StringOfChar(SPACE_CHAR, FIndentLength);
   FShapeColors[shpNone] := clNone;
end;

procedure TSettings.Save;
begin
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_INPUT, FParseInput);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_OUTPUT, FParseOutput);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_ASSIGN, FParseAssign);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_MULTI_ASSIGN, FParseMultiAssign);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_CONDITION, FParseCondition);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_FOR, FParseFor);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_CASE, FParseCase);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_SUBROUTINE, FParseRoutineCall);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PARSE_RETURN, FParseReturn);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_CONFIRM_REMOVE, FConfirmRemove);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PRINT_MULTI_PAGES, FPrintMultPages);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_PRINT_MULTI_PAGES_HORZ, FPrintMultPagesHorz);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_AUTOSELECT_CODE_BLOCK, FEditorAutoSelectBlock);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_AUTOUPDATE_CODE, FEditorAutoUpdate);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_LEFT, FPrintRect.Left);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_TOP, FPrintRect.Top);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_RIGHT, PRINT_SCALE_BASE - FPrintRect.Right);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PRINT_MARGIN_BOTTOM, PRINT_SCALE_BASE - FPrintRect.Bottom);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_GUTTER, FEditorShowGutter);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EDITOR_CODE_FOLDING, FEditorCodeFolding);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_RICHTEXT, FEditorShowRichText);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EDITOR_SHOW_SCROLLBARS, FEditorShowScrollbars);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EDITOR_INDENT_GUIDES, FEditorIndentGuides);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_SHOW_STATUSBAR, FEditorShowStatusBar);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_ENABLE_DBUFFERING, FEnableDBuffering);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_SHOW_FUNC_LABELS, FShowFuncLabels);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_SHOW_BLOCK_LABELS, FShowBlockLabels);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_VALIDATE_DECLARATION, FValidateDeclaration);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_NAVIGATOR_ALPHA_VALUE, FNavigatorAlphaValue);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_NAVIGATOR_ALPHA_VISIBLE, FNavigatorAlphaVisible);
   FSettingsFile.WriteBool(SETTINGS_SECTION, KEY_EXPLORER_AUTO_NAV, FExplorerAutoNav);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_SELECT_COLOR, FSelectColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PEN_COLOR, FPenColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_FONT_COLOR, FEditorFontColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_BKG_COLOR, FEditorBkgColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_STRING_COLOR, FEditorStringColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_KEYWORD_COLOR, FEditorKeywordColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_NUMBER_COLOR, FEditorNumberColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_COMMENT_COLOR, FEditorCommentColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_DOCUMENT_COLOR, FEditorDocumentColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_ALINE_COLOR, FEditorALineColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_SELECT_COLOR, FEditorSelectColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_GUTTER_COLOR, FEditorGutterColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_IDENT_COLOR, FEditorIdentColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_RIGHT_EDGE_COLOR, FEditorRightEdgeColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_RIGHT_EDGE_COLUMN, FEditorRightEdgeColumn);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_FONT_SIZE, FEditorFontSize);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_DESKTOP_COLOR, FDesktopColor);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_INDENT, FIndentLength);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_EDITOR_INDENT_CHAR, Integer(FIndentChar));
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_ELLIPSE_COLOR, FShapeColors[shpEllipse]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_DIAMOND_COLOR, FShapeColors[shpDiamond]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_PARALLELOGRAM_COLOR, FShapeColors[shpParallel]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_RECTANGLE_COLOR, FShapeColors[shpRectangle]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_FOLDER_COLOR, FShapeColors[shpFolder]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_ROADSIGN_COLOR, FShapeColors[shpRoadSign]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_ROUTINE_COLOR, FShapeColors[shpRoutine]);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_FONT_COLOR, FFontColor);
   FSettingsFile.WriteString(SETTINGS_SECTION, KEY_LOCALIZATION_FILE, FTranslateFile);
   FSettingsFile.WriteString(SETTINGS_SECTION, KEY_FLOWCHART_FONT_NAME, FFlowchartFontName);
   FSettingsFile.WriteString(SETTINGS_SECTION, KEY_CURRENT_LANGUAGE, FCurrentLangName);
   FSettingsFile.WriteInteger(SETTINGS_SECTION, KEY_FLOWCHART_FONT_SIZE, FFlowchartFontSize);
end;

procedure TSettings.ResetCurrentLangName;
begin
   var lName := FCurrentLangName;
   FCurrentLangName := 'a2s%WE';
   SetCurrentLangName(lName);
end;

procedure TSettings.SetCurrentLangName(const ACurrentLangName: string);
begin
   if FCurrentLangName <> ACurrentLangName then
   begin
      var lang := GInfra.SetCurrentLang(ACurrentLangName);
      if lang <> nil then
      begin
         FCurrentLangName := lang.Name;
         if lang.Parser = nil then
         begin
            FParseInput := False;
            FParseOutput := False;
            FParseAssign := False;
            FParseMultiAssign := False;
            FParseCondition := False;
            FParseFor := False;
            FParseCase := False;
            FParseRoutineCall := False;
            FParseReturn := False;
         end;
         FormatSettings.DecimalSeparator := lang.DecimalSeparator;
      end;
   end;
end;

function TSettings.IndentString(ATimes: integer = 1): string;
begin
   if ATimes = 1 then
      result := FIndentSpaces
   else
      result := DupeString(FIndentSpaces, ATimes);
end;

procedure TSettings.UpdateForHLighter(AHLighter: TSynCustomHighlighter);
begin
   FEditorShowRichText := FEditorShowRichText and (AHLighter <> nil);
end;

procedure TSettings.LoadFromEditor;
begin
   var eForm := TInfra.GetEditorForm;
   FEditorShowStatusBar  := eForm.miStatusBar.Checked;
   FEditorShowScrollbars := eForm.miScrollBars.Checked;
   FEditorShowGutter     := eForm.miGutter.Checked;
   FEditorIndentGuides   := eForm.miIndentGuides.Checked;
   FEditorShowRichText   := eForm.miRichText.Checked;
   FEditorCodeFolding    := eForm.miCodeFoldingEnable.Checked;
end;

function TSettings.GetShapeColor(const shape: TColorShape): TColor;
begin
   result := FShapeColors[shape];
end;

procedure TSettings.LoadFromForm;
begin

   var redrawFlow := False;
   var colorChanged := False;
   var applyAll := True;

   var sForm := TInfra.GetSettingsForm;

   FParseInput       := sForm.chkParseInput.Checked;
   FParseOutput      := sForm.chkParseOutput.Checked;
   FParseAssign      := sForm.chkParseAssign.Checked;
   FParseMultiAssign := sForm.chkParseMultiAssign.Checked;
   FParseCondition   := sForm.chkParseCondition.Checked;
   FParseFor         := sForm.chkParseFor.Checked;
   FParseCase        := sForm.chkParseCase.Checked;
   FParseRoutineCall := sForm.chkParseRoutine.Checked;
   FParseReturn      := sForm.chkParseReturn.Checked;

   // setting the following values from Settings form is not yet supported
   // FEditorDocumentColor
   // FEditorRightEdgeColor
   // FEditorRightEdgeColumn

   FEditorBkgColor        := sForm.pnlEditorBkg.Color;
   FEditorFontColor       := sForm.pnlEditorFont.Color;
   FEditorNumberColor     := sForm.pnlEditorNumber.Color;
   FEditorStringColor     := sForm.pnlEditorString.Color;
   FEditorKeywordColor    := sForm.pnlEditorKeyword.Color;
   FEditorCommentColor    := sForm.pnlEditorComment.Color;
   FEditorALineColor      := sForm.pnlEditorActiveLine.Color;
   FEditorSelectColor     := sForm.pnlEditorSelect.Color;
   FEditorGutterColor     := sForm.pnlEditorGutter.Color;
   FEditorIdentColor      := sForm.pnlEditorIdent.Color;
   FEditorAutoSelectBlock := sForm.chkAutoSelectCode.Checked;
   FEditorAutoUpdate      := sForm.chkAutoUpdateCode.Checked;
   FValidateDeclaration   := sForm.chkValidateConsts.Checked;
   FEditorFontSize        := StrToIntDef(sForm.cbFontSize.Text, EDITOR_DEFAULT_FONT_SIZE);
   FIndentLength          := StrToIntDef(sForm.edtEditorIndent.Text, EDITOR_DEFAULT_INDENT_LENGTH);

   if sForm.cbIndentChar.ItemIndex = 1 then
      FIndentChar := TAB_CHAR
   else
      FIndentChar := SPACE_CHAR;
   FIndentSpaces := StringOfChar(SPACE_CHAR, FIndentLength);

   for var shape := Low(TColorShape) to High(TColorShape) do
   begin
      var lColor := sForm.GetShapeColor(shape);
      if lColor <> FShapeColors[shape] then
      begin
         FShapeColors[shape] := lColor;
         colorChanged := True;
      end;
   end;

   if (FFontColor <> sForm.pnlFont.Color) and not TInfra.IsNOkColor(sForm.pnlFont.Color) then
   begin
      colorChanged := True;
      FFontColor := sForm.pnlFont.Color;
   end;
   if FDesktopColor <> sForm.pnlDesktop.Color then
   begin
      colorChanged := True;
      FDesktopColor := sForm.pnlDesktop.Color;
   end;
   if FPenColor <> sForm.pnlPen.Color then
   begin
      colorChanged := True;
      FPenColor := sForm.pnlPen.Color;
   end;
   if (FSelectColor <> sForm.pnlSelect.Color) and  (sForm.pnlSelect.Color <> sForm.pnlDesktop.Color) then
      FSelectColor := sForm.pnlSelect.Color;
   FConfirmRemove := sForm.chkConfirmRemove.Checked;
   FPrintMultPages := sForm.chkMultiPrint.Checked;
   FPrintMultPagesHorz := sForm.chkMultiPrintHorz.Checked;

   if sForm.edtTranslateFile.Text <> FTranslateFile then
   begin
      if sForm.edtTranslateFile.Text = '' then
      begin
         FTranslateFile := '';
         if i18Manager.LoadDefaultLabels = 0 then
            Application.Terminate;
      end
      else if i18Manager.LoadAllLabels(sForm.edtTranslateFile.Text) > 0 then
         FTranslateFile := sForm.edtTranslateFile.Text;
   end;

   var lang := GInfra.GetLangDefinition(sForm.cbLanguage.Text);
   if lang <> nil then
   begin
      lang.CompilerCommand := Trim(sForm.edtCompiler.Text);
      lang.CompilerCommandNoMain := Trim(sForm.edtCompilerNoMain.Text);
      lang.CompilerFileEncoding := sForm.cbFileEncoding.Text;
   end;

   if FShowFuncLabels <> sForm.chkShowFuncLabels.Checked then
   begin
      FShowFuncLabels := not FShowFuncLabels;
      redrawFlow := True;
   end;

   if FShowBlockLabels <> sForm.chkShowBlockLabels.Checked then
   begin
      FShowBlockLabels := not FShowBlockLabels;
      redrawFlow := True;
   end;

   var flowFontName: string := sForm.edtFontNameSize.Text;
   var tokens := flowFontName.Split([FLOWCHART_FONT_NAMESIZE_SEP], 2);
   flowFontName := tokens[0];
   var flowFontSize := tokens[1].ToInteger;

   if (GProject <> nil) and ((FEnableDBuffering <> sForm.chkEnableDBuffer.Checked)
                        or (GInfra.CurrentLang.Name <> sForm.cbLanguage.Text)
                        or (FFlowchartFontName <> flowFontName)
                        or (FFlowchartFontSize <> flowFontSize)) then
   begin
      if TInfra.ShowQuestionBox('CloseProjectAsk', [sLineBreak]) = mrYes then
         TInfra.Reset
      else
         applyAll := False;
   end;

   if applyAll then
   begin
      FEnableDBuffering := sForm.chkEnableDBuffer.Checked;
      FFlowchartFontName := flowFontName;
      FFlowchartFontSize := flowFontSize;
      if CurrentLangName <> sForm.cbLanguage.Text then
      begin
         CurrentLangName := sForm.cbLanguage.Text;
   {$IFDEF USE_CODEFOLDING}
         TInfra.GetEditorForm.ReloadFoldRegions;
   {$ENDIF}
      end;
   end;

   if GProject <> nil then
      GProject.RefreshStatements;

   TInfra.GetEditorForm.SetFormAttributes;

   FPrintRect.Left   := StrToIntDef(sForm.edtMarginLeft.Text, DEFAULT_PRINT_MARGIN);
   FPrintRect.Top    := StrToIntDef(sForm.edtMarginTop.Text, DEFAULT_PRINT_MARGIN);
   FPrintRect.Right  := PRINT_SCALE_BASE - StrToIntDef(sForm.edtMarginRight.Text, DEFAULT_PRINT_MARGIN);
   FPrintRect.Bottom := PRINT_SCALE_BASE - StrToIntDef(sForm.edtMarginBottom.Text, DEFAULT_PRINT_MARGIN);

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

function TSettings.UpdateEditor: boolean;
begin
   result := TInfra.GetEditorForm.Visible and FEditorAutoUpdate;
end;

function TSettings.ExecuteParse(AParserMode: TYYMode): boolean;
begin
   case AParserMode of
      yymInput:     result := FParseInput;
      yymOutput:    result := FParseOutput;
      yymAssign:    result := FParseAssign;
      yymFuncCall:  result := FParseRoutineCall;
      yymFor:       result := FParseFor;
      yymReturn:    result := FParseReturn;
      yymCondition: result := FParseCondition;
      yymCase,
      yymCaseValue: result := FParseCase;
   else
      result := False;
   end;
end;

end.
