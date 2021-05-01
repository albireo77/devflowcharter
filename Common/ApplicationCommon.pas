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



unit ApplicationCommon;

interface

uses
   WinApi.Windows, Vcl.Forms, Vcl.StdCtrls, Vcl.Grids, Vcl.Controls, Vcl.Graphics,
   System.SysUtils, System.Classes, System.StrUtils,
   Vcl.ComCtrls, LocalizationManager, Project, Settings, LangDefinition, CommonTypes,
   Base_Form, CommonInterfaces, Functions_Form, DataTypes_Form, Declarations_Form,
   Main_Form, Base_Block, SynEditTypes, Settings_Form, Editor_Form, Explorer_Form,
   UserFunction, BlockTabSheet, About_Form, YaccLib;

type

   TClipbrd = record
      UndoObject: TObject;  // last removed TBlock or TComment
      Instance: TControl;   // TBlock or TComment which actually is copied to clipboard
   end;

   TInfra = class(TObject)
      private
         FTemplateLang,
         FCurrentLang: TLangDefinition;
         FLangArray: array of TLangDefinition;
         FLangArrayHigh: integer;
         class var FParsedEdit: TCustomEdit;
      public
         property CurrentLang: TLangDefinition read FCurrentLang;
         property TemplateLang: TLangDefinition read FTemplateLang;
         constructor Create;
         destructor Destroy; override;
         class procedure ShowWarningBox(const AWarnMsg: string);
         class procedure ShowFormattedWarningBox(const AKey: string; Args: array of const);
         class procedure ShowErrorBox(const AErrorMsg: string; AError: TError);
         class procedure ShowFormattedErrorBox(const AKey: string; Args: array of const; AError: TError);
         class procedure SetInitialSettings;
         class procedure PopulateDataTypeCombo(AcbType: TComboBox; ASkipIndex: integer = 100);
         class procedure PrintBitmap(ABitmap: TBitmap);
         class function InsertTemplateLines(ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; AObject: TObject = nil): integer; overload;
         class function InsertTemplateLines(ADestList: TStringList; const APlaceHolder: string; ATemplate: TStringList; AObject: TObject = nil): integer; overload;
         class procedure ChangeLine(const ALine: TChangeLine);
         class procedure SetFontSize(AControl: TControl; ASize: integer);
         class procedure UpdateCodeEditor(AObject: TObject = nil);
         class procedure OnKeyDownSelectAll(Sender: TObject; var Key: Word; Shift: TShiftState);
         class procedure InsertLinesIntoList(ADestList, ASourceList: TStringList; AFromLine: integer);
         class procedure DecrementNodeSiblingOffsets(ANode: TTreeNode);
         class procedure DeleteLinesContaining(ALines: TStrings; const AText: string);
         class procedure MoveWin(AWinControl: TWinControl; x, y: integer); overload;
         class procedure MoveWin(AWinControl: TWinControl; const APoint: TPoint); overload;
         class procedure MoveWinTopZ(AWinControl: TWinControl; x, y: integer);
         class procedure IndentSpacesToTabs(ALines: TStringList);
         class function GetScrolledPoint(AMemo: TCustomMemo): TPoint;
         class function CreateDOSProcess(const ACommand: string; ADir: string = ''): boolean;
         class function ShowQuestionBox(const AMsg: string; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class function ShowFormattedQuestionBox(const AKey: string; Args: array of const; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class function FindText(ASubstr, AText: string; idx: integer; ACaseSens: boolean): integer;
         class function IsPrinter: boolean;
         class function IsValidControl(AObject: TObject): boolean;
         class function SameStrings(const AStr1: string; const AStr2: string): boolean;
         class function GetDataTypesForm: TDataTypesForm;
         class function GetFunctionsForm: TFunctionsForm;
         class function GetDeclarationsForm: TDeclarationsForm;
         class function GetEditorForm: TEditorForm;
         class function GetSettingsForm: TSettingsForm;
         class function GetAboutForm: TAboutForm;
         class function GetExplorerForm: TExplorerForm;
         class function GetMainForm: TMainForm;
         class function GetActiveEdit: TCustomEdit;
         class function GetParsedBlock: TBlock;
         class function GetParsedEdit: TCustomEdit;
         class function Parse(AEdit: TCustomEdit; AParserMode: TYYMode): boolean; overload;
         class function Parse(const AText: string; AParserMode: TYYMode): boolean; overload;
         class function IsNOkColor(AColor: TColor): boolean;
         class function GetChangeLine(AObject: TObject; AEdit: TCustomEdit = nil; const ATemplate: string = ''): TChangeLine;
         class function GetCaretPos(AEdit: TCustomEdit): TBufferCoord;
         class function ExtractIndentString(const AText: string): string;
         class function GetFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
         class function FindDuplicatedPage(APage: TTabSheet; const ACaption: TCaption): TTabSheet;
         class function GetComboMaxWidth(ACombo: TComboBox): integer;
         class function ExportToFile(AExport: IExportable): TError;
         class function StripInstrEnd(const ALine: string): string;
         class function CompareProgramVersion(const AVersion: string): integer;
         class function GetBaseForms: IEnumerable<TBaseForm>;
         class function DecodeFontStyle(AValue: integer): TFontStyles;
         class function EncodeFontStyle(AStyle: TFontStyles): integer;
         class function GetDimensionCount(const AText: string): integer;
         class function GetDimensions(const AText: string): TArray<string>;
         class function GetTextWidth(const AText: string; AControl: TControl): integer;
         class function GetAutoWidth(AControl: TControl): integer;
         class function GetLibObject: TObject;
         class function GetParserErrMsg: string;
         class function FindLastRow(AObject: TObject; AStart: integer; ALines: TStrings): integer;
         class function DecodeCheckBoxState(const AState: string): TCheckBoxState;
         class function GetPageFromXY(APageControl: TPageControl; x, y: integer): TTabSheet;
         class function GetPageFromTabIndex(APageControl: TPageControl; ATabIndex: integer): TTabSheet;
         class function IndexOf<T>(const AValue: T; const AArray: TArray<T>): integer;
         function GetNativeDataType(const AName: string): PNativeDataType;
         function GetNativeFunction(const AName: string): PNativeFunction;
         function GetLangDefinition(const AName: string): TLangDefinition;
         function SetCurrentLang(const ALangName: string): TLangDefinition;
         function ValidateConstId(const AId: string): integer;
         function ValidateId(const AId: string): integer;
         function ParseVarSize(const ASize: string): boolean;
         procedure SetLangHiglighterAttributes;
         procedure GetLangNames(AList: TStrings);
         procedure SetHLighters;
   end;

const   // Global constants

        PROGRAM_NAME        = 'devFlowcharter';

        NEW_PROJECT_CAPTION = PROGRAM_NAME;
        PROJECT_CAPTION     = NEW_PROJECT_CAPTION + ' - ';

        // hint duration in milliseconds
        HINT_PAUSE       = 5000;

        INCORRECT_IDENT  = -6;
        DUPLICATED_IDENT = -7;
        RESERVED_IDENT   = -8;
        INVALID_INIT_VAL = -9;
        VALID_IDENT      =  1;

        LOOP_BLOCKS = [blWhile, blRepeat, blFor];

        EDITOR_DEFAULT_INDENT_LENGTH = 2;
        EDITOR_DEFAULT_FONT_SIZE = 10;
        LABEL_DEFAULT_FONT_SIZE = 10;
        EDITOR_DEFAULT_GUTTER_FONT_SIZE = 8;

        TAB_CHAR        = #9;
        SPACE_CHAR      = #32;
        INDENT_XML_CHAR = TAB_CHAR;

        PAGE_CAPTION_ATTR = 'tab';
        COMMENT_ATTR      = 'comment';
        PAGE_FRONT_ATTR   = 'pageFront';
        LANG_ATTR         = 'language';
        FOLDED_ATTR       = 'folded';
        FOLD_TEXT_ATTR    = 'foldtext';
        FRAME_ATTR        = 'frame';
        BLOCK_TYPE_ATTR   = 'type';
        ID_ATTR           = 'hash';
        BRANCH_STMNT_ATTR = 'bstmnt_hash';
        FONT_SIZE_ATTR    = 'fontsize';
        FONT_STYLE_ATTR   = 'fontstyle';
        Z_ORDER_ATTR      = 'ZOrdVal';
        EXTERN_ATTR       = 'extern';
        SIZE_ATTR         = 'size';
        INIT_ATTR         = 'init';
        VALUE_ATTR        = 'value';
        NAME_ATTR         = 'name';
        TYPE_ATTR         = 'type';
        IS_HEADER_ATTR    = 'isHeader';
        APP_VERSION_ATTR  = 'devFVersion';
        KIND_ATTR         = 'kind';
        POINTER_ATTR      = 'pointer';
        BLOCK_TAG         = 'block';
        BRANCH_TAG        = 'branch';
        TEXT_TAG          = 'text';
        VAR_TAG           = 'var';
        CONST_TAG         = 'const';
        DATATYPE_TAG      = 'datatype';
        FUNCTION_TAG      = 'routine';
        HEADER_TAG        = 'header';
        FILE_CONTENTS_TAG = 'FileContentsTemplate';

        LB_PHOLDER  = '#!';
        LB_PHOLDER2  = '##';

        VERSION_NUMBER_SEP  = '.';

        MAIN_PAGE_MARKER  = 'mainPage#!';

        MARGIN_X = 50;
        MARGIN_Y = 50;

        OK_COLOR = clGreen;
        NOK_COLOR = clRed;
        WARN_COLOR = clOlive;
        TEXT_COLOR = clGrayText;
        BLACK_COLOR = clWindowText;
        DEFAULT_DESKTOP_COLOR = clWhite;
        MATCH_BRACKET_COLOR = clRed;

        ID_ALLOW_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

        FLOWCHART_DEFAULT_FONT_NAME = 'Tahoma';
        FLOWCHART_MIN_FONT_SIZE = 8;
        FLOWCHART_MAX_FONT_SIZE = FLOWCHART_MIN_FONT_SIZE + 4;
        FLOWCHART_VALID_FONT_SIZES = [FLOWCHART_MIN_FONT_SIZE..FLOWCHART_MAX_FONT_SIZE];
        FLOWCHART_FONT_NAMESIZE_SEP = ' : ';

        ROW_NOT_FOUND = -1;
        BRANCH_IDX_NOT_FOUND = -1;

        FUNCTION_TYPE_IND = -5;

        PRIMARY_PLACEHOLDER = '%s1';

        DEF_PAGE_CAPTION_KEY = 'mainPage';

        UNKNOWN_VERSION = 'unknown';

        SETTINGS_SECTION = 'Settings';

        PRINT_SCALE_BASE     = 100;   // 100 %
        DEFAULT_PRINT_MARGIN = 5;     //   5 %

        DECLARATIONS_FORM_RIGHT_MARGIN = 16;

        RTF_FILES_FILTER_KEY = 'RTFFilesFilter';
        HTML_FILES_FILTER_KEY = 'HTMLFilesFilter';
        XML_FILES_FILTER_KEY = 'XMLFilesFilter';
        BMP_FILES_FILTER_KEY = 'BMPFilesFilter';
        PNG_FILES_FILTER_KEY = 'PNGFilesFilter';
        JPG_FILES_FILTER_KEY = 'JPGFilesFilter';
        EDITOR_DIALOG_FILTER_KEYS: TArray<string> = [RTF_FILES_FILTER_KEY, HTML_FILES_FILTER_KEY];
        PROJECT_DIALOG_FILTER_KEYS: TArray<string> = [XML_FILES_FILTER_KEY, BMP_FILES_FILTER_KEY, PNG_FILES_FILTER_KEY, JPG_FILES_FILTER_KEY];

        // Language identifiers; must be identical to value in <Name> tag in XML language definition file
        PASCAL_LANG_ID  = 'Pascal';
        C_LANG_ID       = 'ANSI C';
        TIBASIC_LANG_ID = 'TIBASIC';
        PYTHON_LANG_ID  = 'Python 3';
        JAVA_LANG_ID    = 'Java';

var     // Global variables

    GClpbrd:        TClipbrd;
    GInfra:         TInfra;
    GProject:       TProject;
    GSettings:      TSettings;
    GCustomCursor:  TCustomCursor;
    GErr_text:      string;
    i18Manager:     Ti18Manager;

    function CompareIntegers(AList: TStringList; idx1, idx2: integer): integer;

implementation

uses
   Vcl.Printers, WinApi.Messages, Vcl.Menus, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.Imaging.PngImage,
   System.Math, System.TypInfo, Generics.Collections, System.IOUtils, System.Rtti,
   UserDataType, XMLProcessor, SynEditHighlighter, Main_Block, BaseEnumerator, System.Character,
   System.Generics.Defaults;

type
   THackCustomEdit = class(TCustomEdit);
   THackControl = class(TControl);

constructor TInfra.Create;
var
   searchRec: TSearchRec;
   lang: TLangDefinition;
   lFile, langDir: string;
begin
   inherited Create;
   langDir := GSettings.LanguageDefinitionsDir;
   if FindFirst(langDir + '*.xml', faAnyFile, searchRec) = 0 then
   try
      repeat
         lFile := TPath.GetFullPath(langDir + searchRec.Name);
         lang := TLangDefinition.Create;
         if TXMLProcessor.ImportFromXMLFile(lang.ImportFromXML, impAll, lFile, true).IsEmpty then
            lang.Free
         else
         begin
            lang.DefFile := lFile;
            lang.LoadCompilerData;
            FLangArray := FLangArray + [lang];
         end;
      until FindNext(searchRec) <> 0;
   finally
      FindClose(searchRec);
   end;
   FTemplateLang := TLangDefinition.Create;
   FLangArray := FLangArray + [FTemplateLang];
   FCurrentLang := FLangArray[0];
   FLangArrayHigh := High(FLangArray);
end;

destructor TInfra.Destroy;
var
   i: integer;
begin
   for i := 0 to FLangArrayHigh do
      FLangArray[i].Free;
   FLangArray := nil;
   inherited Destroy;
end;

class procedure TInfra.UpdateCodeEditor(AObject: TObject = nil);
begin
   if GSettings.UpdateEditor then
      GetEditorForm.RefreshEditorForObject(AObject);
   GProject.SetChanged;
end;

class function TInfra.ExportToFile(AExport: IExportable): TError;
var
   graphic: TGraphic;
   dialog: TSaveDialog;
   filterKey: string;
begin
   result := errNone;
   if AExport <> nil then
   begin
      dialog := GetMainForm.ExportDialog;
      dialog.FileName := AExport.GetExportFileName;
      dialog.Filter := i18Manager.GetJoinedString('|', PROJECT_DIALOG_FILTER_KEYS);
      dialog.FilterIndex := 1;
      if dialog.Execute then
      begin
         graphic := nil;
         filterKey := PROJECT_DIALOG_FILTER_KEYS[dialog.FilterIndex-1];
         if filterKey = XML_FILES_FILTER_KEY then
            result := AExport.ExportToXMLFile(dialog.Filename)
         else if filterKey = BMP_FILES_FILTER_KEY then
            graphic := TBitmap.Create
         else if filterKey = PNG_FILES_FILTER_KEY then
            graphic := TPNGImage.Create
         else if filterKey = JPG_FILES_FILTER_KEY then
            graphic := TJPEGImage.Create;
         if graphic <> nil then
         try
            AExport.ExportToGraphic(graphic);
            graphic.SaveToFile(dialog.Filename);
         finally
            graphic.Free;
         end;
      end;
   end;
end;

class function TInfra.IndexOf<T>(const AValue: T; const AArray: TArray<T>): integer;
var
   i: integer;
begin
   result := -1;
   for i := 0 to High(AArray) do
   begin
      if TComparer<T>.Default.Compare(AValue, AArray[i]) = 0 then
      begin
         result := i;
         break;
      end;
   end;
end;

function TInfra.SetCurrentLang(const ALangName: string): TLangDefinition;
var
   lang: TLangDefinition;
begin
   if ALangName.IsEmpty then
      FCurrentLang := FLangArray[0]
   else
   begin
      lang := GetLangDefinition(ALangName);
      if lang <> nil then
         FCurrentLang := lang;
   end;
   result := FCurrentLang;
end;

procedure TInfra.GetLangNames(AList: TStrings);
var
   i: integer;
begin
   for i := 0 to FLangArrayHigh do
      AList.Add(FLangArray[i].Name);
end;

function TInfra.GetNativeDataType(const AName: string): PNativeDataType;
var
   i: integer;
begin
   result := nil;
   for i := 0 to High(FCurrentLang.NativeDataTypes) do
   begin
      if SameStrings(AName, FCurrentLang.NativeDataTypes[i].Name) then
      begin
         result := @FCurrentLang.NativeDataTypes[i];
         break;
      end;
   end;
end;

function TInfra.GetNativeFunction(const AName: string): PNativeFunction;
var
   i: integer;
begin
   result := nil;
   for i := 0 to High(FCurrentLang.NativeFunctions) do
   begin
      if SameStrings(AName, FCurrentLang.NativeFunctions[i].Name) then
      begin
         result := @FCurrentLang.NativeFunctions[i];
         break;
      end;
   end;
end;

procedure TInfra.SetHLighters;
var
   i: integer;
   comp: TComponent;
   lang: TLangDefinition;
begin
   for i := 0 to FLangArrayHigh-1 do
   begin
      lang := FLangArray[i];
      comp := GetEditorForm.FindComponent(lang.HighLighterVarName);
      if comp is TSynCustomHighlighter then
         lang.HighLighter := TSynCustomHighlighter(comp);
      if lang = GInfra.CurrentLang then
         GSettings.UpdateForHLighter(lang.HighLighter);
   end;
end;

class procedure TInfra.OnKeyDownSelectAll(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if (ssCtrl in Shift) and (Key = Ord('A')) and (Sender is TCustomEdit) then
      TCustomEdit(Sender).SelectAll;
end;

class function TInfra.CreateDOSProcess(const ACommand: string; ADir: string = ''): boolean;
var
   StartupInfo: TStartupInfo;
   ProcessInfo: TProcessInformation;
begin
   if not DirectoryExists(ADir) then
      ADir := GetCurrentDir;
   StartupInfo := Default(TStartupInfo);
   StartupInfo.cb := SizeOf(StartupInfo);
   StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow := SW_SHOW;
   result := CreateProcess(nil,
      PChar(ACommand),
      nil,
      nil,
      True,
      CREATE_NEW_CONSOLE or
      NORMAL_PRIORITY_CLASS,
      nil,
      PChar(ADir),
      StartupInfo,
      ProcessInfo);
   if result then
   begin
      WaitForSingleObject(ProcessInfo.hProcess, 0);
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
   end;
end;

class procedure TInfra.ShowErrorBox(const AErrorMsg: string; AError: TError);
const
   ErrorsTypeArray: array[TError] of string = (' ', 'DeclareError', 'IOError', 'ValidationError', 'ConvertError', 'SyntaxError',
                    'PrintError', 'CompileError', 'ImportError', 'Error');
begin
   if AError <> errNone then
      Application.MessageBox(PChar(AErrorMsg), PChar(i18Manager.GetString(ErrorsTypeArray[AError])), MB_ICONERROR);
end;

class procedure TInfra.ShowFormattedErrorBox(const AKey: string; Args: array of const; AError: TError);
begin
   ShowErrorBox(i18Manager.GetFormattedString(AKey, Args), AError);
end;

class procedure TInfra.ShowFormattedWarningBox(const AKey: string; Args: array of const);
begin
   ShowWarningBox(i18Manager.GetFormattedString(AKey, Args));
end;

class procedure TInfra.ShowWarningBox(const AWarnMsg: string);
begin
   Application.MessageBox(PChar(AWarnMsg), PChar(i18Manager.GetString('Warning')), MB_ICONWARNING);
end;

class function TInfra.ShowQuestionBox(const AMsg: string; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
begin
   result := Application.MessageBox(PChar(AMsg), PChar(i18Manager.GetString('Confirmation')), AFlags);
end;

class function TInfra.ShowFormattedQuestionBox(const AKey: string; Args: array of const; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
begin
   result := ShowQuestionBox(i18Manager.GetFormattedString(AKey, Args), AFlags);
end;

class procedure TInfra.SetInitialSettings;
var
   baseForm: TBaseForm;
begin
   with GClpbrd do
   begin
      UndoObject.Free;
      UndoObject := nil;
      Instance := nil;
   end;
   FParsedEdit := nil;
   GProject.Free;
   GProject := nil;
   GCustomCursor := crNormal;
   Screen.Cursor := crDefault;
   for baseForm in GetBaseForms do
      baseForm.ResetForm;
end;

class function TInfra.GetBaseForms: IEnumerable<TBaseForm>;
var
   list: TList<TBaseForm>;
   i: integer;
   comp: TComponent;
begin
   list := TList<TBaseForm>.Create;
   for i := 0 to Application.ComponentCount-1 do
   begin
      comp := Application.Components[i];
      if comp is TBaseForm then
         list.Add(TBaseForm(comp));
   end;
   result := TEnumeratorFactory<TBaseForm>.Create(list);
end;

// compares two strings based on current case-sensitive context
class function TInfra.SameStrings(const AStr1: string; const AStr2: string): boolean;
begin
   if GInfra.CurrentLang.CaseSensitiveSyntax then
      result := SameStr(AStr1, AStr2)
   else
      result := SameText(AStr1, AStr2);
end;

class procedure TInfra.PrintBitmap(ABitmap: TBitmap);
var
   i, maxValue, maxTmp, last_err, stepY, rowCount, lineHeight, stepX, LogPixX1,
   LogPixY1, LogPixX2, LogPixY2, ScaleX, ScaleY, colCount, j: integer;
   fPoint, pos: TPoint;
   fLine1, fLine2, fLine3: string;
   status: DWORD;
   HeaderSize, ImageSize: DWORD;
   BitmapHeader: pBitmapInfo;
   BitmapImage: POINTER;
   printRect: TRect;
begin
   if not IsPrinter then
     ShowErrorBox(i18Manager.GetString('NoPrinter'), errPrinter)
   else if MainForm.PrintDialog.Execute then
   begin
     last_err := 0;
     ABitmap.PixelFormat := pf24bit;
     status := GDI_ERROR;
     Printer.Orientation := poPortrait;
     Printer.Copies := MainForm.PrintDialog.Copies;
     Printer.BeginDoc;
     Printer.Canvas.Font.Name := FLOWCHART_DEFAULT_FONT_NAME;
     Printer.Canvas.Font.Height := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY) * 12 div 72;
     lineHeight := Printer.Canvas.TextHeight('X');
     Inc(lineHeight, lineHeight * 8 div 100);
     printRect := GSettings.PrintRect;
     ScaleX := Printer.PageWidth div PRINT_SCALE_BASE;
     ScaleY := Printer.PageHeight div PRINT_SCALE_BASE;
     printRect.Width := ScaleX * printRect.Width;
     printRect.Height := ScaleY * printRect.Height - lineHeight;
     printRect.Left := ScaleX * printRect.Left;
     printRect.Top := ScaleY * printRect.Top;
     fPoint := printRect.BottomRight;
     stepX := ABitmap.Width;
     stepY := ABitmap.Height;
     rowCount := 1;
     colCount := 1;
     if GSettings.PrintMultPages then
     begin
        printRect.Height := printRect.Height - lineHeight;
        if GSettings.PrintMultPagesHorz then
        begin
           printRect.Height := printRect.Height - 2*lineHeight;
           LogPixX1 := GetDeviceCaps(ABitmap.Canvas.Handle, LOGPIXELSX);
           LogPixY1 := GetDeviceCaps(ABitmap.Canvas.Handle, LOGPIXELSY);
           LogPixX2 := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
           LogPixY2 := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY);
           if LogPixX1 > LogPixX2 then      // horizontal
              ScaleX := LogPixX1 div LogPixX2
           else
              ScaleX := LogPixX2 div LogPixX1;
           if LogPixY1 > LogPixY2 then      // vertical
              ScaleY := LogPixY1 div LogPixY2
           else
              ScaleY := LogPixY2 div LogPixY1;
           stepX := printRect.Width div ScaleX;
           stepY := printRect.Height div ScaleY;
        end
        else
           stepY := printRect.Height * ABitmap.Width div printRect.Width;
        colCount := ABitmap.Width div stepX;
        if not IsZero(ABitmap.Width mod stepX) then
           Inc(colCount);
        rowCount := ABitmap.Height div stepY;
        if not IsZero(ABitmap.Height mod stepY) then
           Inc(rowCount);
     end;
     if (stepX / stepY) > (printRect.Width / printRect.Height) then
        printRect.Height := stepY * printRect.Width div stepX
     else
        printRect.Width := stepX * printRect.Height div stepY;
     for i := 1 to colCount do
     begin
        for j := 1 to rowCount do
        begin
           GetDIBSizes(ABitmap.Handle, HeaderSize, ImageSize);
           GetMem(BitmapHeader, HeaderSize);
           GetMem(BitmapImage, ImageSize);
           try
              GetDIB(ABitmap.Handle, ABitmap.Palette, BitmapHeader^, BitmapImage^);
              status := StretchDIBits(Printer.Canvas.Handle,
                                      printRect.Left,
                                      printRect.Top,
                                      printRect.Width,
                                      printRect.Height,
                                      (i-1)*stepX,
                                      ABitmap.Height-j*stepY,
                                      stepX,
                                      stepY,
                                      BitmapImage,
                                      TBitmapInfo(BitmapHeader^),
                                      DIB_RGB_COLORS,
                                      SRCCOPY);
           finally
              FreeMem(BitmapHeader, HeaderSize);
              FreeMem(BitmapImage, ImageSize);
           end;
           if status = GDI_ERROR then
           begin
              last_err := GetLastError;
              Printer.Abort;
              break;
           end
           else
           begin
              pos := fPoint;
              if GSettings.PrintMultPages then
              begin
                 fLine1 := i18Manager.GetFormattedString('PageCount', [(i-1)*rowCount+j, colCount*rowCount]);
                 fLine2 := i18Manager.GetFormattedString('ColumnCount', [i, colCount]);
                 fLine3 := i18Manager.GetFormattedString('RowCount', [j, rowCount]);
                 maxValue := Printer.Canvas.TextWidth(GProject.Name);
                 maxTmp := Printer.Canvas.TextWidth(fLine1);
                 if maxValue < maxTmp then
                    maxValue := maxTmp;
                 maxTmp := Printer.Canvas.TextWidth(fLine2);
                 if maxValue < maxTmp then
                    maxValue := maxTmp;
                 maxTmp := Printer.Canvas.TextWidth(fLine3);
                 if maxValue < maxTmp then
                    maxValue := maxTmp;
                 Dec(pos.X, maxValue);
                 if GSettings.PrintMultPagesHorz then
                 begin
                    Printer.Canvas.TextOut(pos.X, pos.Y, fLine3);
                    Dec(pos.Y, lineHeight);
                    Printer.Canvas.TextOut(pos.X, pos.Y, fLine2);
                    Dec(pos.Y, lineHeight);
                 end;
                 Printer.Canvas.TextOut(pos.X, pos.Y, fLine1);
                 Dec(pos.Y, lineHeight);
              end
              else
                 Dec(pos.X, Printer.Canvas.TextWidth(GProject.Name));
              Printer.Canvas.TextOut(pos.X, pos.Y, GProject.Name);
              if not ((j = rowCount) and (i = colCount)) then
                 Printer.NewPage;
           end;
        end;
        if status = GDI_ERROR then break;
     end;
     if status <> GDI_ERROR then
        Printer.EndDoc
     else
        ShowErrorBox(i18Manager.GetFormattedString('PrinterError', [sLineBreak, SysErrorMessage(last_err)]), errPrinter);
   end;
end;

class function TInfra.FindText(ASubstr, AText: string; idx: integer; ACaseSens: boolean): integer;
begin
   AText := Copy(AText, idx);
   if not ACaseSens then
   begin
      AText := AText.ToUpper;
      ASubstr := ASubstr.ToUpper;
   end;
   result := Pos(ASubstr, AText);
   if result > 0 then
      result :=  result + idx - 1;
end;

class function TInfra.IsPrinter: boolean;
begin
   result := true;
   try
      Printer.PrinterIndex := -1;
   except
      result := false;
   end;
end;

class function TInfra.IsValidControl(AObject: TObject): boolean;
begin
   try
      result := (AObject is TControl) and (TControl(AObject).Parent <> nil);
   except
      result := false;
   end;
end;

class function TInfra.GetComboMaxWidth(ACombo: TComboBox): integer;
var
   i, len: integer;
begin
   result := 0;
   if ACombo <> nil then
   begin
      for i := 0 to ACombo.Items.Count-1 do
      begin
         len := ACombo.Canvas.TextWidth(ACombo.Items[i]);
         if len > result then
            result := len;
      end;
      result := result + 28;
   end;
end;

class procedure TInfra.PopulateDataTypeCombo(AcbType: TComboBox; ASkipIndex: integer = 100);
var
   i, idx: integer;
   userType: TUserDataType;
   lType, lName: string;
   lang: TLangDefinition;
begin
   lType := AcbType.Text;
   AcbType.Items.BeginUpdate;
   AcbType.Clear;

   if AcbType.Tag = FUNCTION_TYPE_IND then
      AcbType.Items.Add(i18Manager.GetString('NoType'));

// first, populate with native types from language definition XML file
   for i := 0 to High(GInfra.CurrentLang.NativeDataTypes) do
      AcbType.Items.Add(GInfra.CurrentLang.NativeDataTypes[i].Name);

// next, populate with user data types defined in GUI
   if GInfra.CurrentLang.EnabledUserDataTypes and (GProject <> nil) then
   begin
      lang := nil;
      if GInfra.CurrentLang.EnabledPointers then
      begin
         if Assigned(GInfra.CurrentLang.GetPointerTypeName) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.TemplateLang.GetPointerTypeName) then
            lang := GInfra.TemplateLang;
      end;
      for userType in GProject.GetUserDataTypes do
      begin
         lName := userType.GetName;
         if (userType.PageIndex < ASkipIndex) and not lName.IsEmpty then
         begin
            AcbType.Items.Add(lName);
            if userType.chkAddPtrType.Checked and (lang <> nil) then
               AcbType.Items.Add(lang.GetPointerTypeName(lName));
         end;
      end;
   end;
   idx := AcbType.Items.IndexOf(lType);
   if idx = -1 then
      idx := 0;
   AcbType.ItemIndex := idx;
   AcbType.Items.EndUpdate;
   AcbType.Width := GetComboMaxWidth(AcbType);
   AcbType.DropDownCount := AcbType.Items.Count;
   AcbType.Hint := AcbType.Text;
end;

function TInfra.GetLangDefinition(const AName: string): TLangDefinition;
var
   i: integer;
begin
   result := nil;
   for i := 0 to FLangArrayHigh do
   begin
      if SameText(FLangArray[i].Name, AName) then
      begin
         result := FLangArray[i];
         break;
      end;
   end;
end;

procedure TInfra.SetLangHiglighterAttributes;
var
   i: integer;
begin
   for i := 0 to FLangArrayHigh do
   begin
      if Assigned(FLangArray[i].SetHLighterAttrs) then
         FLangArray[i].SetHLighterAttrs;
   end;
end;

class function TInfra.StripInstrEnd(const ALine: string): string;
var
   iend: string;
begin
   result := ALine;
   iend := GInfra.CurrentLang.InstrEnd;
   if (not result.IsEmpty) and not iend.IsEmpty then
   begin
      if result.Trim = iend then
         result := ReplaceStr(result, iend, '')
      else if EndsText(iend + iend, result) then
         SetLength(result, result.Length - iend.Length);
   end;
end;

class function TInfra.FindLastRow(AObject: TObject; AStart: integer; ALines: TStrings): integer;
var
   i: integer;
begin
   result := AStart;
   for i := result+1 to ALines.Count-1 do
   begin
      if ALines.Objects[i] = AObject then
         result := i;
   end;
end;

class function TInfra.InsertTemplateLines(ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; AObject: TObject = nil): integer;
var
   template: TStringList;
begin
   template := nil;
   if not ATemplateString.IsEmpty then
   begin
      template := TStringList.Create;
      template.Text := ATemplateString;
   end;
   try
      result := InsertTemplateLines(ADestList, APlaceHolder, template, AObject);
   finally
      template.Free;
   end;
end;

class function TInfra.InsertTemplateLines(ADestList: TStringList; const APlaceHolder: string; ATemplate: TStringList; AObject: TObject = nil): integer;
var
   i, a, p: integer;
   lBegin, lEnd: string;
   obj: TObject;
begin
   result := -1;
   i := 0;
   while i < ADestList.Count do
   begin
      p := Pos(APlaceHolder, ADestList[i]);
      if p <> 0 then
      begin
         lBegin := '';
         lEnd := '';
         if (ATemplate <> nil) and (ATemplate.Count > 0) then
         begin
            for a := p+APlaceHolder.Length to ADestList[i].Length do
               lEnd := lEnd + ADestList[i][a];
            for a := 1 to p-1 do
               lBegin := lBegin + ADestList[i][a];
            if ADestList.Capacity < ADestList.Count + ATemplate.Count then
               ADestList.Capacity := ADestList.Count + ATemplate.Count;
            for a := ATemplate.Count-1 downto 0 do
            begin
               if AObject <> nil then
                  obj := AObject
               else
                  obj := ATemplate.Objects[a];
               ADestList.InsertObject(i, lBegin + ATemplate[a] + lEnd, obj);
            end;
            ADestList.Delete(i+ATemplate.Count);
         end
         else
         begin
            if ADestList[i].Trim = APlaceHolder then
               ADestList.Delete(i)
            else
            begin
               ADestList[i] := ReplaceStr(ADestList[i], APlaceHolder, '');
               if AObject <> nil then
                  ADestList.Objects[i] := AObject;
            end;
         end;
         result := i;
         break;
      end;
      i := i + 1;
   end;
end;

class procedure TInfra.InsertLinesIntoList(ADestList, ASourceList: TStringList; AFromLine: integer);
var
   i, lineCount: integer;
begin
   if AFromLine < 0 then
      ADestList.AddStrings(ASourceList)
   else
   begin
      lineCount := ADestList.Count + ASourceList.Count;
      if ADestList.Capacity < lineCount then
         ADestList.Capacity := lineCount;
      for i := ASourceList.Count-1 downto 0 do
         ADestList.InsertObject(AFromLine, ASourceList.Strings[i], ASourceList.Objects[i]);
   end;
end;

class function TInfra.DecodeFontStyle(AValue: integer): TFontStyles;
begin
   result := [];
   if (AValue and 1) = 1 then
      Include(result, fsBold);
   if (AValue and 2) = 2 then
      Include(result, fsItalic);
   if (AValue and 4) = 4 then
      Include(result, fsUnderline);
   if (AValue and 8) = 8 then
      Include(result, fsStrikeOut);
end;

class function TInfra.EncodeFontStyle(AStyle: TFontStyles): integer;
begin
   result := 0;
   if fsBold in AStyle then
      result := 1;
   if fsItalic in AStyle then
      result := result + 2;
   if fsUnderline in AStyle then
      result := result + 4;
   if fsStrikeOut in AStyle then
      result := result + 8;
end;

class procedure TInfra.DecrementNodeSiblingOffsets(ANode: TTreeNode);
var
   i: integer;
   node: TTreeNodeWithFriend;
begin
   if ANode.Parent <> nil then
   begin
      for i := ANode.Index+1 to ANode.Parent.Count-1 do
      begin
         node := TTreeNodeWithFriend(ANode.Parent.Item[i]);
         if (node.Data <> nil) and (node.Data = ANode.Data) and (node.Offset > 0) then
            node.Offset := node.Offset - 1;
      end;
   end;
end;

class function TInfra.GetDataTypesForm: TDataTypesForm;
begin
   result := DataTypesForm;
end;

class function TInfra.GetFunctionsForm: TFunctionsForm;
begin
   result := FunctionsForm;
end;

class function TInfra.GetDeclarationsForm: TDeclarationsForm;
begin
   result := DeclarationsForm;
end;

class function TInfra.GetMainForm: TMainForm;
begin
   result := MainForm;
end;

class function TInfra.GetEditorForm: TEditorForm;
begin
   result := EditorForm;
end;

class function TInfra.GetSettingsForm: TSettingsForm;
begin
   result := SettingsForm;
end;

class function TInfra.GetAboutForm: TAboutForm;
begin
   result := AboutForm;
end;

class function TInfra.GetExplorerForm: TExplorerForm;
begin
   result := ExplorerForm;
end;

class function TInfra.CompareProgramVersion(const AVersion: string): integer;
var
   currVersion: string;
   nums, numsCurr: TArray<string>;
   i, e1, e2: integer;
begin
   result := 0;
   currVersion := GetAboutForm.GetProgramVersion;
   if AVersion.IsEmpty or (currVersion = UNKNOWN_VERSION) or (currVersion = AVersion) then
      Exit;
   nums := AVersion.Split([VERSION_NUMBER_SEP], 4);
   numsCurr := currVersion.Split([VERSION_NUMBER_SEP], 4);
   for i := 0 to High(numsCurr) do
   begin
      if (result <> 0) or (i > High(nums)) then
         break;
      e1 := StrToIntDef(nums[i], -1);
      e2 := StrToIntDef(numsCurr[i], -1);
      if e1 > e2 then
         result := 1
      else if e1 < e2 then
         result := -1;
   end;
end;

class function TInfra.GetActiveEdit: TCustomEdit;
var
   control: TControl;
begin
   result := nil;
   control := GetMainForm.ActiveControl;
   if (control is TCustomEdit) and control.HasParent then
      result := TCustomEdit(control);
end;

class function TInfra.GetParsedBlock: TBlock;
var
   edit: TCustomEdit;
begin
   result := nil;
   edit := GetParsedEdit;
   if (edit <> nil) and (edit.Parent is TBlock) then
      result := TBlock(edit.Parent);
end;

class function TInfra.GetParsedEdit: TCustomEdit;
begin
   if FParsedEdit = nil then
      FParsedEdit := GetActiveEdit;
   result := FParsedEdit;
end;

class function TInfra.Parse(AEdit: TCustomEdit; AParserMode: TYYMode): boolean;
begin
   result := false;
   FParsedEdit := AEdit;
   try
      result := Parse(Trim(AEdit.Text), AParserMode);
   except on E: Exception do
      Application.ShowException(E);
   end;
   FParsedEdit := nil;
end;

class function TInfra.Parse(const AText: string; AParserMode: TYYMode): boolean;
begin
   result := true;
   if Assigned(GInfra.CurrentLang.Parse) then
      result := GInfra.CurrentLang.Parse(AText, AParserMode);
end;

class function TInfra.GetParserErrMsg: string;
begin
   result := '';
   if GInfra.CurrentLang.Parser <> nil then
      result := GInfra.CurrentLang.Parser.GetErrMsg;
end;

class function TInfra.IsNOkColor(AColor: TColor): boolean;
begin
   result := (AColor = NOK_COLOR) or (AColor = WARN_COLOR);
end;

class function TInfra.GetChangeLine(AObject: TObject; AEdit: TCustomEdit = nil; const ATemplate: string = ''): TChangeLine;
var
   templateLines: TStringList;
   i, p: integer;
   indent, template: string;
begin
   p := 0;
   result := TChangeLine.New;
   result.EditCaretXY := TInfra.GetCaretPos(AEdit);
   if AObject <> nil then
   begin
      result.CodeRange := GetEditorForm.SelectCodeRange(AObject, false);
      if result.CodeRange.FirstRow <> ROW_NOT_FOUND then
      begin
         templateLines := TStringList.Create;
         try
            template := ATemplate;
            if template.IsEmpty then
            begin
               if AObject is TBlock then
                  template := GInfra.CurrentLang.GetBlockTemplate(TBlock(AObject).BType);
               if template.IsEmpty then
                  template := PRIMARY_PLACEHOLDER;
            end;
            templateLines.Text := template;
            for i := 0 to templateLines.Count-1 do
            begin
               p := Pos(PRIMARY_PLACEHOLDER, templateLines[i]);
               if p <> 0 then
               begin
                  if (i = templateLines.Count-1) and (i <> 0) then
                     result.Row := result.CodeRange.LastRow
                  else
                     result.Row := result.CodeRange.FirstRow + i;
                  result.Text := templateLines[i];
                  break;
               end;
            end;
            indent := TInfra.ExtractIndentString(result.CodeRange.Lines[result.Row]);
            result.Col := indent.Length;
            if result.Row = ROW_NOT_FOUND then    // row with placeholder not found
            begin
               result.Row := result.CodeRange.FirstRow;
               result.Text := result.CodeRange.Lines[result.Row];
            end
            else
            begin
               result.Text := indent + result.Text.TrimLeft;
               result.Col := p + result.Col;
            end;
         finally
            templateLines.Free;
         end;
      end;
   end;
end;

class function TInfra.GetScrolledPoint(AMemo: TCustomMemo): TPoint;
var
   c: cardinal;
   charIndex: integer;
   r: TRect;
begin
   AMemo.Perform(EM_GETRECT, 0, LPARAM(@r));
   c := AMemo.Perform(EM_CHARFROMPOS, 0, MakeLong(r.Left, r.Top+2));
   charIndex := LOWORD(c);
   result.Y := HIWORD(c);
   result.X := charIndex - AMemo.Perform(EM_LINEINDEX, result.Y, 0);
end;

class function TInfra.GetCaretPos(AEdit: TCustomEdit): TBufferCoord;
begin
   result := BufferCoord(0, 0);
   if AEdit is TCustomMemo then
   begin
      result.Line := SendMessage(AEdit.Handle, EM_LINEFROMCHAR, AEdit.SelStart, 0);
      result.Char := AEdit.SelStart - SendMessage(AEdit.Handle, EM_LINEINDEX, result.Line, 0);
   end
   else if AEdit <> nil then
      result.Char := AEdit.SelStart;
end;

class function TInfra.ExtractIndentString(const AText: string): string;
var
   i, len: integer;
begin
   result := AText;
   i := 1;
   len := result.Length;
   while (i <= len) and result[i].IsWhiteSpace do
      i := i + 1;
   SetLength(result, i-1);
end;

class function TInfra.GetFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
var
   mainBlock: TMainBlock;
begin
   result := nil;
   if ABlock <> nil then
   begin
      mainBlock := TMainBlock(ABlock.TopParentBlock);
      if mainBlock.UserFunction is TUserFunction then
         result := TUserFunction(mainBlock.UserFunction).Header;
   end;
end;

class procedure TInfra.ChangeLine(const ALine: TChangeLine);
begin
   if (ALine.CodeRange.Lines <> nil) and (ALine.Row >= 0) and (ALine.Row < ALine.CodeRange.Lines.Count) then
      ALine.CodeRange.Lines[ALine.Row] := ALine.Text;
end;

class procedure TInfra.SetFontSize(AControl: TControl; ASize: integer);
var
   flag: boolean;
begin
   flag := (AControl is TCustomEdit) and (THackCustomEdit(AControl).BorderStyle = bsNone);
   if flag then THackCustomEdit(AControl).BorderStyle := bsSingle;
   THackControl(AControl).Font.Size := ASize;
   if flag then THackCustomEdit(AControl).BorderStyle := bsNone;
end;

class function TInfra.FindDuplicatedPage(APage: TTabSheet; const ACaption: TCaption): TTabSheet;
var
   i: integer;
   page: TTabSheet;
begin
   result := nil;
   if APage <> nil then
   begin
      for i := 0 to APage.PageControl.PageCount-1 do
      begin
         page := APage.PageControl.Pages[i];
         if (page <> APage) and SameCaption(page.Caption, ACaption) then
         begin
            result := page;
            break;
         end;
      end;
   end;
end;

class function TInfra.DecodeCheckBoxState(const AState: string): TCheckBoxState;
var
   i: integer;
begin
   if MatchText(AState, ['0', 'false', '']) then
      result := cbUnchecked
   else if (CompareText(AState, 'true') = 0) or TryStrToInt(AState, i) then
      result := cbChecked
   else
      result := TRttiEnumerationType.GetValue<TCheckBoxState>(AState);
end;

class function TInfra.GetLibObject: TObject;
begin
   result := GetEditorForm.memCodeEditor;
end;

class function TInfra.GetTextWidth(const AText: string; AControl: TControl): integer;
var
   fontInfo: PPropInfo;
   prop: TObject;
begin
   result := 0;
   fontInfo := GetPropInfo(AControl, 'Font');
   if fontInfo <> nil then
   begin
      prop := GetObjectProp(AControl, fontInfo);
      if prop is TFont then
      begin
         with TControlCanvas.Create do
         try
            Control := AControl;
            Font.Assign(TFont(prop));
            result := TextWidth(AText);
         finally
            Free;
         end;
      end;
   end;
end;

class function TInfra.GetAutoWidth(AControl: TControl): integer;
begin
   result := -1;
   if AControl is TCheckBox then
      result := GetTextWidth(TCheckBox(AControl).Caption, AControl) + GetSystemMetrics(SM_CXMENUCHECK) + 3
   else if AControl is TCustomEdit then
      result := GetTextWidth(TCustomEdit(AControl).Text, AControl);
end;

class procedure TInfra.IndentSpacesToTabs(ALines: TStringList);
var
   i, a: integer;
   line: string;
begin
   for i := 0 to ALines.Count-1 do
   begin
      line := ALines[i];
      a := Length(ExtractIndentString(line));
      if a > 0 then
         ALines[i] := StringOfChar(TAB_CHAR, a) + Copy(line, a + 1);
   end;
end;

class function TInfra.GetDimensions(const AText: string): TArray<string>;
var
   txt, s: string;
   d, i: integer;
begin
   txt := ReplaceStr(AText, ' ', '');
   d := GetDimensionCount(txt);
   if d < 1 then
      Exit(nil);
   SetLength(result, d);
   s := '';
   d := 0;
   for i := 1 to txt.Length do
   begin
      if txt[i] = ']' then
      begin
         result[d] := s;
         s := '';
         d := d + 1;
      end
      else if txt[i] <> '[' then
         s := s + txt[i];
   end;
end;

class function TInfra.GetDimensionCount(const AText: string): integer;
var
   i, len: integer;
   txt: string;
   nextOpen: boolean;
begin
   txt := ReplaceStr(AText, ' ', '');
   len := txt.Length;
   if len = 0 then
      Exit(-1);
   if txt[1] = '[' then
   begin
      if txt[len] <> ']' then
         Exit(-1);
      result := 0;
      nextOpen := true;
      for i := 1 to len do
      begin
         if txt[i] = '[' then
         begin
            if nextOpen then
               nextOpen := false
            else
            begin
               result := -1;
               break;
            end;
         end
         else if txt[i] = ']' then
         begin
            if not nextOpen then
            begin
               if (txt[i-1] = '[') and not GInfra.CurrentLang.AllowUnboundedArrays then
               begin
                  result := -1;
                  break;
               end;
               nextOpen := true;
            end
            else
            begin
               result := -1;
               break;
            end;
            result := result + 1;
         end;
      end;
   end
   else if txt <> '1' then
      result := -1
   else
      result := 0;
end;

class procedure TInfra.DeleteLinesContaining(ALines: TStrings; const AText: string);
var
   i: integer;
begin
   for i := ALines.Count-1 downto 0 do
   begin
      if ALines.Strings[i].Contains(AText) then
         ALines.Delete(i);
   end;
end;

class procedure TInfra.MoveWin(AWinControl: TWinControl; const APoint: TPoint);
begin
   MoveWin(AWinControl, APoint.X, APoint.Y);
end;

class procedure TInfra.MoveWin(AWinControl: TWinControl; x, y: integer);
begin
   SetWindowPos(AWinControl.Handle, 0, x, y, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
end;

class procedure TInfra.MoveWinTopZ(AWinControl: TWinControl; x, y: integer);
begin
   SetWindowPos(AWinControl.Handle, HWND_TOP, x, y, 0, 0, SWP_NOSIZE);
end;

class function TInfra.GetPageFromXY(APageControl: TPageControl; x, y: integer): TTabSheet;
begin
   result := GetPageFromTabIndex(APageControl, APageControl.IndexOfTabAt(x, y));
end;

class function TInfra.GetPageFromTabIndex(APageControl: TPageControl; ATabIndex: integer): TTabSheet;
var
   i, idx: integer;
begin
   result := nil;
   idx := ATabIndex;
   for i := 0 to ATabIndex do
   begin
      if not APageControl.Pages[i].TabVisible then
         Inc(idx);
   end;
   if idx <> -1 then
      result := APageControl.Pages[idx];
end;

function TInfra.ValidateConstId(const AId: string): integer;
var
   i: integer;
begin
   result := ValidateId(AId);
   if (result = INCORRECT_IDENT) and (not CurrentLang.ConstIDSpecChars.IsEmpty) and not AId.Trim.IsEmpty then
   begin
      result := VALID_IDENT;
      for i := 1 to AId.Length do
      begin
         if (Pos(AId[i], CurrentLang.ConstIDSpecChars) = 0) and not CharInSet(AId[i], ID_ALLOW_CHARS) then
         begin
            result := INCORRECT_IDENT;
            break;
         end;
      end;
   end;
end;

function TInfra.ParseVarSize(const ASize: string): boolean;
var
   lang: TLangDefinition;
   goParse: boolean;
begin
   result := true;
   lang := GetLangDefinition(PASCAL_LANG_ID);
   goParse := (lang <> nil) and Assigned(lang.Parse);
   if (ASize <> '') and ((ASize[1] = '0') or (ASize[1] = '-') or (goParse and not lang.Parse(ASize, yymVarSize))) then
      result := false;
end;

function TInfra.ValidateId(const AId: string): integer;
begin
   result := VALID_IDENT;
   if not IsValidIdent(AId) then
      result := INCORRECT_IDENT
   else if CurrentLang.Keywords.IndexOf(AId) <> -1 then
      result := RESERVED_IDENT;
end;

function CompareIntegers(AList: TStringList; idx1, idx2: integer): integer;
begin
   result := AList[idx1].ToInteger - AList[idx2].ToInteger;
end;

initialization

   GSettings := TSettings.Create;

   i18Manager := Ti18Manager.Create;
   if i18Manager.LoadDynamicLabels(GSettings.TranslateFile) = 0 then
      i18Manager.LoadDefaultLabels;

   GInfra := TInfra.Create;

finalization

   GInfra.Free;
   GInfra := nil;

   GSettings.Save;
   GSettings.Free;
   GSettings := nil;

   i18Manager.Free;
   i18Manager := nil;

end.


