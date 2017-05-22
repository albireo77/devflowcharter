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
   System.Win.Registry, System.SysUtils, System.Classes, System.StrUtils, Vcl.ComCtrls,
   LocalizationManager, Project, Settings, LangDefinition, CommonTypes, Base_Form,
   CommonInterfaces, Functions_Form, DataTypes_Form, Declarations_Form, Main_Form,
   Base_Block, SynEditTypes, Settings_Form, Editor_Form, Explorer_Form, UserFunction,
   BlockTabSheet;

type

   TClipbrd = record
      UndoObject: TObject;  // last removed TBlock or TComment
      Instance: TControl;   // TBlock or TComment which actually is copied to clipboard
   end;

   TInfra = class(TObject)
      private
         FDummyLang,
         FCurrentLang: TLangDefinition;
         FLangArray: array of TLangDefinition;
         FLangArrayCount: integer;
      public
         property CurrentLang: TLangDefinition read FCurrentLang;
         property DummyLang: TLangDefinition read FDummyLang;
         class function CreateDOSProcess(const ACommand: string; ADir: string = ''): Boolean;
         class procedure ShowErrorBox(const AErrMsg: string; const AErrType: TErrorType);
         class procedure ShowFormattedErrorBox(const AKey: string; Args: array of const; const AErrType: TErrorType);
         class function ShowQuestionBox(const AMsg: string; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class function ShowFormattedQuestionBox(const AKey: string; Args: array of const; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class procedure SetInitialSettings;
         class function FindText(ASubstr, AText: string; const idx: integer; const ACaseSens: boolean): integer;
         class function IsPrinter: boolean;
         class function IsValid(const AObject: TObject): boolean;
         class function SameStrings(const AStr1: string; const AStr2: string): boolean;
         class procedure PopulateDataTypeCombo(const AcbType: TComboBox; const ASkipIndex: integer = 100);
         class procedure PrintBitmap(const ABitmap: TBitmap);
         class procedure InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; const AObject: TObject = nil); overload;
         class procedure InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplate: TStringList; const AObject: TObject = nil); overload;
         function GetNativeDataType(const AName: string): PNativeDataType;
         function GetLangDefinition(const AName: string): TLangDefinition;
         procedure SetLangHiglighterAttributes;
         function SetCurrentLang(const ALangName: string): TLangDefinition;
         class procedure InsertLinesIntoList(ADestList, ASourceList: TStringList; AFromLine: integer);
         procedure GetLangNames(const AList: TStrings);
         class function DecodeFontStyle(AValue: integer): TFontStyles;
         class function EncodeFontStyle(AStyle: TFontStyles): string;
         class function FindParentForm(const AControl: TControl): TBaseForm;
         procedure ReadFromRegistry;
         procedure WriteToRegistry;
         procedure SetHLighters;
         class procedure InitFocusInfo(var AFocusInfo: TFocusInfo);
         class function GetDataTypesForm: TDataTypesForm;
         class function GetFunctionsForm: TFunctionsForm;
         class function GetDeclarationsForm: TDeclarationsForm;
         class function GetEditorForm: TEditorForm;
         class function GetSettingsForm: TSettingsForm;
         class function GetExplorerForm: TExplorerForm;
         class function GetMainForm: TMainForm;
         class function GetActiveEdit: TCustomEdit;
         class function GetParsedBlock: TBlock;
         class function GetParsedEdit: TCustomEdit;
         class function Parse(const AEdit: TCustomEdit; const AParserMode: TParserMode): boolean; overload;
         class function Parse(const AText: string; const AParserMode: TParserMode): boolean; overload;
         class function IsNOkColor(const AColor: TColor): boolean;
         class function GetChangeLine(const AObject: TObject; const AEdit: TCustomEdit = nil; const ATemplate: string = ''): TChangeLine;
         class function GetCaretPos(const AEdit: TCustomEdit): TBufferCoord;
         class function ExtractIndentString(const AText: string): string;
         class procedure ChangeLine(const ALine: TChangeLine);
         class procedure InitChangeLine(var AChangeLine: TChangeLine);
         class procedure InitCodeRange(var ACodeRange: TCodeRange);
         class procedure SetFontSize(const AControl: TControl; const ASize: integer);
         class function GetFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
         class function GetPageIndex(const APageControl: TPageControl; X, Y: integer): integer;
         class function FindDuplicatedPage(const APage: TTabSheet; const ACaption: TCaption): TTabSheet;
         class function GetComboMaxWidth(const ACombo: TComboBox): integer;
         class procedure UpdateCodeEditor(AObject: TObject = nil);
         class function ExportToFile(AExport: IExportable): TErrorType;
         class function GetDisplayRect(const APage: TBlockTabSheet): TRect;
         class procedure OnKeyDownSelectAll(Sender: TObject; var Key: Word; Shift: TShiftState);
         function ValidateConstId(const AId: string): integer;
         function ValidateId(const AId: string): integer;
         constructor Create;
         destructor Destroy; override;
   end;

const   // Global constants

        PROGRAM_NAME      = 'devFlowcharter';

        // registry key with application settings
        REGISTRY_KEY      = 'Software\' + PROGRAM_NAME;

        MAIN_FORM_CAPTION = PROGRAM_NAME + ' - ';

        // hint duration in milliseconds
        HINT_PAUSE       = 5000;

        INCORRECT_IDENT  = -6;
        DUPLICATED_IDENT = -7;
        RESERVED_IDENT   = -8;
        INVALID_INIT_VAL = -9;
        VALID_IDENT      =  1;

        FLOWCHART_BLOCKS = [blAssign, blMultAssign, blInput, blOutput, blFuncCall,
                            blWhile, blRepeat, blIf, blIfElse, blFor, blCase, blMain, blReturn];

        GROUP_BLOCKS = [blWhile, blRepeat, blIf, blIfElse, blFor, blCase, blMain];

        LOOP_BLOCKS = [blWhile, blRepeat, blFor];

        EDITOR_DEFAULT_INDENT_LENGTH = 2;
        EDITOR_DEFAULT_FONT_SIZE = 10;
        LABEL_DEFAULT_FONT_SIZE = 10;
        EDITOR_DEFAULT_GUTTER_FONT_SIZE = 8;

        INDENT_CHAR     = #32;         // space
        INDENT_XML_CHAR = #9;          // tab

        PAGE_CAPTION_ATTR = 'tab';
        COMMENT_ATTR      = 'comment';
        PAGE_ORDER_ATTR   = 'pageOrder';
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
        BLOCK_TAG         = 'block';
        BRANCH_TAG        = 'branch';
        TEXT_TAG          = 'text';
        VAR_TAG           = 'var';
        CONST_TAG         = 'const';
        DATATYPE_TAG      = 'structure';
        FUNCTION_TAG      = 'routine';
        HEADER_TAG        = 'header';

        CRLF_PLACEHOLDER  = '#!';

        PAGE_LIST_DELIM   = ',';

        MAIN_PAGE_MARKER  = 'mainPage#!';

        MARGIN_X = 50;
        MARGIN_Y = 50;

        OK_COLOR = clGreen;
        NOK_COLOR = clRed;
        WARN_COLOR = clOlive;
        TEXT_COLOR = clGrayText;
        BLACK_COLOR = clWindowText;
        DEFAULT_DESKTOP_COLOR = clWhite;

        FLOWCHART_DEFAULT_FONT_NAME = 'Tahoma';

        CRLF = #13#10;

        ID_ALLOW_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

        ROW_NOT_FOUND = -1;

        FUNCTION_TYPE_IND = -5;

        PRIMARY_PLACEHOLDER = '%s1';

        LANG_DEFS_PATH = 'LanguageDefinitions\';

        DEF_PAGE_CAPTION_KEY = 'mainPage';

        PRINT_SCALE_BASE     = 100;   // 100 %
        DEFAULT_PRINT_MARGIN = 5;     //   5 %

        // Language identifiers; must be identical to value in <Name> tag in XML language definition file
        PASCAL_LANG_ID  = 'Pascal';
        C_LANG_ID       = 'ANSI C';
        TIBASIC_LANG_ID = 'TIBASIC';

        KEY_CURRENT_LANGUAGE = 'CurrentLanguageName';

        PARSER_ERRORS_ARRAY: array[TParserMode] of string = ('BadGeneric', 'BadCondition', 'BadAssign', 'BadInput', 'BadOutput',
                             'BadFor', 'BadFunction', 'BadCase', 'BadCase', 'BadReturnVal', '');

var     // Global variables

    GClpbrd:        TClipbrd;
    GInfra:         TInfra;
    GProject:       TProject;
    GSettings:      TSettings;
    GChange:        byte;
    GCustomCursor:  TCustomCursor;
    GErr_text:      string;
    GParser_Mode:   TParserMode;
    i18Manager:     Ti18Manager;
    errString:      string;       // error string returned from parser

    function CompareIntegers(AList: TStringList; idx1, idx2: integer): integer;

implementation

uses
   Vcl.Printers, WinApi.Messages, Vcl.Menus, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.Imaging.PngImage,
   UserDataType, XMLProcessor, SynEditHighlighter, Main_Block;

type
   THackCustomEdit = class(TCustomEdit);
   THackControl = class(TControl);

var
   FParsedEdit: TCustomEdit;

constructor TInfra.Create;
var
   i: integer;
   searchRec: TSearchRec;
   lang: TLangDefinition;
   lFile: string;
begin
   inherited Create;
   i := 0;
   if FindFirst(LANG_DEFS_PATH + '*.xml', faAnyFile, searchRec) = 0 then
   try
      repeat
         lFile := LANG_DEFS_PATH + searchRec.Name;
         lang := TLangDefinition.Create;
         if TXMLProcessor.ImportFromXMLFile(lang.ImportFromXML, lFile, true) <> '' then
         begin
            lang.DefFile := lFile;
            SetLength(FLangArray, i+1);
            FLangArray[i] := lang;
            i := i + 1;
         end
         else
            lang.Free;
      until FindNext(searchRec) <> 0;
   finally
      FindClose(searchRec);
   end;
   FLangArrayCount := i + 1;
   SetLength(FLangArray, FLangArrayCount);
   FLangArray[i] := TLangDefinition.Create;
   FDummyLang := FLangArray[i];
   FCurrentLang := FLangArray[0];
end;

destructor TInfra.Destroy;
var
   i: integer;
begin
   for i := 0 to FLangArrayCount-1 do
      FLangArray[i].Free;
   FLangArray := nil;
   inherited Destroy;
end;

class procedure TInfra.UpdateCodeEditor(AObject: TObject = nil);
begin
   if GSettings.UpdateEditor then
      GetEditorForm.RefreshEditorForObject(AObject);
   GChange := 1;
end;

procedure TInfra.WriteToRegistry;
var
   i: integer;
   reg: TRegistry;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKey(REGISTRY_KEY, true) then
      begin
         reg.WriteString(KEY_CURRENT_LANGUAGE, FCurrentLang.Name);
         for i := 0 to FLangArrayCount-2 do
            FLangArray[i].WriteCompilerData(reg);
      end;
   finally
      reg.Free;
   end;
end;

procedure TInfra.ReadFromRegistry;
var
   lang: TLangDefinition;
   reg: TRegistry;
   i: integer;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
         if reg.ValueExists(KEY_CURRENT_LANGUAGE) then
         begin
            lang := GetLangDefinition(reg.ReadString(KEY_CURRENT_LANGUAGE));
            if lang <> nil then
               FCurrentLang := lang
         end;
         for i := 0 to FLangArrayCount-2 do
            FLangArray[i].ReadCompilerData(reg);
      end;
   finally
      reg.Free;
   end;
end;

class function TInfra.ExportToFile(AExport: IExportable): TErrorType;
var
   graphic: TGraphic;
   dialog: TSaveDialog;
begin
   result := errNone;
   if AExport <> nil then
   begin
      dialog := GetMainForm.ExportDialog;
      dialog.FileName := AExport.GetExportFileName;
      dialog.Filter := i18Manager.GetString('XMLFilesFilter') + '|' +
                       i18Manager.GetString('BMPFilesFilter') + '|' +
                       i18Manager.GetString('PNGFilesFilter') + '|' +
                       i18Manager.GetString('JPGFilesFilter');
      dialog.FilterIndex := 1;
      if dialog.Execute then
      begin
         graphic := nil;
         case dialog.FilterIndex of
            1: result := AExport.ExportToXMLFile(dialog.Filename);
            2: graphic := TBitmap.Create;
            3: graphic := TPNGImage.Create;
            4: graphic := TJPEGImage.Create;
         end;
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

function TInfra.SetCurrentLang(const ALangName: string): TLangDefinition;
var
   lang: TLangDefinition;
begin
   lang := GetLangDefinition(ALangName);
   if (lang <> nil) and (lang <> FCurrentLang) then
   begin
      FCurrentLang := lang;
      GSettings.UpdateForLang(FCurrentLang);
   end;
   result := FCurrentLang;
end;

procedure TInfra.GetLangNames(const AList: TStrings);
var
   i: integer;
begin
   for i := 0 to FLangArrayCount-1 do
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

procedure TInfra.SetHLighters;
var
   i: integer;
   comp: TComponent;
begin
   for i := 0 to FLangArrayCount-2 do
   begin
      comp := GetEditorForm.FindComponent(FLangArray[i].HighLighterVarName);
      if comp is TSynCustomHighlighter then
         FLangArray[i].HighLighter := TSynCustomHighlighter(comp);
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

class procedure TInfra.ShowErrorBox(const AErrMsg: string; const AErrType: TErrorType);
const
   ErrorsTypeArray: array[TErrorType] of string = (' ', 'DeclareError', 'IOError', 'ValidationError', 'ConvertError', 'SyntaxError',
                    'PrintError', 'CompileError', 'ImportError', 'Error');
begin
   if AErrType <> errNone then
      Application.MessageBox(PChar(AErrMsg), PChar(i18Manager.GetString(ErrorsTypeArray[AErrType])), MB_ICONERROR);
end;

class procedure TInfra.ShowFormattedErrorBox(const AKey: string; Args: array of const; const AErrType: TErrorType);
begin
   ShowErrorBox(i18Manager.GetFormattedString(AKey, Args), AErrType);
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
   itr: IIterator;
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
   GChange := 0;
   GCustomCursor := crNormal;
   Screen.Cursor := crDefault;
   itr := TBaseFormIterator.Create;
   while itr.HasNext do
      TBaseForm(itr.Next).ResetForm;
end;

// compares two strings based on current case-sensitive context
class function TInfra.SameStrings(const AStr1: string; const AStr2: string): boolean;
begin
   if GInfra.CurrentLang.CaseSensitiveSyntax then
      result := SameStr(AStr1, AStr2)
   else
      result := SameText(AStr1, AStr2);
end;

class procedure TInfra.PrintBitmap(const ABitmap: TBitmap);
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
        if (ABitmap.Width mod stepX) <> 0 then
           Inc(colCount);
        rowCount := ABitmap.Height div stepY;
        if (ABitmap.Height mod stepY) <> 0 then
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
        ShowErrorBox(i18Manager.GetFormattedString('PrintError', [CRLF, SysErrorMessage(last_err)]), errPrinter);
   end;
end;

class function TInfra.FindText(ASubstr, AText: string; const idx: integer; const ACaseSens: boolean): integer;
begin
   AText := Copy(AText, idx, MAXINT);
   if not ACaseSens then
   begin
      AText := UpperCase(AText);
      ASubstr := UpperCase(ASubstr);
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

class function TInfra.IsValid(const AObject: TObject): boolean;
begin
  result := false;
  if Assigned(AObject) then
  try
     if Pointer(PPointer(AObject)^) = Pointer(Pointer(Cardinal(PPointer(AObject)^) + Cardinal(vmtSelfPtr))^) then
        result := true;
  except
  end;
end;

class function TInfra.FindParentForm(const AControl: TControl): TBaseForm;
var
   winControl: TWinControl;
begin
   result := nil;
   if AControl is TBaseForm then
      result := TBaseForm(AControl)
   else if AControl <> nil then
   begin
      winControl := AControl.Parent;
      while (winControl <> nil) and not (winControl is TBaseForm) do
         winControl := winControl.Parent;
      if winControl is TBaseForm then
         result := TBaseForm(winControl);
   end;
end;

class function TInfra.GetComboMaxWidth(const ACombo: TComboBox): integer;
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

class procedure TInfra.PopulateDataTypeCombo(const AcbType: TComboBox; const ASkipIndex: integer = 100);
var
   i, idx: integer;
   dataType: TUserDataType;
   lType, lName: string;
   iter: IIterator;
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
      iter := GProject.GetUserDataTypes;
      while iter.HasNext do
      begin
         dataType := TUserDataType(iter.Next);
         lName := dataType.GetName;
         if (dataType.PageIndex < ASkipIndex) and (lName <> '') then
         begin
            AcbType.Items.Add(lName);
            if dataType.chkAddPtrType.Checked then
            begin
               lang := nil;
               if Assigned(GInfra.CurrentLang.GetPointerTypeName) then
                  lang := GInfra.CurrentLang
               else if Assigned(GInfra.DummyLang.GetPointerTypeName) then
                  lang := GInfra.DummyLang;
               if lang <> nil then
                  AcbType.Items.Add(lang.GetPointerTypeName(lName));
            end;
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
end;

function TInfra.GetLangDefinition(const AName: string): TLangDefinition;
var
   i: integer;
begin
   result := nil;
   for i := 0 to FLangArrayCount-1 do
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
   for i := 0 to FLangArrayCount-1 do
   begin
      if Assigned(FLangArray[i].SetHLighterAttrs) then
         FLangArray[i].SetHLighterAttrs;
   end;
end;

class procedure TInfra.InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; const AObject: TObject = nil);
var
   template: TStringList;
begin
   template := nil;
   if ATemplateString <> '' then
   begin
      template := TStringList.Create;
      template.Text := ATemplateString;
   end;
   try
      InsertTemplateLines(ADestList, APlaceHolder, template, AObject);
   finally
      template.Free;
   end;
end;

class procedure TInfra.InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplate: TStringList; const AObject: TObject = nil);
var
   i, a, p: integer;
   lBegin, lEnd: string;
   obj: TObject;
begin
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
            for a := p+Length(APlaceHolder) to Length(ADestList[i]) do
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
            if Trim(ADestList[i]) = APlaceHolder then
               ADestList.Delete(i)
            else
            begin
               ADestList[i] := ReplaceStr(ADestList[i], APlaceHolder, '');
               if AObject <> nil then
                  ADestList.Objects[i] := AObject;
            end;
         end;
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

class function TInfra.EncodeFontStyle(AStyle: TFontStyles): string;
var
   val: integer;
begin
   val := 0;
   if fsBold in AStyle then
      val := 1;
   if fsItalic in AStyle then
      val := val + 2;
   if fsUnderline in AStyle then
      val := val + 4;
   if fsStrikeOut in AStyle then
      val := val + 8;
   result := IntToStr(val);
end;

class procedure TInfra.InitFocusInfo(var AFocusInfo: TFocusInfo);
begin
   with AFocusInfo do
   begin
      Line := -1;
      RelativeLine := 0;
      SelStart := -1;
      SelText := '';
      LineText := '';
      FocusEdit := nil;
      FocusEditForm := nil;
      FocusEditCallBack := nil;
      ActiveControl := nil;
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

class function TInfra.GetExplorerForm: TExplorerForm;
begin
   result := ExplorerForm;
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

class function TInfra.Parse(const AEdit: TCustomEdit; const AParserMode: TParserMode): boolean;
begin
   FParsedEdit := AEdit;
   result := Parse(Trim(AEdit.Text), AParserMode);
   FParsedEdit := nil;
end;

class function TInfra.Parse(const AText: string; const AParserMode: TParserMode): boolean;
begin
   result := true;
   errString := '';
   if Assigned(GInfra.CurrentLang.Parse) and (GInfra.CurrentLang.Parse(AText, AParserMode) = 1) then
   begin
      if errString = '' then
         errString := i18Manager.GetString(PARSER_ERRORS_ARRAY[AParserMode]);
      result := false;
   end;
end;

class function TInfra.IsNOkColor(const AColor: TColor): boolean;
begin
   result := (AColor = NOK_COLOR) or (AColor = WARN_COLOR);
end;

class procedure TInfra.InitCodeRange(var ACodeRange: TCodeRange);
begin
   with ACodeRange do
   begin
      IsFolded := false;
      FirstRow := ROW_NOT_FOUND;
      LastRow := ROW_NOT_FOUND;
      Lines := nil;
{$IFDEF USE_CODEFOLDING}
      FoldRange := nil;
{$ENDIF}
   end;
end;

class procedure TInfra.InitChangeLine(var AChangeLine: TChangeLine);
begin
   with AChangeLine do
   begin
      Text := '';
      Row := ROW_NOT_FOUND;
      Col := 0;
      EditCaretXY := BufferCoord(0, 0);
      InitCodeRange(CodeRange);
   end;
end;

class function TInfra.GetChangeLine(const AObject: TObject; const AEdit: TCustomEdit = nil; const ATemplate: string = ''): TChangeLine;
var
   templateLines: TStringList;
   i, p: integer;
   indent: string;
begin
   p := 0;
   InitChangeLine(result);
   result.EditCaretXY := TInfra.GetCaretPos(AEdit);
   if AObject <> nil then
   begin
      result.CodeRange := GetEditorForm.SelectCodeRange(AObject, false);
      if result.CodeRange.FirstRow <> ROW_NOT_FOUND then
      begin
         templateLines := TStringList.Create;
         try
            if ATemplate <> '' then
               templateLines.Text := ATemplate
            else
               templateLines.Text := GInfra.CurrentLang.GetTemplate(AObject.ClassType);
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
            result.Col := Length(indent);
            if result.Row = ROW_NOT_FOUND then    // row with placeholder not found
            begin
               result.Row := result.CodeRange.FirstRow;
               result.Text := result.CodeRange.Lines[result.Row];
            end
            else
            begin
               result.Text := indent + result.Text;
               result.Col := p + result.Col;
            end;
         finally
            templateLines.Free;
         end;
      end;
   end;
end;

class function TInfra.GetCaretPos(const AEdit: TCustomEdit): TBufferCoord;
begin
   result.Line := 0;
   result.Char := 0;
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
   i: integer;
begin
   result := AText;
   for i := 1 to Length(result) do
   begin
      if not CharInSet(result[i], [#32, #9, INDENT_CHAR]) then
         break;
   end;
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

class procedure TInfra.SetFontSize(const AControl: TControl; const ASize: integer);
var
   flag: boolean;
begin
   flag := (AControl is TCustomEdit) and (THackCustomEdit(AControl).BorderStyle = bsNone);
   if flag then THackCustomEdit(AControl).BorderStyle := bsSingle;
   THackControl(AControl).Font.Size := ASize;
   if flag then THackCustomEdit(AControl).BorderStyle := bsNone;
end;

// function to get correct page index iwhen some pages are not visible in PageControl
class function TInfra.GetPageIndex(const APageControl: TPageControl; X, Y: integer): integer;
var
   i, c: integer;
begin
  c := APageControl.IndexOfTabAt(X, Y);
  i := 0;
  while i <= c do
  begin
    if not APageControl.Pages[i].TabVisible then
      Inc(c);
    Inc(i);
  end;
  result := c;
end;

class function TInfra.FindDuplicatedPage(const APage: TTabSheet; const ACaption: TCaption): TTabSheet;
var
   i: integer;
begin
   result := nil;
   if APage <> nil then
   begin
      for i := 0 to APage.PageControl.PageCount-1 do
      begin
         if SameCaption(APage.PageControl.Pages[i].Caption, ACaption) and (APage.PageControl.Pages[i] <> APage) then
         begin
            result := APage.PageControl.Pages[i];
            break;
         end;
      end;
   end;
end;

class function TInfra.GetDisplayRect(const APage: TBlockTabSheet): TRect;
var
   dv, dh: integer;
begin
   dv := 0;
   dh := 0;
   result.Top := APage.Form.VertScrollBar.Position - APage.Top;
   if result.Top < 0 then
   begin
      dv := result.Top;
      result.Top := 0;
   end;
   result.Left := APage.Form.HorzScrollBar.Position - APage.Left;
   if result.Left < 0 then
   begin
      dh := result.Left;
      result.Left := 0;
   end;
   result.Width := APage.Form.ClientWidth + dh;
   result.Height := APage.Form.ClientHeight + dv;
end;

function TInfra.ValidateConstId(const AId: string): integer;
var
   i: integer;
begin
   result := ValidateId(AId);
   if (result = INCORRECT_IDENT) and (CurrentLang.ConstIDSpecChars <> '') and (Trim(AId) <> '') then
   begin
      result := VALID_IDENT;
      for i := 1 to Length(AId) do
      begin
         if (Pos(AId[i], CurrentLang.ConstIDSpecChars) = 0) and not CharInSet(AId[i], ID_ALLOW_CHARS) then
         begin
            result := INCORRECT_IDENT;
            break;
         end;
      end;
   end;
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
   result := StrToInt(AList[idx1]) - StrToInt(AList[idx2]);
end;

initialization

   GSettings := TSettings.Create;
   GSettings.ReadFromRegistry;

   i18Manager := Ti18Manager.Create;
   if i18Manager.LoadDynamicLabels(GSettings.TranslateFile) = 0 then
      i18Manager.LoadDefaultLabels;

   GInfra := TInfra.Create;

finalization

   if GInfra <> nil then
      GInfra.WriteToRegistry;
   GInfra.Free;
   GInfra := nil;

end.


