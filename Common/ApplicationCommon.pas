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
   Windows, Forms, StdCtrls, Grids, Controls, Graphics, Registry, SysUtils, Classes,
   StrUtils, Types, ComCtrls, LocalizationManager, Project, Settings, LangDefinition,
   CommonTypes, Base_Form, CommonInterfaces, Functions_Form, DataTypes_Form, Declarations_Form,
   Main_Form, Base_Block, SynEditTypes, Settings_Form, Editor_Form, Explorer_Form,
   UserFunction;

type

   TClipbrd = record
      UndoObject: TObject;  // last removed TBlock or TComment
      Instance: TControl;   // TBlock or TComment which actually is copied to clipboard
   end;

   TIdentInfo = record
      Ident: string;
      IdentType,
      TType,
      TypeOriginal,
      TypePointer,
      Size: integer;
      SizeAsString,
      SizeExpArrayAsString,
      TypeAsString,
      TypeOriginalAsString,
      Value: string;
      DimensCount: integer;
      IsInteger,
      IsReal,
      IsNumeric,
      IsStruct,
      IsEnum,
      IsPointer: boolean;
      Scope: integer;
   end;

   TInfra = class(TObject)
      private
         FDummyLang,
         FCurrentLang: TLangDefinition;
         FLangArray: array of TLangDefinition;
      public
         property CurrentLang: TLangDefinition read FCurrentLang;
         property DummyLang: TLangDefinition read FDummyLang;
         class function IsWin9x: boolean;
         class function CreateDOSProcess(const ACmdLine: string; const ADir: string = ''): Boolean;
         class procedure ShowErrorBox(const AErrMsg: string; const AErrType: TErrorType);
         class procedure ShowFormattedErrorBox(const AKey: string; Args: array of const; const AErrType: TErrorType);
         class function ShowQuestionBox(const AMsg: string; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class function ShowFormattedQuestionBox(const AKey: string; Args: array of const; AFlags: Longint = MB_ICONQUESTION + MB_YESNOCANCEL): integer;
         class procedure SetInitialSettings;
         class function RPos(const AChar: char; const AString: string): integer;
         class function FindText(ASubstr, AText: string; const AStartIndex: integer; const ACaseSens: boolean): integer;
         class function IsPrinter: boolean;
         class function IsValid(const AObject: TObject): boolean;
         class function SameStrings(const AStr1: string; const AStr2: string): boolean;
         class procedure PopulateDataTypeCombo(const AcbType: TComboBox; const ASkipIndex: integer = 100);
         class procedure PrintBitmap(const ABitmap: TBitmap);
         class procedure InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; const AObject: TObject = nil); overload;
         class procedure InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplate: TStringList; const AObject: TObject = nil); overload;
         function GetNativeDataType(const AName: string): PNativeDataType;
         function GetLangDefinition(const AName: string): TLangDefinition;
         function GetLangIndex(const AName: string): integer;
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
         class function ParentToClient(const AControl: TControl; const APoint: TPoint; AParent: TWinControl = nil): TPoint;
         class function ClientToParent(const AControl: TControl; const APoint: TPoint; AParent: TWinControl = nil): TPoint;
         class procedure UpdateCodeEditor(AObject: TObject = nil);
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

        OK_COLOR    = clGreen;
        NOK_COLOR   = clRed;
        WARN_COLOR  = clOlive;
        TEXT_COLOR  = clGrayText;
        BLACK_COLOR = clWindowText;

        FLOWCHART_DEFAULT_FONT_NAME = 'Tahoma';

        CRLF = #13#10;

        ID_ALLOW_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

        ROW_NOT_FOUND = -1;

        FUNCTION_TYPE_IND = -5;

        PRIMARY_PLACEHOLDER = '%s1';

        LANG_DEFS_PATH = 'LanguageDefinitions\';

        DEF_PAGE_CAPTION_KEY = 'mainPage';

        // Language identifiers; must be identical to value in <Name> tag in XML language definition file
        PASCAL_LANG_ID  = 'Pascal';
        C_LANG_ID       = 'ANSI C';
        TIBASIC_LANG_ID = 'TIBASIC';
        DUMMY_LANG_ID   = '   ';

        KEY_COMPILER_COMMAND        = 'CompilerPath';
        KEY_COMPILER_COMMAND_NOMAIN = 'CompilerPathNoMain';
        KEY_CURRENT_LANGUAGE        = 'CurrentLanguageName';

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
   Printers, UserDataType, XMLProcessor, SynEditHighlighter, Main_Block, Messages, Menus,
   FastcodeAnsiStringReplaceUnit;

type
   THackCustomEdit = class(TCustomEdit);
   THackControl = class(TControl);

var
   FParsedEdit: TCustomEdit;

constructor TInfra.Create;
var
   i: integer;
   SearchRec: TSearchRec;
   lLangDef: TLangDefinition;
   lFile: string;
begin
   inherited Create;
   i := 0;
   if FindFirst(LANG_DEFS_PATH + '*.xml', faAnyFile, SearchRec) = 0 then
   try
      repeat
         lFile := LANG_DEFS_PATH + SearchRec.Name;
         lLangDef := TLangDefinition.Create;
         if TXMLProcessor.ImportFromXMLFile(lLangDef.ImportLangDef, lFile, true) <> '' then
         begin
            lLangDef.DefFile := lFile;
            SetLength(FLangArray, i+1);
            FLangArray[i] := lLangDef;
            i := i + 1;
         end
         else
            lLangDef.Free;
      until FindNext(SearchRec) <> 0;
   finally
      FindClose(SearchRec);
   end;
   SetLength(FLangArray, i+1);
   FLangArray[i] := TLangDefinition.Create;
   FDummyLang := FLangArray[i];
   FCurrentLang := FLangArray[0];
end;

destructor TInfra.Destroy;
var
   i: integer;
begin
   for i := 0 to High(FLangArray) do
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
   lRegistry: TRegistry;
begin
   lRegistry := TRegistry.Create;
   try
      if lRegistry.OpenKey(REGISTRY_KEY, true) then
      begin
         lRegistry.WriteString(KEY_CURRENT_LANGUAGE, FCurrentLang.Name);
         for i := 0 to High(FLangArray)-1 do
         begin
            lRegistry.WriteString(KEY_COMPILER_COMMAND + '_' + FLangArray[i].Name, FLangArray[i].CompilerCommand);
            lRegistry.WriteString(KEY_COMPILER_COMMAND_NOMAIN + '_' + FLangArray[i].Name, FLangArray[i].CompilerCommandNoMain);
         end;
      end;
   finally
      lRegistry.Free;
   end;
end;

procedure TInfra.ReadFromRegistry;
var
   lLangDef: TLangDefinition;
   lRegistry: TRegistry;
   i: integer;
begin
   lRegistry := TRegistry.Create;
   try
      if lRegistry.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
         if lRegistry.ValueExists(KEY_CURRENT_LANGUAGE) then
         begin
            lLangDef := GetLangDefinition(lRegistry.ReadString(KEY_CURRENT_LANGUAGE));
            if lLangDef <> nil then
               FCurrentLang := lLangDef
         end;
         for i := 0 to High(FLangArray)-1 do
         begin
            if lRegistry.ValueExists(KEY_COMPILER_COMMAND + '_' + FLangArray[i].Name) then
                FLangArray[i].CompilerCommand := lRegistry.ReadString(KEY_COMPILER_COMMAND + '_' + FLangArray[i].Name);
            if lRegistry.ValueExists(KEY_COMPILER_COMMAND_NOMAIN + '_' + FLangArray[i].Name) then
                FLangArray[i].CompilerCommandNoMain := lRegistry.ReadString(KEY_COMPILER_COMMAND_NOMAIN + '_' + FLangArray[i].Name);
         end;
      end;
   finally
      lRegistry.Free;
   end;
end;

function TInfra.SetCurrentLang(const ALangName: string): TLangDefinition;
var
   lLangDef: TLangDefinition;
begin
   lLangDef := GetLangDefinition(ALangName);
   if (lLangDef <> nil) and (lLangDef <> FCurrentLang) then
   begin
      FCurrentLang := lLangDef;
      GSettings.UpdateForLang(FCurrentLang);
   end;
   result := FCurrentLang;
end;

procedure TInfra.GetLangNames(const AList: TStrings);
var
   i: integer;
begin
   for i := 0 to High(FLangArray) do
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
   lComponent: TComponent;
begin
   for i := 0 to High(FLangArray)-1 do
   begin
      lComponent := GetEditorForm.FindComponent(FLangArray[i].HighLighterVarName);
      if lComponent is TSynCustomHighlighter then
         FLangArray[i].HighLighter := TSynCustomHighlighter(lComponent);
   end;
end;

class function TInfra.IsWin9x: boolean;
var
   lpVerInfo: OSVERSIONINFO;
begin
    FillChar(lpVerInfo, SizeOf(lpVerInfo), #0);
    lpVerInfo.dwOSVersionInfoSize := SizeOf(lpVerInfo);
    result := GetVersionEx(lpVerInfo) and (lpVerInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);   // check if Win 9x
end;

class function TInfra.CreateDOSProcess(const ACmdLine: string; const ADir: string = ''): Boolean;
var
  OldCursor: TCursor;
  pCommandLine, pDirectory: array[0..MAX_PATH] of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  hAppProcess, hAppThread: THandle;
begin

  { save the cursor }
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourglass;

  { copy the parameter Pascal strings to null terminated strings }
  StrPCopy(pCommandLine, ACmdLine);
  if DirectoryExists(ADir) then
     StrPCopy(pDirectory, ADir)
  else
     StrPCopy(pDirectory, GetCurrentDir);

  try

    { prepare StartupInfo structure }
    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb          := SizeOf(StartupInfo);
    StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOW;

    { create the app }
    Result := CreateProcess(nil,     { pointer to name of executable module }
      pCommandLine,                  { pointer to command line string }
      nil,                           { pointer to process security attributes }
      nil,                           { pointer to thread security attributes }
      True,                          { handle inheritance flag }
      CREATE_NEW_CONSOLE or
      NORMAL_PRIORITY_CLASS,         { creation flags }
      nil,                           { pointer to new environment block }
      pDirectory,                    { pointer to current directory name }
      StartupInfo,                   { pointer to STARTUPINFO }
      ProcessInfo);                  { pointer to PROCESS_INF }

    { wait for the app to finish its job and take the handles to free them later }
    if Result then
    begin
      WaitForSingleObject(ProcessInfo.hProcess, 200);
      hAppProcess := ProcessInfo.hProcess;
      hAppThread  := ProcessInfo.hThread;
    end

  finally
    { close the handles
      Kernel objects, like the process and the files we created in this case,
      are maintained by a usage count.
      So, for cleaning up purposes we have to close the handles
      to inform the system that we don't need the objects anymore }
    if hAppThread <> 0 then CloseHandle(hAppThread);
    if hAppProcess <> 0 then CloseHandle(hAppProcess);
    { restore the old cursor }
    Screen.Cursor := OldCursor;
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
      result := AnsiSameStr(AStr1, AStr2)
   else
      result := AnsiSameText(AStr1, AStr2);
end;

class procedure TInfra.PrintBitmap(const ABitmap: TBitmap);
var
   i, maxValue, maxTmp, last_err, stepY, rowCount, lineHeight, regionWidth, regionHeight, stepX,
   regionX, regionY, LogPixX1, LogPixY1, LogPixX2, LogPixY2, ScaleX, ScaleY, colCount, j: integer;
   fPoint, pos: TPoint;
   fLine1, fLine2, fLine3: string;
   status: DWORD;
   HeaderSize, ImageSize: DWORD;
   BitmapHeader: pBitmapInfo;
   BitmapImage: POINTER;
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
     Printer.Canvas.Font.Height := MulDiv(GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY), 12, 72);
     lineHeight := Printer.Canvas.TextHeight('X');
     Inc(lineHeight, MulDiv(lineHeight, 8, 100));
     regionX := MulDiv(Printer.PageWidth, GSettings.PrintMargins.Left, 100);
     regionY := MulDiv(Printer.PageHeight, GSettings.PrintMargins.Top, 100);
     regionWidth := MulDiv(Printer.PageWidth, 100-(GSettings.PrintMargins.Left+GSettings.PrintMargins.Right), 100);
     regionHeight := MulDiv(Printer.PageHeight, 100-(GSettings.PrintMargins.Top+GSettings.PrintMargins.Bottom), 100) - lineHeight;
     fPoint.X := regionX + regionWidth;
     fPoint.Y := regionY + regionHeight;
     stepX := ABitmap.Width;
     stepY := ABitmap.Height;
     rowCount := 1;
     colCount := 1;
     if GSettings.PrintMultPages then
     begin
        Dec(regionHeight, lineHeight);
        if GSettings.PrintMultPagesHorz then
        begin
           Dec(regionHeight, 2*lineHeight);
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
           stepX := regionWidth div ScaleX;
           stepY := regionHeight div ScaleY;
        end
        else
           stepY := MulDiv(regionHeight, ABitmap.Width, regionWidth);
        colCount := ABitmap.Width div stepX;
        if (ABitmap.Width mod stepX) <> 0 then
           Inc(colCount);
        rowCount := ABitmap.Height div stepY;
        if (ABitmap.Height mod stepY) <> 0 then
           Inc(rowCount);
     end;
     if (stepX / stepY) > (regionWidth / regionHeight) then
        regionHeight := MulDiv(stepY, regionWidth, stepX)
     else
        regionWidth  := MulDiv(stepX, regionHeight, stepY);
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
                                      regionX,
                                      regionY,
                                      regionWidth,
                                      regionHeight,
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

class function TInfra.RPos(const AChar: char; const AString: string): integer;
var
   i: integer;
begin
   result := 0;
   for i := Length(AString) downto 1 do
   begin
      if AString[i] = AChar then
      begin
         result := i;
         break;
      end;
   end;
end;

class function TInfra.FindText(ASubstr, AText: string; const AStartIndex: integer; const ACaseSens: boolean): integer;
begin
   AText := AnsiRightStr(AText, Length(AText)-AStartIndex);
   if not ACaseSens then
   begin
      AText := AnsiUpperCase(AText);
      ASubstr := AnsiUpperCase(ASubstr);
   end;
   result := AnsiPos(ASubstr, AText) - 1;
   if result <> -1 then
      result :=  result + AStartIndex;
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
type
  PPVmt = ^PVmt;
  PVmt = ^TVmt;
  TVmt = record
     SelfPtr: TClass;
     Other: array[0..17] of Pointer;
  end;
var
  Vmt: PVmt;
begin
  result := false;
  if Assigned(AObject) then
  begin
     try
        Vmt := PVmt(AObject.ClassType);
        Dec(Vmt);
        if AObject.ClassType = Vmt.SelfPtr then
           result := true;
     except
     end;
  end;
end;

class function TInfra.FindParentForm(const AControl: TControl): TBaseForm;
var
   lWinControl: TWinControl;
begin
   result := nil;
   if AControl is TBaseForm then
      result := TBaseForm(AControl)
   else if AControl <> nil then
   begin
      lWinControl := AControl.Parent;
      while (lWinControl <> nil) and not (lWinControl is TBaseForm) do
         lWinControl := lWinControl.Parent;
      if lWinControl is TBaseForm then
         result := TBaseForm(lWinControl);
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
   lDataType: TUserDataType;
   lType, lName: string;
   iter: IIterator;
   lLang: TLangDefinition;
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
         lDataType := TUserDataType(iter.Next);
         lName := lDataType.GetName;
         if (lDataType.PageIndex < ASkipIndex) and (lName <> '') then
         begin
            AcbType.Items.Add(lName);
            if lDataType.chkAddPtrType.Checked then
            begin
               lLang := nil;
               if Assigned(GInfra.CurrentLang.GetPointerTypeName) then
                  lLang := GInfra.CurrentLang
               else if Assigned(GInfra.DummyLang.GetPointerTypeName) then
                  lLang := GInfra.DummyLang;
               if lLang <> nil then
                  AcbType.Items.Add(lLang.GetPointerTypeName(lName));
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

function TInfra.GetLangIndex(const AName: string): integer;
var
   i: integer;
begin
   result := -1;
   for i := 0 to High(FLangArray) do
   begin
      if AnsiSameText(FLangArray[i].Name, AName) then
      begin
         result := i;
         break;
      end;
   end;
end;

function TInfra.GetLangDefinition(const AName: string): TLangDefinition;
var
   i: integer;
begin
   result := nil;
   for i := 0 to High(FLangArray) do
   begin
      if AnsiSameText(FLangArray[i].Name, AName) then
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
   for i := 0 to High(FLangArray) do
   begin
      if Assigned(FLangArray[i].SetHLighterAttrs) then
         FLangArray[i].SetHLighterAttrs;
   end;
end;

class procedure TInfra.InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplateString: string; const AObject: TObject = nil);
var
   lTemplate: TStringList;
begin
   lTemplate := nil;
   if ATemplateString <> '' then
   begin
      lTemplate := TStringList.Create;
      lTemplate.Text := ATemplateString;
   end;
   try
      InsertTemplateLines(ADestList, APlaceHolder, lTemplate, AObject);
   finally
      lTemplate.Free;
   end;
end;

class procedure TInfra.InsertTemplateLines(const ADestList: TStringList; const APlaceHolder: string; const ATemplate: TStringList; const AObject: TObject = nil);
var
   i, a, lPos: integer;
   lBegin, lEnd: string;
   lObject: TObject;
begin
   i := 0;
   while i < ADestList.Count do
   begin
      lPos := AnsiPos(APlaceHolder, ADestList[i]);
      if lPos <> 0 then
      begin
         lBegin := '';
         lEnd := '';
         if (ATemplate <> nil) and (ATemplate.Count > 0) then
         begin
            for a := lPos+Length(APlaceHolder) to Length(ADestList[i]) do
               lEnd := lEnd + ADestList[i][a];
            for a := 1 to lPos-1 do
               lBegin := lBegin + ADestList[i][a];
            if ADestList.Capacity < ADestList.Count + ATemplate.Count then
               ADestList.Capacity := ADestList.Count + ATemplate.Count;
            for a := ATemplate.Count-1 downto 0 do
            begin
               if AObject <> nil then
                  lObject := AObject
               else
                  lObject := ATemplate.Objects[a];
               ADestList.InsertObject(i, lBegin + ATemplate[a] + lEnd, lObject);
            end;
            ADestList.Delete(i+ATemplate.Count);
         end
         else
         begin
            if Trim(ADestList[i]) = APlaceHolder then
               ADestList.Delete(i)
            else
            begin
               ADestList[i] := FastCodeAnsiStringReplace(ADestList[i], APlaceHolder, '');
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
   i, lLineCount: integer;
begin
   if AFromLine < 0 then
      ADestList.AddStrings(ASourceList)
   else
   begin
      lLineCount := ADestList.Count + ASourceList.Count;
      if ADestList.Capacity < lLineCount then
         ADestList.Capacity := lLineCount;
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
   lValue: integer;
begin
   lValue := 0;
   if fsBold in AStyle then
      lValue := 1;
   if fsItalic in AStyle then
      lValue := lValue + 2;
   if fsUnderline in AStyle then
      lValue := lValue + 4;
   if fsStrikeOut in AStyle then
      lValue := lValue + 8;
   result := IntToStr(lValue);
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
   lControl: TControl;
begin
   result := nil;
   lControl := GetMainForm.ActiveControl;
   if (lControl is TCustomEdit) and lControl.HasParent then
      result := TCustomEdit(lControl);
end;

class function TInfra.GetParsedBlock: TBlock;
var
   lEdit: TCustomEdit;
begin
   result := nil;
   lEdit := GetParsedEdit;
   if (lEdit <> nil) and (lEdit.Parent is TBlock) then
      result := TBlock(lEdit.Parent);
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
   lTemplateLines: TStringList;
   i, lPos: integer;
   lIndent: string;
begin
   lPos := 0;
   InitChangeLine(result);
   result.EditCaretXY := TInfra.GetCaretPos(AEdit);
   if AObject <> nil then
   begin
      result.CodeRange := GetEditorForm.SelectCodeRange(AObject, false);
      if result.CodeRange.FirstRow <> ROW_NOT_FOUND then
      begin
         lTemplateLines := TStringList.Create;
         try
            if ATemplate <> '' then
               lTemplateLines.Text := ATemplate
            else
               lTemplateLines.Text := GInfra.CurrentLang.GetTemplate(AObject.ClassType);
            for i := 0 to lTemplateLines.Count-1 do
            begin
               lPos := AnsiPos(PRIMARY_PLACEHOLDER, lTemplateLines[i]);
               if lPos <> 0 then
               begin
                  if (i = lTemplateLines.Count-1) and (i <> 0) then
                     result.Row := result.CodeRange.LastRow
                  else
                     result.Row := result.CodeRange.FirstRow + i;
                  result.Text := lTemplateLines[i];
                  break;
               end;
            end;
            lIndent := TInfra.ExtractIndentString(result.CodeRange.Lines[result.Row]);
            result.Col := Length(lIndent);
            if result.Row = ROW_NOT_FOUND then    // row with placeholder not found
            begin
               result.Row := result.CodeRange.FirstRow;
               result.Text := result.CodeRange.Lines[result.Row];
            end
            else
            begin
               result.Text := lIndent + result.Text;
               result.Col := lPos + result.Col;
            end;
         finally
            lTemplateLines.Free;
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
   for i := 1 to Length(AText) do
   begin
      if not (AText[i] in [#32, #9, INDENT_CHAR]) then
         break;
   end;
   result := AnsiLeftStr(AText, i-1);
end;

class function TInfra.GetFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
var
   lMainBlock: TMainBlock;
begin
   result := nil;
   if ABlock <> nil then
   begin
      lMainBlock := TMainBlock(ABlock.TopParentBlock);
      if lMainBlock.UserFunction is TUserFunction then
         result := TUserFunction(lMainBlock.UserFunction).Header;
   end;
end;

class procedure TInfra.ChangeLine(const ALine: TChangeLine);
begin
   if (ALine.CodeRange.Lines <> nil) and (ALine.Row >= 0) and (ALine.Row < ALine.CodeRange.Lines.Count) then
      ALine.CodeRange.Lines[ALine.Row] := ALine.Text;
end;

class procedure TInfra.SetFontSize(const AControl: TControl; const ASize: integer);
var
   lFlag: boolean;
begin
   lFlag := (AControl is TCustomEdit) and (THackCustomEdit(AControl).BorderStyle = bsNone);
   if lFlag then THackCustomEdit(AControl).BorderStyle := bsSingle;
   THackControl(AControl).Font.Size := ASize;
   if lFlag then THackCustomEdit(AControl).BorderStyle := bsNone;
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
         if AnsiSameCaption(APage.PageControl.Pages[i].Caption, ACaption) and (APage.PageControl.Pages[i] <> APage) then
         begin
            result := APage.PageControl.Pages[i];
            break;
         end;
      end;
   end;
end;

// functions below are to fix buggy VCL TControl.ParentToClient/ClientToParent when AParent.Parent is not nil
class function TInfra.ParentToClient(const AControl: TControl; const APoint: TPoint; AParent: TWinControl = nil): TPoint;
begin
   result := AControl.ParentToClient(APoint, AParent);
   if (AParent <> nil) and (AParent.Parent <> nil) then
      result := Point(result.X + AParent.Left, result.Y + AParent.Top);
end;

class function TInfra.ClientToParent(const AControl: TControl; const APoint: TPoint; AParent: TWinControl = nil): TPoint;
begin
   result := AControl.ClientToParent(APoint, AParent);
   if (AParent <> nil) and (AParent.Parent <> nil) then
      result := Point(result.X - AParent.Left, result.Y - AParent.Top);
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
         if (AnsiPos(AId[i], CurrentLang.ConstIDSpecChars) = 0) and not (AId[i] in ID_ALLOW_CHARS) then
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


