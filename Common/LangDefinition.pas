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

unit LangDefinition;

interface

uses
{$IFDEF USE_CODEFOLDING}
    SynEditCodeFolding,
{$ENDIF}
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, SizeEdit,
    SynEditHighlighter, YaccLib, DeclareList, UserFunction, CommonTypes, OmniXML,
    UserDataType;

type

{$IFDEF USE_CODEFOLDING}
   TFoldRegionRecord = record
      Open,
      Close: string;
      AddClose,
      NoSubFolds,
      WholeWords: boolean;
      RegionType: TFoldRegionType;
   end;
{$ENDIF}

   TLangDefinition = class
      Name,
      CommentBegin, CommentEnd,
      DefaultExt,
      CompilerCommand,
      CompilerCommandNoMain,
      InputFunction,
      OutputFunction,
      LibraryExt,
      AssignOperator,
      VarEntry,
      VarEntryArray,
      VarEntryArraySize,
      VarEntryInit,
      VarEntryInitExtern,
      ConstTemplate,
      ConstEntry,
      ConstEntryExtern,
      WhileTemplate,
      IfTemplate,
      IfElseTemplate,
      RepeatUntilTemplate,
      ForDoTemplate,
      ElseLabel,
      LabelFontName,
      LabelRepeat,
      LabelWhile,
      LabelIf,
      LabelIfElse,
      LabelIn,
      LabelOut,
      LabelFor,
      LabelFuncCall,
      LabelAssign,
      LabelMultiAssign,
      LabelReturn,
      LabelCase,
      LabelText,
      LabelFolder,
      LabelMain,
      ReturnDesc,
      RepeatDesc,
      ForAsc1,
      ForAsc2,
      ForDesc1,
      ForDesc2,
      CaseOfTemplate,
      CaseOfValueTemplate,
      CaseOfDefaultValueTemplate,
      PointerTypeMask,
      ProgramTemplate,
      MainProgramTemplate,
      ProgramReturnTemplate,
      FunctionTemplate,
      FunctionHeaderTemplate,
      FunctionHeaderTypeNone1,
      FunctionHeaderTypeNotNone1,
      FunctionHeaderTypeNone2,
      FunctionHeaderTypeNotNone2,
      VarTemplate,
      InputTemplate,
      OutputTemplate,
      AssignTemplate,
      TextTemplate,
      FolderTemplate,
      ReturnTemplate,
      FunctionCallTemplate,
      ProgramHeaderTemplate,
      FunctionHeaderArgsEntryMask,
      FunctionHeaderArgsEntryRef,
      FunctionHeaderArgsEntryArray,
      FunctionHeaderArgsEntryRecord,
      FunctionHeaderArgsEntryEnum,
      UserTypeDesc,
      LibTemplate,
      LibEntry,
      LibEntryList,
      DataTypesTemplate,
      FunctionsTemplate,
      DataTypeIntMask,
      DataTypeRealMask,
      DataTypeOtherMask,
      DataTypeArrayMask,
      DataTypeRecordTemplate,
      DataTypeRecordFieldMask,
      DataTypeRecordFieldArrayMask,
      DataTypeEnumTemplate,
      DataTypeEnumEntryList,
      ProcedureLabelKey,
      FunctionLabelKey,
      ProgramLabelKey,
      HighLighterVarName,
      FuncBrackets,
      ExternEntry,
      ConstIDSpecChars,
      DefFile: string;
      LabelFontSize,
      FunctionHeaderArgsStripCount,
      VarEntryArraySizeStripCount,
      LibEntryListStripCount,
      DataTypeEnumEntryListStripCount: integer;
      HighLighter: TSynCustomHighlighter;
{$IFDEF USE_CODEFOLDING}
      FoldRegions: array of TFoldRegionRecord;
{$ENDIF}
      Parser: TCustomParser;
      NativeDataTypes: array of TNativeDataType;
      KeyWords: TStringList;
      NativeFunctions: TStringList;
      EnabledConsts,
      EnabledVars,
      EnabledCompiler,
      EnabledUserFunctionHeader,
      EnabledUserFunctionBody,
      EnabledUserDataTypes,
      EnabledExplorer,
      EnabledCodeGenerator,
      EnabledMainProgram,
      CaseSensitiveSyntax,
      UpperCaseConstId,
      AllowEnumsInForLoop,
      AllowUserFunctionOverload,
      RepeatUntilAsDoWhile,
      GenExternVarConst: boolean;
      InOutCursorPos,
      FuncBracketsCursorPos: integer;
      PreGenerationActivities: procedure;
      ProgramHeaderSectionGenerator: procedure (ALines: TStringList);
      LibSectionGenerator: procedure (ALines: TStringList);
      UserDataTypesSectionGenerator: procedure (ALines: TStringList);
      VarSectionGenerator: procedure (ALines: TStringList; AVarList: TVarDeclareList);
      ConstSectionGenerator: procedure (ALines: TStringList; AConstList: TConstDeclareList);
      UserFunctionsSectionGenerator: procedure (ALines: TStringList; ASkipBodyGenerate: boolean);
      MainProgramSectionGenerator: procedure (ALines: TStringList; ADeep: integer);
      GetUserFuncDesc: function (AHeader: TUserFunctionHeader): string;
      GetUserTypeDesc: function (ADataType: TUserDataType): string;
      SetHLighterAttrs: procedure;
      GetPointerTypeName: function (const val: string): string;
      GetLiteralType: function (const val: string): integer;
      IsPointerType: function (const AName: string): boolean;
      GetOriginalType: function (const APtrType: string): string;
      AreTypesCompatible: function (const AType1, AType2: integer): boolean;
      Parse: function (const AText: string; const AParserMode: TParserMode): integer;
      SkipFuncBodyGen: function: boolean;
      GetMainProgramDesc: function: string;
      constructor Create;
      destructor Destroy; override;
      function ImportLangDef(const root: IXMLElement): TErrorType;
      function GetTemplate(const AClass: TClass): string;
      function GetTemplateExpr(const AClass: TClass): string;
      function GetArraySizes(const ASizeEdit: TSizeEdit): string;
   end;


implementation

uses
   ApplicationCommon, StrUtils, XMLProcessor, WhileDo_Block, RepeatUntil_Block,
   ForDo_Block, Case_Block, If_Block, IfElse_Block, Main_Block, InOut_Block,
   Assign_Block, MulAssign_Block, Return_Block, Text_Block, FunctionCall_Block,
   Folder_Block;

constructor TLangDefinition.Create;
begin
   inherited;
   DefaultExt := 'txt';
   LibraryExt := '.lib';
   AssignOperator := '=';
   NativeFunctions := TStringList.Create;
   KeyWords := TStringList.Create;
   UpperCaseConstId := true;
   PreGenerationActivities := nil;
   ProgramHeaderSectionGenerator := nil;
   LibSectionGenerator := nil;
   UserDataTypesSectionGenerator := nil;
   ConstSectionGenerator := nil;
   UserFunctionsSectionGenerator := nil;
   MainProgramSectionGenerator := nil;
   GetUserFuncDesc := nil;
   GetUserTypeDesc := nil;
   GetMainProgramDesc := nil;
   SetHLighterAttrs := nil;
   GetLiteralType := nil;
   GetPointerTypeName := nil;
   IsPointerType := nil;
   Parse := nil;
   AreTypesCompatible := nil;
   GetOriginalType := nil;
   SkipFuncBodyGen := nil;
   LabelFontName := FLOWCHART_DEFAULT_FONT_NAME;
end;

destructor TLangDefinition.Destroy;
begin
   NativeFunctions.Free;
   KeyWords.Free;
   NativeDataTypes := nil;
   Parser.Free;
{$IFDEF USE_CODEFOLDING}
   FoldRegions := nil;
{$ENDIF}
   inherited;
end;

function TLangDefinition.ImportLangDef(const root: IXMLElement): TErrorType;
var
   tag, tag1: IXMLElement;
   lVal: string;
   i, a, lCount: integer;
begin

   result := errNone;

   lVal := '';
   tag := TXMLProcessor.FindChildTag(root, 'Name');
   if tag <> nil then
      lVal := Trim(tag.Text);
   if lVal = '' then
   begin
      GErr_Text := i18Manager.GetString('NameTagNotFound');
      result := errValidate;
      exit;
   end
   else
      Name := lVal;

   tag := TXMLProcessor.FindChildTag(root, 'CommentBegin');
   if tag <> nil then
      CommentBegin := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'CommentEnd');
   if tag <> nil then
      CommentEnd := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'InputFunction');
   if tag <> nil then
      InputFunction := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'OutputFunction');
   if tag <> nil then
      OutputFunction := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'AssignTemplate');
   if tag <> nil then
      AssignTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'InputTemplate');
   if tag <> nil then
      InputTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'OutputTemplate');
   if tag <> nil then
      OutputTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ProcedureLabelKey');
   if tag <> nil then
      ProcedureLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionLabelKey');
   if tag <> nil then
      FunctionLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ProgramLabelKey');
   if tag <> nil then
      ProgramLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'HighLighterVarName');
   if tag <> nil then
      HighLighterVarName := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FuncBracketsCursorPos');
   if tag <> nil then
      FuncBracketsCursorPos := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'FuncBrackets');
   if tag <> nil then
      FuncBrackets := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ExternEntry');
   if tag <> nil then
      ExternEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ConstIDSpecChars');
   if tag <> nil then
      ConstIDSpecChars := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarTemplate');
   if tag <> nil then
      VarTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderTypeNotNone1');
   if tag <> nil then
      FunctionHeaderTypeNotNone1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderTypeNone1');
   if tag <> nil then
      FunctionHeaderTypeNone1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderTypeNotNone2');
   if tag <> nil then
      FunctionHeaderTypeNotNone2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderTypeNone2');
   if tag <> nil then
      FunctionHeaderTypeNone2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionTemplate');
   if tag <> nil then
      FunctionTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderTemplate');
   if tag <> nil then
      FunctionHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'PointerTypeMask');
   if tag <> nil then
      PointerTypeMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'CaseOfTemplate');
   if tag <> nil then
      CaseOfTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'CaseOfValueTemplate');
   if tag <> nil then
      CaseOfValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'CaseOfDefaultValueTemplate');
   if tag <> nil then
      CaseOfDefaultValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'WhileTemplate');
   if tag <> nil then
      WhileTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'IfTemplate');
   if tag <> nil then
      IfTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'IfElseTemplate');
   if tag <> nil then
      IfElseTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ElseLabel');
   if tag <> nil then
      ElseLabel := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelFontName');
   if (tag <> nil) and (Screen.Fonts.IndexOf(tag.Text) <> -1) then
      LabelFontName := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelFontSize');
   if tag <> nil then
      LabelFontSize := StrToIntDef(tag.Text, 12);

   tag := TXMLProcessor.FindChildTag(root, 'LabelRepeat');
   if tag <> nil then
      LabelRepeat := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelWhile');
   if tag <> nil then
      LabelWhile := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelFor');
   if tag <> nil then
      LabelFor := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelCase');
   if tag <> nil then
      LabelCase := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelIf');
   if tag <> nil then
      LabelIf := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelIfElse');
   if tag <> nil then
      LabelIfElse := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelFuncCall');
   if tag <> nil then
      LabelFuncCall := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelReturn');
   if tag <> nil then
      LabelReturn := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelText');
   if tag <> nil then
      LabelText := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelFolder');
   if tag <> nil then
      LabelFolder := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelIn');
   if tag <> nil then
      LabelIn := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelOut');
   if tag <> nil then
      LabelOut := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelAssign');
   if tag <> nil then
      LabelAssign := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LabelMultiAssign');
   if tag <> nil then
      LabelMultiAssign := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ReturnDesc');
   if tag <> nil then
      ReturnDesc := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'RepeatDesc');
   if tag <> nil then
      RepeatDesc := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'RepeatUntilTemplate');
   if tag <> nil then
      RepeatUntilTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'RepeatUntilAsDoWhile');
   if tag <> nil then
      RepeatUntilAsDoWhile := CompareText(tag.Text, 'True') = 0;

   tag := TXMLProcessor.FindChildTag(root, 'MainProgramTemplate');
   if tag <> nil then
      MainProgramTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ProgramReturnTemplate');
   if tag <> nil then
      ProgramReturnTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ProgramTemplate');
   if tag <> nil then
      ProgramTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ForDoTemplate');
   if tag <> nil then
      ForDoTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ForAsc1');
   if tag <> nil then
      ForAsc1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ForAsc2');
   if tag <> nil then
      ForAsc2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ForDesc1');
   if tag <> nil then
      ForDesc1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ForDesc2');
   if tag <> nil then
      ForDesc2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'TextTemplate');
   if tag <> nil then
      TextTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FolderTemplate');
   if tag <> nil then
      FolderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionCallTemplate');
   if tag <> nil then
      FunctionCallTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypesTemplate');
   if tag <> nil then
      DataTypesTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionsTemplate');
   if tag <> nil then
      FunctionsTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeIntMask');
   if tag <> nil then
      DataTypeIntMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeRealMask');
   if tag <> nil then
      DataTypeRealMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeOtherMask');
   if tag <> nil then
      DataTypeOtherMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeRecordTemplate');
   if tag <> nil then
      DataTypeRecordTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeEnumTemplate');
   if tag <> nil then
      DataTypeEnumTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeEnumEntryList');
   if tag <> nil then
      DataTypeEnumEntryList := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeEnumEntryListStripCount');
   if tag <> nil then
      DataTypeEnumEntryListStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeArrayMask');
   if tag <> nil then
      DataTypeArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeRecordFieldMask');
   if tag <> nil then
      DataTypeRecordFieldMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DataTypeRecordFieldArrayMask');
   if tag <> nil then
      DataTypeRecordFieldArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ConstTemplate');
   if tag <> nil then
      ConstTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ConstEntry');
   if tag <> nil then
      ConstEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ConstEntryExtern');
   if tag <> nil then
      ConstEntryExtern := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntryInit');
   if tag <> nil then
      VarEntryInit := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntryInitExtern');
   if tag <> nil then
      VarEntryInitExtern := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntry');
   if tag <> nil then
      VarEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntryArray');
   if tag <> nil then
      VarEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntryArraySize');
   if tag <> nil then
      VarEntryArraySize := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'VarEntryArraySizeStripCount');
   if tag <> nil then
      VarEntryArraySizeStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'LibEntryListStripCount');
   if tag <> nil then
      LibEntryListStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'LibTemplate');
   if tag <> nil then
      LibTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LibEntry');
   if tag <> nil then
      LibEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LibEntryList');
   if tag <> nil then
      LibEntryList := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsEntryArray');
   if tag <> nil then
      FunctionHeaderArgsEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsEntryRef');
   if tag <> nil then
      FunctionHeaderArgsEntryRef := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsEntryRecord');
   if tag <> nil then
      FunctionHeaderArgsEntryRecord := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsEntryEnum');
   if tag <> nil then
      FunctionHeaderArgsEntryEnum := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsEntryMask');
   if tag <> nil then
      FunctionHeaderArgsEntryMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'FunctionHeaderArgsStripCount');
   if tag <> nil then
      FunctionHeaderArgsStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'ProgramHeaderTemplate');
   if tag <> nil then
      ProgramHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'ReturnTemplate');
   if tag <> nil then
      ReturnTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'DefaultExt');
   if tag <> nil then
      DefaultExt := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'LibraryExt');
   if tag <> nil then
      LibraryExt := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'AssignOperator');
   if tag <> nil then
      AssignOperator := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'InOutCursorPos');
   if tag <> nil then
      InOutCursorPos := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(root, 'UserTypeDesc');
   if tag <> nil then
      UserTypeDesc := tag.Text;

   tag := TXMLProcessor.FindChildTag(root, 'EnabledConsts');
   if tag <> nil then
      EnabledConsts := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledVars');
   if tag <> nil then
      EnabledVars := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledCompiler');
   if tag <> nil then
      EnabledCompiler := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledUserFunctionHeader');
   if tag <> nil then
      EnabledUserFunctionHeader := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledUserFunctionBody');
   if tag <> nil then
      EnabledUserFunctionBody := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledUserDataTypes');
   if tag <> nil then
      EnabledUserDataTypes := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledExplorer');
   if tag <> nil then
      EnabledExplorer := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledCodeGenerator');
   if tag <> nil then
      EnabledCodeGenerator := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'EnabledMainProgram');
   if tag <> nil then
      EnabledMainProgram := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'GenExternVarConst');
   if tag <> nil then
      GenExternVarConst := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'CaseSensitiveSyntax');
   if tag <> nil then
      CaseSensitiveSyntax := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'UpperCaseConstId');
   if tag <> nil then
      UpperCaseConstId := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'AllowEnumsInForLoop');
   if tag <> nil then
      AllowEnumsInForLoop := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'AllowUserFunctionOverload');
   if tag <> nil then
      AllowUserFunctionOverload := Odd(StrToIntDef(tag.Text, 0));

   tag := TXMLProcessor.FindChildTag(root, 'NativeDataTypes');
   if tag <> nil then
   begin
      a := 0;
      lCount := TXMLProcessor.CountChildTags(tag, 'DataType', true);
      SetLength(NativeDataTypes, lCount);
      tag := TXMLProcessor.FindChildTag(tag, 'DataType');
      while (tag <> nil) and (a < lCount) do
      begin
         lVal := Trim(tag.Text);
         if lVal <> '' then
         begin
            with NativeDataTypes[a] do
            begin
               Name := lVal;
               OrigType := @NativeDataTypes[a];
               lVal := tag.GetAttribute('kind');
               if lVal = 'int' then
                  Kind := tpInt
               else if lVal = 'real' then
                  Kind := tpReal
               else if lVal = 'bool' then
                  Kind := tpBool
               else if lVal = 'string' then
                  Kind := tpString
               else if lVal = 'ptr' then
               begin
                  Kind := tpPtr;
                  lVal := Trim(tag.GetAttribute('origtype'));
                  for i := 0 to a-1 do
                  begin
                     if AnsiSameText(lVal, NativeDataTypes[i].Name) then
                     begin
                        OrigType := @NativeDataTypes[i];
                        break;
                     end;
                  end;
               end
               else
                  Kind := tpOther;
            end;
            a := a + 1;
         end;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
   end;
   tag := TXMLProcessor.FindChildTag(root, 'KeyWords');
   if tag <> nil then
   begin
      KeyWords.Sorted := false;
      KeyWords.CaseSensitive := CaseSensitiveSyntax;
      tag := TXMLProcessor.FindChildTag(tag, 'KeyWord');
      while tag <> nil do
      begin
         KeyWords.Add(tag.Text);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      KeyWords.Sort;
   end;
   tag := TXMLProcessor.FindChildTag(root, 'NativeFunctions');
   if tag <> nil then
   begin
      NativeFunctions.Sorted := false;
      NativeFunctions.CaseSensitive := CaseSensitiveSyntax;
      tag := TXMLProcessor.FindChildTag(tag, 'Function');
      while tag <> nil do
      begin
         lVal := Trim(tag.Text);
         if lVal <> '' then
            NativeFunctions.Add(lVal);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      NativeFunctions.Sort;
   end;
{$IFDEF USE_CODEFOLDING}
   tag := TXMLProcessor.FindChildTag(root, 'FoldRegions');
   if tag <> nil then
   begin
      i := 0;
      lCount := TXMLProcessor.CountChildTags(tag, 'FoldRegion');
      SetLength(FoldRegions, lCount);
      tag := TXMLProcessor.FindChildTag(tag, 'FoldRegion');
      while (tag <> nil) and (i < lCount) do
      begin
         with FoldRegions[i] do
         begin
            tag1 := TXMLProcessor.FindChildTag(tag, 'Open');
            if tag1 <> nil then
               Open := tag1.GetAttribute('Keyword');
            tag1 := TXMLProcessor.FindChildTag(tag, 'Close');
               Close := tag1.GetAttribute('Keyword');
            AddClose := Odd(StrToIntDef(tag.GetAttribute('AddClose'), 0));
            NoSubFolds := Odd(StrToIntDef(tag.GetAttribute('NoSubFolds'), 1));
            WholeWords := Odd(StrToIntDef(tag.GetAttribute('WholeWords'), 1));
            if tag.GetAttribute('Type') = '0' then
               RegionType := rtChar
            else
               RegionType := rtKeyword;
         end;
         tag := TXMLProcessor.FindNextTag(tag);
         i := i + 1;
      end;
   end;
{$ENDIF}
end;

function TLangDefinition.GetTemplate(const AClass: TClass): string;
begin
   result := '';
   if AClass = TWhileDoBlock then
      result := WhileTemplate
   else if AClass = TRepeatUntilBlock then
      result := RepeatUntilTemplate
   else if AClass = TForDoBlock then
      result := ForDoTemplate
   else if AClass = TCaseBlock then
      result := CaseOfTemplate
   else if AClass = TIfBlock then
      result := IfTemplate
   else if AClass = TIfElseBlock then
      result := IfElseTemplate
   else if AClass = TMainBlock then
      result := ProgramTemplate
   else if AClass = TInputBlock then
      result := InputTemplate
   else if AClass = TOutputBlock then
      result := OutputTemplate
   else if (AClass = TAssignBlock) or (AClass = TMultiAssignBlock) then
      result := AssignTemplate
   else if AClass = TReturnBlock then
      result := ReturnTemplate
   else if AClass = TTextBlock then
      result := TextTemplate
   else if AClass = TFolderBlock then
      result := FolderTemplate
   else if AClass = TFunctionCallBlock then
      result := FunctionCallTemplate;
   if result = '' then
      result := PRIMARY_PLACEHOLDER;
end;

function TLangDefinition.GetTemplateExpr(const AClass: TClass): string;
var
   lTemplateLines: TStringList;
   i: integer;
begin
   result := '';
   lTemplateLines := TStringList.Create;
   try
      lTemplateLines.Text := GetTemplate(AClass);
      for i := 0 to lTemplateLines.Count-1 do
      begin
         if AnsiPos(PRIMARY_PLACEHOLDER, lTemplateLines[i]) <> 0 then
         begin
            result := lTemplateLines[i];
            break;
         end;
      end;
   finally
      lTemplateLines.Free;
   end;
end;

function TLangDefinition.GetArraySizes(const ASizeEdit: TSizeEdit): string;
var
   i: integer;
begin
   result := '';
   if ASizeEdit <> nil then
   begin
      for i := 1 to ASizeEdit.DimensionCount do
         result := result + Format(VarEntryArraySize, [ASizeEdit.GetDimension(i)]);
      result := AnsiLeftStr(result, Length(result)-VarEntryArraySizeStripCount);
   end;
end;

end.
