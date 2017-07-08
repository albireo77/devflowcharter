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
    System.Classes, System.SysUtils, SizeEdit, SynEditHighlighter, YaccLib, DeclareList, UserFunction,
    CommonTypes, OmniXML, UserDataType, System.Win.Registry;

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
   private
      FName,
      FCompilerRegKey,
      FCompilerNoMainRegKey,
      FCompilerFileEncodingKey: string;
   public
      CommentBegin, CommentEnd,
      DefaultExt,
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
      LabelInstr,
      LabelMultiInstr,
      LabelReturn,
      LabelCase,
      LabelText,
      LabelFolder,
      LabelMain,
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
      InstrTemplate,
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
      DefFile,
      CompilerCommand,
      CompilerCommandNoMain,
      CompilerFileEncoding,
      InstrEnd,
      ForDoVarString,
      RepeatUntilDescTemplate,
      ForDoDescTemplate,
      ReturnDescTemplate,
      CaseOfDescTemplate: string;
      DecimalSeparator: char;
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
      EnabledPointers,
      EnabledExplorer,
      EnabledCodeGenerator,
      EnabledMainProgram,
      CaseSensitiveSyntax,
      UpperCaseConstId,
      AllowEnumsInForLoop,
      AllowUserFunctionOverload,
      RepeatUntilAsDoWhile,
      GenExternVarConst,
      ForDoVarList,
      AllowUnboundedArrays: boolean;
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
      GetUserFuncDesc: function (AHeader: TUserFunctionHeader; ALongDesc: boolean = true): string;
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
      property Name: string read FName;
      constructor Create;
      destructor Destroy; override;
      function ImportFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
      function GetTemplate(const AClass: TClass): string;
      function GetTemplateExpr(const AClass: TClass): string;
      function GetArraySizes(const ASizeEdit: TSizeEdit): string;
      procedure WriteCompilerData(ARegistry: TRegistry);
      procedure ReadCompilerData(ARegistry: TRegistry);
      function GetFileEncoding: TEncoding;
   end;


implementation

uses
   Vcl.Forms, ApplicationCommon, XMLProcessor, WhileDo_Block, RepeatUntil_Block,
   ForDo_Block, Case_Block, If_Block, IfElse_Block, Main_Block, InOut_Block,
   Instr_Block, MultiInstr_Block, Return_Block, Text_Block, FunctionCall_Block,
   Folder_Block;

constructor TLangDefinition.Create;
begin
   inherited;
   FName := '   ';
   DefaultExt := 'txt';
   LibraryExt := '.lib';
   AssignOperator := '=';
   ForDoVarString := '=';
   NativeFunctions := TStringList.Create;
   KeyWords := TStringList.Create;
   UpperCaseConstId := true;
   EnabledPointers := true;
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
   CompilerFileEncoding := 'ANSI';
   DecimalSeparator := '.';
   LabelFontName := FLOWCHART_DEFAULT_FONT_NAME;
   LabelFontSize := LABEL_DEFAULT_FONT_SIZE;
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

function TLangDefinition.ImportFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
var
   tag: IXMLElement;
   lVal, lName, kinds: string;
   lKind: TDataTypeKind;
   lOrigType, lType: PNativeDataType;
   i, a, count: integer;
{$IFDEF USE_CODEFOLDING}
   tag1: IXMLElement;
{$ENDIF}
begin

   result := errNone;

   lVal := '';
   tag := TXMLProcessor.FindChildTag(ATag, 'Name');
   if tag <> nil then
      lVal := tag.Text.Trim;
   if lVal.IsEmpty then
   begin
      GErr_Text := i18Manager.GetString('NameTagNotFound');
      result := errValidate;
      exit;
   end
   else
      FName := lVal;

   FCompilerRegKey := 'CompilerPath_' + FName;
   FCompilerNoMainRegKey := 'CompilerPathNoMain_' + FName;
   FCompilerFileEncodingKey := 'CompilerFileEncoding_' + FName;

   tag := TXMLProcessor.FindChildTag(ATag, 'CommentBegin');
   if tag <> nil then
      CommentBegin := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CommentEnd');
   if tag <> nil then
      CommentEnd := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'InputFunction');
   if tag <> nil then
      InputFunction := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'OutputFunction');
   if tag <> nil then
      OutputFunction := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'InstrTemplate');
   if tag <> nil then
      InstrTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'InputTemplate');
   if tag <> nil then
      InputTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'OutputTemplate');
   if tag <> nil then
      OutputTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProcedureLabelKey');
   if tag <> nil then
      ProcedureLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionLabelKey');
   if tag <> nil then
      FunctionLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramLabelKey');
   if tag <> nil then
      ProgramLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'HighLighterVarName');
   if tag <> nil then
      HighLighterVarName := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FuncBracketsCursorPos');
   if tag <> nil then
      FuncBracketsCursorPos := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'FuncBrackets');
   if tag <> nil then
      FuncBrackets := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DecimalSeparator');
   if (tag <> nil) and not tag.Text.IsEmpty then
      DecimalSeparator := tag.Text[1];

   tag := TXMLProcessor.FindChildTag(ATag, 'ExternEntry');
   if tag <> nil then
      ExternEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoVarString');
   if tag <> nil then
      ForDoVarString := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstIDSpecChars');
   if tag <> nil then
      ConstIDSpecChars := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'InstrEnd');
   if tag <> nil then
      InstrEnd := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarTemplate');
   if tag <> nil then
      VarTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeNotNone1');
   if tag <> nil then
      FunctionHeaderTypeNotNone1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeNone1');
   if tag <> nil then
      FunctionHeaderTypeNone1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeNotNone2');
   if tag <> nil then
      FunctionHeaderTypeNotNone2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeNone2');
   if tag <> nil then
      FunctionHeaderTypeNone2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionTemplate');
   if tag <> nil then
      FunctionTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTemplate');
   if tag <> nil then
      FunctionHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'PointerTypeMask');
   if tag <> nil then
      PointerTypeMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfTemplate');
   if tag <> nil then
      CaseOfTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfValueTemplate');
   if tag <> nil then
      CaseOfValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfDefaultValueTemplate');
   if tag <> nil then
      CaseOfDefaultValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'WhileTemplate');
   if tag <> nil then
      WhileTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'IfTemplate');
   if tag <> nil then
      IfTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'IfElseTemplate');
   if tag <> nil then
      IfElseTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ElseLabel');
   if tag <> nil then
      ElseLabel := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFontName');
   if (tag <> nil) and (Screen.Fonts.IndexOf(tag.Text) <> -1) then
      LabelFontName := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFontSize');
   if tag <> nil then
      LabelFontSize := StrToIntDef(tag.Text, LABEL_DEFAULT_FONT_SIZE);

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelRepeat');
   if tag <> nil then
      LabelRepeat := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelWhile');
   if tag <> nil then
      LabelWhile := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFor');
   if tag <> nil then
      LabelFor := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelCase');
   if tag <> nil then
      LabelCase := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelIf');
   if tag <> nil then
      LabelIf := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelIfElse');
   if tag <> nil then
      LabelIfElse := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFuncCall');
   if tag <> nil then
      LabelFuncCall := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelReturn');
   if tag <> nil then
      LabelReturn := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelText');
   if tag <> nil then
      LabelText := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFolder');
   if tag <> nil then
      LabelFolder := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelIn');
   if tag <> nil then
      LabelIn := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelOut');
   if tag <> nil then
      LabelOut := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelInstr');
   if tag <> nil then
      LabelInstr := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelMultiInstr');
   if tag <> nil then
      LabelMultiInstr := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'RepeatUntilTemplate');
   if tag <> nil then
      RepeatUntilTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'RepeatUntilDescTemplate');
   if tag <> nil then
      RepeatUntilDescTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ReturnDescTemplate');
   if tag <> nil then
      ReturnDescTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoDescTemplate');
   if tag <> nil then
      ForDoDescTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfDescTemplate');
   if tag <> nil then
      CaseOfDescTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'MainProgramTemplate');
   if tag <> nil then
      MainProgramTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramReturnTemplate');
   if tag <> nil then
      ProgramReturnTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramTemplate');
   if tag <> nil then
      ProgramTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoTemplate');
   if tag <> nil then
      ForDoTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForAsc1');
   if tag <> nil then
      ForAsc1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForAsc2');
   if tag <> nil then
      ForAsc2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDesc1');
   if tag <> nil then
      ForDesc1 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDesc2');
   if tag <> nil then
      ForDesc2 := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'TextTemplate');
   if tag <> nil then
      TextTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FolderTemplate');
   if tag <> nil then
      FolderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionCallTemplate');
   if tag <> nil then
      FunctionCallTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypesTemplate');
   if tag <> nil then
      DataTypesTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionsTemplate');
   if tag <> nil then
      FunctionsTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeIntMask');
   if tag <> nil then
      DataTypeIntMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRealMask');
   if tag <> nil then
      DataTypeRealMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeOtherMask');
   if tag <> nil then
      DataTypeOtherMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRecordTemplate');
   if tag <> nil then
      DataTypeRecordTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeEnumTemplate');
   if tag <> nil then
      DataTypeEnumTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeEnumEntryList');
   if tag <> nil then
      DataTypeEnumEntryList := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeEnumEntryListStripCount');
   if tag <> nil then
      DataTypeEnumEntryListStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeArrayMask');
   if tag <> nil then
      DataTypeArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRecordFieldMask');
   if tag <> nil then
      DataTypeRecordFieldMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRecordFieldArrayMask');
   if tag <> nil then
      DataTypeRecordFieldArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstTemplate');
   if tag <> nil then
      ConstTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstEntry');
   if tag <> nil then
      ConstEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstEntryExtern');
   if tag <> nil then
      ConstEntryExtern := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntryInit');
   if tag <> nil then
      VarEntryInit := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntryInitExtern');
   if tag <> nil then
      VarEntryInitExtern := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntry');
   if tag <> nil then
      VarEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntryArray');
   if tag <> nil then
      VarEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntryArraySize');
   if tag <> nil then
      VarEntryArraySize := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarEntryArraySizeStripCount');
   if tag <> nil then
      VarEntryArraySizeStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'LibEntryListStripCount');
   if tag <> nil then
      LibEntryListStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'LibTemplate');
   if tag <> nil then
      LibTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LibEntry');
   if tag <> nil then
      LibEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LibEntryList');
   if tag <> nil then
      LibEntryList := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryArray');
   if tag <> nil then
      FunctionHeaderArgsEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryRef');
   if tag <> nil then
      FunctionHeaderArgsEntryRef := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryRecord');
   if tag <> nil then
      FunctionHeaderArgsEntryRecord := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryEnum');
   if tag <> nil then
      FunctionHeaderArgsEntryEnum := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryMask');
   if tag <> nil then
      FunctionHeaderArgsEntryMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsStripCount');
   if tag <> nil then
      FunctionHeaderArgsStripCount := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramHeaderTemplate');
   if tag <> nil then
      ProgramHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ReturnTemplate');
   if tag <> nil then
      ReturnTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DefaultExt');
   if tag <> nil then
      DefaultExt := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LibraryExt');
   if tag <> nil then
      LibraryExt := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'AssignOperator');
   if tag <> nil then
      AssignOperator := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'InOutCursorPos');
   if tag <> nil then
      InOutCursorPos := StrToIntDef(tag.Text, 0);

   tag := TXMLProcessor.FindChildTag(ATag, 'UserTypeDesc');
   if tag <> nil then
      UserTypeDesc := tag.Text;

   ForDoVarList              := TXMLProcessor.GetBoolFromChildTag(ATag, 'ForDoVarList', ForDoVarList);
   EnabledPointers           := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledPointers', EnabledPointers);
   RepeatUntilAsDoWhile      := TXMLProcessor.GetBoolFromChildTag(ATag, 'RepeatUntilAsDoWhile', RepeatUntilAsDoWhile);
   EnabledConsts             := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledConsts', EnabledConsts);
   EnabledVars               := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledVars', EnabledVars);
   EnabledCompiler           := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledCompiler', EnabledCompiler);
   EnabledUserFunctionHeader := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledUserFunctionHeader', EnabledUserFunctionHeader);
   EnabledUserFunctionBody   := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledUserFunctionBody', EnabledUserFunctionBody);
   EnabledUserDataTypes      := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledUserDataTypes', EnabledUserDataTypes);
   EnabledExplorer           := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledExplorer', EnabledExplorer);
   EnabledCodeGenerator      := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledCodeGenerator', EnabledCodeGenerator);
   EnabledMainProgram        := TXMLProcessor.GetBoolFromChildTag(ATag, 'EnabledMainProgram', EnabledMainProgram);
   GenExternVarConst         := TXMLProcessor.GetBoolFromChildTag(ATag, 'GenExternVarConst', GenExternVarConst);
   CaseSensitiveSyntax       := TXMLProcessor.GetBoolFromChildTag(ATag, 'CaseSensitiveSyntax', CaseSensitiveSyntax);
   UpperCaseConstId          := TXMLProcessor.GetBoolFromChildTag(ATag, 'UpperCaseConstId', UpperCaseConstId);
   AllowEnumsInForLoop       := TXMLProcessor.GetBoolFromChildTag(ATag, 'AllowEnumsInForLoop', AllowEnumsInForLoop);
   AllowUserFunctionOverload := TXMLProcessor.GetBoolFromChildTag(ATag, 'AllowUserFunctionOverload', AllowUserFunctionOverload);
   AllowUnboundedArrays      := TXMLProcessor.GetBoolFromChildTag(ATag, 'AllowUnboundedArrays', AllowUnboundedArrays);

   tag := TXMLProcessor.FindChildTag(ATag, 'NativeDataTypes');
   if tag <> nil then
   begin
      a := 0;
      tag := TXMLProcessor.FindChildTag(tag, 'DataType');
      while tag <> nil do
      begin
         lOrigType := nil;
         lName := tag.Text.Trim;
         if not lName.IsEmpty then
         begin
            kinds := tag.GetAttribute('kind');
            if kinds = 'int' then
               lKind := tpInt
            else if kinds = 'real' then
               lKind := tpReal
            else if kinds = 'bool' then
               lKind := tpBool
            else if kinds = 'string' then
               lKind := tpString
            else if kinds = 'ptr' then
            begin
               if EnabledPointers then
               begin
                  lKind := tpPtr;
                  lVal := tag.GetAttribute('origtype').Trim;
                  for i := 0 to a-1 do
                  begin
                     if SameText(lVal, NativeDataTypes[i].Name) then
                     begin
                        lOrigType := @NativeDataTypes[i];
                        break;
                     end;
                  end;
               end
               else
                  lName := '';
            end
            else
               lKind := tpOther;
         end;
         if not lName.IsEmpty then
         begin
            a := a + 1;
            SetLength(NativeDataTypes, a);
            lType := @NativeDataTypes[a-1];
            lType.Name := lName;
            lType.Kind := lKind;
            if lOrigType = nil then
               lOrigType := @lType;
            lType.OrigType := lOrigType;
         end;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
   end;
   tag := TXMLProcessor.FindChildTag(ATag, 'KeyWords');
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
   tag := TXMLProcessor.FindChildTag(ATag, 'NativeFunctions');
   if tag <> nil then
   begin
      NativeFunctions.Sorted := false;
      NativeFunctions.CaseSensitive := CaseSensitiveSyntax;
      tag := TXMLProcessor.FindChildTag(tag, 'Function');
      while tag <> nil do
      begin
         lVal := tag.Text.Trim;
         if not lVal.IsEmpty then
            NativeFunctions.Add(lVal);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      NativeFunctions.Sort;
   end;
{$IFDEF USE_CODEFOLDING}
   tag := TXMLProcessor.FindChildTag(ATag, 'FoldRegions');
   if tag <> nil then
   begin
      i := 0;
      count := TXMLProcessor.CountChildTags(tag, 'FoldRegion');
      SetLength(FoldRegions, count);
      tag := TXMLProcessor.FindChildTag(tag, 'FoldRegion');
      while (tag <> nil) and (i < count) do
      begin
         with FoldRegions[i] do
         begin
            tag1 := TXMLProcessor.FindChildTag(tag, 'Open');
            if tag1 <> nil then
               Open := tag1.GetAttribute('Keyword');
            tag1 := TXMLProcessor.FindChildTag(tag, 'Close');
               Close := tag1.GetAttribute('Keyword');
            AddClose := TXMLProcessor.GetBoolFromAttr(tag, 'AddClose', false);
            NoSubFolds := TXMLProcessor.GetBoolFromAttr(tag, 'NoSubFolds', true);
            WholeWords := TXMLProcessor.GetBoolFromAttr(tag, 'WholeWords', true);
            if tag.GetAttribute('Type') = 'rtChar' then
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

procedure TLangDefinition.WriteCompilerData(ARegistry: TRegistry);
begin
   ARegistry.WriteString(FCompilerRegKey, CompilerCommand);
   ARegistry.WriteString(FCompilerNoMainRegKey, CompilerCommandNoMain);
   ARegistry.WriteString(FCompilerFileEncodingKey, CompilerFileEncoding);
end;

procedure TLangDefinition.ReadCompilerData(ARegistry: TRegistry);
begin
   if ARegistry.ValueExists(FCompilerRegKey) then
      CompilerCommand := ARegistry.ReadString(FCompilerRegKey);
   if ARegistry.ValueExists(FCompilerNoMainRegKey) then
      CompilerCommandNoMain := ARegistry.ReadString(FCompilerNoMainRegKey);
   if ARegistry.ValueExists(FCompilerFileEncodingKey) then
      CompilerFileEncoding := ARegistry.ReadString(FCompilerFileEncodingKey);
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
   else if (AClass = TInstrBlock) or (AClass = TMultiInstrBlock) then
      result := InstrTemplate
   else if AClass = TReturnBlock then
      result := ReturnTemplate
   else if AClass = TTextBlock then
      result := TextTemplate
   else if AClass = TFolderBlock then
      result := FolderTemplate
   else if AClass = TFunctionCallBlock then
      result := FunctionCallTemplate;
end;

function TLangDefinition.GetTemplateExpr(const AClass: TClass): string;
var
   templateLines: TStringList;
   i: integer;
   template: string;
begin
   result := '';
   template := GetTemplate(AClass);
   if template.IsEmpty then
      template := PRIMARY_PLACEHOLDER;
   templateLines := TStringList.Create;
   try
      templateLines.Text := template;
      for i := 0 to templateLines.Count-1 do
      begin
         if Pos(PRIMARY_PLACEHOLDER, templateLines[i]) <> 0 then
         begin
            result := templateLines[i];
            break;
         end;
      end;
   finally
      templateLines.Free;
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
      if (VarEntryArraySizeStripCount > 0) and not result.IsEmpty then
         SetLength(result, result.Length - VarEntryArraySizeStripCount);
   end;
end;

function TLangDefinition.GetFileEncoding: TEncoding;
begin
   if CompilerFileEncoding = 'ASCII' then
      result := TEncoding.ASCII
   else if CompilerFileEncoding = 'UTF-7' then
      result := TEncoding.UTF7
   else if CompilerFileEncoding = 'UTF-8' then
      result := TEncoding.UTF8
   else if CompilerFileEncoding = 'Unicode' then
      result := TEncoding.Unicode
   else if CompilerFileEncoding = 'BE Unicode' then
      result := TEncoding.BigEndianUnicode
   else
      result := TEncoding.ANSI;
end;

end.
