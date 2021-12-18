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
    Types, OmniXML, UserDataType;

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
      FCompilerKey,
      FCompilerNoMainKey,
      FCompilerFileEncodingKey,
      FVarTemplate,
      FConstTemplate,
      FDataTypesTemplate,
      FFunctionsTemplate,
      FFileContentsTemplate,
      FLibTemplate,
      FProgramHeaderTemplate: string;
      function GetVarTemplate: string;
      function GetConstTemplate: string;
      function GetDataTypesTemplate: string;
      function GetFunctionsTemplate: string;
      function GetFileContentsTemplate: string;
      function GetLibTemplate: string;
      function GetProgramHeaderTemplate: string;
      procedure InitBlockTemplates;
      procedure ImportBlockTemplates(ATag: IXMLElement);
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
      ConstEntry,
      ConstEntryArray,
      ConstTypeGeneric,
      ConstTypeNotGeneric,
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
      ForDoAsc1,
      ForDoAsc2,
      ForDoDesc1,
      ForDoDesc2,
      CaseOfFirstValueTemplate,
      CaseOfValueTemplate,
      CaseOfDefaultValueTemplate,
      PointerTypeMask,
      MainFunctionTemplate,
      ProgramReturnTemplate,
      FunctionTemplate,
      FunctionHeaderTemplate,
      FunctionHeaderDescTemplate,
      ConstructorHeaderTemplate,
      FunctionHeaderTypeNone1,
      FunctionHeaderTypeNotNone1,
      FunctionHeaderTypeNone2,
      FunctionHeaderTypeNotNone2,
      FunctionHeaderDescParmMask,
      FunctionHeaderDescReturnMask,
      FunctionHeaderArgsEntryMask,
      FunctionHeaderArgsEntryRef,
      FunctionHeaderArgsEntryArray,
      FunctionHeaderArgsEntryDefault,
      FunctionHeaderArgsEntryRecord,
      FunctionHeaderArgsEntryEnum,
      UserTypeDesc,
      LibEntry,
      LibEntryList,
      DataTypeExternal,
      DataTypeTransExternal,
      DataTypeNotExternal,
      DataTypeIntMask,
      DataTypeRealMask,
      DataTypeOtherMask,
      DataTypeArrayMask,
      DataTypeUnboundedArrayMask,
      DataTypeRecordTemplate,
      DataTypeRecordFieldMask,
      DataTypeRecordFieldArrayMask,
      DataTypeEnumTemplate,
      DataTypeEnumEntryList,
      ProcedureLabelKey,
      FunctionLabelKey,
      ProgramLabelKey,
      ConstructorLabelKey,
      GlobalVarsLabelKey,
      GlobalConstsLabelKey,
      HighLighterVarName,
      FuncBrackets,
      VarExtern,
      VarTransExtern,
      VarNotExtern,
      ConstExtern,
      ConstTransExtern,
      ConstNotExtern,
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
      CaseOfDescTemplate,
      ExternalLabel,
      StaticLabel,
      RecordLabel,
      FunctionHeaderExternal,
      FunctionHeaderTransExternal,
      FunctionHeaderNotExternal,
      FunctionHeaderStatic,
      FunctionHeaderNotStatic,
      FunctionHeaderTypeArray,
      FunctionHeaderTypeNotArray: string;
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
      NativeFunctions: array of TNativeFunction;
      KeyWords: TStringList;
      EnabledConsts,
      EnabledVars,
      EnabledCompiler,
      EnabledUserFunctionHeader,
      EnabledUserFunctionBody,
      EnabledUserDataTypes,
      EnabledUserDataTypeInt,
      EnabledUserDataTypeReal,
      EnabledUserDataTypeOther,
      EnabledUserDataTypeEnum,
      EnabledUserDataTypeArray,
      EnabledPointers,
      EnabledExplorer,
      EnabledCodeGenerator,
      EnabledMainProgram,
      CaseSensitiveSyntax,
      UpperCaseConstId,
      AllowEnumsInForLoop,
      AllowUserFunctionOverload,
      RepeatUntilAsDoWhile,
      ForDoVarList,
      CodeIncludeExternVarConst,
      CodeIncludeExternDataType,
      CodeIncludeExternFunction,
      AllowUnboundedArrays,
      AllowDuplicatedLibs,
      AllowTransExternVarConst,
      AllowTransExternFunction,
      AllowTransExternDataType: boolean;
      InOutCursorPos,
      FuncBracketsCursorPos: integer;
      BlockTemplates: array[TBlockType] of string;
      ExecuteBeforeGeneration: procedure;
      ExecuteAfterGeneration: procedure;
      ProgramHeaderSectionGenerator: procedure (ALines: TStringList);
      LibSectionGenerator: procedure (ALines: TStringList);
      UserDataTypesSectionGenerator: procedure (ALines: TStringList);
      VarSectionGenerator: procedure (ALines: TStringList; AVarList: TVarDeclareList);
      ConstSectionGenerator: procedure (ALines: TStringList; AConstList: TConstDeclareList);
      UserFunctionsSectionGenerator: procedure (ALines: TStringList; ASkipBodyGenerate: boolean);
      MainFunctionSectionGenerator: procedure (ALines: TStringList; ADeep: integer);
      FileContentsGenerator: function (ALines: TStringList; ASkipBodyGenerate: boolean): boolean;
      GetUserFuncDesc: function (AHeader: TUserFunctionHeader; AFullParams: boolean = true; AIncludeDesc: boolean = true): string;
      GetUserFuncHeaderDesc: function (AHeader: TUserFunctionHeader): string;
      GetUserTypeDesc: function (ADataType: TUserDataType): string;
      SetHLighterAttrs: procedure;
      GetPointerTypeName: function (const val: string): string;
      GetConstantType: function (const val: string; var AGenericType: string): integer;
      IsPointerType: function (const AName: string): boolean;
      GetOriginalType: function (const APtrType: string): string;
      AreTypesCompatible: function (AType1, AType2: integer): boolean;
      Parse: function (const AText: string; AParserMode: TYYMode): boolean;
      SkipFuncBodyGen: function: boolean;
      GetMainProgramDesc: function: string;
      property Name: string read FName;
      constructor Create;
      destructor Destroy; override;
      function ImportFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
      function GetBlockTemplate(ABlockType: TBlockType): string;
      function GetArraySizes(ASizeEdit: TSizeEdit): string;
      procedure SaveCompilerData;
      procedure LoadCompilerData;
      function GetFileEncoding: TEncoding;
      property VarTemplate: string read GetVarTemplate;
      property ConstTemplate: string read GetConstTemplate;
      property DataTypesTemplate: string read GetDataTypesTemplate;
      property FunctionsTemplate: string read GetFunctionsTemplate;
      property FileContentsTemplate: string read GetFileContentsTemplate;
      property LibTemplate: string read GetLibTemplate;
      property ProgramHeaderTemplate: string read GetProgramHeaderTemplate;
   end;

const
   BLOCK_TO_TEMPLATE_TAG_MAP: array[TBlockType] of string = ('', 'InstrTemplate', 'InstrTemplate', 'InputTemplate', 'OutputTemplate',
                                                             'FunctionCallTemplate', 'WhileTemplate', 'RepeatUntilTemplate', 'IfTemplate',
                                                             'IfElseTemplate', 'ForDoTemplate', 'CaseOfTemplate', 'FunctionBodyTemplate',
                                                             '', 'ReturnTemplate', 'TextTemplate', 'FolderTemplate');


implementation

uses
   Vcl.Forms, System.StrUtils, System.IniFiles, XMLProcessor, Constants, Infrastructure;

constructor TLangDefinition.Create;
begin
   inherited;
   FName := '   ';
   DefaultExt := 'txt';
   LibraryExt := '.lib';
   AssignOperator := '=';
   ForDoVarString := '=';
   KeyWords := TStringList.Create;
   UpperCaseConstId := true;
   EnabledPointers := true;
   ExecuteBeforeGeneration := nil;
   ExecuteAfterGeneration := nil;
   ProgramHeaderSectionGenerator := nil;
   LibSectionGenerator := nil;
   UserDataTypesSectionGenerator := nil;
   ConstSectionGenerator := nil;
   UserFunctionsSectionGenerator := nil;
   MainFunctionSectionGenerator := nil;
   FileContentsGenerator := nil;
   GetUserFuncDesc := nil;
   GetUserFuncHeaderDesc := nil;
   GetUserTypeDesc := nil;
   GetMainProgramDesc := nil;
   SetHLighterAttrs := nil;
   GetConstantType := nil;
   GetPointerTypeName := nil;
   IsPointerType := nil;
   Parse := nil;
   AreTypesCompatible := nil;
   GetOriginalType := nil;
   SkipFuncBodyGen := nil;
   CompilerFileEncoding := 'ANSI';
   DecimalSeparator := '.';
   GlobalVarsLabelKey := 'GlobalVars';
   GlobalConstsLabelKey := 'GlobalConsts';
   EnabledUserDataTypeInt := true;
   EnabledUserDataTypeReal := true;
   EnabledUserDataTypeOther := true;
   EnabledUserDataTypeEnum := true;
   EnabledUserDataTypeArray := true;
   LabelFontName := FLOWCHART_DEFAULT_FONT_NAME;
   LabelFontSize := LABEL_DEFAULT_FONT_SIZE;
   ElseLabel := i18Manager.GetString('ElseLabel');
   InitBlockTemplates;
end;

destructor TLangDefinition.Destroy;
begin
   KeyWords.Free;
   KeyWords := nil;
   NativeDataTypes := nil;
   NativeFunctions := nil;
   Parser.Free;
   Parser := nil;
   if not FName.Trim.IsEmpty then
      SaveCompilerData;
{$IFDEF USE_CODEFOLDING}
   FoldRegions := nil;
{$ENDIF}
   inherited;
end;

function TLangDefinition.ImportFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   tag: IXMLElement;
   val, lName, kinds: string;
   lKind: TDataTypeKind;
   lOrigType, lType: PNativeDataType;
   i, a, count: integer;
   l3Strings: T3Strings;
begin
   result := errNone;
   val := TXMLProcessor.GetTextFromChild(ATag, 'Name').Trim;
   if val.IsEmpty then
   begin
      GErr_Text := i18Manager.GetString('NameTagNotFound');
      Exit(errValidate);
   end
   else
      FName := val;

   FCompilerKey := 'CompilerPath_' + FName;
   FCompilerNoMainKey := 'CompilerPathNoMain_' + FName;
   FCompilerFileEncodingKey := 'CompilerFileEncoding_' + FName;

   ImportBlockTemplates(ATag);

   tag := TXMLProcessor.FindChildTag(ATag, 'DecimalSeparator');
   if (tag <> nil) and not tag.Text.IsEmpty then
      DecimalSeparator := tag.Text[1];

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeModifier1');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeNone1 := l3Strings.S0;
      FunctionHeaderTypeNotNone1 := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeModifier2');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeNone2 := l3Strings.S0;
      FunctionHeaderTypeNotNone2 := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderExternalModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      FunctionHeaderExternal := l3Strings.S0;
      FunctionHeaderNotExternal := l3Strings.S1;
      FunctionHeaderTransExternal := l3Strings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderStaticModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      FunctionHeaderStatic := l3Strings.S0;
      FunctionHeaderNotStatic := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeArrayModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeArray := l3Strings.S0;
      FunctionHeaderTypeNotArray := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarExternModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      VarExtern := l3Strings.S0;
      VarNotExtern := l3Strings.S1;
      VarTransExtern := l3Strings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstExternModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      ConstExtern := l3Strings.S0;
      ConstNotExtern := l3Strings.S1;
      ConstTransExtern := l3Strings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoTemplateModifier1');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      ForDoAsc1 := l3Strings.S0;
      ForDoDesc1 := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoTemplateModifier2');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      ForDoAsc2 := l3Strings.S0;
      ForDoDesc2 := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeExternalModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      DataTypeExternal := l3Strings.S0;
      DataTypeNotExternal := l3Strings.S1;
      DataTypeTransExternal := l3Strings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstTypeModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      ConstTypeNotGeneric := l3Strings.S0;
      ConstTypeGeneric := l3Strings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFontName');
   if (tag <> nil) and (Screen.Fonts.IndexOf(tag.Text) <> -1) then
      LabelFontName := tag.Text;

   with TXMLProcessor do
   begin
      CommentBegin                   := GetTextFromChild(ATag, 'CommentBegin');
      CommentEnd                     := GetTextFromChild(ATag, 'CommentEnd');
      InputFunction                  := GetTextFromChild(ATag, 'InputFunction');
      OutputFunction                 := GetTextFromChild(ATag, 'OutputFunction');
      ProcedureLabelKey              := GetTextFromChild(ATag, 'ProcedureLabelKey');
      FunctionLabelKey               := GetTextFromChild(ATag, 'FunctionLabelKey');
      ConstructorLabelKey            := GetTextFromChild(ATag, 'ConstructorLabelKey');
      ProgramLabelKey                := GetTextFromChild(ATag, 'ProgramLabelKey');
      GlobalVarsLabelKey             := GetTextFromChild(ATag, 'GlobalVarsLabelKey', GlobalVarsLabelKey);
      GlobalConstsLabelKey           := GetTextFromChild(ATag, 'GlobalConstsLabelKey', GlobalConstsLabelKey);
      HighLighterVarName             := GetTextFromChild(ATag, 'HighLighterVarName');
      ForDoVarString                 := GetTextFromChild(ATag, 'ForDoVarString', ForDoVarString);
      ConstIDSpecChars               := GetTextFromChild(ATag, 'ConstIDSpecChars');
      FuncBrackets                   := GetTextFromChild(ATag, 'FuncBrackets');
      InstrEnd                       := GetTextFromChild(ATag, 'InstrEnd');
      FVarTemplate                   := GetTextFromChild(ATag, 'VarTemplate');
      FunctionTemplate               := GetTextFromChild(ATag, 'FunctionTemplate');
      FunctionHeaderTemplate         := GetTextFromChild(ATag, 'FunctionHeaderTemplate');
      FunctionHeaderDescTemplate     := GetTextFromChild(ATag, 'FunctionHeaderDescTemplate');
      FunctionHeaderDescParmMask     := GetTextFromChild(ATag, 'FunctionHeaderDescParmMask');
      FunctionHeaderDescReturnMask   := GetTextFromChild(ATag, 'FunctionHeaderDescReturnMask');
      ConstructorHeaderTemplate      := GetTextFromChild(ATag, 'ConstructorHeaderTemplate');
      PointerTypeMask                := GetTextFromChild(ATag, 'PointerTypeMask');
      CaseOfValueTemplate            := GetTextFromChild(ATag, 'CaseOfValueTemplate');
      CaseOfFirstValueTemplate       := GetTextFromChild(ATag, 'CaseOfFirstValueTemplate');
      CaseOfDefaultValueTemplate     := GetTextFromChild(ATag, 'CaseOfDefaultValueTemplate');
      ElseLabel                      := GetTextFromChild(ATag, 'ElseLabel', ElseLabel);
      LabelRepeat                    := GetTextFromChild(ATag, 'LabelRepeat');
      LabelWhile                     := GetTextFromChild(ATag, 'LabelWhile');
      LabelFor                       := GetTextFromChild(ATag, 'LabelFor');
      LabelCase                      := GetTextFromChild(ATag, 'LabelCase');
      LabelIf                        := GetTextFromChild(ATag, 'LabelIf');
      LabelIfElse                    := GetTextFromChild(ATag, 'LabelIfElse');
      LabelFuncCall                  := GetTextFromChild(ATag, 'LabelFuncCall');
      LabelReturn                    := GetTextFromChild(ATag, 'LabelReturn');
      LabelText                      := GetTextFromChild(ATag, 'LabelText');
      LabelFolder                    := GetTextFromChild(ATag, 'LabelFolder');
      LabelIn                        := GetTextFromChild(ATag, 'LabelIn');
      LabelOut                       := GetTextFromChild(ATag, 'LabelOut');
      LabelInstr                     := GetTextFromChild(ATag, 'LabelInstr');
      LabelMultiInstr                := GetTextFromChild(ATag, 'LabelMultiInstr');
      RepeatUntilDescTemplate        := GetTextFromChild(ATag, 'RepeatUntilDescTemplate');
      ReturnDescTemplate             := GetTextFromChild(ATag, 'ReturnDescTemplate');
      ForDoDescTemplate              := GetTextFromChild(ATag, 'ForDoDescTemplate');
      CaseOfDescTemplate             := GetTextFromChild(ATag, 'CaseOfDescTemplate');
      MainFunctionTemplate           := GetTextFromChild(ATag, 'MainFunctionTemplate');
      ProgramReturnTemplate          := GetTextFromChild(ATag, 'ProgramReturnTemplate');
      FDataTypesTemplate             := GetTextFromChild(ATag, 'DataTypesTemplate');
      FFunctionsTemplate             := GetTextFromChild(ATag, 'FunctionsTemplate');
      FFileContentsTemplate          := GetTextFromChild(ATag, FILE_CONTENTS_TAG);
      DataTypeIntMask                := GetTextFromChild(ATag, 'DataTypeIntMask');
      DataTypeRealMask               := GetTextFromChild(ATag, 'DataTypeRealMask');
      DataTypeOtherMask              := GetTextFromChild(ATag, 'DataTypeOtherMask');
      DataTypeRecordTemplate         := GetTextFromChild(ATag, 'DataTypeRecordTemplate');
      DataTypeEnumTemplate           := GetTextFromChild(ATag, 'DataTypeEnumTemplate');
      DataTypeEnumEntryList          := GetTextFromChild(ATag, 'DataTypeEnumEntryList');
      DataTypeArrayMask              := GetTextFromChild(ATag, 'DataTypeArrayMask');
      DataTypeUnboundedArrayMask     := GetTextFromChild(ATag, 'DataTypeUnboundedArrayMask');
      DataTypeRecordFieldMask        := GetTextFromChild(ATag, 'DataTypeRecordFieldMask');
      DataTypeRecordFieldArrayMask   := GetTextFromChild(ATag, 'DataTypeRecordFieldArrayMask');
      FConstTemplate                 := GetTextFromChild(ATag, 'ConstTemplate');
      ConstEntry                     := GetTextFromChild(ATag, 'ConstEntry');
      ConstEntryArray                := GetTextFromChild(ATag, 'ConstEntryArray');
      VarEntryInit                   := GetTextFromChild(ATag, 'VarEntryInit');
      VarEntryInitExtern             := GetTextFromChild(ATag, 'VarEntryInitExtern');
      VarEntry                       := GetTextFromChild(ATag, 'VarEntry');
      VarEntryArray                  := GetTextFromChild(ATag, 'VarEntryArray');
      VarEntryArraySize              := GetTextFromChild(ATag, 'VarEntryArraySize');
      FLibTemplate                   := GetTextFromChild(ATag, 'LibTemplate');
      LibEntry                       := GetTextFromChild(ATag, 'LibEntry');
      LibEntryList                   := GetTextFromChild(ATag, 'LibEntryList');
      FunctionHeaderArgsEntryArray   := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryArray');
      FunctionHeaderArgsEntryDefault := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryDefault');
      FunctionHeaderArgsEntryRef     := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryRef');
      FunctionHeaderArgsEntryRecord  := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryRecord');
      FunctionHeaderArgsEntryEnum    := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryEnum');
      FunctionHeaderArgsEntryMask    := GetTextFromChild(ATag, 'FunctionHeaderArgsEntryMask');
      FProgramHeaderTemplate         := GetTextFromChild(ATag, 'ProgramHeaderTemplate');
      DefaultExt                     := GetTextFromChild(ATag, 'DefaultExt', DefaultExt);
      LibraryExt                     := GetTextFromChild(ATag, 'LibraryExt', LibraryExt);
      AssignOperator                 := GetTextFromChild(ATag, 'AssignOperator', AssignOperator);
      UserTypeDesc                   := GetTextFromChild(ATag, 'UserTypeDesc');
      ExternalLabel                  := GetTextFromChild(ATag, 'ExternalLabel');
      StaticLabel                    := GetTextFromChild(ATag, 'StaticLabel');
      RecordLabel                    := GetTextFromChild(ATag, 'RecordLabel');

      FuncBracketsCursorPos           := GetIntFromChild(ATag, 'FuncBracketsCursorPos');
      LabelFontSize                   := GetIntFromChild(ATag, 'LabelFontSize', LabelFontSize);
      VarEntryArraySizeStripCount     := GetIntFromChild(ATag, 'VarEntryArraySizeStripCount');
      LibEntryListStripCount          := GetIntFromChild(ATag, 'LibEntryListStripCount');
      FunctionHeaderArgsStripCount    := GetIntFromChild(ATag, 'FunctionHeaderArgsStripCount');
      InOutCursorPos                  := GetIntFromChild(ATag, 'InOutCursorPos');
      DataTypeEnumEntryListStripCount := GetIntFromChild(ATag, 'DataTypeEnumEntryListStripCount');

      ForDoVarList              := GetBoolFromChild(ATag, 'ForDoVarList', ForDoVarList);
      EnabledPointers           := GetBoolFromChild(ATag, 'EnabledPointers', EnabledPointers);
      RepeatUntilAsDoWhile      := GetBoolFromChild(ATag, 'RepeatUntilAsDoWhile', RepeatUntilAsDoWhile);
      EnabledConsts             := GetBoolFromChild(ATag, 'EnabledConsts', EnabledConsts);
      EnabledVars               := GetBoolFromChild(ATag, 'EnabledVars', EnabledVars);
      EnabledCompiler           := GetBoolFromChild(ATag, 'EnabledCompiler', EnabledCompiler);
      EnabledUserFunctionHeader := GetBoolFromChild(ATag, 'EnabledUserFunctionHeader', EnabledUserFunctionHeader);
      EnabledUserFunctionBody   := GetBoolFromChild(ATag, 'EnabledUserFunctionBody', EnabledUserFunctionBody);
      EnabledUserDataTypes      := GetBoolFromChild(ATag, 'EnabledUserDataTypes', EnabledUserDataTypes);
      EnabledUserDataTypeInt    := GetBoolFromChild(ATag, 'EnabledUserDataTypeInt', EnabledUserDataTypeInt);
      EnabledUserDataTypeReal   := GetBoolFromChild(ATag, 'EnabledUserDataTypeReal', EnabledUserDataTypeReal);
      EnabledUserDataTypeOther  := GetBoolFromChild(ATag, 'EnabledUserDataTypeOther', EnabledUserDataTypeOther);
      EnabledUserDataTypeArray  := GetBoolFromChild(ATag, 'EnabledUserDataTypeArray', EnabledUserDataTypeArray);
      EnabledUserDataTypeEnum   := GetBoolFromChild(ATag, 'EnabledUserDataTypeEnum', EnabledUserDataTypeEnum);
      EnabledExplorer           := GetBoolFromChild(ATag, 'EnabledExplorer', EnabledExplorer);
      EnabledCodeGenerator      := GetBoolFromChild(ATag, 'EnabledCodeGenerator', EnabledCodeGenerator);
      EnabledMainProgram        := GetBoolFromChild(ATag, 'EnabledMainProgram', EnabledMainProgram);
      CaseSensitiveSyntax       := GetBoolFromChild(ATag, 'CaseSensitiveSyntax', CaseSensitiveSyntax);
      UpperCaseConstId          := GetBoolFromChild(ATag, 'UpperCaseConstId', UpperCaseConstId);
      AllowEnumsInForLoop       := GetBoolFromChild(ATag, 'AllowEnumsInForLoop', AllowEnumsInForLoop);
      AllowUserFunctionOverload := GetBoolFromChild(ATag, 'AllowUserFunctionOverload', AllowUserFunctionOverload);
      AllowUnboundedArrays      := GetBoolFromChild(ATag, 'AllowUnboundedArrays', AllowUnboundedArrays);
      AllowDuplicatedLibs       := GetBoolFromChild(ATag, 'AllowDuplicatedLibs', AllowDuplicatedLibs);
      AllowTransExternVarConst  := GetBoolFromChild(ATag, 'AllowTransExternVarConst', AllowTransExternVarConst);
      AllowTransExternFunction  := GetBoolFromChild(ATag, 'AllowTransExternFunction', AllowTransExternFunction);
      AllowTransExternDataType  := GetBoolFromChild(ATag, 'AllowTransExternDataType', AllowTransExternDataType);
      CodeIncludeExternVarConst := GetBoolFromChild(ATag, 'CodeIncludeExternVarConst', CodeIncludeExternVarConst);
      CodeIncludeExternDataType := GetBoolFromChild(ATag, 'CodeIncludeExternDataType', CodeIncludeExternDataType);
      CodeIncludeExternFunction := GetBoolFromChild(ATag, 'CodeIncludeExternFunction', CodeIncludeExternFunction);
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'NativeDataTypes');
   if tag <> nil then
   begin
      a := 0;
      count := TXMLProcessor.CountChildTags(tag, 'DataType', true);
      SetLength(NativeDataTypes, count);
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
                  val := tag.GetAttribute('origtype').Trim;
                  for i := 0 to a-1 do
                  begin
                     if SameText(val, NativeDataTypes[i].Name) then
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
            lType := @NativeDataTypes[a-1];
            lType.Name := lName;
            lType.Kind := lKind;
            if lOrigType = nil then
               lOrigType := lType;
            lType.OrigType := lOrigType;
            lType.IsGeneric := TXMLProcessor.GetBool(tag, 'generic');
            lType.Lib := tag.GetAttribute('library');
         end;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      if a < count then
         SetLength(NativeDataTypes, a);
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
      count := TXMLProcessor.CountChildTags(tag, 'Function', true);
      SetLength(NativeFunctions, count);
      tag := TXMLProcessor.FindChildTag(tag, 'Function');
      i := 0;
      while tag <> nil do
      begin
         val := tag.Text.Trim;
         if not val.IsEmpty then
         begin
            with NativeFunctions[i] do
            begin
               Name := val;
               Brackets := tag.GetAttribute('brackets');
               BracketsCursorPos := TXMLProcessor.GetInt(tag, 'bracketsCursorPos');
               Caption := tag.GetAttribute('caption').Trim;
               Hint := tag.GetAttribute('hint').Trim;
               Lib := tag.GetAttribute('library').Trim;
            end;
            i := i + 1;
         end;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
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
            var tag1 := TXMLProcessor.FindChildTag(tag, 'Open');
            if tag1 <> nil then
               Open := tag1.GetAttribute('Keyword');
            tag1 := TXMLProcessor.FindChildTag(tag, 'Close');
            if tag1 <> nil then
               Close := tag1.GetAttribute('Keyword');
            AddClose := TXMLProcessor.GetBool(tag, 'AddClose');
            NoSubFolds := TXMLProcessor.GetBool(tag, 'NoSubFolds', true);
            WholeWords := TXMLProcessor.GetBool(tag, 'WholeWords', true);
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

procedure TLangDefinition.LoadCompilerData;
var
   sFile: TCustomIniFile;
begin
   sFile := GSettings.SettingsFile;
   CompilerCommand := sFile.ReadString(SETTINGS_SECTION, FCompilerKey, '');
   CompilerCommandNoMain := sFile.ReadString(SETTINGS_SECTION, FCompilerNoMainKey, '');
   CompilerFileEncoding := sFile.ReadString(SETTINGS_SECTION, FCompilerFileEncodingKey, '');
end;

procedure TLangDefinition.SaveCompilerData;
var
   sFile: TCustomIniFile;
begin
   sFile := GSettings.SettingsFile;
   sFile.WriteString(SETTINGS_SECTION, FCompilerKey, CompilerCommand);
   sFile.WriteString(SETTINGS_SECTION, FCompilerNoMainKey, CompilerCommandNoMain);
   sFile.WriteString(SETTINGS_SECTION, FCompilerFileEncodingKey, CompilerFileEncoding);
end;

function TLangDefinition.GetBlockTemplate(ABlockType: TBlockType): string;
begin
   result := BlockTemplates[ABlockType];
end;

function TLangDefinition.GetArraySizes(ASizeEdit: TSizeEdit): string;
var
   i: integer;
   dims: TArray<string>;
begin
   result := '';
   if ASizeEdit <> nil then
   begin
      dims := ASizeEdit.GetDimensions;
      for i := 0 to High(dims) do
         result := result + Format(VarEntryArraySize, [dims[i]]);
      if VarEntryArraySizeStripCount > 0 then
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

function TLangDefinition.GetVarTemplate: string;
begin
   result := ReplaceStr(FVarTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetConstTemplate: string;
begin
   result := ReplaceStr(FConstTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetDataTypesTemplate: string;
begin
   result := ReplaceStr(FDataTypesTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetFunctionsTemplate: string;
begin
   result := ReplaceStr(FFunctionsTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetFileContentsTemplate: string;
begin
   result := ReplaceStr(FFileContentsTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetLibTemplate: string;
begin
   result := ReplaceStr(FLibTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

function TLangDefinition.GetProgramHeaderTemplate: string;
begin
   result := ReplaceStr(FProgramHeaderTemplate, INDENT_XML_CHAR, GSettings.IndentString);
end;

procedure TLangDefinition.InitBlockTemplates;
begin
   for var blockType := Low(TBlockType) to High(TBlockType) do
      BlockTemplates[blockType] := i18Manager.GetString(BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
end;

procedure TLangDefinition.ImportBlockTemplates(ATag: IXMLElement);
begin
   for var blockType := Low(TBlockType) to High(TBlockType) do
      BlockTemplates[blockType] := TXMLProcessor.GetTextFromChild(ATag, BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
end;

end.
