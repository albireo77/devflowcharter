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
    CommonTypes, OmniXML, UserDataType;

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
   Vcl.Forms, System.StrUtils, System.IniFiles, ApplicationCommon, XMLProcessor;

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
   val := '';
   tag := TXMLProcessor.FindChildTag(ATag, 'Name');
   if tag <> nil then
      val := tag.Text.Trim;
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

   tag := TXMLProcessor.FindChildTag(ATag, 'ProcedureLabelKey');
   if tag <> nil then
      ProcedureLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionLabelKey');
   if tag <> nil then
      FunctionLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstructorLabelKey');
   if tag <> nil then
      ConstructorLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramLabelKey');
   if tag <> nil then
      ProgramLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'GlobalVarsLabelKey');
   if tag <> nil then
      GlobalVarsLabelKey := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'GlobalConstsLabelKey');
   if tag <> nil then
      GlobalConstsLabelKey := tag.Text;

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
      FVarTemplate := tag.Text;

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

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionTemplate');
   if tag <> nil then
      FunctionTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTemplate');
   if tag <> nil then
      FunctionHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderDescTemplate');
   if tag <> nil then
      FunctionHeaderDescTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderDescParmMask');
   if tag <> nil then
      FunctionHeaderDescParmMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderDescReturnMask');
   if tag <> nil then
      FunctionHeaderDescReturnMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstructorHeaderTemplate');
   if tag <> nil then
      ConstructorHeaderTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'PointerTypeMask');
   if tag <> nil then
      PointerTypeMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfValueTemplate');
   if tag <> nil then
      CaseOfValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfFirstValueTemplate');
   if tag <> nil then
      CaseOfFirstValueTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'CaseOfDefaultValueTemplate');
   if tag <> nil then
      CaseOfDefaultValueTemplate := tag.Text;

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

   tag := TXMLProcessor.FindChildTag(ATag, 'MainFunctionTemplate');
   if tag <> nil then
      MainFunctionTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ProgramReturnTemplate');
   if tag <> nil then
      ProgramReturnTemplate := tag.Text;

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

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypesTemplate');
   if tag <> nil then
      FDataTypesTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionsTemplate');
   if tag <> nil then
      FFunctionsTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, FILE_CONTENTS_TAG);
   if tag <> nil then
      FFileContentsTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeExternalModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      DataTypeExternal := l3Strings.S0;
      DataTypeNotExternal := l3Strings.S1;
      DataTypeTransExternal := l3Strings.S2;
   end;

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

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeUnboundedArrayMask');
   if tag <> nil then
      DataTypeUnboundedArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRecordFieldMask');
   if tag <> nil then
      DataTypeRecordFieldMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeRecordFieldArrayMask');
   if tag <> nil then
      DataTypeRecordFieldArrayMask := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstTemplate');
   if tag <> nil then
      FConstTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstEntry');
   if tag <> nil then
      ConstEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstEntryArray');
   if tag <> nil then
      ConstEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstTypeModifier');
   if tag <> nil then
   begin
      l3Strings := T3Strings.Extract(tag.Text);
      ConstTypeNotGeneric := l3Strings.S0;
      ConstTypeGeneric := l3Strings.S1;
   end;

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
      FLibTemplate := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LibEntry');
   if tag <> nil then
      LibEntry := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'LibEntryList');
   if tag <> nil then
      LibEntryList := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryArray');
   if tag <> nil then
      FunctionHeaderArgsEntryArray := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderArgsEntryDefault');
   if tag <> nil then
      FunctionHeaderArgsEntryDefault := tag.Text;

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
      FProgramHeaderTemplate := tag.Text;

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

   tag := TXMLProcessor.FindChildTag(ATag, 'ExternalLabel');
   if tag <> nil then
      ExternalLabel := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'StaticLabel');
   if tag <> nil then
      StaticLabel := tag.Text;

   tag := TXMLProcessor.FindChildTag(ATag, 'RecordLabel');
   if tag <> nil then
      RecordLabel := tag.Text;

   with TXMLProcessor do
   begin
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
   result := ReplaceStr(FVarTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetConstTemplate: string;
begin
   result := ReplaceStr(FConstTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetDataTypesTemplate: string;
begin
   result := ReplaceStr(FDataTypesTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetFunctionsTemplate: string;
begin
   result := ReplaceStr(FFunctionsTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetFileContentsTemplate: string;
begin
   result := ReplaceStr(FFileContentsTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetLibTemplate: string;
begin
   result := ReplaceStr(FLibTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

function TLangDefinition.GetProgramHeaderTemplate: string;
begin
   result := ReplaceStr(FProgramHeaderTemplate, INDENT_XML_CHAR, GSettings.IndentSpaces);
end;

procedure TLangDefinition.InitBlockTemplates;
var
   blockType: TBlockType;
begin
   for blockType := Low(TBlockType) to High(TBlockType) do
   begin
      BlockTemplates[blockType] := i18Manager.GetString(BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
   end;
end;

procedure TLangDefinition.ImportBlockTemplates(ATag: IXMLElement);
var
   value: string;
   tag: IXMLElement;
   blockType: TBlockType;
begin
   for blockType := Low(TBlockType) to High(TBlockType) do
   begin
      tag := TXMLProcessor.FindChildTag(ATag, BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
      if tag <> nil then
         value := tag.Text
      else
         value := '';
      BlockTemplates[blockType] := value;
   end;
end;

end.
