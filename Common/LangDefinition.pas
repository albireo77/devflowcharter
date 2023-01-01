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
      FProgramTemplate,
      FLibTemplate,
      FProgramHeaderTemplate: string;
      function GetVarTemplate: string;
      function GetConstTemplate: string;
      function GetDataTypesTemplate: string;
      function GetFunctionsTemplate: string;
      function GetProgramTemplate: string;
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
      BeforeProgramGenerator: procedure;
      AfterProgramGenerator: procedure;
      ProgramHeaderSectionGenerator: procedure (ALines: TStringList);
      LibSectionGenerator: procedure (ALines: TStringList);
      UserDataTypesSectionGenerator: procedure (ALines: TStringList);
      VarSectionGenerator: procedure (ALines: TStringList; AVarList: TVarDeclareList);
      ConstSectionGenerator: procedure (ALines: TStringList; AConstList: TConstDeclareList);
      UserFunctionsSectionGenerator: procedure (ALines: TStringList; ASkipBodyGenerate: boolean);
      MainFunctionSectionGenerator: procedure (ALines: TStringList; ADeep: integer);
      ProgramGenerator: procedure (ALines: TStringList);
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
      property ProgramTemplate: string read GetProgramTemplate;
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
   Vcl.Forms, System.IniFiles, XMLProcessor, Constants, Infrastructure, OmniXMLUtils;

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
   i, a: integer;
   threeStrings: T3Strings;
begin
   result := errNone;
   val := GetNodeTextStr(ATag, 'Name', '');
   if val.IsEmpty then
   begin
      GErr_Text := i18Manager.GetString('NameTagNotFound');
      Exit(errValidate);
   end;

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
      threeStrings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeNone1 := threeStrings.S0;
      FunctionHeaderTypeNotNone1 := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeModifier2');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeNone2 := threeStrings.S0;
      FunctionHeaderTypeNotNone2 := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderExternalModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      FunctionHeaderExternal := threeStrings.S0;
      FunctionHeaderNotExternal := threeStrings.S1;
      FunctionHeaderTransExternal := threeStrings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderStaticModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      FunctionHeaderStatic := threeStrings.S0;
      FunctionHeaderNotStatic := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'FunctionHeaderTypeArrayModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      FunctionHeaderTypeArray := threeStrings.S0;
      FunctionHeaderTypeNotArray := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'VarExternModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      VarExtern := threeStrings.S0;
      VarNotExtern := threeStrings.S1;
      VarTransExtern := threeStrings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstExternModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      ConstExtern := threeStrings.S0;
      ConstNotExtern := threeStrings.S1;
      ConstTransExtern := threeStrings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoTemplateModifier1');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      ForDoAsc1 := threeStrings.S0;
      ForDoDesc1 := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ForDoTemplateModifier2');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      ForDoAsc2 := threeStrings.S0;
      ForDoDesc2 := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'DataTypeExternalModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      DataTypeExternal := threeStrings.S0;
      DataTypeNotExternal := threeStrings.S1;
      DataTypeTransExternal := threeStrings.S2;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'ConstTypeModifier');
   if tag <> nil then
   begin
      threeStrings := T3Strings.Extract(tag.Text);
      ConstTypeNotGeneric := threeStrings.S0;
      ConstTypeGeneric := threeStrings.S1;
   end;

   tag := TXMLProcessor.FindChildTag(ATag, 'LabelFontName');
   if (tag <> nil) and (Screen.Fonts.IndexOf(tag.Text) <> -1) then
      LabelFontName := tag.Text;

   CommentBegin                    := GetNodeTextStr(ATag, 'CommentBegin', CommentBegin);
   CommentEnd                      := GetNodeTextStr(ATag, 'CommentEnd', CommentEnd);
   InputFunction                   := GetNodeTextStr(ATag, 'InputFunction', InputFunction);
   OutputFunction                  := GetNodeTextStr(ATag, 'OutputFunction', OutputFunction);
   ProcedureLabelKey               := GetNodeTextStr(ATag, 'ProcedureLabelKey', ProcedureLabelKey);
   FunctionLabelKey                := GetNodeTextStr(ATag, 'FunctionLabelKey', FunctionLabelKey);
   ConstructorLabelKey             := GetNodeTextStr(ATag, 'ConstructorLabelKey', ConstructorLabelKey);
   ProgramLabelKey                 := GetNodeTextStr(ATag, 'ProgramLabelKey', ProgramLabelKey);
   GlobalVarsLabelKey              := GetNodeTextStr(ATag, 'GlobalVarsLabelKey', GlobalVarsLabelKey);
   GlobalConstsLabelKey            := GetNodeTextStr(ATag, 'GlobalConstsLabelKey', GlobalConstsLabelKey);
   HighLighterVarName              := GetNodeTextStr(ATag, 'HighLighterVarName', HighLighterVarName);
   ForDoVarString                  := GetNodeTextStr(ATag, 'ForDoVarString', ForDoVarString);
   ConstIDSpecChars                := GetNodeTextStr(ATag, 'ConstIDSpecChars', ConstIDSpecChars);
   FuncBrackets                    := GetNodeTextStr(ATag, 'FuncBrackets', FuncBrackets);
   InstrEnd                        := GetNodeTextStr(ATag, 'InstrEnd', InstrEnd);
   FVarTemplate                    := GetNodeTextStr(ATag, 'VarTemplate', FVarTemplate);
   FunctionTemplate                := GetNodeTextStr(ATag, 'FunctionTemplate', FunctionTemplate);
   FunctionHeaderTemplate          := GetNodeTextStr(ATag, 'FunctionHeaderTemplate', FunctionHeaderTemplate);
   FunctionHeaderDescTemplate      := GetNodeTextStr(ATag, 'FunctionHeaderDescTemplate', FunctionHeaderDescTemplate);
   FunctionHeaderDescParmMask      := GetNodeTextStr(ATag, 'FunctionHeaderDescParmMask', FunctionHeaderDescParmMask);
   FunctionHeaderDescReturnMask    := GetNodeTextStr(ATag, 'FunctionHeaderDescReturnMask', FunctionHeaderDescReturnMask);
   ConstructorHeaderTemplate       := GetNodeTextStr(ATag, 'ConstructorHeaderTemplate', ConstructorHeaderTemplate);
   PointerTypeMask                 := GetNodeTextStr(ATag, 'PointerTypeMask', PointerTypeMask);
   CaseOfValueTemplate             := GetNodeTextStr(ATag, 'CaseOfValueTemplate', CaseOfValueTemplate);
   CaseOfFirstValueTemplate        := GetNodeTextStr(ATag, 'CaseOfFirstValueTemplate', CaseOfFirstValueTemplate);
   CaseOfDefaultValueTemplate      := GetNodeTextStr(ATag, 'CaseOfDefaultValueTemplate', CaseOfDefaultValueTemplate);
   ElseLabel                       := GetNodeTextStr(ATag, 'ElseLabel', ElseLabel);
   LabelRepeat                     := GetNodeTextStr(ATag, 'LabelRepeat', LabelRepeat);
   LabelWhile                      := GetNodeTextStr(ATag, 'LabelWhile', LabelWhile);
   LabelFor                        := GetNodeTextStr(ATag, 'LabelFor', LabelFor);
   LabelCase                       := GetNodeTextStr(ATag, 'LabelCase', LabelCase);
   LabelIf                         := GetNodeTextStr(ATag, 'LabelIf', LabelIf);
   LabelIfElse                     := GetNodeTextStr(ATag, 'LabelIfElse', LabelIfElse);
   LabelFuncCall                   := GetNodeTextStr(ATag, 'LabelFuncCall', LabelFuncCall);
   LabelText                       := GetNodeTextStr(ATag, 'LabelText', LabelText);
   LabelFolder                     := GetNodeTextStr(ATag, 'LabelFolder', LabelFolder);
   LabelIn                         := GetNodeTextStr(ATag, 'LabelIn', LabelIn);
   LabelOut                        := GetNodeTextStr(ATag, 'LabelOut', LabelOut);
   LabelInstr                      := GetNodeTextStr(ATag, 'LabelInstr', LabelInstr);
   LabelMultiInstr                 := GetNodeTextStr(ATag, 'LabelMultiInstr', LabelMultiInstr);
   RepeatUntilDescTemplate         := GetNodeTextStr(ATag, 'RepeatUntilDescTemplate', RepeatUntilDescTemplate);
   ReturnDescTemplate              := GetNodeTextStr(ATag, 'ReturnDescTemplate', ReturnDescTemplate);
   ForDoDescTemplate               := GetNodeTextStr(ATag, 'ForDoDescTemplate', ForDoDescTemplate);
   CaseOfDescTemplate              := GetNodeTextStr(ATag, 'CaseOfDescTemplate', CaseOfDescTemplate);
   MainFunctionTemplate            := GetNodeTextStr(ATag, 'MainFunctionTemplate', MainFunctionTemplate);
   ProgramReturnTemplate           := GetNodeTextStr(ATag, 'ProgramReturnTemplate', ProgramReturnTemplate);
   FDataTypesTemplate              := GetNodeTextStr(ATag, 'DataTypesTemplate', FDataTypesTemplate);
   FFunctionsTemplate              := GetNodeTextStr(ATag, 'FunctionsTemplate', FFunctionsTemplate);
   FProgramTemplate                := GetNodeTextStr(ATag, PROGRAM_TEMPLATE_TAG, FProgramTemplate);
   DataTypeIntMask                 := GetNodeTextStr(ATag, 'DataTypeIntMask', DataTypeIntMask);
   DataTypeRealMask                := GetNodeTextStr(ATag, 'DataTypeRealMask', DataTypeRealMask);
   DataTypeOtherMask               := GetNodeTextStr(ATag, 'DataTypeOtherMask', DataTypeOtherMask);
   DataTypeRecordTemplate          := GetNodeTextStr(ATag, 'DataTypeRecordTemplate', DataTypeRecordTemplate);
   DataTypeEnumTemplate            := GetNodeTextStr(ATag, 'DataTypeEnumTemplate', DataTypeEnumTemplate);
   DataTypeEnumEntryList           := GetNodeTextStr(ATag, 'DataTypeEnumEntryList', DataTypeEnumEntryList);
   DataTypeArrayMask               := GetNodeTextStr(ATag, 'DataTypeArrayMask', DataTypeArrayMask);
   DataTypeUnboundedArrayMask      := GetNodeTextStr(ATag, 'DataTypeUnboundedArrayMask', DataTypeUnboundedArrayMask);
   DataTypeRecordFieldMask         := GetNodeTextStr(ATag, 'DataTypeRecordFieldMask', DataTypeRecordFieldMask);
   DataTypeRecordFieldArrayMask    := GetNodeTextStr(ATag, 'DataTypeRecordFieldArrayMask', DataTypeRecordFieldArrayMask);
   FConstTemplate                  := GetNodeTextStr(ATag, 'ConstTemplate', FConstTemplate);
   ConstEntry                      := GetNodeTextStr(ATag, 'ConstEntry', ConstEntry);
   ConstEntryArray                 := GetNodeTextStr(ATag, 'ConstEntryArray', ConstEntryArray);
   VarEntryInit                    := GetNodeTextStr(ATag, 'VarEntryInit', VarEntryInit);
   VarEntryInitExtern              := GetNodeTextStr(ATag, 'VarEntryInitExtern', VarEntryInitExtern);
   VarEntry                        := GetNodeTextStr(ATag, 'VarEntry', VarEntry);
   VarEntryArray                   := GetNodeTextStr(ATag, 'VarEntryArray', VarEntryArray);
   VarEntryArraySize               := GetNodeTextStr(ATag, 'VarEntryArraySize', VarEntryArraySize);
   FLibTemplate                    := GetNodeTextStr(ATag, 'LibTemplate', FLibTemplate);
   LibEntry                        := GetNodeTextStr(ATag, 'LibEntry', LibEntry);
   LibEntryList                    := GetNodeTextStr(ATag, 'LibEntryList', LibEntryList);
   FunctionHeaderArgsEntryArray    := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryArray', FunctionHeaderArgsEntryArray);
   FunctionHeaderArgsEntryDefault  := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryDefault', FunctionHeaderArgsEntryDefault);
   FunctionHeaderArgsEntryRef      := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryRef', FunctionHeaderArgsEntryRef);
   FunctionHeaderArgsEntryRecord   := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryRecord', FunctionHeaderArgsEntryRecord);
   FunctionHeaderArgsEntryEnum     := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryEnum', FunctionHeaderArgsEntryEnum);
   FunctionHeaderArgsEntryMask     := GetNodeTextStr(ATag, 'FunctionHeaderArgsEntryMask', FunctionHeaderArgsEntryMask);
   FProgramHeaderTemplate          := GetNodeTextStr(ATag, 'ProgramHeaderTemplate', FProgramHeaderTemplate);
   DefaultExt                      := GetNodeTextStr(ATag, 'DefaultExt', DefaultExt);
   LibraryExt                      := GetNodeTextStr(ATag, 'LibraryExt', LibraryExt);
   AssignOperator                  := GetNodeTextStr(ATag, 'AssignOperator', AssignOperator);
   UserTypeDesc                    := GetNodeTextStr(ATag, 'UserTypeDesc', UserTypeDesc);
   ExternalLabel                   := GetNodeTextStr(ATag, 'ExternalLabel', ExternalLabel);
   StaticLabel                     := GetNodeTextStr(ATag, 'StaticLabel', StaticLabel);
   RecordLabel                     := GetNodeTextStr(ATag, 'RecordLabel', RecordLabel);

   FuncBracketsCursorPos           := GetNodeTextInt(ATag, 'FuncBracketsCursorPos', FuncBracketsCursorPos);
   LabelFontSize                   := GetNodeTextInt(ATag, 'LabelFontSize', LabelFontSize);
   VarEntryArraySizeStripCount     := GetNodeTextInt(ATag, 'VarEntryArraySizeStripCount', VarEntryArraySizeStripCount);
   LibEntryListStripCount          := GetNodeTextInt(ATag, 'LibEntryListStripCount', LibEntryListStripCount);
   FunctionHeaderArgsStripCount    := GetNodeTextInt(ATag, 'FunctionHeaderArgsStripCount', FunctionHeaderArgsStripCount);
   InOutCursorPos                  := GetNodeTextInt(ATag, 'InOutCursorPos', InOutCursorPos);
   DataTypeEnumEntryListStripCount := GetNodeTextInt(ATag, 'DataTypeEnumEntryListStripCount', DataTypeEnumEntryListStripCount);

   ForDoVarList                    := GetNodeTextBool(ATag, 'ForDoVarList', ForDoVarList);
   EnabledPointers                 := GetNodeTextBool(ATag, 'EnabledPointers', EnabledPointers);
   RepeatUntilAsDoWhile            := GetNodeTextBool(ATag, 'RepeatUntilAsDoWhile', RepeatUntilAsDoWhile);
   EnabledConsts                   := GetNodeTextBool(ATag, 'EnabledConsts', EnabledConsts);
   EnabledVars                     := GetNodeTextBool(ATag, 'EnabledVars', EnabledVars);
   EnabledCompiler                 := GetNodeTextBool(ATag, 'EnabledCompiler', EnabledCompiler);
   EnabledUserFunctionHeader       := GetNodeTextBool(ATag, 'EnabledUserFunctionHeader', EnabledUserFunctionHeader);
   EnabledUserFunctionBody         := GetNodeTextBool(ATag, 'EnabledUserFunctionBody', EnabledUserFunctionBody);
   EnabledUserDataTypes            := GetNodeTextBool(ATag, 'EnabledUserDataTypes', EnabledUserDataTypes);
   EnabledUserDataTypeInt          := GetNodeTextBool(ATag, 'EnabledUserDataTypeInt', EnabledUserDataTypeInt);
   EnabledUserDataTypeReal         := GetNodeTextBool(ATag, 'EnabledUserDataTypeReal', EnabledUserDataTypeReal);
   EnabledUserDataTypeOther        := GetNodeTextBool(ATag, 'EnabledUserDataTypeOther', EnabledUserDataTypeOther);
   EnabledUserDataTypeArray        := GetNodeTextBool(ATag, 'EnabledUserDataTypeArray', EnabledUserDataTypeArray);
   EnabledUserDataTypeEnum         := GetNodeTextBool(ATag, 'EnabledUserDataTypeEnum', EnabledUserDataTypeEnum);
   EnabledExplorer                 := GetNodeTextBool(ATag, 'EnabledExplorer', EnabledExplorer);
   EnabledCodeGenerator            := GetNodeTextBool(ATag, 'EnabledCodeGenerator', EnabledCodeGenerator);
   EnabledMainProgram              := GetNodeTextBool(ATag, 'EnabledMainProgram', EnabledMainProgram);
   CaseSensitiveSyntax             := GetNodeTextBool(ATag, 'CaseSensitiveSyntax', CaseSensitiveSyntax);
   UpperCaseConstId                := GetNodeTextBool(ATag, 'UpperCaseConstId', UpperCaseConstId);
   AllowEnumsInForLoop             := GetNodeTextBool(ATag, 'AllowEnumsInForLoop', AllowEnumsInForLoop);
   AllowUserFunctionOverload       := GetNodeTextBool(ATag, 'AllowUserFunctionOverload', AllowUserFunctionOverload);
   AllowUnboundedArrays            := GetNodeTextBool(ATag, 'AllowUnboundedArrays', AllowUnboundedArrays);
   AllowDuplicatedLibs             := GetNodeTextBool(ATag, 'AllowDuplicatedLibs', AllowDuplicatedLibs);
   AllowTransExternVarConst        := GetNodeTextBool(ATag, 'AllowTransExternVarConst', AllowTransExternVarConst);
   AllowTransExternFunction        := GetNodeTextBool(ATag, 'AllowTransExternFunction', AllowTransExternFunction);
   AllowTransExternDataType        := GetNodeTextBool(ATag, 'AllowTransExternDataType', AllowTransExternDataType);
   CodeIncludeExternVarConst       := GetNodeTextBool(ATag, 'CodeIncludeExternVarConst', CodeIncludeExternVarConst);
   CodeIncludeExternDataType       := GetNodeTextBool(ATag, 'CodeIncludeExternDataType', CodeIncludeExternDataType);
   CodeIncludeExternFunction       := GetNodeTextBool(ATag, 'CodeIncludeExternFunction', CodeIncludeExternFunction);

   tag := TXMLProcessor.FindChildTag(ATag, 'NativeDataTypes');
   if tag <> nil then
   begin
      a := 0;
      var datatypeNodes := FilterNodes(tag, 'DataType');
      var count := TXMLProcessor.CountNodesWithText(datatypeNodes);
      SetLength(NativeDataTypes, count);
      var node := datatypeNodes.NextNode;
      while node <> nil do
      begin
         lOrigType := nil;
         lName := node.Text.Trim;
         if not lName.IsEmpty then
         begin
            kinds := GetNodeAttrStr(node, 'kind', '');
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
                  val := GetNodeAttrStr(node, 'origtype', '').Trim;
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
            lType.IsGeneric := GetNodeAttrBool(node, 'generic', false);
            lType.Lib := GetNodeAttrStr(node, 'library', '');
         end;
         node := datatypeNodes.NextNode;
      end;
      if a < count then
         SetLength(NativeDataTypes, a);
   end;
   tag := TXMLProcessor.FindChildTag(ATag, 'KeyWords');
   if tag <> nil then
   begin
      KeyWords.Sorted := false;
      KeyWords.CaseSensitive := CaseSensitiveSyntax;
      GetNodesText(tag, 'KeyWord', KeyWords);
      KeyWords.Sort;
   end;
   tag := TXMLProcessor.FindChildTag(ATag, 'NativeFunctions');
   if tag <> nil then
   begin
      var functionNodes := FilterNodes(tag, 'Function');
      SetLength(NativeFunctions, TXMLProcessor.CountNodesWithText(functionNodes));
      var node := functionNodes.NextNode;
      i := 0;
      while node <> nil do
      begin
         val := node.Text.Trim;
         if not val.IsEmpty then
         begin
            with NativeFunctions[i] do
            begin
               Name := val;
               Brackets := GetNodeAttrStr(node, 'brackets', '');
               BracketsCursorPos := GetNodeAttrInt(node, 'bracketsCursorPos', 0);
               Caption := GetNodeAttrStr(node, 'caption', '').Trim;
               Hint := GetNodeAttrStr(node, 'hint', '').Trim;
               Lib := GetNodeAttrStr(node, 'library', '').Trim;
            end;
            Inc(i);
         end;
         node := functionNodes.NextNode;
      end;
   end;
{$IFDEF USE_CODEFOLDING}
   tag := TXMLProcessor.FindChildTag(ATag, 'FoldRegions');
   if tag <> nil then
   begin
      i := 0;
      var foldRegionNodes := FilterNodes(tag, 'FoldRegion');
      SetLength(FoldRegions, foldRegionNodes.Length);
      var node := foldRegionNodes.NextNode;
      while node <> nil do
      begin
         with FoldRegions[i] do
         begin
            var tag1 := TXMLProcessor.FindChildTag(node, 'Open');
            if tag1 <> nil then
               Open := tag1.GetAttribute('Keyword');
            tag1 := TXMLProcessor.FindChildTag(node, 'Close');
            if tag1 <> nil then
               Close := tag1.GetAttribute('Keyword');
            AddClose := GetNodeAttrBool(node, 'AddClose', false);
            NoSubFolds := GetNodeAttrBool(node, 'NoSubFolds', true);
            WholeWords := GetNodeAttrBool(node, 'WholeWords', true);
            if GetNodeAttrStr(node, 'Type', '') = 'rtChar' then
               RegionType := rtChar
            else
               RegionType := rtKeyword;
         end;
         node := foldRegionNodes.NextNode;
         Inc(i);
      end;
   end;
{$ENDIF}
end;

procedure TLangDefinition.LoadCompilerData;
begin
   var sFile := GSettings.SettingsFile;
   CompilerCommand := sFile.ReadString(SETTINGS_SECTION, FCompilerKey, '');
   CompilerCommandNoMain := sFile.ReadString(SETTINGS_SECTION, FCompilerNoMainKey, '');
   CompilerFileEncoding := sFile.ReadString(SETTINGS_SECTION, FCompilerFileEncodingKey, '');
end;

procedure TLangDefinition.SaveCompilerData;
begin
   var sFile := GSettings.SettingsFile;
   sFile.WriteString(SETTINGS_SECTION, FCompilerKey, CompilerCommand);
   sFile.WriteString(SETTINGS_SECTION, FCompilerNoMainKey, CompilerCommandNoMain);
   sFile.WriteString(SETTINGS_SECTION, FCompilerFileEncodingKey, CompilerFileEncoding);
end;

function TLangDefinition.GetBlockTemplate(ABlockType: TBlockType): string;
begin
   result := BlockTemplates[ABlockType];
end;

function TLangDefinition.GetArraySizes(ASizeEdit: TSizeEdit): string;
begin
   result := '';
   if ASizeEdit <> nil then
   begin
      var dims := ASizeEdit.GetDimensions;
      for var i := 0 to High(dims) do
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
   result := TInfra.ReplaceXMLIndents(FVarTemplate);
end;

function TLangDefinition.GetConstTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FConstTemplate);
end;

function TLangDefinition.GetDataTypesTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FDataTypesTemplate);
end;

function TLangDefinition.GetFunctionsTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FFunctionsTemplate);
end;

function TLangDefinition.GetProgramTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FProgramTemplate);
end;

function TLangDefinition.GetLibTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FLibTemplate);
end;

function TLangDefinition.GetProgramHeaderTemplate: string;
begin
   result := TInfra.ReplaceXMLIndents(FProgramHeaderTemplate);
end;

procedure TLangDefinition.InitBlockTemplates;
begin
   for var blockType := Low(TBlockType) to High(TBlockType) do
      BlockTemplates[blockType] := i18Manager.GetString(BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
end;

procedure TLangDefinition.ImportBlockTemplates(ATag: IXMLElement);
begin
   for var blockType := Low(TBlockType) to High(TBlockType) do
      BlockTemplates[blockType] := GetNodeTextStr(ATag, BLOCK_TO_TEMPLATE_TAG_MAP[blockType], '');
end;

end.
