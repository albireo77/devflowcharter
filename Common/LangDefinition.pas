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
    System.Classes, System.SysUtils, SizeEdit, SynEditHighlighter, YaccLib, DeclareList,
    UserFunction, Types, UserDataType, MSXML2_TLB;

type

{$IFDEF USE_CODEFOLDING}
   PFoldRegion = ^TFoldRegion;
   TFoldRegion = record
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
      procedure ImportBlockTemplates(ATag: IXMLDOMElement);
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
      FoldRegions: array of PFoldRegion;
{$ENDIF}
      Parser: TCustomParser;
      NativeDataTypes: array of PNativeDataType;
      NativeFunctions: array of PNativeFunction;
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
      UserDataTypeGenerator: procedure (ALines: TStringList; ADataType: TUserDataType);
      UserDataTypesSectionGenerator: procedure (ALines: TStringList);
      VarSectionGenerator: procedure (ALines: TStringList; AVarList: TVarDeclareList);
      ConstSectionGenerator: procedure (ALines: TStringList; AConstList: TConstDeclareList);
      UserFunctionGenerator: procedure (ALines: TStringList; AFunction: TUserFunction; ASkipBodyGen: boolean);
      UserFunctionsSectionGenerator: procedure (ALines: TStringList; ASkipBodyGenerate: boolean);
      MainFunctionSectionGenerator: procedure (ALines: TStringList; ADeep: integer);
      ProgramGenerator: procedure (ALines: TStringList);
      GetUserFuncDesc: function (AHeader: TUserFunctionHeader; AFullParams: boolean = True; AIncludeDesc: boolean = True): string;
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
      function ImportFromXML(ATag: IXMLDOMElement; AImportMode: TImportMode): TError;
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
   Vcl.Forms, System.Rtti, Constants, Infrastructure, XMLUtils;

constructor TLangDefinition.Create;
begin
   inherited;
   FName := '   ';
   DefaultExt := 'txt';
   LibraryExt := '.lib';
   AssignOperator := '=';
   ForDoVarString := '=';
   KeyWords := TStringList.Create;
   UpperCaseConstId := True;
   EnabledPointers := True;
   CompilerFileEncoding := 'ANSI';
   DecimalSeparator := '.';
   GlobalVarsLabelKey := 'GlobalVars';
   GlobalConstsLabelKey := 'GlobalConsts';
   EnabledUserDataTypeInt := True;
   EnabledUserDataTypeReal := True;
   EnabledUserDataTypeOther := True;
   EnabledUserDataTypeEnum := True;
   EnabledUserDataTypeArray := True;
   LabelFontName := FLOWCHART_DEFAULT_FONT_NAME;
   LabelFontSize := LABEL_DEFAULT_FONT_SIZE;
   ElseLabel := trnsManager.GetString('ElseLabel');
   InitBlockTemplates;
end;

destructor TLangDefinition.Destroy;
begin
   KeyWords.Free;
   KeyWords := nil;
   for var t in NativeDataTypes do
      Dispose(t);
   for var f in NativeFunctions do
      Dispose(f);
   NativeDataTypes := nil;
   NativeFunctions := nil;
   Parser.Free;
   Parser := nil;
   if not FName.Trim.IsEmpty then
      SaveCompilerData;
{$IFDEF USE_CODEFOLDING}
   for var fr in FoldRegions do
      Dispose(fr);
   FoldRegions := nil;
{$ENDIF}
   inherited;
end;

function TLangDefinition.ImportFromXML(ATag: IXMLDOMElement; AImportMode: TImportMode): TError;
label
   skip;
begin

   result := errNone;
   var name := GetChildTextStr(ATag, 'Name');
   if name.IsEmpty then
   begin
      GErr_Text := trnsManager.GetString('NameTagNotFound');
      Exit(errValidate);
   end;

   FName := name;

   FCompilerKey := 'CompilerPath_' + FName;
   FCompilerNoMainKey := 'CompilerPathNoMain_' + FName;
   FCompilerFileEncodingKey := 'CompilerFileEncoding_' + FName;

   ImportBlockTemplates(ATag);

   var s := GetChildTextStr(ATag, 'DecimalSeparator');
   if not s.IsEmpty then
      DecimalSeparator := s[1];

   var s3: T3Strings;
   s := GetChildTextStr(ATag, 'FunctionHeaderTypeModifier1');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      FunctionHeaderTypeNone1 := s3.S0;
      FunctionHeaderTypeNotNone1 := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'FunctionHeaderTypeModifier2');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      FunctionHeaderTypeNone2 := s3.S0;
      FunctionHeaderTypeNotNone2 := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'FunctionHeaderExternalModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      FunctionHeaderExternal := s3.S0;
      FunctionHeaderNotExternal := s3.S1;
      FunctionHeaderTransExternal := s3.S2;
   end;

   s := GetChildTextStr(ATag, 'FunctionHeaderStaticModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      FunctionHeaderStatic := s3.S0;
      FunctionHeaderNotStatic := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'FunctionHeaderTypeArrayModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      FunctionHeaderTypeArray := s3.S0;
      FunctionHeaderTypeNotArray := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'VarExternModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      VarExtern := s3.S0;
      VarNotExtern := s3.S1;
      VarTransExtern := s3.S2;
   end;

   s := GetChildTextStr(ATag, 'ConstExternModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      ConstExtern := s3.S0;
      ConstNotExtern := s3.S1;
      ConstTransExtern := s3.S2;
   end;

   s := GetChildTextStr(ATag, 'ForDoTemplateModifier1');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      ForDoAsc1 := s3.S0;
      ForDoDesc1 := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'ForDoTemplateModifier2');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      ForDoAsc2 := s3.S0;
      ForDoDesc2 := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'DataTypeExternalModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      DataTypeExternal := s3.S0;
      DataTypeNotExternal := s3.S1;
      DataTypeTransExternal := s3.S2;
   end;

   s := GetChildTextStr(ATag, 'ConstTypeModifier');
   if not s.IsEmpty then
   begin
      s3 := T3Strings.Extract(s);
      ConstTypeNotGeneric := s3.S0;
      ConstTypeGeneric := s3.S1;
   end;

   s := GetChildTextStr(ATag, 'LabelFontName');
   if Screen.Fonts.Contains(s) then
      LabelFontName := s;

   CommentBegin                    := GetChildTextStr(ATag, 'CommentBegin', CommentBegin);
   CommentEnd                      := GetChildTextStr(ATag, 'CommentEnd', CommentEnd);
   InputFunction                   := GetChildTextStr(ATag, 'InputFunction', InputFunction);
   OutputFunction                  := GetChildTextStr(ATag, 'OutputFunction', OutputFunction);
   ProcedureLabelKey               := GetChildTextStr(ATag, 'ProcedureLabelKey', ProcedureLabelKey);
   FunctionLabelKey                := GetChildTextStr(ATag, 'FunctionLabelKey', FunctionLabelKey);
   ConstructorLabelKey             := GetChildTextStr(ATag, 'ConstructorLabelKey', ConstructorLabelKey);
   ProgramLabelKey                 := GetChildTextStr(ATag, 'ProgramLabelKey', ProgramLabelKey);
   GlobalVarsLabelKey              := GetChildTextStr(ATag, 'GlobalVarsLabelKey', GlobalVarsLabelKey);
   GlobalConstsLabelKey            := GetChildTextStr(ATag, 'GlobalConstsLabelKey', GlobalConstsLabelKey);
   HighLighterVarName              := GetChildTextStr(ATag, 'HighLighterVarName', HighLighterVarName);
   ForDoVarString                  := GetChildTextStr(ATag, 'ForDoVarString', ForDoVarString);
   ConstIDSpecChars                := GetChildTextStr(ATag, 'ConstIDSpecChars', ConstIDSpecChars);
   FuncBrackets                    := GetChildTextStr(ATag, 'FuncBrackets', FuncBrackets);
   InstrEnd                        := GetChildTextStr(ATag, 'InstrEnd', InstrEnd);
   FVarTemplate                    := GetChildTextStr(ATag, 'VarTemplate', FVarTemplate);
   FunctionTemplate                := GetChildTextStr(ATag, 'FunctionTemplate', FunctionTemplate);
   FunctionHeaderTemplate          := GetChildTextStr(ATag, 'FunctionHeaderTemplate', FunctionHeaderTemplate);
   FunctionHeaderDescTemplate      := GetChildTextStr(ATag, 'FunctionHeaderDescTemplate', FunctionHeaderDescTemplate);
   FunctionHeaderDescParmMask      := GetChildTextStr(ATag, 'FunctionHeaderDescParmMask', FunctionHeaderDescParmMask);
   FunctionHeaderDescReturnMask    := GetChildTextStr(ATag, 'FunctionHeaderDescReturnMask', FunctionHeaderDescReturnMask);
   ConstructorHeaderTemplate       := GetChildTextStr(ATag, 'ConstructorHeaderTemplate', ConstructorHeaderTemplate);
   PointerTypeMask                 := GetChildTextStr(ATag, 'PointerTypeMask', PointerTypeMask);
   CaseOfValueTemplate             := GetChildTextStr(ATag, 'CaseOfValueTemplate', CaseOfValueTemplate);
   CaseOfFirstValueTemplate        := GetChildTextStr(ATag, 'CaseOfFirstValueTemplate', CaseOfFirstValueTemplate);
   CaseOfDefaultValueTemplate      := GetChildTextStr(ATag, 'CaseOfDefaultValueTemplate', CaseOfDefaultValueTemplate);
   ElseLabel                       := GetChildTextStr(ATag, 'ElseLabel', ElseLabel);
   LabelRepeat                     := GetChildTextStr(ATag, 'LabelRepeat', LabelRepeat);
   LabelWhile                      := GetChildTextStr(ATag, 'LabelWhile', LabelWhile);
   LabelFor                        := GetChildTextStr(ATag, 'LabelFor', LabelFor);
   LabelCase                       := GetChildTextStr(ATag, 'LabelCase', LabelCase);
   LabelIf                         := GetChildTextStr(ATag, 'LabelIf', LabelIf);
   LabelIfElse                     := GetChildTextStr(ATag, 'LabelIfElse', LabelIfElse);
   LabelFuncCall                   := GetChildTextStr(ATag, 'LabelFuncCall', LabelFuncCall);
   LabelText                       := GetChildTextStr(ATag, 'LabelText', LabelText);
   LabelFolder                     := GetChildTextStr(ATag, 'LabelFolder', LabelFolder);
   LabelIn                         := GetChildTextStr(ATag, 'LabelIn', LabelIn);
   LabelOut                        := GetChildTextStr(ATag, 'LabelOut', LabelOut);
   LabelInstr                      := GetChildTextStr(ATag, 'LabelInstr', LabelInstr);
   LabelMultiInstr                 := GetChildTextStr(ATag, 'LabelMultiInstr', LabelMultiInstr);
   RepeatUntilDescTemplate         := GetChildTextStr(ATag, 'RepeatUntilDescTemplate', RepeatUntilDescTemplate);
   ReturnDescTemplate              := GetChildTextStr(ATag, 'ReturnDescTemplate', ReturnDescTemplate);
   ForDoDescTemplate               := GetChildTextStr(ATag, 'ForDoDescTemplate', ForDoDescTemplate);
   CaseOfDescTemplate              := GetChildTextStr(ATag, 'CaseOfDescTemplate', CaseOfDescTemplate);
   MainFunctionTemplate            := GetChildTextStr(ATag, 'MainFunctionTemplate', MainFunctionTemplate);
   ProgramReturnTemplate           := GetChildTextStr(ATag, 'ProgramReturnTemplate', ProgramReturnTemplate);
   FDataTypesTemplate              := GetChildTextStr(ATag, 'DataTypesTemplate', FDataTypesTemplate);
   FFunctionsTemplate              := GetChildTextStr(ATag, 'FunctionsTemplate', FFunctionsTemplate);
   FProgramTemplate                := GetChildTextStr(ATag, PROGRAM_TEMPLATE_TAG, FProgramTemplate);
   DataTypeIntMask                 := GetChildTextStr(ATag, 'DataTypeIntMask', DataTypeIntMask);
   DataTypeRealMask                := GetChildTextStr(ATag, 'DataTypeRealMask', DataTypeRealMask);
   DataTypeOtherMask               := GetChildTextStr(ATag, 'DataTypeOtherMask', DataTypeOtherMask);
   DataTypeRecordTemplate          := GetChildTextStr(ATag, 'DataTypeRecordTemplate', DataTypeRecordTemplate);
   DataTypeEnumTemplate            := GetChildTextStr(ATag, 'DataTypeEnumTemplate', DataTypeEnumTemplate);
   DataTypeEnumEntryList           := GetChildTextStr(ATag, 'DataTypeEnumEntryList', DataTypeEnumEntryList);
   DataTypeArrayMask               := GetChildTextStr(ATag, 'DataTypeArrayMask', DataTypeArrayMask);
   DataTypeUnboundedArrayMask      := GetChildTextStr(ATag, 'DataTypeUnboundedArrayMask', DataTypeUnboundedArrayMask);
   DataTypeRecordFieldMask         := GetChildTextStr(ATag, 'DataTypeRecordFieldMask', DataTypeRecordFieldMask);
   DataTypeRecordFieldArrayMask    := GetChildTextStr(ATag, 'DataTypeRecordFieldArrayMask', DataTypeRecordFieldArrayMask);
   FConstTemplate                  := GetChildTextStr(ATag, 'ConstTemplate', FConstTemplate);
   ConstEntry                      := GetChildTextStr(ATag, 'ConstEntry', ConstEntry);
   ConstEntryArray                 := GetChildTextStr(ATag, 'ConstEntryArray', ConstEntryArray);
   VarEntryInit                    := GetChildTextStr(ATag, 'VarEntryInit', VarEntryInit);
   VarEntryInitExtern              := GetChildTextStr(ATag, 'VarEntryInitExtern', VarEntryInitExtern);
   VarEntry                        := GetChildTextStr(ATag, 'VarEntry', VarEntry);
   VarEntryArray                   := GetChildTextStr(ATag, 'VarEntryArray', VarEntryArray);
   VarEntryArraySize               := GetChildTextStr(ATag, 'VarEntryArraySize', VarEntryArraySize);
   FLibTemplate                    := GetChildTextStr(ATag, 'LibTemplate', FLibTemplate);
   LibEntry                        := GetChildTextStr(ATag, 'LibEntry', LibEntry);
   LibEntryList                    := GetChildTextStr(ATag, 'LibEntryList', LibEntryList);
   FunctionHeaderArgsEntryArray    := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryArray', FunctionHeaderArgsEntryArray);
   FunctionHeaderArgsEntryDefault  := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryDefault', FunctionHeaderArgsEntryDefault);
   FunctionHeaderArgsEntryRef      := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryRef', FunctionHeaderArgsEntryRef);
   FunctionHeaderArgsEntryRecord   := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryRecord', FunctionHeaderArgsEntryRecord);
   FunctionHeaderArgsEntryEnum     := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryEnum', FunctionHeaderArgsEntryEnum);
   FunctionHeaderArgsEntryMask     := GetChildTextStr(ATag, 'FunctionHeaderArgsEntryMask', FunctionHeaderArgsEntryMask);
   FProgramHeaderTemplate          := GetChildTextStr(ATag, 'ProgramHeaderTemplate', FProgramHeaderTemplate);
   DefaultExt                      := GetChildTextStr(ATag, 'DefaultExt', DefaultExt);
   LibraryExt                      := GetChildTextStr(ATag, 'LibraryExt', LibraryExt);
   AssignOperator                  := GetChildTextStr(ATag, 'AssignOperator', AssignOperator);
   UserTypeDesc                    := GetChildTextStr(ATag, 'UserTypeDesc', UserTypeDesc);
   ExternalLabel                   := GetChildTextStr(ATag, 'ExternalLabel', ExternalLabel);
   StaticLabel                     := GetChildTextStr(ATag, 'StaticLabel', StaticLabel);
   RecordLabel                     := GetChildTextStr(ATag, 'RecordLabel', RecordLabel);

   FuncBracketsCursorPos           := GetChildTextInt(ATag, 'FuncBracketsCursorPos', FuncBracketsCursorPos);
   LabelFontSize                   := GetChildTextInt(ATag, 'LabelFontSize', LabelFontSize);
   VarEntryArraySizeStripCount     := GetChildTextInt(ATag, 'VarEntryArraySizeStripCount', VarEntryArraySizeStripCount);
   LibEntryListStripCount          := GetChildTextInt(ATag, 'LibEntryListStripCount', LibEntryListStripCount);
   FunctionHeaderArgsStripCount    := GetChildTextInt(ATag, 'FunctionHeaderArgsStripCount', FunctionHeaderArgsStripCount);
   InOutCursorPos                  := GetChildTextInt(ATag, 'InOutCursorPos', InOutCursorPos);
   DataTypeEnumEntryListStripCount := GetChildTextInt(ATag, 'DataTypeEnumEntryListStripCount', DataTypeEnumEntryListStripCount);

   ForDoVarList                    := GetChildTextBool(ATag, 'ForDoVarList', ForDoVarList);
   EnabledPointers                 := GetChildTextBool(ATag, 'EnabledPointers', EnabledPointers);
   RepeatUntilAsDoWhile            := GetChildTextBool(ATag, 'RepeatUntilAsDoWhile', RepeatUntilAsDoWhile);
   EnabledConsts                   := GetChildTextBool(ATag, 'EnabledConsts', EnabledConsts);
   EnabledVars                     := GetChildTextBool(ATag, 'EnabledVars', EnabledVars);
   EnabledCompiler                 := GetChildTextBool(ATag, 'EnabledCompiler', EnabledCompiler);
   EnabledUserFunctionHeader       := GetChildTextBool(ATag, 'EnabledUserFunctionHeader', EnabledUserFunctionHeader);
   EnabledUserFunctionBody         := GetChildTextBool(ATag, 'EnabledUserFunctionBody', EnabledUserFunctionBody);
   EnabledUserDataTypes            := GetChildTextBool(ATag, 'EnabledUserDataTypes', EnabledUserDataTypes);
   EnabledUserDataTypeInt          := GetChildTextBool(ATag, 'EnabledUserDataTypeInt', EnabledUserDataTypeInt);
   EnabledUserDataTypeReal         := GetChildTextBool(ATag, 'EnabledUserDataTypeReal', EnabledUserDataTypeReal);
   EnabledUserDataTypeOther        := GetChildTextBool(ATag, 'EnabledUserDataTypeOther', EnabledUserDataTypeOther);
   EnabledUserDataTypeArray        := GetChildTextBool(ATag, 'EnabledUserDataTypeArray', EnabledUserDataTypeArray);
   EnabledUserDataTypeEnum         := GetChildTextBool(ATag, 'EnabledUserDataTypeEnum', EnabledUserDataTypeEnum);
   EnabledExplorer                 := GetChildTextBool(ATag, 'EnabledExplorer', EnabledExplorer);
   EnabledCodeGenerator            := GetChildTextBool(ATag, 'EnabledCodeGenerator', EnabledCodeGenerator);
   EnabledMainProgram              := GetChildTextBool(ATag, 'EnabledMainProgram', EnabledMainProgram);
   CaseSensitiveSyntax             := GetChildTextBool(ATag, 'CaseSensitiveSyntax', CaseSensitiveSyntax);
   UpperCaseConstId                := GetChildTextBool(ATag, 'UpperCaseConstId', UpperCaseConstId);
   AllowEnumsInForLoop             := GetChildTextBool(ATag, 'AllowEnumsInForLoop', AllowEnumsInForLoop);
   AllowUserFunctionOverload       := GetChildTextBool(ATag, 'AllowUserFunctionOverload', AllowUserFunctionOverload);
   AllowUnboundedArrays            := GetChildTextBool(ATag, 'AllowUnboundedArrays', AllowUnboundedArrays);
   AllowDuplicatedLibs             := GetChildTextBool(ATag, 'AllowDuplicatedLibs', AllowDuplicatedLibs);
   AllowTransExternVarConst        := GetChildTextBool(ATag, 'AllowTransExternVarConst', AllowTransExternVarConst);
   AllowTransExternFunction        := GetChildTextBool(ATag, 'AllowTransExternFunction', AllowTransExternFunction);
   AllowTransExternDataType        := GetChildTextBool(ATag, 'AllowTransExternDataType', AllowTransExternDataType);
   CodeIncludeExternVarConst       := GetChildTextBool(ATag, 'CodeIncludeExternVarConst', CodeIncludeExternVarConst);
   CodeIncludeExternDataType       := GetChildTextBool(ATag, 'CodeIncludeExternDataType', CodeIncludeExternDataType);
   CodeIncludeExternFunction       := GetChildTextBool(ATag, 'CodeIncludeExternFunction', CodeIncludeExternFunction);

   var tag := FindChildTag(ATag, 'NativeDataTypes');
   if tag <> nil then
   begin
      var datatypeNodes := tag.SelectNodes('DataType');
      var dnode := datatypeNodes.NextNode;
      while dnode <> nil do
      begin
         var originalType: PNativeDataType := nil;
         name := Trim(dnode.Text);
         if name.IsEmpty then
            goto skip;
         var kind := TRttiEnumerationType.GetValue<TDataTypeKind>('tp' + GetNodeAttrStr(dnode, 'kind', 'Other'));
         if Ord(kind) < 0 then
            kind := tpOther;
         if kind = tpPtr then
         begin
            if not EnabledPointers then
               goto skip;
            var val := GetNodeAttrStr(dnode, 'origtype', '').Trim;
            for var t in NativeDataTypes do
            begin
               if SameText(val, t.Name) then
               begin
                  originalType := t;
                  break;
               end;
            end;
         end;
         var dataType := New(PNativeDataType);
         dataType.Name := name;
         dataType.Kind := kind;
         if originalType = nil then
            originalType := dataType;
         dataType.OriginalType := originalType;
         dataType.IsGeneric := GetNodeAttrBool(dnode, 'generic', False);
         dataType.Lib := GetNodeAttrStr(dnode, 'library', '');
         NativeDataTypes := NativeDataTypes + [dataType];
         skip:
         dnode := datatypeNodes.NextNode;
      end;
   end;
   tag := FindChildTag(ATag, 'KeyWords');
   if tag <> nil then
   begin
      KeyWords.Sorted := False;
      KeyWords.CaseSensitive := CaseSensitiveSyntax;
      GetNodesText(tag, 'KeyWord', KeyWords);
      KeyWords.Sort;
   end;
   tag := FindChildTag(ATag, 'NativeFunctions');
   if tag <> nil then
   begin
      var functionNodes := tag.SelectNodes('Function');
      var fnode := functionNodes.NextNode;
      while fnode <> nil do
      begin
         name := Trim(fnode.Text);
         if not name.IsEmpty then
         begin
            var nativeFunction := New(PNativeFunction);
            nativeFunction.Name := name;
            nativeFunction.Brackets := GetNodeAttrStr(fnode, 'brackets', '');
            nativeFunction.BracketsCursorPos := GetNodeAttrInt(fnode, 'bracketsCursorPos', 0);
            nativeFunction.Caption := GetNodeAttrStr(fnode, 'caption', '').Trim;
            nativeFunction.Hint := GetNodeAttrStr(fnode, 'hint', '').Trim;
            nativeFunction.Lib := GetNodeAttrStr(fnode, 'library', '').Trim;
            NativeFunctions := NativeFunctions + [nativeFunction];
         end;
         fnode := functionNodes.NextNode;
      end;
   end;
{$IFDEF USE_CODEFOLDING}
   tag := FindChildTag(ATag, 'FoldRegions');
   if tag <> nil then
   begin
      var foldRegionNodes := tag.SelectNodes('FoldRegion');
      var rnode := foldRegionNodes.NextNode;
      while rnode <> nil do
      begin
         var foldRegion := New(PFoldRegion);
         foldRegion.Open := GetNodeAttrStr(FindChildTag(rnode, 'Open'), 'Keyword', '');
         foldRegion.Close := GetNodeAttrStr(FindChildTag(rnode, 'Close'), 'Keyword', '');
         foldRegion.AddClose := GetNodeAttrBool(rnode, 'AddClose', False);
         foldRegion.NoSubFolds := GetNodeAttrBool(rnode, 'NoSubFolds', True);
         foldRegion.WholeWords := GetNodeAttrBool(rnode, 'WholeWords', True);
         if GetNodeAttrStr(rnode, 'Type', '') = 'rtChar' then
            foldRegion.RegionType := rtChar
         else
            foldRegion.RegionType := rtKeyword;
         FoldRegions := FoldRegions + [foldRegion];
         rnode := foldRegionNodes.NextNode;
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
      for var dim in ASizeEdit.GetDimensions do
         result := result + Format(VarEntryArraySize, [dim]);
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
      BlockTemplates[blockType] := trnsManager.GetString(BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
end;

procedure TLangDefinition.ImportBlockTemplates(ATag: IXMLDOMElement);
begin
   for var blockType := Low(TBlockType) to High(TBlockType) do
      BlockTemplates[blockType] := GetChildTextStr(ATag, BLOCK_TO_TEMPLATE_TAG_MAP[blockType]);
end;

end.
