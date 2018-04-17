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



{

This unit contains stuff to support Pascal code generation. Generators for other languages should follow this unit.

In general, support for programming language in devFlowcharter can be done on 2 levels:

1. External (required)
   To support language externally, it's enough to add prepared language definition file in LanguageDefinitions directory.
   Refer to LanguageDefinitions/Example.xml for reference.
   Advantage: can be done by application user
   Disadvantage: code generation is very basic with not much control.

2. Internal (optional)
   Apart from creating language definition file as described in point 1, developer can create unit similar to this one
   and add it to devFlowcharter Delphi project.
   Advantage: much better code generation limited only by developer invention; possibility to create and use of internal parser
   Disadvantage: can be done only by developer on code level
   To support language internally do the following:
   2a. Add (optionally) highlighter component from SynEdit suite for new language. Put it on EditorForm.
   2b. Create (optionally) parser class for new language using yacc/lex from Parsers\Common directory.
       For hardcore developers only! Parser unit must be added to project.
       In parser unit change parser class name from TYacc (default) to specific one (e.g. TPythonParser).
       Parsers\Common\ParserFunctions.pas contains language independent routines helpful in creating various parsers.
   2c. SaveAs this template unit (e.g. Python_Generator.pas), add it to devFlowcharter Delphi project and implement generator procedures:
          _PreGenerationActivities
          _ProgramHeaderSectionGenerator
          _RoutineSectionGenerator
          _MainProgramSectionGenerator
          _TypeSectionGenerator
          _VarSectionGenerator
          _ConstSectionGenerator
          _LibSectionGenerator
          _GetUserTypeDesc
          _GetUserFuncDesc
          _SetHighlighterAttributes   (should be implemented only if SynEdit highlighter component exists)
          _GetLiteralType   (function to get datatype of given literal; Example: _GetLiteralType('6.5e-2') = PASCAL_REAL_TYPE)
          _GetPointerTypeName  (function to obtain pointer datatype for given datatype; Example: _GetPointerTypeName('integer') = '^integer')
          _IsPointerType    (function to verify if given datatype is of pointer type; Example: _IsPointerType('^integer') = true)
          _GetOriginalType    (function to obtain original datatype from given pointer data type; Example: _GetOriginalType('^integer') = 'integer')
          _Parse (function to parse expressions with internal parser)
          _GetHLighterVarName (function to obtain variable name of highlighter component from point 2a)
       If any generation method is not needed, it may not be implemented at all.
   2d. Add language identifier string constant (e.g. PYTHON_LANG_ID) to ApplicationCommon unit.
       It should contain the same value as <Name> tag in language definition XML file.
   2e. Refer to _Block.pas units in Blocks directory. Modify GenerateCode method in _Block classes by adding new "if" section.
       This part is responsible for generating code inside main section of the program.

}

unit Pascal_Generator;

interface

const
        PASCAL_STRING_DELIM = #39;
var
        // flag to check if random function is used
        rand_flag: byte = 0;
        // datatype indexes used by internal parser
        PASCAL_INT_TYPE,
        PASCAL_REAL_TYPE,
        PASCAL_STRING_TYPE,
        PASCAL_BOOL_TYPE,
        PASCAL_CHAR_TYPE,
        PASCAL_INT_PTR_TYPE,
        PASCAL_REAL_PTR_TYPE,
        PASCAL_STRING_PTR_TYPE,
        PASCAL_TEXT_FILE_TYPE: integer;

implementation

uses
   System.SysUtils, System.StrUtils, System.Classes, Vcl.Graphics, SynHighlighterPas,
   Pascal_Parser, Main_Block, ApplicationCommon, DeclareList, Settings, LocalizationManager,
   LangDefinition, CommonTypes, ParserHelper;

var
   lLangDef: TLangDefinition;

procedure Pascal_ExecuteBeforeGeneration;
begin
   // execute parse to set _flag variables
   rand_flag := 0;
   if GProject <> nil then
      GProject.RefreshStatements;
end;

procedure Pascal_ProgramHeaderSectionGenerator(ALines: TStringList);
var
   progName: string;
begin

   GInfra.DummyLang.ProgramHeaderSectionGenerator(ALines);

   if GProject.Name.IsEmpty then
      progName := i18Manager.GetString('Unknown')
   else
      progName := GProject.Name;
   progName := ReplaceStr(progName, ' ', '_') + ';';

   if GProject.GetMainBlock <> nil then
      ALines.Add('program ' + progName)
   else
   begin
      ALines.Add('unit ' + progName);
      ALines.Add('');
      ALines.Add('interface');
   end;
end;

procedure Pascal_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   buf, varSize, varInit, line, varName, varType, currType, dim: string;
   i, a, b, dCount, cnt: integer;
   dims: TArray<string>;
begin
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) and (GProject <> nil) and (GProject.GlobalVars <> nil) then
   begin
      cnt := 0;
      for a := 0 to GProject.GlobalVars.cbType.Items.Count-1 do
      begin
         buf := '';
         currType := GProject.GlobalVars.cbType.Items[a];
         for i := 1 to AVarList.sgList.RowCount-2 do
         begin
            varName := AVarList.sgList.Cells[VAR_NAME_COL, i];
            varType := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            dCount := AVarList.GetDimensionCount(varName);
            if (dCount < 0) or AVarList.IsExternal(i) then
               continue;
            if varType = currType then
            begin
               varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
               if (dCount = 0) and varInit.IsEmpty then
               begin
                  if buf.IsEmpty then
                     buf := varName
                  else
                     buf := buf + ', ' + varName;
               end
               else
               begin
                  line := GSettings.IndentString + varName + ': ';
                  if dCount > 0 then
                  begin
                     varSize := '';
                     dims := AVarList.GetDimensions(varName);
                     if dims <> nil then
                     begin
                        for b := 0 to High(dims) do
                        begin
                           dim := dims[b];
                           if dim.IsEmpty then
                              varSize := varSize + 'array of '
                           else
                              varSize := varSize + '1..' + dim + ', ';
                        end;
                        if varSize.EndsWith(', ') then
                        begin
                           SetLength(varSize, Length(varSize)-2);
                           varSize := 'array[' + varSize + '] of ';
                        end;
                     end;
                     line := line + varSize;
                  end;
                  line := line + varType;
                  if not varInit.IsEmpty then
                     line := line + ' = ' + varInit;
                  if cnt = 0 then
                  begin
                     ALines.Add('var');
                     cnt := 1;
                  end;
                  ALines.AddObject(line + ';', AVarList);
               end;
            end;
         end;
         if not buf.IsEmpty then
         begin
            if cnt = 0 then
            begin
               ALines.Add('var');
               cnt := 1;
            end;
            ALines.AddObject(GSettings.IndentString + buf + ': ' + currType + ';', AVarList);
         end;
      end;
   end;

end;

procedure Pascal_MainFunctionSectionGenerator(ALines: TStringList; deep: integer);
var
   block: TMainBlock;
   idx: integer;
begin
   if GProject <> nil then
   begin
      block := GProject.GetMainBlock;
      if block <> nil then
      begin
         idx := ALines.Count;
         block.GenerateCode(ALines, lLangDef.Name, deep);
         if rand_flag <> 0 then
            ALines.Insert(idx+1, DupeString(GSettings.IndentString, deep+1) + 'Randomize;');
      end
      else
      begin
         ALines.Add('implementation');
         ALines.Add('');
         if Assigned(GInfra.DummyLang.UserFunctionsSectionGenerator) then
            GInfra.DummyLang.UserFunctionsSectionGenerator(ALines, false);
         ALines.Add('end.');
         ALines.Add('');
      end;
   end;
end;

procedure Pascal_SetHLighterAttrs;
var
   pascalHighlighter: TSynPasSyn;
   bkgColor: TColor;
begin
   if (lLangDef <> nil) and (lLangDef.HighLighter is TSynPasSyn) then
   begin
      bkgColor := GSettings.EditorBkgColor;
      pascalHighlighter := TSynPasSyn(lLangDef.HighLighter);
      pascalHighlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      pascalHighlighter.StringAttri.Background     := bkgColor;
      pascalHighlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      pascalHighlighter.NumberAttri.Background     := bkgColor;
      pascalHighlighter.FloatAttri.Foreground      := GSettings.EditorNumberColor;
      pascalHighlighter.FloatAttri.Background      := bkgColor;
      pascalHighlighter.HexAttri.Foreground        := GSettings.EditorNumberColor;
      pascalHighlighter.HexAttri.Background        := bkgColor;
      pascalHighlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      pascalHighlighter.CommentAttri.Background    := bkgColor;
      pascalHighlighter.DirectiveAttri.Foreground  := GSettings.EditorCommentColor;
      pascalHighlighter.DirectiveAttri.Background  := bkgColor;
      pascalHighlighter.CharAttri.Foreground       := GSettings.EditorStringColor;
      pascalHighlighter.CharAttri.Background       := bkgColor;
      pascalHighlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      pascalHighlighter.KeyAttri.Background        := bkgColor;
      pascalHighlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      pascalHighlighter.IdentifierAttri.Background := bkgColor;
   end;
end;

function Pascal_GetLiteralType(const AValue: string): integer;
var
   i, len: integer;
   f: double;
   b: boolean;
begin
   result := UNKNOWN_TYPE;
   len := AValue.Length;
   if len > 0 then
   begin
      if not TryStrToInt(AValue, i) then
      begin
         if not TryStrToFloat(AValue, f) then
         begin
            if not TryStrToBool(AValue, b) then
            begin
               if (AValue[1] = PASCAL_STRING_DELIM) and (AValue[len] = PASCAL_STRING_DELIM) then
               begin
                  if len = 3 then
                     result := PASCAL_CHAR_TYPE
                  else if len <> 1 then
                     result := PASCAL_STRING_TYPE;
               end
               else if TInfra.SameStrings(AValue, 'nil') then
                  result := GENERIC_PTR_TYPE;
            end
            else
               result := PASCAL_BOOL_TYPE
         end
         else
            result := PASCAL_REAL_TYPE;
      end
      else
         result := PASCAL_INT_TYPE;
   end;
end;

function Pascal_IsPointerType(const AType: string): boolean;
begin
   result := (AType.Length > 1) and (AType[1] = '^');
end;

function Pascal_GetOriginalType(const AType: string): string;
begin
   result := AType;
   if Pascal_IsPointerType(result) then
      result := Copy(result, 2, MAXINT);
end;

function Pascal_AreTypesCompatible(AType1, AType2: integer): boolean;
begin
   result := (AType1 = PASCAL_STRING_TYPE) and (AType2 = PASCAL_CHAR_TYPE);
end;

function Pascal_Parse(const AText: string; const AParserMode: TParserMode): integer;
begin
   result := 0;
   if (lLangDef <> nil) and (lLangDef.Parser is TPascalParser) then
   begin
      lLangDef.Parser.ylex.Reset;
      lLangDef.Parser.ylex.yyinput.AssignString(AText);
      GParser_Mode := AParserMode;
      result := TPascalParser(lLangDef.Parser).yyparse;
   end;
end;

function Pascal_SkipFuncBodyGen: boolean;
begin
   result := (GProject <> nil) and (GProject.GetMainBlock = nil);
end;

initialization

   PASCAL_INT_TYPE        := TParserHelper.GetType('integer', PASCAL_LANG_ID);
   PASCAL_REAL_TYPE       := TParserHelper.GetType('real', PASCAL_LANG_ID);
   PASCAL_STRING_TYPE     := TParserHelper.GetType('string', PASCAL_LANG_ID);
   PASCAL_BOOL_TYPE       := TParserHelper.GetType('boolean', PASCAL_LANG_ID);
   PASCAL_CHAR_TYPE       := TParserHelper.GetType('char', PASCAL_LANG_ID);
   PASCAL_INT_PTR_TYPE    := TParserHelper.GetType('^integer', PASCAL_LANG_ID);
   PASCAL_REAL_PTR_TYPE   := TParserHelper.GetType('^real', PASCAL_LANG_ID);
   PASCAL_STRING_PTR_TYPE := TParserHelper.GetType('^string', PASCAL_LANG_ID);
   PASCAL_TEXT_FILE_TYPE  := TParserHelper.GetType('text', PASCAL_LANG_ID);

   lLangDef := GInfra.GetLangDefinition(PASCAL_LANG_ID);
   if lLangDef <> nil then
   begin
      lLangDef.Parser := TPascalParser.Create;
      lLangDef.ExecuteBeforeGeneration :=  Pascal_ExecuteBeforeGeneration;
      lLangDef.ProgramHeaderSectionGenerator := Pascal_ProgramHeaderSectionGenerator;
      lLangDef.VarSectionGenerator := Pascal_VarSectionGenerator;
      lLangDef.MainFunctionSectionGenerator := Pascal_MainFunctionSectionGenerator;
      lLangDef.SetHLighterAttrs := Pascal_SetHLighterAttrs;
      lLangDef.GetLiteralType := Pascal_GetLiteralType;
      lLangDef.IsPointerType := Pascal_IsPointerType;
      lLangDef.AreTypesCompatible := Pascal_AreTypesCompatible;
      lLangDef.GetOriginalType := Pascal_GetOriginalType;
      lLangDef.Parse := Pascal_Parse;
      lLangDef.SkipFuncBodyGen := Pascal_SkipFuncBodyGen;
   end;

end.