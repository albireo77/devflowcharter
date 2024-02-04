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

          _BeforeProgramGenerator
          _AfterProgramGenerator
          _ProgramHeaderSectionGenerator
          _LibSectionGenerator
          _UserDataTypeGenerator
          _UserDataTypesSectionGenerator
          _VarSectionGenerator
          _ConstSectionGenerator
          _UserFunctionGenerator
          _UserFunctionsSectionGenerator
          _MainFunctionSectionGenerator
          _ProgramGenerator
          _GetUserFuncDesc
          _GetUserFuncHeaderDesc
          _GetUserTypeDesc
          _SetHLighterAttrs
          _GetPointerTypeName
          _GetConstantType
          _IsPointerType
          _GetOriginalType
          _AreTypesCompatible
          _SkipFuncBodyGen
          _GetMainProgramDesc

       If any generation method is not needed, it may not be implemented at all.
   2d. Add language identifier string constant (e.g. PYTHON_LANG_ID) to Constants unit.
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
   System.SysUtils, System.StrUtils, System.Classes, Vcl.Graphics, Vcl.StdCtrls, SynHighlighterPas,
   Pascal_Parser, Main_Block, Infrastructure, DeclareList, Settings, LocalizationManager,
   LangDefinition, CommonTypes, ParserHelper, YaccLib, Constants;

var
   pascalLang: TLangDefinition;

procedure Pascal_BeforeProgramGenerator;
begin
   // execute parse to set _flag variables
   rand_flag := 0;
   if GProject <> nil then
      GProject.RefreshStatements;
end;

procedure Pascal_ProgramHeaderSectionGenerator(ALines: TStringList);
begin
   GInfra.TemplateLang.ProgramHeaderSectionGenerator(ALines);
   var progName := GProject.Name;
   if progName.IsEmpty then
      progName := i18Manager.GetString('Unknown');
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
   buf, varSize, varInit, line, varName, varType, currType: string;
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
            varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
            varType :=  AVarList.sgList.Cells[VAR_TYPE_COL, i];
            dCount := AVarList.GetDimensionCount(varName);
            if (dCount < 0) or (AVarList.GetExternalState(i) = cbChecked) then
               continue;
            if varType = currType then
            begin
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
                           if dims[b].IsEmpty then
                              varSize := varSize + 'array of '
                           else
                              varSize := varSize + '1..' + dims[b] + ', ';
                        end;
                        if varSize.EndsWith(', ') then
                        begin
                           SetLength(varSize, varSize.Length-2);
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
begin
   if GProject <> nil then
   begin
      var block := GProject.GetMainBlock;
      if block <> nil then
      begin
         var idx := ALines.Count;
         block.GenerateCode(ALines, pascalLang.Name, deep);
         if rand_flag <> 0 then
            ALines.Insert(idx+1, GSettings.IndentString(deep+1) + 'Randomize;');
      end
      else
      begin
         ALines.Add('implementation');
         ALines.Add('');
         if Assigned(GInfra.TemplateLang.UserFunctionsSectionGenerator) then
            GInfra.TemplateLang.UserFunctionsSectionGenerator(ALines, False);
         ALines.Add('end.');
         ALines.Add('');
      end;
   end;
end;

procedure Pascal_SetHLighterAttrs;
begin
   if (pascalLang <> nil) and (pascalLang.HighLighter is TSynPasSyn) then
   begin
      var bkgColor := GSettings.EditorBkgColor;
      var pascalHighlighter := TSynPasSyn(pascalLang.HighLighter);
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

function Pascal_GetConstantType(const AValue: string; var AGenericType: string): integer;
var
   i, len: integer;
   f: double;
   b: boolean;
begin
   result := UNKNOWN_TYPE;
   AGenericType := '';
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
      result := Copy(result, 2);
end;

function Pascal_AreTypesCompatible(AType1, AType2: integer): boolean;
begin
   result := (AType1 = PASCAL_STRING_TYPE) and (AType2 = PASCAL_CHAR_TYPE);
end;

function Pascal_Parse(const AText: string; AParserMode: TYYMode): boolean;
begin
   result := True;
   if (pascalLang <> nil) and (pascalLang.Parser is TPascalParser) then
   begin
      pascalLang.Parser.Reset;
      pascalLang.Parser.ylex.Reset;
      pascalLang.Parser.ylex.yyinput.AssignString(AText);
      pascalLang.Parser.yymode := AParserMode;
      result := TPascalParser(pascalLang.Parser).yyparse = 0;
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

   pascalLang := GInfra.GetLangDefinition(PASCAL_LANG_ID);
   if pascalLang <> nil then
   begin
      pascalLang.Parser := TPascalParser.Create;
      pascalLang.BeforeProgramGenerator :=  Pascal_BeforeProgramGenerator;
      pascalLang.ProgramHeaderSectionGenerator := Pascal_ProgramHeaderSectionGenerator;
      pascalLang.VarSectionGenerator := Pascal_VarSectionGenerator;
      pascalLang.MainFunctionSectionGenerator := Pascal_MainFunctionSectionGenerator;
      pascalLang.SetHLighterAttrs := Pascal_SetHLighterAttrs;
      pascalLang.GetConstantType := Pascal_GetConstantType;
      pascalLang.IsPointerType := Pascal_IsPointerType;
      pascalLang.AreTypesCompatible := Pascal_AreTypesCompatible;
      pascalLang.GetOriginalType := Pascal_GetOriginalType;
      pascalLang.Parse := Pascal_Parse;
      pascalLang.SkipFuncBodyGen := Pascal_SkipFuncBodyGen;
   end;

end.
