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

This unit contains stuff to support Pascal language on code level. Templates for other languages should follow this unit.

In general, support for programming language in devFlowcharter can be done on 2 levels:

1. External (required)
   To support language externally, it's enough to add prepared language definition file in LanguageDefinitions directory.
   Refer to LanguageDefinitions/Example.xml for reference.
   Advantage: can be done by application user
   Disadvantage: code generation is very basic with not much control.

2. Internal (optional)
   Also known as embedded. Apart from creating language definition file as described in point 1, developer can create unit similar to this one
   and add it to devFlowcharter Delphi project.
   Advantage: much better code generation limited only by developer invention; possibility to create and use of internal parser
   Disadvantage: can be done only by developer on code level
   To support language internally do the following:
   2a. Add (optionally) highlighter component from SynEdit suite for new language. Put it on SourceEditorForm.
   2b. Create (optionally) parser class for new language using yacc/lex from Parsers\Common directory.
       For hardcore developers only! Parser unit must be added to project.
       In parser unit change parser class name from TYacc (default) to specific one (e.g. TPythonParser).
       Parsers\Common\ParserFunctions.pas contains language independent routines helpful in creating various parsers.
   2c. SaveAs this template unit (e.g. Python_Template.pas), add it to devFlowcharter Delphi project and implement template procedures:
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
       If any template method is not needed, it may not be implemented at all.
   2d. Add language identifier string constant (e.g. PYTHON_LANG_ID) to ApplicationCommon unit.
       It should contain the same value as <Name> tag in language definition XML file.
   2e. Refer to _Block.pas units in Blocks directory. Modify GenerateCode method in _Block classes by adding new section in "if" instruction.
       This part is responsible for generating code inside main section of the program.

}

unit Pascal_Template;

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
   SysUtils, SynHighlighterPas, StrUtils, Pascal_Parser, Main_Block, ApplicationCommon,
   DeclareList, Settings, LocalizationManager, LangDefinition, Classes, CommonTypes, ParserHelper;

var
   lLangDef: TLangDefinition;

procedure Pascal_PreGenerationActivities;
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

   if GProject.Name = '' then
      progName := i18Manager.GetString('Unknown')
   else
      progName := GProject.Name;
   progName := AnsiReplaceStr(progName, ' ', '_') + ';';

   if GProject.GetMainBlock <> nil then
      ALines.Add('program ' + progName)
   else
   begin
      ALines.Add('unit ' + progName);
      ALines.Add('');
      ALines.Add('interface');
   end;
   ALines.Add('');
end;

{procedure Pascal_LibSectionGenerator(ALines: TStringList);
var
   i: integer;
   libList: TStringList;
   unitsString: string;
begin
   libList := GProject.GetLibraryList;
   try
      if libList.Count > 0 then
      begin
         unitsString := '';
         for i := 0 to libList.Count-2 do
            unitsString := unitsString + libList[i] + ',';
         unitsString := unitsString + libList[libList.Count-1] + ';';
         unitsString := AnsiReplaceStr(unitsString, ',', ', ');
         ALines.Add('uses');
         ALines.Add(GSettings.IndentString + unitsString);
         ALines.Add('');
      end;
   finally
      libList.Free;
   end;
end;}

procedure Pascal_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   bufor, sizeString, lInit, lLine, lName, lType, lCurrentType: string;
   i, a, b, dimensCount, lCount: integer;
begin
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) and (GProject <> nil) and (GProject.GlobalVars <> nil) then
   begin
      lCount := 0;
      for a := 0 to GProject.GlobalVars.cbType.Items.Count-1 do
      begin
         bufor := '';
         lCurrentType := GProject.GlobalVars.cbType.Items[a];
         for i := 1 to AVarList.sgList.RowCount-2 do
         begin
            if AVarList.IsExternal(i) then
               continue;
            lName := AVarList.sgList.Cells[VAR_NAME_COL, i];
            lType := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            if lType = lCurrentType then
            begin
               lInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
               dimensCount := AVarList.GetDimensionCount(lName);
               if (dimensCount = 0) and (lInit = '') then
               begin
                  if bufor = '' then
                     bufor := lName
                  else
                     bufor := bufor + ', ' + lName;
               end
               else
               begin
                  lLine := GSettings.IndentString + lName + ': ';
                  if dimensCount > 0 then
                  begin
                     sizeString := '';
                     for b := 1 to dimensCount do
                     begin
                        sizeString := sizeString + '1..' + AVarList.GetDimension(lName, b);
                        if b < dimensCount then
                           sizeString := sizeString + ', ';
                     end;
                     lLine := lLine + 'array[' + sizeString + '] of ';
                  end;
                  lLine := lLine + lType;
                  if lInit <> '' then
                     lLine := lLine + ' = ' + lInit;
                  if lCount = 0 then
                  begin
                     ALines.Add('var');
                     lCount := 1;
                  end;
                  ALines.AddObject(lLine + ';', AVarList);
               end;
            end;
         end;
         if bufor <> '' then
         begin
            if lCount = 0 then
            begin
               ALines.Add('var');
               lCount := 1;
            end;
            ALines.AddObject(GSettings.IndentString + bufor + ': ' + lCurrentType + ';', AVarList);
         end;
      end;
   end;

end;

{procedure Pascal_ConstSectionGenerator(ALines: TStrings; AConstList: TConstDeclareList);
var
   i, linesCount: integer;
begin
   if (AConstList <> nil) and (AConstList.sgList.RowCount > 2) then
   begin
      linesCount := ALines.Count;
      for i := 1 to AConstList.sgList.RowCount-2 do
      begin
         if not AConstList.IsExternal(i) then
            ALines.Add(GSettings.IndentString + AConstList.sgList.Cells[0, i] + ' = ' + AConstList.sgList.Cells[1, i] + ';');
      end;
      if ALines.Count > linesCount then
         ALines.Insert(linesCount, 'const');
   end;
end;

procedure Pascal_RoutineSectionGenerator(ALines: TStrings; ASkipBodyGen: boolean);
var
   lRoutine: TRoutine;
   lParm: TParameter;
   lHeaderStr, lName: string;
   iter: TRoutineIterator;
   iterp: TParameterIterator;
   lObject: TObject;
   lIterCnt, lCnt: integer;
begin
   iter := GProject.GetRoutineIterator;
   try
      while iter.HasNext do
      begin
         lRoutine := iter.Next;
         lName := lRoutine.GetName;
         if (lName = '') or lRoutine.Header.chkExtDeclare.Checked then continue;
         if not ASkipBodyGen then
            lRoutine.Header.GenerateDescription(ALines);
         if lRoutine.Header.cbType.ItemIndex <> 0 then
            lHeaderStr := 'function '
         else
            lHeaderStr := 'procedure ';
         lHeaderStr := lHeaderStr + lName;
         lIterCnt := lRoutine.Header.ParameterCount;
         if lIterCnt > 0 then
         begin
            iterp := lRoutine.Header.GetParameterIterator;
            try
               lHeaderStr := lHeaderStr + '(';
               lCnt := 0;
               while iterp.HasNext do
               begin
                  lCnt := lCnt + 1;
                  lParm := iterp.Next;
                  if lParm.chkReference.Checked then
                     lHeaderStr := lHeaderStr + 'var ';
                  lHeaderStr := lHeaderStr + Trim(lParm.edtName.Text) + ': ';
                  if lParm.chkTable.Checked then
                     lHeaderStr := lHeaderStr + 'array of ';
                  lHeaderStr := lHeaderStr + lParm.cbType.Text;
                  if lIterCnt <> lCnt then
                     lHeaderStr := lHeaderStr + '; ';
               end;
               lHeaderStr := lHeaderStr + ')';
            finally
               iterp.Free;
            end;
         end;
         if lRoutine.Header.cbType.ItemIndex <> 0 then
            lHeaderStr := lHeaderStr + ': ' + lRoutine.Header.cbType.Text;
         if ASkipBodyGen then
            lObject := nil
         else
            lObject := lRoutine.Header;
         ALines.AddObject(lHeaderStr + ';', lObject);
         if not ASkipBodyGen and (lRoutine.Body <> nil) then
         begin
            Pascal_VarSectionGenerator(ALines, lRoutine.Header.LocalVars);
            lRoutine.Body.GenerateCode(ALines, lLangDef.Name, 0);
         end;
      end;
      if ASkipBodyGen then
         ALines.Add('');
   finally
      iter.Free;
   end;
end;

function Pascal_GetRoutineDescription(AHeader: TRoutineHeader): string;
var
   lname: string;
begin
   if AHeader <> nil then
   begin
      lname := Trim(AHeader.edtName.Text);
      if AHeader.cbType.ItemIndex <> 0 then
         result := i18Manager.GetString('Function') + ' ' + lname + ': ' + AHeader.cbType.Text
      else
         result := i18Manager.GetString('Procedure') + ' ' + lname;
   end
   else
      result := i18Manager.GetString('MainProgram');
end;}

procedure Pascal_MainProgramSectionGenerator(ALines: TStringList; deep: integer);
var
   lBlock: TMainBlock;
   idx: integer;
begin
   if GProject <> nil then
   begin
      lBlock := GProject.GetMainBlock;
      if lBlock <> nil then
      begin
          idx := ALines.Count;
         lBlock.GenerateCode(ALines, lLangDef.Name, deep);
         if rand_flag = 1 then
            ALines.Insert(idx+1, DupeString(GSettings.IndentString, deep+1) + 'Randomize;');
      end
      else
      begin
         ALines.Add('');
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
begin
   if (lLangDef <> nil) and (lLangDef.HighLighter is TSynPasSyn) then
   begin
      pascalHighlighter := TSynPasSyn(lLangDef.HighLighter);
      pascalHighlighter.StringAttri.Foreground    := GSettings.EditorStringColor;
      pascalHighlighter.NumberAttri.Foreground    := GSettings.EditorNumberColor;
      pascalHighlighter.FloatAttri.Foreground     := GSettings.EditorNumberColor;
      pascalHighlighter.HexAttri.Foreground       := GSettings.EditorNumberColor;
      pascalHighlighter.CommentAttri.Foreground   := GSettings.EditorCommentColor;
      pascalHighlighter.DirectiveAttri.Foreground := GSettings.EditorCommentColor;
      pascalHighlighter.CharAttri.Foreground      := GSettings.EditorStringColor;
      pascalHighlighter.StringAttri.Background    := GSettings.EditorBkgColor;
      pascalHighlighter.NumberAttri.Background    := GSettings.EditorBkgColor;
      pascalHighlighter.FloatAttri.Background     := GSettings.EditorBkgColor;
      pascalHighlighter.HexAttri.Background       := GSettings.EditorBkgColor;
      pascalHighlighter.CommentAttri.Background   := GSettings.EditorBkgColor;
      pascalHighlighter.DirectiveAttri.Background := GSettings.EditorBkgColor;
      pascalHighlighter.CharAttri.Background      := GSettings.EditorBkgColor;
   end;
end;

function Pascal_GetLiteralType(const AValue: string): integer;
var
   i: integer;
   f: double;
   b: boolean;
begin
   result := UNKNOWN_TYPE;
   if AValue <> '' then
   begin
      if not TryStrToInt(AValue, i) then
      begin
         if not TryStrToFloat(AValue, f) then
         begin
            if not TryStrToBool(AValue, b) then
            begin
               if (AValue[1] = PASCAL_STRING_DELIM) and (AnsiLastChar(AValue) = PASCAL_STRING_DELIM) then
               begin
                  if Length(AValue) = 3 then
                     result := PASCAL_CHAR_TYPE
                  else
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

{procedure Pascal_TypeSectionGenerator(lines: TStrings);
var
   lDataType: TDataType;
   currentField: TField;
   buffer, sizeString, lName: string;
   iter: TDataTypeIterator;
   iterf: TFieldIterator;
   linesCount, dimensCount, lCnt, b: integer;
begin
   linesCount := lines.Count;
   iter := GProject.GetDataTypeIterator;
   try
      while iter.HasNext do
      begin
         lDataType := iter.Next;
         lName := lDataType.GetName;
         if (lName <> '') and not lDataType.rbInt.Checked and not lDataType.rbReal.Checked and not lDataType.chkExtDeclare.Checked then
         begin
            buffer := '';
            b := 0;
            if lDataType.rbStruct.Checked then
               lines.AddObject(GSettings.IndentString + lName + ' = record', lDataType)
            else
            begin
               buffer := GSettings.IndentString + lName + ' = ';
               if lDataType.rbEnum.Checked then
                  buffer := buffer + '(';
            end;
            iterf := lDataType.GetFieldIterator;
            lCnt := iterf.Count;
            try
               while iterf.HasNext do
               begin
                  currentField := iterf.Next;
                  dimensCount := currentField.edtSize.DimensionCount;
                  if lDataType.rbStruct.Checked then
                  begin
                     buffer := DupeString(GSettings.IndentString, 2) + currentField.edtName.Text + ': ';
                     if dimensCount > 0 then
                     begin
                        sizeString := '';
                        for b := 1 to dimensCount do
                        begin
                           sizeString := sizeString + '1..' + currentField.edtSize.GetDimension(b);
                           if b < dimensCount then
                              sizeString := sizeString + ', ';
                        end;
                        buffer := buffer + 'array[' + sizeString + '] of ';
                     end;
                     buffer := buffer + currentField.cbType.Text + ';';
                     lines.Add(buffer);
                  end
                  else if lDataType.rbEnum.Checked then
                  begin
                     b := b + 1;
                     buffer := buffer + currentField.edtName.Text;
                     if b <> lCnt then
                        buffer := buffer + ', ';
                  end
                  else if lDatatype.rbArray.Checked then
                  begin
                     buffer := buffer + 'array[';
                     for b := 1 to dimensCount do
                     begin
                        buffer := buffer + '1..' + currentField.edtSize.GetDimension(b);
                        if b < dimensCount then
                           buffer := buffer + ', ';
                     end;
                     buffer := buffer + '] of ' + currentField.cbType.Text;
                     break;
                  end
                  else
                  begin
                     buffer := buffer + currentField.edtName.Text;
                     break;
                  end;
               end;
            finally
               iterf.Free;
            end;
            if lDataType.rbStruct.Checked then
               buffer := GSettings.IndentString + 'end'
            else if lDataType.rbEnum.Checked then
               buffer := buffer + ')';
            lines.AddObject(buffer + ';', lDataType);
            lines.Add('');
         end;
      end;
   finally
      iter.Free;
   end;
   if lines.Count > linesCount then
      lines.Insert(linesCount, 'type');
end;

function Pascal_GetPointerTypeName(const AName: string): string;
begin
   result := '^' + AName;
end;}

function Pascal_GetOriginalType(const APtrType: string): string;
begin
   if (APtrType <> '') and (APtrType[1] = '^') then
      result := AnsiRightStr(APtrType, Length(APtrType)-1)
   else
      result := APtrType;
end;

function Pascal_IsPointerType(const AName: string): boolean;
begin
   result := (AName <> '') and (AName[1] = '^');
end;

function Pascal_AreTypesCompatible(const AType1, AType2: integer): boolean;
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
      lLangDef.PreGenerationActivities :=  Pascal_PreGenerationActivities;
      lLangDef.ProgramHeaderSectionGenerator := Pascal_ProgramHeaderSectionGenerator;
      //lLangDef.LibSectionGenerator := Pascal_LibSectionGenerator;
      //lLangDef.TypeSectionGenerator := Pascal_TypeSectionGenerator;
      lLangDef.VarSectionGenerator := Pascal_VarSectionGenerator;
      //lLangDef.ConstSectionGenerator := Pascal_ConstSectionGenerator;
      //lLangDef.RoutineSectionGenerator := Pascal_RoutineSectionGenerator;
      //lLangDef.GetRoutineDescription := Pascal_GetRoutineDescription;
      lLangDef.MainProgramSectionGenerator := Pascal_MainProgramSectionGenerator;
      lLangDef.SetHLighterAttrs := Pascal_SetHLighterAttrs;
      lLangDef.GetLiteralType := Pascal_GetLiteralType;
      //lLangDef.GetPointerTypeName := Pascal_GetPointerTypeName;
      lLangDef.IsPointerType := Pascal_IsPointerType;
      lLangDef.AreTypesCompatible := Pascal_AreTypesCompatible;
      lLangDef.GetOriginalType := Pascal_GetOriginalType;
      lLangDef.Parse := Pascal_Parse;
      lLangDef.SkipFuncBodyGen := Pascal_SkipFuncBodyGen;
   end;

end.