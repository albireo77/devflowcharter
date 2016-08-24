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


 
{ This unit contains stuff to support C language }

unit C_Template;

interface

const
        C_STRING_DELIM = #34;
        C_CHAR_DELIM   = #39;
var
        // flags to include header files
        io_flag,          // stdio.h
        math_flag,        // math.h
        str_flag: byte;   // string.h
        // datatype indexes used to evaluate expression type
        C_INT_TYPE,
        C_REAL_TYPE,
        C_CHAR_TYPE,
        C_CHAR_PTR_TYPE: integer;

implementation

uses
   SysUtils, SynHighlighterCpp, StrUtils, Main_Block, ApplicationCommon, LangDefinition,
   Classes, ParserHelper;

var
   lLangDef: TLangDefinition;

procedure C_PreGenerationActivities;
begin
   // execute parse to set _flag variables
   io_flag   := 0;
   math_flag := 0;
   str_flag  := 0;
   if GProject <> nil then
      GProject.RefreshStatements;
end;

procedure C_LibSectionGenerator(ALines: TStringList);
var
   libList: TStringList;
   i: integer;
   lIncludeStr: string;
   lIsQuoted: boolean;
begin
   libList := GProject.GetLibraryList;
   try
      if math_flag = 1 then libList.Add('math');
      if str_flag = 1 then libList.Add('string');
      if io_flag = 1 then libList.Add('stdio');
      if libList.Count > 0 then
      begin
         for i := 0 to libList.Count-1 do
         begin
            lIncludeStr := '#include ';
            lIsQuoted := AnsiPos('"', libList[i]) <> 0;
            if not lIsQuoted then
               lIncludeStr := lIncludeStr + '<';
            lIncludeStr := lIncludeStr + libList[i];
            if AnsiPos('.', libList[i]) = 0 then
               lIncludeStr := lIncludeStr + lLangDef.LibraryExt;
            if not lIsQuoted then
               lIncludeStr := lIncludeStr + '>';
            ALines.Add(lIncludeStr);
         end;
         ALines.Add('');
      end;
   finally
      libList.Free;
   end;
end;

{procedure C_TypeSectionGenerator(ALines: TStrings);
var
   lDataType: TDataType;
   currentField: TField;
   buffer, lName: string;
   iter: TDataTypeIterator;
   iterf: TFieldIterator;
   dimensCount, b: integer;
begin
   iter := GProject.GetDataTypeIterator;
   try
      while iter.HasNext do
      begin
         lDataType := iter.Next;
         lName := lDataType.GetName;
         if (lName <> '') and not lDataType.rbInt.Checked and not lDataType.rbReal.Checked and not lDataType.chkExtDeclare.Checked then
         begin
            buffer := '';
            if lDataType.rbStruct.Checked then
            begin
               ALines.AddObject('struct ' + lName, lDataType);
               ALines.Add('{');
            end;
            iterf := lDataType.GetFieldIterator;
            try
               while iterf.HasNext do
               begin
                  currentField := iterf.Next;
                  dimensCount := currentField.edtSize.DimensionCount;
                  if lDataType.rbStruct.Checked then
                  begin
                     buffer := GSettings.IndentString;
                     if IsStructType(GetType(currentField.cbType.Text)) then
                        buffer := buffer + 'struct ';
                     buffer := buffer + currentField.cbType.Text + ' ' + currentField.edtName.Text;
                     for b := 1 to dimensCount do
                        buffer := buffer + '[' + currentField.edtSize.GetDimension(b) + ']';
                     ALines.Add(buffer + ';');
                  end
                  else if lDataType.rbEnum.Checked then
                     buffer := buffer + currentField.edtName.Text + ', '
                  else if lDataType.rbArray.Checked then
                  begin
                     buffer := 'typedef ' + currentField.cbType.Text + ' ' + lName;
                     for b := 1 to dimensCount do
                        buffer := buffer + '[' + currentField.edtSize.GetDimension(b) + ']';
                     buffer := buffer + ';';
                     break;
                  end
                  else
                  begin
                     buffer := 'typedef ' + currentField.edtName.Text + ' ' + lName + ';';
                     break;
                  end;
               end;
            finally
               iterf.Free;
            end;
            if lDataType.rbStruct.Checked then}
            //   buffer := '};'
            //else if lDataType.rbEnum.Checked then
            //   buffer := 'enum ' + lDataType.edtName.Text + ' {' + AnsiLeftStr(buffer, Length(buffer)-2) + '};';
{            ALines.AddObject(buffer, lDataType);
            ALines.Add('');
         end;
      end;
   finally
      iter.Free;
   end;
end;

procedure C_VarSectionGenerator(ALines: TStrings; AVarList: TVarDeclareList);
var
   i, dimensCount, b, lType: integer;
   buffer: string;
   lInit: string;
begin
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) then
   begin
      for i := 1 to AVarList.sgList.RowCount-2 do
      begin
         buffer := '';
         lType := GetType(AVarList.sgList.Cells[1, i]);
         if IsStructType(lType) then
            buffer := 'struct '
         else if IsEnumType(lType) then
            buffer := 'enum ';
         buffer := buffer + AVarList.sgList.Cells[1, i] + ' ' + AVarList.sgList.Cells[0, i];
         dimensCount := AVarList.GetDimensionCount(AVarList.sgList.Cells[0, i]);
         if dimensCount > 0 then
         begin
            for b := 1 to dimensCount do
               buffer := buffer + '[' + AVarList.GetDimension(AVarList.sgList.Cells[0, i], b) + ']';
         end;
         lInit := Trim(AVarList.sgList.Cells[3, i]);
         if lInit <> '' then
            buffer := buffer + ' = ' + lInit;
         ALines.Add(buffer + ';')
      end;
   end;
end;

procedure C_ConstSectionGenerator(ALines: TStrings; AConstList: TConstDeclareList);
var
   i: integer;
begin
   if (AConstList <> nil) and (AConstList.sgList.RowCount > 2) then
   begin
      for i := 1 to AConstList.sgList.RowCount-2 do
      begin
         if not AConstList.IsExternal(i) then
            ALines.Add('#define ' + AConstList.sgList.Cells[0, i] + ' ' + AConstList.sgList.Cells[1, i]);
      end;
   end;
end;

procedure C_LocalVarSectionGenerator(const ALines: TStrings; const AVarList: TVarDeclareList; const idx: integer);
var
   lList: TStringList;
   i: integer;
begin
   lList := TStringList.Create;
   try
      C_VarSectionGenerator(lList, AVarList);
      for i := 0 to lList.Count-1 do
         ALines.Insert(idx+i, GSettings.IndentString + lList[i]);
      if lList.Count > 0 then
         ALines.Insert(idx+i, '');
   finally
      lList.Free;
   end;
end;

procedure C_RoutineSectionGenerator(ALines: TStrings; ASkipBodyGen: boolean);
var
   lRoutine: TRoutine;
   parameter: TParameter;
   header, lName: string;
   iter: TRoutineIterator;
   iterp: TParameterIterator;
   idx, lType, lCount: integer;
begin
   iter := GProject.GetRoutineIterator;
   try
      while iter.HasNext do
      begin
         lRoutine := iter.Next;
         lName := lRoutine.GetName;
         if (lName = '') or lRoutine.Header.chkExtDeclare.Checked then continue;
         if lRoutine.Header.cbType.ItemIndex <> 0 then
            header := lRoutine.Header.cbType.Text + ' '
         else
            header := 'void ';
         header := header + lName + '(';
         lCount := lRoutine.Header.ParameterCount;
         if lCount > 0 then
         begin
            idx := 0;
            iterp := lRoutine.Header.GetParameterIterator;
            try
               while iterp.HasNext do
               begin
                  idx := idx + 1;
                  parameter := iterp.Next;
                  lType := GetType(parameter.cbType.Text);
                  if IsStructType(lType) then
                     header := header + 'struct '
                  else if IsEnumType(lType) then
                     header := header + 'enum ';
                  if parameter.chkReference.Checked then
                     header := header + C_GetPointerTypeName(parameter.cbType.Text)
                  else
                     header := header + parameter.cbType.Text;
                  header := header + ' ' + Trim(parameter.edtName.Text);
                  if parameter.chkTable.Checked then
                     header := header + '[]';
                  if idx <> lCount then
                     header := header + ', ';
               end;
            finally
               iterp.Free;
            end;
         end;
         lRoutine.Header.GenerateDescription(ALines);
         ALines.AddObject(header + ')', lRoutine.Header);
         idx := ALines.Count+1;
         if lRoutine.Body <> nil then
         begin
            lRoutine.Body.GenerateCode(ALines, lLangDef.Name, 0);
            C_LocalVarSectionGenerator(ALines, lRoutine.Header.LocalVars, idx);
         end;
      end;
   finally
      iter.Free;
   end;
end;}

{function C_GetRoutineDescription(AHeader: TRoutineHeader): string;
begin
   if AHeader <> nil then
   begin
      if AHeader.cbType.ItemIndex <> 0 then
         result := AHeader.cbType.Text + ' '
      else
         result := 'void ';
      result := result + Trim(AHeader.edtName.Text);
   end
   else
      result := i18Manager.GetString('MainProgram');
end;}

procedure C_MainProgramSectionGenerator(ALines: TStringList; ADeep: integer);
var
   lBlock: TMainBlock;
begin
   if GProject <> nil then
   begin
      lBlock := GProject.GetMainBlock;
      if lBlock <> nil then
         lBlock.GenerateCode(ALines, lLangDef.Name, ADeep);
   end;
end;

procedure C_SetHLighterAttrs;
var
   cHighlighter: TSynCppSyn;
begin
   if (lLangDef <> nil) and (lLangDef.HighLighter is TSynCppSyn) then
   begin
      cHighlighter := TSynCppSyn(lLangDef.HighLighter);
      cHighlighter.StringAttri.Foreground  := GSettings.EditorStringColor;
      cHighlighter.CharAttri.Foreground    := GSettings.EditorStringColor;
      cHighlighter.NumberAttri.Foreground  := GSettings.EditorNumberColor;
      cHighlighter.FloatAttri.Foreground   := GSettings.EditorNumberColor;
      cHighlighter.HexAttri.Foreground     := GSettings.EditorNumberColor;
      cHighlighter.OctalAttri.Foreground   := GSettings.EditorNumberColor;
      cHighlighter.CommentAttri.Foreground := GSettings.EditorCommentColor;
      cHighlighter.StringAttri.Background  := GSettings.EditorBkgColor;
      cHighlighter.NumberAttri.Background  := GSettings.EditorBkgColor;
      cHighlighter.FloatAttri.Background   := GSettings.EditorBkgColor;
      cHighlighter.OctalAttri.Background   := GSettings.EditorBkgColor;
      cHighlighter.HexAttri.Background     := GSettings.EditorBkgColor;
      cHighlighter.CommentAttri.Background := GSettings.EditorBkgColor;
      cHighlighter.CharAttri.Background    := GSettings.EditorBkgColor;
   end;
end;

function C_GetLiteralType(const AValue: string): integer;
var
   i: integer;
   f: double;
begin
   result := UNKNOWN_TYPE;
   if AValue <> '' then
   begin
      if not TryStrToInt(AValue, i) then
      begin
         if not TryStrToFloat(AValue, f) then
         begin
            if (AValue[1] = C_STRING_DELIM) and (AnsiLastChar(AValue) = C_STRING_DELIM) then
               result := C_CHAR_PTR_TYPE
            else if (Length(AValue) = 3) and (AValue[1] = C_CHAR_DELIM) and (AValue[3] = C_CHAR_DELIM) then
               result := C_CHAR_TYPE
            else if TInfra.SameStrings(AValue, 'NULL') then
               result := GENERIC_PTR_TYPE;
         end
         else
            result := C_REAL_TYPE;
      end
      else
         result := C_INT_TYPE;
   end;
end;

{function C_GetPointerTypeName(const AValue: string): string;
begin
   result := AValue + '*';
end;}

function C_IsPointerType(const AValue: string): boolean;
begin
   result := (AValue <> '') and (AnsiLastChar(AValue) = '*');
end;

function C_GetOriginalType(const APtrType: string): string;
begin
   if C_IsPointerType(APtrType) then
      result := AnsiLeftStr(APtrType, Length(APtrType)-1)
   else
      result := APtrType;
end;

function C_SkipFuncBodyGen: boolean;
begin
   result := false;
end;

initialization

   C_INT_TYPE      := TParserHelper.GetType('int', C_LANG_ID);
   C_REAL_TYPE     := TParserHelper.GetType('float', C_LANG_ID);
   C_CHAR_TYPE     := TParserHelper.GetType('char', C_LANG_ID);
   C_CHAR_PTR_TYPE := TParserHelper.GetType('char*', C_LANG_ID);

   lLangDef := GInfra.GetLangDefinition(C_LANG_ID);
   if lLangDef <> nil then
   begin
      lLangDef.PreGenerationActivities :=  C_PreGenerationActivities;
      lLangDef.LibSectionGenerator := C_LibSectionGenerator;
      //lLangDef.TypeSectionGenerator := C_TypeSectionGenerator;
      //lLangDef.VarSectionGenerator := C_VarSectionGenerator;
      //lLangDef.ConstSectionGenerator := C_ConstSectionGenerator;
      //lLangDef.RoutineSectionGenerator := C_RoutineSectionGenerator;
      //lLangDef.GetRoutineDescription := C_GetRoutineDescription;
      lLangDef.MainProgramSectionGenerator := C_MainProgramSectionGenerator;
      lLangDef.SetHLighterAttrs := C_SetHLighterAttrs;
      lLangDef.GetLiteralType := C_GetLiteralType;
      //lLangDef.GetPointerTypeName := C_GetPointerTypeName;
      lLangDef.IsPointerType := C_IsPointerType;
      lLangDef.GetOriginalType := C_GetOriginalType;
      lLangDef.SkipFuncBodyGen := C_SkipFuncBodyGen;
   end;
   
end.