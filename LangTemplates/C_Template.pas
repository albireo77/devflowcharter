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
   System.SysUtils, System.StrUtils, System.Classes, SynHighlighterCpp, Vcl.Graphics,
   Main_Block, ApplicationCommon, LangDefinition, ParserHelper;

var
   cLang: TLangDefinition;

procedure C_ExecuteBeforeGeneration;
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
   libIncl, lib: string;
begin
   libList := GProject.GetLibraryList;
   try
      if math_flag <> 0 then
         libList.Add('math');
      if str_flag <> 0 then
         libList.Add('string');
      if io_flag <> 0 then
         libList.Add('stdio');
      if libList.Count > 0 then
      begin
         for lib in libList do
         begin
            libIncl := lib;
            if not lib.Contains('.') then
               libIncl := libIncl + cLang.LibraryExt;
            if not lib.Contains('"') then
               libIncl := '<' + libIncl + '>';
            libIncl := '#include ' + libIncl;
            ALines.Add(libIncl);
         end;
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

procedure C_MainFunctionSectionGenerator(ALines: TStringList; ADeep: integer);
var
   lBlock: TMainBlock;
begin
   if GProject <> nil then
   begin
      lBlock := GProject.GetMainBlock;
      if lBlock <> nil then
         lBlock.GenerateCode(ALines, cLang.Name, ADeep);
   end;
end;

procedure C_SetHLighterAttrs;
var
   hlighter: TSynCppSyn;
   bkgColor: TColor;
begin
   if (cLang <> nil) and (cLang.HighLighter is TSynCppSyn) then
   begin
      bkgColor := GSettings.EditorBkgColor;
      hlighter := TSynCppSyn(cLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := bkgColor;
      hlighter.CharAttri.Foreground       := GSettings.EditorStringColor;
      hlighter.CharAttri.Background       := bkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := bkgColor;
      hlighter.FloatAttri.Foreground      := GSettings.EditorNumberColor;
      hlighter.FloatAttri.Background      := bkgColor;
      hlighter.HexAttri.Foreground        := GSettings.EditorNumberColor;
      hlighter.HexAttri.Background        := bkgColor;
      hlighter.OctalAttri.Foreground      := GSettings.EditorNumberColor;
      hlighter.OctalAttri.Background      := bkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := bkgColor;
      hlighter.DirecAttri.Foreground      := GSettings.EditorCommentColor;
      hlighter.DirecAttri.Background      := bkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := bkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := bkgColor;
   end;
end;

function C_GetLiteralType(const AValue: string): integer;
var
   i: integer;
   f: double;
begin
   result := UNKNOWN_TYPE;
   if not AValue.IsEmpty then
   begin
      if not TryStrToInt(AValue, i) then
      begin
         if not TryStrToFloat(AValue, f) then
         begin
            if AValue.StartsWith(C_STRING_DELIM) and AValue.EndsWith(C_STRING_DELIM) then
               result := C_CHAR_PTR_TYPE
            else if (AValue.Length = 3) and (AValue[1] = C_CHAR_DELIM) and (AValue[3] = C_CHAR_DELIM) then
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

function C_IsPointerType(const AType: string): boolean;
begin
   result := (AType.Length > 1) and AType.EndsWith('*');
end;

function C_GetOriginalType(const AType: string): string;
begin
   result := AType;
   if C_IsPointerType(result) then
      SetLength(result, result.Length - 1);
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

   cLang := GInfra.GetLangDefinition(C_LANG_ID);
   if cLang <> nil then
   begin
      cLang.ExecuteBeforeGeneration :=  C_ExecuteBeforeGeneration;
      cLang.LibSectionGenerator := C_LibSectionGenerator;
      //cLang.TypeSectionGenerator := C_TypeSectionGenerator;
      //cLang.VarSectionGenerator := C_VarSectionGenerator;
      //cLang.ConstSectionGenerator := C_ConstSectionGenerator;
      //cLang.RoutineSectionGenerator := C_RoutineSectionGenerator;
      //cLang.GetRoutineDescription := C_GetRoutineDescription;
      cLang.MainFunctionSectionGenerator := C_MainFunctionSectionGenerator;
      cLang.SetHLighterAttrs := C_SetHLighterAttrs;
      cLang.GetLiteralType := C_GetLiteralType;
      //cLang.GetPointerTypeName := C_GetPointerTypeName;
      cLang.IsPointerType := C_IsPointerType;
      cLang.GetOriginalType := C_GetOriginalType;
      cLang.SkipFuncBodyGen := C_SkipFuncBodyGen;
   end;
   
end.