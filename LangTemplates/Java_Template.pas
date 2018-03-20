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



{ This unit contains stuff to support Java language }

unit Java_Template;

interface

implementation

uses
   System.Classes, System.SysUtils, Vcl.Graphics, Vcl.StdCtrls, SynHighlighterJava, DeclareList,
   ApplicationCommon, UserDataType, UserFunction, LangDefinition, ParserHelper,
   CommonTypes;

const
   IMPORT_MASK = 'import %s.%s;';

var
   javaLang: TLangDefinition;
   ImportLibs: TStringList;

function CheckForDataType(const AType: string): boolean;
var
   varList: TVarDeclareList;
   i: integer;
   varType: string;
   dataType: TUserDataType;
   field: TField;
   func: TUserFunction;
   param: TParameter;
begin

   result := false;

   // search in global variables
   varList := GProject.GlobalVars;
   if varList <> nil then
   begin
      for i := 1 to varList.sgList.RowCount-2 do
      begin
         varType := varList.sgList.Cells[VAR_TYPE_COL, i];
         if varType = AType then
            Exit(true);
      end;
   end;

   // search in data types
   for dataType in GProject.GetUserDataTypes do
   begin
      if not dataType.GetName.IsEmpty then
      begin
         for field in dataType.GetFields do
         begin
            if field.cbType.Enabled and (field.cbType.Text = AType) then
               Exit(true);
         end;
      end;
   end;

   // search in functions
   for func in GProject.GetUserFunctions do
   begin
      if (func.Header <> nil) and not func.GetName.IsEmpty then
      begin
         if func.Header.cbType.Text = AType then
            Exit(true);
         varList := func.Header.LocalVars;
         if varList <> nil then
         begin
            for i := 1 to varList.sgList.RowCount-2 do
            begin
               varType := varList.sgList.Cells[VAR_TYPE_COL, i];
               if varType = AType then
                  Exit(true);
            end;
         end;
         for param in func.Header.GetParameters do
         begin
            if param.cbType.Text = AType then
               Exit(true);
         end;
      end;
   end;
end;

procedure Java_LibSectionGenerator(ALines: TStringList);
var
   i: integer;
   libList: TStringList;
   typeName, libImport: string;
   pNativeType: PNativeDataType;
begin

   ImportLibs.Clear;

   for i := 0 to High(javaLang.NativeDataTypes) do
   begin
      pNativeType := @javaLang.NativeDataTypes[i];
      if (pNativeType.Lib <> '') and CheckForDataType(pNativeType.Name) then
         ImportLibs.Add(Format(IMPORT_MASK, [pNativeType.Lib, pNativeType.Name]));
   end;

   libList := GProject.GetLibraryList;
   try
      for i := 0 to libList.Count-1 do
      begin
         typeName := '';
         if libList.Objects[i] is TCustomEdit then
            typeName := Trim(TCustomEdit(libList.Objects[i]).Text);
         if not typeName.IsEmpty then
         begin
            libImport := Format(IMPORT_MASK, [libList.Strings[i], typeName]);
            if ImportLibs.IndexOf(libImport) = -1 then
               ImportLibs.Add(libImport);
         end;
      end;
   finally
      libList.Free;
   end;

   for i := 0 to ImportLibs.Count-1 do
      ALines.Add(ImportLibs[i]);
end;

function ExtractImplementer(const ATypeName: string; const AContents: string): string;
const
   LIST_IMPLEMENTERS: array[1..5] of string = ('CopyOnWriteArrayList', 'ArrayList', 'LinkedList', 'Stack', 'Vector');
   LIST_IMPLEMENTERS_PACKAGE: array[1..5] of string = ('java.util.concurrent.CopyOnWriteArrayList', 'java.util.ArrayList', 'java.util.LinkedList', 'java.util.Stack', 'java.util.Vector');
var
   i: integer;
begin
   result := '';
   if ATypeName = 'List' then
   begin
      for i := 1 to Length(LIST_IMPLEMENTERS) do
      begin
         if AContents.Contains(LIST_IMPLEMENTERS[i]) then
            Exit(LIST_IMPLEMENTERS_PACKAGE[i]);
      end;
   end;
end;

procedure Java_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   i, p1, p2: integer;
   varType, varInit, varVal, generic, varImpl: string;
begin
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) then
   begin
      for i := 1 to AVarList.sgList.RowCount-2 do
      begin
         varType :=  AVarList.sgList.Cells[VAR_TYPE_COL, i];
         varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
         generic := '';
         if TParserHelper.IsGenericType(varType) and not varInit.IsEmpty then
         begin
            p1 := Pos('<', varInit);
            p2 := LastDelimiter('>', varInit);
            if (p1 > 0) and (p2 > p1) then
               generic := Copy(varInit, p1, p2-p1+1);
            varImpl := ExtractImplementer(varType, varInit);
            if not varImpl.IsEmpty then
            begin
               if ImportLibs.IndexOf(varImpl) = -1 then
               begin
                  ImportLibs.Add(varImpl);
               end;
            end;
            varInit := ' = ' + varInit
         end;
         varVal := varType + generic + ' ' + AVarList.sgList.Cells[VAR_NAME_COL, i];
         varVal := varVal + varInit + ';';
         ALines.Add(varVal);
      end;
   end;
end;

procedure Java_SetHLighterAttrs;
var
   hlighter: TSynJavaSyn;
   bkgColor: TColor;
begin
   if (javaLang <> nil) and (javaLang.HighLighter is TSynJavaSyn) then
   begin
      bkgColor := GSettings.EditorBkgColor;
      hlighter := TSynJavaSyn(javaLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := bkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := bkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := bkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := bkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := bkgColor;
   end;
end;



initialization

   ImportLibs := TStringList.Create;

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
   begin
      javaLang.LibSectionGenerator := Java_LibSectionGenerator;
      javaLang.VarSectionGenerator := Java_VarSectionGenerator;
      javaLang.SetHLighterAttrs := Java_SetHLighterAttrs;
   end;

finalization

   ImportLibs.Free;
       
end.