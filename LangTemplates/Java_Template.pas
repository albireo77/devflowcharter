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
   System.Classes, System.SysUtils, SynHighlighterJava, DeclareList, ApplicationCommon,
   UserDataType, UserFunction, LangDefinition;

const
   IMPORT_MASK = 'import %s;';

var
   javaLang: TLangDefinition;
   importLibs: TStringList;

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
begin

   importLibs.Clear;

   if CheckForDataType('BigDecimal') then
      importLibs.Add('java.math.BigDecimal');

   if CheckForDataType('Date') then
      importLibs.Add('java.util.Date');

   if CheckForDataType('Calendar') then
      importLibs.Add('java.util.Calendar');

   if CheckForDataType('LocalDate') then
      importLibs.Add('java.time.LocalDate');

   if CheckForDataType('LocalDateTime') then
      importLibs.Add('java.time.LocalDateTime');

   if CheckForDataType('LocalTime') then
      importLibs.Add('java.time.LocalTime');

   if CheckForDataType('List') then
      importLibs.Add('java.util.List');

   if CheckForDataType('Map') then
      importLibs.Add('java.util.Map');

   if CheckForDataType('Set') then
      importLibs.Add('java.util.Set');

   if CheckForDataType('Queue') then
      importLibs.Add('java.util.Queue');

   if CheckForDataType('Random') then
      importLibs.Add('java.util.Random');

   if CheckForDataType('DateFormat') then
      importLibs.Add('java.text.DateFormat');

   if CheckForDataType('Reader') then
      importLibs.Add('java.io.Reader');

   if CheckForDataType('Writer') then
      importLibs.Add('java.io.Writer');

   if CheckForDataType('InputStream') then
      importLibs.Add('java.io.InputStream');

   if CheckForDataType('OutputStream') then
      importLibs.Add('java.io.OutputStream');

   if CheckForDataType('File') then
      importLibs.Add('java.io.File');

   for i := 0 to importLibs.Count-1 do
      ALines.Add(Format(IMPORT_MASK, [importLibs[i]]));
end;

procedure Java_SetHLighterAttrs;
var
   hlighter: TSynJavaSyn;
begin
   if (javaLang <> nil) and (javaLang.HighLighter is TSynJavaSyn) then
   begin
      hlighter := TSynJavaSyn(javaLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := GSettings.EditorBkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := GSettings.EditorBkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := GSettings.EditorBkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := GSettings.EditorBkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := GSettings.EditorBkgColor;
   end;
end;



initialization

   importLibs := TStringList.Create;

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
   begin
      javaLang.LibSectionGenerator := Java_LibSectionGenerator;
      javaLang.SetHLighterAttrs := Java_SetHLighterAttrs;
   end;

finalization

   importLibs.Free;
       
end.