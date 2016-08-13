{
   Copyright (C) 2006 The devFlowcharter project
   Author: David Fernando Suescun Ramirez dashja@gmail.com http://sourceforge.net/projects/daisuke-edit/ 
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

unit TiBasic68k_Template;

interface

implementation

uses
   SysUtils, UserFunction, StrUtils, DeclareList, Main_Block, LangDefinition,
   ApplicationCommon, Classes, CommonInterfaces;

var
   lLangDef: TLangDefinition;

procedure TIBASIC_ProgramHeaderSectionGenerator(ALines: TStringList);
begin
   if GProject.Name <> '' then
      ALines.Add(GProject.Name + '()');
   ALines.AddObject('Prgm', GProject.GetMainBlock);
end;

procedure TIBASIC_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   buffer: string;
   i: integer;
begin
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) then
   begin
      buffer := '';
      for i := 1 to AVarList.sgList.RowCount-2 do
         buffer := buffer + AVarList.sgList.Cells[VAR_NAME_COL, i] + ', ';
      buffer := AnsiLeftStr(buffer, Length(buffer)-2);
      if buffer <> '' then
         ALines.AddObject('Local ' + buffer, AVarList);
   end;
end;

procedure TIBASIC_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
var
   lFunction: TUserFunction;
   header, params, funcPrefix, lName: string;
   iterp, iter: IIterator;
begin
   if GProject <> nil then
   begin
      iter := GProject.GetUserFunctions;
      while iter.HasNext do
      begin
         params := '';
         lFunction := TUserFunction(iter.Next);
         lName := lFunction.GetName;
         if (lName = '') or lFunction.Header.chkExtDeclare.Checked then continue;
         if lFunction.Header.cbType.ItemIndex <> 0 then
            funcPrefix := 'Func'
         else
            funcPrefix := 'Prgm';
         header := 'Define ' + lName + '(';
         iterp := lFunction.Header.GetParameterIterator;
         while iterp.HasNext do
            params := params + Trim(TParameter(iterp.Next).edtName.Text) + ',';
         if params <> '' then
            header := header + AnsiLeftStr(params, Length(params)-1);
         header := header + ')=' + funcPrefix;
         lFunction.Header.GenerateDescription(ALines);
         ALines.AddObject(header, lFunction.Header);
         if lFunction.Body <> nil then
         begin
            TIBASIC_VarSectionGenerator(ALines, lFunction.Header.LocalVars);
            lFunction.Body.GenerateCode(ALines, lLangDef.Name, 0);
         end;
         ALines.AddObject('End' + funcPrefix, lFunction.Header);
         ALines.Add('');
      end;
   end;
end;

procedure TIBASIC_MainProgramSectionGenerator(ALines: TStringList; deep: integer);
var
   lBlock: TMainBlock;
begin
   if GProject <> nil then
   begin
      lBlock := GProject.GetMainBlock;
      if lBlock <> nil then
      begin
         lBlock.GenerateCode(ALines, lLangDef.Name, deep);
         ALines.AddObject('EndPrgm', lBlock);
      end;
   end;
end;

{function TIBASIC_GetRoutineDescription(AHeader: TRoutineHeader): string;
begin
   if AHeader <> nil then
   begin
      if AHeader.cbType.ItemIndex <> 0 then
         result := 'Func '
      else
         result := 'Prgm ';
      result := result + Trim(AHeader.edtName.Text);
   end
   else
      result := i18Manager.GetString('MainProgram');
end;}

initialization

   lLangDef := GInfra.GetLangDefinition(TIBASIC_LANG_ID);
   if lLangDef <> nil then
   begin
      lLangDef.ProgramHeaderSectionGenerator := TIBASIC_ProgramHeaderSectionGenerator;
      lLangDef.VarSectionGenerator := TIBASIC_VarSectionGenerator;
      lLangDef.UserFunctionsSectionGenerator := TIBASIC_UserFunctionsSectionGenerator;
      //lLangDef.GetRoutineDescription := TIBASIC_GetRoutineDescription;
      lLangDef.MainProgramSectionGenerator := TIBASIC_MainProgramSectionGenerator;
   end;

end.
