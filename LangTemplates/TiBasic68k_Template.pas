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
   System.SysUtils, System.Classes, System.StrUtils, UserFunction, DeclareList,
   Main_Block, LangDefinition, ApplicationCommon, CommonInterfaces;

var
   lLangDef: TLangDefinition;

procedure TIBASIC_ProgramHeaderSectionGenerator(ALines: TStringList);
begin
   if not GProject.Name.IsEmpty then
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
      begin
         if i <> 1 then
            buffer := buffer + ', ';
         buffer := buffer + AVarList.sgList.Cells[VAR_NAME_COL, i];
      end;
      if not buffer.IsEmpty then
         ALines.AddObject('Local ' + buffer, AVarList);
   end;
end;

procedure TIBASIC_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
var
   func: TUserFunction;
   funcHeader, funcParms, funcPrefix, funcName: string;
   iterp, iter: IIterator;
begin
   if GProject <> nil then
   begin
      iter := GProject.GetUserFunctions;
      while iter.HasNext do
      begin
         func := TUserFunction(iter.Next);
         funcName := func.GetName;
         if (funcName = '') or func.Header.chkExtDeclare.Checked then
            continue;
         funcPrefix := IfThen(func.Header.cbType.ItemIndex <> 0, 'Func', 'Prgm');
         funcParms := '';
         iterp := func.Header.GetParameterIterator;
         while iterp.HasNext do
         begin
            if not funcParms.IsEmpty then
               funcParms := funcParms + ',';
            funcParms := funcParms + Trim(TParameter(iterp.Next).edtName.Text);
         end;
         funcHeader := 'Define ' + funcName + '(' + funcParms + ')=' + funcPrefix;
         func.Header.GenerateDescription(ALines);
         ALines.AddObject(funcHeader, func.Header);
         if func.Body <> nil then
         begin
            TIBASIC_VarSectionGenerator(ALines, func.Header.LocalVars);
            func.Body.GenerateCode(ALines, lLangDef.Name, 0);
         end;
         ALines.AddObject('End' + funcPrefix, func.Header);
         ALines.Add('');
      end;
   end;
end;

procedure TIBASIC_MainProgramSectionGenerator(ALines: TStringList; deep: integer);
var
   block: TMainBlock;
begin
   if GProject <> nil then
   begin
      block := GProject.GetMainBlock;
      if block <> nil then
      begin
         block.GenerateCode(ALines, lLangDef.Name, deep);
         ALines.AddObject('EndPrgm', block);
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
