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

unit TiBasic68k_Generator;

interface

implementation

uses
   System.SysUtils, System.Classes, System.StrUtils, UserFunction, DeclareList,
   LangDefinition, Infrastructure, Constants;

var
   tiBasicLang: TLangDefinition;

procedure TIBASIC_ProgramHeaderSectionGenerator(ALines: TStringList);
begin
   if not GProject.Name.IsEmpty then
      ALines.Add(GProject.Name + '()');
   ALines.AddObject('Prgm', GProject.GetMainBlock);
end;

procedure TIBASIC_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
begin
   if AVarList <> nil then
   begin
      var buffer := '';
      for var i := 1 to AVarList.sgList.RowCount-2 do
      begin
         if not buffer.IsEmpty then
            buffer := buffer + ', ';
         buffer := buffer + AVarList.sgList.Cells[VAR_NAME_COL, i];
      end;
      if not buffer.IsEmpty then
         ALines.AddObject('Local ' + buffer, AVarList);
   end;
end;

procedure TIBASIC_UserFunctionGenerator(ALines: TStringList; AFunction: TUserFunction; ASkipBodyGen: boolean);
begin
   var funcName := AFunction.GetName;
   if (funcName <> '') and (tiBasicLang.CodeIncludeExternFunction or not AFunction.Header.chkExternal.Checked) then
   begin
      var funcPrefix := IfThen(AFunction.Header.cbType.ItemIndex <> 0, 'Func', 'Prgm');
      var funcParms := '';
      for var param in AFunction.Header.GetParameters do
      begin
         if not funcParms.IsEmpty then
            funcParms := funcParms + ',';
         funcParms := funcParms + Trim(param.edtName.Text);
      end;
      var funcHeader := 'Define ' + funcName + '(' + funcParms + ')=' + funcPrefix;
      AFunction.Header.GenerateDescription(ALines);
      ALines.AddObject(funcHeader, AFunction.Header);
      if AFunction.Body <> nil then
      begin
         TIBASIC_VarSectionGenerator(ALines, AFunction.Header.LocalVars);
         AFunction.Body.GenerateCode(ALines, tiBasicLang.Name, 0);
      end;
      ALines.AddObject('End' + funcPrefix, AFunction.Header);
      ALines.Add('');
   end;
end;

procedure TIBASIC_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
begin
   for var func in GProject.GetUserFunctions do
      TIBASIC_UserFunctionGenerator(ALines, func, ASkipBodyGen);
end;

procedure TIBASIC_MainFunctionSectionGenerator(ALines: TStringList; deep: integer);
begin
   var mainBlock := GProject.GetMainBlock;
   if mainBlock <> nil then
   begin
      mainBlock.GenerateCode(ALines, tiBasicLang.Name, deep);
      ALines.AddObject('EndPrgm', mainBlock);
   end;
end;

initialization

   tiBasicLang := GInfra.GetLangDefinition(TIBASIC_LANG_ID);
   if tiBasicLang <> nil then
   begin
      tiBasicLang.ProgramHeaderSectionGenerator := TIBASIC_ProgramHeaderSectionGenerator;
      tiBasicLang.VarSectionGenerator := TIBASIC_VarSectionGenerator;
      tiBasicLang.UserFunctionGenerator := TIBASIC_UserFunctionGenerator;
      tiBasicLang.UserFunctionsSectionGenerator := TIBASIC_UserFunctionsSectionGenerator;
      tiBasicLang.MainFunctionSectionGenerator := TIBASIC_MainFunctionSectionGenerator;
   end;

end.
