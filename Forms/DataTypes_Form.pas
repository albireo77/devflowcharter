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



unit DataTypes_Form;

interface

uses
   OmniXML, PageControl_Form, Types;

type

  TDataTypesForm = class(TPageControlForm)
     procedure miAddClick(Sender: TObject); override;
     procedure FormDeactivate(Sender: TObject); override;
     procedure pgcTabsChanging(Sender: TObject; var AllowChange: Boolean);
     function IsEnabled: boolean; override;
  public
     { Public declarations }
     function ImportTabsFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError; override;
     procedure RefreshTabs; override;
     procedure ResetForm; override;
  end;

var
   DataTypesForm: TDataTypesForm;

implementation

uses
   Infrastructure, UserDataType;

{$R *.dfm}

procedure TDataTypesForm.RefreshTabs;
begin
   inherited;
   for var i := 0 to pgcTabs.PageCount-1 do
   begin
      var dataType := TUserDataType(pgcTabs.Pages[i]);
      for var field in dataType.GetFields do
         TInfra.PopulateDataTypeCombo(field.cbType, dataType.PageIndex);
   end;
end;

procedure TDataTypesForm.miAddClick(Sender: TObject);
begin
   var dataType := TUserDataType.Create(Self);
   pgcTabs.ActivePage := dataType;
   dataType.edtName.SetFocus;
   if Assigned(dataType.edtName.OnChange) then
      dataType.edtName.OnChange(dataType.edtName);
   TInfra.UpdateCodeEditor(dataType);
end;

procedure TDataTypesForm.FormDeactivate(Sender: TObject);
begin
   if GProject <> nil then
   begin
      if GProject.GlobalVars <> nil then
         TInfra.PopulateDataTypeCombo(GProject.GlobalVars.cbType);
      GProject.PopulateDataTypeSets;
      GProject.PopulateDataTypeCombos;
      GProject.RefreshVarTypes;
   end;
   TInfra.GetFunctionsForm.RefreshTabs;
   inherited FormDeactivate(Sender);
end;

procedure TDataTypesForm.pgcTabsChanging(Sender: TObject; var AllowChange: Boolean);
begin
   RefreshTabs;
end;

function TDataTypesForm.ImportTabsFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
begin
   result := GProject.ImportUserDataTypesFromXML(ANode, AImportMode);
end;

procedure TDataTypesForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
   FPrefix := 'struct_';
end;

function TDataTypesForm.IsEnabled: boolean;
begin
   result := GInfra.CurrentLang.EnabledUserDataTypes;
end;

end.
