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
   OmniXML, PageControl_Form, CommonTypes;

type

  TDataTypesForm = class(TPageControlForm)
     procedure miAddClick(Sender: TObject); override;
     procedure FormDeactivate(Sender: TObject); override;
     procedure pgcTabsChanging(Sender: TObject; var AllowChange: Boolean);
  public
     { Public declarations }
     procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
     procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
     function ImportTabsFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType; override;
     procedure RefreshTabs; override;
     procedure ResetForm; override;
  end;

var
   DataTypesForm: TDataTypesForm;

implementation

uses
   System.SysUtils, Vcl.Forms, ApplicationCommon, XMLProcessor, UserDataType, CommonInterfaces;

{$R *.dfm}

procedure TDataTypesForm.RefreshTabs;
var
   i: integer;
   dataType: TUserDataType;
   field: TField;
begin
   inherited;
   for i := 0 to pgcTabs.PageCount-1 do
   begin
      dataType := TUserDataType(pgcTabs.Pages[i]);
      for field in dataType.GetFields do
         TInfra.PopulateDataTypeCombo(field.cbType, dataType.PageIndex);
   end;
end;

procedure TDataTypesForm.miAddClick(Sender: TObject);
var
   dataType: TUserDataType;
begin
   dataType := TUserDataType.Create(Self);
   pgcTabs.ActivePage := dataType;
   dataType.edtName.SetFocus;
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
   end;
   TInfra.GetFunctionsForm.RefreshTabs;
   inherited FormDeactivate(Sender);
end;

procedure TDataTypesForm.pgcTabsChanging(Sender: TObject; var AllowChange: Boolean);
begin
   RefreshTabs;
end;

function TDataTypesForm.ImportTabsFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
begin
   result := GProject.ImportUserDataTypesFromXML(ATag, true);
end;

procedure TDataTypesForm.ExportSettingsToXMLTag(ATag: IXMLElement);
var
   dataType: TUserDataType;
   val: integer;
begin
   ATag.SetAttribute('struct_win_h', Height.ToString);
   if Visible then
   begin
      ATag.SetAttribute('struct_win_show', 'true');
      ATag.SetAttribute('struct_win_x', Left.ToString);
      ATag.SetAttribute('struct_win_y', Top.ToString);
      if pgcTabs.ActivePageIndex <> -1 then
      begin
         dataType := TUserDataType(pgcTabs.Pages[pgcTabs.ActivePageIndex]);
         ATag.SetAttribute('struct_idx', dataType.PageIndex.ToString);
         val := dataType.ScrollPos;
         if val > 0 then
            ATag.SetAttribute('struct_scroll_v', val.ToString);
      end;
      if WindowState = wsMinimized then
         ATag.SetAttribute('struct_win_min', 'true');
   end;
end;

procedure TDataTypesForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   dataType: TUserDataType;
   val: integer;
begin
   val := StrToIntDef(ATag.GetAttribute('struct_win_h'), -1);
   if val > -1 then
      Height := val;
   if TXMLProcessor.GetBoolFromXMLNode(ATag, 'struct_win_show') and GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      Position := poDesigned;
      if TXMLProcessor.GetBoolFromXMLNode(ATag, 'struct_win_min') then
         WindowState := wsMinimized;
      val := StrToIntDef(ATag.GetAttribute('struct_win_x'), -1);
      if val > -1 then
         Left := val;
      val := StrToIntDef(ATag.GetAttribute('struct_win_y'), -1);
      if val > -1 then
         Top := val;
      val := StrToIntDef(ATag.GetAttribute('struct_idx'), -2);
      if (val >= 0) and (val < pgcTabs.PageCount) then
      begin
         pgcTabs.ActivePageIndex := val;
         dataType := TUserDataType(pgcTabs.Pages[val]);
         val := StrToIntDef(ATag.GetAttribute('struct_scroll_v'), 0);
         if val > 0 then
            dataType.ScrollPos := val;
      end;
      Show;
   end;
end;

procedure TDataTypesForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
end;

end.
