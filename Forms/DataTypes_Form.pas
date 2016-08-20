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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  ComCtrls, StdCtrls, OmniXML, PageControl_Form, CommonTypes;

type

  TDataTypesForm = class(TPageControlForm)
    procedure miAddClick(Sender: TObject); override;
    procedure FormDeactivate(Sender: TObject); override;
    procedure pgcTabsChanging(Sender: TObject; var AllowChange: Boolean);
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(const ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const ATag: IXMLElement); override;
    function ImportTabsFromXMLTag(const ATag: IXMLElement): TErrorType; override;
    procedure RefreshTabs; override;
    procedure ResetForm; override;
  end;

var
  DataTypesForm: TDataTypesForm;

implementation

uses
   ApplicationCommon, XMLProcessor, UserDataType, CommonInterfaces;

{$R *.dfm}

procedure TDataTypesForm.RefreshTabs;
var
   i: integer;
   lDataType: TUserDataType;
   iter: IIterator;
begin
   inherited;
   for i := 0 to pgcTabs.PageCount-1 do
   begin
      lDataType := TUserDataType(pgcTabs.Pages[i]);
      iter := lDataType.GetFieldIterator;
      while iter.HasNext do
         TInfra.PopulateDataTypeCombo(TField(iter.Next).cbType, lDataType.PageIndex);
   end;
end;

procedure TDataTypesForm.miAddClick(Sender: TObject);
var
   lDataType: TUserDataType;
begin
   lDataType := TUserDataType.Create(Self);
   pgcTabs.ActivePage := lDataType;
   lDataType.edtName.SetFocus;
   lDataType.edtName.OnChange(lDataType.edtName);
   TInfra.UpdateCodeEditor(lDataType);
end;

procedure TDataTypesForm.FormDeactivate(Sender: TObject);
begin
   if GProject <> nil then
   begin
      if GProject.GlobalVars <> nil then
         TInfra.PopulateDataTypeCombo(GProject.GlobalVars.cbType);
      GProject.PopulateDataTypes;
      GProject.PopulateDataTypeCombos;
   end;
   TInfra.GetFunctionsForm.RefreshTabs;
   inherited FormDeactivate(Sender);
end;

procedure TDataTypesForm.pgcTabsChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
   RefreshTabs;
end;

function TDataTypesForm.ImportTabsFromXMLTag(const ATag: IXMLElement): TErrorType;
begin
   result := GProject.ImportUserDataTypesFromXML(ATag);
end;

procedure TDataTypesForm.ExportSettingsToXMLTag(const ATag: IXMLElement);
var
   lDataType: TUserDataType;
   val: integer;
begin
   ATag.SetAttribute('struct_win_h', IntToStr(Height));
   if Visible then
   begin
      ATag.SetAttribute('struct_win_show', '1');
      ATag.SetAttribute('struct_win_x', IntToStr(Left));
      ATag.SetAttribute('struct_win_y', IntToStr(Top));
      if pgcTabs.ActivePageIndex <> -1 then
      begin
         lDataType := TUserDataType(pgcTabs.Pages[pgcTabs.ActivePageIndex]);
         ATag.SetAttribute('struct_idx', IntToStr(lDataType.PageIndex));
         val := lDataType.ScrollPos;
         if val > 0 then
            ATag.SetAttribute('struct_scroll_v', IntToStr(val));
      end;
      if WindowState = wsMinimized then
         ATag.SetAttribute('struct_win_min', '1');
   end;
end;

procedure TDataTypesForm.ImportSettingsFromXMLTag(const ATag: IXMLElement);
var
   lDataType: TUserDataType;
   val: integer;
begin
   val := StrToIntDef(ATag.GetAttribute('struct_win_h'), -1);
   if val > -1 then
      Height := val;
   if (ATag.GetAttribute('struct_win_show') = '1') and GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      Position := poDesigned;
      if ATag.GetAttribute('struct_win_min') = '1' then
         WindowState := wsMinimized;
      val := StrToIntDef(ATag.GetAttribute('struct_win_x'), -1);
      if val > -1 then
         Left := val;
      val := StrToIntDef(ATag.GetAttribute('struct_win_y'), -1);
      if val > -1 then
         Top := val;
      val := StrToIntDef(ATag.GetAttribute('struct_idx'), -2);
      if (pgcTabs.PageCount > 0) and (val in [0..pgcTabs.PageCount-1]) then
      begin
         pgcTabs.ActivePageIndex := val;
         lDataType := TUserDataType(pgcTabs.Pages[val]);
         val := StrToIntDef(ATag.GetAttribute('struct_scroll_v'), 0);
         if val > 0 then
            lDataType.ScrollPos := val;
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
