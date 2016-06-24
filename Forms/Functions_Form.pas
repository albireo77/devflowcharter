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



unit Functions_Form;

interface

uses
   Forms, StdCtrls, ExtCtrls, Graphics, Controls, Menus, ComCtrls, SysUtils,
   Classes, Types, Windows, OmniXML, PageControl_Form,
   CommonTypes;

type

  TFunctionsForm = class(TPageControlForm)
    procedure miAddClick(Sender: TObject); override;
    procedure pgcTabsChange(Sender: TObject); override;
  public
    { Public declarations }
    function ImportTabsFromXMLTag(const rootTag: IXMLElement): TErrorType; override;
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
    procedure RefreshTabs; override;
    procedure ResetForm; override;
    procedure AddUserFunction(const ABodyTopLeft: TPoint);
  end;

var
  FunctionsForm: TFunctionsForm;

implementation

{$R *.dfm}

uses
   ApplicationCommon, Base_Block, Main_Block, Navigator_Form, UserFunction,
   CommonInterfaces;

procedure TFunctionsForm.miAddClick(Sender: TObject);
begin
   AddUserFunction(TInfra.GetMainForm.GetMainBlockNextTopLeft);
end;

procedure TFunctionsForm.AddUserFunction(const ABodyTopLeft: TPoint);
var
   lHeader: TUserFunctionHeader;
   lBody: TMainBlock;
begin
   Show;
   lBody := TMainBlock.Create(GProject.GetActivePage, ABodyTopLeft);
   lHeader := TUserFunctionHeader.Create(Self);
   TUserFunction.Create(lHeader, lBody);
   if CanFocus then
      SetFocus;
   pgcTabs.ActivePage := lHeader;
   if lHeader.edtName.CanFocus then
      lHeader.edtName.SetFocus;
   lHeader.edtName.OnChange(lHeader.edtName);
   if GSettings.UpdateEditor and (lHeader.Font.Color <> NOK_COLOR) then
      TInfra.GetEditorForm.RefreshEditorForObject(lHeader);
   GChange := 1;
end;

procedure TFunctionsForm.pgcTabsChange(Sender: TObject);
var
   lBody: TMainBlock;
begin
   inherited pgcTabsChange(Sender);
   lBody := TUserFunctionHeader(pgcTabs.ActivePage).UserFunction.Body;
   if (lBody <> nil) and lBody.Visible then
   begin
      lBody.Page.Form.ScrollInView(lBody);
      lBody.BringAllToFront;
      NavigatorForm.Invalidate;
   end;
end;

procedure TFunctionsForm.ExportSettingsToXMLTag(const root: IXMLElement);
var
   lFunctionHeader: TUserFunctionHeader;
   val: integer;
begin
   RefreshTabs;
   root.SetAttribute('func_win_h', IntToStr(Height));
   if Visible then
   begin
      root.SetAttribute('func_win_show', '1');
      root.SetAttribute('func_win_x', IntToStr(Left));
      root.SetAttribute('func_win_y', IntToStr(Top));
      if pgcTabs.ActivePageIndex <> -1 then
      begin
         lFunctionHeader := TUserFunctionHeader(pgcTabs.Pages[pgcTabs.ActivePageIndex]);
         root.SetAttribute('func_idx', IntToStr(lFunctionHeader.PageIndex));
         val := lFunctionHeader.ScrollPos;
         if val > 0 then
            root.SetAttribute('func_scroll_v', IntToStr(val));
      end;
      if WindowState = wsMinimized then
         root.SetAttribute('func_win_min', '1');
   end;
end;

function TFunctionsForm.ImportTabsFromXMLTag(const rootTag: IXMLElement): TErrorType;
begin
   result := GProject.ImportUserFunctionsFromXML(rootTag);
end;

procedure TFunctionsForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   lFunctionHeader: TUserFunctionHeader;
   val: integer;
begin
   val := StrToIntDef(root.GetAttribute('func_win_h'), -1);
   if val > -1 then
      Height := val;
   if (root.GetAttribute('func_win_show') = '1') and GInfra.CurrentLang.EnabledUserFunctionHeader then
   begin
      Position := poDesigned;
      if root.GetAttribute('func_win_min') = '1' then
         WindowState := wsMinimized;
      val := StrToIntDef(root.GetAttribute('func_win_x'), -1);
      if val > -1 then
         Left := val;
      val := StrToIntDef(root.GetAttribute('func_win_y'), -1);
      if val > -1 then
         Top := val;
      val := StrToIntDef(root.GetAttribute('func_idx'), -2);
      if (pgcTabs.PageCount > 0) and (val in [0..pgcTabs.PageCount-1]) then
      begin
         pgcTabs.ActivePageIndex := val;
         lFunctionHeader := TUserFunctionHeader(pgcTabs.Pages[val]);
         val := StrToIntDef(root.GetAttribute('func_scroll_v'), 0);
         if val > 0 then
            lFunctionHeader.ScrollPos := val;
      end;
      Show;
   end;
end;

procedure TFunctionsForm.RefreshTabs;
var
   i: integer;
   lFunctionHeader: TUserFunctionHeader;
   iter: IIterator;
begin
   inherited;
   for i := 0 to pgcTabs.PageCount-1 do
   begin
      lFunctionHeader := TUserFunctionHeader(pgcTabs.Pages[i]);
      TInfra.PopulateDataTypeCombo(lFunctionHeader.LocalVars.cbType);
      TInfra.PopulateDataTypeCombo(lFunctionHeader.cbType);
      iter := lFunctionHeader.GetParameterIterator;
      while iter.HasNext do
         TInfra.PopulateDataTypeCombo(TParameter(iter.Next).cbType);
   end;
end;

procedure TFunctionsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 625;
end;

end.
