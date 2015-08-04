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



unit Declarations_Form;

interface

uses
   Controls, Forms, StdCtrls, Grids, SysUtils, Classes, Windows, OmniXML, Base_Form;

type

  TDeclarationsForm = class(TBaseForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
    procedure ResetForm; override;
  end;

var
  DeclarationsForm: TDeclarationsForm;

implementation

uses
   ApplicationCommon, Dialogs;

{$R *.dfm}

procedure TDeclarationsForm.ExportSettingsToXMLTag(const root: IXMLElement);
begin
   root.SetAttribute('var_win_h', IntToStr(Height));
   root.SetAttribute('var_win_w', IntToStr(Width));
   if Visible then
   begin
      root.SetAttribute('var_win_show', '1');
      root.SetAttribute('var_win_x', IntToStr(Left));
      root.SetAttribute('var_win_y', IntToStr(Top));
      if WindowState = wsMinimized then
         root.SetAttribute('var_win_min', '1');
   end;
end;

procedure TDeclarationsForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   val: integer;
begin
   val := StrToIntDef(root.GetAttribute('var_win_h'), -1);
   if val > -1 then
      Height := val;
   val := StrToIntDef(root.GetAttribute('var_win_w'), -1);
   if val > -1 then
      Width := val;
   if (root.GetAttribute('var_win_show') = '1') and (GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts) then
   begin
      Position := poDesigned;
      if root.GetAttribute('var_win_min') = '1' then
         WindowState := wsMinimized;
      val := StrToIntDef(root.GetAttribute('var_win_x'), -1);
      if val > -1 then
         Left := val;
      val := StrToIntDef(root.GetAttribute('var_win_y'), -1);
      if val > -1 then
         Top := val;
      Show;
   end;
end;

procedure TDeclarationsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
end;

procedure TDeclarationsForm.FormShow(Sender: TObject);
begin
   if GProject <> nil then
   begin
      if GProject.GlobalVars <> nil then
         GProject.GlobalVars.SetDefaultFocus
      else if GProject.GlobalConsts <> nil then
         GProject.GlobalConsts.SetDefaultFocus;
   end;
end;

end.
