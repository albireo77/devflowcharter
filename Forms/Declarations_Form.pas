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
   OmniXML, Base_Form;

type

  TDeclarationsForm = class(TBaseForm)
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
    procedure ResetForm; override;
  end;

var
  DeclarationsForm: TDeclarationsForm;

implementation

uses
   Vcl.Forms, System.SysUtils, ApplicationCommon, XMLProcessor, DeclareList;

{$R *.dfm}

procedure TDeclarationsForm.ExportSettingsToXMLTag(ATag: IXMLElement);
begin
   ATag.SetAttribute('var_win_h', Height.ToString);
   ATag.SetAttribute('var_win_w', Width.ToString);
   if Visible then
   begin
      ATag.SetAttribute('var_win_show', 'true');
      ATag.SetAttribute('var_win_x', Left.ToString);
      ATag.SetAttribute('var_win_y', Top.ToString);
      if WindowState = wsMinimized then
         ATag.SetAttribute('var_win_min', 'true');
   end;
end;

procedure TDeclarationsForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   val: integer;
begin
   val := StrToIntDef(ATag.GetAttribute('var_win_h'), -1);
   if val > -1 then
      Height := val;
   val := StrToIntDef(ATag.GetAttribute('var_win_w'), -1);
   if val > -1 then
      Width := val;
   if TXMLProcessor.GetBoolFromAttr(ATag, 'var_win_show') and (GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts) then
   begin
      Position := poDesigned;
      if TXMLProcessor.GetBoolFromAttr(ATag, 'var_win_min') then
         WindowState := wsMinimized;
      val := StrToIntDef(ATag.GetAttribute('var_win_x'), -1);
      if val > -1 then
         Left := val;
      val := StrToIntDef(ATag.GetAttribute('var_win_y'), -1);
      if val > -1 then
         Top := val;
      Show;
   end;
end;

procedure TDeclarationsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
   Width := 610;
end;

procedure TDeclarationsForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
   i: integer;
   list: TDeclareList;
begin
   i := ControlCount;
   if (i > 0) and  (Controls[i-1] is TDeclareList) then
   begin
      list := TDeclareList(Controls[i-1]);
      if NewWidth < (list.GetMinWidth + list.Left + DECLARATIONS_FORM_RIGHT_MARGIN)  then
         Resize := false;
   end;
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
