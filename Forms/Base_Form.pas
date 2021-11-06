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



unit Base_Form;

interface

uses
   Vcl.Forms, System.Classes, Vcl.Graphics, Vcl.Controls, OmniXML, BaseEnumerator,
   Types, Interfaces, Vcl.ExtCtrls;

type

  TBaseForm = class(TForm, IWithFocus)
    protected
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    public
      procedure Localize(AList: TStringList); virtual;
      procedure ImportSettingsFromXMLTag(ATag: IXMLElement); virtual;
      procedure ExportSettingsToXMLTag(ATag: IXMLElement); virtual;
      procedure ResetForm; virtual;
      procedure Show;
      function RetrieveFocus(AInfo: TFocusInfo): boolean;
      function CanBeFocused: boolean;
      function IsOverlapped: boolean;
      function GetFocusColor: TColor;
      function Remove(ANode: TTreeNodeWithFriend = nil): boolean;
      function CanRemove: boolean;
      function IsBoldDesc: boolean;
      function GetTreeNodeText(ANodeOffset: integer = 0): string;
  end;

implementation

uses
   WinApi.Windows, Infrastructure, Constants;

procedure TBaseForm.Localize(AList: TStringList);
begin
   Caption := AList.Values['FormCaption'];
end;

procedure TBaseForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
begin
{}
end;

procedure TBaseForm.ExportSettingsToXMLTag(ATag: IXMLElement);
begin
{}
end;

procedure TBaseForm.ResetForm;
begin
   Close;
   WindowState := wsNormal;
   Position := poMainFormCenter;
end;

function TBaseForm.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   Show;
   if (AInfo.ActiveControl <> nil) and AInfo.ActiveControl.CanFocus then
      AInfo.ActiveControl.SetFocus;
   result := true;
end;

function TBaseForm.CanBeFocused: boolean;
begin
   result := true;
end;

function TBaseForm.IsOverlapped: boolean;
var
   brect, wrect: TRect;
   wnd: HWND;
begin
   result := false;
   brect := BoundsRect;
   wnd := GetWindow(TInfra.GetMainForm.Handle, GW_HWNDFIRST);
   while (wnd <> 0) and (wnd <> Handle) do
   begin
      if IsWindowVisible(wnd) then
      begin
         GetWindowRect(wnd, wrect);
         if brect.IntersectsWith(wrect) then
         begin
            result := true;
            break;
         end;
       end;
       wnd := GetNextWindow(wnd, GW_HWNDNEXT)
   end;
end;

procedure TBaseForm.Show;
begin
   if not Visible then
      inherited Show;
   if WindowState = wsMinimized then
      WindowState := wsNormal;
   if IsOverlapped then
      BringToFront;
end;

function TBaseForm.GetFocusColor: TColor;
begin
   result := OK_COLOR;
end;

function TBaseForm.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := CanRemove;
end;

function TBaseForm.CanRemove: boolean;
begin
   result := false;
end;

function TBaseForm.IsBoldDesc: boolean;
begin
   result := true;
end;

function TBaseForm.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := Caption;
end;

procedure TBaseForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
   if Key in TO_MAIN_FORM_KEYS then
      TInfra.GetMainForm.KeyDown(Key, Shift);
end;

end.
