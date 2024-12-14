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
   Vcl.Forms, System.Classes, Vcl.Graphics, OmniXML, Types, Interfaces;

type

  TBaseForm = class(TForm, IWithFocus)
    protected
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    public
      procedure AfterTranslation(AList: TStringList); virtual;
      procedure ImportFromXML(ANode: IXMLNode); virtual;
      procedure ExportToXML(ANode: IXMLNode); virtual;
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

procedure TBaseForm.AfterTranslation(AList: TStringList);
begin
   Caption := AList.Values['FormCaption'];
end;

procedure TBaseForm.ImportFromXML(ANode: IXMLNode);
begin
{}
end;

procedure TBaseForm.ExportToXML(ANode: IXMLNode);
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
   result := True;
end;

function TBaseForm.CanBeFocused: boolean;
begin
   result := True;
end;

function TBaseForm.IsOverlapped: boolean;
begin
   result := False;
   var wrect: TRect;
   var brect := BoundsRect;
   var wnd := GetWindow(TInfra.GetMainForm.Handle, GW_HWNDFIRST);
   while (wnd <> 0) and (wnd <> Handle) do
   begin
      if IsWindowVisible(wnd) then
      begin
         GetWindowRect(wnd, wrect);
         if brect.IntersectsWith(wrect) then
         begin
            result := True;
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
   result := False;
end;

function TBaseForm.IsBoldDesc: boolean;
begin
   result := True;
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
