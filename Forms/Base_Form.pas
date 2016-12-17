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
   Forms, Classes, OmniXML, BaseIterator, CommonInterfaces, Graphics;

type

  TBaseForm = class(TForm, IFocusable)
    public
      procedure Localize(const AList: TStringList); virtual;
      procedure ImportSettingsFromXMLTag(const ATag: IXMLElement); virtual;
      procedure ExportSettingsToXMLTag(const ATag: IXMLElement); virtual;
      procedure ResetForm; virtual;
      procedure Show;
      function RetrieveFocus(AInfo: TFocusInfo): boolean;
      function CanBeFocused: boolean;
      function IsOverlapped: boolean;
      function GetFocusColor: TColor;
      function Remove: boolean;
      function CanBeRemoved: boolean;
      function IsBoldDesc: boolean;
  end;

  TBaseFormIterator = class(TBaseIterator)
    public
      constructor Create;
  end;

implementation

uses
   Types, ApplicationCommon, Windows, Contnrs;

procedure TBaseForm.Localize(const AList: TStringList);
begin
   Caption := AList.Values['FormCaption'];
end;

procedure TBaseForm.ImportSettingsFromXMLTag(const ATag: IXMLElement);
begin
{}
end;

procedure TBaseForm.ExportSettingsToXMLTag(const ATag: IXMLElement);
begin
{}
end;

procedure TBaseForm.ResetForm;
begin
   Close;
   WindowState := wsNormal;
   Position := poMainFormCenter;
end;

constructor TBaseFormIterator.Create;
var
   i: integer;
   comp: TComponent;
   list: TObjectList;
begin
   list := TObjectList.Create(false);
   inherited Create(list);
   for i := 0 to Application.ComponentCount-1 do
   begin
      comp := Application.Components[i];
      if comp is TBaseForm then
         list.Add(comp);
   end;
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
   rect: TRect;
   rgn, tmpRgn: HRGN;
   rType: integer;
   wnd: HWND;
begin
   rect := BoundsRect;
   rgn := CreateRectRgnIndirect(rect);
   wnd := GetWindow(TInfra.GetMainForm.Handle, GW_HWNDFIRST);
   RType := NULLREGION;
   while (wnd <> 0) and (wnd <> Handle) do
   begin
      if IsWindowVisible(wnd) then
      begin
         GetWindowRect(wnd, rect);
         tmpRgn := CreateRectRgnIndirect(rect);
         rType := CombineRgn(tmpRgn, rgn, tmpRgn, RGN_AND);
         DeleteObject(tmpRgn);
       end;
       if rType <> NULLREGION then
          break;
       wnd := GetNextWindow(wnd, GW_HWNDNEXT)
   end;
   DeleteObject(rgn);
   result := rType <> NULLREGION;
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

function TBaseForm.Remove: boolean;
begin
   result := CanBeRemoved;
end;

function TBaseForm.CanBeRemoved: boolean;
begin
   result := false;
end;

function TBaseForm.IsBoldDesc: boolean;
begin
   result := true;
end;

end.
