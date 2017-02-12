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



unit BlockTabSheet;

interface

uses
   System.Classes, Vcl.ComCtrls, Vcl.Controls, WinApi.Messages, CommonInterfaces,
   Main_Form;

type

   TBlockTabSheet = class(TTabSheet)
   private
      FForm: TMainForm;
   public
      DrawI: boolean;
      constructor Create(AMainForm: TMainForm);
      destructor Destroy; override;
      property Form: TMainForm read FForm;
   protected
      procedure PageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
   end;

implementation

uses
   System.Types, WinApi.Windows, ApplicationCommon, UserFunction;

constructor TBlockTabSheet.Create(AMainForm: TMainForm);
begin
   inherited Create(AMainForm.pgcPages);
   PageControl := AMainForm.pgcPages;
   FForm := AMainForm;
   ParentFont := false;
   Brush.Color := GSettings.DesktopColor;
   Align := alClient;
   OnMouseUp := PageMouseUp;
   DrawI := true;
end;

destructor TBlockTabSheet.Destroy;
var
   iter: IIterator;
   func: TUserFunction;
begin
   if GProject <> nil then
   begin
      iter := GProject.GetUserFunctions;
      while iter.HasNext do
      begin
         func := TUserFunction(iter.Next);
         if func.Body.Page = Self then
            func.Free;
      end;
   end;
   inherited Destroy;
end;

procedure TBlockTabSheet.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
   FillRect(Msg.DC, ClientRect, Brush.Handle);
   Msg.Result := 1;
end;

procedure TBlockTabSheet.PageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pnt: TPoint;
begin
   if (Button = mbRight) and (GProject <> nil) then
   begin
      pnt := ClientToScreen(Point(X, Y));
      FForm.pmPages.PopupComponent := Self;
      FForm.pmPages.Popup(pnt.X, pnt.Y);
   end;
end;

end.

