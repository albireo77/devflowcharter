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
   System.Classes, Vcl.ComCtrls, Vcl.Controls, Vcl.Forms, WinApi.Messages, System.Types,
   Vcl.Graphics, CommonInterfaces, Main_Form, OmniXML;

type

   TScrollBoxEx = class;

   TBlockTabSheet = class(TTabSheet)
   private
      FForm: TMainForm;
   public
      DrawI: boolean;
      Box: TScrollBoxEx;
      constructor Create(AMainForm: TMainForm);
      destructor Destroy; override;
      procedure ExportToXMLTag(ATag: IXMLElement);
      procedure ImportFromXMLTag(ATag: IXMLElement);
      property Form: TMainForm read FForm;
   end;

   TScrollBoxEx = class(TScrollBox)
      procedure ScrollV(var Msg: TWMVScroll); message WM_VSCROLL;
      procedure ScrollH(var Msg: TWMHScroll); message WM_HSCROLL;
      procedure AutoScrollInView(AControl: TControl); override;
   private
      FPage: TBlockTabSheet;
      procedure PerformFormsRepaint;
   protected
      procedure BoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
   public
      constructor Create(APage: TBlockTabSheet);
      procedure PaintToCanvas(ACanvas: TCanvas);
      procedure SetScrollBars;
      function GetBottomRight: TPoint;
      function GetDisplayRect: TRect;
      procedure BoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure BoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                MousePos: TPoint; var Handled: Boolean);
      property Page: TBlockTabSheet read FPage;
   end;

implementation

uses
   WinApi.Windows, System.SysUtils, ApplicationCommon, UserFunction, Navigator_Form;

constructor TBlockTabSheet.Create(AMainForm: TMainForm);
begin
   inherited Create(AMainForm.pgcPages);
   PageControl := AMainForm.pgcPages;
   FForm := AMainForm;
   ParentFont := false;
   Align := alClient;
   DrawI := true;
   Box := TScrollBoxEx.Create(Self);
   Box.Parent := Self;
end;

destructor TBlockTabSheet.Destroy;
var
   func: TUserFunction;
begin
   if GProject <> nil then
   begin
      for func in GProject.GetUserFunctions do
      begin
         if func.Body.Page = Self then
            func.Free;
      end;
   end;
   inherited Destroy;
end;

procedure TBlockTabSheet.ExportToXMLTag(ATag: IXMLElement);
var
   tag: IXMLElement;
   lName: string;
begin
   tag := ATag.OwnerDocument.CreateElement('page');
   ATag.AppendChild(tag);
   if GProject.GetMainPage = Self then
      lName := MAIN_PAGE_MARKER
   else
      lName := Caption;
   tag.SetAttribute('name', lName);
   tag.SetAttribute('hScrollRange', Box.HorzScrollBar.Range.ToString);
   tag.SetAttribute('vScrollRange', Box.VertScrollBar.Range.ToString);
   tag.SetAttribute('hScrollPos', Box.HorzScrollBar.Position.ToString);
   tag.SetAttribute('vScrollPos', Box.VertScrollBar.Position.ToString);
end;

procedure TBlockTabSheet.ImportFromXMLTag(ATag: IXMLElement);
var
   val: integer;
begin
   val := StrToIntDef(ATag.GetAttribute('hScrollRange'), -1);
   if val > -1 then
      Box.HorzScrollBar.Range := val;
   val := StrToIntDef(ATag.GetAttribute('hScrollPos'), -1);
   if val > -1 then
      Box.HorzScrollBar.Position := val;
   val := StrToIntDef(ATag.GetAttribute('vScrollRange'), -1);
   if val > -1 then
      Box.VertScrollBar.Range := val;
   val := StrToIntDef(ATag.GetAttribute('vScrollPos'), -1);
   if val > -1 then
      Box.VertScrollBar.Position := val;
end;

constructor TScrollBoxEx.Create(APage: TBlockTabSheet);
begin
   inherited Create(APage);
   FPage := APage;
   Align := alClient;
   ParentFont := false;
   Font.Size := GSettings.FlowchartFontSize;
   BorderStyle := bsNone;
   Color := GSettings.DesktopColor;
   VertScrollBar.Tracking := true;
   HorzScrollBar.Tracking := true;
   PopupMenu := APage.Form.pmPages;
   OnMouseUp := BoxMouseUp;
   OnMouseWheel := BoxMouseWheel;
end;

procedure TScrollBoxEx.ScrollV(var Msg: TWMVScroll);
begin
   inherited;
   PerformFormsRepaint;
end;

procedure TScrollBoxEx.ScrollH(var Msg: TWMHScroll);
begin
   inherited;
   PerformFormsRepaint;
end;

function TScrollBoxEx.GetBottomRight: TPoint;
var
   cPoint, sPoint: TPoint;
   i: integer;
begin
   result := TPoint.Zero;
   sPoint := Point(HorzScrollBar.Position, VertScrollBar.Position);
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i].Visible then
      begin
         cPoint := Controls[i].BoundsRect.BottomRight + sPoint;
         if cPoint.X > result.X then
            result.X := cPoint.X;
         if cPoint.Y > result.Y then
            result.Y := cPoint.Y;
      end;
   end;
   result := result + Point(MARGIN_X, MARGIN_Y);
end;

procedure TScrollBoxEx.SetScrollBars;
var
   pnt: TPoint;
begin
   pnt := GetBottomRight;
   if pnt.X > ClientWidth then
      HorzScrollBar.Range := pnt.X
   else
      HorzScrollBar.Range := ClientWidth;
   if pnt.Y > ClientHeight then
      VertScrollBar.Range := pnt.Y
   else
      VertScrollBar.Range := ClientHeight;
   NavigatorForm.Invalidate;
end;

procedure TScrollBoxEx.PerformFormsRepaint;
begin
   if GSettings.EnableDBuffering or NavigatorForm.Visible then
      Repaint;
   NavigatorForm.Invalidate;
end;

procedure TScrollBoxEx.BoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pnt: TPoint;
begin
   if (Button = mbRight) and (GProject <> nil) then
   begin
      pnt := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      PopupMenu.Popup(pnt.X, pnt.Y);
   end;
end;

procedure TScrollBoxEx.BoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   y: integer;
begin
   y := 0;
   if Key = VK_NEXT then
      y := 15 * 20
   else if Key = VK_PRIOR then
      y := -15 * 20
   else if (Sender <> FPage.Form) or (ssCtrl in Shift) then
   begin
      case Key of
         VK_DOWN, VK_RIGHT: y := 15;
         VK_UP, VK_LEFT:    y := -15;
      end;
   end;
   if y <> 0 then
   begin
      if Key in [VK_LEFT, VK_RIGHT] then
         HorzScrollBar.Position := HorzScrollBar.Position + y
      else
         VertScrollBar.Position := VertScrollBar.Position + y;
      Key := 0;
      PerformFormsRepaint;
   end;
end;

procedure TScrollBoxEx.BoxMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   w: Word;
begin
   if WheelDelta < 0 then
      w := VK_DOWN
   else
      w := VK_UP;
   BoxKeyDown(Self, w, [ssCtrl]);
end;

function TScrollBoxEx.GetDisplayRect: TRect;
begin
   result.Top := VertScrollBar.Position;
   result.Left := HorzScrollBar.Position;
   result.Bottom := result.Top + ClientHeight;
   result.Right := result.Left + ClientWidth;
end;

// don't remove this method
procedure TScrollBoxEx.AutoScrollInView(AControl: TControl);
begin
   //inherited AutoScrollInView(AControl);
end;

procedure TScrollBoxEx.PaintToCanvas(ACanvas: TCanvas);
var
   lWnd: THandle;
   i: integer;
   wCtrl: TWinControl;
begin
   ACanvas.Brush.Style := bsSolid;
   ACanvas.Brush.Color := Color;
   PatBlt(ACanvas.Handle, ACanvas.ClipRect.Left, ACanvas.ClipRect.Top, ACanvas.ClipRect.Right, ACanvas.ClipRect.Bottom, PATCOPY);
   lWnd := GetWindow(GetTopWindow(Handle), GW_HWNDLAST);
   while lWnd <> 0 do
   begin
      for i := 0 to ControlCount-1 do
      begin
         if Controls[i].Visible and (Controls[i] is TWinControl) then
         begin
            wCtrl := TWinControl(Controls[i]);
            if wCtrl.Handle = lWnd then
            begin
               wCtrl.PaintTo(ACanvas, wCtrl.Left + HorzScrollBar.Position, wCtrl.Top + VertScrollBar.Position);
               break;
            end;
         end;
      end;
      lWnd := GetNextWindow(lWnd, GW_HWNDPREV);
   end;
end;

end.

