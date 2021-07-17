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
   Vcl.Graphics, Interfaces, Main_Form, OmniXML;

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
      function IsMain: boolean;
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
   System.SysUtils, System.StrUtils, System.Math, System.UITypes, WinApi.Windows,
   UserFunction, Navigator_Form, XMLProcessor, Infrastructure, Constants;

constructor TBlockTabSheet.Create(AMainForm: TMainForm);
begin
   inherited Create(AMainForm.pgcPages);
   PageControl := AMainForm.pgcPages;
   FForm := AMainForm;
   ParentFont := false;
   Align := alClient;
   DrawI := true;
   Box := TScrollBoxEx.Create(Self);
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

function TBlockTabSheet.IsMain: boolean;
begin
   result := (GProject <> nil) and (GProject.GetMainPage = Self);
end;

procedure TBlockTabSheet.ExportToXMLTag(ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   tag := ATag.OwnerDocument.CreateElement('page');
   ATag.AppendChild(tag);
   tag.SetAttribute('name', IfThen(IsMain, MAIN_PAGE_MARKER, Caption));
   tag.SetAttribute('hScrollRange', Box.HorzScrollBar.Range.ToString);
   tag.SetAttribute('vScrollRange', Box.VertScrollBar.Range.ToString);
   tag.SetAttribute('hScrollPos', Box.HorzScrollBar.Position.ToString);
   tag.SetAttribute('vScrollPos', Box.VertScrollBar.Position.ToString);
end;

procedure TBlockTabSheet.ImportFromXMLTag(ATag: IXMLElement);
var
   val: integer;
begin
   val := TXMLProcessor.GetInt(ATag, 'hScrollRange', -1);
   if val > -1 then
      Box.HorzScrollBar.Range := val;
   val := TXMLProcessor.GetInt(ATag, 'hScrollPos', -1);
   if val > -1 then
      Box.HorzScrollBar.Position := val;
   val := TXMLProcessor.GetInt(ATag, 'vScrollRange', -1);
   if val > -1 then
      Box.VertScrollBar.Range := val;
   val := TXMLProcessor.GetInt(ATag, 'vScrollPos', -1);
   if val > -1 then
      Box.VertScrollBar.Position := val;
end;

constructor TScrollBoxEx.Create(APage: TBlockTabSheet);
begin
   inherited Create(APage);
   Parent := APage;
   FPage := APage;
   Align := alClient;
   ParentFont := false;
   Font.Size := GSettings.FlowchartFontSize;
   BorderStyle := bsNone;
   Color := GSettings.DesktopColor;
   HorzScrollBar.Range := ClientWidth;
   VertScrollBar.Range := ClientHeight;
   HorzScrollBar.Tracking := true;
   VertScrollBar.Tracking := true;
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
   if Parent <> nil then
   begin
      pnt := GetBottomRight;
      HorzScrollBar.Range := Max(pnt.X, ClientWidth);
      VertScrollBar.Range := Max(pnt.Y, ClientHeight);
   end;
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
   if Key = vkNext then
      y := 15 * 20
   else if Key = vkPrior then
      y := -15 * 20
   else if (Sender <> FPage.Form) or (ssCtrl in Shift) then
   begin
      case Key of
         vkDown, vkRight: y := 15;
         vkUp, vkLeft:    y := -15;
      end;
   end;
   if y <> 0 then
   begin
      if Key in [vkLeft, vkRight] then
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
      w := vkDown
   else
      w := vkUp;
   BoxKeyDown(Self, w, [ssCtrl]);
end;

function TScrollBoxEx.GetDisplayRect: TRect;
begin
   result := Bounds(HorzScrollBar.Position, VertScrollBar.Position, ClientWidth, ClientHeight);
end;

// don't remove this method
procedure TScrollBoxEx.AutoScrollInView(AControl: TControl);
begin
   //inherited AutoScrollInView(AControl);
end;

procedure TScrollBoxEx.PaintToCanvas(ACanvas: TCanvas);
var
   hnd: THandle;
   winCtrl: TWinControl;
begin
   with ACanvas do
   begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Color;
      FillRect(ClipRect);
   end;
   hnd := GetWindow(GetTopWindow(Handle), GW_HWNDLAST);
   while hnd <> 0 do
   begin
      winCtrl := FindControl(hnd);
      if (winCtrl <> nil) and winCtrl.Visible then
         winCtrl.PaintTo(ACanvas, winCtrl.Left + HorzScrollBar.Position, winCtrl.Top + VertScrollBar.Position);
      hnd := GetNextWindow(hnd, GW_HWNDPREV);
   end;
end;

end.

