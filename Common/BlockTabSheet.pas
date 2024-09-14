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
   Vcl.Graphics, Main_Form, OmniXML;

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
      procedure ExportToXML(ANode: IXMLNode);
      procedure ImportFromXML(ANode: IXMLNode);
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
      property Page: TBlockTabSheet read FPage;
   end;

implementation

uses
   System.StrUtils, System.Math, System.UITypes, WinApi.Windows, Navigator_Form,
   OmniXMLUtils, Infrastructure, Constants;

constructor TBlockTabSheet.Create(AMainForm: TMainForm);
begin
   inherited Create(AMainForm.pgcPages);
   PageControl := AMainForm.pgcPages;
   FForm := AMainForm;
   ParentFont := False;
   Align := alClient;
   DrawI := True;
   Box := TScrollBoxEx.Create(Self);
   Box.UseWheelForScrolling := True;
   Box.StyleElements := Box.StyleElements - [seClient];
end;

destructor TBlockTabSheet.Destroy;
begin
   if GProject <> nil then
   begin
      for var func in GProject.GetUserFunctions do
      begin
         if func.Body.Page = Self then
            func.Free;
      end;
   end;
   inherited Destroy;
end;

function TBlockTabSheet.IsMain: boolean;
begin
   result := (GProject <> nil) and (GProject.MainPage = Self);
end;

procedure TBlockTabSheet.ExportToXML(ANode: IXMLNode);
begin
   var node := AppendNode(ANode, 'page');
   SetNodeAttrStr(node, 'name', IfThen(IsMain, MAIN_PAGE_MARKER, Caption));
   SetNodeAttrInt(node, 'hScrollRange', Box.HorzScrollBar.Range);
   SetNodeAttrInt(node, 'vScrollRange', Box.VertScrollBar.Range);
   SetNodeAttrInt(node, 'hScrollPos', Box.HorzScrollBar.Position);
   SetNodeAttrInt(node, 'vScrollPos', Box.VertScrollBar.Position);
end;

procedure TBlockTabSheet.ImportFromXML(ANode: IXMLNode);
begin
   Box.HorzScrollBar.Range := GetNodeAttrInt(ANode, 'hScrollRange');
   Box.HorzScrollBar.Position := GetNodeAttrInt(ANode, 'hScrollPos');
   Box.VertScrollBar.Range := GetNodeAttrInt(ANode, 'vScrollRange');
   Box.VertScrollBar.Position := GetNodeAttrInt(ANode, 'vScrollPos');
end;

constructor TScrollBoxEx.Create(APage: TBlockTabSheet);
begin
   inherited Create(APage);
   Parent := APage;
   FPage := APage;
   Align := alClient;
   ParentFont := False;
   Font.Size := GSettings.FlowchartFontSize;
   BorderStyle := bsNone;
   Color := GSettings.DesktopColor;
   HorzScrollBar.Range := ClientWidth;
   VertScrollBar.Range := ClientHeight;
   HorzScrollBar.Tracking := True;
   VertScrollBar.Tracking := True;
   PopupMenu := APage.Form.pmPages;
   OnMouseUp := BoxMouseUp;
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
begin
   result := TPoint.Zero;
   var sPoint := Point(HorzScrollBar.Position, VertScrollBar.Position);
   for var control in GetControls([ceftVisible]) do
   begin
      var cPoint := control.BoundsRect.BottomRight + sPoint;
      if cPoint.X > result.X then
         result.X := cPoint.X;
      if cPoint.Y > result.Y then
         result.Y := cPoint.Y;
   end;
   result := result + Point(MARGIN_X, MARGIN_Y);
end;

procedure TScrollBoxEx.SetScrollBars;
begin
   if Parent <> nil then
   begin
      var pnt := GetBottomRight;
      var hr := Max(pnt.X, ClientWidth);
      var vr := Max(pnt.Y, ClientHeight);
      if HorzScrollBar.Range <> hr then
         HorzScrollBar.Range := hr;
      if VertScrollBar.Range <> vr then
         VertScrollBar.Range := vr;
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
begin
   if (Button = mbRight) and (GProject <> nil) then
   begin
      var pnt := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      PopupMenu.Popup(pnt.X, pnt.Y);
   end;
end;

procedure TScrollBoxEx.BoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   var y := 0;
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
begin
   ACanvas.Brush.Style := bsSolid;
   ACanvas.Brush.Color := Color;
   ACanvas.FillRect(ACanvas.ClipRect);
   ACanvas.Lock;
   try
      var hnd := GetWindow(GetTopWindow(Handle), GW_HWNDLAST);
      while hnd <> 0 do
      begin
         var winCtrl := FindControl(hnd);
         if (winCtrl <> nil) and winCtrl.Visible then
            winCtrl.PaintTo(ACanvas, winCtrl.Left + HorzScrollBar.Position, winCtrl.Top + VertScrollBar.Position);
         hnd := GetNextWindow(hnd, GW_HWNDPREV);
      end;
   finally
      ACanvas.Unlock;
   end;
end;

end.

