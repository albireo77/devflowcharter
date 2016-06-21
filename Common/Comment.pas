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



unit Comment;

interface

uses
   Windows, Messages, Controls, Forms, StdCtrls, Graphics, Classes, OmniXML, CommonInterfaces,
   BlockTabSheet;

type

   TComment = class(TMemo, IXMLable, IWinControl, IMaxBoundable, ISortable)
      private
         FPinControl: TControl;
         FPage: TBlockTabSheet;
         FActive: boolean;
         FZOrderValue: integer;
      protected
         procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
         procedure MyOnDblClick(Sender: TObject);
         procedure MyOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
         procedure MyOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
         procedure SetActive(const AValue: boolean);
         function GetActive: boolean;
         procedure MyOnChange(Sender: TObject);
         procedure SetPage(APage: TBlockTabSheet);
      public
         property PinControl: TControl read FPinControl write FPinControl;
         property Page: TBlockTabSheet read FPage write SetPage;
         constructor Create(const APage: TBlockTabSheet; const ALeft, ATop, AWidth, AHeight: Integer; const AUpdateZOrderComponents: boolean = true);
         constructor CreateDefault(const APage: TBlockTabSheet);
         destructor Destroy; override;
         procedure ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl);
         procedure ExportToXMLTag(const ATag: IXMLElement);
         procedure ExportToXMLTag2(const ATag: IXMLElement);
         procedure PaintToCanvas(const ACanvas: TCanvas);
         function GetMaxBounds: TPoint;
         function GetHandle: THandle;
         procedure BringAllToFront;
         procedure SetZOrderValue(const AValue: integer);
         function GetZOrderValue: integer;
         function GetSortValue(const ASortType: integer): integer;
   end;

implementation

uses
   ApplicationCommon, SysUtils, XMLProcessor, UserFunction, Main_Block, Navigator_Form;

constructor TComment.Create(const APage: TBlockTabSheet; const ALeft, ATop, AWidth, AHeight: Integer; const AUpdateZOrderComponents: boolean = true);
begin

   inherited Create(APage.Form);

   FPage := APage;
   Parent := APage;
   Color := APage.Brush.Color;
   Font.Size := 8;
   Font.Color := clNavy;
   Font.Name := GSettings.FlowchartFontName;
   FActive := true;
   DoubleBuffered := true;
   Constraints.MinWidth := 25;
   Constraints.MinHeight := 25;
   BorderStyle := bsNone;
   Ctl3D := false;
   FZOrderValue := -1;
   PopupMenu := APage.Form.pmPages;
   SetBounds(ALeft, ATop, AWidth, AHeight);
   GProject.AddComponent(Self);

   OnKeyDown   := MyOnKeyDown;
   OnMouseDown := MouseDown;
   OnMouseMove := MouseMove;
   OnDblClick  := MyOnDblClick;
   OnChange    := MyOnChange;
   OnContextPopup := MyOnContextPopup;
end;

constructor TComment.CreateDefault(const APage: TBlockTabSheet);
begin
   Create(APage, 20, 20, 150, 50, false);
end;

destructor TComment.Destroy;
begin
   Hide;
   FPage.Form.SetScrollBars;
   inherited Destroy;
end;

procedure TComment.SetPage(APage: TBlockTabSheet);
begin
   if FPage <> APage then
   begin
      FPage := APage;
      Parent := APage;
   end;
end;

procedure TComment.SetActive(const AValue: boolean);
begin
   if AValue <> FActive then
      FActive := AValue;
end;

function TComment.GetActive: boolean;
begin
   result := FActive;
end;

procedure TComment.BringAllToFront;
begin
   BringToFront;
end;

procedure TComment.SetZOrderValue(const AValue: integer);
begin
   FZOrderValue := AValue;
end;

function TComment.GetZOrderValue: integer;
begin
   result := FZOrderValue;
end;

function TComment.GetSortValue(const ASortType: integer): integer;
begin
   result := FZOrderValue;
end;

procedure TComment.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then
   begin
      if ssShift in Shift then
      begin
         if Trim(Text) <> '' then
            BeginDrag(true)
      end
      else
      begin
         GChange := 1;
         BringToFront;
         ReleaseCapture;
         SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
         FPage.Form.SetScrollBars
      end;
   end;
end;

procedure TComment.PaintToCanvas(const ACanvas: TCanvas);
var
   lBStyle: TBorderStyle;
   lStart: integer;
begin
   if Visible and (FPinControl = nil) then
   begin
      ACanvas.Lock;
      try
         lBStyle := BorderStyle;
         lStart := SelStart;
         BorderStyle := bsNone;
         PaintTo(ACanvas, Left + Parent.Left, Top + Parent.Top);
         BorderStyle := lBStyle;
         SelStart := lStart;
      finally
         ACanvas.Unlock;
      end;
   end;
end;

function TComment.GetHandle: THandle;
begin
   result := 0;
   if Visible then
      result := Handle;
end;

function TComment.GetMaxBounds: TPoint;
begin
   result := Point(0, 0);
   if Visible then
   begin
      result.X := BoundsRect.Right + FPage.Form.HorzScrollBar.Position + MARGIN_X;
      result.Y := BoundsRect.Bottom + FPage.Form.VertScrollBar.Position + MARGIN_Y;
   end;
end;

procedure TComment.MyOnChange(Sender: TObject);
begin
   NavigatorForm.Repaint;
end;

procedure TComment.CMMouseLeave(var Msg: TMessage);
var
   lStart: integer;
begin
   lStart := SelStart;
   BorderStyle := bsNone;
   SelStart := lStart;
end;

procedure TComment.MyOnDblClick(Sender: TObject);
begin
   SelectAll;
end;

procedure TComment.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if PtInRect(Rect(Width-5, 0, Width, Height-5), Point(X, Y)) then
      Cursor := crSizeWE
   else if PtInRect(Rect(0, Height-5, Width-5, Height), Point(X, Y)) then
      Cursor := crSizeNS
   else if PtInRect(Rect(Width-5, Height-5, Width, Height), Point(X, Y)) then
      Cursor := crSizeNWSE
   else if Cursor <> crDefault then
      Cursor := crDefault;
end;

procedure TComment.NCHitTest(var Msg: TWMNCHitTest);
var
   lStart: integer;
begin
   inherited;
   lStart := SelStart;
   BorderStyle := bsSingle;
   SelStart := lStart;
   if GetAsyncKeyState(VK_LBUTTON) <> 0 then
   begin
      GChange := 1;
      case Cursor of
         crSizeWE:   Msg.Result := HTRIGHT;
         crSizeNS:   Msg.Result := HTBOTTOM;
         crSizeNWSE: Msg.Result := HTBOTTOMRIGHT;
      end;
   end;
end;

procedure TComment.MyOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if (ssCtrl in Shift) and (Key = Ord('A')) then
      SelectAll;
end;

procedure TComment.MyOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
   lPoint: TPoint;
begin
   Handled := true;   
   lPoint := ClientToScreen(MousePos);
   PopupMenu.PopupComponent := Self;
   PopupMenu.Popup(lPoint.X, lPoint.Y);
end;

procedure TComment.ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl);
var
   lValue: integer;
begin
   FPage.Form.VertScrollBar.Position := 0;
   FPage.Form.HorzScrollBar.Position := 0;
   SetBounds(StrToInt(rootTag.GetAttribute('x')),
             StrToInt(rootTag.GetAttribute('y')),
             StrToInt(rootTag.GetAttribute('w')),
             StrToInt(rootTag.GetAttribute('h')));
   lValue := StrToIntDef(rootTag.GetAttribute('fontsize'), 8);
   if lValue in [8, 10, 12] then
      Font.Size := lValue;
   FZOrderValue := StrToIntDef(rootTag.GetAttribute('ZOrdVal'), -1);
   lValue := StrToIntDef(rootTag.GetAttribute('fontstyle'), 0);
   if lValue > 0 then
      Font.Style := TInfra.DecodeFontStyle(lValue);
   Text := rootTag.Text;
   Visible := rootTag.GetAttribute('v') = IntToStr(Ord(true));
   FPinControl := APinControl;
end;

procedure TComment.ExportToXMLTag(const ATag: IXMLElement);
begin
   if FPinControl = nil then
      ExportToXMLTag2(ATag);
end;

procedure TComment.ExportToXMLTag2(const ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   tag := ATag.OwnerDocument.CreateElement('comment');
   TXMLProcessor.AddCDATA(tag, Text);
   tag.SetAttribute('x', IntToStr(Left + FPage.Form.HorzScrollBar.Position));
   tag.SetAttribute('y', IntToStr(Top + FPage.Form.VertScrollBar.Position));
   tag.SetAttribute('w', IntToStr(Width));
   tag.SetAttribute('h', IntToStr(Height));
   tag.SetAttribute('fontsize', IntToStr(Font.Size));
   tag.SetAttribute('v', IntToStr(Ord(Visible)));
   tag.SetAttribute('ZOrdVal', IntToStr(FZOrderValue));
   tag.SetAttribute(PAGE_CAPTION_ATTR, FPage.Caption);
   if Font.Style <> [] then
      tag.SetAttribute('fontstyle', TInfra.EncodeFontStyle(Font.Style));
   ATag.AppendChild(tag);
end;

end.
