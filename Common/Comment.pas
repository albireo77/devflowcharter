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
   WinApi.Windows, WinApi.Messages, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, System.Classes,
   OmniXML, CommonInterfaces, BlockTabSheet, MemoEx;

type

   PPoint = ^TPoint;

   TComment = class(TMemoEx, IXMLable, IWinControl, IMaxBoundable, IGenericComparable, IMemoEx)
      private
         FPinControl: TControl;
         FPage: TBlockTabSheet;
         FActive,
         FIsHeader: boolean;
         FZOrder: integer;
      protected
         FMouseLeave: boolean;
         procedure OnMouseDownComment(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure OnMouseMoveComment(Sender: TObject; Shift: TShiftState; X, Y: Integer);
         procedure OnDblClickComment(Sender: TObject);
         procedure ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
         procedure MyOnMouseLeave(Sender: TObject);
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         procedure SetActive(AValue: boolean);
         function GetActive: boolean;
         procedure OnChangeComment(Sender: TObject);
         procedure SetPage(APage: TBlockTabSheet);
         procedure SetIsHeader(AValue: boolean);
      public
         property PinControl: TControl read FPinControl write FPinControl;
         property Page: TBlockTabSheet read FPage write SetPage;
         property IsHeader: boolean read FIsHeader write SetIsHeader;
         constructor Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer; AUpdateZOrderComponents: boolean = true);
         constructor CreateDefault(APage: TBlockTabSheet);
         function Clone(APage: TBlockTabSheet; ATopLeft: PPoint = nil): TComment;
         destructor Destroy; override;
         procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl);
         procedure ExportToXMLTag(ATag: IXMLElement);
         procedure ExportToXMLTag2(ATag: IXMLElement);
         function GetMaxBounds: TPoint;
         function GetHandle: THandle;
         procedure BringAllToFront;
         procedure SetZOrder(AValue: integer);
         function GetZOrder: integer;
         function GetCompareValue(ACompareType: integer): integer;
         function GetMemoEx: TMemoEx;
   end;

implementation

uses
   Vcl.Graphics, System.SysUtils, System.UITypes, System.Types, ApplicationCommon,
   XMLProcessor, UserFunction, Main_Block, Navigator_Form;

constructor TComment.Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer; AUpdateZOrderComponents: boolean = true);
begin
   inherited Create(APage.Form);
   Parent := APage;
   FPage := APage;
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
   FZOrder := -1;
   PopupMenu := APage.Form.pmPages;
   FMouseLeave := true;
   SetBounds(ALeft, ATop, AWidth, AHeight);
   GProject.AddComponent(Self);

   OnMouseDown    := OnMouseDownComment;
   OnMouseMove    := OnMouseMoveComment;
   OnDblClick     := OnDblClickComment;
   OnChange       := OnChangeComment;
   OnContextPopup := ContextPopup;
   OnMouseLeave   := MyOnMouseLeave;
end;

function TComment.Clone(APage: TBlockTabSheet; ATopLeft: PPoint = nil): TComment;
var
   lTopLeft: TPoint;
begin
   if ATopLeft = nil then
      lTopLeft := BoundsRect.TopLeft
   else
      lTopLeft := ATopLeft^;
   result := TComment.Create(APage, lTopLeft.X, lTopLeft.Y, Width, Height);
   result.Font.Assign(Font);
   result.Text := Text;
   result.Visible := Visible;
end;

constructor TComment.CreateDefault(APage: TBlockTabSheet);
begin
   Create(APage, 20, 20, 150, 50, false);
end;

destructor TComment.Destroy;
begin
   Hide;
   FPage.Form.SetScrollBars;
   inherited Destroy;
end;

function TComment.GetMemoEx: TMemoEx;
begin
   result := Self;
end;

procedure TComment.MyOnMouseLeave(Sender: TObject);
begin
   if FMouseLeave then
   begin
      ChangeBorderStyle(bsNone);
      UpdateScrolls;
   end;
end;

procedure TComment.SetPage(APage: TBlockTabSheet);
begin
   if FPage <> APage then
   begin
      FPage := APage;
      Parent := APage;
   end;
end;

procedure TComment.SetActive(AValue: boolean);
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

procedure TComment.SetZOrder(AValue: integer);
begin
   FZOrder := FPage.PageIndex * 100 + AValue;
end;

function TComment.GetZOrder: integer;
begin
   result := FZOrder;
end;

function TComment.GetCompareValue(ACompareType: integer): integer;
begin
   result := FZOrder;
end;

procedure TComment.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
   inherited;
   if (Msg.WindowPos.x <> 0) or (Msg.WindowPos.y <> 0) then
      GProject.SetChanged;
end;

procedure TComment.SetIsHeader(AValue: boolean);
var
   comment: TComment;
begin
   if FIsHeader then
      FIsHeader := false
   else
   begin
      for comment in GProject.GetComments do
      begin
         if (comment <> Self) and comment.FIsHeader then
         begin
            comment.FIsHeader := false;
            break;
         end;
      end;
      FIsHeader := true;
   end;
end;

procedure TComment.OnMouseDownComment(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
         BringToFront;
         ReleaseCapture;
         SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
         FPage.Form.SetScrollBars
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
   result := TPoint.Zero;
   if Visible then
   begin
      result.X := BoundsRect.Right + MARGIN_X;
      result.Y := BoundsRect.Bottom + MARGIN_Y;
   end;
end;

procedure TComment.OnChangeComment(Sender: TObject);
begin
   GProject.SetChanged;
   if FIsHeader then
      TInfra.UpdateCodeEditor;
   UpdateScrolls;
   NavigatorForm.Invalidate;
end;

procedure TComment.OnDblClickComment(Sender: TObject);
begin
   SelectAll;
end;

procedure TComment.OnMouseMoveComment(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   pnt: TPoint;
begin
   pnt := Point(X, Y);
   if Rect(Width-5, 0, Width, Height-5).Contains(pnt) then
      Cursor := crSizeWE
   else if Rect(0, Height-5, Width-5, Height).Contains(pnt) then
      Cursor := crSizeNS
   else if Rect(Width-5, Height-5, Width, Height).Contains(pnt) then
      Cursor := crSizeNWSE
   else if Cursor <> crDefault then
      Cursor := crDefault;
end;

procedure TComment.NCHitTest(var Msg: TWMNCHitTest);
begin
   inherited;
   ChangeBorderStyle(bsSingle);
   FMouseLeave := true;
   if GetAsyncKeyState(VK_LBUTTON) <> 0 then
   begin
      FMouseLeave := false;
      case Cursor of
         crSizeWE:   Msg.Result := HTRIGHT;
         crSizeNS:   Msg.Result := HTBOTTOM;
         crSizeNWSE: Msg.Result := HTBOTTOMRIGHT;
      end;
   end;
end;

procedure TComment.ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
   pnt: TPoint;
begin
   Handled := true;
   pnt := ClientToScreen(MousePos);
   PopupMenu.PopupComponent := Self;
   PopupMenu.Popup(pnt.X, pnt.Y);
end;

procedure TComment.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl);
var
   v: integer;
begin
   if ATag <> nil then
   begin
      FPage.Form.VertScrollBar.Position := 0;
      FPage.Form.HorzScrollBar.Position := 0;
      SetBounds(ATag.GetAttribute('x').ToInteger, ATag.GetAttribute('y').ToInteger,
                ATag.GetAttribute('w').ToInteger, ATag.GetAttribute('h').ToInteger);
      v := StrToIntDef(ATag.GetAttribute(FONT_SIZE_ATTR), 8);
      if v in [8, 10, 12] then
         Font.Size := v;
      FZOrder := StrToIntDef(ATag.GetAttribute(Z_ORDER_ATTR), -1);
      v := StrToIntDef(ATag.GetAttribute(FONT_STYLE_ATTR), 0);
      if v > 0 then
         Font.Style := TInfra.DecodeFontStyle(v);
      Text := ATag.Text;
      Visible := TXMLProcessor.GetBoolFromAttr(ATag, 'v');
      FPinControl := APinControl;
      FIsHeader := TXMlProcessor.GetBoolFromAttr(ATag, IS_HEADER_ATTR);
      GetFromXML(ATag);
   end;
end;

procedure TComment.ExportToXMLTag(ATag: IXMLElement);
begin
   if (FPinControl = nil) and (GProject.FindMainBlockForControl(Self) = nil) then
      ExportToXMLTag2(ATag);
end;

procedure TComment.ExportToXMLTag2(ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   if ATag <> nil then
   begin
      tag := ATag.OwnerDocument.CreateElement(COMMENT_ATTR);
      TXMLProcessor.AddCDATA(tag, Text);
      tag.SetAttribute('x', Left.ToString);
      tag.SetAttribute('y', Top.ToString);
      tag.SetAttribute('w', Width.ToString);
      tag.SetAttribute('h', Height.ToString);
      tag.SetAttribute(FONT_SIZE_ATTR, Font.Size.ToString);
      tag.SetAttribute('v', Visible.ToString);
      tag.SetAttribute(Z_ORDER_ATTR, FZOrder.ToString);
      tag.SetAttribute(IS_HEADER_ATTR, FIsHeader.ToString);
      if FPage <> GProject.GetMainPage then
         tag.SetAttribute(PAGE_CAPTION_ATTR, FPage.Caption);
      if Font.Style <> [] then
         tag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      SaveInXML(tag);
      ATag.AppendChild(tag);
   end;
end;

end.
