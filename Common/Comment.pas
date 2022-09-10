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
   WinApi.Messages, Vcl.Controls, System.Types, System.Classes, OmniXML, Interfaces,
   BlockTabSheet, MemoEx;

type

   TComment = class(TMemoEx, IXMLable, IWinControl, IGenericComparable, IMemoEx)
      private
         FPinControl: TControl;
         FPage: TBlockTabSheet;
         FActive: boolean;
         FZOrder: integer;
      protected
         FMouseLeave: boolean;
         procedure OnMouseDownComment(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure OnMouseMoveComment(Sender: TObject; Shift: TShiftState; X, Y: Integer);
         procedure OnDblClickComment(Sender: TObject);
         procedure OnContextPopupComment(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
         procedure OnMouseLeaveComment(Sender: TObject);
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         procedure WMExitSizeMove(var Msg: TWMMove); message WM_EXITSIZEMOVE;
         procedure SetActive(AValue: boolean);
         procedure SetAlignment(AValue: TAlignment); override;
         function GetActive: boolean;
         procedure OnChangeComment(Sender: TObject);
         procedure SetPage(APage: TBlockTabSheet);
         procedure SetIsHeader(AValue: boolean);
         function GetIsHeader: boolean;
         procedure OnEndDragComment(Sender, Target: TObject; X, Y: Integer);
      public
         property PinControl: TControl read FPinControl write FPinControl;
         property Page: TBlockTabSheet read FPage write SetPage;
         property IsHeader: boolean read GetIsHeader write SetIsHeader;
         constructor Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer);
         constructor CreateDefault(APage: TBlockTabSheet);
         function Clone(APage: TBlockTabSheet; const ATopLeft: TPoint): TComment; overload;
         function Clone(APage: TBlockTabSheet): TComment; overload;
         destructor Destroy; override;
         procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl);
         procedure ExportToXMLTag(ATag: IXMLElement);
         procedure ExportToXMLTag2(ATag: IXMLElement);
         function GetHandle: THandle;
         procedure BringAllToFront;
         procedure SetZOrder(AValue: integer);
         function GetZOrder: integer;
         function GetCompareValue(ACompareType: integer): integer;
         function GetMemoEx: TMemoEx;
   end;

implementation

uses
   Vcl.Graphics, Vcl.Forms, System.SysUtils, System.UITypes, WinApi.Windows, Infrastructure,
   XMLProcessor, UserFunction, Main_Block, Navigator_Form, Constants;

constructor TComment.Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer);
begin
   inherited Create(APage.Form);
   Parent := APage.Box;
   FPage := APage;
   Color := APage.Box.Color;
   Font.Size := GSettings.FlowchartFontSize;
   Font.Color := clNavy;
   Font.Name := GSettings.FlowchartFontName;
   FActive := true;
   DoubleBuffered := true;
   Constraints.MinWidth := 25;
   Constraints.MinHeight := 25;
   FZOrder := -1;
   PopupMenu := APage.Box.PopupMenu;
   FMouseLeave := true;
   SetBounds(ALeft, ATop, AWidth, AHeight);
   GProject.AddComponent(Self);

   OnEndDrag      := OnEndDragComment;
   OnMouseDown    := OnMouseDownComment;
   OnMouseMove    := OnMouseMoveComment;
   OnDblClick     := OnDblClickComment;
   OnChange       := OnChangeComment;
   OnContextPopup := OnContextPopupComment;
   OnMouseLeave   := OnMouseLeaveComment;
end;

function TComment.Clone(APage: TBlockTabSheet; const ATopLeft: TPoint): TComment;
begin
   result := TComment.Create(APage, ATopLeft.X, ATopLeft.Y, Width, Height);
   result.CloneFrom(Self);
end;

function TComment.Clone(APage: TBlockTabSheet): TComment;
begin
   result := TComment.Create(APage, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

constructor TComment.CreateDefault(APage: TBlockTabSheet);
begin
   Create(APage, 20, 20, 150, 50);
end;

destructor TComment.Destroy;
begin
   Hide;
   FPage.Box.SetScrollBars;
   IsHeader := false;
   inherited Destroy;
end;

function TComment.GetMemoEx: TMemoEx;
begin
   result := Self;
end;

procedure TComment.OnMouseLeaveComment(Sender: TObject);
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
      Parent := APage.Box;
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

procedure TComment.SetAlignment(AValue: TAlignment);
begin
   if AValue <> Alignment then
   begin
      inherited SetAlignment(AValue);
      NavigatorForm.Invalidate;
   end;
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

function TComment.GetIsHeader: boolean;
begin
   result := GProject.HeaderComment = Self;
end;

procedure TComment.SetIsHeader(AValue: boolean);
begin
   if AValue then
      GProject.HeaderComment := Self
   else if GProject.HeaderComment = Self then
      GProject.HeaderComment := nil;
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
         var br := BoundsRect;
         var b := br.Bottom;
         var r := br.Right;
         ReleaseCapture;
         SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
         br := BoundsRect;
         if (b <> br.Bottom) or (r <> br.Right) then
            FPage.Box.SetScrollBars;
      end;
   end;
end;

function TComment.GetHandle: THandle;
begin
   result := 0;
   if Visible then
      result := Handle;
end;

procedure TComment.OnChangeComment(Sender: TObject);
begin
   GProject.SetChanged;
   if IsHeader then
      TInfra.UpdateCodeEditor;
   UpdateScrolls;
   NavigatorForm.Invalidate;
end;

procedure TComment.OnDblClickComment(Sender: TObject);
begin
   SelectAll;
end;

procedure TComment.OnMouseMoveComment(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   var pnt := Point(X, Y);
   if Rect(Width-5, 0, Width, Height-5).Contains(pnt) then
      Cursor := crSizeWE
   else if Rect(0, Height-5, Width-5, Height).Contains(pnt) then
      Cursor := crSizeNS
   else if Rect(Width-5, Height-5, Width, Height).Contains(pnt) then
      Cursor := crSizeNWSE
   else
      Cursor := crDefault;
end;

procedure TComment.NCHitTest(var Msg: TWMNCHitTest);
begin
   inherited;
   ChangeBorderStyle(bsSingle);
   FMouseLeave := true;
   if GetAsyncKeyState(vkLButton) <> 0 then
   begin
      FMouseLeave := false;
      case Cursor of
         crSizeWE:   Msg.Result := HTRIGHT;
         crSizeNS:   Msg.Result := HTBOTTOM;
         crSizeNWSE: Msg.Result := HTBOTTOMRIGHT;
      end;
   end;
end;

procedure TComment.WMExitSizeMove(var Msg: TWMMove);
begin
   inherited;
   ChangeBorderStyle(bsNone);
   UpdateScrolls;
   FPage.Box.SetScrollBars;
end;

procedure TComment.OnContextPopupComment(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
   Handled := true;
   var pnt := ClientToScreen(MousePos);
   PopupMenu.PopupComponent := Self;
   PopupMenu.Popup(pnt.X, pnt.Y);
end;

procedure TComment.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl);
begin
   if ATag <> nil then
   begin
      SetBounds(TXMLProcessor.GetIntFromAttr(ATag, 'x'),
                TXMLProcessor.GetIntFromAttr(ATag, 'y'),
                TXMLProcessor.GetIntFromAttr(ATag, 'w'),
                TXMLProcessor.GetIntFromAttr(ATag, 'h'));
      var v := TXMLProcessor.GetIntFromAttr(ATag, FONT_SIZE_ATTR);
      if v in FLOWCHART_VALID_FONT_SIZES then
         Font.Size := v;
      FZOrder := TXMLProcessor.GetIntFromAttr(ATag, Z_ORDER_ATTR, -1);
      v := TXMLProcessor.GetIntFromAttr(ATag, FONT_STYLE_ATTR);
      if v > 0 then
         Font.Style := TInfra.DecodeFontStyle(v);
      Text := ATag.Text;
      Visible := TXMLProcessor.GetBoolFromAttr(ATag, 'v');
      FPinControl := APinControl;
      IsHeader := TXMlProcessor.GetBoolFromAttr(ATag, IS_HEADER_ATTR);
      GetFromXML(ATag);
   end;
end;

procedure TComment.ExportToXMLTag(ATag: IXMLElement);
begin
   if (FPinControl = nil) and (GProject.FindMainBlockForControl(Self) = nil) then
      ExportToXMLTag2(ATag);
end;

procedure TComment.ExportToXMLTag2(ATag: IXMLElement);
begin
   if ATag <> nil then
   begin
      var tag := ATag.OwnerDocument.CreateElement(COMMENT_ATTR);
      TXMLProcessor.AddCDATA(tag, Text);
      tag.SetAttribute('x', Left.ToString);
      tag.SetAttribute('y', Top.ToString);
      tag.SetAttribute('w', Width.ToString);
      tag.SetAttribute('h', Height.ToString);
      tag.SetAttribute(FONT_SIZE_ATTR, Font.Size.ToString);
      tag.SetAttribute('v', Visible.ToString);
      tag.SetAttribute(Z_ORDER_ATTR, FZOrder.ToString);
      tag.SetAttribute(IS_HEADER_ATTR, IsHeader.ToString);
      if not FPage.IsMain then
         tag.SetAttribute(PAGE_CAPTION_ATTR, FPage.Caption);
      if Font.Style <> [] then
         tag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style).ToString);
      SaveInXML(tag);
      ATag.AppendChild(tag);
   end;
end;

procedure TComment.OnEndDragComment(Sender, Target: TObject; X, Y: Integer);
begin
   ChangeBorderStyle(bsNone);
   inherited;
end;

end.
