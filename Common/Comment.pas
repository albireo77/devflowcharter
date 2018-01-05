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
         procedure ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
         procedure MyOnMouseLeave(Sender: TObject);
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         procedure SetActive(AValue: boolean);
         function GetActive: boolean;
         procedure OnChangeComment(Sender: TObject);
         procedure SetPage(APage: TBlockTabSheet);
         procedure SetIsHeader(AValue: boolean);
         function GetIsHeader: boolean;
      public
         property PinControl: TControl read FPinControl write FPinControl;
         property Page: TBlockTabSheet read FPage write SetPage;
         property IsHeader: boolean read GetIsHeader write SetIsHeader;
         constructor Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer);
         constructor CreateDefault(APage: TBlockTabSheet);
         function Clone(APage: TBlockTabSheet; ATopLeft: PPoint = nil): TComment;
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
   Vcl.Graphics, System.SysUtils, System.UITypes, System.Types, ApplicationCommon,
   XMLProcessor, UserFunction, Main_Block, Navigator_Form;

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
   BorderStyle := bsNone;
   Ctl3D := false;
   FZOrder := -1;
   PopupMenu := APage.Box.PopupMenu;
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
   pnt: TPoint;
begin
   if ATopLeft = nil then
   begin
      pnt := BoundsRect.TopLeft;
      ATopLeft := @pnt;
   end;
   result := TComment.Create(APage, ATopLeft^.X, ATopLeft^.Y, Width, Height);
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
         ReleaseCapture;
         SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
         FPage.Box.SetScrollBars
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
      SetBounds(ATag.GetAttribute('x').ToInteger, ATag.GetAttribute('y').ToInteger,
                ATag.GetAttribute('w').ToInteger, ATag.GetAttribute('h').ToInteger);
      v := StrToIntDef(ATag.GetAttribute(FONT_SIZE_ATTR), GSettings.FlowchartFontSize);
      if v in FLOWCHART_VALID_FONT_SIZES then
         Font.Size := v;
      FZOrder := StrToIntDef(ATag.GetAttribute(Z_ORDER_ATTR), -1);
      v := StrToIntDef(ATag.GetAttribute(FONT_STYLE_ATTR), 0);
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
      tag.SetAttribute(IS_HEADER_ATTR, IsHeader.ToString);
      if FPage <> GProject.GetMainPage then
         tag.SetAttribute(PAGE_CAPTION_ATTR, FPage.Caption);
      if Font.Style <> [] then
         tag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      SaveInXML(tag);
      ATag.AppendChild(tag);
   end;
end;

end.
