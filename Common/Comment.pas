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
         procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
         procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
         procedure DblClick; override;
         procedure OnContextPopupComment(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
         procedure OnMouseLeaveComment(Sender: TObject);
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         procedure WMExitSizeMove(var Msg: TWMMove); message WM_EXITSIZEMOVE;
         procedure SetActive(AValue: boolean);
         procedure SetAlignment(AValue: TAlignment); override;
         function GetActive: boolean;
         procedure Change; override;
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
         procedure ImportFromXML(ANode: IXMLNode; APinControl: TControl);
         procedure ExportToXML(ANode: IXMLNode);
         procedure ExportToXML2(ANode: IXMLNode);
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
   OmniXMLUtils, UserFunction, Main_Block, Navigator_Form, Constants;

constructor TComment.Create(APage: TBlockTabSheet; ALeft, ATop, AWidth, AHeight: Integer);
begin
   inherited Create(APage.Form);
   Parent := APage.Box;
   FPage := APage;
   Color := APage.Box.Color;
   Font.Size := GSettings.FlowchartFontSize;
   Font.Color := clNavy;
   Font.Name := GSettings.FlowchartFontName;
   FActive := True;
   DoubleBuffered := True;
   Constraints.MinWidth := 25;
   Constraints.MinHeight := 25;
   FZOrder := -1;
   PopupMenu := APage.Box.PopupMenu;
   FMouseLeave := True;
   SetBounds(ALeft, ATop, AWidth, AHeight);
   GProject.AddComponent(Self);

   OnEndDrag := OnEndDragComment;
   OnMouseLeave := OnMouseLeaveComment;
   OnContextPopup := OnContextPopupComment;
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
   IsHeader := False;
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

procedure TComment.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   if Button = mbLeft then
   begin
      if ssShift in Shift then
      begin
         if Trim(Text) <> '' then
            BeginDrag(True)
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

procedure TComment.Change;
begin
   inherited;
   GProject.SetChanged;
   if IsHeader then
      TInfra.UpdateCodeEditor;
   UpdateScrolls;
   NavigatorForm.Invalidate;
end;

procedure TComment.DblClick;
begin
   inherited;
   SelectAll;
end;

procedure TComment.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   inherited;
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
   FMouseLeave := True;
   if GetAsyncKeyState(vkLButton) <> 0 then
   begin
      FMouseLeave := False;
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
   Handled := True;
   var pnt := ClientToScreen(MousePos);
   PopupMenu.PopupComponent := Self;
   PopupMenu.Popup(pnt.X, pnt.Y);
end;

procedure TComment.ImportFromXML(ANode: IXMLNode; APinControl: TControl);
begin
   if ANode <> nil then
   begin
      SetBounds(GetNodeAttrInt(ANode, 'x'),
                GetNodeAttrInt(ANode, 'y'),
                GetNodeAttrInt(ANode, 'w'),
                GetNodeAttrInt(ANode, 'h'));
      var v := GetNodeAttrInt(ANode, FONT_SIZE_ATTR);
      if v in FLOWCHART_VALID_FONT_SIZES then
         Font.Size := v;
      FZOrder := GetNodeAttrInt(ANode, Z_ORDER_ATTR);
      v := GetNodeAttrInt(ANode, FONT_STYLE_ATTR, 0);
      if v > 0 then
         Font.Style := TInfra.DecodeFontStyle(v);
      Text := ANode.Text;
      Visible := GetNodeAttrBool(ANode, 'v');
      FPinControl := APinControl;
      IsHeader := GetNodeAttrBool(ANode, IS_HEADER_ATTR);
      GetFromXML(ANode);
   end;
end;

procedure TComment.ExportToXML(ANode: IXMLNode);
begin
   if (FPinControl = nil) and (GProject.FindMainBlockForControl(Self) = nil) then
      ExportToXML2(ANode);
end;

procedure TComment.ExportToXML2(ANode: IXMLNode);
begin
   if ANode <> nil then
   begin
      var node := AppendNode(ANode, COMMENT_TAG);
      SetCDataChild(node, Text);
      SetNodeAttrInt(node, 'x', Left);
      SetNodeAttrInt(node, 'y', Top);
      SetNodeAttrInt(node, 'w', Width);
      SetNodeAttrInt(node, 'h', Height);
      SetNodeAttrInt(node, FONT_SIZE_ATTR, Font.Size);
      SetNodeAttrBool(node, 'v', Visible);
      SetNodeAttrInt(node, Z_ORDER_ATTR, FZOrder);
      SetNodeAttrBool(node, IS_HEADER_ATTR, IsHeader);
      if not FPage.IsMain then
         SetNodeAttrStr(node, PAGE_CAPTION_ATTR, FPage.Caption);
      if Font.Style <> [] then
         SetNodeAttrInt(node, FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      SaveInXML(node);
   end;
end;

procedure TComment.OnEndDragComment(Sender, Target: TObject; X, Y: Integer);
begin
   ChangeBorderStyle(bsNone);
   inherited;
end;

end.
