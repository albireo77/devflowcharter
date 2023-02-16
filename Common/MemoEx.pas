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



unit MemoEx;

interface

uses
   Vcl.StdCtrls, Vcl.Forms, System.Classes, System.UITypes, Vcl.Controls, OmniXML, CommonTypes;

type

   TWinControlHack = class(TWinControl);

   TMemoEx = class(TCustomMemo)
      private
         FHasVScroll,
         FHasHScroll: boolean;
         procedure ResetScrollBars(const AStyle: TScrollStyle);
         procedure UpdateVScroll;
         procedure UpdateHScroll;
         procedure SetHasVScroll(AValue: boolean);
         procedure SetHasHScroll(AValue: boolean);
      protected
         procedure SetWordWrap(AValue: boolean);
         procedure SetScrollBars(AValue: TScrollStyle);
         procedure SetAlignment(AValue: TAlignment); virtual;
         procedure ChangeBorderStyle(AStyle: TBorderStyle);
         procedure KeyDown(var Key: Word; Shift: TShiftState); override;
         procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
         procedure Change; override;
      public
         EditFormWidth,
         EditFormHeight: integer;
         property HasVScroll: boolean read FHasVScroll write SetHasVScroll;
         property HasHScroll: boolean read FHasHScroll write SetHasHScroll;
         constructor Create(AOwner: TComponent); override;
         procedure UpdateScrolls;
         procedure GetFromXML(ANode: IXMLNode);
         procedure SaveInXML(ANode: IXMLNode);
         function Clone(AOwner: TComponent): TMemoEx;
         procedure CloneFrom(AMemo: TMemoEx);
      published
         property OnChange;
         property PopupMenu;
         property Font;
         property Color;
         property BorderStyle;
         property WordWrap write SetWordWrap;
   end;

implementation

uses
   WinApi.Windows, WinApi.Messages, System.StrUtils, System.SysUtils, System.Rtti,
   OmniXMLUtils, Infrastructure, Navigator_Form;

constructor TMemoEx.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   EditMargins.Auto := True;
   EditFormWidth := 280;
   EditFormHeight := 182;
   FHasVScroll := True;
   ParentCtl3D := False;
   BorderStyle := bsNone;
   Ctl3D := False;
end;

procedure TMemoEx.KeyDown(var Key: Word; Shift: TShiftState);
begin
   inherited;
   if (ssCtrl in Shift) and (Key = Ord('A')) then
      SelectAll;
end;

procedure TMemoEx.SetHasVScroll(AValue: boolean);
begin
   if AValue <> FHasVScroll then
   begin
      FHasVScroll := AValue;
      UpdateVScroll;
   end;
end;

function TMemoEx.Clone(AOwner: TComponent): TMemoEx;
begin
   result := TMemoEx.Create(AOwner);
   result.CloneFrom(Self);
end;

procedure TMemoEx.CloneFrom(AMemo: TMemoEx);
begin
   Visible := AMemo.Visible;
   SetBounds(Left, Top, AMemo.Width, AMemo.Height);
   Font.Assign(AMemo.Font);
   Text := AMemo.Text;
   WordWrap := AMemo.WordWrap;
   Alignment := AMemo.Alignment;
   EditFormWidth := AMemo.EditFormWidth;
   EditFormHeight := AMemo.EditFormHeight;
   HasVScroll := AMemo.HasVScroll;
   HasHScroll := AMemo.HasHScroll;
   var pos := TInfra.GetScrolledPos(AMemo);
   Perform(EM_LINESCROLL, pos.X, pos.Y);
end;

procedure TMemoEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   if Button = mbLeft then
      TWinControlHack(Parent).MouseDown(Button, Shift, X+Left, Y+Top);
end;

procedure TMemoEx.Change;
begin
   inherited;
   UpdateScrolls;
   if NavigatorForm.InvalidateIndicator then
      NavigatorForm.Invalidate;
end;

procedure TMemoEx.SetHasHScroll(AValue: boolean);
begin
   if AValue <> FHasHScroll then
   begin
      FHasHScroll := AValue;
      if FHasHScroll then
         WordWrap := False;
      UpdateHScroll;
   end;
end;

procedure TMemoEx.SetWordWrap(AValue: boolean);
begin
   if AValue and not WordWrap then
      SetHasHScroll(False);
   inherited SetWordWrap(AValue);
end;

procedure TMemoEx.SetAlignment(AValue: TAlignment);
begin
   if AValue <> Alignment then
   begin
      var pos := TInfra.GetScrolledPos(Self);
      inherited SetAlignment(AValue);
      Perform(EM_LINESCROLL, pos.X, pos.Y);
   end;
end;

procedure TMemoEx.SetScrollBars(AValue: TScrollStyle);
begin
   if AValue <> ScrollBars then
   begin
      var pos := TInfra.GetScrolledPos(Self);
      inherited SetScrollBars(AValue);
      Perform(EM_LINESCROLL, pos.X, pos.Y);
   end;
end;

procedure TMemoEx.ChangeBorderStyle(AStyle: TBorderStyle);
begin
   if AStyle <> BorderStyle then
   begin
      var pos := TInfra.GetScrolledPos(Self);
      BorderStyle := AStyle;
      Perform(EM_LINESCROLL, pos.X, pos.Y);
   end;
end;

procedure TMemoEx.ResetScrollBars(const AStyle: TScrollStyle);
begin
   var sStyle := TScrollStyle.ssVertical; 
   if AStyle = TScrollStyle.ssVertical then
      sStyle := TScrollStyle.ssHorizontal;
   if ScrollBars = TScrollStyle.ssBoth then
      ScrollBars := AStyle
   else if ScrollBars = sStyle then
      ScrollBars := TScrollStyle.ssNone;
end;

procedure TMemoEx.UpdateVScroll;
begin
   if FHasVScroll then
   begin
      var lineCount := 0;
      var hnd := GetDC(Handle);
      try
         var oldFont := SelectObject(hnd, Font.Handle);
         try
            var r: TRect;
            var txtMetrics: TTextMetric;
            Perform(EM_GETRECT, 0, LPARAM(@r));
            GetTextMetrics(hnd, txtMetrics);
            lineCount := r.Height div txtMetrics.tmHeight;
         finally
            SelectObject(hnd, oldFont);
         end;
      finally
         ReleaseDC(Handle, hnd);
      end;
      var count := Lines.Count;
      if EndsText(sLineBreak, Text) then
         Inc(count);
      if count > lineCount then
      begin
         if ScrollBars = TScrollStyle.ssNone then
            ScrollBars := TScrollStyle.ssVertical
         else if ScrollBars = TScrollStyle.ssHorizontal then
            ScrollBars := TScrollStyle.ssBoth;
      end
      else
         ResetScrollBars(TScrollStyle.ssHorizontal);
   end
   else
      ResetScrollBars(TScrollStyle.ssHorizontal);
end;

procedure TMemoEx.UpdateHScroll;
begin
   if FHasHScroll and not WordWrap then
   begin
      var w := 0;
      for var i := 0 to Lines.Count-1 do
      begin
         var cnt := TInfra.GetTextWidth(Lines[i], Self);
         if cnt > w then
            w := cnt;
      end;
      var r: TRect;
      Perform(EM_GETRECT, 0, LPARAM(@r));
      if w > r.Width then
      begin
         if ScrollBars = TScrollStyle.ssNone then
            ScrollBars := TScrollStyle.ssHorizontal
         else if ScrollBars = TScrollStyle.ssVertical then
            ScrollBars := TScrollStyle.ssBoth;
      end
      else
         ResetScrollBars(TScrollStyle.ssVertical);
   end
   else
      ResetScrollBars(TScrollStyle.ssVertical);
end;

procedure TMemoEx.UpdateScrolls;
begin
   UpdateVScroll;
   UpdateHScroll;
   Repaint;
end;

procedure TMemoEx.GetFromXML(ANode: IXMLNode);
begin
   if ANode <> nil then
   begin
      EditFormWidth := GetNodeAttrInt(ANode, 'memW');
      EditFormHeight := GetNodeAttrInt(ANode, 'memH');
      HasVScroll := GetNodeAttrBool(ANode, 'mem_vscroll');
      HasHScroll := GetNodeAttrBool(ANode, 'mem_hscroll');
      WordWrap := GetNodeAttrBool(ANode, 'mem_wordwrap');
      var h := GetNodeAttrInt(ANode, 'mem_hscroll_pos');
      var v := GetNodeAttrInt(ANode, 'mem_vscroll_pos');
      Perform(EM_LINESCROLL, h, v);
      Alignment := TRttiEnumerationType.GetValue<TAlignment>(GetNodeAttrStr(ANode, 'mem_align'));
   end;
end;

procedure TMemoEx.SaveInXML(ANode: IXMLNode);
begin
   if ANode <> nil then
   begin
      SetNodeAttrInt(ANode, 'memW', EditFormWidth);
      SetNodeAttrInt(ANode, 'memH', EditFormHeight);
      SetNodeAttrBool(ANode, 'mem_vscroll', HasVScroll);
      SetNodeAttrBool(ANode, 'mem_hscroll', HasHScroll);
      SetNodeAttrBool(ANode, 'mem_wordwrap', WordWrap);
      SetNodeAttrStr(ANode, 'mem_align', TRttiEnumerationType.GetName(Alignment));
      var pos := TInfra.GetScrolledPos(Self);
      SetNodeAttrInt(ANode, 'mem_hscroll_pos', pos.X);
      SetNodeAttrInt(ANode, 'mem_vscroll_pos', pos.Y);
   end;
end;

end.
