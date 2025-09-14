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
   Vcl.StdCtrls, Vcl.Forms, System.Classes, System.UITypes, Vcl.Controls, MSXML2_TLB, CommonTypes;

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
         procedure GetFromXML(ATag: IXMLDOMElement);
         procedure SaveInXML(ATag: IXMLDOMElement);
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
   XMLUtils, Infrastructure, Navigator_Form;

constructor TMemoEx.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   StyleElements := StyleElements - [seClient];
   EditMargins.Auto := True;
   EditFormWidth := 280;
   EditFormHeight := 182;
   FHasVScroll := True;
   BorderStyle := bsNone;
end;

procedure TMemoEx.KeyDown(var Key: Word; Shift: TShiftState);
begin
   inherited;
   if (Shift = [ssCtrl]) and (Key = Ord('A')) then
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
      if EndsText(Lines.LineBreak, Text) then
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

procedure TMemoEx.GetFromXML(ATag: IXMLDOMElement);
begin
   if ATag <> nil then
   begin
      EditFormWidth := GetNodeAttrInt(ATag, 'memW', EditFormWidth);
      EditFormHeight := GetNodeAttrInt(ATag, 'memH', EditFormHeight);
      HasVScroll := GetNodeAttrBool(ATag, 'mem_vscroll', HasVScroll);
      HasHScroll := GetNodeAttrBool(ATag, 'mem_hscroll', HasHScroll);
      WordWrap := GetNodeAttrBool(ATag, 'mem_wordwrap', WordWrap);
      var h := GetNodeAttrInt(ATag, 'mem_hscroll_pos', 0);
      var v := GetNodeAttrInt(ATag, 'mem_vscroll_pos', 0);
      Perform(EM_LINESCROLL, h, v);
      Alignment := TRttiEnumerationType.GetValue<TAlignment>(GetNodeAttrStr(ATag, 'mem_align', 'taLeftJustify'));
   end;
end;

procedure TMemoEx.SaveInXML(ATag: IXMLDOMElement);
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute('memW', EditFormWidth);
      ATag.SetAttribute('memH', EditFormHeight);
      ATag.SetAttribute('mem_vscroll', HasVScroll);
      ATag.SetAttribute('mem_hscroll', HasHScroll);
      ATag.SetAttribute('mem_wordwrap', WordWrap);
      ATag.SetAttribute('mem_align', TRttiEnumerationType.GetName(Alignment));
      var pos := TInfra.GetScrolledPos(Self);
      ATag.SetAttribute('mem_hscroll_pos', pos.X);
      ATag.SetAttribute('mem_vscroll_pos', pos.Y);
   end;
end;

end.
