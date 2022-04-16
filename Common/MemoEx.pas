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
   Vcl.StdCtrls, Vcl.Forms, System.Classes, System.UITypes, OmniXML, CommonTypes;

type
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
         procedure SetAlignment(AValue: TAlignment);
         procedure ChangeBorderStyle(AStyle: TBorderStyle);
      public
         EditFormWidth,
         EditFormHeight: integer;
         property HasVScroll: boolean read FHasVScroll write SetHasVScroll;
         property HasHScroll: boolean read FHasHScroll write SetHasHScroll;
         constructor Create(AOwner: TComponent); override;
         procedure UpdateScrolls;
         procedure GetFromXML(ATag: IXMLElement);
         procedure SaveInXML(ATag: IXMLElement);
         function Clone(AOwner: TComponent): TMemoEx;
         procedure CloneFrom(AMemo: TMemoEx);
      published
         property OnDblClick;
         property OnMouseDown;
         property OnKeyUp;
         property OnChange;
         property PopupMenu;
         property Font;
         property Color;
         property BorderStyle;
         property WordWrap write SetWordWrap;
         property ScrollBars write SetScrollBars;
         property Alignment write SetAlignment;
   end;

implementation

uses
   WinApi.Windows, WinApi.Messages, System.StrUtils, System.SysUtils, System.Rtti,
   XMLProcessor, Infrastructure;

constructor TMemoEx.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   EditFormWidth := 280;
   EditFormHeight := 182;
   FHasVScroll := true;
   ParentCtl3D := false;
   BorderStyle := bsNone;
   Ctl3D := false;
   OnKeyDown := TInfra.OnKeyDownSelectAll;
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
   var pnt := TInfra.GetScrolledPoint(AMemo);
   Perform(EM_LINESCROLL, pnt.X, pnt.Y);
end;


procedure TMemoEx.SetHasHScroll(AValue: boolean);
begin
   if AValue <> FHasHScroll then
   begin
      FHasHScroll := AValue;
      if FHasHScroll then
         WordWrap := false;
      UpdateHScroll;
   end;
end;

procedure TMemoEx.SetWordWrap(AValue: boolean);
begin
   if AValue and not WordWrap then
      SetHasHScroll(false);
   inherited SetWordWrap(AValue);
end;

procedure TMemoEx.SetAlignment(AValue: TAlignment);
begin
   if AValue <> Alignment then
   begin
      var pnt := TInfra.GetScrolledPoint(Self);
      inherited SetAlignment(AValue);
      Perform(EM_LINESCROLL, pnt.X, pnt.Y);
   end;
end;

procedure TMemoEx.SetScrollBars(AValue: TScrollStyle);
begin
   if AValue <> ScrollBars then
   begin
      var pnt := TInfra.GetScrolledPoint(Self);
      inherited SetScrollBars(AValue);
      Perform(EM_LINESCROLL, pnt.X, pnt.Y);
   end;
end;

procedure TMemoEx.ChangeBorderStyle(AStyle: TBorderStyle);
begin
   if AStyle <> BorderStyle then
   begin
      var pnt := TInfra.GetScrolledPoint(Self);
      BorderStyle := AStyle;
      Perform(EM_LINESCROLL, pnt.X, pnt.Y);
   end;
end;

procedure TMemoEx.ResetScrollBars(const AStyle: TScrollStyle);
var
   sStyle: TScrollStyle;
begin
   if AStyle = TScrollStyle.ssVertical then
      sStyle := TScrollStyle.ssHorizontal
   else
      sStyle := TScrollStyle.ssVertical;
   if ScrollBars = TScrollStyle.ssBoth then
      ScrollBars := AStyle
   else if ScrollBars = sStyle then
      ScrollBars := TScrollStyle.ssNone;
end;

procedure TMemoEx.UpdateVScroll;
var
   count, lineCount: integer;
   oldFont: HFont;
   hnd: THandle;
   txtMetric: TTextMetric;
   r: TRect;
begin
   if FHasVScroll then
   begin
      hnd := GetDC(Handle);
      try
         oldFont := SelectObject(hnd, Font.Handle);
         try
            Perform(EM_GETRECT, 0, LPARAM(@r));
            GetTextMetrics(hnd, txtMetric);
            lineCount := r.Height div txtMetric.tmHeight;
         finally
            SelectObject(hnd, oldFont);
         end;
      finally
         ReleaseDC(Handle, hnd);
      end;
      count := Lines.Count;
      if EndsText(sLineBreak, Text) then
         count := count + 1;
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
var
   cnt, w, i: integer;
   r: TRect;
begin
   if FHasHScroll and not WordWrap then
   begin
      w := 0;
      for i := 0 to Lines.Count-1 do
      begin
         cnt := TInfra.GetTextWidth(Lines[i], Self);
         if cnt > w then
            w := cnt;
      end;
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

procedure TMemoEx.GetFromXML(ATag: IXMLElement);
begin
   if ATag <> nil then
   begin
      EditFormWidth := TXMLProcessor.GetIntFromAttr(ATag, 'memW', EditFormWidth);
      EditFormHeight := TXMLProcessor.GetIntFromAttr(ATag, 'memH', EditFormHeight);
      HasVScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_vscroll', FHasVScroll);
      HasHScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_hscroll', FHasHScroll);
      WordWrap := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_wordwrap', WordWrap);
      var h := TXMLProcessor.GetIntFromAttr(ATag, 'mem_hscroll_pos');
      var v := TXMLProcessor.GetIntFromAttr(ATag, 'mem_vscroll_pos');
      Perform(EM_LINESCROLL, h, v);
      var val := ATag.GetAttribute('mem_align');
      if not val.IsEmpty then
         Alignment := TRttiEnumerationType.GetValue<TAlignment>(val);
   end;
end;

procedure TMemoEx.SaveInXML(ATag: IXMLElement);
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute('memW', EditFormWidth.ToString);
      ATag.SetAttribute('memH', EditFormHeight.ToString);
      ATag.SetAttribute('mem_vscroll', HasVScroll.ToString);
      ATag.SetAttribute('mem_hscroll', HasHScroll.ToString);
      ATag.SetAttribute('mem_wordwrap', WordWrap.ToString);
      ATag.SetAttribute('mem_align', TRttiEnumerationType.GetName(Alignment));
      var pnt := TInfra.GetScrolledPoint(Self);
      ATag.SetAttribute('mem_hscroll_pos', pnt.X.ToString);
      ATag.SetAttribute('mem_vscroll_pos', pnt.Y.ToString);
   end;
end;

end.
