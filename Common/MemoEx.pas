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
   Vcl.StdCtrls, System.Classes, System.UITypes, WinApi.Messages, Vcl.Controls,
   Vcl.Forms, OmniXML;

const
   CM_UPDATE_SCROLLS = CM_BASE + 1002;
   CM_HIDE_SCROLLS = CM_BASE + 1003;
   CM_HIDE_ALL = CM_BASE + 1004;

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
         procedure CM_UpdateScrolls(var msg: TMessage); message CM_UPDATE_SCROLLS;
         procedure CM_HideScrolls(var msg: TMessage); message CM_HIDE_SCROLLS;
         procedure CM_HideAll(var msg: TMessage); message CM_HIDE_ALL;
      protected
         procedure SetWordWrap(AValue: boolean);
         procedure SetScrollBars(AValue: TScrollStyle);
         procedure MyOnMouseLeave(Sender: TObject); virtual;
         procedure MyOnMouseEnter(Sender: TObject);
         procedure MyOnExit(Sender: TObject);
         procedure MyOnEnter(Sender: TObject);
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
   end;

implementation

uses
   System.StrUtils, WinApi.Windows, Vcl.Graphics, System.SysUtils, XMLProcessor,
   ApplicationCommon, SynEdit;

constructor TMemoEx.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   EditFormWidth := 280;
   EditFormHeight := 182;
   OnKeyDown := TInfra.OnKeyDownSelectAll;
   OnMouseLeave := MyOnMouseLeave;
   OnMouseEnter := MyOnMouseEnter;
   OnExit := MyOnExit;
   OnEnter := MyOnEnter;
end;

procedure TMemoEx.SetHasVScroll(AValue: boolean);
begin
   if AValue <> FHasVScroll then
   begin
      FHasVScroll := AValue;
      UpdateVScroll;
   end;
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
   if (AValue <> WordWrap) and AValue then
      SetHasHScroll(false);
   inherited SetWordWrap(AValue);
end;

procedure TMemoEx.SetScrollBars(AValue: TScrollStyle);
var
   s, l: integer;
   b: TBufferCoord;
begin
   if AValue <> ScrollBars then
   begin
      s := SelStart;
      l := SelLength;
      b := TInfra.GetMemoCoord(Self, 0, 0);
      inherited SetScrollBars(AValue);
      SelStart := s;
      SelLength := l;
      Perform(EM_LINESCROLL, b.Char, b.Line);
   end;
end;

procedure TMemoEx.MyOnMouseLeave(Sender: TObject);
begin
   if not Focused then
      PostMessage(Handle, CM_HIDE_SCROLLS, 0, 0);
end;

procedure TMemoEx.MyOnMouseEnter(Sender: TObject);
begin
   PostMessage(Handle, CM_UPDATE_SCROLLS, 0, 0);
end;

procedure TMemoEx.MyOnExit(Sender: TObject);
begin
   PostMessage(Handle, CM_HIDE_SCROLLS, 0, 0);
end;

procedure TMemoEx.MyOnEnter(Sender: TObject);
begin
   PostMessage(Handle, CM_UPDATE_SCROLLS, 0, 0);
end;

procedure TMemoEx.ChangeBorderStyle(AStyle: TBorderStyle);
var
   s, l: integer;
   b: TBufferCoord;
begin
   if AStyle <> BorderStyle then
   begin
      GProject.ChangingOn := false;
      b := TInfra.GetMemoCoord(Self, 0, 0);
      s := SelStart;
      l := SelLength;
      BorderStyle := AStyle;
      SelStart := s;
      SelLength := l;
      Perform(EM_LINESCROLL, b.Char, b.Line);
      GProject.ChangingOn := true;
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
   editRect: TRect;
begin
   if FHasVScroll then
   begin
      hnd := GetDC(Handle);
      try
         oldFont := SelectObject(hnd, Font.Handle);
         try
            Perform(EM_GETRECT, 0, LPARAM(@editRect));
            GetTextMetrics(hnd, txtMetric);
            lineCount := editRect.Height div txtMetric.tmHeight;
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
   lCanvas: TCanvas;
   editRect: TRect;
begin
   if FHasHScroll and not WordWrap then
   begin
      w := 0;
      lCanvas := TCanvas.Create;
      try
         lCanvas.Font.Assign(Font);
         lCanvas.Handle := GetDC(Handle);
         for i := 0 to Lines.Count-1 do
         begin
            cnt := lCanvas.TextWidth(Lines[i]);
            if cnt > w then
               w := cnt;
         end;
         Perform(EM_GETRECT, 0, LPARAM(@editRect));
         if w > editRect.Width then
         begin
            if ScrollBars = TScrollStyle.ssNone then
               ScrollBars := TScrollStyle.ssHorizontal
            else if ScrollBars = TScrollStyle.ssVertical then
               ScrollBars := TScrollStyle.ssBoth;
         end
         else
            ResetScrollBars(TScrollStyle.ssVertical);
      finally
         ReleaseDC(Handle, lCanvas.Handle);
         lCanvas.Free;
      end;
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

procedure TMemoEx.CM_UpdateScrolls(var msg: TMessage);
begin
   UpdateScrolls;
end;

procedure TMemoEx.CM_HideScrolls(var msg: TMessage);
begin
   ScrollBars := ssNone;
end;

procedure TMemoEx.CM_HideAll(var msg: TMessage);
begin
   ChangeBorderStyle(bsNone);
   ScrollBars := ssNone;
end;

procedure TMemoEx.GetFromXML(ATag: IXMLElement);
var
   val: string;
   h, v: integer;
begin
   if ATag <> nil then
   begin
      EditFormWidth := StrToIntDef(ATag.GetAttribute('memW'), EditFormWidth);
      EditFormHeight := StrToIntDef(ATag.GetAttribute('memH'), EditFormHeight);
      HasVScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_vscroll', FHasVScroll);
      HasHScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_hscroll', FHasHScroll);
      WordWrap := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_wordwrap', WordWrap);
      v := StrToIntDef(ATag.GetAttribute('mem_vscroll_pos'), 0);
      h := StrToIntDef(ATag.GetAttribute('mem_hscroll_pos'), 0);
      Perform(EM_LINESCROLL, h, v);
      val := ATag.GetAttribute('mem_align');
      if not val.IsEmpty then
         Alignment := TInfra.StringToEnum<TAlignment>(val);
   end;
end;

procedure TMemoEx.SaveInXML(ATag: IXMLElement);
var
   b: TBufferCoord;
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute('memW', EditFormWidth.ToString);
      ATag.SetAttribute('memH', EditFormHeight.ToString);
      ATag.SetAttribute('mem_vscroll', HasVScroll.ToString);
      ATag.SetAttribute('mem_hscroll', HasHScroll.ToString);
      ATag.SetAttribute('mem_wordwrap', WordWrap.ToString);
      ATag.SetAttribute('mem_align', TInfra.EnumToString<TAlignment>(Alignment));
      b := TInfra.GetMemoCoord(Self, 0, 0);
      ATag.SetAttribute('mem_vscroll_pos', b.Line.ToString);
      ATag.SetAttribute('mem_hscroll_pos', b.Char.ToString);
   end;
end;

end.
