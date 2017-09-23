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
   Vcl.StdCtrls, System.Classes, OmniXML;

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
   end;

implementation

uses
   System.StrUtils, WinApi.Windows, Vcl.Graphics, WinApi.Messages, System.SysUtils,
   XMLProcessor, System.UITypes;

constructor TMemoEx.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   EditFormWidth := 280;
   EditFormHeight := 182;
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
   pos, count, lineCount: integer;
   oldFont: HFont;
   hnd: THandle;
   txtMetric: TTextMetric;
begin
   pos := SelStart;
   if FHasVScroll then
   begin
      hnd := GetDC(Handle);
      try
         oldFont := SelectObject(hnd, Font.Handle);
         try
            GetTextMetrics(hnd, txtMetric);
            lineCount := (ClientHeight - 4)  div (txtMetric.tmHeight + txtMetric.tmExternalLeading)
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
   SelStart := pos;
end;

procedure TMemoEx.UpdateHScroll;
var
   pos, cnt, w, i: integer;
   lCanvas: TCanvas;
   margns: longint;
begin
   pos := SelStart;
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
         margns := SendMessage(Handle, EM_GETMARGINS, 0, 0);
         if w > (ClientWidth - HiWord(margns) - LoWord(margns) - 3) then
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
   SelStart := pos;
end;

procedure TMemoEx.UpdateScrolls;
begin
   UpdateVScroll;
   UpdateHScroll;
end;

procedure TMemoEx.GetFromXML(ATag: IXMLElement);
begin
   if ATag <> nil then
   begin
      EditFormWidth := StrToIntDef(ATag.GetAttribute('memW'), EditFormWidth);
      EditFormHeight := StrToIntDef(ATag.GetAttribute('memH'), EditFormHeight);
      HasVScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_vscroll', FHasVScroll);
      HasHScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_hscroll', FHasHScroll);
      WordWrap := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_wordwrap', WordWrap);
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
   end;
end;


end.
