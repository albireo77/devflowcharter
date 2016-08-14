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



unit InOut_Block;

interface

uses
   Controls, Forms, StdCtrls, Graphics, Classes, SysUtils, Base_Block, CommonInterfaces;

type

   TInOutBlock = class(TBlock)
      protected
         FLabel,
         FLabelSegoe: string;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; virtual;
         procedure Paint; override;
         procedure PutTextControls; override;
      public
         procedure ChangeColor(const AColor: TColor); override;
   end;

   TInputBlock = class(TInOutBlock)
      public
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; override;
         constructor Create(const ABranch: TBranch); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
   end;

   TOutputBlock = class(TInOutBlock)
      public
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; override;
         constructor Create(const ABranch: TBranch); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
   end;

implementation

uses
   ApplicationCommon, Windows, StrUtils, CommonTypes;

constructor TInOutBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.Color := GSettings.InOutColor;
   PutTextControls;
   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := 30;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 40;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 150;
   Constraints.MinHeight := 61;
end;

constructor TInputBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   FType := blInput;
   FLabel := i18Manager.GetString('CaptionIn');
   FLabelSegoe := GInfra.CurrentLang.LabelIn;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatement.Text := GInfra.CurrentLang.InputFunction;
   FStatement.SelStart := Length(FStatement.Text) + GInfra.CurrentLang.InOutCursorPos;
end;

constructor TOutputBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   FType := blOutput;
   FLabel := i18Manager.GetString('CaptionOut');
   FLabelSegoe := GInfra.CurrentLang.LabelOut;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatement.Text := GInfra.CurrentLang.OutputFunction;
   FStatement.SelStart := Length(FStatement.Text) + GInfra.CurrentLang.InOutCursorPos;
end;

function TInputBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TInputBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

function TOutputBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TOutputBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

constructor TInputBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 150, 61);
end;

constructor TOutputBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 150, 61);
end;

procedure TInOutBlock.Paint;
var
   R: TRect;
   w: integer;
   lFontStyles: TFontStyles;
begin
   inherited;
   w := Canvas.TextWidth(FLabel);
   DrawArrowLine(Point(BottomPoint.X, 30), Point(BottomPoint.X, Height-1));
   with Canvas do
   begin
      Brush.Style := bsClear;
      if GSettings.InOutColor <> GSettings.DesktopColor then
         Brush.Color := GSettings.InOutColor;
      Polygon([Point(20, 0),
               Point(Width-1, 0),
               Point(Width-21, 30),
               Point(0, 30),
               Point(20, 0)]);
      MoveTo(w+32, 0);
      LineTo(w+12, 30);
      lFontStyles := Font.Style;
      Font.Style := [];
      R := Rect(16, 15-(TextHeight('X') div 2), w+16, 23);
      DrawText(Handle, PChar(FLabel), -1, R, DT_CENTER);
      Font.Style := lFontStyles;
   end;
   DrawBlockLabel(5, 30, FLabelSegoe);
   DrawI;
end;

procedure TInOutBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.InOutColor = GSettings.DesktopColor then
      FStatement.Color := AColor
   else
      FStatement.Color := GSettings.InOutColor;
end;

procedure TInOutBlock.PutTextControls;
var
   lTop, lLeft: integer;
begin
   lLeft := Canvas.TextWidth(FLabel) + 31;
   lTop := 17 - FStatement.Height div 2;
   if lTop + FStatement.Height > 29 then
      lTop := 30 - FStatement.Height;
   if lTop < 4 then
      lLeft := lLeft + 4 - lTop;
   FStatement.SetBounds(lLeft, lTop, Width-lLeft-20, FStatement.Height);
end;

end.
