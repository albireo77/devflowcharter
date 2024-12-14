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
   Base_Block, Types, YaccLib;

type

   TInOutBlock = class(TBlock)
      protected
         FLabel,
         FLabelSegoe: string;
         constructor Create(ABranch: TBranch;
                            const ABlockParms: TBlockParms;
                            const AText: string;
                            AdjustWidth: boolean;
                            AParserMode: TYYMode); overload;
         procedure Paint; override;
         procedure PutTextControls; override;
   end;

   TInputBlock = class(TInOutBlock)
      public
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         constructor Create(ABranch: TBranch); overload;
   end;

   TOutputBlock = class(TInOutBlock)
      public
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         constructor Create(ABranch: TBranch); overload;
   end;

implementation

uses
   Vcl.Controls, Vcl.Graphics, System.Classes, WinApi.Windows, System.Math, Infrastructure;

const
   DEF_BLOCK_WIDTH = 153;
   DEF_BLOCK_HEIGHT = 64;
   BOTTOM_POINT_Y = DEF_BLOCK_HEIGHT - 30;

constructor TInOutBlock.Create(ABranch: TBranch;
                               const ABlockParms: TBlockParms;
                               const AText: string;
                               AdjustWidth: boolean;
                               AParserMode: TYYMode);
begin

   inherited Create(ABranch, ABlockParms, shpParallel, AParserMode, taLeftJustify);

   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.Text := AText;
   PutTextControls;
   if AdjustWidth then
   begin
      var w :=  TInfra.GetTextWidth(FStatement.Text, FStatement) + FStatement.Left + 20;
      if Width < w then
         Width := w;
   end;
   var p := GInfra.CurrentLang.InOutCursorPos;
   FStatement.SelStart := IfThen(p <= 0, Length(FStatement.Text) + p, p - 1);
   BottomHook := Width div 2;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := BOTTOM_POINT_Y;
   IPoint.X := BottomHook + 30;
   IPoint.Y := BOTTOM_POINT_Y + 10;
   TopHook.X := BottomHook;
   Constraints.MinWidth := DEF_BLOCK_WIDTH;
   Constraints.MinHeight := DEF_BLOCK_HEIGHT;
end;

constructor TInputBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin
   FLabel := trnsManager.GetString('CaptionIn');
   FLabelSegoe := GInfra.CurrentLang.LabelIn;
   inherited Create(ABranch, ABlockParms, GInfra.CurrentLang.InputFunction, False, yymInput);
end;

constructor TInputBlock.Create(ABranch: TBranch);
begin
   FLabel := trnsManager.GetString('CaptionIn');
   FLabelSegoe := GInfra.CurrentLang.LabelIn;
   inherited Create(ABranch, TBlockParms.New(blInput, 0, 0, DEF_BLOCK_WIDTH, DEF_BLOCK_HEIGHT), GInfra.CurrentLang.InputFunction, True, yymInput);
end;

constructor TOutputBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin
   FLabel := trnsManager.GetString('CaptionOut');
   FLabelSegoe := GInfra.CurrentLang.LabelOut;
   inherited Create(ABranch, ABlockParms, GInfra.CurrentLang.OutputFunction, False, yymOutput);
end;

constructor TOutputBlock.Create(ABranch: TBranch);
begin
   FLabel := trnsManager.GetString('CaptionOut');
   FLabelSegoe := GInfra.CurrentLang.LabelOut;
   inherited Create(ABranch, TBlockParms.New(blOutput, 0, 0, DEF_BLOCK_WIDTH, DEF_BLOCK_HEIGHT), GInfra.CurrentLang.OutputFunction, True, yymOutput);
end;

procedure TInOutBlock.Paint;
begin
   inherited;
   var w := Canvas.TextWidth(FLabel);
   DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
   SetBrushColor(FShape);
   Canvas.Polygon([Point(20, 0),
                   Point(Width-1, 0),
                   Point(Width-21, BOTTOM_POINT_Y),
                   Point(0, BOTTOM_POINT_Y),
                   Point(20, 0)]);
   Canvas.MoveTo(w+33, 0);
   Canvas.LineTo(w+13, BOTTOM_POINT_Y);
   var fontStyles := Canvas.Font.Style;
   Canvas.Font.Style := [];
   var R := Rect(17, 1, w+17, BOTTOM_POINT_Y-3);
   DrawText(Canvas.Handle, PChar(FLabel), -1, R, DT_SINGLELINE or DT_VCENTER);
   Canvas.Font.Style := fontStyles;
   DrawBlockLabel(5, BOTTOM_POINT_Y, FLabelSegoe);
   DrawI;
end;

procedure TInOutBlock.PutTextControls;
begin
   var l := Canvas.TextWidth(FLabel) + 34;
   var t := BOTTOM_POINT_Y - FStatement.Height;
   if t > 5 then
      Inc(t, 6);
   FStatement.SetBounds(l, t div 2, Width-l-20, FStatement.Height);
end;

end.
