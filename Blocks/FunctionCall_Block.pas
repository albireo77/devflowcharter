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



unit FunctionCall_Block;

interface

uses
   Base_Block, Types;

type

   TFunctionCallBlock = class(TBlock)
      public
         constructor Create(AParentBranch: TBranch); overload;
         constructor Create(AParentBranch: TBranch; const ABlockParms: TBlockParms); overload;
      protected
         procedure Paint; override;
         procedure PutTextControls; override;
         function CalculateStatementHeight: integer;
   end;


implementation

uses
   Vcl.Controls, Vcl.Graphics, System.Types, System.Classes, Infrastructure, YaccLib;

constructor TFunctionCallBlock.Create(AParentBranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(AParentBranch, ABlockParms, shpRoutine, yymFuncCall, taLeftJustify);

   FStatement.SetBounds(10, 1, ABlockParms.w-20, CalculateStatementHeight);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.SetLRMargins(1, 1);

   BottomHook := ABlockParms.w div 2;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := FStatement.BoundsRect.Bottom + 1;
   IPoint.X := BottomHook + 30;
   IPoint.Y := BottomPoint.Y + 8;
   TopHook.X := BottomHook;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 51;
end;

constructor TFunctionCallBlock.Create(AParentBranch: TBranch);
begin
   Create(AParentBranch, TBlockParms.New(blFuncCall, 0, 0, 140, 51));
end;

procedure TFunctionCallBlock.Paint;
begin
   inherited;
   SetBrushColor(FShape);
   var br := FStatement.BoundsRect.BottomRight;
   Inc(br.Y);
   BottomPoint.Y := br.Y;
   IPoint.Y := br.Y + 8;
   var r := Rect(0, FStatement.Top-1, Width, br.Y);
   Canvas.Rectangle(r);
   DrawBlockLabel(1, br.Y-2, GInfra.CurrentLang.LabelFuncCall);
   DrawArrowVert(BottomPoint, Height-1);
   r := Rect(FStatement.Left-4, FStatement.Top-1, FStatement.Left-1, br.Y);
   Canvas.Rectangle(r);
   r.SetLocation(br.X+1, r.Top);
   Canvas.Rectangle(r);
   DrawI;
end;

function TFunctionCallBlock.CalculateStatementHeight: integer;
begin
   result := Abs(FStatement.Font.Height) + 6;
end;

procedure TFunctionCallBlock.PutTextControls;
begin
   FStatement.Height := CalculateStatementHeight;
   BottomPoint.Y := FStatement.BoundsRect.Bottom + 1;
   IPoint.Y := BottomPoint.Y + 8;
end;

end.
