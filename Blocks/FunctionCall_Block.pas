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
   Vcl.Graphics, Base_Block, CommonInterfaces;

type

   TFunctionCallBlock = class(TBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight: integer; AId: integer = ID_INVALID); overload;
         function Clone(ABranch: TBranch): TBlock; override;
      protected
         procedure Paint; override;
   end;


implementation

uses
   Vcl.Controls, Vcl.Forms, System.Classes, System.Types, ApplicationCommon, CommonTypes;

constructor TFunctionCallBlock.Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight: integer; AId: integer = ID_INVALID);
begin
   FType := blFuncCall;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatement.SetBounds(10, 1, AWidth-20, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FShape := shpRoutine;
   FStatement.Color := GSettings.GetShapeColor(FShape);
   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := FStatement.BoundsRect.Bottom + 1;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 30;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 51;
end;

function TFunctionCallBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := TFunctionCallBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

constructor TFunctionCallBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 51);
end;

procedure TFunctionCallBlock.Paint;
var
   bo, ri: integer;
   lColor: TColor;
   r: TRect;
begin
   inherited;
   with Canvas do
   begin
      if FStatement <> nil then
      begin
         Brush.Style := bsClear;
         lColor := GSettings.GetShapeColor(FShape);
         if lColor <> GSettings.DesktopColor then
            Brush.Color := lColor;
         bo := FStatement.BoundsRect.Bottom + 1;
         ri := FStatement.BoundsRect.Right + 1;
         BottomPoint.Y := bo;
         r := Rect(0, FStatement.Top-1, Width, bo);
         Rectangle(r);
         DrawBlockLabel(5, bo, GInfra.CurrentLang.LabelFuncCall);
         DrawArrow(BottomPoint, BottomPoint.X, Height-1);
         r := Rect(FStatement.Left-4, FStatement.Top-1, FStatement.Left-1, bo);
         Rectangle(r);
         r := Rect(ri, FStatement.Top-1, ri+3, bo);
         Rectangle(r);
      end;
   end;
   DrawI;
end;

end.
