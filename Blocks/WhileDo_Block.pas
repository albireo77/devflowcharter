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



unit WhileDo_Block;

interface

uses
   Controls, Forms, StdCtrls, Graphics, Classes, SysUtils, Base_Block, CommonInterfaces, Types;

type

   TWhileDoBlock = class(TGroupBlock)
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         procedure ChangeColor(const AColor: TColor); override;
      protected
         procedure Paint; override;
         procedure SetWidth(const AMinX: integer); override;
         function GetDiamondPoint: TPoint; override;
   end;

implementation

uses
   ApplicationCommon, StrUtils, CommonTypes, Return_Block;

constructor TWhileDoBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
begin

   FType := blWhile;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

   FInitParms.Width := 200;
   FInitParms.Height := 131;
   FInitParms.BottomHook := 100;
   FInitParms.BranchPoint.X := 100;
   FInitParms.BottomPoint.X := 189;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 22;

   TopHook.Y := 79;
   BottomPoint.X := Width-11;
   BottomPoint.Y := 50;
   BottomHook := b_hook;
   TopHook.X := p1X;
   IPoint.Y := 74;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Color := GSettings.DiamondColor;
   FStatement.Alignment := taCenter;
   PutTextControls;

end;

function TWhileDoBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TWhileDoBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, Branch.Hook.X, Branch.Hook.Y);
   result.CloneFrom(Self);
end;

constructor TWhileDoBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 200, 131, 100, 100, 109);
end;

procedure TWhileDoBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 40;
      PutTextControls;
      
      DrawArrowLine(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      DrawArrowLine(Point(BottomPoint.X, 49), Point(BottomPoint.X, Height-1));
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         DrawArrowLine(Point(5, Height-21), Point(5, 0), arrMiddle);
         Canvas.Polyline([Point(BottomHook, Height-21),
                          Point(5, Height-21),
                          Point(5, 0),
                          Point(Branch.Hook.X, 0)]);
      end;
      DrawTextLabel(Branch.Hook.X-10, 80, FTrueLabel, true);
      DrawTextLabel(Branch.Hook.X+60, 28, FFalseLabel);
      DrawBlockLabel(Branch.Hook.X-40, 10, GInfra.CurrentLang.LabelWhile, true);
      with Canvas do
      begin
         MoveTo(Branch.Hook.X, 0);
         LineTo(Branch.Hook.X, 19);
         MoveTo(Branch.Hook.X+60, 49);
         LineTo(BottomPoint.X, 49);
      end;
   end;
   DrawI;
end;

function TWhileDoBlock.GetDiamondPoint: TPoint;
begin
   result := Point(Branch.Hook.X, 19);
end;

procedure TWhileDoBlock.SetWidth(const AMinX: integer);
begin
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
   BottomPoint.X := Width - 11;
end;

procedure TWhileDoBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.DiamondColor = GSettings.DesktopColor then
      FStatement.Color := AColor
   else
      FStatement.Color := GSettings.DiamondColor;
end;

end.