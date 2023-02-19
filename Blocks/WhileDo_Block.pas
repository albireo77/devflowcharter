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
   System.Types, Base_Block, Types;

type

   TWhileDoBlock = class(TGroupBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
      protected
         procedure Paint; override;
         procedure SetWidth(AMinX: integer); override;
         function GetDiamondTop: TPoint; override;
   end;

implementation

uses
   System.Classes, Return_Block, Infrastructure, YaccLib;

constructor TWhileDoBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpDiamond, taCenter, yymCondition);

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
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   IPoint.Y := 69;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
end;

constructor TWhileDoBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blWhile, 0, 0, 200, 131, 100, 109, 100));
end;

procedure TWhileDoBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 40;
      BottomPoint.Y := FDiamond.Right.Y;
      TopHook := FDiamond.Bottom;

      DrawArrow(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         Canvas.MoveTo(BottomHook, Height-21);
         Canvas.LineTo(5, Height-21);
         DrawArrowTo(5, 0, arrMiddle);
         Canvas.LineTo(TopHook.X, 0);
      end;
      DrawTextLabel(FDiamond.Bottom.X-10, FDiamond.Bottom.Y, FTrueLabel, True);
      DrawTextLabel(FDiamond.Right.X, FDiamond.Right.Y-5, FFalseLabel, False, True);
      DrawBlockLabel(FDiamond.Left.X+5, FDiamond.Left.Y-5, GInfra.CurrentLang.LabelWhile, True, True);
      Canvas.MoveTo(TopHook.X, 0);
      Canvas.LineTo(FDiamond.Top.X, FDiamond.Top.Y);
      Canvas.PenPos := FDiamond.Right;
      Canvas.LineTo(BottomPoint.X, BottomPoint.Y);
      DrawArrowTo(BottomPoint.X, Height-1);
   end;
   DrawI;
end;

function TWhileDoBlock.GetDiamondTop: TPoint;
begin
   result := Point(Branch.Hook.X, 19);
end;

procedure TWhileDoBlock.SetWidth(AMinX: integer);
begin
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
   BottomPoint.X := Width - 11;
end;

end.