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



unit RepeatUntil_Block;

interface

uses
   System.Types, Base_Block, Types;

type

   TRepeatUntilBlock = class(TGroupBlock)
      private
         FLeftLabel, FRightLabel: string;
      public
         constructor Create(AParentBranch: TBranch); overload;
         constructor Create(AParentBranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GetDescTemplate(const ALangId: string): string; override;
      protected
         procedure Paint; override;
         procedure SetWidth(AMinX: integer); override;
         function GetDiamondTop: TPoint; override;
   end;

implementation

uses
   System.Classes, System.Math, Infrastructure, YaccLib;

constructor TRepeatUntilBlock.Create(AParentBranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(AParentBranch, ABlockParms, shpDiamond, taCenter, yymCondition);

   FInitParms.Width := 240;
   FInitParms.Height := 111;
   FInitParms.BottomHook := 120;
   FInitParms.BranchPoint.X := 120;
   FInitParms.BottomPoint.X := 229;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 82;

   if GInfra.CurrentLang.RepeatUntilAsDoWhile then
   begin
      FLeftLabel := FTrueLabel;
      FRightLabel := FFalseLabel;
   end
   else
   begin
      FLeftLabel := FFalseLabel;
      FRightLabel := FTrueLabel;
   end;
   BottomPoint.X := Width - 11;
   BottomPoint.Y := Height - 50;
   TopHook.Y := 0;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
end;

constructor TRepeatUntilBlock.Create(AParentBranch: TBranch);
begin
   Create(AParentBranch, TBlockParms.New(blRepeat, 0, 0, 240, 111, 120, 29, 120));
end;

procedure TRepeatUntilBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := BottomHook + 40;
      IPoint.Y := Height - 30;
      BottomPoint.Y := FDiamond.Right.Y;

      Canvas.PenPos := FDiamond.Left;
      Canvas.LineTo(5, FDiamond.Left.Y);
      DrawArrowTo(5, 0, arrMiddle);
      Canvas.LineTo(Branch.Hook.X, TopHook.Y);
      DrawArrowTo(Branch.Hook.X, Branch.Hook.Y);

      DrawTextLabel(FDiamond.Left.X, FDiamond.Left.Y-5, FLeftLabel, True, True);
      DrawTextLabel(FDiamond.Right.X, FDiamond.Right.Y-5, FRightLabel, False, True);
      DrawBlockLabel(FDiamond.Bottom.X-30, FDiamond.Bottom.Y-10, GInfra.CurrentLang.LabelRepeat, True);
      Canvas.PenPos := FDiamond.Right;
      Canvas.LineTo(BottomPoint.X, FDiamond.Right.Y);
      DrawArrowTo(BottomPoint.X, Height-1);
   end;
   DrawI;
end;

function TRepeatUntilBlock.GetDescTemplate(const ALangId: string): string;
begin
   result := '';
   var lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.RepeatUntilDescTemplate;
end;

procedure TRepeatUntilBlock.SetWidth(AMinX: integer);
begin
   Width := Max(BottomHook + 121, AMinX);
   BottomPoint.X := Width - 11;
end;

function TRepeatUntilBlock.GetDiamondTop: TPoint;
begin
   result := Point(BottomHook, Height-81);
end;

end.