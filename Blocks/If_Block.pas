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


unit If_Block;

interface

uses
   System.Types, Base_Block, Types;

type

   TIfBlock = class(TGroupBlock)
      private
         FTrueLabel,
         FFalseLabel: string;
      public
         constructor Create(AParentBranch: TBranch); overload;
         constructor Create(AParentBranch: TBranch; const ABlockParms: TBlockParms); overload;
      protected
         procedure Paint; override;
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
         procedure SetWidth(AMinX: integer); override;
         function GetDiamondTop: TPoint; override;
   end;

implementation

uses
   System.Classes, Infrastructure, YaccLib;

constructor TIfBlock.Create(AParentBranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(AParentBranch, ABlockParms, shpDiamond, taCenter, yymCondition);

   FInitParms.Width := 200;
   FInitParms.Height := 121;
   FInitParms.BottomHook := 100;
   FInitParms.BranchPoint.X := 100;
   FInitParms.BottomPoint.X := 100;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   FTrueLabel := trnsManager.GetString('CaptionTrue');
   FFalseLabel := trnsManager.GetString('CaptionFalse');

   BottomPoint.X := ABlockParms.bh;
   BottomPoint.Y := Height-31;
   TopHook.Y := 60;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   IPoint.Y := 50;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
end;

constructor TIfBlock.Create(AParentBranch: TBranch);
begin
   Create(AParentBranch, TBlockParms.New(blIf, 0, 0, 200, 121, 100, 89, 100));
end;

procedure TIfBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 40;
      TopHook := FDiamond.Bottom;
      BottomPoint.Y := Height - 31;

      DrawArrowVert(TopHook.Y, Branch.Hook);
      DrawTextLabel(FDiamond.Bottom.X-10, FDiamond.Bottom.Y, FTrueLabel, True);
      DrawTextLabel(FDiamond.Right.X, FDiamond.Right.Y-5, FFalseLabel, False, True);
      DrawBlockLabel(FDiamond.Left.X-5, FDiamond.Left.Y-5, GInfra.CurrentLang.LabelIf, True, True);

      Canvas.PenPos := FDiamond.Right;
      Canvas.LineTo(Width-11, FDiamond.Right.Y);
      DrawArrowTo(Width-11, Height-31, arrMiddle);
      Canvas.LineTo(BottomPoint.X, Height-31);
      DrawArrowVert(BottomPoint, Height-1);
   end;
   DrawI;
end;

procedure TIfBlock.SetWidth(AMinX: integer);
begin
   BottomPoint.X := BottomHook;
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
end;

function TIfBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if result and FVResize then
   begin
      if Expanded then
         Inc(Branch.Hook.Y, NewHeight-Height)
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 28;
      end;
   end;
   if result and FHResize and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

function TIfBlock.GetDiamondTop: TPoint;
begin
   result := Point(Branch.Hook.X, 0);
end;

end.