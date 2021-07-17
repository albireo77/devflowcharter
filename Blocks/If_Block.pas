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
   Vcl.Graphics, System.Types, Base_Block, Types;

type

   TIfBlock = class(TGroupBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
      protected
         procedure Paint; override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure SetWidth(AMinX: integer); override;
         function GetDiamondTop: TPoint; override;
   end;

implementation

uses
   System.Classes, Infrastructure;

constructor TIfBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms);

   FInitParms.Width := 200;
   FInitParms.Height := 121;
   FInitParms.BottomHook := 100;
   FInitParms.BranchPoint.X := 100;
   FInitParms.BottomPoint.X := 100;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   BottomPoint.X := ABlockParms.bh;
   BottomPoint.Y := Height-31;
   TopHook.Y := 60;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   IPoint.Y := 50;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Alignment := taCenter;
end;

constructor TIfBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blIf, 0, 0, 200, 121, 100, 89, 100));
end;

procedure TIfBlock.Paint;
var
   dRight, dBottom, dLeft: TPoint;
begin
   inherited;
   if Expanded then
   begin
      dRight := FDiamond[D_RIGHT];
      dBottom := FDiamond[D_BOTTOM];
      dLeft := FDiamond[D_LEFT];
      IPoint.X := Branch.Hook.X + 40;
      TopHook := dBottom;
      BottomPoint.Y := Height - 31;

      DrawArrow(TopHook, Branch.Hook);
      DrawTextLabel(dBottom.X-10, dBottom.Y, FTrueLabel, true);
      DrawTextLabel(dRight.X, dRight.Y-5, FFalseLabel, false, true);
      DrawBlockLabel(dLeft.X-5, dLeft.Y-5, GInfra.CurrentLang.LabelIf, true, true);

      Canvas.PenPos := dRight;
      Canvas.LineTo(Width-11, dRight.Y);
      DrawArrowTo(Width-11, Height-31, arrMiddle);
      Canvas.LineTo(BottomPoint.X, Height-31);
      DrawArrow(BottomPoint, BottomPoint.X, Height-1);
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

procedure TIfBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if Resize and FVResize then
   begin
      if Expanded then
         Inc(Branch.Hook.Y, NewHeight-Height)
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 28;
      end;
   end;
   if Resize and FHResize and not Expanded then
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