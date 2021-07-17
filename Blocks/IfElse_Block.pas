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



unit IfElse_Block;

interface

uses
   WinApi.Windows, Vcl.Graphics, Vcl.ComCtrls, Base_Block, OmniXML, Types;

type

   TIfElseBlock = class(TGroupBlock)
      protected
         TrueBranch,
         FalseBranch: TBranch;
         TrueHook,
         FalseHook: integer;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure Paint; override;
         procedure SetWidth(AMinX: integer); override;
         function GetDiamondTop: TPoint; override;
         function GetBlockParms: TBlockParms; override;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         procedure ResizeHorz(AContinue: boolean); override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ExpandFold(AResize: boolean); override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         procedure SaveInXML(ATag: IXMLElement); override;
   end;

const
   TRUE_BRANCH_IDX = PRIMARY_BRANCH_IDX;
   FALSE_BRANCH_IDX = TRUE_BRANCH_IDX + 1;

implementation

uses
   System.SysUtils, System.Classes, System.Types, System.Math, Return_Block, Infrastructure;

constructor TIfElseBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms);

   FInitParms.Width := 240;
   FInitParms.Height := 101;
   FInitParms.BottomHook := 120;
   FInitParms.BranchPoint.X := 5;
   FInitParms.BottomPoint.X := 120;
   FInitParms.P2X := 229;

   TrueBranch := Branch;
   FalseBranch := AddBranch(ABlockParms.br2);

   FFixedBranches := 2;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.th;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := Height-1;
   IPoint.Y := 50;
   TopHook.Y := 30;
   TrueHook := ABlockParms.trh;
   FalseHook := ABlockParms.flh;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Alignment := taCenter;

end;

constructor TIfElseBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blIfElse, 0, 0, 240, 101, 5, 70, 120, 120, 229, 70, 5, 229));
end;

function TIfElseBlock.GetBlockParms: TBlockParms;
begin
   result := TBlockParms.New(
      blIfElse,
      Left,
      Top,
      Width,
      Height,
      TrueBranch.Hook.X,
      TrueBranch.Hook.Y,
      BottomHook,
      TopHook.X,
      FalseBranch.Hook.X,
      FalseBranch.Hook.Y,
      TrueHook,
      FalseHook);
end;

procedure TIfElseBlock.Paint;
var
   dRight, dLeft: TPoint;
begin
   inherited;
   if Expanded then
   begin
      dRight := FDiamond[D_RIGHT];
      dLeft := FDiamond[D_LEFT];
      IPoint.X := TopHook.X + 40;
      BottomPoint.X := BottomHook;
      BottomPoint.Y := Height - 25;
      TopHook.Y := dLeft.Y;

      DrawArrow(BottomHook, Height-30, BottomHook, Height-1);
      DrawArrow(TrueBranch.Hook.X, TopHook.Y, TrueBranch.Hook);
      DrawArrow(FalseBranch.Hook.X, TopHook.Y, FalseBranch.Hook);
      if TrueBranch.FindInstanceOf(TReturnBlock) = -1 then
         DrawArrow(TrueHook, Height-30, BottomHook-5, Height-30);
      if FalseBranch.FindInstanceOf(TReturnBlock) = -1 then
         DrawArrow(FalseHook, Height-30, BottomHook+4, Height-30);

      Canvas.Ellipse(BottomHook-5, Height-34, BottomHook+5, Height-24);
      Canvas.MoveTo(FalseBranch.Hook.X, dRight.Y);
      Canvas.LineTo(dRight.X, dRight.Y);
      Canvas.MoveTo(TrueBranch.Hook.X, dLeft.Y);
      Canvas.LineTo(dLeft.X, dLeft.Y);

      DrawTextLabel(dLeft.X, dLeft.Y-5, FTrueLabel, true, true);
      DrawTextLabel(dRight.X, dRight.Y-5, FFalseLabel, false, true);
      DrawBlockLabel(dLeft.X+5, dLeft.Y+5, GInfra.CurrentLang.LabelIfElse, true);
   end;
   DrawI;
end;

procedure TIfElseBlock.ResizeHorz(AContinue: boolean);
var
   leftX, maxXTrue, minXFalse, rightX, dlt: integer;
   block: TBlock;
begin

   if (TrueBranch.Count = 0) and (FalseBranch.Count = 0) then  // no child blocks
   begin
      Width := FInitParms.Width;
      TrueBranch.Hook.X := FInitParms.BranchPoint.X;
      TrueBranch.Hook.Y := 70;
      BottomPoint.X := FInitParms.BottomPoint.X;
      FalseBranch.Hook.X := FInitParms.P2X;
      FalseBranch.Hook.Y := 70;
      BottomHook := FInitParms.BottomHook;
      TopHook.X := FInitParms.Width div 2;
      TrueHook := TrueBranch.Hook.X;
      FalseHook := FalseBranch.Hook.X;
      if AContinue then
         ParentBlock.ResizeHorz(AContinue);
      Exit;
   end;

   LinkBlocks;

   if (Ired <> FALSE_BRANCH_IDX) and (TrueBranch.Count > 0) then           // TRUE branch
   begin
      leftX := 10;
      for block in TrueBranch do
          leftX := Min(block.Left, leftX);
      TrueBranch.Hook.X := TrueBranch.Hook.X - leftX + 10;
      LinkBlocks;

      maxXTrue := BottomHook - 30;
      for block in TrueBranch do
          maxXTrue := Max(block.BoundsRect.Right, maxXTrue);
      dlt := maxXTrue - BottomHook + 30;
      Inc(TopHook.X, dlt);
      BottomHook := BottomHook + dlt;
      BottomPoint.X := BottomHook;
      Width := Width + dlt + 10;
      Inc(FalseBranch.Hook.X, dlt);
      LinkBlocks;
      TrueHook := TrueBranch.Last.Left + TrueBranch.Last.BottomPoint.X;
      if FalseBranch.Count > 0 then
         FalseHook := FalseBranch.Last.Left + FalseBranch.Last.BottomPoint.X
      else
      begin
         FalseHook := FalseBranch.Hook.X;
         Width := FalseHook + 11;
      end;
   end;

   if (Ired <> TRUE_BRANCH_IDX) and (FalseBranch.Count > 0) then           // FALSE branch
   begin
      minXFalse := BottomHook + 30;
      for block in FalseBranch do
          minXFalse := Min(block.Left, minXFalse);
      dlt := BottomHook + 30 - minXFalse;
      FalseBranch.Hook.X := FalseBranch.Hook.X + dlt;
      LinkBlocks;

      rightX := 0;
      for block in FalseBranch do
          rightX := Max(block.BoundsRect.Right, rightX);
      Width := rightX + 10;
      LinkBlocks;
      FalseHook := FalseBranch.Last.Left + FalseBranch.Last.BottomPoint.X;
      if TrueBranch.Count > 0 then
         TrueHook := TrueBranch.Last.Left + TrueBranch.Last.BottomPoint.X
      else
         TrueHook := TrueBranch.Hook.X;
   end;

   if AContinue then
      ParentBlock.ResizeHorz(AContinue);

end;

procedure TIfElseBlock.ResizeVert(AContinue: boolean);
var
   b1, b2: TBranch;
begin
   if TrueBranch.Height > FalseBranch.Height then
   begin
      b1 := TrueBranch;
      b2 := FalseBranch;
   end
   else
   begin
      b1 := FalseBranch;
      b2 := TrueBranch;
   end;
   b1.Hook.Y := 70;
   Height := b1.Height + b1.Hook.Y + 31;
   b2.Hook.Y := Height - b2.Height - 31;
   LinkBlocks;
   if AContinue then
      ParentBlock.ResizeVert(AContinue);
end;

procedure TIfElseBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if Resize and FVResize then
   begin
      if Expanded then
      begin
         Inc(TrueBranch.Hook.Y, NewHeight-Height);
         Inc(FalseBranch.Hook.Y, NewHeight-Height);
      end
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 28;
      end;
   end;
   if FHResize and Resize and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

procedure TIfElseBlock.SetWidth(AMinX: integer);
begin
end;

function TIfElseBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
var
   elseNode: TTreeNodeWithFriend;
   block: TBlock;
begin
   result := inherited GenerateTree(AParentNode);
   elseNode := TTreeNodeWithFriend(AParentNode.Owner.AddChild(AParentNode, GInfra.CurrentLang.ElseLabel));
   TTreeNodeWithFriend(result).Friend := elseNode;
   for block in FalseBranch do
       block.GenerateTree(elseNode);
end;

procedure TIfElseBlock.ExpandFold(AResize: boolean);
begin
   if Expanded then
      FFoldParms.P2X := FalseBranch.Hook.X
   else
      FalseBranch.Hook.X := FFoldParms.P2X;
   inherited ExpandFold(AResize);
end;

function TIfElseBlock.GetDiamondTop: TPoint;
begin
   result := Point(TopHook.X, 0);
end;

procedure TIfElseBlock.SaveInXML(ATag: IXMLElement);
var
   th, fbrx: integer;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      if Expanded then
      begin
         fbrx := FalseBranch.Hook.X;
         th := TopHook.X;
      end
      else
      begin
         fbrx := FFoldParms.P2X;
         th := FFoldParms.TopHook;
      end;
      ATag.SetAttribute('fbrx', fbrx.ToString);
      ATag.SetAttribute('th', th.ToString);
      ATag.SetAttribute('fbry', FalseBranch.Hook.Y.ToString);
      ATag.SetAttribute('flh', FalseHook.ToString);
      ATag.SetAttribute('trh', TrueHook.ToString);
   end;
end;

end.
