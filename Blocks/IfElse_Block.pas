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
   WinApi.Windows, Vcl.ComCtrls, Base_Block, OmniXML, Types;

type

   TIfElseBlock = class(TGroupBlock)
      protected
         TrueBranch,
         FalseBranch: TBranch;
         TrueHook,
         FalseHook: integer;
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
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
         procedure SaveInXML(ANode: IXMLNode); override;
   end;

const
   TRUE_BRANCH_IDX = PRIMARY_BRANCH_IDX;
   FALSE_BRANCH_IDX = TRUE_BRANCH_IDX + 1;

implementation

uses
   System.SysUtils, System.Classes, System.Math, Infrastructure, YaccLib, OmniXMLUtils;

constructor TIfElseBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpDiamond, taCenter, yymCondition);

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
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := TopHook.X + 40;
      BottomPoint.X := BottomHook;
      BottomPoint.Y := Height - 25;
      TopHook.Y := FDiamond.Left.Y;

      DrawArrow(BottomHook, Height-30, BottomHook, Height-1);
      DrawArrow(Point(TrueBranch.Hook.X, TopHook.Y), TrueBranch.Hook);
      DrawArrow(Point(FalseBranch.Hook.X, TopHook.Y), FalseBranch.Hook);
      if not TrueBranch.EndsWithReturnBlock then
         DrawArrow(TrueHook, Height-30, BottomHook-5, Height-30);
      if not FalseBranch.EndsWithReturnBlock then
         DrawArrow(FalseHook, Height-30, BottomHook+4, Height-30);

      Canvas.Ellipse(BottomHook-5, Height-34, BottomHook+5, Height-24);
      Canvas.MoveTo(FalseBranch.Hook.X, FDiamond.Right.Y);
      Canvas.LineTo(FDiamond.Right.X, FDiamond.Right.Y);
      Canvas.MoveTo(TrueBranch.Hook.X, FDiamond.Left.Y);
      Canvas.LineTo(FDiamond.Left.X, FDiamond.Left.Y);

      DrawTextLabel(FDiamond.Left.X, FDiamond.Left.Y-5, FTrueLabel, True, True);
      DrawTextLabel(FDiamond.Right.X, FDiamond.Right.Y-5, FFalseLabel, False, True);
      DrawBlockLabel(FDiamond.Left.X+10, FDiamond.Left.Y+5, GInfra.CurrentLang.LabelIfElse, True);
   end;
   DrawI;
end;

procedure TIfElseBlock.ResizeHorz(AContinue: boolean);
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
   LinkAllBlocks;
   if (FRedArrow <> FALSE_BRANCH_IDX) and (TrueBranch.Count > 0) then           // TRUE branch
   begin
      var lx := 10;
      for var block in TrueBranch do
          lx := Min(block.Left, lx);
      TrueBranch.Hook.X := TrueBranch.Hook.X - lx + 10;
      LinkAllBlocks;
      var mx := BottomHook - 30;
      for var block in TrueBranch do
          mx := Max(block.BoundsRect.Right, mx);
      var dlt := mx - BottomHook + 30;
      Inc(TopHook.X, dlt);
      BottomHook := mx + 30;
      BottomPoint.X := BottomHook;
      Width := Width + dlt + 10;
      Inc(FalseBranch.Hook.X, dlt);
      LinkAllBlocks;
      TrueHook := TrueBranch.Last.Left + TrueBranch.Last.BottomPoint.X;
      if FalseBranch.Count > 0 then
         FalseHook := FalseBranch.Last.Left + FalseBranch.Last.BottomPoint.X
      else
      begin
         FalseHook := FalseBranch.Hook.X;
         Width := FalseHook + 11;
      end;
   end;
   if (FRedArrow <> TRUE_BRANCH_IDX) and (FalseBranch.Count > 0) then           // FALSE branch
   begin
      var mx := BottomHook + 30;
      for var block in FalseBranch do
          mx := Min(block.Left, mx);
      var dlt := BottomHook + 30 - mx;
      FalseBranch.Hook.X := FalseBranch.Hook.X + dlt;
      LinkAllBlocks;
      var rx := 0;
      for var block in FalseBranch do
          rx := Max(block.BoundsRect.Right, rx);
      Width := rx + 10;
      LinkAllBlocks;
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
begin
   var b1 := FalseBranch;
   var b2 := TrueBranch;
   if TrueBranch.Height > FalseBranch.Height then
   begin
      b1 := TrueBranch;
      b2 := FalseBranch;
   end;
   b1.Hook.Y := 70;
   Height := b1.Height + b1.Hook.Y + 31;
   b2.Hook.Y := Height - b2.Height - 31;
   LinkAllBlocks;
   if AContinue then
      ParentBlock.ResizeVert(AContinue);
end;

function TIfElseBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if result and FVResize then
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
   if FHResize and result and not Expanded then
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
begin
   result := inherited GenerateTree(AParentNode);
   var elseNode := TTreeNodeWithFriend(AParentNode.Owner.AddChild(AParentNode, GInfra.CurrentLang.ElseLabel));
   TTreeNodeWithFriend(result).Friend := elseNode;
   for var block in FalseBranch do
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

procedure TIfElseBlock.SaveInXML(ANode: IXMLNode);
begin
   inherited SaveInXML(ANode);
   if ANode <> nil then
   begin
      var fbrx := FFoldParms.P2X;
      var th := FFoldParms.TopHook;
      if Expanded then
      begin
         fbrx := FalseBranch.Hook.X;
         th := TopHook.X;
      end;
      SetNodeAttrInt(ANode, 'fbrx', fbrx);
      SetNodeAttrInt(ANode, 'th', th);
      SetNodeAttrInt(ANode, 'fbry', FalseBranch.Hook.Y);
      SetNodeAttrInt(ANode, 'flh', FalseHook);
      SetNodeAttrInt(ANode, 'trh', TrueHook);
   end;
end;

end.
