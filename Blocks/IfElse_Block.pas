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
   WinApi.Windows, Vcl.Graphics, Vcl.ComCtrls, Base_Block, OmniXML, CommonInterfaces;

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
         function GetDiamondPoint: TPoint; override;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, p1X, p3X, b_hook, t_hook, p1Y, p3Y, f_hook, tt_hook: integer; AId: integer = ID_INVALID); overload;
         function Clone(ABranch: TBranch): TBlock; override;
         procedure ResizeHorz(AContinue: boolean); override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ExpandFold(AResize: boolean); override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         procedure SaveInXML(ATag: IXMLElement); override;
   end;

const
   TRUE_BRANCH_IND = PRIMARY_BRANCH_IND;
   FALSE_BRANCH_IND = TRUE_BRANCH_IND + 1;

implementation

uses
   System.SysUtils, System.Classes, System.Types, Return_Block, CommonTypes, ApplicationCommon;

constructor TIfElseBlock.Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, p1X, p3X, b_hook, t_hook, p1Y, p3Y, f_hook, tt_hook: integer; AId: integer = ID_INVALID);
begin

   FType := blIfElse;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

   FInitParms.Width := 240;
   FInitParms.Height := 101;
   FInitParms.BottomHook := 120;
   FInitParms.BranchPoint.X := 5;
   FInitParms.BottomPoint.X := 120;
   FInitParms.P2X := 229;

   TrueBranch := Branch;
   FalseBranch := AddBranch(Point(p3X, p3Y));

   BottomHook := b_hook;
   TopHook.X := t_hook;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := Height-1;
   IPoint.Y := 50;
   TopHook.Y := 30;
   TrueHook := tt_hook;
   FalseHook := f_hook;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Alignment := taCenter;
   PutTextControls;

end;

function TIfElseBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := TIfElseBlock.Create(ABranch, Left, Top, Width, Height, TrueBranch.Hook.X, FalseBranch.Hook.X, BottomHook,
                                 TopHook.X, TrueBranch.Hook.Y, FalseBranch.Hook.Y, FalseHook, TrueHook);
   result.CloneFrom(Self);
end;

constructor TIfElseBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 240, 101, 5, 229, 120, 120, 70, 70, 229, 5);
end;

procedure TIfElseBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := TopHook.X + 40;
      BottomPoint.X := BottomHook;
      BottomPoint.Y := Height - 25;
      PutTextControls;
      
      DrawArrowLine(Point(BottomHook, Height-30), Point(BottomHook, Height-1));
      DrawArrowLine(Point(TrueBranch.Hook.X, 30), TrueBranch.Hook);
      DrawArrowLine(Point(FalseBranch.Hook.X, 30), FalseBranch.Hook);
      if TrueBranch.FindInstanceOf(TReturnBlock) = -1 then
         DrawArrowLine(Point(TrueHook, Height-30), Point(BottomHook-5, Height-30));
      if FalseBranch.FindInstanceOf(TReturnBlock) = -1 then
         DrawArrowLine(Point(FalseHook, Height-30), Point(BottomHook+4, Height-30));
      with Canvas do
      begin
         Ellipse(BottomHook-5, Height-34, BottomHook+5, Height-24);
         MoveTo(TrueBranch.Hook.X, 30);
         LineTo(TopHook.X-60, 30);
         MoveTo(FalseBranch.Hook.X, 30);
         LineTo(TopHook.X+60, 30);
      end;
      DrawTextLabel(TopHook.X-60, 9, FTrueLabel, true);
      DrawTextLabel(TopHook.X+60, 9, FFalseLabel);
      DrawBlockLabel(TopHook.X-52, 35, GInfra.CurrentLang.LabelIfElse, true);
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
      Ired := -1;
      if AContinue then
         ParentBlock.ResizeHorz(AContinue);
      exit;
   end;

   LinkBlocks;

   if (Ired <> FALSE_BRANCH_IND) and (TrueBranch.Count > 0) then           // TRUE branch
   begin
      leftX := 10;
      for block in TrueBranch do
      begin
         if block.Left < leftX then
            leftX := block.Left;
      end;
      TrueBranch.Hook.X := TrueBranch.Hook.X - leftX + 10;
      LinkBlocks;

      maxXTrue := BottomHook - 30;
      for block in TrueBranch do
      begin
         if block.BoundsRect.Right > maxXTrue then
            maxXTrue := block.BoundsRect.Right;
      end;
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
         Width := FalseBranch.Hook.X + 11;
      end;
   end;

   if (Ired <> TRUE_BRANCH_IND) and (FalseBranch.Count > 0) then           // FALSE branch
   begin
      minXFalse := BottomHook + 30;
      for block in FalseBranch do
      begin
         if block.Left < minXFalse then
            minXFalse := block.Left;
      end;
      dlt := BottomHook + 30 - minXFalse;
      FalseBranch.Hook.X := FalseBranch.Hook.X + dlt;
      LinkBlocks;

      rightX := 0;
      for block in FalseBranch do
      begin
         if block.BoundsRect.Right > rightX then
            rightX := block.BoundsRect.Right;
      end;
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
         BottomPoint.Y := NewHeight - 30;
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
   newNode: TTreeNode;
   block: TBlock;
begin
   result := inherited GenerateTree(AParentNode);
   newNode := AParentNode.Owner.AddChild(AParentNode, GInfra.CurrentLang.ElseLabel);
   for block in FalseBranch do
       block.GenerateTree(newNode);
end;

procedure TIfElseBlock.ExpandFold(AResize: boolean);
begin
   if Expanded then
      FFoldParms.P2X := FalseBranch.Hook.X
   else
      FalseBranch.Hook.X := FFoldParms.P2X;
   inherited ExpandFold(AResize);
end;

function TIfElseBlock.GetDiamondPoint: TPoint;
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
