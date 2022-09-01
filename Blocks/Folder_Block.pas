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



unit Folder_Block;

interface

uses
   System.Types, Base_Block, Types;

type

   TFolderBlock = class(TGroupBlock)
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
   System.StrUtils, System.Classes, Infrastructure;

constructor TFolderBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpFolder, taLeftJustify);

   FInitParms.Width := 140;
   FInitParms.Height := 61;
   FInitParms.BottomHook := 70;
   FInitParms.BranchPoint.X := 70;
   FInitParms.BottomPoint.X := 70;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   BottomPoint.X := ABlockParms.bh;
   BottomPoint.Y := Height-31;
   TopHook.Y := 0;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   IPoint.Y := 8;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Free;
   FStatement := nil;
end;

constructor TFolderBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blFolder, 0, 0, 140, 61, 70, 30, 70));
end;

procedure TFolderBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 30;
      BottomPoint.Y := Height - 31;
      DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
      DrawArrow(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      DrawBlockLabel(5, 2, GInfra.CurrentLang.LabelFolder);
   end;
   DrawI;
end;

function TFolderBlock.GetDiamondTop: TPoint;
begin
   result := Point(-1, -1);
end;

procedure TFolderBlock.SetWidth(AMinX: integer);
begin
   BottomPoint.X := BottomHook;
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
end;

procedure TFolderBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
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

end.