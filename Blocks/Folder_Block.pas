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
   Controls, Forms, StdCtrls, Graphics, Classes, SysUtils, Base_Block, CommonInterfaces, Types;

type

   TFolderBlock = class(TGroupBlock)
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
      protected
         procedure Paint; override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure SetWidth(const AMinX: integer); override;
         function GetDiamondPoint: TPoint; override;
   end;

implementation

uses
   StrUtils, CommonTypes, ApplicationCommon;

constructor TFolderBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
begin

   FType := blFolder;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

   FInitParms.Width := 140;
   FInitParms.Height := 61;
   FInitParms.BottomHook := 70;
   FInitParms.BranchPoint.X := 70;
   FInitParms.BottomPoint.X := 70;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   BottomPoint.X := b_hook;
   BottomPoint.Y := Height-31;
   TopHook.Y := 0;
   BottomHook := b_hook;
   TopHook.X := p1X;
   IPoint.Y := 8;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Free;
   FStatement := nil;
end;

function TFolderBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TFolderBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, Branch.Hook.X, Branch.Hook.Y);
   result.CloneFrom(Self);
end;

constructor TFolderBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 61, 70, 70, 30);
end;

procedure TFolderBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 30;
      BottomPoint.Y := Height - 31;
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1));
      DrawArrowLine(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      DrawBlockLabel(5, 2, GInfra.CurrentLang.LabelFolder);
   end;
   DrawI;
end;

function TFolderBlock.GetDiamondPoint: TPoint;
begin
   result := Point(-1, -1);
end;

procedure TFolderBlock.SetWidth(const AMinX: integer);
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
   if Resize and VResizeInd then
   begin
      if Expanded then
         Inc(Branch.Hook.Y, NewHeight-Height)
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 30;
      end;
   end;
   if Resize and HResizeInd and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

end.