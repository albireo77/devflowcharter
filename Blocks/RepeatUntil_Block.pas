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
   Controls, Messages, Forms, StdCtrls, Graphics, Classes, SysUtils, Base_Block,
   CommonInterfaces, Types;

type

   TRepeatUntilBlock = class(TGroupBlock)
      private
         FLeftLabel, FRightLabel: string;
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         procedure ChangeColor(const AColor: TColor); override;
         function GetDescription: string; override;
      protected
         procedure Paint; override;
         procedure SetWidth(const AMinX: integer); override;
         function GetDiamondPoint: TPoint; override;
   end;

implementation

uses
   ApplicationCommon, Windows, StrUtils, CommonTypes, FastcodeAnsiStringReplaceUnit;

constructor TRepeatUntilBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
begin

   FType := blRepeat;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

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
   BottomHook := b_hook;
   TopHook.X := p1X;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Color := GSettings.DiamondColor;
   FStatement.Alignment := taCenter;
   PutTextControls;

end;

function TRepeatUntilBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TRepeatUntilBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, Branch.Hook.X, Branch.Hook.Y);
   result.CloneFrom(Self);
end;

constructor TRepeatUntilBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 240, 111, 120, 120, 29);
end;

procedure TRepeatUntilBlock.Paint;
var
   lLeftLabel, lRightLabel: string;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := BottomHook + 40;
      IPoint.Y := Height - 25;
      BottomPoint.Y := Height - 50;
      PutTextControls;

      DrawArrowLine(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      DrawArrowLine(Point(5, Height-51), Point(5, 0), arrMiddle);
      DrawArrowLine(Point(BottomPoint.X, Height-51), Point(BottomPoint.X, Height-1));
      DrawTextLabel(BottomHook-60, Height-72, FLeftLabel, true);
      DrawTextLabel(BottomHook+60, Height-72, FRightLabel);
      DrawBlockLabel(BottomHook-20, Height-32, GInfra.CurrentLang.LabelRepeat, true);
      with Canvas do
      begin
         MoveTo(BottomPoint.X, Height-51);
         LineTo(BottomHook+60, Height-51);
         MoveTo(BottomHook-60, Height-51);
         LineTo(5, Height-51);
         MoveTo(5, 0);
         LineTo(Branch.Hook.X, TopHook.Y);
      end;
   end;
   DrawI;
end;

procedure TRepeatUntilBlock.SetWidth(const AMinX: integer);
begin
   if AMinX < BottomHook + 121 then
      Width := BottomHook + 121
   else
      Width := AMinX;
   BottomPoint.X := Width - 11;
end;

function TRepeatUntilBlock.GetDiamondPoint: TPoint;
begin
   result := Point(BottomHook, Height-81);
end;

procedure TRepeatUntilBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.DiamondColor = GSettings.DesktopColor then
      FStatement.Color := AColor
   else
      FStatement.Color := GSettings.DiamondColor;
end;

function TRepeatUntilBlock.GetDescription: string;
begin
   if GInfra.CurrentLang.RepeatDesc <> '' then
      result := FastCodeAnsiStringReplace(GInfra.CurrentLang.RepeatDesc, PRIMARY_PLACEHOLDER, Trim(FStatement.Text))
   else
      result := inherited GetDescription;
end;

end.