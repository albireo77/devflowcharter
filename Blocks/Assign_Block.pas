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


unit Assign_Block;

interface

uses
   Vcl.Graphics, System.SysUtils, Base_Block, CommonInterfaces;

type

   TAssignBlock = class(TBlock)
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         procedure ChangeColor(const AColor: TColor); override;
      protected
         procedure Paint; override;
   end;


implementation

uses
   System.StrUtils, Vcl.Forms, Vcl.Controls, System.Types, System.Classes, CommonTypes,
   ApplicationCommon;

constructor TAssignBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin

   FType := blAssign;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatement.SetBounds(0, 0, AWidth, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.BorderStyle := bsSingle;
   FStatement.Color := GSettings.RectColor;

   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := 19;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 30;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 51;
end;

constructor TAssignBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 51);
end;

function TAssignBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TAssignBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

procedure TAssignBlock.Paint;
begin
   inherited;
   DrawArrowLine(Point(BottomPoint.X, 19), Point(BottomPoint.X, Height-1));
   DrawBlockLabel(5, 20, GInfra.CurrentLang.LabelAssign);
   DrawI;
end;

procedure TAssignBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.RectColor = GSettings.DesktopColor then
      FStatement.Color := AColor
   else
      FStatement.Color := GSettings.RectColor;
end;

end.