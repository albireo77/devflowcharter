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


unit Instr_Block;

interface

uses
   Vcl.Graphics, System.SysUtils, Base_Block, CommonInterfaces;

type

   TInstrBlock = class(TBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight: integer; AId: integer = ID_INVALID); overload;
         function Clone(ABranch: TBranch): TBlock; override;
      protected
         procedure Paint; override;
   end;


implementation

uses
   System.StrUtils, Vcl.Forms, Vcl.Controls, System.Types, System.Classes, CommonTypes,
   ApplicationCommon;

constructor TInstrBlock.Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight: integer; AId: integer = ID_INVALID);
begin

   FType := blInstr;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatement.SetBounds(0, 0, AWidth, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.BorderStyle := bsSingle;

   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := 19;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 30;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 51;
end;

constructor TInstrBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 51);
end;

function TInstrBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := TInstrBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

procedure TInstrBlock.Paint;
begin
   inherited;
   DrawArrowLine(BottomPoint.X, 19, BottomPoint.X, Height-1);
   DrawBlockLabel(5, 20, GInfra.CurrentLang.LabelInstr);
   DrawI;
end;

end.