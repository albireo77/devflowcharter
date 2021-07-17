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
   Vcl.Graphics, System.SysUtils, Base_Block, Types;

type

   TInstrBlock = class(TBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
      protected
         procedure Paint; override;
   end;


implementation

uses
   System.StrUtils, Vcl.Forms, Vcl.Controls, System.Types, System.Classes, Infrastructure;

constructor TInstrBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms);

   FStatement.SetBounds(1, 1, ABlockParms.w-2, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.SetLRMargins(2, 2);

   BottomHook := ABlockParms.w div 2;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := FStatement.BoundsRect.Bottom + 1;
   IPoint.X := BottomHook + 30;
   IPoint.Y := BottomPoint.Y + 8;
   TopHook.X := BottomHook;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 51;
end;

constructor TInstrBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blInstr, 0, 0, 140, 51));
end;

procedure TInstrBlock.Paint;
var
   r: TRect;
begin
   inherited;
   r := FStatement.BoundsRect;
   r.Inflate(1, 1);
   BottomPoint.Y := r.Bottom;
   IPoint.Y := r.Bottom + 8;
   DrawArrow(BottomPoint, BottomPoint.X, Height-1);
   Canvas.FrameRect(r);
   DrawBlockLabel(5, r.Bottom, GInfra.CurrentLang.LabelInstr);
   DrawI;
end;

end.