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



unit Text_Block;

interface

uses
   Controls, StdCtrls, Graphics, Classes, Base_Block, SysUtils, CommonInterfaces,
   ExtCtrls, MultiLine_Block;

type

  TCorner = class(TPanel)
     protected
        procedure Paint; override;
  end;

   TTextBlock = class(TMultiLineBlock)
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; override;
         function Clone(const ABranch: TBranch): TBlock; override;
         procedure ChangeColor(const AColor: TColor); override;
      protected
         FCorner: TCorner;
         procedure Paint; override;
         procedure OnChangeMemo(Sender: TObject); override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
   end;

implementation

uses
   ApplicationCommon, StrUtils, CommonTypes;

constructor TTextBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   FType := blText;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatements.Font.Color := TEXT_COLOR;
   Font.Color := TEXT_COLOR;
   FCorner := TCorner.Create(Self);
   FCorner.Parent := Self;
   FCorner.Color := GSettings.RectColor;
   FCorner.BevelOuter := bvNone;
   FCorner.Ctl3D := false;
   FCorner.DoubleBuffered := true;
   FCorner.ControlStyle := FCorner.ControlStyle + [csOpaque];
   FCorner.SetBounds(Width-15, 0, 15, 15);
end;

constructor TTextBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 91);
end;

function TTextBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TTextBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

procedure TTextBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelText);
   if FCorner <> nil then
      FCorner.Invalidate;
end;

procedure TCorner.Paint;
var
   lParent: TTextBlock;
begin
   inherited;
   lParent := TTextBlock(Parent);
   Canvas.Pen.Color := clBlack;
   Canvas.PolyLine([Point(0, 0), Point(Width-1, Height-1), Point(0, Height-1), Point(0, 0)]);
   Canvas.Brush.Color := lParent.FStatements.Color;
   Canvas.FloodFill(2, Height-2, clBlack, fsBorder);
   Canvas.Brush.Color := lParent.ParentBlock.Color;
   Canvas.FloodFill(Width-1, 0, clBlack, fsBorder);
end;

procedure TTextBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   inherited MyOnCanResize(Sender, NewWidth, NewHeight, Resize);
   if HResizeInd and Resize then
      FCorner.Left := Width - 15;
end;

procedure TTextBlock.OnChangeMemo(Sender: TObject);
begin
   inherited;
   UpdateEditor(nil);
end;

procedure TTextBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   FStatements.Font.Color := Font.Color;
end;

end.
