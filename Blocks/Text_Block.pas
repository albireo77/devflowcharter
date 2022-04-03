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
   Vcl.Graphics, Vcl.ExtCtrls, Vcl.Controls, Base_Block, MultiLine_Block, Types;

type

  THackWinControl = class(TWinControl);

  TCorner = class(TPanel)
     protected
        procedure Paint; override;
  end;

   TTextBlock = class(TMultiLineBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         procedure ChangeColor(AColor: TColor); override;
      protected
         FCorner: TCorner;
         procedure Paint; override;
         procedure OnChangeMemo(Sender: TObject); override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
   end;

implementation

uses
   System.Classes, System.Types, Constants, Infrastructure;

constructor TTextBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin
   inherited Create(ABranch, ABlockParms);
   FStatements.Font.Color := TEXT_COLOR;
   Font.Color := TEXT_COLOR;
   FCorner := TCorner.Create(Self);
   FCorner.Parent := Self;
   FCorner.Color := FStatements.Color;
   FCorner.BevelOuter := bvNone;
   FCorner.Ctl3D := false;
   FCorner.DoubleBuffered := true;
   FCorner.ControlStyle := FCorner.ControlStyle + [csOpaque];
   FCorner.SetBounds(Width-15, 0, 15, 15);
end;

constructor TTextBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blText, 0, 0, 140, 91));
end;

procedure TTextBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelText);
end;

procedure TTextBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   inherited MyOnCanResize(Sender, NewWidth, NewHeight, Resize);
   if FHResize and Resize then
      FCorner.Left := Width - FCorner.Width;
end;

procedure TTextBlock.OnChangeMemo(Sender: TObject);
begin
   inherited;
   UpdateEditor(nil);
end;

procedure TTextBlock.ChangeColor(AColor: TColor);
begin
   inherited ChangeColor(AColor);
   FStatements.Font.Color := Font.Color;
   FCorner.Color := FStatements.Color;
end;

procedure TCorner.Paint;
begin
   inherited;
   var r := ClientRect;
   r.Inflate(0, 0, -1, -1);

   Canvas.Pen.Color := THackWinControl(Parent).Color;
   Canvas.Brush.Color := Canvas.Pen.Color;
   Canvas.Polygon([r.TopLeft, Point(r.Right, r.Top), r.BottomRight, r.TopLeft]);

   Canvas.Pen.Color := GSettings.PenColor;
   Canvas.Brush.Color := Color;
   Canvas.Polygon([r.TopLeft, r.BottomRight, Point(r.Left, r.Bottom), r.TopLeft]);
end;

end.
