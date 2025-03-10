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

  TWinControlHack = class(TWinControl);

  TCorner = class(TPanel)
     protected
        procedure Paint; override;
  end;

   TTextBlock = class(TMultiLineBlock)
      public
         constructor Create(AParentBranch: TBranch); overload;
         constructor Create(AParentBranch: TBranch; const ABlockParms: TBlockParms); overload;
         procedure ChangeColor(AColor: TColor); override;
      protected
         FCorner: TCorner;
         procedure Paint; override;
         procedure OnChangeStatements(Sender: TObject);
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
   end;

implementation

uses
   System.Types, Constants, Infrastructure;

constructor TTextBlock.Create(AParentBranch: TBranch; const ABlockParms: TBlockParms);
begin
   inherited Create(AParentBranch, ABlockParms);
   FStatements.Font.Color := TEXT_COLOR;
   FStatements.OnChange := OnChangeStatements;
   Font.Color := TEXT_COLOR;
   FCorner := TCorner.Create(Self);
   FCorner.Parent := Self;
   FCorner.Color := FStatements.Color;
   FCorner.BevelOuter := bvNone;
   FCorner.DoubleBuffered := True;
   FCorner.ControlStyle := FCorner.ControlStyle + [csOpaque];
   FCorner.SetBounds(Width-15, 0, 15, 15);
end;

constructor TTextBlock.Create(AParentBranch: TBranch);
begin
   Create(AParentBranch, TBlockParms.New(blText, 0, 0, 140, 91));
end;

procedure TTextBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelText);
end;

function TTextBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := inherited;
   if FHResize and result then
      FCorner.Left := Width - FCorner.Width;
end;

procedure TTextBlock.OnChangeStatements(Sender: TObject);
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

   Canvas.Pen.Color := TWinControlHack(Parent).Color;
   Canvas.Brush.Color := Canvas.Pen.Color;
   Canvas.Polygon([r.TopLeft, Point(r.Right, r.Top), r.BottomRight]);

   Canvas.Pen.Color := GSettings.PenColor;
   Canvas.Brush.Color := Color;
   Canvas.Polygon([r.TopLeft, r.BottomRight, Point(r.Left, r.Bottom)]);
end;

end.
