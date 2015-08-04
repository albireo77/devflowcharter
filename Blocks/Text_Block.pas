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
   ExtCtrls, StatementMemo;

type

  TCornerPanel = class(TPanel)
     protected
        procedure Paint; override;
  end;

   TTextBlock = class(TBlock)
      public
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload;
         constructor Create(const ABranch: TBranch; const ASource: TTextBlock); overload;
         constructor Create(const ABranch: TBranch); overload;
         function GetTextControl: TCustomEdit; override;
         procedure ChangeColor(const AColor: TColor); override;
         function GetFrontMemo: TMemo; override;
         procedure UpdateCodeEditor(AEdit: TCustomEdit); override;
      protected
         FStatements: TStatementMemo;
         FCorner: TCornerPanel;
         procedure Paint; override;
         procedure MyOnDblClick(Sender: TObject);
         procedure OnChangeMemo(Sender: TObject); override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
   end;

implementation

uses
   ApplicationCommon, StrUtils, CommonTypes, Forms, LangDefinition, SourceEditor_Form;

constructor TTextBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin

   FType := blText;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatements := TStatementMemo.Create(Self);
   FStatements.Parent := Self;
   FStatements.SetBounds(0, 0, AWidth, Height-31);
   FStatements.OnDblClick := MyOnDblClick;
   FStatements.OnMouseDown := OnMouseDown;
   FStatements.OnChange := OnChangeMemo;
   if FStatements.CanFocus then
      FStatements.SetFocus;

   FCorner := TCornerPanel.Create(Self);
   FCorner.Parent := Self;
   FCorner.Color := GSettings.RectColor;
   FCorner.BevelOuter := bvNone;
   FCorner.Ctl3D := false;
   FCorner.DoubleBuffered := true;
   FCorner.ControlStyle := FCorner.ControlStyle + [csOpaque];
   FCorner.SetBounds(Width-15, 0, 15, 15);

   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := Height - 31;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := FStatements.Height + 10;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 48;
   FStatement.Visible := false;         // statement isn't used in this block

end;

constructor TTextBlock.Create(const ABranch: TBranch; const ASource: TTextBlock);
begin

   Create(ABranch, ASource.Left, ASource.Top, ASource.Width, ASource.Height);

   ChangeFontSize(ASource.FStatement.Font.Size);
   ChangeFontStyle(ASource.FStatement.Font.Style);
   Visible := ASource.Visible;
   FStatements.Text := ASource.FStatements.Text;

end;

constructor TTextBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 91);
end;

procedure TTextBlock.MyOnDblClick(Sender: TObject);
begin
   FStatements.SelectAll;
end;


procedure TTextBlock.Paint;
begin
   inherited;
   TInfra.DrawArrowLine(Canvas, Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
   if FCorner <> nil then
      FCorner.Repaint;
   DrawI;
end;

procedure TCornerPanel.Paint;
var
   lParent: TTextBlock;
begin
   inherited;
   with Canvas do
   begin
      lParent := TTextBlock(Parent);
      Pen.Color := clBlack;
      PolyLine([Point(0, 0), Point(Width-1, Height-1), Point(0, Height-1), Point(0, 0)]);
      Brush.Color := lParent.FStatements.Color;
      FloodFill(2, Height-2, clBlack, fsBorder);
      Brush.Color := lParent.ParentBlock.Color;
      FloodFill(Width-1, 0, clBlack, fsBorder);
   end;
end;

function TTextBlock.GetTextControl: TCustomEdit;
begin
   result := FStatements;
end;

function TTextBlock.GetFrontMemo: TMemo;
begin
   result := FStatements;
end;

procedure TTextBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if HResizeInd and Resize then
   begin
      BottomPoint.X := Width div 2;
      IPoint.X := BottomPoint.X + 30;
      TopHook.X := BottomPoint.X;
      FCorner.Left := Width - 15;
   end;
   if VResizeInd and Resize then
      IPoint.Y := FStatements.Height + 10;
end;

procedure TTextBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.RectColor = GSettings.DesktopColor then
      FStatements.Color := AColor
   else
      FStatements.Color := GSettings.RectColor;
   FStatements.Font.Color := GSettings.FontColor;
end;

procedure TTextBlock.UpdateCodeEditor(AEdit: TCustomEdit);
var
   lRange: TCodeRange;
   lTemplateLines: TStringList;
   i: integer;
begin
   lRange := SourceEditorForm.SelectCodeBlock(Self, false);
   if lRange.FirstLineIdx <> -1 then
   begin
      lTemplateLines := TStringList.Create;
      try
         GenerateCode(lTemplateLines, GInfra.CurrentLang.Name, SourceEditorForm.GetIndentLevel(lRange.FirstLineIdx));
         with SourceEditorForm.memCodeEditor do
         begin
            Lines.BeginUpdate;
            for i := 0 to lRange.LastLineIdx - lRange.FirstLineIdx do
               Lines.Delete(lRange.FirstLineIdx);
            for i := lTemplateLines.Count-1 downto 0 do
               Lines.InsertObject(lRange.FirstLineIdx, lTemplateLines[i], lTemplateLines.Objects[i]);
            GotoLineAndCenter(lRange.FirstLineIdx);
            Lines.EndUpdate;
            OnChange(SourceEditorForm.memCodeEditor);
         end;
      finally
         lTemplateLines.Free;
      end;
   end;
end;

procedure TTextBlock.OnChangeMemo(Sender: TObject);
begin
   inherited;
   if GSettings.UpdateCodeEditor and not SkipUpdateCodeEditor then
      UpdateCodeEditor(nil);
end;

end.
