{
   Copyright © 2007 Frost666, The devFlowcharter project.
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

unit Return_Block;

interface

uses
   Controls, Graphics, Classes, SysUtils, Base_Block, ComCtrls, CommonInterfaces;

type

   TReturnBlock = class(TBlock)
      public
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload;
         constructor Create(const ABranch: TBranch; const ASource: TReturnBlock); overload;
         constructor Create(const ABranch: TBranch); overload;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(const AColor: TColor); override;
      protected
         FReturnLabel: string;
         procedure Paint; override;
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         function GetDefaultWidth: integer; override;
   end;

implementation

uses
   Types, Windows, ApplicationCommon, StrUtils, Forms, Project, UserFunction, Main_Block, CommonTypes;

constructor TReturnBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
var
   lDefWidth: integer;
begin

   FType := blReturn;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FReturnLabel := i18Manager.GetString('CaptionExit');

   lDefWidth := GetDefaultWidth;
   if lDefWidth > Width then
      Width := lDefWidth;

   FStatement.SetBounds((Width div 2)-30, 31, 60, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.Alignment := taCenter;

   BottomPoint.X := Width div 2;
   BottomPoint.Y := 19;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 30;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
end;

constructor TReturnBlock.Create(const ABranch: TBranch; const ASource: TReturnBlock);
begin
   Create(ABranch, ASource.Left, ASource.Top, ASource.Width, ASource.Height);
   SetFont(ASource.Font);
   Visible := ASource.Visible;
   FStatement.Text := ASource.FStatement.Text;
end;

constructor TReturnBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 53);
end;

procedure TReturnBlock.Paint;
var
   lFontStyles: TFontStyles;
begin
   inherited;
   lFontStyles := Canvas.Font.Style;
   Canvas.Font.Style := [];
   TInfra.DrawEllipsedText(Canvas, Point(Width div 2, 30), FReturnLabel);
   Canvas.Font.Style := lFontStyles;
   DrawI;
end;

function TReturnBlock.GetDefaultWidth: integer;
var
   R: TRect;
begin
   R := TInfra.GetEllipseTextRect(Canvas, Point(0, 0), FReturnLabel);
   result := R.Right - R.Left + 48;
end;

function TReturnBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lIndent, lExpr: string;
   iter: IIterator;
   lFunction: TUserFunction;
   lIsInFunc: boolean;
   lTmpList: TStringList;
begin
   result := 0;
   if ALangId = PASCAL_LANG_ID then
   begin
      lIndent := DupeString(GSettings.IndentString, ADeep);
      lExpr := Trim(FStatement.Text);
      lIsInFunc := false;
      if lExpr <> '' then
      begin
         iter := GProject.GetUserFunctionIterator;
         while iter.HasNext do
         begin
            lFunction := TUserFunction(iter.Next);
            lIsInFunc := lFunction.Active and (lFunction.Body = FTopParentBlock) and (lFunction.Header <> nil) and (lFunction.Header.cbType.ItemIndex > 0);
            if lIsInFunc then break;
         end;
      end;
      lTmpList := TStringList.Create;
      try
         if lIsInFunc then
            lTmpList.AddObject(lIndent + lFunction.Header.edtName.Text + ' ' + GInfra.GetLangDefinition(ALangId).AssignOperator + ' ' + lExpr + ';', Self);
         if not ((TMainBlock(FTopParentBlock).GetBranch(PRIMARY_BRANCH_IND).Last = Self) and lIsInFunc) then
            lTmpList.AddObject(lIndent + 'exit;', Self);
         TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
         result := lTmpList.Count;
      finally
         lTmpList.Free;
      end;
   end
   else
      result := inherited GenerateCode(ALines, ALangId, ADeep, AFromLine);
end;

procedure TReturnBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   FStatement.Color := AColor;
end;

procedure TReturnBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
begin
   SelectBlock(Point(X, Y));
end;


end.
