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
   Controls, Graphics, Classes, SysUtils, Base_Block, ComCtrls, CommonInterfaces, StdCtrls;

type

   TReturnBlock = class(TBlock)
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(const AColor: TColor); override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function GetDescription: string; override;
      protected
         FReturnLabel: string;
         procedure Paint; override;
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         function GetDefaultWidth: integer;
   end;

implementation

uses
   Types, Windows, ApplicationCommon, StrUtils, Forms, Project, UserFunction,
   Main_Block, CommonTypes, FastcodeAnsiStringReplaceUnit;

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

   FStatement.SetBounds((Width div 2)-26, 31, 52, 19);
   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.Alignment := taCenter;

   BottomPoint.X := Width div 2;
   BottomPoint.Y := 19;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := 30;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
end;

function TReturnBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TReturnBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
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
   DrawEllipsedText(Point(Width div 2, 30), FReturnLabel);
   DrawBlockLabel(1, Height-1, GInfra.CurrentLang.LabelReturn, false, true);
   Canvas.Font.Style := lFontStyles;
   DrawI;
end;

function TReturnBlock.GetDefaultWidth: integer;
var
   R: TRect;
begin
   R := GetEllipseTextRect(Point(0, 0), FReturnLabel);
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
   if fsStrikeOut in Font.Style then
      exit;
   if ALangId = PASCAL_LANG_ID then
   begin
      lIndent := DupeString(GSettings.IndentString, ADeep);
      lExpr := Trim(FStatement.Text);
      lIsInFunc := false;
      if lExpr <> '' then
      begin
         iter := GProject.GetUserFunctions;
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

procedure TReturnBlock.UpdateEditor(AEdit: TCustomEdit);
var
   lLine: TChangeLine;
   lList: TStringList;
begin
   if PerformEditorUpdate then
   begin
      lLine := TInfra.GetChangeLine(Self, FStatement);
      if lLine.Row <> ROW_NOT_FOUND then
      begin
         lList := TStringList.Create;
         try
            GenerateCode(lList, GInfra.CurrentLang.Name, 0);
            lLine.Text := TInfra.ExtractIndentString(lLine.Text) + lList.Text;
         finally
            lList.Free;
         end;
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(lLine);
         TInfra.GetEditorForm.SetCaretPos(lLine);
      end;
   end;
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

function TReturnBlock.GetDescription: string;
begin
   if GInfra.CurrentLang.ReturnDesc <> '' then
      result := FastCodeAnsiStringReplace(GInfra.CurrentLang.ReturnDesc, PRIMARY_PLACEHOLDER, Trim(FStatement.Text))
   else
      result := inherited GetDescription;
end;


end.
