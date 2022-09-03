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
   Vcl.Graphics, System.Classes, Vcl.StdCtrls, Base_Block, Types, System.Types;

type

   TReturnBlock = class(TBlock)
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(AColor: TColor); override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function GetDescTemplate(const ALangId: string): string; override;
      protected
         FReturnLabel: string;
         procedure Paint; override;
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         function GetReturnEllipseRect: TRect;
         procedure PutTextControls; override;
   end;

implementation

uses
   Vcl.Controls, System.SysUtils, System.UITypes, System.Math, Infrastructure,
   Project, UserFunction, LangDefinition, Constants, YaccLib;

constructor TReturnBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpEllipse, yymReturn, taCenter);

   FReturnLabel := i18Manager.GetString('CaptionExit');

   Width := Max(Width, GetReturnEllipseRect.Width+48);

   BottomHook := Width div 2;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := 19;
   TopHook.X := BottomHook;

   PutTextControls;

   FStatement.Anchors := [akRight, akLeft, akTop];
   FStatement.Color := GSettings.DesktopColor;
end;

constructor TReturnBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blReturn, 0, 0, 140, 63));
end;

procedure TReturnBlock.Paint;
begin
   inherited;
   var fontStyles := Canvas.Font.Style;
   Canvas.Font.Style := [];
   DrawEllipsedText(BottomHook, GetReturnEllipseRect.Height, FReturnLabel);
   Canvas.Font.Style := fontStyles;
   DrawI;
end;

function TReturnBlock.GetReturnEllipseRect: TRect;
begin
   result := GetEllipseTextRect(0, 0, FReturnLabel);
end;

function TReturnBlock.GetDescTemplate(const ALangId: string): string;
begin
   result := '';
   var lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.ReturnDescTemplate;
end;

function TReturnBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      Exit;
   if ALangId = PASCAL_LANG_ID then
   begin
      var expr := Trim(FStatement.Text);
      var inFunction := false;
      var userFunction: TUserFunction := nil;
      if not expr.IsEmpty then
      begin
         for userFunction in GProject.GetUserFunctions do
         begin
            inFunction := userFunction.Active and (userFunction.Body = FTopParentBlock) and (userFunction.Header <> nil) and (userFunction.Header.cbType.ItemIndex > 0);
            if inFunction then
               break;
         end;
      end;
      var tmpList := TStringList.Create;
      try
         var indnt := GSettings.IndentString(ADeep);
         if inFunction then
            tmpList.AddObject(Format('%s%s %s %s;', [indnt, userFunction.Header.edtName.Text, GInfra.GetLangDefinition(ALangId).AssignOperator, expr]), Self);
         if not (((FTopParentBlock.Branch.Count > 0) and (FTopParentBlock.Branch.Last = Self)) and inFunction) then
            tmpList.AddObject(indnt + 'exit;', Self);
         TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
         result := tmpList.Count;
      finally
         tmpList.Free;
      end;
   end
   else
      result := inherited GenerateCode(ALines, ALangId, ADeep, AFromLine);
end;

procedure TReturnBlock.UpdateEditor(AEdit: TCustomEdit);
begin
   if PerformEditorUpdate then
   begin
      var chLine := TInfra.GetChangeLine(Self, FStatement);
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         var list := TStringList.Create;
         try
            GenerateCode(list, GInfra.CurrentLang.Name, 0);
            chLine.Text := TInfra.ExtractIndentString(chLine.Text) + list.Text;
         finally
            list.Free;
         end;
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(chLine);
         TInfra.GetEditorForm.SetCaretPos(chLine);
      end;
   end;
end;

procedure TReturnBlock.ChangeColor(AColor: TColor);
begin
   inherited ChangeColor(AColor);
   FStatement.Color := AColor;
end;

procedure TReturnBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if IsAtSelectPos(Point(X, Y)) then
      Select
   else
      DeSelect;
end;

procedure TReturnBlock.PutTextControls;
begin
   var y := GetReturnEllipseRect.Height + 1;
   IPoint.X := BottomHook + 30;
   IPoint.Y := y;
   FStatement.SetBounds(BottomHook-26, y, 52, FStatement.Height);
end;

end.
