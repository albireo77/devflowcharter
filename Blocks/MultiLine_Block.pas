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



unit MultiLine_Block;

interface

uses
   Controls, StdCtrls, Graphics, Classes, Base_Block, SysUtils, CommonInterfaces,
   ExtCtrls, StatementMemo, ComCtrls;

type

   TMultiLineBlock = class(TBlock)
      public
         FStatements: TStatementMemo;
         function GetTextControl: TCustomEdit; override;
         procedure ChangeColor(const AColor: TColor); override;
         function GetFrontMemo: TMemo; override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
      protected
         FErrLine: integer;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; virtual;
         procedure Paint; override;
         procedure OnDblClickMemo(Sender: TObject);
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure OnMouseDownMemo(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure OnKeyUpMemo(Sender: TObject; var Key: Word; Shift: TShiftState);
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; override;
   end;

implementation

uses
{$IFDEF USE_CODEFOLDING}
   SynEditCodeFolding,
{$ENDIF}
   ApplicationCommon, StrUtils, CommonTypes, Forms, LangDefinition, Windows;

constructor TMultiLineBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatements := TStatementMemo.Create(Self);
   FStatements.Parent := Self;
   FStatements.SetBounds(0, 0, AWidth, Height-31);
   FStatements.OnDblClick := OnDblClickMemo;
   FStatements.OnMouseDown := OnMouseDownMemo;
   FStatements.OnKeyUp := OnKeyUpMemo;
   FStatements.OnChange := OnChangeMemo;
   FStatements.PopupMenu := Page.Form.pmEdits;
   if FStatements.CanFocus then
      FStatements.SetFocus;

   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := Height - 31;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := FStatements.Height + 10;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 48;
   FStatement.Free;
   FStatement := nil;
   FErrLine := -1;
end;

procedure TMultiLineBlock.OnDblClickMemo(Sender: TObject);
begin
   FStatements.SelectAll;
end;

procedure TMultiLineBlock.Paint;
begin
   inherited;
   DrawArrowLine(Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
   DrawI;
end;

function TMultiLineBlock.GetTextControl: TCustomEdit;
begin
   result := FStatements;
end;

function TMultiLineBlock.GetFrontMemo: TMemo;
begin
   result := FStatements;
end;

procedure TMultiLineBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if HResizeInd and Resize then
   begin
      BottomPoint.X := Width div 2;
      IPoint.X := BottomPoint.X + 30;
      TopHook.X := BottomPoint.X;
   end;
   if VResizeInd and Resize then
      IPoint.Y := FStatements.Height + 10;
end;

procedure TMultiLineBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.RectColor = GSettings.DesktopColor then
      FStatements.Color := AColor
   else
      FStatements.Color := GSettings.RectColor;
end;

procedure TMultiLineBlock.UpdateEditor(AEdit: TCustomEdit);
var
   lLine: TChangeLine;
   lTemplateLines: TStringList;
   i, lRowNumber: integer;
{$IFDEF USE_CODEFOLDING}
   lFoldRegion: TFoldRegionItem;
   lFoldRanges: TSynEditFoldRanges;
{$ENDIF}
begin
   if PerformEditorUpdate then
   begin
      lLine := TInfra.GetChangeLine(Self, FStatements);
      if lLine.CodeRange.FirstRow <> ROW_NOT_FOUND then
      begin
         if GSettings.UpdateEditor and not SkipUpdateEditor then
         begin
            lTemplateLines := TStringList.Create;
            try
               GenerateCode(lTemplateLines, GInfra.CurrentLang.Name, TInfra.GetEditorForm.GetIndentLevel(lLine.CodeRange.FirstRow, lLine.CodeRange.Lines));
               if lLine.CodeRange.Lines <> nil then
               begin
                  lRowNumber := lLine.CodeRange.LastRow - lLine.CodeRange.FirstRow + 1;
                  lLine.CodeRange.Lines.BeginUpdate;
                  for i := 1 to lRowNumber do
                     lLine.CodeRange.Lines.Delete(lLine.CodeRange.FirstRow);
{$IFDEF USE_CODEFOLDING}
                  if lLine.CodeRange.FoldRange <> nil then
                  begin
                     if lLine.CodeRange.IsFolded then
                     begin
                        lRowNumber := lTemplateLines.Count - lRowNumber;
                        lLine.CodeRange.FoldRange.Widen(lRowNumber);
                        for i := 0 to lTemplateLines.Count-1 do
                           lLine.CodeRange.Lines.InsertObject(lLine.CodeRange.FirstRow, lTemplateLines[i], lTemplateLines.Objects[i]);
                     end
                     else
                     begin
                        lFoldRegion := lLine.CodeRange.FoldRange.FoldRegion;
                        TInfra.GetEditorForm.RemoveFoldRange(lLine.CodeRange.FoldRange);
                        for i := lTemplateLines.Count-1 downto 0 do
                           lLine.CodeRange.Lines.InsertObject(lLine.CodeRange.FirstRow, lTemplateLines[i], lTemplateLines.Objects[i]);
                        TInfra.GetEditorForm.OnChangeEditor;
                        lFoldRanges := TInfra.GetEditorForm.FindFoldRangesInCodeRange(lLine.CodeRange, lTemplateLines.Count);
                        try
                           if (lFoldRanges <> nil) and (lFoldRanges.Count > 0) and (lFoldRanges[0].FoldRegion = lFoldRegion) and not lFoldRanges[0].Collapsed then
                           begin
                              TInfra.GetEditorForm.memCodeEditor.Collapse(lFoldRanges[0]);
                              TInfra.GetEditorForm.memCodeEditor.Refresh;
                           end;
                        finally
                           lFoldRanges.Free;
                        end;
                     end;
                  end
                  else
{$ENDIF}
                  begin
                     for i := lTemplateLines.Count-1 downto 0 do
                        lLine.CodeRange.Lines.InsertObject(lLine.CodeRange.FirstRow, lTemplateLines[i], lTemplateLines.Objects[i]);
                  end;
                  lLine.CodeRange.Lines.EndUpdate;
                  TInfra.GetEditorForm.OnChangeEditor;
               end;
            finally
               lTemplateLines.Free;
            end;
         end;
         TInfra.GetEditorForm.SetCaretPos(lLine);
      end;
   end;
end;

procedure TMultiLineBlock.OnMouseDownMemo(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      OnMouseDown(Sender, Button, Shift, X, Y);
   if Button = mbLeft then
      TInfra.GetEditorForm.SetCaretPos(TInfra.GetChangeLine(Self, FStatements));
end;

procedure TMultiLineBlock.OnKeyUpMemo(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT] then
      OnMouseDownMemo(Sender, mbLeft, Shift, 0, 0);
end;

function TMultiLineBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lErrMsg, lLabel: string;
   i: integer;
begin
   result := AParentNode;
   lErrMsg := GetErrorMsg(FStatements);
   for i := 0 to FStatements.Lines.Count-1 do
   begin
      if Trim(FStatements.Lines[i]) <> '' then
      begin
         lLabel := FStatements.Lines[i];
         if i = FErrLine then
            lLabel := lLabel + lErrMsg;
         AParentNode.Owner.AddChildObject(AParentNode, lLabel, FStatements);
      end;
   end;
   if lErrMsg <> '' then
   begin
      AParentNode.MakeVisible;
      AParentNode.Expand(false);
   end;
end;

end.
