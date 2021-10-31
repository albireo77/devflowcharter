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
   Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, System.Classes, Vcl.ComCtrls, Base_Block,
   StatementMemo, MemoEx, Types;

type

   TMultiLineBlock = class(TBlock)
      public
         FStatements: TStatementMemo;
         function GetTextControl: TCustomEdit; override;
         function GetMemoEx: TMemoEx; override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         function GetTreeNodeText(ANodeOffset: integer = 0): string; override;
         procedure CloneFrom(ABlock: TBlock); override;
      protected
         FErrLine: integer;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload; virtual;
         procedure Paint; override;
         procedure OnDblClickMemo(Sender: TObject);
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure OnMouseDownMemo(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure OnKeyUpMemo(Sender: TObject; var Key: Word; Shift: TShiftState);
   end;

implementation

uses
{$IFDEF USE_CODEFOLDING}
   SynEditCodeFolding,
{$ENDIF}
   System.SysUtils, System.Types, System.UITypes, Infrastructure, Constants, LangDefinition;

constructor TMultiLineBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms);

   FStatements := TStatementMemo.Create(Self);
   FStatements.Parent := Self;
   FStatements.SetBounds(1, 1, ABlockParms.w-2, Height-31);
   FStatements.Font.Assign(Font);
   FStatements.OnDblClick := OnDblClickMemo;
   FStatements.OnMouseDown := OnMouseDownMemo;
   FStatements.OnKeyUp := OnKeyUpMemo;
   FStatements.OnChange := OnChangeMemo;
   FStatements.PopupMenu := Page.Form.pmEdits;
   if FStatements.CanFocus then
      FStatements.SetFocus;

   BottomHook := ABlockParms.w div 2;
   BottomPoint.X := BottomHook;
   BottomPoint.Y := FStatements.BoundsRect.Bottom + 1;
   IPoint.X := BottomHook + 30;
   IPoint.Y := BottomPoint.Y + 8;
   TopHook.X := BottomHook;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 48;
   FStatement.Free;
   FStatement := nil;
   FErrLine := -1;
end;

procedure TMultiLineBlock.CloneFrom(ABlock: TBlock);
begin
   inherited CloneFrom(ABlock);
   if ABlock is TMultiLineBlock then
      FStatements.CloneFrom(TMultiLineBlock(ABlock).FStatements)
end;

procedure TMultiLineBlock.OnDblClickMemo(Sender: TObject);
begin
   FStatements.SelectAll;
end;

procedure TMultiLineBlock.Paint;
begin
   inherited;
   var r := FStatements.BoundsRect;
   r.Inflate(1, 1);
   BottomPoint.Y := r.Bottom;
   IPoint.Y := r.Bottom + 8;
   DrawArrow(BottomPoint, BottomPoint.X, Height-1);
   Canvas.FrameRect(r);
   DrawI;
end;

function TMultiLineBlock.GetTextControl: TCustomEdit;
begin
   result := FStatements;
end;

function TMultiLineBlock.GetMemoEx: TMemoEx;
begin
   result := FStatements;
end;

procedure TMultiLineBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if FHResize and Resize then
   begin
      BottomPoint.X := Width div 2;
      IPoint.X := BottomPoint.X + 30;
      TopHook.X := BottomPoint.X;
   end;
   if FVResize and Resize then
   begin
      BottomPoint.Y := FStatements.BoundsRect.Bottom + 1;
      IPoint.Y := BottomPoint.Y + 8;
   end;
end;

procedure TMultiLineBlock.UpdateEditor(AEdit: TCustomEdit);
var
   chLine: TChangeLine;
   templateLines: TStringList;
   i, rowNum: integer;
begin
   if PerformEditorUpdate then
   begin
      chLine := TInfra.GetChangeLine(Self, FStatements);
      if chLine.CodeRange.FirstRow <> ROW_NOT_FOUND then
      begin
         if (chLine.CodeRange.Lines <> nil) and GSettings.UpdateEditor and not SkipUpdateEditor then
         begin
            templateLines := TStringList.Create;
            try
               GenerateCode(templateLines, GInfra.CurrentLang.Name, TInfra.GetEditorForm.GetIndentLevel(chLine.CodeRange.FirstRow, chLine.CodeRange.Lines));
               if GSettings.IndentChar = TAB_CHAR then
                  TInfra.IndentSpacesToTabs(templateLines);
               rowNum := chLine.CodeRange.LastRow - chLine.CodeRange.FirstRow + 1;
               chLine.CodeRange.Lines.BeginUpdate;
               for i := 1 to rowNum do
                  chLine.CodeRange.Lines.Delete(chLine.CodeRange.FirstRow);
{$IFDEF USE_CODEFOLDING}
               if chLine.CodeRange.FoldRange <> nil then
               begin
                  if chLine.CodeRange.IsFolded then
                  begin
                     rowNum := templateLines.Count - rowNum;
                     chLine.CodeRange.FoldRange.Widen(rowNum);
                     for i := 0 to templateLines.Count-1 do
                        chLine.CodeRange.Lines.InsertObject(chLine.CodeRange.FirstRow, templateLines[i], templateLines.Objects[i]);
                  end
                  else
                  begin
                     var foldRegion := chLine.CodeRange.FoldRange.FoldRegion;
                     TInfra.GetEditorForm.RemoveFoldRange(chLine.CodeRange.FoldRange);
                     for i := templateLines.Count-1 downto 0 do
                        chLine.CodeRange.Lines.InsertObject(chLine.CodeRange.FirstRow, templateLines[i], templateLines.Objects[i]);
                     TInfra.GetEditorForm.OnChangeEditor;
                     var foldRanges := TInfra.GetEditorForm.FindFoldRangesInCodeRange(chLine.CodeRange, templateLines.Count);
                     try
                        if (foldRanges <> nil) and (foldRanges.Count > 0) and (foldRanges[0].FoldRegion = foldRegion) and not foldRanges[0].Collapsed then
                        begin
                           TInfra.GetEditorForm.memCodeEditor.Collapse(foldRanges[0]);
                           TInfra.GetEditorForm.memCodeEditor.Refresh;
                        end;
                     finally
                        foldRanges.Free;
                     end;
                  end;
               end
               else
{$ENDIF}
               begin
                  for i := templateLines.Count-1 downto 0 do
                     chLine.CodeRange.Lines.InsertObject(chLine.CodeRange.FirstRow, templateLines[i], templateLines.Objects[i]);
               end;
               chLine.CodeRange.Lines.EndUpdate;
               TInfra.GetEditorForm.OnChangeEditor;
            finally
               templateLines.Free;
            end;
         end;
         TInfra.GetEditorForm.SetCaretPos(chLine);
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
   if Key in [vkUp, vkDown, vkLeft, vkRight] then
      OnMouseDownMemo(Sender, mbLeft, Shift, 0, 0);
end;

function TMultiLineBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
begin
   result := AParentNode;
   for var i := 0 to FStatements.Lines.Count-1 do
      TTreeNodeWithFriend(AParentNode.Owner.AddChildObject(AParentNode, GetTreeNodeText(i), FStatements)).Offset := i;
   if TInfra.IsNOkColor(FStatements.Font.Color) then
   begin
      AParentNode.MakeVisible;
      AParentNode.Expand(false);
   end;
end;

function TMultiLineBlock.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := '';
   if ANodeOffset < FStatements.Lines.Count then
   begin
      result := FStatements.Lines[ANodeOffset];
      if ANodeOffset = FErrLine then
         result := result + GetErrorMsg(FStatements);
   end;
end;

end.
