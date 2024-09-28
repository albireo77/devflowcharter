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


unit Case_Block;

interface

uses
   Vcl.StdCtrls, Vcl.Graphics, System.Classes, Vcl.ComCtrls, System.Types, Vcl.Controls,
   Base_Block, OmniXML, Types, Statement;

type

   TCaseBlock = class(TGroupBlock)
      protected
         FCaseLabel: string;
         DefaultBranch: TBranch;
         procedure Paint; override;
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
         procedure EditorAction(AEdit: TCustomEdit);
         function GetDiamondTop: TPoint; override;
         procedure PlaceBranchStatement(ABranch: TBranch);
         function  GetTemplateByControl(AControl: TControl; var AObject: TObject): string;
         procedure AfterRemovingBranch; override;
         function CreateBranchStatement(ABranchStatementId: integer = ID_INVALID): TStatement;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         procedure ResizeHorz(AContinue: boolean); override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ExpandFold(AResize: boolean); override;
         function AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchTextId: integer = ID_INVALID): TBranch; override;
         function InsertNewBranch(AIndex: integer): TBranch;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(ANode: IXMLNode): TError; override;
         procedure SaveInXML(ANode: IXMLNode); override;
         procedure ChangeColor(AColor: TColor); override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function IsDuplicatedCase(AEdit: TCustomEdit): boolean;
         procedure CloneFrom(ABlock: TBlock); override;
         function GetDescTemplate(const ALangId: string): string; override;
         function GetTreeNodeText(ANodeOffset: integer = 0): string; override;
   end;

const
   DEFAULT_BRANCH_IDX = PRIMARY_BRANCH_IDX;

implementation

uses
   System.StrUtils, System.Math, System.SysUtils, Infrastructure, Constants, YaccLib,
   OmniXMLUtils;

constructor TCaseBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpDiamond, taCenter, yymCase);

   FInitParms.Width := 200;
   FInitParms.Height := 131;
   FInitParms.BottomHook := 100;
   FInitParms.BranchPoint.X := 100;
   FInitParms.BottomPoint.X := 100;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   DefaultBranch := Branch;

   BottomPoint.X := ABlockParms.br.X;
   BottomPoint.Y := Height-31;
   TopHook.Y := 70;
   BottomHook := ABlockParms.bh;
   TopHook.X := ABlockParms.br.X;
   IPoint.Y := 50;
   FCaseLabel := i18Manager.GetString('CaptionCase');
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.EditorAction := EditorAction;

end;

procedure TCaseBlock.CloneFrom(ABlock: TBlock);
begin
   inherited CloneFrom(ABlock);
   if ABlock is TCaseBlock then
   begin
      var caseBlock := TCaseBlock(ABlock);
      for var i := DEFAULT_BRANCH_IDX+1 to caseBlock.FBranchList.Count-1 do
      begin
         var br2 := GetBranch(i);
         if br2 <> nil then
         begin
            var br := caseBlock.FBranchList[i];
            br2.Statement.Text := br.Statement.Text;
            br2.Statement.Visible := br.Statement.Visible;
         end;
      end;
   end;
end;

constructor TCaseBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blCase, 0, 0, 200, 131, 100, 99, 100));
end;

procedure TCaseBlock.Paint;
begin
   inherited;
   if Expanded then
   begin
      var pnt: TPoint;
      IPoint.X := DefaultBranch.Hook.X - 40;
      TopHook.Y := FDiamond.Bottom.Y + 10;
      BottomPoint.Y := Height - 31;
      DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
      for var i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
      begin
         pnt := FBranchList[i].Hook;
         DrawArrow(pnt.X, TopHook.Y, pnt.X, pnt.Y);
         PlaceBranchStatement(FBranchList[i]);
      end;
      var x := FDiamond.Bottom.X + FDiamond.Width div 4;
      var y := FDiamond.Bottom.Y - FDiamond.Height div 4 + 3;
      DrawTextLabel(x, y, FCaseLabel);
      DrawBlockLabel(FDiamond.Right.X+5, 1, GInfra.CurrentLang.LabelCase, False, True);
      Canvas.MoveTo(pnt.X, TopHook.Y);
      Canvas.LineTo(DefaultBranch.Hook.X, TopHook.Y);
      Canvas.LineTo(DefaultBranch.Hook.X, TopHook.Y-10);
      Canvas.MoveTo(BottomHook, BottomPoint.Y);
      Canvas.LineTo(BottomPoint.X, BottomPoint.Y);
   end;
   DrawI;
end;

procedure TCaseBlock.EditorAction(AEdit: TCustomEdit);
begin
   if GSettings.ParseCase then
   begin
      for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         var br := FBranchList[i];
         if br.Statement <> AEdit then
            br.Statement.Change;
      end;
   end;
   UpdateEditor(AEdit);
end;

function TCaseBlock.IsDuplicatedCase(AEdit: TCustomEdit): boolean;
begin
   result := False;
   if (AEdit <> nil) and (AEdit.Parent = Self) then
   begin
      for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         var edit := FBranchList[i].Statement;
         if (edit <> AEdit) and (Trim(edit.Text) = Trim(AEdit.Text)) then
         begin
            result := True;
            break;
         end;
      end;
   end;
end;

function TCaseBlock.AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchTextId: integer = ID_INVALID): TBranch;
begin
   result := inherited AddBranch(AHook, ABranchId);
   if FBranchList.IndexOf(result) > DEFAULT_BRANCH_IDX then       // don't execute when default branch is being added in constructor
   begin
      result.Statement := CreateBranchStatement(ABranchTextId);
      PlaceBranchStatement(result);
   end;
end;

function TCaseBlock.InsertNewBranch(AIndex: integer): TBranch;
begin
   result := nil;
   if AIndex < 0 then
      AIndex := FBranchList.Count;
   if AIndex > DEFAULT_BRANCH_IDX then
   begin
      result := TBranch.Create(Self, Point(FBranchList[AIndex-1].GetMostRight+60, Height-32));
      FBranchList.Insert(AIndex, result);
      FTopParentBlock.LockDrawing;
      try
         result.Statement := CreateBranchStatement;
         PlaceBranchStatement(result);
         ResizeWithDrawLock;
      finally
         FTopParentBlock.UnLockDrawing;
      end;
   end;
end;

function TCaseBlock.CreateBranchStatement(ABranchStatementId: integer = ID_INVALID): TStatement;
begin
   result := TStatement.Create(Self, yymCaseValue, taRightJustify, ABranchStatementId);
   result.Color := Color;
   result.EditorAction := UpdateEditor;
end;

procedure TCaseBlock.PlaceBranchStatement(ABranch: TBranch);
begin
   var idx := FBranchList.IndexOf(ABranch);
   if idx > DEFAULT_BRANCH_IDX then
   begin
      var prevBranch := FBranchList[idx-1];
      if prevBranch <> nil then
      begin
         var w := Min(ABranch.Hook.X-prevBranch.Hook.X-10, 300);
         ABranch.Statement.SetBounds(ABranch.Hook.X-w-5, TopHook.Y+1, w, ABranch.Statement.Height);
      end;
   end;
end;

procedure TCaseBlock.ResizeHorz(AContinue: boolean);
begin
   BottomHook := Branch.Hook.X;
   var rightX := 100;
   for var i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      var br := FBranchList[i];
      var leftX := rightX;
      br.Hook.X := leftX;
      var x := leftX;
      LinkBlocks(br);
      for var block in br do
         x := Min(block.Left, x);
      Inc(br.hook.X, leftX-x);
      LinkBlocks(br);
      PlaceBranchStatement(br);
      if not br.EndsWithReturnBlock then
      begin
         if br.Count > 0 then
            BottomHook := br.Last.Left + br.Last.BottomPoint.X
         else
            BottomHook := br.Hook.X;
      end;
      rightX := br.GetMostRight + 60;
   end;
   TopHook.X := DefaultBranch.Hook.X;
   BottomPoint.X := DefaultBranch.Hook.X;
   Width := rightX - 30;
   if AContinue then
      ParentBlock.ResizeHorz(AContinue);
end;

procedure TCaseBlock.ResizeVert(AContinue: boolean);
begin
   var maxh := 0;
   var hBranch := DefaultBranch;
   for var i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      var br := FBranchList[i];
      var h := br.Height;
      if h > maxh then
      begin
         maxh := h;
         hBranch := br;
      end;
   end;
   hBranch.Hook.Y := 99;
   Height := maxh + 131;
   for var i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      var br := FBranchList[i];
      if br <> hBranch then
         br.Hook.Y := maxh - br.Height + 99;
   end;
   LinkAllBlocks;
   if AContinue then
      ParentBlock.ResizeVert(AContinue);
end;

function TCaseBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin

   result := 0;
   var caseOfTemplate := GetBlockTemplate(ALangId);

   if (fsStrikeOut in Font.Style) or caseOfTemplate.IsEmpty then
      Exit;

   var statement := Trim(FStatement.Text);
   var caseLines := TStringList.Create;
   var tmpList := TStringList.Create;
   var lines := TStringList.Create;
   try
      for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         var edit := FBranchList[i].Statement;
         var obj := TObject(edit);
         var template := GetTemplateByControl(edit, obj);
         tmpList.Text := ReplaceStr(template, BRANCH_PLACEHOLDER + '1', BRANCH_PLACEHOLDER + i.ToString);
         caseLines.AddStrings(tmpList);
         tmpList.Clear;
         for var a := 0 to caseLines.Count-1 do
         begin
            if caseLines[a].Contains(PRIMARY_PLACEHOLDER) then
            begin
               caseLines[a] := ReplaceStr(caseLines[a], PRIMARY_PLACEHOLDER, Trim(edit.Text));
               caseLines.Objects[a] := obj;
            end;
            if caseLines[a].Contains('%s2') then
               caseLines[a] := ReplaceStr(caseLines[a], '%s2', statement);
         end;
      end;

      lines.Text := ReplaceStr(caseOfTemplate, PRIMARY_PLACEHOLDER, statement);
      TInfra.InsertTemplateLines(lines, '%s2', caseLines);

      var defTemplate := IfThen(DefaultBranch.Count > 0, GInfra.GetLangDefinition(ALangId).CaseOfDefaultValueTemplate);
      TInfra.InsertTemplateLines(lines, '%s3', defTemplate);

      GenerateTemplateSection(tmpList, lines, ALangId, ADeep);
      TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
      result := tmpList.Count;
   finally
      lines.Free;
      tmpList.Free;
      caseLines.Free;
   end;
end;

function  TCaseBlock.GetTemplateByControl(AControl: TControl; var AObject: TObject): string;
begin
   case GetBranchIndexByControl(AControl) of
      DEFAULT_BRANCH_IDX+1:
      begin
         result := GInfra.CurrentLang.CaseOfFirstValueTemplate;
         if result.IsEmpty then
            result := GInfra.CurrentLang.CaseOfValueTemplate
         else
            AObject := Self;
      end;
      BRANCH_IDX_NOT_FOUND: result := '';
   else
      result := GInfra.CurrentLang.CaseOfValueTemplate;
   end;
end;

procedure TCaseBlock.UpdateEditor(AEdit: TCustomEdit);
begin
   if AEdit = FStatement then
   begin
      if GInfra.CurrentLang.CaseOfFirstValueTemplate.IsEmpty then
         inherited UpdateEditor(AEdit)
      else
         EditorAction(nil);
   end
   else if (AEdit <> nil) and ShouldFocusEditor then
   begin
      var obj := TObject(AEdit);
      var chLine := TInfra.GetChangeLine(obj, AEdit, GetTemplateByControl(AEdit, obj));
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, Trim(AEdit.Text));
         chLine.Text := ReplaceStr(chLine.Text, '%s2', Trim(FStatement.Text));
         TInfra.GetEditorForm.UpdateEditorForBlock(Self, chLine);
      end;
   end;
end;

function TCaseBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := (NewHeight >= Constraints.MinHeight) and (NewWidth >= Constraints.MinWidth);
   if result and FVResize then
   begin
      if Expanded then
      begin
         for var i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
            Inc(FBranchList[i].Hook.Y, NewHeight-Height);
      end
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 28;
      end;
   end;
   if result and FHResize and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

function TCaseBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
begin

   var exp1 := False;
   var exp2 := False;

   if TInfra.IsNOkColor(FStatement.Font.Color) then
      exp1 := True;

   result := AParentNode.Owner.AddChildObject(AParentNode, GetTreeNodeText, FStatement);

   for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
   begin
      var br := FBranchList[i];
      if TInfra.IsNOkColor(br.Statement.Font.Color) then
         exp2 := True;
      var newNode := TTreeNodeWithFriend(AParentNode.Owner.AddChildObject(result, GetTreeNodeText(i), br.Statement));
      newNode.Offset := i;
      for var block in br do
         block.GenerateTree(newNode);
   end;

   var newNode := TTreeNodeWithFriend(AParentNode.Owner.AddChild(result, i18Manager.GetString('DefValue')));
   newNode.Offset := FBranchList.Count;

   for var block in DefaultBranch do
      block.GenerateTree(newNode);

   if exp1 then
   begin
      AParentNode.MakeVisible;
      AParentNode.Expand(False);
   end;

   if exp2 then
   begin
      result.MakeVisible;
      result.Expand(False);
   end;

end;

function TCaseBlock.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := '';
   if ANodeOffset = 0 then
      result := inherited GetTreeNodeText(ANodeOffset)
   else if ANodeOffset < FBranchList.Count then
   begin
      var bStatement := FBranchList[ANodeOffset].Statement;
      result := bStatement.Text + ': ' + GetErrorMsg(bStatement);
   end;
end;

function TCaseBlock.GetDescTemplate(const ALangId: string): string;
begin
   result := '';
   var lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.CaseOfDescTemplate;
end;

procedure TCaseBlock.ExpandFold(AResize: boolean);
begin
   inherited ExpandFold(AResize);
   for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.Visible := Expanded;
end;

function TCaseBlock.CountErrWarn: TErrWarnCount;
begin
   result := inherited CountErrWarn;
   for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
   begin
      if FBranchList[i].Statement.GetFocusColor = NOK_COLOR then
         Inc(result.ErrorCount);
   end;
end;

function TCaseBlock.GetDiamondTop: TPoint;
begin
   result := Point(DefaultBranch.Hook.X, 0);
end;

procedure TCaseBlock.AfterRemovingBranch;
begin
   for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.Change;
   inherited;
end;

procedure TCaseBlock.ChangeColor(AColor: TColor);
begin
   inherited ChangeColor(AColor);
   for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.Color := AColor;
end;

function TCaseBlock.GetFromXML(ANode: IXMLNode): TError;
begin
   result := inherited GetFromXML(ANode);
   if ANode <> nil then
   begin
      var branchNodes := FilterNodes(ANode, BRANCH_TAG);
      branchNodes.NextNode;          // skip default branch stored in first tag
      var branchNode := branchNodes.NextNode;
      FRefreshMode := True;
      for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         if branchNode <> nil then
         begin
            var node := FindNode(branchNode, 'value');
            if node <> nil then
               FBranchList[i].Statement.Text := node.Text;
         end;
         branchNode := branchNodes.NextNode;
      end;
      FRefreshMode := False;
      Repaint;
   end;
end;

procedure TCaseBlock.SaveInXML(ANode: IXMLNode);
begin
   inherited SaveInXML(ANode);
   if ANode <> nil then
   begin
      var branchNodes := FilterNodes(ANode, BRANCH_TAG);
      branchNodes.NextNode;          // skip default branch stored in first tag
      var branchNode := branchNodes.NextNode;
      for var i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         if branchNode = nil then
            break;
         SetNodeCData(branchNode, 'value', FBranchList[i].Statement.Text);
         branchNode := branchNodes.NextNode;
      end;
   end;
end;

end.