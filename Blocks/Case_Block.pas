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
   Vcl.StdCtrls, Vcl.Graphics, System.Classes, System.SysUtils, Vcl.ComCtrls, System.Types,
   Vcl.Controls, Base_Block, OmniXML, CommonTypes, Statement;

type

   TCaseBlock = class(TGroupBlock)
      protected
         FCaseLabel: string;
         DefaultBranch: TBranch;
         procedure Paint; override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure OnStatementChange(AStatement: TStatement);
         function GetDiamondTop: TPoint; override;
         procedure PlaceBranchStatement(ABranch: TBranch);
         function  GetTemplateByControl(AControl: TControl; var AObject: TObject): string;
         procedure AfterRemovingBranch; override;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         procedure ResizeHorz(AContinue: boolean); override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ExpandFold(AResize: boolean); override;
         function AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchStmntId: integer = ID_INVALID): TBranch; override;
         function InsertNewBranch(AIndex: integer): TBranch;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(ATag: IXMLElement): TError; override;
         procedure SaveInXML(ATag: IXMLElement); override;
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
   System.StrUtils, System.UITypes, System.Math, XMLProcessor, Return_Block, Navigator_Form,
   LangDefinition, ApplicationCommon;

constructor TCaseBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms);

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
   FStatement.Alignment := taCenter;
   FStatement.OnChangeExtend := OnStatementChange;

end;

procedure TCaseBlock.CloneFrom(ABlock: TBlock);
var
   i: integer;
   br, br2: TBranch;
   caseBlock: TCaseBlock;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TCaseBlock then
   begin
      caseBlock := TCaseBlock(ABlock);
      for i := DEFAULT_BRANCH_IDX+1 to caseBlock.FBranchList.Count-1 do
      begin
         br2 := GetBranch(i);
         if br2 <> nil then
         begin
            br := caseBlock.FBranchList[i];
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
var
   pnt, dBottom, dRight: TPoint;
   i, x, y: integer;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := DefaultBranch.Hook.X - 40;
      dBottom := FDiamond[D_BOTTOM];
      dRight := FDiamond[D_RIGHT];
      TopHook.Y := dBottom.Y + 10;
      BottomPoint.Y := Height - 31;
      DrawArrow(BottomPoint, BottomPoint.X, Height-1);
      for i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
      begin
         pnt := FBranchList[i].Hook;
         DrawArrow(pnt.X, TopHook.Y, pnt);
         PlaceBranchStatement(FBranchList[i]);
      end;
      x := dBottom.X + (dRight.X - dBottom.X) div 2;
      y := dBottom.Y - (dBottom.Y - dRight.Y) div 2 + 3;
      DrawTextLabel(x, y, FCaseLabel);
      DrawBlockLabel(dRight.X+5, 1, GInfra.CurrentLang.LabelCase, false, true);
      Canvas.MoveTo(pnt.X, TopHook.Y);
      Canvas.LineTo(DefaultBranch.Hook.X, TopHook.Y);
      Canvas.LineTo(DefaultBranch.Hook.X, TopHook.Y-10);
      Canvas.MoveTo(BottomHook, BottomPoint.Y);
      Canvas.LineTo(BottomPoint.X, BottomPoint.Y);
   end;
   DrawI;
end;

procedure TCaseBlock.OnStatementChange(AStatement: TStatement);
var
   i: integer;
   br: TBranch;
begin
   if GSettings.ParseCase then
   begin
      for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         br := FBranchList[i];
         if br.Statement <> AStatement then
            br.Statement.Change;
      end;
   end;
end;

function TCaseBlock.IsDuplicatedCase(AEdit: TCustomEdit): boolean;
var
   i: integer;
   edit: TCustomEdit;
begin
   result := false;
   if (AEdit <> nil) and (AEdit.Parent = Self) then
   begin
      for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      begin
         edit := FBranchList[i].Statement;
         if (edit <> AEdit) and (Trim(edit.Text) = Trim(AEdit.Text)) then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

function TCaseBlock.AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchStmntId: integer = ID_INVALID): TBranch;
begin
   result := inherited AddBranch(AHook, ABranchId);
   if FBranchList.IndexOf(result) > DEFAULT_BRANCH_IDX then       // don't execute when default branch is being added in constructor
   begin
      result.Statement := TStatement.Create(Self, ABranchStmntId);
      result.Statement.Alignment := taRightJustify;
      PlaceBranchStatement(result);
   end;
end;

function TCaseBlock.InsertNewBranch(AIndex: integer): TBranch;
var
   lock: boolean;
   pnt: TPoint;
begin
   result := nil;
   if AIndex < 0 then
      AIndex := FBranchList.Count;
   if AIndex > DEFAULT_BRANCH_IDX then
   begin
      pnt := Point(FBranchList[AIndex-1].GetMostRight+60, Height-32);
      result := TBranch.Create(Self, pnt);
      FBranchList.Insert(AIndex, result);
      lock := LockDrawing;
      try
         result.Statement := TStatement.Create(Self);
         result.Statement.Alignment := taRightJustify;
         PlaceBranchStatement(result);
         ResizeWithDrawLock;
      finally
         if lock then
            UnLockDrawing;
      end;
   end;
end;

procedure TCaseBlock.PlaceBranchStatement(ABranch: TBranch);
var
   prevBranch: TBranch;
   idx, w: integer;
begin
   idx := FBranchList.IndexOf(ABranch);
   if idx > DEFAULT_BRANCH_IDX then
   begin
      prevBranch := FBranchList[idx-1];
      if prevBranch <> nil then
      begin
         w := Min(ABranch.Hook.X-prevBranch.Hook.X-10, 300);
         ABranch.Statement.SetBounds(ABranch.Hook.X-w-5, TopHook.Y+1, w, ABranch.Statement.Height);
      end;
   end;
end;

procedure TCaseBlock.ResizeHorz(AContinue: boolean);
var
   x, leftX, rightX, i: integer;
   br: TBranch;
   block: TBlock;
begin
   BottomHook := Branch.Hook.X;
   rightX := 100;
   for i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      br := FBranchList[i];
      leftX := rightX;
      br.Hook.X := leftX;
      x := leftX;
      LinkBlocks(i);
      for block in br do
         x := Min(block.Left, x);
      Inc(br.hook.X, leftX-x);
      LinkBlocks(i);
      PlaceBranchStatement(br);
      if br.FindInstanceOf(TReturnBlock) = -1 then
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
var
   maxh, h, i: integer;
   br, hBranch: TBranch;
begin
   maxh := 0;
   hBranch := DefaultBranch;
   for i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      br := FBranchList[i];
      h := br.Height;
      if h > maxh then
      begin
         maxh := h;
         hBranch := br;
      end;
   end;
   hBranch.Hook.Y := 99;
   Height := maxh + 131;
   for i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
   begin
      br := FBranchList[i];
      if br <> hBranch then
         br.Hook.Y := maxh - br.Height + 99;
   end;
   LinkBlocks;
   if AContinue then
      ParentBlock.ResizeVert(AContinue);
end;

function TCaseBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
var
   defTemplate, template, caseOfTemplate, statement: string;
   i, a: integer;
   lines, caseLines, tmpList: TStringList;
   obj: TObject;
   edit: TCustomEdit;
begin

   result := 0;
   if fsStrikeOut in Font.Style then
      Exit;

   caseOfTemplate := GetBlockTemplate(ALangId);
   if not caseOfTemplate.IsEmpty then
   begin
      statement := Trim(FStatement.Text);
      caseLines := TStringList.Create;
      tmpList := TStringList.Create;
      try
         for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
         begin
            tmpList.Clear;
            edit := FBranchList[i].Statement;
            obj := edit;
            template := GetTemplateByControl(edit, obj);
            tmpList.Text := ReplaceStr(template, '%b1', '%b' + i.ToString);
            caseLines.AddStrings(tmpList);
            for a := 0 to caseLines.Count-1 do
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
         tmpList.Clear;
         lines := TStringList.Create;
         try
            lines.Text := ReplaceStr(caseOfTemplate, PRIMARY_PLACEHOLDER, statement);
            TInfra.InsertTemplateLines(lines, '%s2', caseLines);
            defTemplate := IfThen(DefaultBranch.Count > 0, GInfra.GetLangDefinition(ALangId).CaseOfDefaultValueTemplate);
            TInfra.InsertTemplateLines(lines, '%s3', defTemplate);
            GenerateTemplateSection(tmpList, lines, ALangId, ADeep);
         finally
            lines.Free;
         end;
         TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
         result := tmpList.Count;
      finally
         caseLines.Free;
         tmpList.Free;
      end;
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
var
   chLine: TChangeLine;
   obj: TObject;
begin
   if AEdit = FStatement then
   begin
      if GInfra.CurrentLang.CaseOfFirstValueTemplate.IsEmpty then
         inherited UpdateEditor(AEdit)
      else
         OnStatementChange(nil);
   end
   else if (AEdit <> nil) and PerformEditorUpdate then
   begin
      obj := AEdit;
      chLine := TInfra.GetChangeLine(obj, AEdit, GetTemplateByControl(AEdit, obj));
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, Trim(AEdit.Text));
         chLine.Text := ReplaceStr(chLine.Text, '%s2', Trim(FStatement.Text));
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(chLine);
         TInfra.GetEditorForm.SetCaretPos(chLine);
      end;
   end;
end;

procedure TCaseBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
   i: integer;
begin
   Resize := (NewHeight >= Constraints.MinHeight) and (NewWidth >= Constraints.MinWidth);
   if Resize and FVResize then
   begin
      if Expanded then
      begin
         for i := DEFAULT_BRANCH_IDX to FBranchList.Count-1 do
            Inc(FBranchList[i].Hook.Y, NewHeight-Height);
      end
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 28;
      end;
   end;
   if Resize and FHResize and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

function TCaseBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
var
   newNode: TTreeNodeWithFriend;
   br: TBranch;
   exp1, exp2: boolean;
   i: integer;
   block: TBlock;
begin

   exp1 := false;
   exp2 := false;

   if TInfra.IsNOkColor(FStatement.Font.Color) then
      exp1 := true;

   result := AParentNode.Owner.AddChildObject(AParentNode, GetTreeNodeText, FStatement);

   for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
   begin
      br := FBranchList[i];
      if TInfra.IsNOkColor(br.Statement.Font.Color) then
         exp2 := true;
      newNode := TTreeNodeWithFriend(AParentNode.Owner.AddChildObject(result, GetTreeNodeText(i), br.Statement));
      newNode.Offset := i;
      for block in br do
         block.GenerateTree(newNode);
   end;

   newNode := TTreeNodeWithFriend(AParentNode.Owner.AddChild(result, i18Manager.GetString('DefValue')));
   newNode.Offset := FBranchList.Count;

   for block in DefaultBranch do
      block.GenerateTree(newNode);

   if exp1 then
   begin
      AParentNode.MakeVisible;
      AParentNode.Expand(false);
   end;

   if exp2 then
   begin
      result.MakeVisible;
      result.Expand(false);
   end;

end;

function TCaseBlock.GetTreeNodeText(ANodeOffset: integer = 0): string;
var
   bStatement: TStatement;
begin
   result := '';
   if ANodeOffset = 0 then
      result := inherited GetTreeNodeText(ANodeOffset)
   else if ANodeOffset < FBranchList.Count then
   begin
      bStatement := FBranchList[ANodeOffset].Statement;
      result := bStatement.Text + ': ' + GetErrorMsg(bStatement);
   end;
end;

function TCaseBlock.GetDescTemplate(const ALangId: string): string;
var
   lang: TLangDefinition;
begin
   result := '';
   lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.CaseOfDescTemplate;
end;

procedure TCaseBlock.ExpandFold(AResize: boolean);
var
   i: integer;
begin
   inherited ExpandFold(AResize);
   for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.Visible := Expanded;
end;

function TCaseBlock.CountErrWarn: TErrWarnCount;
var
   i: integer;
begin
   result := inherited CountErrWarn;
   for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
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
var
   i: integer;
begin
   for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.DoEnter;
   inherited;
end;

procedure TCaseBlock.ChangeColor(AColor: TColor);
var
   i: integer;
begin
   inherited ChangeColor(AColor);
   for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
      FBranchList[i].Statement.Color := AColor;
end;

function TCaseBlock.GetFromXML(ATag: IXMLElement): TError;
var
   tag, tag2: IXMLElement;
   i: integer;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if tag <> nil then
      begin
         tag := TXMLProcessor.FindNextTag(tag);   // skip default branch stored in first tag
         FRefreshMode := true;
         for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
         begin
            if tag <> nil then
            begin
               tag2 := TXMLProcessor.FindChildTag(tag, 'value');
               if tag2 <> nil then
                  FBranchList[i].Statement.Text := tag2.Text;
            end;
            tag := TXMLProcessor.FindNextTag(tag);
         end;
         FRefreshMode := false;
      end;
      Repaint;
   end;
end;

procedure TCaseBlock.SaveInXML(ATag: IXMLElement);
var
   tag, tag2: IXMLElement;
   i: integer;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if tag <> nil then
      begin
         tag := TXMLProcessor.FindNextTag(tag);   // skip default branch stored in first tag
         for i := DEFAULT_BRANCH_IDX+1 to FBranchList.Count-1 do
         begin
            if tag <> nil then
            begin
               tag2 := ATag.OwnerDocument.CreateElement('value');
               TXMLProcessor.AddCDATA(tag2, FBranchList[i].Statement.Text);
               tag.AppendChild(tag2);
            end;
            tag := TXMLProcessor.FindNextTag(tag);
         end;
      end;
   end;
end;

end.