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
   Vcl.Controls, Base_Block, OmniXML, CommonInterfaces, CommonTypes;

type

   TCaseBlock = class(TGroupBlock)
      protected
         FCaseLabel: string;
         DefaultBranch: TBranch;
         procedure Paint; override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure OnFStatementChange(AEdit: TCustomEdit);
         function GetDiamondTop: TPoint; override;
         procedure PlaceBranchStatement(ABranch: TBranch);
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, Alower_hook, p1X, p1Y: integer; AId: integer = ID_INVALID); overload;
         function Clone(ABranch: TBranch): TBlock; override;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         procedure ResizeHorz(AContinue: boolean); override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ExpandFold(AResize: boolean); override;
         procedure RemoveBranch(AIndex: integer);
         function AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchStmntId: integer = ID_INVALID): TBranch; override;
         function InsertNewBranch(AIndex: integer): TBranch;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(ATag: IXMLElement); override;
         procedure RefreshCaseValues;
         procedure ChangeColor(AColor: TColor); override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function IsDuplicatedCase(AEdit: TCustomEdit): boolean;
         procedure CloneFrom(ABlock: TBlock); override;
         function GetDescTemplate(const ALangId: string): string; override;
         function GetBranchIndexByControl(AControl: TControl): integer;
   end;

const
   DEFAULT_BRANCH_IND = PRIMARY_BRANCH_IND;

implementation

uses
   System.StrUtils, System.UITypes, System.Math, XMLProcessor, Return_Block, Navigator_Form,
   LangDefinition, Statement, ApplicationCommon;

constructor TCaseBlock.Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, Alower_hook, p1X, p1Y: integer; AId: integer = ID_INVALID);
begin

   FType := blCase;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

   FInitParms.Width := 200;
   FInitParms.Height := 131;
   FInitParms.BottomHook := 100;
   FInitParms.BranchPoint.X := 100;
   FInitParms.BottomPoint.X := 100;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 32;

   DefaultBranch := Branch;

   BottomPoint.X := p1X;
   BottomPoint.Y := Height-31;
   TopHook.Y := 70;
   BottomHook := Alower_hook;
   TopHook.X := p1X;
   IPoint.Y := 50;
   FCaseLabel := i18Manager.GetString('CaptionCase');
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Alignment := taCenter;
   FStatement.OnChangeCallBack := OnFStatementChange;
   PutTextControls;

end;

function TCaseBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := TCaseBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, DefaultBranch.Hook.X, DefaultBranch.Hook.Y);
   result.CloneFrom(Self);
end;

procedure TCaseBlock.CloneFrom(ABlock: TBlock);
var
   i: integer;
   lBranch, lBranch2: TBranch;
   caseBlock: TCaseBlock;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TCaseBlock then
   begin
      caseBlock := TCaseBlock(ABlock);
      for i := DEFAULT_BRANCH_IND+1 to caseBlock.FBranchList.Count-1 do
      begin
         lBranch2 := GetBranch(i);
         if lBranch2 = nil then
            continue;
         lBranch := caseBlock.FBranchList[i];
         if (lBranch2.Statement <> nil) and (lBranch.Statement <> nil) then
         begin
            lBranch2.Statement.Text := lBranch.Statement.Text;
            lBranch2.Statement.Visible := lBranch.Statement.Visible;
         end;
      end;
   end;
end;

constructor TCaseBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 200, 131, 100, 100, 99);
end;

procedure TCaseBlock.Paint;
var
   pnt, dBottom, dRight: PPoint;
   i, x, y: integer;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := DefaultBranch.Hook.X - 40;
      dBottom := @FDiamond[D_BOTTOM];
      dRight := @FDiamond[D_RIGHT];
      TopHook.Y := dBottom.Y + 10;
      BottomPoint.Y := Height - 31;
      DrawArrow(BottomPoint, BottomPoint.X, Height-1);
      for i := DEFAULT_BRANCH_IND to FBranchList.Count-1 do
      begin
         pnt := @FBranchList[i].Hook;
         DrawArrow(pnt.X, TopHook.Y, pnt^);
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

procedure TCaseBlock.OnFStatementChange(AEdit: TCustomEdit);
var
   i: integer;
   lBranch: TBranch;
begin
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if (lBranch.Statement <> nil) and (lBranch.Statement <> AEdit) then
         lBranch.Statement.Change;
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
      for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
      begin
         edit := FBranchList[i].Statement;
         if (edit <> AEdit) and (edit <> nil) and (Trim(edit.Text) = Trim(AEdit.Text)) then
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
   if FBranchList.IndexOf(result) > DEFAULT_BRANCH_IND then       // don't execute when default branch is being added in constructor
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
   if AIndex > DEFAULT_BRANCH_IND then
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
   if idx > DEFAULT_BRANCH_IND then
   begin
      prevBranch := FBranchList[idx-1];
      if (prevBranch <> nil) and (ABranch.Statement <> nil) then
      begin
         w := Min(ABranch.Hook.X-prevBranch.Hook.X-10, 300);
         ABranch.Statement.SetBounds(ABranch.Hook.X-w-5, TopHook.Y+1, w, ABranch.Statement.Height);
      end;
   end;
end;

procedure TCaseBlock.ResizeHorz(AContinue: boolean);
var
   x, leftX, rightX, i: integer;
   lBranch: TBranch;
   block: TBlock;
begin
   BottomHook := Branch.Hook.X;
   rightX := 100;
   for i := DEFAULT_BRANCH_IND to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      leftX := rightX;
      lBranch.Hook.X := leftX;
      x := leftX;
      LinkBlocks(i);
      for block in lBranch do
          x := Min(block.Left, x);
      Inc(lBranch.hook.X, leftX-x);
      LinkBlocks(i);
      PlaceBranchStatement(lBranch);
      if lBranch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         if lBranch.Count > 0 then
            BottomHook := lBranch.Last.Left + lBranch.Last.BottomPoint.X
         else
            BottomHook := lBranch.Hook.X;
      end;
      rightX := lBranch.GetMostRight + 60;
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
   lBranch, hBranch: TBranch;
begin
   maxh := 0;
   hBranch := DefaultBranch;
   for i := DEFAULT_BRANCH_IND to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      h := lBranch.Height;
      if h > maxh then
      begin
         maxh := h;
         hBranch := lBranch;
      end;
   end;
   hBranch.Hook.Y := 99;
   Height := maxh + 131;
   for i := DEFAULT_BRANCH_IND to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if lBranch <> hBranch then
         lBranch.Hook.Y := maxh - lBranch.Height + 99;
   end;
   LinkBlocks;
   if AContinue then
      ParentBlock.ResizeVert(AContinue);
end;

function TCaseBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
var
   defTemplate, template, statement: string;
   i, a: integer;
   langDef: TLangDefinition;
   lines, caseLines, tmpList: TStringList;
   obj: TObject;
begin

   result := 0;
   if fsStrikeOut in Font.Style then
      Exit;

   langDef := GInfra.GetLangDefinition(ALangId);
   if (langDef <> nil) and not langDef.CaseOfTemplate.IsEmpty then
   begin
      statement := Trim(FStatement.Text);
      caseLines := TStringList.Create;
      tmpList := TStringList.Create;
      try
         for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
         begin
            tmpList.Clear;
            template := '';
            obj := FBranchList[i].Statement;
            if i = DEFAULT_BRANCH_IND+1 then
            begin
               template := langDef.CaseOfFirstValueTemplate;
               if template.IsEmpty then
                  template := langDef.CaseOfValueTemplate
               else
                  obj := Self;
            end
            else
               template := langDef.CaseOfValueTemplate;
            tmpList.Text := ReplaceStr(template, '%b1', '%b' + i.ToString);
            caseLines.AddStrings(tmpList);
            for a := 0 to caseLines.Count-1 do
            begin
               if caseLines[a].Contains(PRIMARY_PLACEHOLDER) then
               begin
                  caseLines[a] := ReplaceStr(caseLines[a], PRIMARY_PLACEHOLDER, Trim(FBranchList[i].Statement.Text));
                  caseLines[a] := ReplaceStr(caseLines[a], '%s2', statement);
                  caseLines.Objects[a] := obj;
                  break;
               end;
            end;
         end;
         tmpList.Clear;
         lines := TStringList.Create;
         try
            lines.Text := ReplaceStr(langDef.CaseOfTemplate, PRIMARY_PLACEHOLDER, statement);
            TInfra.InsertTemplateLines(lines, '%s2', caseLines);
            defTemplate := IfThen(DefaultBranch.Count > 0, langDef.CaseOfDefaultValueTemplate);
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

function TCaseBlock.GetBranchIndexByControl(AControl: TControl): integer;
var
   i: integer;
begin
   result := -1;
   if FBranchList = nil then
      Exit;
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      if FBranchList[i].Statement = AControl then
      begin
         result := i;
         break;
      end;
   end;
end;

procedure TCaseBlock.UpdateEditor(AEdit: TCustomEdit);
var
   chLine: TChangeLine;
   template: string;
   i: integer;
   obj: TObject;
begin
   if AEdit = FStatement then
   begin
      if GInfra.CurrentLang.CaseOfFirstValueTemplate.IsEmpty then
         inherited UpdateEditor(AEdit)
      else
      begin
         for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
            FBranchList[i].Statement.Change;
      end;
   end
   else if (AEdit <> nil) and PerformEditorUpdate then
   begin
      template := '';
      obj := AEdit;
      if GetBranchIndexByControl(AEdit) = DEFAULT_BRANCH_IND+1 then
      begin
         template := GInfra.CurrentLang.CaseOfFirstValueTemplate;
         if template.IsEmpty then
            template := GInfra.CurrentLang.CaseOfValueTemplate
         else
            obj := Self;
      end
      else
         template := GInfra.CurrentLang.CaseOfValueTemplate;
      chLine := TInfra.GetChangeLine(obj, AEdit, template);
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

procedure TCaseBlock.RemoveBranch(AIndex: integer);
var
   lBranch: TBranch;
begin
   lBranch := GetBranch(AIndex);
   if lBranch <> nil then
   begin
      if (GClpbrd.UndoObject is TBlock) and (TBlock(GClpbrd.UndoObject).ParentBranch = lBranch) then
         GClpbrd.UndoObject.Free;
      if FBranchList.Remove(lBranch) <> -1 then
      begin
         ResizeWithDrawLock;
         RefreshCaseValues;
         NavigatorForm.Invalidate;
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
         for i := DEFAULT_BRANCH_IND to FBranchList.Count-1 do
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
   errMsg, descTemplate: string;
   newNode: TTreeNode;
   lBranch: TBranch;
   exp1, exp2: boolean;
   i: integer;
   block: TBlock;
begin

   exp1 := false;
   exp2 := false;

   errMsg := GetErrorMsg(FStatement);
   if not errMsg.IsEmpty then
      exp1 := true;

   descTemplate := GetDescTemplate(GInfra.CurrentLang.Name);
   result := AParentNode.Owner.AddChildObject(AParentNode, FillTemplate(GInfra.CurrentLang.Name, descTemplate) + errMsg, FStatement);

   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if lBranch.Statement <> nil then
      begin
         errMsg := GetErrorMsg(lBranch.Statement);
         if not errMsg.IsEmpty then
            exp2 := true;
         newNode := AParentNode.Owner.AddChildObject(result, lBranch.Statement.Text + ': ' + errMsg, lBranch.Statement);
      end;
      for block in lBranch do
          block.GenerateTree(newNode);
   end;

   newNode := AParentNode.Owner.AddChild(result, i18Manager.GetString('DefValue'));

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
   lBranch: TBranch;
begin
   inherited ExpandFold(AResize);
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if lBranch.Statement <> nil then
         lBranch.Statement.Visible := Expanded;
   end;
end;

function TCaseBlock.CountErrWarn: TErrWarnCount;
var
   i: integer;
   lBranch: TBranch;
begin
   result := inherited CountErrWarn;
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if (lBranch.Statement <> nil) and (lBranch.Statement.GetFocusColor = NOK_COLOR) then
         Inc(result.ErrorCount);
   end;
end;

function TCaseBlock.GetDiamondTop: TPoint;
begin
   result := Point(DefaultBranch.Hook.X, 0);
end;

procedure TCaseBlock.RefreshCaseValues;
var
   i: integer;
   lBranch: TBranch;
begin
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if lBranch.Statement <> nil then
         lBranch.Statement.DoEnter;
   end;
end;

procedure TCaseBlock.ChangeColor(AColor: TColor);
var
   i: integer;
   lBranch: TBranch;
begin
   inherited ChangeColor(AColor);
   for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
   begin
      lBranch := FBranchList[i];
      if lBranch.Statement <> nil then
         lBranch.Statement.Color := AColor;
   end;
end;

function TCaseBlock.GetFromXML(ATag: IXMLElement): TErrorType;
var
   tag, tag2: IXMLElement;
   i: integer;
   stmnt: TStatement;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if tag <> nil then
      begin
         tag := TXMLProcessor.FindNextTag(tag);   // skip default branch stored in first tag
         FRefreshMode := true;
         for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
         begin
            stmnt := FBranchList[i].Statement;
            if (tag <> nil) and (stmnt <> nil) then
            begin
               tag2 := TXMLProcessor.FindChildTag(tag, 'value');
               if tag2 <> nil then
                  stmnt.Text := tag2.Text;
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
   stmnt: TStatement;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if tag <> nil then
      begin
         tag := TXMLProcessor.FindNextTag(tag);   // skip default branch stored in first tag
         for i := DEFAULT_BRANCH_IND+1 to FBranchList.Count-1 do
         begin
            stmnt := FBranchList[i].Statement;
            if (tag <> nil) and (stmnt <> nil) then
            begin
               tag2 := ATag.OwnerDocument.CreateElement('value');
               TXMLProcessor.AddCDATA(tag2, stmnt.Text);
               tag.AppendChild(tag2);
            end;
            tag := TXMLProcessor.FindNextTag(tag);
         end;
      end;
   end;
end;

end.