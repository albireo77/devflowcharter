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
   Controls, Forms, StdCtrls, Graphics, Classes, SysUtils, Base_Block, ComCtrls,
   Types, OmniXML, CommonInterfaces, CommonTypes;

type

   TCaseBlock = class(TGroupBlock)
      protected
         FCaseLabel: string;
         DefaultBranch: TBranch;
         procedure Paint; override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure OnFStatementChange(AEdit: TCustomEdit);
         function GetDiamondPoint: TPoint; override;
         procedure PlaceBranchStatement(const ABranch: TBranch);
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, Alower_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; override;
         procedure ResizeHorz(const AContinue: boolean); override;
         procedure ResizeVert(const AContinue: boolean); override;
         procedure ExpandFold(const AResize: boolean); override;
         procedure RemoveBranch;
         function AddBranch(const AHook: TPoint; const AResizeInd: boolean; const ABranchId: integer = ID_INVALID; const ABranchStmntId: integer = ID_INVALID): TBranch; override;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(const ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(const ATag: IXMLElement); override;
         procedure RefreshCaseValues;
         procedure ChangeColor(const AColor: TColor); override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function IsDuplicatedCase(AEdit: TCustomEdit): boolean;
         procedure CloneFrom(ABlock: TBlock); override;
   end;

const
   DEFAULT_BRANCH_IND = PRIMARY_BRANCH_IND;

implementation

uses
   StrUtils, XMLProcessor, Return_Block, Navigator_Form, FastcodeAnsiStringReplaceUnit, Messages,
   LangDefinition, Windows, Statement, ApplicationCommon;

constructor TCaseBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, Alower_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
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
   FStatement.Color := GSettings.DiamondColor;
   FStatement.Alignment := taCenter;
   FStatement.OnChangeCallBack := OnFStatementChange;
   PutTextControls;

end;

function TCaseBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TCaseBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, DefaultBranch.Hook.X, DefaultBranch.Hook.Y);
   result.CloneFrom(Self);
end;

procedure TCaseBlock.CloneFrom(ABlock: TBlock);
var
   i: integer;
   lBranch, lBranch2: TBranch;
   lCaseBlock: TCaseBlock;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TCaseBlock then
   begin
      lCaseBlock := TCaseBlock(ABlock);
      for i := DEFAULT_BRANCH_IND+1 to High(lCaseBlock.FBranchArray) do
      begin
         lBranch2 := GetBranch(i);
         if lBranch2 = nil then
            continue;
         lBranch := lCaseBlock.GetBranch(i);
         if (lBranch2.Statement <> nil) and (lBranch.Statement <> nil) then
         begin
            lBranch2.Statement.Text := lBranch.Statement.Text;
            lBranch2.Statement.Visible := lBranch.Statement.Visible;
         end;
      end;
   end;
end;

constructor TCaseBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 200, 131, 100, 100, 99);
end;

procedure TCaseBlock.Paint;
var
   lPoint: TPoint;
   i: integer;
begin
   inherited;
   if Expanded then
   begin
      lPoint := DefaultBranch.Hook;
      IPoint.X := lPoint.X - 40;
      PutTextControls;
      BottomPoint.Y := Height - 31;
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1));
      for i := DEFAULT_BRANCH_IND to High(FBranchArray) do
      begin
         lPoint := FBranchArray[i].Hook;
         DrawArrowLine(Point(lPoint.X, TopHook.Y), lPoint);
      end;
      DrawTextLabel(DefaultBranch.Hook.X+40, 48, FCaseLabel);
      DrawBlockLabel(DefaultBranch.Hook.X+60, 1, GInfra.CurrentLang.LabelCase);
      with Canvas do
      begin
         MoveTo(lPoint.X, TopHook.Y);
         LineTo(DefaultBranch.Hook.X, TopHook.Y);
         LineTo(DefaultBranch.Hook.X, TopHook.Y-10);
         MoveTo(BottomHook, BottomPoint.Y);
         LineTo(BottomPoint.X, BottomPoint.Y);
      end;
   end;
   DrawI;
end;

procedure TCaseBlock.OnFStatementChange(AEdit: TCustomEdit);
var
   i: integer;
   lBranch: TBranch;
begin
   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      lBranch := FBranchArray[i];
      if (lBranch.Statement <> nil) and (lBranch.Statement <> AEdit) then
         lBranch.Statement.Change;
   end;
end;

function TCaseBlock.IsDuplicatedCase(AEdit: TCustomEdit): boolean;
var
   i: integer;
   lStatement: TCustomEdit;
begin
   result := false;
   if (AEdit <> nil) and (AEdit.Parent = Self) then
   begin
      for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
      begin
         lStatement := FBranchArray[i].Statement;
         if (lStatement <> AEdit) and (lStatement <> nil) and (Trim(lStatement.Text) = Trim(AEdit.Text)) then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

function TCaseBlock.AddBranch(const AHook: TPoint; const AResizeInd: boolean; const ABranchId: integer = ID_INVALID; const ABranchStmntId: integer = ID_INVALID): TBranch;
var
   lLocked: boolean;
begin
   result := inherited AddBranch(AHook, AResizeInd, ABranchId);
   if result.Index > DEFAULT_BRANCH_IND then       // don't execute when default branch is being added in constructor
   begin
      lLocked := false;                    // statement edit box must not exist for default (primary) branch
      if AResizeInd then
         lLocked := LockDrawing;
      try
         result.Statement := TStatement.Create(Self, ABranchStmntId);
         result.Statement.Alignment := taRightJustify;
         PlaceBranchStatement(result);
         if AResizeInd then
         begin
            Width := result.Hook.X + 30;
            BottomHook := result.Hook.X;
            ParentBlock.ResizeHorz(true);
         end;
      finally
         if lLocked then
            UnLockDrawing;
      end;
   end;
end;

procedure TCaseBlock.PlaceBranchStatement(const ABranch: TBranch);
var
   lPrevBranch: TBranch;
begin
   if ABranch <> nil then
   begin
      lPrevBranch := ABranch.ParentBlock.GetBranch(ABranch.Index-1);
      if (lPrevBranch <> nil) and (ABranch.Statement <> nil) then
         ABranch.Statement.SetBounds(lPrevBranch.Hook.X+5, 71, ABranch.Hook.X-lPrevBranch.Hook.X-10, ABranch.Statement.Height);
   end;
end;

procedure TCaseBlock.ResizeHorz(const AContinue: boolean);
var
   x, leftX, rightX, i: integer;
   lBranch: TBranch;
   lBlock: TBlock;
begin
   BottomHook := Branch.Hook.X;
   rightX := 100;
   for i := DEFAULT_BRANCH_IND to High(FBranchArray) do
   begin
      leftX := rightX;
      lBranch := FBranchArray[i];
      lBranch.Hook.X := leftX;
      x := leftX;
      LinkBlocks(i);

      lBlock := lBranch.First;
      while lBlock <> nil do
      begin
         if lBlock.Left < x then
            x := lBlock.Left;
         lBlock := lBlock.Next;
      end;

      Inc(lBranch.hook.X, leftX-x);
      LinkBlocks(i);
      PlaceBranchStatement(lBranch);
      if lBranch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         lBlock := lBranch.Last;
         if lBlock <> nil then
            BottomHook := lBlock.Left + lBlock.BottomPoint.X
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

procedure TCaseBlock.ResizeVert(const AContinue: boolean);
var
   lMaxHeight, lHeight, idx, i, lLastBranch: integer;
   lBranch: TBranch;
begin

   lMaxHeight := 0;
   idx := DEFAULT_BRANCH_IND;
   lLastBranch := High(FBranchArray);

   for i := DEFAULT_BRANCH_IND to lLastBranch do
   begin
      lHeight := FBranchArray[i].Height;
      if lHeight > lMaxHeight then
      begin
         lMaxHeight := lHeight;
         idx := i;
      end;
   end;

   for i := DEFAULT_BRANCH_IND to lLastBranch do
   begin
      lBranch := FBranchArray[i];
      if i = idx then
      begin
         lBranch.Hook.Y := 99;
         Height := lMaxHeight + 131;
      end
      else
         lBranch.Hook.Y := lMaxHeight - lBranch.Height + 99;
   end;

   LinkBlocks;

   if AContinue then
      ParentBlock.ResizeVert(AContinue);
      
end;

function TCaseBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lIndent, lIndent2, lLine, lDefTemplate: string;
   i, lBCount, lAddInd, a: integer;
   lLangDef: TLangDefinition;
   lStrListCase: TStringList;
   lTemplateCaseValue, lTmpList, lTmpList1: TStringList;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   lIndent := DupeString(GSettings.IndentString, ADeep);
   lIndent2 := lIndent + GSettings.IndentString;
   lLine := Trim(FStatement.Text);

      if ALangId = TIBASIC_LANG_ID then
      begin
         lBCount := BranchCount;
         lAddInd := 0;
         lTmpList := TStringList.Create;
         try
            if lBCount > 1 then
            begin
               lTmpList.AddObject(lIndent + 'If (' + lLine + ' = ' + Trim(FBranchArray[2].Statement.Text) + ') Then', Self);
               GenerateNestedCode(lTmpList, 2, ADeep+1, ALangId);
               lAddInd := 1;
            end;
            if lBCount > 2 then
            begin
               for i := 3 to High(FBranchArray) do
               begin
                  lTmpList.AddObject(lIndent + 'Else If (' + lLine + ' = ' + Trim(FBranchArray[i].Statement.Text) + ') Then', FBranchArray[i].Statement);
                  GenerateNestedCode(lTmpList, i, ADeep+1, ALangId);
               end;
            end;
            if FBranchArray[DEFAULT_BRANCH_IND].first <> nil then
            begin
               if lBCount = 1 then
                  lTmpList.AddObject(lIndent + 'If (' + lLine + ' = ' + lLine + ') Then', Self)
               else
                  lTmpList.Add(lIndent + 'Else');
               GenerateNestedCode(lTmpList, DEFAULT_BRANCH_IND, ADeep+1, ALangId);
               lAddInd := 1;
            end;
            if lAddInd = 1 then
               lTmpList.AddObject(lIndent + 'EndIf', Self);
            TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
            result := lTmpList.Count;
         finally
            lTmpList.Free;
         end;
      end
      else
      begin
         lLangDef := GInfra.GetLangDefinition(ALangId);
         if (lLangDef <> nil) and (lLangDef.CaseOfTemplate <> '') then
         begin
            lTemplateCaseValue := TStringList.Create;
            lTmpList := TStringList.Create;
            lTmpList1 := TStringList.Create;
            try
               for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
               begin
                  lTmpList.Clear;
                  lTmpList.Text := FastCodeAnsiStringReplace(lLangDef.CaseOfValueTemplate, '%b1', '%b'+IntToStr(i));
                  lTemplateCaseValue.AddStrings(lTmpList);
                  for a := 0 to lTemplateCaseValue.Count-1 do
                  begin
                     if AnsiPos(PRIMARY_PLACEHOLDER, lTemplateCaseValue[a]) <> 0 then
                     begin
                        lTemplateCaseValue[a] := FastCodeAnsiStringReplace(lTemplateCaseValue[a], PRIMARY_PLACEHOLDER, Trim(FBranchArray[i].Statement.Text));
                        lTemplateCaseValue.Objects[a] := FBranchArray[i].Statement;
                        break;
                     end;
                  end;
               end;
               lStrListCase := TStringList.Create;
               try
                  lStrListCase.Text := FastCodeAnsiStringReplace(lLangDef.CaseOfTemplate, PRIMARY_PLACEHOLDER, lLine);
                  TInfra.InsertTemplateLines(lStrListCase, '%s2', lTemplateCaseValue);
                  if FBranchArray[DEFAULT_BRANCH_IND].first <> nil then
                     lDefTemplate := lLangDef.CaseOfDefaultValueTemplate
                  else
                     lDefTemplate := '';
                  TInfra.InsertTemplateLines(lStrListCase, '%s3', lDefTemplate);
                  GenerateTemplateSection(lTmpList1, lStrListCase, ALangId, ADeep);
               finally
                  lStrListCase.Free;
               end;
               TInfra.InsertLinesIntoList(ALines, lTmpList1, AFromLine);
               result := lTmpList1.Count;
            finally
               lTemplateCaseValue.Free;
               lTmpList.Free;
               lTmpList1.Free;
            end;
         end;
      end;
end;

procedure TCaseBlock.UpdateEditor(AEdit: TCustomEdit);
var
   lLine: TChangeLine;
begin
   if AEdit = FStatement then
      inherited UpdateEditor(AEdit)
   else if (AEdit <> nil) and PerformEditorUpdate then
   begin
      lLine := TInfra.GetChangeLine(AEdit, AEdit, GInfra.CurrentLang.CaseOfValueTemplate);
      if lLine.Row <> ROW_NOT_FOUND then
      begin
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, PRIMARY_PLACEHOLDER, AEdit.Text);
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(lLine);
         TInfra.GetEditorForm.SetCaretPos(lLine);
      end;
   end;
end;

procedure TCaseBlock.RemoveBranch;
var
   i, lLastBranch: integer;
   lBranch: TBranch;
begin
   lBranch := GetBranch(Ired);
   if (Ired > DEFAULT_BRANCH_IND) and (lBranch <> nil) then
   begin
       if (GClpbrd.UndoObject is TBlock) and (TBlock(GClpbrd.UndoObject).ParentBranch = lBranch) then
          GClpbrd.UndoObject.Free;
       lBranch.Free;
       lLastBranch := High(FBranchArray);
       for i := Ired to lLastBranch-1 do
          FBranchArray[i] := FBranchArray[i+1];
       SetLength(FBranchArray, lLastBranch);
       ResizeWithDrawLock;
       RefreshCaseValues;
       NavigatorForm.Invalidate;
   end;
end;

procedure TCaseBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
   i: integer;
begin
   Resize := (NewHeight >= Constraints.MinHeight) and (NewWidth >= Constraints.MinWidth);
   if Resize and VResizeInd then
   begin
      if Expanded then
      begin
         for i := DEFAULT_BRANCH_IND to High(FBranchArray) do
            Inc(FBranchArray[i].Hook.Y, NewHeight-Height);
      end
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 30;
      end;
   end;
   if Resize and HResizeInd and not Expanded then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

function TCaseBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lErrMsg: string;
   lNewNode: TTreeNode;
   lBranch: TBranch;
   lExpand1, lExpand2: boolean;
   i: integer;
   lBlock: TBlock;
begin

   lExpand1 := false;
   lExpand2 := false;

   lErrMsg := GetErrorMsg(FStatement);
   if lErrMsg <> '' then
      lExpand1 := true;

   result := AParentNode.Owner.AddChildObject(AParentNode, GetDescription + lErrMsg, FStatement);

   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      lBranch := FBranchArray[i];
      if lBranch.Statement <> nil then
      begin
         lErrMsg := GetErrorMsg(lBranch.Statement);
         if lErrMsg <> '' then
            lExpand2 := true;
         lNewNode := AParentNode.Owner.AddChildObject(result, lBranch.Statement.Text + ': ' + lErrMsg, lBranch.Statement);
      end;
      lBlock := lBranch.First;
      while lBlock <> nil do
      begin
         lBlock.GenerateTree(lNewNode);
         lBlock := lBlock.Next;
      end;
   end;

   lNewNode := AParentNode.Owner.AddChild(result, i18Manager.GetString('DefValue'));

   lBlock := DefaultBranch.First;
   while lBlock <> nil do
   begin
      lBlock.GenerateTree(lNewNode);
      lBlock := lBlock.Next;
   end;

   if lExpand1 then
   begin
      AParentNode.MakeVisible;
      AParentNode.Expand(false);
   end;

   if lExpand2 then
   begin
      result.MakeVisible;
      result.Expand(false);
   end;

end;

procedure TCaseBlock.ExpandFold(const AResize: boolean);
var
   i: integer;
begin
   inherited ExpandFold(AResize);
   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      if FBranchArray[i].Statement <> nil then
         FBranchArray[i].Statement.Visible := Expanded;
   end;
end;

function TCaseBlock.CountErrWarn: TErrWarnCount;
var
   i: integer;
begin
   result := inherited CountErrWarn;
   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      if (FBranchArray[i].Statement <> nil) and (FBranchArray[i].Statement.GetFocusColor = NOK_COLOR) then
         Inc(result.ErrorCount);
   end;
end;

function TCaseBlock.GetDiamondPoint: TPoint;
begin
   result := Point(DefaultBranch.Hook.X, 0);
end;

procedure TCaseBlock.RefreshCaseValues;
var
   i: integer;
begin
   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      if FBranchArray[i].Statement <> nil then
         FBranchArray[i].Statement.DoEnter;
   end;
end;

procedure TCaseBlock.ChangeColor(const AColor: TColor);
var
   i: integer;
begin

   inherited ChangeColor(AColor);

   if GSettings.DiamondColor = GSettings.DesktopColor then
      FStatement.Color := AColor
   else
      FStatement.Color := GSettings.DiamondColor;

   for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
   begin
      if FBranchArray[i].Statement <> nil then
         FBranchArray[i].Statement.Color := AColor;
   end;
   
end;

function TCaseBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   lTag, lTag2: IXMLElement;
   i: integer;
   lStatement: TStatement;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      lTag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if lTag <> nil then
      begin
         lTag := TXMLProcessor.FindNextTag(lTag);   // skip default branch stored in first tag
         FRefreshMode := true;
         for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
         begin
            lStatement := FBranchArray[i].Statement;
            if (lTag <> nil) and (lStatement <> nil) then
            begin
               lTag2 := TXMLProcessor.FindChildTag(lTag, 'value');
               if lTag2 <> nil then
                  lStatement.Text := lTag2.Text;
            end;
            lTag := TXMLProcessor.FindNextTag(lTag);
         end;
         FRefreshMode := false;
      end;
      Repaint;
   end;
end;

procedure TCaseBlock.SaveInXML(const ATag: IXMLElement);
var
   lTag, lTag2: IXMLElement;
   i: integer;
   lStatement: TStatement;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      lTag := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if lTag <> nil then
      begin
         lTag := TXMLProcessor.FindNextTag(lTag);   // skip default branch stored in first tag
         for i := DEFAULT_BRANCH_IND+1 to High(FBranchArray) do
         begin
            lStatement := FBranchArray[i].Statement;
            if (lTag <> nil) and (lStatement <> nil) then
            begin
               lTag2 := ATag.OwnerDocument.CreateElement('value');
               TXMLProcessor.AddCDATA(lTag2, lStatement.Text);
               lTag.AppendChild(lTag2);
            end;
            lTag := TXMLProcessor.FindNextTag(lTag);
         end;
      end;
   end;
end;

end.