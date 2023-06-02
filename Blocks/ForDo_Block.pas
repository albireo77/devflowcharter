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



unit ForDo_Block;

interface

uses
   Vcl.StdCtrls, Vcl.Graphics, System.Classes, Base_Block, Statement, OmniXML,
   Types, LangDefinition;

type

   TForDoBlock = class(TGroupBlock)
      protected
         FDescOrder,
         FIsInitialized: boolean;
         FForLabel: string;
         procedure Paint; override;
         procedure VarOnClick(Sender: TObject);
         procedure VarOnChange(Sender: TObject);
         procedure VarListOnCloseUp(Sender: TObject);
         procedure SetWidth(AMinX: integer); override;
         procedure SetDescOrder(AValue: boolean);
         procedure PutTextControls; override;
         function GetTextTop: integer;
         function FillExpression(const AExpression: string; const ALangId: string): string;
         procedure OnChangeAll;
         procedure EditorAction(AEdit: TCustomEdit);
      public
         edtStart, edtStop: TStatement;
         cbVar: TComboBox;
         edtVar: TEdit;
         property DescOrder: boolean read FDescOrder write SetDescOrder;
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         procedure ExpandFold(AResize: boolean); override;
         function GetTextControl: TCustomEdit; override;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(ANode: IXMLNode): TError; override;
         procedure SaveInXML(ANode: IXMLNode); override;
         procedure ChangeColor(AColor: TColor); override;
         procedure PopulateComboBoxes; override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; override;
         procedure CloneFrom(ABlock: TBlock); override;
         function FillTemplate(const ALangId: string; const ATemplate: string = ''): string; override;
         function FillCodedTemplate(const ALangId: string): string; override;
         function GetDescTemplate(const ALangId: string): string; override;
         procedure ResizeVert(AContinue: boolean); override;
         procedure ResizeHorz(AContinue: boolean); override;
   end;

implementation

uses
   Vcl.Controls, Vcl.Forms, System.SysUtils, System.StrUtils, System.Math, YaccLib,
   Infrastructure, Constants, Main_Block, UserFunction, Return_Block, OmniXMLUtils;

const
   DEFAULT_WIDTH = 246;
   DEFAULT_HEIGHT = 91;
   DEFAULT_BOTTOM_HOOK = DEFAULT_WIDTH div 2;
   RIGHT_MARGIN = 11;

constructor TForDoBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin

   inherited Create(ABranch, ABlockParms, shpRoadSign, taLeftJustify, yymFor);

   FInitParms.Width := DEFAULT_WIDTH;
   FInitParms.Height := DEFAULT_HEIGHT;
   FInitParms.BottomHook := DEFAULT_BOTTOM_HOOK;
   FInitParms.BranchPoint.X := DEFAULT_BOTTOM_HOOK;
   FInitParms.BottomPoint.X := DEFAULT_WIDTH - RIGHT_MARGIN;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 22;

   var sColor := GSettings.GetShapeColor(FShape);
   FForLabel := i18Manager.GetString('CaptionFor');

   edtStart := TStatement.Create(Self, yymFor, taLeftJustify);
   edtStart.Color := sColor;
   edtStart.EditorAction := EditorAction;

   edtStop := TStatement.Create(Self, yymFor, taLeftJustify);
   edtStop.Color := sColor;
   edtStop.EditorAction := EditorAction;

   cbVar := TComboBox.Create(Self);
   cbVar.Parent := Self;
   cbVar.StyleElements := cbVar.StyleElements - [seClient];
   cbVar.Visible := False;
   cbVar.Font.Color := GSettings.FontColor;
   cbVar.Ctl3D := False;
   cbVar.BevelInner := bvRaised;
   cbVar.BevelKind := bkSoft;
   cbVar.BevelOuter := bvNone;
   cbVar.OnCloseUp := VarListOnCloseUp;
   cbVar.Style := csOwnerDrawFixed;
   cbVar.Color := sColor;

   edtVar := TEdit.Create(Self);
   edtVar.Parent := Self;
   edtVar.StyleElements := edtVar.StyleElements - [seClient];
   edtVar.ReadOnly := GInfra.CurrentLang.ForDoVarList;
   edtVar.ShowHint := True;
   edtVar.AutoSelect := False;
   edtVar.DoubleBuffered := True;
   edtVar.Color := sColor;
   
   PopulateComboBoxes;

   edtVar.BorderStyle := bsNone;
   edtVar.BevelInner := bvNone;
   edtVar.BevelOuter := bvNone;
   edtVar.OnClick := VarOnClick;
   edtVar.OnChange := VarOnChange;

   OnChangeAll;

   BottomPoint := Point(Width-RIGHT_MARGIN, 20);
   TopHook := Point(ABlockParms.br.X, 39);
   BottomHook := ABlockParms.bh;
   Constraints.MinWidth := DEFAULT_WIDTH;
   Constraints.MinHeight := DEFAULT_HEIGHT;
   FStatement.Free;
   FStatement := nil;
   FIsInitialized := True;
end;

procedure TForDoBlock.CloneFrom(ABlock: TBlock);
begin
   if ABlock is TForDoBlock then
   begin
      var forBlock := TForDoBlock(ABlock);
      edtStart.Text := forBlock.edtStart.Text;
      edtStop.Text := forBlock.edtStop.Text;
      edtVar.Text := forBlock.edtVar.Text;
      MyOnChange(edtVar);
      cbVar.ItemIndex := forBlock.cbVar.ItemIndex;
      FDescOrder := forBlock.FDescOrder;
      if not forBlock.Expanded then
      begin
         edtStart.Visible := False;
         edtStop.Visible := False;
         edtVar.Visible := False;
      end;
   end;
   inherited CloneFrom(ABlock);
   PutTextControls;
end;

constructor TForDoBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blFor, 0, 0, DEFAULT_WIDTH, DEFAULT_HEIGHT, DEFAULT_BOTTOM_HOOK, 69, DEFAULT_BOTTOM_HOOK));
end;

procedure TForDoBlock.EditorAction(AEdit: TCustomEdit);
begin
   var w := TInfra.GetAutoWidth(AEdit, 30);
   if w <> AEdit.Width then
   begin
      AEdit.Width := w;
      PutTextControls;
      FInitParms.Width := edtStop.Left + edtStop.Width + 82;
      FInitParms.BottomPoint.X := FInitParms.Width - RIGHT_MARGIN;
   end;
   UpdateEditor(AEdit);
end;

procedure TForDoBlock.SetDescOrder(AValue: boolean);
begin
   if AValue <> FDescOrder then
   begin
      FDescOrder := AValue;
      Repaint;
      if ShouldUpdateEditor then
         UpdateEditor(nil);
   end;
end;

function TForDoBlock.GetTextTop: integer;
begin
   result := 16 + edtStart.Height div 2;
end;

procedure TForDoBlock.PutTextControls;
begin
   var t := GetTextTop;
   var r := DrawTextLabel(Branch.Hook.X-97, t, FForLabel, False, True, False).Right;
   cbVar.SetBounds(r+1, 34-t, edtVar.Width+5, cbVar.Height);
   edtVar.SetBounds(cbVar.Left+4, 38-t, edtVar.Width, edtVar.Height);
   r := DrawTextLabel(edtVar.Left + edtVar.Width + 3, t, GInfra.CurrentLang.ForDoVarString, False, True, False).Right;
   edtStart.SetBounds(r+4, 38-t, edtStart.Width, edtStart.Height);
   r := DrawTextLabel(edtStart.Left + edtStart.Width + 3, t, IfThen(FDescOrder, '«', '»'), False, True, False).Right;
   edtStop.SetBounds(r+4, 38-t, edtStop.Width, edtStop.Height);
   Invalidate;
end;

procedure TForDoBlock.Paint;
begin
   inherited;
   if Expanded and FIsInitialized then
   begin
      var bhx := Branch.Hook.X;
      var t := GetTextTop;
      var bst := edtStop.BoundsRect.Right + 6;
      IPoint := Point(bst + 16, 35);
      DrawArrow(Point(bhx, TopHook.Y), Branch.Hook);
      DrawArrow(Width-11, 19, Width-RIGHT_MARGIN, Height-1);
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         DrawArrow(5, Height-21, 5, 19, arrMiddle);
         Canvas.Polyline([Point(BottomHook, Height-21),
                          Point(5, Height-21),
                          Point(5, 19),
                          Point(bhx-100, 19)]);
      end;
      Canvas.MoveTo(bst+30, 19);
      Canvas.LineTo(Width-RIGHT_MARGIN, 19);
      SetBrushColor(FShape);
      Canvas.Polygon([Point(bhx-100, 0),
                      Point(bst-9, 0),
                      Point(bst+30, 19),
                      Point(bst-9, TopHook.Y),
                      Point(bhx-100, TopHook.Y),
                      Point(bhx-100, 0)]);
      DrawTextLabel(edtVar.BoundsRect.Right+3, t, GInfra.CurrentLang.ForDoVarString, False, True);
      DrawTextLabel(edtStart.BoundsRect.Right+3, t, IfThen(FDescOrder, '«', '»'), False, True);
      DrawTextLabel(bhx-97, t, FForLabel, False, True);
      DrawBlockLabel(bhx-100, 40, GInfra.CurrentLang.LabelFor);
   end;
   DrawI;
end;

procedure TForDoBlock.VarOnClick(Sender: TObject);
begin
   edtVar.Visible := not GInfra.CurrentLang.ForDoVarList;
   cbVar.Visible := not edtVar.Visible;
end;

procedure TForDoBlock.VarListOnCloseUp(Sender: TObject);
begin
   cbVar.Hide;
   edtVar.Show;
   if cbVar.Text = edtVar.Text then
      Exit;
   GProject.SetChanged;
   edtVar.Text := cbVar.Text;
   if (edtVar.Text <> '') or not GSettings.ParseFor then
   begin
      edtVar.Hint := i18Manager.GetFormattedString('ExpOk', [edtVar.Text, sLineBreak]);
      edtVar.Font.Color := GSettings.FontColor;
   end
   else
   begin
      edtVar.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
      edtVar.Font.Color := NOK_COLOR;
   end;
   if ShouldUpdateEditor then
      UpdateEditor(nil);
end;

procedure TForDoBlock.VarOnChange(Sender: TObject);
begin
   GProject.SetChanged;
   edtVar.Font.Color := GSettings.FontColor;
   edtVar.Hint := i18Manager.GetFormattedString('ExpOk', [edtVar.Text, sLineBreak]);
   if not GInfra.CurrentLang.ForDoVarList then
      UpdateEditor(edtVar);
   if GSettings.ParseFor then
   begin
      var isVarOk := True;
      if (GProject.GlobalVars = nil) or not GProject.GlobalVars.IsValidLoopVar(edtVar.Text) then
      begin
         var header := TInfra.GetFunctionHeader(Self);
         isVarOk := (header <> nil) and header.LocalVars.IsValidLoopVar(edtVar.Text);
      end;
      if not isVarOk then
      begin
         edtVar.Font.Color := NOK_COLOR;
         if edtVar.Text <> '' then
            edtVar.Hint := i18Manager.GetFormattedString('BadCVar', [edtVar.Text, sLineBreak])
         else
            edtVar.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
      end;
   end;
   var w := TInfra.GetAutoWidth(edtVar, IfThen(GInfra.CurrentLang.ForDoVarList, 28, 5));
   if w <> edtVar.Width then
   begin
      edtVar.Width := w;
      PutTextControls;
   end;
end;

function TForDoBlock.FillCodedTemplate(const ALangId: string): string;
begin
   result := '';
   var i := 0;
   var varVal := Trim(edtVar.Text);
   var startVal := Trim(edtStart.Text);
   var stopVal := Trim(edtStop.Text);
   if ALangId = PYTHON_LANG_ID then
   begin
      result := 'for ' + varVal + ' ' + GInfra.GetLangDefinition(ALangId).ForDoVarString + ' ';
      if stopVal.IsEmpty then
      begin
         if TryStrToInt(startVal, i) then
            result := result + 'range(' + startVal + '):'
         else
            result := result + startVal + ':';
      end
      else
      begin
         if startVal.IsEmpty then
            result := result + 'range(' + stopVal + '):'
         else
            result := result + 'range(' + startVal + ', ' + stopVal + '):'
      end;
   end
   else if ALangId = JAVA_LANG_ID then
   begin
      if stopVal.IsEmpty then
         result := 'for (' + varVal + ' : ' + startVal + ') {'
      else
      begin
         var varName := varVal;
         i := LastDelimiter(' ', varName);
         if i > 0 then
            varName := Copy(varName, i+1);
         result := Format('for (%s = %s; %s %s %s; %s%s) {', [varVal, startVal, varName, IfThen(FDescOrder, '>=', '<='), stopVal, varName, IfThen(FDescOrder, '--', '++')]);
      end;
   end;
end;

function TForDoBlock.FillTemplate(const ALangId: string; const ATemplate: string = ''): string;
begin
   result := FillExpression(FindTemplate(ALangId, ATemplate), ALangId);
end;

function TForDoBlock.GetDescTemplate(const ALangId: string): string;
begin
   result := '';
   var lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.ForDoDescTemplate;
end;

function TForDoBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      Exit;
   var indent := GSettings.IndentString(ADeep);
   var tmpList := TStringList.Create;
   try
      if (ALangId = PYTHON_LANG_ID) or (ALangId = JAVA_LANG_ID) then   // for Java and Python it's impossible to create suitable for..do XML template so hardcoded template must be used
      begin
         var line := indent + FillCodedTemplate(ALangId);
         tmpList.AddObject(line, Self);
         GenerateNestedCode(tmpList, PRIMARY_BRANCH_IDX, ADeep+1, ALangId);
         if ALangId = JAVA_LANG_ID then
            tmpList.AddObject(indent + '}', Self);
      end
      else
      begin
         var template := FillTemplate(ALangId, GetBlockTemplate(ALangId));
         if not template.IsEmpty then
            GenerateTemplateSection(tmpList, template, ALangId, ADeep);
      end;
      TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
      result := tmpList.Count;
   finally
      tmpList.Free;
   end;
end;

procedure TForDoBlock.SetWidth(AMinX: integer);
begin
   Width := IfThen(AMinX < FInitParms.Width-30, FInitParms.Width, AMinX+30);
   BottomPoint.X := Width - RIGHT_MARGIN;
end;

procedure TForDoBlock.ExpandFold(AResize: boolean);
begin
   edtStart.Visible := not Expanded;
   edtStop.Visible := not Expanded;
   cbVar.Visible := False;
   edtVar.Visible := not Expanded;
   inherited ExpandFold(AResize);
end;

function TForDoBlock.GetTextControl: TCustomEdit;
begin
   if (edtStart <> nil) and (edtStart.GetFocusColor = NOK_COLOR) then
      result := edtStart
   else if (edtVar <> nil) and (edtVar.Font.Color = NOK_COLOR) then
      result := edtVar
   else
      result := edtStop;
end;

function TForDoBlock.CountErrWarn: TErrWarnCount;
begin
   result := inherited CountErrWarn;
   var tc := GetTextControl;
   if (tc <> edtVar) and (edtVar.Font.Color = NOK_COLOR) then
      Inc(result.ErrorCount);
   if (tc <> edtStart) and (edtStart.GetFocusColor = NOK_COLOR) then
      Inc(result.ErrorCount);
   if (tc <> edtStop) and (edtStop.GetFocusColor = NOK_COLOR) then
      Inc(result.ErrorCount);
end;

procedure TForDoBlock.ChangeColor(AColor: TColor);
begin
   inherited ChangeColor(AColor);
   var lColor := GSettings.GetShapeColor(FShape);
   if lColor = GSettings.DesktopColor then
   begin
      edtStart.Color := AColor;
      edtStop.Color := AColor;
      cbVar.Color := AColor;
      edtVar.Color := AColor;
   end
   else
   begin
      edtStart.Color := lColor;
      edtStop.Color := lColor;
      cbVar.Color := lColor;
      edtVar.Color := lColor;
   end;
end;

procedure TForDoBlock.PopulateComboBoxes;
begin
   inherited PopulateComboBoxes;
   if GInfra.CurrentLang.ForDoVarList then
   begin
      cbVar.Items.Clear;
      if GProject.GlobalVars <> nil then
         GProject.GlobalVars.FillForList(cbVar.Items);
      var header := TInfra.GetFunctionHeader(Self);
      if header <> nil then
         header.LocalVars.FillForList(cbVar.Items);
      cbVar.ItemIndex := cbVar.Items.IndexOf(edtVar.Text);
   end;
end;

function TForDoBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   var edit := TCustomEdit(edtStop);
   if not AInfo.SelText.IsEmpty then
   begin
      var expr := GetBlockTemplateExpr(GInfra.CurrentLang.Name);
      var i := Pos(PRIMARY_PLACEHOLDER, expr);
      if i <> 0 then
      begin
         i := i + Length(edtVar.Text);
         if AInfo.SelStart < i then
            edit := edtVar
         else
         begin
            expr := ReplaceStr(expr, PRIMARY_PLACEHOLDER, edtVar.Text);
            i := Pos('%s2', expr);
            if i <> 0 then
            begin
               i := i + Length(Trim(edtStart.Text));
               if AInfo.SelStart < i then
                  edit := edtStart;
            end;
         end;
      end;
   end;
   AInfo.FocusEdit := edit;
   result := inherited RetrieveFocus(AInfo);
end;

procedure TForDoBlock.ResizeVert(AContinue: boolean);
begin
   inherited ResizeVert(AContinue);
   PutTextControls;
end;

procedure TForDoBlock.ResizeHorz(AContinue: boolean);
begin
   inherited ResizeHorz(AContinue);
   PutTextControls;
end;

procedure TForDoBlock.UpdateEditor(AEdit: TCustomEdit);
begin
   if ShouldFocusEditor then
   begin
      var chLine := TInfra.GetChangeLine(Self);
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         var langName := GInfra.CurrentLang.Name;
         if GetBlockTemplate(langName).IsEmpty then
            chLine.Text := TInfra.ExtractIndentString(chLine.Text) + FillCodedTemplate(langName)
         else
            chLine.Text := FillExpression(chLine.Text, langName);
         TInfra.GetEditorForm.UpdateEditorForBlock(Self, chLine);
      end;
   end;
end;

function TForDoBlock.FillExpression(const AExpression: string; const ALangId: string): string;
begin
   var lang := GInfra.GetLangDefinition(ALangId);
   if not AExpression.IsEmpty then
   begin
      result := ReplaceStr(AExpression, PRIMARY_PLACEHOLDER, Trim(edtVar.Text));
      result := ReplaceStr(result, '%s2', Trim(edtStart.Text));
      result := ReplaceStr(result, '%s3', Trim(edtStop.Text));
      result := ReplaceStr(result, '%s4', IfThen(FDescOrder, lang.ForDoDesc1, lang.ForDoAsc1));
      result := ReplaceStr(result, '%s5', IfThen(FDescOrder, lang.ForDoDesc2, lang.ForDoAsc2));
   end
   else
      result := FillCodedTemplate(lang.Name);
end;

function TForDoBlock.GetFromXML(ANode: IXMLNode): TError;
begin
   inherited GetFromXML(ANode);
   if ANode <> nil then
   begin
      var node := FindNode(ANode, 'i_var');
      if node <> nil then
      begin
         cbVar.Text := node.Text;
         edtVar.Text := node.Text;
      end;
      FRefreshMode := True;
      node := FindNode(ANode, 'init_val');
      if node <> nil then
         edtStart.Text := ReplaceStr(node.Text, '#', ' ');
      node := FindNode(ANode, 'end_val');
      if node <> nil then
         edtStop.Text := ReplaceStr(node.Text, '#' , ' ');
      FRefreshMode := False;
      FDescOrder := GetNodeAttrStr(ANode, 'order') = 'ordDesc';
   end;
   OnChangeAll;
end;

procedure TForDoBlock.SaveInXML(ANode: IXMLNode);
begin
   inherited SaveInXML(ANode);
   if ANode <> nil then
   begin
      SetNodeText(ANode, 'i_var', edtVar.Text);
      SetNodeText(ANode, 'init_val', ReplaceStr(edtStart.Text, ' ', '#'));
      SetNodeText(ANode, 'end_val', ReplaceStr(edtStop.Text, ' ', '#'));
      SetNodeAttrStr(ANode, 'order', IfThen(FDescOrder, 'ordDesc', 'ordAsc'));
   end;
end;

procedure TForDoBlock.OnChangeAll;
begin
   edtStart.EditorAction(edtStart);
   edtStop.EditorAction(edtStop);
   edtVar.OnChange(edtVar);
end;

end.
