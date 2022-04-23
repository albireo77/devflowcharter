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
         function GetFromXML(ATag: IXMLElement): TError; override;
         procedure SaveInXML(ATag: IXMLElement); override;
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
   Infrastructure, Constants, XMLProcessor, Main_Block, UserFunction, Return_Block;

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
   edtVar.ReadOnly := GInfra.CurrentLang.ForDoVarList;
   edtVar.ShowHint := True;
   edtVar.AutoSelect := False;
   edtVar.DoubleBuffered := true;
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
   FIsInitialized := true;
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
         edtStart.Visible := false;
         edtStop.Visible := false;
         edtVar.Visible := false;
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
      if GSettings.UpdateEditor and not SkipUpdateEditor then
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
   var r := DrawTextLabel(Branch.Hook.X-97, t, FForLabel, false, true, false).Right;
   cbVar.SetBounds(r+1, 34-t, edtVar.Width+5, cbVar.Height);
   edtVar.SetBounds(cbVar.Left+4, 38-t, edtVar.Width, edtVar.Height);
   r := DrawTextLabel(edtVar.Left + edtVar.Width + 3, t, GInfra.CurrentLang.ForDoVarString, false, true, false).Right;
   edtStart.SetBounds(r+4, 38-t, edtStart.Width, edtStart.Height);
   r := DrawTextLabel(edtStart.Left + edtStart.Width + 3, t, IfThen(FDescOrder, '«', '»'), false, true, false).Right;
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
      DrawArrow(bhx, TopHook.Y, Branch.Hook);
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
      DrawTextLabel(edtVar.BoundsRect.Right+3, t, GInfra.CurrentLang.ForDoVarString, false, true);
      DrawTextLabel(edtStart.BoundsRect.Right+3, t, IfThen(FDescOrder, '«', '»'), false, true);
      DrawTextLabel(bhx-97, t, FForLabel, false, true);
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
   if GSettings.UpdateEditor and not SkipUpdateEditor then
      UpdateEditor(nil);
end;

procedure TForDoBlock.VarOnChange(Sender: TObject);
var
   header: TUserFunctionHeader;
   isOk: boolean;
   w: integer;
begin
   GProject.SetChanged;
   edtVar.Font.Color := GSettings.FontColor;
   edtVar.Hint := i18Manager.GetFormattedString('ExpOk', [edtVar.Text, sLineBreak]);
   if not GInfra.CurrentLang.ForDoVarList then
      UpdateEditor(edtVar);
   if GSettings.ParseFor then
   begin
      if (GProject.GlobalVars <> nil) and GProject.GlobalVars.IsValidLoopVar(edtVar.Text) then
         isOk := true
      else
      begin
         header := TInfra.GetFunctionHeader(Self);
         isOk := (header <> nil) and header.LocalVars.IsValidLoopVar(edtVar.Text);
      end;
      if not isOk then
      begin
         edtVar.Font.Color := NOK_COLOR;
         if edtVar.Text <> '' then
            edtVar.Hint := i18Manager.GetFormattedString('BadCVar', [edtVar.Text, sLineBreak])
         else
            edtVar.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
      end;
   end;
   w := TInfra.GetAutoWidth(edtVar, IfThen(GInfra.CurrentLang.ForDoVarList, 28, 5));
   if w <> edtVar.Width then
   begin
      edtVar.Width := w;
      PutTextControls;
   end;
end;

function TForDoBlock.FillCodedTemplate(const ALangId: string): string;
var
   varVal, startVal, stopVal, varName: string;
   lang: TLangDefinition;
   i: integer;
begin
   result := '';
   varVal := Trim(edtVar.Text);
   startVal := Trim(edtStart.Text);
   stopVal := Trim(edtStop.Text);
   lang := GInfra.GetLangDefinition(ALangId);
   if ALangId = PYTHON_LANG_ID then
   begin
      result := 'for ' + varVal + ' ' + lang.ForDoVarString + ' ';
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
         varName := varVal;
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
var
   template, line, indent: string;
   tmpList: TStringList;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      Exit;
   indent := GSettings.IndentString(ADeep);
   tmpList := TStringList.Create;
   try
      if (ALangId = PYTHON_LANG_ID) or (ALangId = JAVA_LANG_ID) then   // for Java and Python it's impossible to create suitable for..do XML template so hardcoded template must be used
      begin
         line := indent + FillCodedTemplate(ALangId);
         tmpList.AddObject(line, Self);
         GenerateNestedCode(tmpList, PRIMARY_BRANCH_IDX, ADeep+1, ALangId);
         if ALangId = JAVA_LANG_ID then
            tmpList.AddObject(indent + '}', Self);
      end
      else
      begin
         template := FillTemplate(ALangId, GetBlockTemplate(ALangId));
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
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
   BottomPoint.X := Width - RIGHT_MARGIN;
end;

procedure TForDoBlock.ExpandFold(AResize: boolean);
begin
   edtStart.Visible := not Expanded;
   edtStop.Visible := not Expanded;
   cbVar.Visible := false;
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
      with cbVar do
      begin
         Items.Clear;
         if GProject.GlobalVars <> nil then
            GProject.GlobalVars.FillForList(Items);
         var header := TInfra.GetFunctionHeader(Self);
         if header <> nil then
            header.LocalVars.FillForList(Items);
         ItemIndex := Items.IndexOf(edtVar.Text);
      end;
   end;
end;

function TForDoBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   edit: TCustomEdit;
   expr: string;
   i: integer;
begin
   edit := edtStop;
   if not AInfo.SelText.IsEmpty then
   begin
      expr := GetBlockTemplateExpr(GInfra.CurrentLang.Name);
      i := Pos(PRIMARY_PLACEHOLDER, expr);
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
   var langName := GInfra.CurrentLang.Name;
   if PerformEditorUpdate then
   begin
      var chLine := TInfra.GetChangeLine(Self);
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         if GetBlockTemplate(langName).IsEmpty then
            chLine.Text := TInfra.ExtractIndentString(chLine.Text) + FillCodedTemplate(langName)
         else
            chLine.Text := FillExpression(chLine.Text, langName);
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(chLine);
         TInfra.GetEditorForm.SetCaretPos(chLine);
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

function TForDoBlock.GetFromXML(ATag: IXMLElement): TError;
begin
   inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      var tag := TXMLProcessor.FindChildTag(ATag, 'i_var');
      if tag <> nil then
      begin
         cbVar.Text := tag.Text;
         edtVar.Text := tag.Text;
      end;
      FRefreshMode := true;
      tag := TXMLProcessor.FindChildTag(ATag, 'init_val');
      if tag <> nil then
         edtStart.Text := ReplaceStr(tag.Text, '#', ' ');
      tag := TXMLProcessor.FindChildTag(ATag, 'end_val');
      if tag <> nil then
         edtStop.Text := ReplaceStr(tag.Text, '#' , ' ');
      FRefreshMode := false;
      FDescOrder := ATag.GetAttribute('order') = 'ordDesc';
   end;
   OnChangeAll;
end;

procedure TForDoBlock.SaveInXML(ATag: IXMLElement);
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      var tag := ATag.OwnerDocument.CreateElement('i_var');
      TXMLProcessor.AddText(tag, edtVar.Text);
      ATag.AppendChild(tag);
      tag := ATag.OwnerDocument.CreateElement('init_val');
      TXMLProcessor.AddText(tag, ReplaceStr(edtStart.Text, ' ', '#'));
      ATag.AppendChild(tag);
      tag := ATag.OwnerDocument.CreateElement('end_val');
      TXMLProcessor.AddText(tag, ReplaceStr(edtStop.Text, ' ', '#'));
      ATag.AppendChild(tag);
      ATag.SetAttribute('order', IfThen(FDescOrder, 'ordDesc', 'ordAsc'));
   end;
end;

procedure TForDoBlock.OnChangeAll;
begin
   edtStart.EditorAction(edtStart);
   edtStop.EditorAction(edtStop);
   edtVar.OnChange(edtVar);
end;

end.