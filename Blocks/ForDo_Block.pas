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
   CommonInterfaces, CommonTypes, LangDefinition;

type

   TForDoBlock = class(TGroupBlock)
      protected
         FDescOrder: boolean;
         FForLabel: string;
         procedure Paint; override;
         procedure VarOnClick(Sender: TObject);
         procedure VarOnChange(Sender: TObject);
         procedure VarListOnCloseUp(Sender: TObject);
         procedure SetWidth(AMinX: integer); override;
         procedure SetDescOrder(AValue: boolean);
         procedure PutTextControls; override;
      public
         cbVariable: TComboBox;
         edtStartVal, edtStopVal: TStatement;
         edtVariable: TEdit;
         property DescOrder: boolean read FDescOrder write SetDescOrder;
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, b_hook, px1, p1Y: integer; AId: integer = ID_INVALID); overload;
         function Clone(ABranch: TBranch): TBlock; override;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         procedure ExpandFold(AResize: boolean); override;
         function GetTextControl: TCustomEdit; override;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(ATag: IXMLElement); override;
         procedure ChangeColor(AColor: TColor); override;
         procedure PopulateComboBoxes; override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; override;
         procedure CloneFrom(ABlock: TBlock); override;
         function FillTemplate(const ALangId: string; const ATemplate: string = ''): string; override;
         function FillCodedTemplate(const ALangId: string): string; override;
         function GetDescTemplate(const ALangId: string): string; override;
   end;

implementation

uses
   Vcl.Controls, Vcl.Forms, System.SysUtils, System.StrUtils, System.Types, System.UITypes,
   System.Math, ApplicationCommon, XMLProcessor, Main_Block, UserFunction, Return_Block;

constructor TForDoBlock.Create(ABranch: TBranch; ALeft, ATop, AWidth, AHeight, b_hook, px1, p1Y: integer; AId: integer = ID_INVALID);
begin

   FType := blFor;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, Point(pX1, p1Y), AId);

   FInitParms.Width := 240;
   FInitParms.Height := 91;
   FInitParms.BottomHook := 120;
   FInitParms.BranchPoint.X := 120;
   FInitParms.BottomPoint.X := 229;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 22;

   FShape := shpRoadSign;

   edtStartVal := TStatement.Create(Self);
   edtStartVal.Color := GSettings.GetShapeColor(FShape);
   edtStartVal.Font.Size := FStatement.Font.Size;
   edtStartVal.DoubleBuffered := true;

   edtStopVal := TStatement.Create(Self);
   edtStopVal.Color := edtStartVal.Color;
   edtStopVal.Font.Size := FStatement.Font.Size;
   edtStopVal.DoubleBuffered := true;

   cbVariable := TComboBox.Create(Self);
   cbVariable.Parent := Self;
   cbVariable.Visible := False;
   cbVariable.Font.Color := GSettings.FontColor;
   cbVariable.Font.Size := FStatement.Font.Size;
   cbVariable.Font.Name := GSettings.FlowchartFontName;
   cbVariable.Ctl3D := False;
   cbVariable.BevelInner := bvRaised;
   cbVariable.BevelKind := bkSoft;
   cbVariable.BevelOuter := bvNone;
   cbVariable.OnCloseUp := VarListOnCloseUp;
   cbVariable.Style := csOwnerDrawFixed;
   cbVariable.Color := edtStartVal.Color;

   edtVariable := TEdit.Create(Self);
   edtVariable.Parent := Self;
   edtVariable.ReadOnly := GInfra.CurrentLang.ForDoVarList;
   edtVariable.ShowHint := True;
   edtVariable.AutoSelect := False;
   edtVariable.Color := edtStartVal.Color;
   edtVariable.Font.Size := FStatement.Font.Size;
   edtVariable.Font.Name := GSettings.FlowchartFontName;
   edtVariable.DoubleBuffered := true;
   
   PopulateComboBoxes;
   PutTextControls;

   if GSettings.ParseFor then
   begin
      edtVariable.Font.Color := NOK_COLOR;
      edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
   end
   else
   begin
      edtVariable.Font.Color := GSettings.FontColor;
      edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', ['', sLineBreak]);
   end;

   edtVariable.BorderStyle := bsNone;
   edtVariable.BevelInner := bvNone;
   edtVariable.BevelOuter := bvNone;
   edtVariable.OnClick := VarOnClick;
   edtVariable.OnChange := VarOnChange;

   BottomPoint := Point(Width-11, 20);
   TopHook := Point(pX1, 39);
   BottomHook := b_hook;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FForLabel := i18Manager.GetString('CaptionFor');
   FStatement.Free;
   FStatement := nil;
end;

function TForDoBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := TForDoBlock.Create(ABranch, Left, Top, Width, Height, BottomHook, Branch.Hook.X, Branch.Hook.Y);
   result.CloneFrom(Self);
end;

procedure TForDoBlock.CloneFrom(ABlock: TBlock);
var
   forBlock: TForDoBlock;
begin
   if ABlock is TForDoBlock then
   begin
      forBlock := TForDoBlock(ABlock);
      edtStartVal.Text := forBlock.edtStartVal.Text;
      edtStopVal.Text := forBlock.edtStopVal.Text;
      edtVariable.Text := forBlock.edtVariable.Text;
      MyOnChange(edtVariable);
      cbVariable.ItemIndex := forBlock.cbVariable.ItemIndex;
      FDescOrder := forBlock.FDescOrder;
      if not forBlock.Expanded then
      begin
         edtStartVal.Visible := false;
         edtStopVal.Visible := false;
         edtVariable.Visible := false;
      end;
   end;
   inherited CloneFrom(ABlock);
end;

constructor TForDoBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 240, 91, 120, 120, 69);
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

procedure TForDoBlock.Paint;
var
   y, bhx: integer;
   lColor: TColor;
begin
   inherited;
   if Expanded then
   begin
      bhx := Branch.Hook.X;
      IPoint.X := bhx + 60;
      IPoint.Y := 35;
      cbVariable.Left := bhx - 81;
      edtVariable.Left := bhx - 77;
      edtStartVal.Left := bhx - 30;
      edtStopVal.Left := bhx + 11;
      DrawArrow(bhx, TopHook.Y, Branch.Hook);
      DrawArrow(Width-11, 19, Width-11, Height-1);
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         DrawArrow(5, Height-21, 5, 19, arrMiddle);
         Canvas.Polyline([Point(BottomHook, Height-21),
                          Point(5, Height-21),
                          Point(5, 19),
                          Point(bhx-100, 19)]);
      end;
      Canvas.MoveTo(bhx+74, 19);
      Canvas.LineTo(Width-11, 19);
      Canvas.Brush.Style := bsClear;
      lColor := GSettings.GetShapeColor(FShape);
      if lColor <> GSettings.DesktopColor then
         Canvas.Brush.Color := lColor;
      Canvas.Polygon([Point(bhx-100, 0),
                      Point(bhx+35, 0),
                      Point(bhx+74, 19),
                      Point(bhx+35, TopHook.Y),
                      Point(bhx-100, TopHook.Y),
                      Point(bhx-100, 0)]);
      y :=  edtStartVal.BoundsRect.Bottom - 6;
      DrawTextLabel(bhx-42, y, GInfra.CurrentLang.ForDoVarString, false, true);
      DrawTextLabel(bhx+1, y, IfThen(FDescOrder, '«', '»'), false, true);
      DrawTextLabel(bhx-97, y, FForLabel, false, true);
      DrawBlockLabel(bhx-100, 40, GInfra.CurrentLang.LabelFor);
   end;
   DrawI;
end;

procedure TForDoBlock.VarOnClick(Sender: TObject);
begin
   edtVariable.Visible := not GInfra.CurrentLang.ForDoVarList;
   cbVariable.Visible := not edtVariable.Visible;
end;

procedure TForDoBlock.VarListOnCloseUp(Sender: TObject);
begin
   cbVariable.Hide;
   edtVariable.Show;
   if cbVariable.Text = edtVariable.Text then
      exit;
   GProject.SetChanged;
   edtVariable.Text := cbVariable.Text;
   if (edtVariable.Text <> '') or not GSettings.ParseFor then
   begin
      edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', [edtVariable.Text, sLineBreak]);
      edtVariable.Font.Color := GSettings.FontColor;
   end
   else
   begin
      edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
      edtVariable.Font.Color := NOK_COLOR;
   end;
   if GSettings.UpdateEditor and not SkipUpdateEditor then
      UpdateEditor(nil);
end;

procedure TForDoBlock.VarOnChange(Sender: TObject);
var
   header: TUserFunctionHeader;
   isOk: boolean;
begin
   GProject.SetChanged;
   edtVariable.Font.Color := GSettings.FontColor;
   edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', [edtVariable.Text, sLineBreak]);
   if not GInfra.CurrentLang.ForDoVarList then
      UpdateEditor(edtVariable);
   if GSettings.ParseFor then
   begin
      if (GProject.GlobalVars <> nil) and GProject.GlobalVars.IsValidLoopVar(edtVariable.Text) then
         isOk := true
      else
      begin
         header := TInfra.GetFunctionHeader(Self);
         isOk := (header <> nil) and header.LocalVars.IsValidLoopVar(edtVariable.Text);
      end;
      if not isOk then
      begin
         edtVariable.Font.Color := NOK_COLOR;
         if edtVariable.Text <> '' then
            edtVariable.Hint := i18Manager.GetFormattedString('BadCVar', [edtVariable.Text, sLineBreak])
         else
            edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [sLineBreak]);
      end;
      edtStartVal.Change;
      edtStopVal.Change;
   end;
end;

function TForDoBlock.FillCodedTemplate(const ALangId: string): string;
var
   varVal, startVal, stopVal, varName: string;
   lang: TLangDefinition;
   i: integer;
begin
   result := '';
   varVal := Trim(edtVariable.Text);
   startVal := Trim(edtStartVal.Text);
   stopVal := Trim(edtStopVal.Text);
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
            varName := Copy(varName, i+1, MaxInt);
         result := Format('for (%s = %s; %s %s %s; %s%s) {', [varVal, startVal, varName, IfThen(FDescOrder, '>=', '<='), stopVal, varName, IfThen(FDescOrder, '--', '++')]);
      end;
   end;
end;

function TForDoBlock.FillTemplate(const ALangId: string; const ATemplate: string = ''): string;
var
   template: string;
   lang: TLangDefinition;
begin
   result := '';
   template := '';
   lang := GInfra.GetLangDefinition(ALangId);
   if ATemplate.IsEmpty then
   begin
      if (lang <> nil) and not lang.ForDoTemplate.IsEmpty then
         template := lang.GetTemplateExpr(TForDoBlock);
   end
   else
      template := ATemplate;
   if (lang <> nil) and not template.IsEmpty then
   begin
      result := ReplaceStr(template, PRIMARY_PLACEHOLDER, Trim(edtVariable.Text));
      result := ReplaceStr(result, '%s2', Trim(edtStartVal.Text));
      result := ReplaceStr(result, '%s3', Trim(edtStopVal.Text));
      result := ReplaceStr(result, '%s4', IfThen(FDescOrder, lang.ForDoDesc1, lang.ForDoAsc1));
      result := ReplaceStr(result, '%s5', IfThen(FDescOrder, lang.ForDoDesc2, lang.ForDoAsc2));
   end
   else
      result := FillCodedTemplate(ALangId);
end;

function TForDoBlock.GetDescTemplate(const ALangId: string): string;
var
   lang: TLangDefinition;
begin
   result := '';
   lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.ForDoDescTemplate;
end;

function TForDoBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
var
   template, line, indent: string;
   tmpList: TStringList;
   lang: TLangDefinition;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   indent := DupeString(GSettings.IndentString, ADeep);
   tmpList := TStringList.Create;
   try
      if (ALangId = PYTHON_LANG_ID) or (ALangId = JAVA_LANG_ID) then   // for Java and Python it's impossible to create suitable for..do XML template so hardcoded template must be used
      begin
         line := indent + FillCodedTemplate(ALangId);
         tmpList.AddObject(line, Self);
         GenerateNestedCode(tmpList, PRIMARY_BRANCH_IND, ADeep+1, ALangId);
         if ALangId = JAVA_LANG_ID then
            tmpList.AddObject(indent + '}', Self);
      end
      else
      begin
         lang := GInfra.GetLangDefinition(ALangId);
         template := FillTemplate(ALangId, lang.ForDoTemplate);
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
   BottomPoint.X := Width - 11;
end;

procedure TForDoBlock.ExpandFold(AResize: boolean);
begin
   edtStartVal.Visible := not Expanded;
   edtStopVal.Visible := not Expanded;
   cbVariable.Visible := false;
   edtVariable.Visible := not Expanded;
   inherited ExpandFold(AResize);
end;

function TForDoBlock.GetTextControl: TCustomEdit;
begin
   if (edtStartVal <> nil) and (edtStartVal.GetFocusColor = NOK_COLOR) then
      result := edtStartVal
   else if (edtVariable <> nil) and (edtVariable.Font.Color = NOK_COLOR) then
      result := edtVariable
   else
      result := edtStopVal;
end;

function TForDoBlock.CountErrWarn: TErrWarnCount;
var
   lTextControl: TCustomEdit;
begin
   result := inherited CountErrWarn;
   lTextControl := GetTextControl;
   if (lTextControl <> edtVariable) and (edtVariable.Font.Color = NOK_COLOR) then
      Inc(result.ErrorCount);
   if (lTextControl <> edtStartVal) and (edtStartVal.GetFocusColor = NOK_COLOR) then
      Inc(result.ErrorCount);
   if (lTextControl <> edtStopVal) and (edtStopVal.GetFocusColor = NOK_COLOR) then
      Inc(result.ErrorCount);
end;

procedure TForDoBlock.ChangeColor(AColor: TColor);
var
   lColor: TColor;
begin
   inherited ChangeColor(AColor);
   lColor := GSettings.GetShapeColor(FShape);
   if lColor = GSettings.DesktopColor then
   begin
      edtStartVal.Color := AColor;
      edtStopVal.Color := AColor;
      cbVariable.Color := AColor;
      edtVariable.Color := AColor;
   end
   else
   begin
      edtStartVal.Color := lColor;
      edtStopVal.Color := lColor;
      cbVariable.Color := lColor;
      edtVariable.Color := lColor;
   end;
end;

procedure TForDoBlock.PopulateComboBoxes;
var
   header: TUserFunctionHeader;
begin
   inherited PopulateComboBoxes;
   if GInfra.CurrentLang.ForDoVarList then
   begin
      with cbVariable do
      begin
         Items.Clear;
         if GProject.GlobalVars <> nil then
            GProject.GlobalVars.FillForList(Items);
         header := TInfra.GetFunctionHeader(Self);
         if header <> nil then
            header.LocalVars.FillForList(Items);
         ItemIndex := Items.IndexOf(edtVariable.Text);
      end;
   end;
end;

function TForDoBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   edit: TCustomEdit;
   expr: string;
   i: integer;
begin
   edit := edtStopVal;
   if not AInfo.SelText.IsEmpty then
   begin
      expr := GInfra.CurrentLang.GetTemplateExpr(TForDoBlock);
      i := Pos(PRIMARY_PLACEHOLDER, expr);
      if i <> 0 then
      begin
         i := i + Length(edtVariable.Text);
         if AInfo.SelStart < i then
            edit := edtVariable
         else
         begin
            expr := ReplaceStr(expr, PRIMARY_PLACEHOLDER, edtVariable.Text);
            i := Pos('%s2', expr);
            if i <> 0 then
            begin
               i := i + Length(Trim(edtStartVal.Text));
               if AInfo.SelStart < i then
                  edit := edtStartVal;
            end;
         end;
      end;
   end;
   AInfo.FocusEdit := edit;
   result := inherited RetrieveFocus(AInfo);
end;

procedure TForDoBlock.PutTextControls;
var
   t, w: integer;
begin
   t := 22 - edtStartVal.Height div 2;
   w := IfThen(t > 8, 33, 30);
   edtStartVal.SetBounds(90, t, 30, edtStartVal.Height);
   edtStopVal.SetBounds(131, t, w, edtStopVal.Height);
   cbVariable.SetBounds(0, t-4, 38, cbVariable.Height);
   edtVariable.SetBounds(0, t, 33, edtVariable.Height);
end;

procedure TForDoBlock.UpdateEditor(AEdit: TCustomEdit);
var
   chLine: TChangeLine;
begin
   if PerformEditorUpdate then
   begin
      if not GInfra.CurrentLang.ForDoTemplate.IsEmpty then
      begin
         chLine := TInfra.GetChangeLine(Self);
         if chLine.Row <> ROW_NOT_FOUND then
         begin
            chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, edtVariable.Text);
            chLine.Text := ReplaceStr(chLine.Text, '%s2', Trim(edtStartVal.Text));
            chLine.Text := ReplaceStr(chLine.Text, '%s3', Trim(edtStopVal.Text));
            chLine.Text := ReplaceStr(chLine.Text, '%s4', IfThen(FDescOrder, GInfra.CurrentLang.ForDoDesc1, GInfra.CurrentLang.ForDoAsc1));
            chLine.Text := ReplaceStr(chLine.Text, '%s5', IfThen(FDescOrder, GInfra.CurrentLang.ForDoDesc2, GInfra.CurrentLang.ForDoAsc2));
            if GSettings.UpdateEditor and not SkipUpdateEditor then
               TInfra.ChangeLine(chLine);
            TInfra.GetEditorForm.SetCaretPos(chLine);
         end;
      end
      else
         TInfra.UpdateCodeEditor(Self);
   end;
end;

function TForDoBlock.GetFromXML(ATag: IXMLElement): TErrorType;
var
   tag: IXMLElement;
begin
   inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, 'i_var');
      if tag <> nil then
      begin
         cbVariable.Text := tag.Text;
         edtVariable.Text := tag.Text;
      end;
      FRefreshMode := true;
      tag := TXMLProcessor.FindChildTag(ATag, 'init_val');
      if tag <> nil then
         edtStartVal.Text := ReplaceStr(tag.Text, '#', ' ');
      tag := TXMLProcessor.FindChildTag(ATag, 'end_val');
      if tag <> nil then
         edtStopVal.Text := ReplaceStr(tag.Text, '#' , ' ');
      FRefreshMode := false;
      FDescOrder := ATag.GetAttribute('order') = 'ordDesc';
   end
end;

procedure TForDoBlock.SaveInXML(ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      tag := ATag.OwnerDocument.CreateElement('i_var');
      TXMLProcessor.AddText(tag, edtVariable.Text);
      ATag.AppendChild(tag);
      tag := ATag.OwnerDocument.CreateElement('init_val');
      TXMLProcessor.AddText(tag, ReplaceStr(edtStartVal.Text, ' ', '#'));
      ATag.AppendChild(tag);
      tag := ATag.OwnerDocument.CreateElement('end_val');
      TXMLProcessor.AddText(tag, ReplaceStr(edtStopVal.Text, ' ', '#'));
      ATag.AppendChild(tag);
      ATag.SetAttribute('order', IfThen(FDescOrder, 'ordDesc', 'ordAsc'));
   end;
end;

end.