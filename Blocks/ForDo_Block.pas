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

  TForOrder = (ordAsc, ordDesc);

   TForDoBlock = class(TGroupBlock)
      protected
         FOrder: TForOrder;
         FForLabel: string;
         procedure Paint; override;
         procedure MyOnClick(Sender: TObject);
         procedure MyOnCloseUp(Sender: TObject);
         procedure SetWidth(const AMinX: integer); override;
         procedure MyOnChange(Sender: TObject);
         procedure SetForOrder(const AValue: TForOrder);
         procedure PutTextControls; override;
         function GetTemplate(ALangDef: TLangDefinition; const ATemplate: string = ''): string;
      public
         cbVariable: TComboBox;
         edtStartVal, edtStopVal: TStatement;
         edtVariable: TEdit;
         property Order: TForOrder read FOrder write SetForOrder;
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, px1, p1Y: integer; const AId: integer = ID_INVALID); overload;
         function Clone(const ABranch: TBranch): TBlock; override;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         procedure ExpandFold(const AResize: boolean); override;
         function GetDescription: string; override;
         function GetTextControl: TCustomEdit; override;
         function CountErrWarn: TErrWarnCount; override;
         function GetFromXML(const ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(const ATag: IXMLElement); override;
         procedure ChangeColor(const AColor: TColor); override;
         procedure PopulateComboBoxes; override;
         procedure UpdateEditor(AEdit: TCustomEdit); override;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; override;
         procedure CloneFrom(ABlock: TBlock); override;
   end;

implementation

uses
   Vcl.Controls, Vcl.Forms, System.SysUtils, System.StrUtils, System.Types, System.UITypes,
   ApplicationCommon, XMLProcessor, Main_Block, UserFunction, Return_Block;

constructor TForDoBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight, b_hook, px1, p1Y: integer; const AId: integer = ID_INVALID);
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
   cbVariable.Color := Color;
   cbVariable.Font.Color := GSettings.FontColor;
   cbVariable.Font.Size := FStatement.Font.Size;
   cbVariable.Font.Name := GSettings.FlowchartFontName;
   cbVariable.Ctl3D := False;
   cbVariable.BevelInner := bvRaised;
   cbVariable.BevelKind := bkSoft;
   cbVariable.BevelOuter := bvNone;
   cbVariable.OnCloseUp := MyOnCloseUp;
   cbVariable.Style := csDropDownList;
   cbVariable.Color := edtStartVal.Color;

   edtVariable := TEdit.Create(Self);
   edtVariable.Parent := Self;
   edtVariable.Color := Color;
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
      edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [CRLF]);
   end
   else
   begin
      edtVariable.Font.Color := GSettings.FontColor;
      edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', ['', CRLF]);
   end;

   edtVariable.BorderStyle := bsNone;
   edtVariable.BevelInner := bvNone;
   edtVariable.BevelOuter := bvNone;
   edtVariable.OnClick := MyOnClick;
   edtVariable.OnChange := MyOnChange;

   BottomPoint := Point(Width-11, 20);
   TopHook := Point(pX1, 39);
   BottomHook := b_hook;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FOrder := ordAsc;
   FForLabel := i18Manager.GetString('CaptionFor');
   FStatement.Free;
   FStatement := nil;
end;

function TForDoBlock.Clone(const ABranch: TBranch): TBlock;
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
      FOrder := forBlock.FOrder;
      if not forBlock.Expanded then
      begin
         edtStartVal.Visible := false;
         edtStopVal.Visible := false;
         edtVariable.Visible := false;
      end;
   end;
   inherited CloneFrom(ABlock);
end;

constructor TForDoBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 240, 91, 120, 120, 69);
end;

procedure TForDoBlock.SetForOrder(const AValue: TForOrder);
begin
   if AValue <> FOrder then
   begin
      FOrder := AValue;
      Repaint;
      if GSettings.UpdateEditor and not SkipUpdateEditor then
         UpdateEditor(nil);
   end;
end;

procedure TForDoBlock.Paint;
const
   lForDirect: array[TForOrder] of char = ('»', '«');
var
   y, bhx: integer;
   lColor: TColor;
begin
   inherited;
   if Expanded and (cbVariable <> nil) and (edtVariable <> nil) and (edtStartVal <> nil) and (edtStopVal <> nil) then
   begin
      bhx := Branch.Hook.X;
      IPoint.X := bhx + 60;
      IPoint.Y := 35;
      cbVariable.Left := bhx - 79;
      edtVariable.Left := bhx - 75;
      edtStartVal.Left := bhx - 30;
      edtStopVal.Left := bhx + 11;
      DrawArrowLine(Point(bhx, TopHook.Y), Branch.Hook);
      DrawArrowLine(Point(Width-11, 19), Point(Width-11, Height-1));
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         DrawArrowLine(Point(5, Height-21), Point(5, 19), arrMiddle);
         Canvas.Polyline([Point(BottomHook, Height-21),
                          Point(5, Height-21),
                          Point(5, 19),
                          Point(bhx-100, 19)]);
      end;
      with Canvas do
      begin
         MoveTo(bhx+74, 19);
         LineTo(Width-11, 19);
         Brush.Style := bsClear;
         lColor := GSettings.GetShapeColor(FShape);
         if lColor <> GSettings.DesktopColor then
            Brush.Color := lColor;
         Polygon([Point(bhx-100, 0),
                  Point(bhx+35, 0),
                  Point(bhx+74, 19),
                  Point(bhx+35, TopHook.Y),
                  Point(bhx-100, TopHook.Y),
                  Point(bhx-100, 0)]);
         y :=  edtStartVal.BoundsRect.Bottom - 6;
         DrawTextLabel(bhx-42, y, GInfra.CurrentLang.ForDoVarString, false, true);
         DrawTextLabel(bhx+1, y, lForDirect[FOrder], false, true);
         DrawTextLabel(bhx-97, y, FForLabel, false, true);
         DrawBlockLabel(bhx-100, 40, GInfra.CurrentLang.LabelFor);
      end;      
   end;
   DrawI;
end;


procedure TForDoBlock.MyOnClick(Sender: TObject);
begin
   GChange := 1;
   edtVariable.Visible := not GInfra.CurrentLang.ForDoVarList;
   cbVariable.Visible := not edtVariable.Visible;
   if not edtVariable.Visible then
      edtVariable.Clear;
end;

procedure TForDoBlock.MyOnCloseUp(Sender: TObject);
begin
   cbVariable.Visible := False;
   if cbVariable.Text <> '' then
      edtVariable.Text := cbVariable.Text;
   edtVariable.Visible := True;
   if (edtVariable.Text <> '') or not GSettings.ParseFor then
   begin
      edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', [edtVariable.Text, CRLF]);
      edtVariable.Font.Color := GSettings.FontColor;
   end
   else
   begin
      edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [CRLF]);
      edtVariable.Font.Color := NOK_COLOR;
   end;
   if GSettings.UpdateEditor and not SkipUpdateEditor then
      UpdateEditor(nil);
end;

function TForDoBlock.GetDescription:string;
begin
   result := GetTemplate(GInfra.CurrentLang, GInfra.CurrentLang.GetTemplateExpr(ClassType));
end;

procedure TForDoBlock.MyOnChange(Sender: TObject);
var
   header: TUserFunctionHeader;
   isOk: boolean;
begin
   edtVariable.Font.Color := GSettings.FontColor;
   edtVariable.Hint := i18Manager.GetFormattedString('ExpOk', [edtVariable.Text, CRLF]);
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
            edtVariable.Hint := i18Manager.GetFormattedString('BadCVar', [edtVariable.Text, CRLF])
         else
            edtVariable.Hint := i18Manager.GetFormattedString('NoCVar', [CRLF]);
      end;
      edtStartVal.Change;
      edtStopVal.Change;
   end;
end;

function TForDoBlock.GetTemplate(ALangDef: TLangDefinition; const ATemplate: string = ''): string;
var
   dir1, dir2, template: string;
begin
   if ATemplate = '' then
      template := ALangDef.ForDoTemplate
   else
      template := ATemplate;
   result := ReplaceStr(template, PRIMARY_PLACEHOLDER, edtVariable.Text);
   result := ReplaceStr(result, '%s2', Trim(edtStartVal.Text));
   result := ReplaceStr(result, '%s3', Trim(edtStopVal.Text));
   if FOrder = ordAsc then
   begin
      dir1 := ALangDef.ForAsc1;
      dir2 := ALangDef.ForAsc2;
   end
   else
   begin
      dir1 := ALangDef.ForDesc1;
      dir2 := ALangDef.ForDesc2;
   end;
   result := ReplaceStr(result, '%s4', dir1);
   result := ReplaceStr(result, '%s5', dir2);
end;

function TForDoBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   template, startVal, stopVal, varVal, line, indnt: string;
   langDef: TLangDefinition;
   tmpList: TStringList;
   i: integer;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   if ALangId = PYTHON_LANG_ID then
   begin
      indnt := DupeString(GSettings.IndentString, ADeep);
      varVal := Trim(edtVariable.Text);
      startVal := Trim(edtStartVal.Text);
      stopVal := Trim(edtStopVal.Text);
      tmpList := TStringList.Create;
      try
         line := indnt + 'for ' + varVal + ' ' + GInfra.CurrentLang.ForDoVarString + ' ';
         if stopVal.IsEmpty then
         begin
            if TryStrToInt(startVal, i) then
               line := line + 'range(' + startVal + '):'
            else
               line := line + startVal + ':';
         end
         else
         begin
            if startVal.IsEmpty then
               line := line + 'range(' + stopVal + '):'
            else
               line := line + 'range(' + startVal + ', ' + stopVal + '):'
         end;
         tmpList.AddObject(line, Self);
         GenerateNestedCode(tmpList, PRIMARY_BRANCH_IND, ADeep+1, ALangId);
         TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
         result := tmpList.Count;
      finally
        tmpList.Free;
      end;
   end
   else
   begin
      langDef := GInfra.GetLangDefinition(ALangId);
      if (langDef <> nil) and (langDef.ForDoTemplate <> '') then
      begin
         tmpList := TStringList.Create;
         try
            template := GetTemplate(langDef);
            GenerateTemplateSection(tmpList, template, ALangId, ADeep);
            TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
            result := tmpList.Count;
         finally
            tmpList.Free;
         end;
      end;
   end;
end;

procedure TForDoBlock.SetWidth(const AMinX: integer);
begin
   if AMinX < FInitParms.Width - 30 then
      Width := FInitParms.Width
   else
      Width := AMinX + 30;
   BottomPoint.X := Width - 11;
end;

procedure TForDoBlock.ExpandFold(const AResize: boolean);
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

procedure TForDoBlock.ChangeColor(const AColor: TColor);
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
   if AInfo.SelText <> '' then
   begin
      expr := GInfra.CurrentLang.GetTemplateExpr(Self.ClassType);
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
   if t > 8 then
      w := 33
   else
      w := 30;
   edtStartVal.SetBounds(90, t, 30, edtStartVal.Height);
   edtStopVal.SetBounds(131, t, w, edtStopVal.Height);
   cbVariable.SetBounds(0, t-4, 38, cbVariable.Height);
   edtVariable.SetBounds(0, t, 33, edtVariable.Height);
end;

procedure TForDoBlock.UpdateEditor(AEdit: TCustomEdit);
var
   str1, str2: string;
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
            if FOrder = ordAsc then
            begin
               str1 := GInfra.CurrentLang.ForAsc1;
               str2 := GInfra.CurrentLang.ForAsc2;
            end
            else
            begin
               str1 := GInfra.CurrentLang.ForDesc1;
               str2 := GInfra.CurrentLang.ForDesc2;
            end;
            chLine.Text := ReplaceStr(chLine.Text, '%s4', str1);
            chLine.Text := ReplaceStr(chLine.Text, '%s5', str2);
            if GSettings.UpdateEditor and not SkipUpdateEditor then
               TInfra.ChangeLine(chLine);
            TInfra.GetEditorForm.SetCaretPos(chLine);
         end;
      end
      else
         TInfra.UpdateCodeEditor(Self);
   end;
end;

function TForDoBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
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
      FOrder := TForOrder(StrToIntDef(ATag.GetAttribute('order'), 0));
   end
end;

procedure TForDoBlock.SaveInXML(const ATag: IXMLElement);
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
      ATag.SetAttribute('order', IntToStr(Ord(FOrder)));
   end;
end;

end.