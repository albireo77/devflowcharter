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
   Controls, Forms, StdCtrls, Graphics, Classes, Menus, SysUtils, Base_Block,
   Statement, OmniXML, CommonInterfaces, CommonTypes, LangDefinition;

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
   ApplicationCommon, StrUtils, XMLProcessor, Main_Block, UserFunction, Return_Block,
   FastcodeAnsiStringReplaceUnit;

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

   edtStartVal := TStatement.Create(Self);
   edtStartVal.Color := GSettings.RoadSignColor;
   edtStartVal.Font.Size := FStatement.Font.Size;
   edtStartVal.DoubleBuffered := true;

   edtStopVal := TStatement.Create(Self);
   edtStopVal.Color := GSettings.RoadSignColor;
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
   cbVariable.Color := GSettings.RoadSignColor;

   edtVariable := TEdit.Create(Self);
   edtVariable.Parent := Self;
   edtVariable.Color := Color;
   edtVariable.ReadOnly := GInfra.CurrentLang.EnabledVars;
   edtVariable.ShowHint := True;
   edtVariable.AutoSelect := False;
   edtVariable.Color := GSettings.RoadSignColor;
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
   lForBlock: TForDoBlock;
begin
   if ABlock is TForDoBlock then
   begin
      lForBlock := TForDoBlock(ABlock);
      edtStartVal.Text := lForBlock.edtStartVal.Text;
      edtStopVal.Text := lForBlock.edtStopVal.Text;
      edtVariable.Text := lForBlock.edtVariable.Text;
      MyOnChange(edtVariable);
      cbVariable.ItemIndex := lForBlock.cbVariable.ItemIndex;
      FOrder := lForBlock.FOrder;
      if not lForBlock.Expanded then
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
   y: integer;
begin
   inherited;
   if Expanded and (cbVariable <> nil) and (edtVariable <> nil) and (edtStartVal <> nil) and (edtStopVal <> nil) then
   begin
      IPoint.X := Branch.Hook.X + 60;
      IPoint.Y := 35;
      cbVariable.Left := Branch.Hook.X-79;
      edtVariable.Left := Branch.Hook.X-75;
      edtStartVal.Left := Branch.Hook.X-30;
      edtStopVal.Left := Branch.Hook.X+11;
      DrawArrowLine(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
      DrawArrowLine(Point(Width-11, 19), Point(Width-11, Height-1));
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
      begin
         DrawArrowLine(Point(5, Height-21), Point(5, 19), arrMiddle);
         Canvas.Polyline([Point(BottomHook, Height-21),
                          Point(5, Height-21),
                          Point(5, 19),
                          Point(Branch.Hook.X-100, 19)]);
      end;
      with Canvas do
      begin
         MoveTo(Branch.Hook.X+74, 19);
         LineTo(Width-11, 19);
         Brush.Style := bsClear;
         if GSettings.RoadSignColor <> GSettings.DesktopColor then
            Brush.Color := GSettings.RoadSignColor;
         Polygon([Point(Branch.Hook.X-100, 0),
                  Point(Branch.Hook.X+35, 0),
                  Point(Branch.Hook.X+74, 19),
                  Point(Branch.Hook.X+35, TopHook.Y),
                  Point(Branch.Hook.X-100, TopHook.Y),
                  Point(Branch.Hook.X-100, 0)]);
         y :=  edtStartVal.BoundsRect.Bottom - 6;
         DrawTextLabel(Branch.Hook.X-42, y, GInfra.CurrentLang.AssignOperator, false, true);
         DrawTextLabel(Branch.Hook.X+1, y, lForDirect[FOrder], false, true);
         DrawTextLabel(Branch.Hook.X-97, y, FForLabel, false, true);
         DrawBlockLabel(Branch.Hook.X-100, 40, GInfra.CurrentLang.LabelFor);
      end;      
   end;
   DrawI;
end;


procedure TForDoBlock.MyOnClick(Sender: TObject);
begin
   GChange := 1;
   edtVariable.Visible := not GInfra.CurrentLang.EnabledVars;
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
   lHeader: TUserFunctionHeader;
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
         lHeader := TInfra.GetFunctionHeader(Self);
         isOk := (lHeader <> nil) and lHeader.LocalVars.IsValidLoopVar(edtVariable.Text);
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
   dir1, dir2, lTemplate: string;
begin
   if ATemplate = '' then
      lTemplate := ALangDef.ForDoTemplate
   else
      lTemplate := ATemplate;
   result := FastCodeAnsiStringReplace(lTemplate, PRIMARY_PLACEHOLDER, edtVariable.Text);
   result := FastCodeAnsiStringReplace(result, '%s2', Trim(edtStartVal.Text));
   result := FastCodeAnsiStringReplace(result, '%s3', Trim(edtStopVal.Text));
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
   result := FastCodeAnsiStringReplace(result, '%s4', dir1);
   result := FastCodeAnsiStringReplace(result, '%s5', dir2);
end;

function TForDoBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lTemplate: string;
   lLangDef: TLangDefinition;
   lTmpList: TStringList;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   lLangDef := GInfra.GetLangDefinition(ALangId);
   if (lLangDef <> nil) and (lLangDef.ForDoTemplate <> '') then
   begin
      lTmpList := TStringList.Create;
      try
         lTemplate := GetTemplate(lLangDef);
         GenerateTemplateSection(lTmpList, lTemplate, ALangId, ADeep);
         TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
         result := lTmpList.Count;
      finally
         lTmpList.Free;
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
begin
   inherited ChangeColor(AColor);
   if GSettings.RoadSignColor = GSettings.DesktopColor then
   begin
      edtStartVal.Color := AColor;
      edtStopVal.Color := AColor;
      cbVariable.Color := AColor;
      edtVariable.Color := AColor;
   end
   else
      begin
      edtStartVal.Color := GSettings.RoadSignColor;
      edtStopVal.Color := GSettings.RoadSignColor;
      cbVariable.Color := GSettings.RoadSignColor;
      edtVariable.Color := GSettings.RoadSignColor;
   end;
end;

procedure TForDoBlock.PopulateComboBoxes;
var
   lHeader: TUserFunctionHeader;
begin
   inherited PopulateComboBoxes;
   if GInfra.CurrentLang.EnabledVars then
   begin
      with cbVariable do
      begin
         Items.Clear;
         if GProject.GlobalVars <> nil then
            GProject.GlobalVars.FillForList(Items);
         lHeader := TInfra.GetFunctionHeader(Self);
         if lHeader <> nil then
            lHeader.LocalVars.FillForList(Items);
         ItemIndex := Items.IndexOf(edtVariable.Text);
      end;
   end;
end;

function TForDoBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   lEdit: TCustomEdit;
   lExpr: string;
   i: integer;
begin
   lEdit := edtStopVal;
   if AInfo.SelText <> '' then
   begin
      lExpr := GInfra.CurrentLang.GetTemplateExpr(Self.ClassType);
      i := AnsiPos(PRIMARY_PLACEHOLDER, lExpr);
      if i <> 0 then
      begin
         i := i + Length(edtVariable.Text);
         if AInfo.SelStart < i then
            lEdit := edtVariable
         else
         begin
            lExpr := FastCodeAnsiStringReplace(lExpr, PRIMARY_PLACEHOLDER, edtVariable.Text);
            i := AnsiPos('%s2', lExpr);
            if i <> 0 then
            begin
               i := i + Length(Trim(edtStartVal.Text));
               if AInfo.SelStart < i then
                  lEdit := edtStartVal;
            end;
         end;
      end;
   end;
   AInfo.FocusEdit := lEdit;
   result := inherited RetrieveFocus(AInfo);
end;

procedure TForDoBlock.PutTextControls;
var
   lTop, lWidth: integer;
begin
   lTop := 22 - edtStartVal.Height div 2;
   if lTop > 8 then
      lWidth := 33
   else
      lWidth := 30;
   edtStartVal.SetBounds(90, lTop, 30, edtStartVal.Height);
   edtStopVal.SetBounds(131, lTop, lWidth, edtStopVal.Height);
   cbVariable.SetBounds(0, lTop-4, 38, cbVariable.Height);
   edtVariable.SetBounds(0, lTop, 33, edtVariable.Height);
end;

procedure TForDoBlock.UpdateEditor(AEdit: TCustomEdit);
var
   lStr1, lStr2: string;
   lLine: TChangeLine;
begin
   if PerformEditorUpdate then
   begin
      lLine := TInfra.GetChangeLine(Self);
      if lLine.Row <> ROW_NOT_FOUND then
      begin
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, PRIMARY_PLACEHOLDER, edtVariable.Text);
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, '%s2', Trim(edtStartVal.Text));
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, '%s3', Trim(edtStopVal.Text));
         if FOrder = ordAsc then
         begin
            lStr1 := GInfra.CurrentLang.ForAsc1;
            lStr2 := GInfra.CurrentLang.ForAsc2;
         end
         else
         begin
            lStr1 := GInfra.CurrentLang.ForDesc1;
            lStr2 := GInfra.CurrentLang.ForDesc2;
         end;
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, '%s4', lStr1);
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, '%s5', lStr2);
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(lLine);
         TInfra.GetEditorForm.SetCaretPos(lLine);
      end;
   end;
end;

function TForDoBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   lTag: IXMLElement;
begin
   inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      lTag := TXMLProcessor.FindChildTag(ATag, 'i_var');
      if lTag <> nil then
      begin
         cbVariable.Text := lTag.Text;
         edtVariable.Text := lTag.Text;
      end;
      FRefreshMode := true;
      lTag := TXMLProcessor.FindChildTag(ATag, 'init_val');
      if lTag <> nil then
         edtStartVal.Text := AnsiReplaceStr(lTag.Text, '#', ' ');
      lTag := TXMLProcessor.FindChildTag(ATag, 'end_val');
      if lTag <> nil then
         edtStopVal.Text := AnsiReplaceStr(lTag.Text, '#' , ' ');
      FRefreshMode := false;
      FOrder := TForOrder(StrToIntDef(ATag.GetAttribute('order'), 0));
   end
end;

procedure TForDoBlock.SaveInXML(const ATag: IXMLElement);
var
   lTag: IXMLElement;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      lTag := ATag.OwnerDocument.CreateElement('i_var');
      TXMLProcessor.AddText(lTag, edtVariable.Text);
      ATag.AppendChild(lTag);
      lTag := ATag.OwnerDocument.CreateElement('init_val');
      TXMLProcessor.AddText(lTag, AnsiReplaceStr(edtStartVal.Text, ' ', '#'));
      ATag.AppendChild(lTag);
      lTag := ATag.OwnerDocument.CreateElement('end_val');
      TXMLProcessor.AddText(lTag, AnsiReplaceStr(edtStopVal.Text, ' ', '#'));
      ATag.AppendChild(lTag);
      ATag.SetAttribute('order', IntToStr(Ord(FOrder)));
   end;
end;

end.