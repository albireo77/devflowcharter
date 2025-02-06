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



unit Main_Block;

interface

uses
   WinApi.Windows, Vcl.Graphics, Vcl.ComCtrls, System.Classes, System.Math, Base_Block,
   OmniXML, Interfaces, Types, BlockTabSheet, Comment;

type

   TMainBlock = class(TGroupBlock, IWinControl)
      private
         FPage: TBlockTabSheet;
         procedure DrawStart;
         procedure DrawStop;
         function GetMaxBounds: TPoint;
      public
         constructor Create(APage: TBlockTabSheet; const ABlockParms: TBlockParms); overload;
         constructor Create(APage: TBlockTabSheet; const ATopLeft: TPoint); overload;
         procedure SaveInXML(ANode: IXMLNode); override;
         procedure ExportToGraphic(AGraphic: TGraphic); override;
         procedure SetWidth(AMinX: integer); override;
         procedure SetZOrder(AValue: integer);
         function ExportToXMLFile(const AFile: string): TError; override;
         function GetExportFileName: string; override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         function GetFromXML(ANode: IXMLNode): TError; override;
         function GetHandle: THandle;
         function GetZOrder: integer;
         function IsBoldDesc: boolean; override;
         procedure Resize; override;
         function Remove(ANode: TTreeNodeWithFriend = nil): boolean; override;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
      protected
         FZOrder: integer;
         FStartLabel,
         FStopLabel: string;
         procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
         procedure Paint; override;
         procedure MoveComment(AComment: TComment; dx, dy: integer); override;
         procedure SetPage(APage: TBlockTabSheet); override;
         function GetFunctionLabel(var ARect: TRect): string;
         function GetPage: TBlockTabSheet; override;
         function GetUndoObject: TObject; override;
   end;

const
   MAIN_BLOCK_DEF_WIDTH = 100;
   MAIN_BLOCK_DEF_HEIGHT = 101;

implementation

uses
   Vcl.Forms, System.SysUtils, System.StrUtils, Infrastructure, XMLProcessor, OmniXMLUtils,
   Navigator_Form, UserFunction, Constants;

constructor TMainBlock.Create(APage: TBlockTabSheet; const ABlockParms: TBlockParms);
begin

   FPage := APage;
   inherited Create(nil, ABlockParms, shpEllipse, taLeftJustify);

   FStartLabel := trnsManager.GetString('CaptionStart');
   FStopLabel  := trnsManager.GetString('CaptionStop');

   var w := Max(GetEllipseTextRect(0, 0, FStartLabel).Width, GetEllipseTextRect(0, 0, FStopLabel).Width) + 40;
   var w2 := w div 2;
   if w > Width then
   begin
      Width := w;
      Branch.Hook.X := w2;
      BottomHook := w2;
      TopHook.X := w2;
   end
   else
   begin
      BottomHook := ABlockParms.bh;
      TopHook.X := ABlockParms.br.X;
   end;

   FInitParms.Width := w;
   FInitParms.Height := MAIN_BLOCK_DEF_HEIGHT;
   FInitParms.BottomHook := w2;
   FInitParms.BranchPoint.X := w2;
   FInitParms.BottomPoint.X := -60000;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 42;

   BottomPoint.X := FInitParms.BottomPoint.X;
   TopHook.Y := TInfra.Scaled(Self, 30);
   FZOrder := -1;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   FStatement.Free;
   FStatement := nil;
end;

constructor TMainBlock.Create(APage: TBlockTabSheet; const ATopLeft: TPoint);
begin
   var blockParms := TBlockParms.New(blMain,
                                 ATopLeft.X,
                                 ATopLeft.Y,
                                 MAIN_BLOCK_DEF_WIDTH,
                                 MAIN_BLOCK_DEF_HEIGHT,
                                 MAIN_BLOCK_DEF_WIDTH div 2,
                                 MAIN_BLOCK_DEF_HEIGHT-42,
                                 MAIN_BLOCK_DEF_WIDTH div 2);
   Create(APage, blockParms);
end;

procedure TMainBlock.SetPage(APage: TBlockTabSheet);
begin
   if FPage <> APage then
   begin
      var unPin := Expanded and (PinComments > 0);
      try
         FPage := APage;
         Parent := APage.Box;
         for var comment in GetPinComments do
             comment.Page := APage;
      finally
         if unPin then
            UnPinComments;
      end;
      var header := GProject.FindFunctionHeader(Self);
      if header <> nil then
         header.SetPageBox(APage.Caption);
   end;
end;

function TMainBlock.GetPage: TBlockTabSheet;
begin
   result := FPage;
end;

procedure TMainBlock.SetZOrder(AValue: integer);
begin
   FZOrder := FPage.PageIndex * 100 + AValue;
end;

function TMainBlock.GetZOrder: integer;
begin
   result := FZOrder;
end;

function TMainBlock.GetHandle: THandle;
begin
   result := 0;
   if Visible then
      result := Handle;
end;

function TMainBlock.GetMaxBounds: TPoint;
begin
   result := TPoint.Zero;
   if Visible then
   begin
      result := BoundsRect.BottomRight;
      if Expanded then
      begin
         for var comment in GetComments do
         begin
            if comment.Visible then
            begin
               var p := comment.BoundsRect.BottomRight;
               result.X := Max(result.X, p.X);
               result.Y := Max(result.Y, p.Y);
            end;
         end;
      end;
      result := result + Point(MARGIN_X, MARGIN_Y);
   end;
end;

procedure TMainBlock.ExportToGraphic(AGraphic: TGraphic);
var
   bitmap: TBitmap;
   pnt: TPoint;
   comment: TComment;
   bStyle: TBorderStyle;
   selStart: integer;
   pdi: boolean;
begin
   DeSelect;
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   pnt := GetMaxBounds;
   pnt.X := pnt.X - Left - MARGIN_X + 1;
   pnt.Y := pnt.Y - Top - MARGIN_Y + 1;
   bitmap.Width := pnt.X;
   bitmap.Height := pnt.Y;
   pdi := FPage.DrawI;
   FPage.DrawI := False;
   bitmap.Canvas.Lock;
   try
      PaintTo(bitmap.Canvas, 1, 1);
      if Expanded then
      begin
         for comment in GetComments do
         begin
            if comment.Visible then
            begin
               bStyle := comment.BorderStyle;
               selStart := comment.SelStart;
               comment.BorderStyle := bsNone;
               comment.PaintTo(bitmap.Canvas, comment.Left-Left, comment.Top-Top);
               comment.BorderStyle := bStyle;
               comment.SelStart := selStart;
            end;
         end;
      end;
   finally
      bitmap.Canvas.Unlock;
      FPage.DrawI := pdi;
   end;
   if AGraphic <> bitmap then
   begin
      AGraphic.Assign(bitmap);
      bitmap.Free;
   end;
end;

procedure TMainBlock.Paint;
var
   fontColor: TColor;
   lLabel: string;
   R: TRect;
   fontStyles: TFontStyles;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 30;
      IPoint.Y := 35;
      fontStyles := Canvas.Font.Style;
      Canvas.Font.Style := [];
      Canvas.Brush.Style := bsClear;
      lLabel := GetFunctionLabel(R);
      if not lLabel.IsEmpty then
      begin
         fontColor := Canvas.Font.Color;
         Canvas.Font.Color := clNavy;
         DrawText(Canvas.Handle, PChar(lLabel), -1, R, 0);
         Canvas.Font.Color := fontColor;
      end;
      DrawStart;
      if not Branch.EndsWithReturnBlock then
         DrawStop;
      Canvas.Font.Style := fontStyles;
      DrawArrow(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
   end;
   DrawI;
end;

procedure TMainBlock.DrawStart;
begin
   DrawEllipsedText(Branch.Hook.X, TopHook.Y, FStartLabel);
end;

procedure TMainBlock.DrawStop;
begin
   var y := GetEllipseTextRect(0, 0, FStopLabel).Height;
   if Branch.IsEmpty then
      Inc(y, Branch.Hook.Y+1)
   else
      Inc(y, Branch.Last.BoundsRect.Bottom);
   DrawEllipsedText(BottomHook, y, FStopLabel);
end;

function TMainBlock.GetFunctionLabel(var ARect: TRect): string;
begin
   result := '';
   ARect := Rect(Branch.Hook.X+75, 7, 0, 0);
   if GSettings.ShowFuncLabels and Expanded then
   begin
      var lang := GInfra.CurrentLang;
      var header := GProject.FindFunctionHeader(Self);
      if header <> nil then
      begin
         if not Assigned(lang.GetUserFuncDesc) then
            lang := GInfra.TemplateLang;
         result := lang.GetUserFuncDesc(header, False, header.chkInclDescFlow.Checked);
      end
      else
      begin
         if not Assigned(lang.GetMainProgramDesc) then
            lang := GInfra.TemplateLang;
         result := lang.GetMainProgramDesc;
      end;
   end;
   if not result.IsEmpty then
   begin
      DrawText(Canvas.Handle, PChar(result), -1, ARect, DT_CALCRECT);
      if not Branch.IsEmpty then
      begin
         var r := Branch.First.BoundsRect.Right + 5;
         if (ARect.Bottom > Branch.First.Top-5) and (ARect.Left < r) then
            ARect.Offset(r - ARect.Left, 0);
      end;
   end;
end;

function TMainBlock.GetExportFileName: string;
begin
   var userFunction := GProject.FindUserFunction(Self);
   if userFunction <> nil then
      result := userFunction.GetName
   else
      result := inherited GetExportFileName;
end;

function TMainBlock.ExportToXMLFile(const AFile: string): TError;
begin
   var userFunction := GProject.FindUserFunction(Self);
   if userFunction <> nil then
      result := TXMLProcessor.ExportToXMLFile(userFunction.ExportToXML, AFile)
   else
      result := inherited ExportToXMLFile(AFile);
end;

procedure TMainBlock.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   if Expanded then
      inherited
   else
   begin
      var p := Point(X, Y);
      if IsAtSelectPos(p) then
         Select
      else
         DeSelect;
      SetCursor(p);
   end;
end;

procedure TMainBlock.Resize;
begin
   FPage.Box.SetScrollbars;
   if FPage.Box.HorzScrollBar.Position + BoundsRect.Right < MARGIN_X then
      Left := 0;
   if FPage.Box.VertScrollBar.Position + BoundsRect.Bottom < MARGIN_Y then
      Top := 0;
   BringAllToFront;
   FPage.Box.Invalidate;
end;

function TMainBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin
   var lang := GInfra.GetLangDefinition(ALangId);
   var tmpList := TStringList.Create;
   try
      if ALangId = TIBASIC_LANG_ID then
         GenerateNestedCode(tmpList, PRIMARY_BRANCH_IDX, ADeep+1, ALangId)
      else if lang <> nil then
      begin
         var vars := GProject.GlobalVars;
         var lName := GProject.Name;
         var template := lang.MainFunctionTemplate;
         var header := GProject.FindFunctionHeader(Self);
         if header <> nil then
         begin
            vars := header.LocalVars;
            lName := header.GetName;
            template := GetBlockTemplate(ALangId);
         end;
         if not template.IsEmpty then
         begin
            var progList := TStringList.Create;
            var varList := TStringList.Create;
            try
               progList.Text := ReplaceStr(template, PRIMARY_PLACEHOLDER, lName);
               if header = nil then
                  TInfra.InsertTemplateLines(progList, '%s3', IfThen(not Branch.EndsWithReturnBlock, lang.ProgramReturnTemplate));
               if not Assigned(lang.VarSectionGenerator) then
                  lang := GInfra.TemplateLang;
               lang.VarSectionGenerator(varList, vars);
               TInfra.InsertTemplateLines(progList, '%s2', varList);
               GenerateTemplateSection(tmpList, progList, ALangId, ADeep);
            finally
               varList.Free;
               progList.Free;
            end;
         end;
      end;
      TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
      result := tmpList.Count;
   finally
      tmpList.Free;
   end;
end;

procedure TMainBlock.SetWidth(AMinX: integer);
var
   minVal, val: integer;
   R: TRect;
begin
   if Expanded then
   begin
      minVal := Branch.GetMostRight + 30;
      if not GetFunctionLabel(R).IsEmpty then
         val := R.Right + 10
      else
         val := minVal;
      if val > minVal then
         minVal := val;
      if AMinX < minVal then
         Width := minVal
      else
         Width := AMinX + 5;
   end;
end;

function TMainBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
begin
   result := AParentNode;
   for var block in Branch do
       block.GenerateTree(AParentNode);
end;

procedure TMainBlock.SaveInXML(ANode: IXMLNode);
begin
   inherited SaveInXML(ANode);
   if ANode <> nil then
   begin
      SetNodeAttrInt(ANode, Z_ORDER_ATTR, FZOrder);
      if not FPage.IsMain then
         SetNodeAttrStr(ANode, PAGE_CAPTION_ATTR, FPage.Caption);
   end;
end;

function TMainBlock.GetFromXML(ANode: IXMLNode): TError;
begin
   result := inherited GetFromXML(ANode);
   if ANode <> nil then
      FZOrder := GetNodeAttrInt(ANode, Z_ORDER_ATTR);
end;

function TMainBlock.IsBoldDesc: boolean;
begin
   result := True;
end;

procedure TMainBlock.MoveComment(AComment: TComment; dx, dy: integer);
begin
   if AComment.Visible then
      TInfra.MoveWinTopZ(AComment, AComment.Left+dx, AComment.Top+dy);
end;

function TMainBlock.GetUndoObject: TObject;
begin
   result := GProject.FindUserFunction(Self);
end;

function TMainBlock.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := inherited Remove(ANode);
   if result then
   begin
      var userFunction := GProject.FindUserFunction(Self);
      if userFunction <> nil then
      begin
         userFunction.Active := False;
         TInfra.UpdateCodeEditor;
      end;
   end;
end;

end.
