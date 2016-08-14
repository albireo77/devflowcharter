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
   Windows, Controls, Forms, Graphics, Classes, Base_Block, ComCtrls, Messages,
   OmniXML, CommonInterfaces, CommonTypes, BaseIterator, BlockTabSheet;

type

   TCommentIterator = class(TBaseIterator);

   TMainBlock = class(TGroupBlock, IWinControl, IMaxBoundable)
      private
         FPage: TBlockTabSheet;
         FLabelRect: TRect;
         FHandle: HDC;
      public
         ShowI: boolean;
         UserFunction: TObject;
         constructor Create(const APage: TBlockTabSheet; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         constructor Create(const APage: TBlockTabSheet; const ATopLeft: TPoint); overload;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; override;
         function GetDescription: string; override;
         function GetFromXML(const ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(const ATag: IXMLElement); override;
         procedure PaintToCanvas(const ACanvas: TCanvas);
         function GetMaxBounds: TPoint;
         procedure ExportToGraphic(const AImage: TGraphic); override;
         procedure SetWidth(const AMinX: integer); override;
         function GetHandle: THandle;
         procedure SetZOrder(const AValue: integer);
         function GetZOrder: integer;
         function IsBoldDesc: boolean; override;
         function Remove: boolean; override;
         procedure DrawLabel;
      protected
         FZOrder: integer;
         FStartLabel,
         FStopLabel: string;
         procedure MyOnResize(Sender: TObject);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         procedure Paint; override;
         function GetFunctionLabel(var ARect: TRect): string;
         function GetDefaultWidth: integer;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         function GetUndoObject: TObject; override;
         procedure SetPage(APage: TBlockTabSheet); override;
         function GetPage: TBlockTabSheet; override;
         procedure OnWindowPosChanged(x, y: integer); override;
   end;

const
   MAIN_BLOCK_DEF_WIDTH = 100;
   MAIN_BLOCK_DEF_HEIGHT = 101;

implementation

uses
   ApplicationCommon, SysUtils, XMLProcessor, StrUtils, DeclareList, FastcodeAnsiStringReplaceUnit,
   Navigator_Form, Return_Block, LangDefinition, UserFunction, Comment, Menus;

constructor TMainBlock.Create(const APage: TBlockTabSheet; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
var
   lDefWidth, lHalfDefWidth: integer;
begin

   FType := blMain;
   FPage := APage;

   inherited Create(nil, ALeft, ATop, AWidth, AHeight, Point(p1X, p1Y), AId);

   FStartLabel := i18Manager.GetString('CaptionStart');
   FStopLabel := i18Manager.GetString('CaptionStop');

   lDefWidth := GetDefaultWidth;
   lHalfDefWidth := lDefWidth div 2;
   if lDefWidth > Width then
   begin
      Width := lDefWidth;
      Branch.Hook.X := lHalfDefWidth;
      BottomHook := lHalfDefWidth;
      TopHook.X := lHalfDefWidth;
   end
   else
   begin
      BottomHook := b_hook;
      TopHook.X := p1X;
   end;

   FInitParms.Width := lDefWidth;
   FInitParms.Height := MAIN_BLOCK_DEF_HEIGHT;
   FInitParms.BottomHook := lHalfDefWidth;
   FInitParms.BranchPoint.X := lHalfDefWidth;
   FInitParms.BottomPoint.X := -60000;
   FInitParms.P2X := 0;
   FInitParms.HeightAffix := 42;

   BottomPoint.X := FInitParms.BottomPoint.X;
   TopHook.Y := 30;
   ShowI := true;
   FZOrder := -1;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   OnResize := MyOnResize;
   FLabelRect := Rect(0, 0, 0, 0);
   FHandle := Canvas.Handle;
   FStatement.Free;
   FStatement := nil;
end;

constructor TMainBlock.Create(const APage: TBlockTabSheet; const ATopLeft: TPoint);
begin
   Create(APage,
          ATopLeft.X,
          ATopLeft.Y,
          MAIN_BLOCK_DEF_WIDTH,
          MAIN_BLOCK_DEF_HEIGHT,
          MAIN_BLOCK_DEF_WIDTH div 2,
          MAIN_BLOCK_DEF_WIDTH div 2,
          MAIN_BLOCK_DEF_HEIGHT-42);
end;

procedure TMainBlock.SetPage(APage: TBlockTabSheet);
var
   iter: IIterator;
   lHeader: TUserFunctionHeader;
   lUnPin: boolean;
begin
   if FPage <> APage then
   begin
      lUnPin := Expanded and (PinComments > 0);
      try
         iter := GetPinComments;
         while iter.HasNext do
            TComment(iter.Next).Page := APage;
      finally
         if lUnPin then
            UnPinComments;
      end;
      FPage := APage;
      Parent := APage;
      if UserFunction <> nil then
      begin
         lHeader := TUserFunction(UserFunction).Header;
         if lHeader <> nil then
            lHeader.SetPageCombo(APage.Caption);
      end;
   end;
end;

function TMainBlock.GetPage: TBlockTabSheet;
begin
   result := FPage;
end;

function TMainBlock.GetDefaultWidth: integer;
var
   R: TRect;
   lWidth: integer;
begin
   R := GetEllipseTextRect(Point(0, 0), FStartLabel);
   result := R.Right - R.Left;
   R := GetEllipseTextRect(Point(0, 0), FStopLabel);
   lWidth := R.Right - R.Left;
   if lWidth > result then
      result := lWidth;
   result := result + 40;
end;

procedure TMainBlock.PaintToCanvas(const ACanvas: TCanvas);
var
   iter: IIterator;
   lComment: TComment;
   lBStyle: TBorderStyle;
   lStart: integer;
begin
   if Visible then
   begin
      ShowI := false;
      ACanvas.Lock;
      PaintTo(ACanvas, Left + Parent.Left, Top + Parent.Top);
      if Expanded then
      begin
         iter := GetComments;
         while iter.HasNext do
         begin
            lComment := TComment(iter.Next);
            if lComment.Visible then
            begin
               lBStyle := lComment.BorderStyle;
               lStart := lComment.SelStart;
               lComment.BorderStyle := bsNone;
               lComment.PaintTo(ACanvas, lComment.Left + lComment.Parent.Left, lComment.Top + lComment.Parent.Top);
               lComment.BorderStyle := lBStyle;
               lComment.SelStart := lStart;
            end;
         end;
      end;
      ACanvas.Unlock;
      ShowI := true;
   end;
end;

procedure TMainBlock.SetZOrder(const AValue: integer);
begin
   FZOrder := FPage.PageIndex * 100 + AValue;
end;

function TMainBlock.GetZOrder: integer;
begin
   result := FZOrder;
end;

function TMainBlock.GetMaxBounds: TPoint;
var
   lPoint: TPoint;
   iter: IIterator;
   lComment: TComment;
begin
   result := Point(0, 0);
   if Visible then
   begin
      result.X := BoundsRect.Right + MARGIN_X;
      result.Y := BoundsRect.Bottom + MARGIN_Y;
      if Expanded then
      begin
         iter := GetComments;
         while iter.HasNext do
         begin
            lComment := TComment(iter.Next);
            if lComment.Visible then
            begin
               lPoint := lComment.GetMaxBounds;
               if lPoint.X > result.X then
                  result.X := lPoint.X;
               if lPoint.Y > result.Y then
                  result.Y := lPoint.Y;
            end;
         end;
      end;
   end;
end;

function TMainBlock.GetHandle: THandle;
begin
   result := 0;
   if Visible then
      result := Handle;
end;

procedure TMainBlock.ExportToGraphic(const AImage: TGraphic);
var
   lBitmap: TBitmap;
   lPoint: TPoint;
   iter: IIterator;
   lComment: TComment;
   lBStyle: TBorderStyle;
   lStart: integer;
begin
   if AImage is TBitmap then
      lBitmap := TBitmap(AImage)
   else
      lBitmap := TBitmap.Create;
   lPoint := GetMaxBounds;
   lPoint.X := lPoint.X - Left - MARGIN_X + 1;
   lPoint.Y := lPoint.Y - Top - MARGIN_Y + 1;
   lBitmap.Width := lPoint.X;
   lBitmap.Height := lPoint.Y;
   ShowI := false;
   lBitmap.Canvas.Lock;
   PaintTo(lBitmap.Canvas, 1, 1);
   if Expanded then
   begin
      iter := GetComments;
      while iter.HasNext do
      begin
         lComment := TComment(iter.Next);
         if lComment.Visible then
         begin
            lBStyle := lComment.BorderStyle;
            lStart := lComment.SelStart;
            lComment.BorderStyle := bsNone;
            lComment.PaintTo(lBitmap.Canvas, lComment.Left-Left, lComment.Top-Top);
            lComment.BorderStyle := lBStyle;
            lComment.SelStart := lStart;
         end;
      end;
   end;
   lBitmap.Canvas.Unlock;
   ShowI := true;
   if AImage <> lBitmap then
   begin
      AImage.Assign(lBitmap);
      lBitmap.Free;
   end;
end;

procedure TMainBlock.Paint;
var
   lColor: TColor;
   lLabel: string;
   R: TRect;
   lFontStyles: TFontStyles;
begin
   inherited;
   if Expanded then
   begin
      IPoint.X := Branch.Hook.X + 30;
      IPoint.Y := 35;
      with Canvas do
      begin
         lFontStyles := Font.Style;
         Font.Style := [];
         Brush.Style := bsClear;
         lLabel := GetFunctionLabel(R);
         if lLabel <> '' then
         begin
            lColor := Font.Color;
            Font.Color := clNavy;
            DrawText(Handle, PChar(lLabel), -1, R, 0);
            Font.Color := lColor;
            if Handle = FHandle then
               FLabelRect := R;
         end
         else
            FLabelRect := Rect(0, 0, 0, 0);
      end;
      DrawEllipsedText(Point(Branch.Hook.X, TopHook.Y), FStartLabel);
      if Branch.FindInstanceOf(TReturnBlock) = -1 then
         DrawEllipsedText(Point(BottomHook, Height-11), FStopLabel);
      Font.Style := lFontStyles;
      DrawArrowLine(Point(Branch.Hook.X, TopHook.Y), Branch.Hook);
   end;
   DrawI;
end;

function TMainBlock.GetFunctionLabel(var ARect: TRect): string;
var
   lLang: TLangDefinition;
   lDelta: integer;
   lHeader: TUserFunctionHeader;
begin
   result := '';
   ARect := Rect(Branch.Hook.X+75, 7, 0, 0);
   if GSettings.ShowFuncLabels and (UserFunction <> nil) and Expanded then
   begin
      lLang := nil;
      lHeader := TUserFunction(UserFunction).Header;
      if lHeader <> nil then
      begin
         if Assigned(GInfra.CurrentLang.GetUserFuncDesc) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.GetUserFuncDesc) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            result := lLang.GetUserFuncDesc(lHeader);
      end
      else
      begin
         if Assigned(GInfra.CurrentLang.GetMainProgramDesc) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.GetMainProgramDesc) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            result := lLang.GetMainProgramDesc;
      end;
   end;
   if result <> '' then
   begin
      DrawText(Canvas.Handle, PChar(result), -1, ARect, DT_CALCRECT);
      if (Branch.First <> nil) and (ARect.Bottom > Branch.First.Top-5) and (ARect.Left < Branch.First.BoundsRect.Right+5) then
      begin
         lDelta := Branch.First.BoundsRect.Right + 5 - ARect.Left;
         ARect.Left := ARect.Left + lDelta;
         ARect.Right := ARect.Right + lDelta;
      end;
   end;
end;

procedure TMainBlock.DrawLabel;
var
   R: TRect;
   lLabel: string;
   lColor: TColor;
begin
   lLabel := GetFunctionLabel(R);
   if lLabel <> '' then
   begin
      with Canvas do
      begin
         if not IsRectEmpty(FLabelRect) then
         begin
            Brush.Style := bsSolid;
            Brush.Color := Color;
            PatBlt(Handle, FLabelRect.Left, FLabelRect.Top, FLabelRect.Right, FLabelRect.Bottom, PATCOPY);
         end;
         lColor := Font.Color;
         Font.Color := clNavy;
         DrawText(Handle, PChar(lLabel), -1, R, 0);
         Font.Color := lColor;
         FLabelRect := R;
      end;
   end;
end;

procedure TMainBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if Expanded then
      inherited MyOnMouseMove(Sender, Shift, X, Y)
   else
   begin
      SelectBlock(Point(X, Y));
      SetCursor(Point(X, Y));
   end;
end;

procedure TMainBlock.MyOnResize(Sender: TObject);
begin
   FPage.Form.SetScrollbars;
   if FPage.Form.HorzScrollBar.Position + BoundsRect.Right < MARGIN_X then
      Left := 0;
   if FPage.Form.VertScrollBar.Position + BoundsRect.Bottom < MARGIN_Y then
      Top := 0;
   BringAllToFront;
end;

function TMainBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lName, lTemplate, lEnding: string;
   lLangDef: TLangDefinition;
   lStrListProgram, lVarStrList, lTmpList: TStringList;
   lVars: TVarDeclareList;
   lIsMainProgram: boolean;
   lHeader: TUserFunctionHeader;
begin
   result := 0;
   lHeader := TInfra.GetFunctionHeader(Self);
   if lHeader <> nil then
   begin
      lVars := lHeader.LocalVars;
      lName := lHeader.GetName;
      lIsMainProgram := false;
   end
   else
   begin
      lVars := GProject.GlobalVars;
      lName := GProject.Name;
      lIsMainProgram := true;
   end;
   if ALangId = TIBASIC_LANG_ID then
   begin
      lTmpList := TStringList.Create;
      try
         GenerateNestedCode(lTmpList, PRIMARY_BRANCH_IND, ADeep+1, ALangId);
         TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
         result := lTmpList.Count;
      finally
         lTmpList.Free;
      end;
   end
   else
   begin
      lLangDef := GInfra.GetLangDefinition(ALangId);
      if lLangDef <> nil then
      begin
         if lIsMainProgram then
            lTemplate := lLangDef.MainProgramTemplate
         else
            lTemplate := lLangDef.ProgramTemplate;
         if lTemplate <> '' then
         begin
            lStrListProgram := TStringList.Create;
            lTmpList := TStringList.Create;
            try
               lStrListProgram.Text := FastCodeAnsiStringReplace(lTemplate, PRIMARY_PLACEHOLDER, lName);
               if lIsMainProgram then
               begin
                  lEnding := '';
                  if not (GetBranch(PRIMARY_BRANCH_IND).Last is TReturnBlock) then
                     lEnding := lLangDef.ProgramReturnTemplate;
                  TInfra.InsertTemplateLines(lStrListProgram, '%s3', lEnding);
               end;
               lVarStrList := TStringList.Create;
               try
                  if Assigned(GInfra.CurrentLang.VarSectionGenerator) then
                     GInfra.CurrentLang.VarSectionGenerator(lVarStrList, lVars)
                  else if Assigned(GInfra.DummyLang.VarSectionGenerator) then
                     GInfra.DummyLang.VarSectionGenerator(lVarStrList, lVars);
                  TInfra.InsertTemplateLines(lStrListProgram, '%s2', lVarStrList);
               finally
                  lVarStrList.Free;
               end;
               GenerateTemplateSection(lTmpList, lStrListProgram, ALangId, ADeep);
               TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
               result := lTmpList.Count;
            finally
               lStrListProgram.Free;
               lTmpList.Free;
            end;
         end;
      end;
   end;
end;

procedure TMainBlock.SetWidth(const AMinX: integer);
var
   minVal, lVal: integer;
   R: TRect;
begin
   minVal := Branch.GetMostRight + 30;
   if GetFunctionLabel(R) <> '' then
      lVal := R.Right + 10
   else
      lVal := minVal;
   if lVal > minVal then
      minVal := lVal;
   if AMinX < minVal then
      Width := minVal
   else
      Width := AMinX + 5;
end;

function TMainBlock.GetDescription: string;
begin
end;

function TMainBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lBlock: TBlock;
begin
   result := AParentNode;
   lBlock := Branch.First;
   while lBlock <> nil do
   begin
      lBlock.GenerateTree(AParentNode);
      lBlock := lBlock.Next;
   end;
end;

procedure TMainBlock.SaveInXML(const ATag: IXMLElement);
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      ATag.SetAttribute(Z_ORDER_ATTR, IntToStr(FZOrder));
      if FPage <> GProject.GetMainPage then
         ATag.SetAttribute(PAGE_CAPTION_ATTR, FPage.Caption);
   end;
end;

function TMainBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
      FZOrder := StrToIntDef(ATag.GetAttribute(Z_ORDER_ATTR), -1);
end;

function TMainBlock.IsBoldDesc: boolean;
begin
   result := true;
end;

procedure TMainBlock.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
   MoveComments(Msg.WindowPos^.x, Msg.WindowPos^.y);
   inherited;
end;

procedure TMainBlock.OnWindowPosChanged(x, y: integer);
begin
// do nothing
end;

function TMainBlock.GetUndoObject: TObject;
begin
   result := UserFunction;
end;

function TMainBlock.Remove: boolean;
begin
   result := inherited Remove;
   if result then
      TUserFunction(UserFunction).Active := false;
end;

end.
