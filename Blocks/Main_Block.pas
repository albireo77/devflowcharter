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
   OmniXML, CommonInterfaces, Main_Form, CommonTypes, BaseIterator;

type

   TCommentIterator = class(TBaseIterator);

   TMainBlock = class(TGroupBlock, IWinControl, IMaxBoundable)
      public
         ShowI: boolean;
         OwnerUserFunction: TObject;
         constructor Create(const AParent: TMainForm; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID); overload;
         constructor Create(const AParent: TMainForm; const ATopLeft: TPoint); overload;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; override;
         function GetDescription: string; override;
         procedure BringAllToFront;
         procedure SetVisible(const AValue: boolean);
         function GetFromXML(const ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(const ATag: IXMLElement); override;
         procedure PaintToCanvas(const ACanvas: TCanvas);
         function GetMaxBounds: TPoint;
         procedure ExportToGraphic(const AImage: TGraphic); override;
         procedure SetWidth(const AMinX: integer); override;
         function GetHandle: THandle;
         procedure SetZOrderValue(const AValue: integer);
         function GetZOrderValue: integer;
         function IsBoldDesc: boolean; override;
      protected
         FZOrderValue: integer;
         FStartLabel,
         FStopLabel: string;
         procedure MyOnResize(Sender: TObject);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
         procedure Paint; override;
         function GetFunctionLabel(var ARect: TRect): string;
         function GetDefaultWidth: integer; override;
         function GetAllPinComments: IIterator;
   end;

const
   MAIN_BLOCK_DEF_WIDTH = 100;
   MAIN_BLOCK_DEF_HEIGHT = 101;


implementation

uses
   ApplicationCommon, SysUtils, XMLProcessor, StrUtils, DeclareList, FastcodeAnsiStringReplaceUnit,
   Navigator_Form, Return_Block, LangDefinition, UserFunction, Comment;

constructor TMainBlock.Create(const AParent: TMainForm; const ALeft, ATop, AWidth, AHeight, b_hook, p1X, p1Y: integer; const AId: integer = ID_INVALID);
var
   lDefWidth, lHalfDefWidth: integer;
begin

   FType := blMain;
   FParentForm := AParent;

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
   FZOrderValue := -1;
   Constraints.MinWidth := FInitParms.Width;
   Constraints.MinHeight := FInitParms.Height;
   OnResize := MyOnResize;
   FStatement.Free;
   FStatement := nil;
   OwnerUserFunction := nil;
end;

constructor TMainBlock.Create(const AParent: TMainForm; const ATopLeft: TPoint);
begin
   Create(AParent,
          ATopLeft.X,
          ATopLeft.Y,
          MAIN_BLOCK_DEF_WIDTH,
          MAIN_BLOCK_DEF_HEIGHT,
          MAIN_BLOCK_DEF_WIDTH div 2,
          MAIN_BLOCK_DEF_WIDTH div 2,
          MAIN_BLOCK_DEF_HEIGHT-42);
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
      PaintTo(ACanvas, Left + FParentForm.HorzScrollBar.Position, Top + FParentForm.VertScrollBar.Position);
      if Expanded then
      begin
         iter := GetAllPinComments;
         while iter.HasNext do
         begin
            lComment := TComment(iter.Next);
            lBStyle := lComment.BorderStyle;
            lStart := lComment.SelStart;
            lComment.BorderStyle := bsNone;
            lComment.PaintTo(ACanvas, lComment.Left + lComment.ParentForm.HorzScrollBar.Position, lComment.Top + lComment.ParentForm.VertScrollBar.Position);
            lComment.BorderStyle := lBStyle;
            lComment.SelStart := lStart;
         end;
      end;
      ACanvas.Unlock;
      ShowI := true;
   end;
end;

procedure TMainBlock.SetZOrderValue(const AValue: integer);
begin
   FZOrderValue := AValue;
end;

function TMainBlock.GetZOrderValue: integer;
begin
   result := FZOrderValue;
end;

function TMainBlock.GetMaxBounds: TPoint;
var
   lPoint: TPoint;
   iter: IIterator;
begin
   result := Point(0, 0);
   if Visible then
   begin
      result.X := BoundsRect.Right + FParentForm.HorzScrollBar.Position + MARGIN_X;
      result.Y := BoundsRect.Bottom + FParentForm.VertScrollBar.Position + MARGIN_Y;
      if Expanded then
      begin
         iter := GetAllPinComments;
         while iter.HasNext do
         begin
            lPoint := TComment(iter.Next).GetMaxBounds;
            if lPoint.X > result.X then
               result.X := lPoint.X;
            if lPoint.Y > result.Y then
               result.Y := lPoint.Y;
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
   lPoint.X := lPoint.X - Left - FParentForm.HorzScrollBar.Position - MARGIN_X + 1;
   lPoint.Y := lPoint.Y - Top - FParentForm.VertScrollBar.Position - MARGIN_Y + 1;
   lBitmap.Width := lPoint.X;
   lBitmap.Height := lPoint.Y;
   ShowI := false;
   lBitmap.Canvas.Lock;
   PaintTo(lBitmap.Canvas, 1, 1);
   if Expanded then
   begin
      iter := GetAllPinComments;
      while iter.HasNext do
      begin
         lComment := TComment(iter.Next);
         lBStyle := lComment.BorderStyle;
         lStart := lComment.SelStart;
         lComment.BorderStyle := bsNone;
         lComment.PaintTo(lBitmap.Canvas, lComment.Left-Left, lComment.Top-Top);
         lComment.BorderStyle := lBStyle;
         lComment.SelStart := lStart;
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
         end;
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
   if GSettings.ShowFlowchartLabels and (OwnerUserFunction <> nil) and Expanded then
   begin
      lLang := nil;
      lHeader := TUserFunction(OwnerUserFunction).Header;
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
   FParentForm.SetScrollbars;
   if FParentForm.HorzScrollBar.Position + BoundsRect.Right < MARGIN_X then
      Left := 0;
   if FParentForm.VertScrollBar.Position + BoundsRect.Bottom < MARGIN_Y then
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
begin
   result := 0;
   if (OwnerUserFunction is TUserFunction) and (TUserFunction(OwnerUserFunction).Header <> nil) then
   begin
      lVars := TUserFunction(OwnerUserFunction).Header.LocalVars;
      lName := TUserFunction(OwnerUserFunction).Header.GetName;
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
   minVal := BottomHook + 55;
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

procedure TMainBlock.SetVisible(const AValue: boolean);
var
   iter: IIterator;
begin
   Visible := AValue;
   if Expanded then
   begin
      iter := GetAllPinComments;
      while iter.HasNext do
         TComment(iter.Next).Visible := AValue;
   end;
   if Visible then
      BringAllToFront;
end;

function TMainBlock.GetAllPinComments: IIterator;
var
   lComment: TComment;
   iterc: IIterator;
   lIterator: TCommentIterator;
begin
   lIterator := TCommentIterator.Create;
   iterc := GProject.GetComments;
   while iterc.HasNext do
   begin
      lComment := TComment(iterc.Next);
      if (lComment.PinControl is TBlock) and (TBlock(lComment.PinControl).TopParentBlock = Self) then
      begin
         SetLength(lIterator.FArray, Length(lIterator.FArray)+1);
         lIterator.FArray[High(lIterator.FArray)] := lComment;
      end;
   end;
   result := lIterator;
end;

procedure TMainBlock.BringAllToFront;
var
   iter: IIterator;
begin
   BringToFront;
   iter := GetAllPinComments;
   while iter.HasNext do
      TComment(iter.Next).BringToFront;
end;

procedure TMainBlock.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
var
   x, y: integer;
   lComment: TComment;
   iter: IIterator;
begin
   y := Msg.WindowPos^.Y;
   x := Msg.WindowPos^.X;
   if (x <> 0) or (y <> 0) then
   begin
      GChange := 1;
      if Expanded then
      begin
         iter := GetAllPinComments;
         while iter.HasNext do
         begin
            lComment := TComment(iter.Next);
            if lComment.Visible then
            begin
               lComment.SetBounds(lComment.Left+x-Left, lComment.Top+y-Top, lComment.Width, lComment.Height);
               lComment.BringToFront;
            end;
         end;
      end;
   end;
   inherited;
end;

procedure TMainBlock.SaveInXML(const ATag: IXMLElement);
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      ATag.SetAttribute('x', IntToStr(Left+FParentForm.HorzScrollBar.Position));
      ATag.SetAttribute('y', IntToStr(Top+FParentForm.VertScrollBar.Position));
      ATag.SetAttribute('ZOrdVal', IntToStr(FZOrderValue));
   end;
end;

function TMainBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
      FZOrderValue := StrToIntDef(ATag.GetAttribute('ZOrdVal'), -1);
end;

function TMainBlock.IsBoldDesc: boolean;
begin
   result := true;
end;

end.
