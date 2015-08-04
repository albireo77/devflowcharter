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



unit MulAssign_Block;

interface

uses
   Controls, StdCtrls, Graphics, Classes, Base_Block, SysUtils, CommonInterfaces,
   Messages, StatementMemo;

type

   TMultiAssignBlock = class(TBlock)
      protected
         FStatements: TStatementMemo;
         procedure Paint; override;
         procedure OnChangeMemo(Sender: TObject); override;
         procedure MyOnDblClick(Sender: TObject);
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
      public
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload;
         constructor Create(const ABranch: TBranch; const ASource: TMultiAssignBlock); overload;
         constructor Create(const ABranch: TBranch); overload;
         function GetTextControl: TCustomEdit; override;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(const AColor: TColor); override;
         function GetFrontMemo: TMemo; override;
         procedure UpdateCodeEditor(AEdit: TCustomEdit); override;
   end;

implementation

uses
   ApplicationCommon, StrUtils, Statement, CommonTypes, Windows, LangDefinition,
   SourceEditor_Form, FastcodeAnsiStringReplaceUnit;

constructor TMultiAssignBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin

   FType := blMultAssign;

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatements := TStatementMemo.Create(Self);
   FStatements.Parent := Self;
   FStatements.SetBounds(0, 0, AWidth, Height-31);
   FStatements.ShowHint := True;
   FStatements.OnChange := OnChangeMemo;
   FStatements.OnDblClick := MyOnDblClick;
   FStatements.OnChange(FStatements);
   FStatements.OnMouseDown := OnMouseDown;
   if FStatements.CanFocus then
      FStatements.SetFocus;

   BottomPoint.X := AWidth div 2;
   BottomPoint.Y := Height - 31;
   IPoint.X := BottomPoint.X + 30;
   IPoint.Y := FStatements.Height + 10;
   BottomHook := BottomPoint.X;
   TopHook.X := BottomPoint.X;
   Constraints.MinWidth := 140;
   Constraints.MinHeight := 48;
   FStatement.Visible := false;         // statement isn't used in this block

end;

constructor TMultiAssignBlock.Create(const ABranch: TBranch; const ASource: TMultiAssignBlock);
begin

   Create(ABranch, ASource.Left, ASource.Top, ASource.Width, ASource.Height);
   
   ChangeFontSize(ASource.FStatement.Font.Size);
   ChangeFontStyle(ASource.FStatement.Font.Style);
   Visible := ASource.Visible;
   FStatements.Text := ASource.FStatements.Text;
end;

constructor TMultiAssignBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 91);
end;

procedure TMultiAssignBlock.MyOnDblClick(Sender: TObject);
begin
   FStatements.SelectAll;
end;


procedure TMultiAssignBlock.Paint;
begin
   inherited;
   TInfra.DrawArrowLine(Canvas, Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
   DrawI;
end;

procedure TMultiAssignBlock.OnChangeMemo(Sender: TObject);
var
   lText, lLine: string;
   i: integer;
begin
   GChange := 1;
   FStatements.Font.Color := GSettings.FontColor;
   lText := Trim(FStatements.Text);
   FStatements.Hint := i18Manager.GetFormattedString('ExpOk', [lText, CRLF]);
   if GSettings.UpdateCodeEditor and not SkipUpdateCodeEditor then
      UpdateCodeEditor(nil);
   if GSettings.ParseAssignMult then
   begin
      if lText = '' then
      begin
         FStatements.Hint := i18Manager.GetFormattedString('NoInstr', [CRLF]);
         FStatements.Font.Color := WARN_COLOR
      end
      else
      begin
         for i := 0 to FStatements.Lines.Count-1 do
         begin
            lLine := Trim(FStatements.Lines.Strings[i]);
            if not TInfra.Parse(lLine, prAssign) then
            begin
               FStatements.Font.Color := NOK_COLOR;
               FStatements.Hint := i18Manager.GetFormattedString('ExpErrMult', [i+1, lLine, CRLF, errString]);
               break;
            end;
         end;
      end;
   end;
   inherited;
end;

function TMultiAssignBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   i: integer;
   lTemplate, lLine: string;
   lLangDef: TLangDefinition;
   lTmpList: TStringList;
begin
   result := 0;
   lLangDef := GInfra.GetLangDefinition(ALangId);
   if (lLangDef <> nil) and (lLangDef.AssignTemplate <> '') then
   begin
      lTmpList := TStringList.Create;
      try
         for i := 0 to FStatements.Lines.Count-1 do
         begin
            lLine := Trim(FStatements.Lines.Strings[i]);
            if lLine <> '' then
            begin
               lTemplate := FastCodeAnsiStringReplace(lLangDef.AssignTemplate, PRIMARY_PLACEHOLDER, lLine);
               GenerateTemplateSection(lTmpList, lTemplate, ALangId, ADeep);
            end
            else
               lTmpList.AddObject('', Self);
         end;
         if lTmpList.Text = '' then
            GenerateTemplateSection(lTmpList, FastCodeAnsiStringReplace(lLangDef.AssignTemplate, PRIMARY_PLACEHOLDER, ''), ALangId, ADeep);
         if AnsiEndsText(CRLF, FStatements.Text) then
            lTmpList.AddObject('', Self);
         TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
         result := lTmpList.Count;
      finally
         lTmpList.Free;
      end;
   end;
end;

function TMultiAssignBlock.GetTextControl: TCustomEdit;
begin
   result := FStatements;
end;

procedure TMultiAssignBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if HResizeInd and Resize then
   begin
      BottomPoint.X := Width div 2;
      IPoint.X := BottomPoint.X + 30;
      TopHook.X := BottomPoint.X;
   end;
   if VResizeInd and Resize then
      IPoint.Y := FStatements.Height + 10;
end;

procedure TMultiAssignBlock.ChangeColor(const AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if GSettings.RectColor = GSettings.DesktopColor then
      FStatements.Color := AColor
   else
      FStatements.Color := GSettings.RectColor;
   FStatements.OnChange(FStatements);
end;

function TMultiAssignBlock.GetFrontMemo: TMemo;
begin
   result := FStatements;
end;

procedure TMultiAssignBlock.UpdateCodeEditor(AEdit: TCustomEdit);
var
   lRange: TCodeRange;
   lTemplateLines: TStringList;
   i: integer;
begin
   lRange := SourceEditorForm.SelectCodeBlock(Self, false);
   if lRange.FirstLineIdx <> -1 then
   begin
      lTemplateLines := TStringList.Create;
      try
         GenerateCode(lTemplateLines, GInfra.CurrentLang.Name, SourceEditorForm.GetIndentLevel(lRange.FirstLineIdx));
         with SourceEditorForm.memCodeEditor do
         begin
            Lines.BeginUpdate;
            for i := 0 to lRange.LastLineIdx - lRange.FirstLineIdx do
               Lines.Delete(lRange.FirstLineIdx);
            for i := lTemplateLines.Count-1 downto 0 do
               Lines.InsertObject(lRange.FirstLineIdx, lTemplateLines[i], lTemplateLines.Objects[i]);
            GotoLineAndCenter(lRange.FirstLineIdx);
            Lines.EndUpdate;
            OnChange(SourceEditorForm.memCodeEditor);
         end;
      finally
         lTemplateLines.Free;
      end;
   end;
end;

end.
