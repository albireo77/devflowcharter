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
   Messages, MultiLine_Block;

type

   TMultiAssignBlock = class(TMultiLineBlock)
      protected
         procedure OnChangeMemo(Sender: TObject); override;
         procedure Paint; override;
      public
         constructor Create(const ABranch: TBranch); overload;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID); overload; override;
         function Clone(const ABranch: TBranch): TBlock; override;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(const AColor: TColor); override;
   end;

implementation

uses
   ApplicationCommon, StrUtils, CommonTypes, Windows, LangDefinition, FastcodeAnsiStringReplaceUnit;

constructor TMultiAssignBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   FType := blMultAssign;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatements.ShowHint := true;
end;

function TMultiAssignBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TMultiAssignBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

procedure TMultiAssignBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelMultiAssign);
end;

constructor TMultiAssignBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 91);
end;

procedure TMultiAssignBlock.OnChangeMemo(Sender: TObject);
var
   lText, lLine: string;
   i: integer;
begin
   GChange := 1;
   FErrLine := -1;
   FStatements.Font.Color := GSettings.FontColor;
   lText := Trim(FStatements.Text);
   FStatements.Hint := i18Manager.GetFormattedString('ExpOk', [lText, CRLF]);
   UpdateEditor(nil);
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
            if not TInfra.Parse(lLine, prsAssign) then
            begin
               FStatements.Font.Color := NOK_COLOR;
               FStatements.Hint := i18Manager.GetFormattedString('ExpErrMult', [i+1, lLine, CRLF, errString]);
               FErrLine := i;
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
   if fsStrikeOut in Font.Style then
      exit;
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

procedure TMultiAssignBlock.ChangeColor(const AColor: TColor);
var
   lBool: boolean;
begin
   inherited ChangeColor(AColor);
   lBool := FRefreshMode;
   FRefreshMode := true;
   try
      FStatements.OnChange(FStatements);
   finally
      FRefreshMode := lBool;
   end;
end;

end.
