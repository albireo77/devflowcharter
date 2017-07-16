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



unit MultiInstr_Block;

interface

uses
   Vcl.Graphics, System.Classes, Base_Block, CommonInterfaces, MultiLine_Block;

type

   TMultiInstrBlock = class(TMultiLineBlock)
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
   System.SysUtils, System.StrUtils, System.UITypes, ApplicationCommon, CommonTypes, LangDefinition;

constructor TMultiInstrBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: integer; const AId: integer = ID_INVALID);
begin
   FType := blMultiInstr;
   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);
   FStatements.ShowHint := true;
end;

function TMultiInstrBlock.Clone(const ABranch: TBranch): TBlock;
begin
   result := TMultiInstrBlock.Create(ABranch, Left, Top, Width, Height);
   result.CloneFrom(Self);
end;

procedure TMultiInstrBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelMultiInstr);
end;

constructor TMultiInstrBlock.Create(const ABranch: TBranch);
begin
   Create(ABranch, 0, 0, 140, 91);
end;

procedure TMultiInstrBlock.OnChangeMemo(Sender: TObject);
var
   txt, line: string;
   i: integer;
begin
   GChange := 1;
   FErrLine := -1;
   FStatements.Font.Color := GSettings.FontColor;
   txt := Trim(FStatements.Text);
   FStatements.Hint := i18Manager.GetFormattedString('ExpOk', [txt, sLineBreak]);
   UpdateEditor(nil);
   if GSettings.ParseMultiAssign then
   begin
      if txt.IsEmpty then
      begin
         FStatements.Hint := i18Manager.GetFormattedString('NoInstr', [sLineBreak]);
         FStatements.Font.Color := WARN_COLOR
      end
      else
      begin
         for i := 0 to FStatements.Lines.Count-1 do
         begin
            line := FStatements.Lines.Strings[i].Trim;
            if not TInfra.Parse(line, prsAssign) then
            begin
               FStatements.Font.Color := NOK_COLOR;
               FStatements.Hint := i18Manager.GetFormattedString('ExpErrMult', [i+1, line, sLineBreak, errString]);
               FErrLine := i;
               break;
            end;
         end;
      end;
   end;
   inherited;
end;

function TMultiInstrBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   i: integer;
   template, line: string;
   lang: TLangDefinition;
   tmpList: TStringList;
begin
   result := 0;
   if (fsStrikeOut in Font.Style) or (FStatements.Text = '') then
      exit;
   lang := GInfra.GetLangDefinition(ALangId);
   if (lang <> nil) and not lang.InstrTemplate.IsEmpty then
   begin
      tmpList := TStringList.Create;
      try
         for i := 0 to FStatements.Lines.Count-1 do
         begin
            line := FStatements.Lines.Strings[i].Trim;
            if not line.IsEmpty then
            begin
               template := ReplaceStr(lang.InstrTemplate, PRIMARY_PLACEHOLDER, line);
               GenerateTemplateSection(tmpList, template, ALangId, ADeep);
            end
            else
               tmpList.AddObject('', Self);
         end;
         if tmpList.Text = '' then
            GenerateTemplateSection(tmpList, ReplaceStr(lang.InstrTemplate, PRIMARY_PLACEHOLDER, ''), ALangId, ADeep);
         if EndsText(sLineBreak, FStatements.Text) then
            tmpList.AddObject('', Self);
         TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
         result := tmpList.Count;
      finally
         tmpList.Free;
      end;
   end;
end;

procedure TMultiInstrBlock.ChangeColor(const AColor: TColor);
var
   b: boolean;
begin
   inherited ChangeColor(AColor);
   b := FRefreshMode;
   FRefreshMode := true;
   try
      FStatements.OnChange(FStatements);
   finally
      FRefreshMode := b;
   end;
end;

end.
