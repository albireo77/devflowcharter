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
   Vcl.Graphics, System.Classes, Base_Block, MultiLine_Block, Types;

type

   TMultiInstrBlock = class(TMultiLineBlock)
      protected
         procedure OnChangeMemo(Sender: TObject); override;
         procedure Paint; override;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload; override;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
         procedure ChangeColor(AColor: TColor); override;
   end;

implementation

uses
   System.SysUtils, System.StrUtils, Infrastructure, YaccLib, Constants;

constructor TMultiInstrBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin
   inherited Create(ABranch, ABlockParms);
   FStatements.ShowHint := true;
end;

procedure TMultiInstrBlock.Paint;
begin
   inherited;
   DrawBlockLabel(5, FStatements.BoundsRect.Bottom+1, GInfra.CurrentLang.LabelMultiInstr);
end;

constructor TMultiInstrBlock.Create(ABranch: TBranch);
begin
   Create(ABranch, TBlockParms.New(blMultiInstr, 0, 0, 140, 91));
end;

procedure TMultiInstrBlock.OnChangeMemo(Sender: TObject);
var
   txt, line: string;
   i: integer;
begin
   GProject.SetChanged;
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
            if not TInfra.Parse(line, yymAssign) then
            begin
               FStatements.Font.Color := NOK_COLOR;
               FStatements.Hint := i18Manager.GetFormattedString('ExpErrMult', [i+1, line, sLineBreak, TInfra.GetParserErrMsg]);
               FErrLine := i;
               break;
            end;
         end;
      end;
   end;
   inherited;
end;

function TMultiInstrBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
var
   i: integer;
   template: string;
   tmpList: TStringList;
begin
   if fsStrikeOut in Font.Style then
      Exit(0);
   template := GetBlockTemplate(ALangId);
   if template.IsEmpty then
      result := inherited GenerateCode(ALines, ALangId, ADeep, AFromLine)
   else
   begin
      tmpList := TStringList.Create;
      try
         for i := 0 to FStatements.Lines.Count-1 do
            GenerateTemplateSection(tmpList, ReplaceStr(template, PRIMARY_PLACEHOLDER, FStatements.Lines.Strings[i].Trim), ALangId, ADeep);
         if tmpList.Text.IsEmpty then
            GenerateTemplateSection(tmpList, ReplaceStr(template, PRIMARY_PLACEHOLDER, ''), ALangId, ADeep);
         if EndsText(sLineBreak, FStatements.Text) then
            tmpList.AddObject(GSettings.IndentString(ADeep), Self);
         TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
         result := tmpList.Count;
      finally
         tmpList.Free;
      end;
   end;
end;

procedure TMultiInstrBlock.ChangeColor(AColor: TColor);
var
   b, chon: boolean;
begin
   inherited ChangeColor(AColor);
   chon := GProject.ChangingOn;
   GProject.ChangingOn := false;
   b := FRefreshMode;
   FRefreshMode := true;
   try
      if Assigned(FStatements.OnChange) then
         FStatements.OnChange(FStatements);
   finally
      FRefreshMode := b;
      GProject.ChangingOn := chon;
   end;
end;

end.
