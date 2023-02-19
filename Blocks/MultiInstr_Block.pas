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
         procedure OnChangeStatements(Sender: TObject);
         procedure Paint; override;
      public
         constructor Create(ABranch: TBranch); overload;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms); overload;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; override;
   end;

implementation

uses
   System.SysUtils, System.StrUtils, Infrastructure, YaccLib, Constants;

constructor TMultiInstrBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms);
begin
   inherited Create(ABranch, ABlockParms);
   FStatements.ShowHint := True;
   FStatements.OnChange := OnChangeStatements;
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

procedure TMultiInstrBlock.OnChangeStatements(Sender: TObject);
begin
   GProject.SetChanged;
   FErrLine := -1;
   var txt := Trim(FStatements.Text);
   var h := i18Manager.GetFormattedString('ExpOk', [txt, sLineBreak]);
   var c := GSettings.FontColor;
   UpdateEditor(nil);
   if GSettings.ParseMultiAssign then
   begin
      if txt.IsEmpty then
      begin
         h := i18Manager.GetFormattedString('NoInstr', [sLineBreak]);
         c := WARN_COLOR
      end
      else
      begin
         for var i := 0 to FStatements.Lines.Count-1 do
         begin
            var line := FStatements.Lines.Strings[i].Trim;
            if not TInfra.Parse(line, yymAssign) then
            begin
               h := i18Manager.GetFormattedString('ExpErrMult', [i+1, line, sLineBreak, TInfra.GetParserErrMsg]);
               c := NOK_COLOR;
               FErrLine := i;
               break;
            end;
         end;
      end;
   end;
   FStatements.Hint := h;
   FStatements.Font.Color := c;
   inherited;
end;

function TMultiInstrBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin
   if fsStrikeOut in Font.Style then
      Exit(0);
   var template := GetBlockTemplate(ALangId);
   if template.IsEmpty then
      result := inherited GenerateCode(ALines, ALangId, ADeep, AFromLine)
   else
   begin
      var tmpList := TStringList.Create;
      try
         for var i := 0 to FStatements.Lines.Count-1 do
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

end.
