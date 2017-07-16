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

unit SizeEdit;

interface

uses
  Vcl.Controls, Vcl.StdCtrls;

type

  TSizeEdit = class(TEdit)
     private
        function GetDimensionCount: integer;
     public
        property DimensionCount: integer read GetDimensionCount default 0;
        constructor Create(AParent: TWinControl);
        function ParseSize: boolean;
        function GetDimension(idx: integer): string;
        procedure OnChangeSize(Sender: TObject);
  end;


implementation

uses
   System.SysUtils, System.StrUtils, System.Math, ApplicationCommon, LangDefinition,
   CommonTypes;

constructor TSizeEdit.Create(AParent: TWinControl);
begin
   inherited Create(AParent);
   Parent := AParent;
   Text := '1';
   ShowHint := true;
   CharCase := ecUpperCase;
   Hint := ReplaceStr(i18Manager.GetString('edtSizeHint'), '##', sLineBreak);
   ParentFont := false;
   Font.Style := [];
   Font.Color := BLACK_COLOR;
   DoubleBuffered := true;
   OnChange := OnChangeSize;
end;

function TSizeEdit.ParseSize: boolean;
var
   txt: string;
   i, dcount: integer;
   lang: TLangDefinition;
begin
   result := false;
   dcount := GetDimensionCount;
   if dcount = MaxInt then
   begin
      result := GInfra.CurrentLang.AllowUnboundedArrays;
      exit;
   end;
   txt := ReplaceStr(Text, ' ', '');
   if (txt.Length > 0) and (Pos(',-', txt) = 0) and (Pos(',0', txt) = 0) and not (CharInSet(txt[1], ['0', '-'])) then
   begin
      result := true;
      lang := GInfra.GetLangDefinition(PASCAL_LANG_ID);
      if (lang <> nil) and Assigned(lang.Parse) then
      begin
         for i := 1 to dcount do
         begin
            result := lang.Parse(GetDimension(i), prsVarSize) = 0;
            if not result then
               break;
         end;
      end;
   end;
end;

function TSizeEdit.GetDimensionCount: integer;
var
   i: integer;
   txt: string;
begin
   result := 0;
   txt := Trim(Text);
   if txt.isEmpty then
      result := MaxInt
   else
   begin
      for i := 1 to txt.Length do
      begin
         if txt[i] = ',' then
            Inc(result);
      end;
      if txt <> '1' then
         Inc(result);
   end;
end;

function TSizeEdit.GetDimension(idx: integer): string;
var
   i, cnt: integer;
   txt: string;
begin
   result := '';
   cnt := 1;
   txt := Trim(Text);
   for i := 1 to txt.length do
   begin
      if txt[i] <> ',' then
         result := result + txt[i]
      else
      begin
         Inc(cnt);
         if cnt > idx then
            break
         else
            result := '';
      end;
   end;
end;

procedure TSizeEdit.OnChangeSize(Sender: TObject);
begin
   Font.Color := IfThen(ParseSize, BLACK_COLOR, NOK_COLOR);
end;

end.
