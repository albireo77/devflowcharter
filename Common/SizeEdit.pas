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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls;

type

  TSizeEdit = class(TEdit)
     private
        function GetDimensionCount: integer;
     public
        property DimensionCount: integer read GetDimensionCount default 0;
        constructor Create(const AParent: TWinControl);
        function ParseSize: boolean;
        function GetDimension(const ADimensIndex: integer): string;
        procedure OnChangeSize(Sender: TObject);
  end;


implementation

uses
   ApplicationCommon, StrUtils, LangDefinition, CommonTypes;

constructor TSizeEdit.Create(const AParent: TWinControl);
begin
   inherited Create(AParent);
   Parent := AParent;
   Text := '1';
   ShowHint := true;
   CharCase := ecUpperCase;
   Hint := AnsiReplaceStr(i18Manager.GetString('edtSizeHint'), '##', CRLF);
   ParentFont := false;
   Font.Style := [];
   Font.Color := BLACK_COLOR;
   DoubleBuffered := true;
   OnChange := OnChangeSize;
end;

function TSizeEdit.ParseSize: boolean;
var
   txt: string;
   i: integer;
   lang: TLangDefinition;
begin
   txt := AnsiReplaceStr(Text, ' ', '');
   result := false;
   if (txt <> '') and (AnsiPos(',-', txt) = 0) and (AnsiPos(',0', txt) = 0) and not (txt[1] in ['0', '-']) then
   begin
      result := true;
      lang := GInfra.GetLangDefinition(PASCAL_LANG_ID);
      if (lang <> nil) and Assigned(lang.Parse) then
      begin
         for i := 1 to GetDimensionCount do
         begin
            result := lang.Parse(GetDimension(i), prsVarSize) = 0;
            if not result then break;
         end;
      end;
   end;
end;

function TSizeEdit.GetDimensionCount: integer;
var
   i, len: integer;
   txt: string;
begin
   result := 0;
   txt := Trim(Text);
   len := Length(txt);
   if len > 0 then
   begin
      for i := 1 to len do
      begin
         if txt[i] = ',' then
            Inc(result);
      end;
      if txt <> '1' then
         Inc(result);
   end;
end;

function TSizeEdit.GetDimension(const ADimensIndex: integer): string;
var
   i, cnt: integer;
   txt: string;
begin
   result := '';
   cnt := 1;
   txt := Trim(Text);
   for i := 1 to Length(txt) do
   begin
      if txt[i] <> ',' then
         result := result + txt[i]
      else
      begin
         Inc(cnt);
         if cnt > ADimensIndex then
            break
         else
            result := '';
      end;
   end;
end;

procedure TSizeEdit.OnChangeSize(Sender: TObject);
begin
   if not ParseSize then
      Font.Color := NOK_COLOR
   else
      Font.Color := BLACK_COLOR;
end;

end.
