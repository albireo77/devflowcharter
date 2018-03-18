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


 
{ This unit contains stuff to support Java language }

unit Java_Template;

interface

implementation

uses
   SynHighlighterJava, ApplicationCommon, LangDefinition;

var
   javaLang: TLangDefinition;

procedure Java_SetHLighterAttrs;
var
   hlighter: TSynJavaSyn;
begin
   if (javaLang <> nil) and (javaLang.HighLighter is TSynJavaSyn) then
   begin
      hlighter := TSynJavaSyn(javaLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := GSettings.EditorBkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := GSettings.EditorBkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := GSettings.EditorBkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := GSettings.EditorBkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := GSettings.EditorBkgColor;
   end;
end;



initialization

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
      javaLang.SetHLighterAttrs := Java_SetHLighterAttrs;
       
end.