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


 
{ This unit contains stuff to support Python language }

unit Python_Template;

interface

implementation

uses
   SynHighlighterPython, ApplicationCommon, LangDefinition;

var
   pythonLang: TLangDefinition;

procedure Python_SetHLighterAttrs;
var
   hlighter: TSynPythonSyn;
begin
   if (pythonLang <> nil) and (pythonLang.HighLighter is TSynPythonSyn) then
   begin
      hlighter := TSynPythonSyn(pythonLang.HighLighter);
      hlighter.StringAttri.Foreground  := GSettings.EditorStringColor;
      hlighter.NumberAttri.Foreground  := GSettings.EditorNumberColor;
      hlighter.FloatAttri.Foreground   := GSettings.EditorNumberColor;
      hlighter.HexAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.OctalAttri.Foreground   := GSettings.EditorNumberColor;
      hlighter.CommentAttri.Foreground := GSettings.EditorCommentColor;
      hlighter.StringAttri.Background  := GSettings.EditorBkgColor;
      hlighter.NumberAttri.Background  := GSettings.EditorBkgColor;
      hlighter.FloatAttri.Background   := GSettings.EditorBkgColor;
      hlighter.OctalAttri.Background   := GSettings.EditorBkgColor;
      hlighter.HexAttri.Background     := GSettings.EditorBkgColor;
      hlighter.CommentAttri.Background := GSettings.EditorBkgColor;
   end;
end;



initialization

   pythonLang := GInfra.GetLangDefinition(PYTHON_LANG_ID);
   if pythonLang <> nil then
      pythonLang.SetHLighterAttrs := Python_SetHLighterAttrs;
       
end.