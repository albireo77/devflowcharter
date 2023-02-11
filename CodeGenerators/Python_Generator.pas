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


 
{ This unit contains stuff to support Python code generation }

unit Python_Generator;

interface

implementation

uses
   SynHighlighterPython, Vcl.Graphics, LangDefinition, Infrastructure, Constants;

var
   pythonLang: TLangDefinition;

procedure Python_SetHLighterAttrs;
begin
   if (pythonLang <> nil) and (pythonLang.HighLighter is TSynPythonSyn) then
   begin
      var bkgColor := GSettings.EditorBkgColor;
      var hlighter := TSynPythonSyn(pythonLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := bkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := bkgColor;
      hlighter.FloatAttri.Foreground      := GSettings.EditorNumberColor;
      hlighter.FloatAttri.Background      := bkgColor;
      hlighter.HexAttri.Foreground        := GSettings.EditorNumberColor;
      hlighter.HexAttri.Background        := bkgColor;
      hlighter.OctalAttri.Foreground      := GSettings.EditorNumberColor;
      hlighter.OctalAttri.Background      := bkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := bkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := bkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := bkgColor;
   end;
end;



initialization

   pythonLang := GInfra.GetLangDefinition(PYTHON_LANG_ID);
   if pythonLang <> nil then
      pythonLang.SetHLighterAttrs := Python_SetHLighterAttrs;
       
end.
