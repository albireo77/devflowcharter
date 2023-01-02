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

unit BlockFactory;

interface

uses
   Base_Block, Types, OmniXML, BlockTabSheet;

type

   TBlockFactory = class(TObject)
   public
      class function Create(ABranch: TBranch; ABlockType: TBlockType): TBlock; overload;
      class function Create(ANode: IXMLNode; ABranch: TBranch): TBlock; overload;
      class function Create(ANode: IXMLNode; ATab: TBlockTabSheet): TBlock; overload;
   end;

implementation

uses
   Instr_Block, MultiInstr_Block, InOut_Block, FunctionCall_Block, WhileDo_Block,
   RepeatUntil_Block, ForDo_Block, IfElse_Block, If_Block,
   Case_Block, Return_Block, Text_Block, Main_Block, Folder_Block;

class function TBlockFactory.Create(ABranch: TBranch; ABlockType: TBlockType): TBlock;
begin
   result := nil;
   case ABlockType of
      blInstr:      result := TInstrBlock.Create(ABranch);
      blMultiInstr: result := TMultiInstrBlock.Create(ABranch);
      blInput:      result := TInputBlock.Create(ABranch);
      blOutput:     result := TOutputBlock.Create(ABranch);
      blFuncCall:   result := TFunctionCallBlock.Create(ABranch);
      blWhile:      result := TWhileDoBlock.Create(ABranch);
      blRepeat:     result := TRepeatUntilBlock.Create(ABranch);
      blFor:        result := TForDoBlock.Create(ABranch);
      blIfElse:     result := TIfElseBlock.Create(ABranch);
      blIf:         result := TIfBlock.Create(ABranch);
      blCase:       result := TCaseBlock.Create(ABranch);
      blReturn:     result := TReturnBlock.Create(ABranch);
      blText:       result := TTextBlock.Create(ABranch);
      blFolder:     result := TFolderBlock.Create(ABranch);
   end;
end;

class function TBlockFactory.Create(ANode: IXMLNode; ABranch: TBranch): TBlock;
begin
   result := nil;
   var p := TBlockParms.New(ANode);
   case p.bt of
      blInstr:      result := TInstrBlock.Create(ABranch, p);
      blMultiInstr: result := TMultiInstrBlock.Create(ABranch, p);
      blInput:      result := TInputBlock.Create(ABranch, p);
      blOutput:     result := TOutputBlock.Create(ABranch, p);
      blFuncCall:   result := TFunctionCallBlock.Create(ABranch, p);
      blWhile:      result := TWhileDoBlock.Create(ABranch, p);
      blRepeat:     result := TRepeatUntilBlock.Create(ABranch, p);
      blIf:         result := TIfBlock.Create(ABranch, p);
      blFor:        result := TForDoBlock.Create(ABranch, p);
      blCase:       result := TCaseBlock.Create(ABranch, p);
      blReturn:     result := TReturnBlock.Create(ABranch, p);
      blText:       result := TTextBlock.Create(ABranch, p);
      blFolder:     result := TFolderBlock.Create(ABranch, p);
      blIfElse:     result := TIfElseBlock.Create(ABranch, p);
   end;
   if result <> nil then
      result.GetFromXML(ANode);
end;

class function TBlockFactory.Create(ANode: IXMLNode; ATab: TBlockTabSheet): TBlock;
begin
   result := nil;
   var p := TBlockParms.New(ANode);
   if p.bt = blMain then
   begin
      result := TMainBlock.Create(ATab, p);
      result.GetFromXML(ANode);
   end;
end;

end.
