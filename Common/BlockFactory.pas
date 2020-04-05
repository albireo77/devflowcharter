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
   Base_Block, CommonTypes, OmniXML, BlockTabSheet;

type

   TBlockFactory = class(TObject)
   public
      class function Create(ABranch: TBranch; ABlockType: TBlockType): TBlock; overload;
      class function Create(ATag: IXMLElement; ABranch: TBranch): TBlock; overload;
      class function Create(ATag: IXMLElement; ATab: TBlockTabSheet): TBlock; overload;
   end;

implementation

uses
   Instr_Block, MultiInstr_Block, InOut_Block, FunctionCall_Block, WhileDo_Block,
   RepeatUntil_Block, ApplicationCommon, ForDo_Block, IfElse_Block, If_Block,
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

class function TBlockFactory.Create(ATag: IXMLElement; ABranch: TBranch): TBlock;
var
   p: TBlockInitParms;
begin
   result := nil;
   p := TBlockInitParms.Extract(ATag);
   case p.bt of
      blInstr:      result := TInstrBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blMultiInstr: result := TMultiInstrBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blInput:      result := TInputBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blOutput:     result := TOutputBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blFuncCall:   result := TFunctionCallBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blWhile:      result := TWhileDoBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blRepeat:     result := TRepeatUntilBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blIf:         result := TIfBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blFor:        result := TForDoBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blCase:       result := TCaseBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blReturn:     result := TReturnBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blText:       result := TTextBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bid);
      blFolder:     result := TFolderBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      blIfElse:     result := TIfElseBlock.Create(ABranch, p.x, p.y, p.w, p.h, p.brx, p.fbrx, p.bh, p.th, p.bry, p.fbry, p.flh, p.trh, p.bid);
   end;
   if result <> nil then
      result.GetFromXML(ATag);
end;

class function TBlockFactory.Create(ATag: IXMLElement; ATab: TBlockTabSheet): TBlock;
var
   p: TBlockInitParms;
begin
   result := nil;
   p := TBlockInitParms.Extract(ATag);
   if p.bt = blMain then
   begin
      result := TMainBlock.Create(ATab, p.x, p.y, p.w, p.h, p.bh, p.brx, p.bry, p.bid);
      result.GetFromXML(ATag);
   end;
end;

end.
