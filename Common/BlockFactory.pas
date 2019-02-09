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
      class function Create(ATag: IXMLElement; ABranch: TBranch; ATab: TBlockTabSheet = nil): TBlock; overload;
   end;

implementation

uses
   System.SysUtils, System.Rtti, Instr_Block, MultiInstr_Block, InOut_Block,
   FunctionCall_Block, WhileDo_Block, RepeatUntil_Block, ApplicationCommon, ForDo_Block,
   IfElse_Block, If_Block, Case_Block, Return_Block, Text_Block, Main_Block,
   CommonInterfaces, Folder_Block, XMLProcessor;

class function TBlockFactory.Create(ABranch: TBranch; ABlockType: TBlockType): TBlock;
begin
   result := nil;
   if ABranch <> nil then
   begin
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
end;

class function TBlockFactory.Create(ATag: IXMLElement; ABranch: TBranch; ATab: TBlockTabSheet = nil): TBlock;
var
   x,y,height,width,brx,bh,th,bry,fbry,fbrx,trh,flh,bid: integer;
   bt: TBlockType;
   attr: string;
begin
   result := nil;
   if ATag <> nil then
   begin
      attr := ATag.GetAttribute(BLOCK_TYPE_ATTR);
      th := StrToIntDef(attr, -1);
      if th = -1 then
         bt := TRttiEnumerationType.GetValue<TBlockType>(attr)
      else
         bt := TBlockType(th);
      x := ATag.GetAttribute('x').ToInteger;
      y := ATag.GetAttribute('y').ToInteger;
      height := ATag.GetAttribute('h').ToInteger;
      width := ATag.GetAttribute('w').ToInteger;
      brx := ATag.GetAttribute('brx').ToInteger;
      bh := ATag.GetAttribute('bh').ToInteger;
      bry := StrToIntDef(ATag.GetAttribute('bry'), 0);
      bid := StrToIntDef(ATag.GetAttribute(ID_ATTR), ID_INVALID);
      if ATab <> nil then
      begin
         if bt = blMain then
            result := TMainBlock.Create(ATab, x, y, width, height, bh, brx, bry, bid);
      end
      else if ABranch <> nil then
      begin
         case bt of
            blInstr:      result := TInstrBlock.Create(ABranch, x, y, width, height, bid);
            blMultiInstr: result := TMultiInstrBlock.Create(ABranch, x, y, width, height, bid);
            blInput:      result := TInputBlock.Create(ABranch, x, y, width, height, bid);
            blOutput:     result := TOutputBlock.Create(ABranch, x, y, width, height, bid);
            blFuncCall:   result := TFunctionCallBlock.Create(ABranch, x, y, width, height, bid);
            blWhile:      result := TWhileDoBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blRepeat:     result := TRepeatUntilBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blIf:         result := TIfBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blFor:        result := TForDoBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blCase:       result := TCaseBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blReturn:     result := TReturnBlock.Create(ABranch, x, y, width, height, bid);
            blText:       result := TTextBlock.Create(ABranch, x, y, width, height, bid);
            blFolder:     result := TFolderBlock.Create(ABranch, x, y, width, height, bh, brx, bry, bid);
            blIfElse:
            begin
               th := ATag.GetAttribute('th').ToInteger;
               fbrx := ATag.GetAttribute('fbrx').ToInteger;
               fbry := ATag.GetAttribute('fbry').ToInteger;
               trh := ATag.GetAttribute('trh').ToInteger;
               flh := ATag.GetAttribute('flh').ToInteger;
               result := TIfElseBlock.Create(ABranch, x, y, width, height, brx, fbrx, bh, th, bry, fbry, flh, trh, bid);
            end;
         end;
      end;
      if result <> nil then
         result.GetFromXML(ATag);
   end;
end;

end.
