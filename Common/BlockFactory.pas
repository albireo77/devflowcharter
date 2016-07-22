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
  Windows, SysUtils, Classes, Graphics, StdCtrls, Base_Block, CommonTypes, OmniXML,
  BlockTabSheet;

type

   TBlockFactory = class(TObject)
   public
      class function Create(const ABranch: TBranch; const ABlockType: TBlockType): TBlock; overload;
      class function Create(const ATag: IXMLElement; const ABranch: TBranch; const ATab: TBlockTabSheet = nil): TBlock; overload;
   end;

implementation

uses
   Assign_Block, MulAssign_Block, InOut_Block, FunctionCall_Block, WhileDo_Block, RepeatUntil_Block, ApplicationCommon,
   ForDo_Block, IfElse_Block, If_Block, Case_Block, Return_Block, Text_Block, Main_Block, CommonInterfaces, Folder_Block;

class function TBlockFactory.Create(const ABranch: TBranch; const ABlockType: TBlockType): TBlock;
begin
   result := nil;
   if ABranch <> nil then
   begin
      case ABlockType of
         blAssign:     result := TAssignBlock.Create(ABranch);
         blMultAssign: result := TMultiAssignBlock.Create(ABranch);
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

class function TBlockFactory.Create(const ATag: IXMLElement; const ABranch: TBranch; const ATab: TBlockTabSheet = nil): TBlock;
var
   left,top,height,width,brx,bh,th,bry,fbry,fbrx,trh,flh,bid,bt: integer;
begin
   result := nil;
   if ATag <> nil then
   begin
      bt := StrToInt(ATag.GetAttribute(BLOCK_TYPE_ATTR));
      left := StrToInt(ATag.GetAttribute('x'));
      top := StrToInt(ATag.GetAttribute('y'));
      height := StrToInt(ATag.GetAttribute('h'));
      width := StrToInt(ATag.GetAttribute('w'));
      brx := StrToInt(ATag.GetAttribute('brx'));
      bh := StrToInt(ATag.GetAttribute('bh'));
      bry := StrToIntDef(ATag.GetAttribute('bry'), 0);
      bid := StrToIntDef(ATag.GetAttribute(ID_ATTR), ID_INVALID);
      if ATab <> nil then
      begin
         if TBlockType(bt) = blMain then
            result := TMainBlock.Create(ATab, left, top, width, height, bh, brx, bry, bid);
      end
      else if ABranch <> nil then
      begin
         case TBlockType(bt) of
            blAssign:     result := TAssignBlock.Create(ABranch, left, top, width, height, bid);
            blMultAssign: result := TMultiAssignBlock.Create(ABranch, left, top, width, height, bid);
            blInput:      result := TInputBlock.Create(ABranch, left, top, width, height, bid);
            blOutput:     result := TOutputBlock.Create(ABranch, left, top, width, height, bid);
            blFuncCall:   result := TFunctionCallBlock.Create(ABranch, left, top, width, height, bid);
            blWhile:      result := TWhileDoBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blRepeat:     result := TRepeatUntilBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blIf:         result := TIfBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blFor:        result := TForDoBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blCase:       result := TCaseBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blReturn:     result := TReturnBlock.Create(ABranch, left, top, width, height, bid);
            blText:       result := TTextBlock.Create(ABranch, left, top, width, height, bid);
            blFolder:     result := TFolderBlock.Create(ABranch, left, top, width, height, bh, brx, bry, bid);
            blIfElse:
            begin
               th := StrToInt(ATag.GetAttribute('th'));
               fbrx := StrToInt(ATag.GetAttribute('fbrx'));
               fbry := StrToInt(ATag.GetAttribute('fbry'));
               trh := StrToInt(ATag.GetAttribute('trh'));
               flh := StrToInt(ATag.GetAttribute('flh'));
               result := TIfElseBlock.Create(ABranch, left, top, width, height, brx, fbrx, bh, th, bry, fbry, flh, trh, bid);
            end;
         end;
      end;
      if result <> nil then
         result.GetFromXML(ATag);
   end;
end;

end.
