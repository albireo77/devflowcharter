{
   Copyright (C) 2011 The devFlowcharter project.
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



unit CommonTypes;

interface

uses
{$IFDEF USE_CODEFOLDING}
   SynEditCodeFolding,
{$ENDIF}
   System.Classes, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Generics.Defaults,
   Vcl.ComCtrls, WinApi.Messages, SynEditTypes, OmniXML;

type

   TCustomCursor = (crNormal, crIfElse, crFor, crRepeat, crWhile, crInstr, crMultiInstr,
                    crIf, crFuncCall, crInput, crOutput, crCase, crReturn, crText, crFolder);

   TErrorType = (errNone, errDeclare, errIO, errValidate, errConvert, errSyntax, errPrinter, errCompile, errImport, errGeneral);

   TBlockType = (blUnknown, blInstr, blMultiInstr, blInput, blOutput, blFuncCall,
                 blWhile, blRepeat, blIf, blIfElse, blFor, blCase, blMain, blComment,
                 blReturn, blText, blFolder);

   TDataTypeKind = (tpInt, tpReal, tpString, tpBool, tpRecord, tpEnum, tpArray, tpPtr, tpOther);

   TUserDataTypeKind = (dtInt, dtRecord, dtArray, dtReal, dtOther, dtEnum);

   TArrowPosition = (arrMiddle, arrEnd);

   TColorShape = (shpNone, shpEllipse, shpParallel, shpDiamond, shpRectangle, shpRoadSign, shpRoutine, shpFolder);

   TImportMode = (impSelectTab, impSelectPopup, impAll);

   TCodeRange = record
      FirstRow,
      LastRow: integer;
      IsFolded: boolean;
      Lines: TStrings;
{$IFDEF USE_CODEFOLDING}
      FoldRange: TSynEditFoldRange;
{$ENDIF}
      class function New: TCodeRange; static;
   end;

   PNativeDataType = ^TNativeDataType;
   TNativeDataType = record
      Name: string;
      Kind: TDataTypeKind;
      OrigType: PNativeDataType;
      IsGeneric: boolean;
      Lib: string;
   end;

   PNativeFunction = ^TNativeFunction;
   TNativeFunction = record
      Name,
      Brackets,
      Hint,
      Caption,
      Lib: string;
      BracketsCursorPos: integer;
   end;

   TErrWarnCount = record
      ErrorCount,
      WarningCount: integer;
   end;

   TChangeLine = record
      Text: string;
      Row,
      Col: integer;
      EditCaretXY: TBufferCoord;
      CodeRange: TCodeRange;
      class function New: TChangeLine; static;
   end;

   TFocusInfo = record
      LineText,
      SelText: string;
      Line,
      RelativeLine,
      SelStart: integer;
      FocusEdit: TCustomEdit;
      FocusEditForm: TForm;
      FocusEditCallBack: procedure(AEdit: TCustomEdit) of object;
      ActiveControl: TWinControl;
      class function New: TFocusInfo; static;
   end;

   T3Strings = record
     S0, S1, S2: string;
     class function Extract(const AFrom: string): T3Strings; static;
   end;

   TBlockInitParms = record
      x, y, h, w, brx, bh, bry, bid, th, fbrx, fbry, trh, flh: integer;
      bt: TBlockType;
      class function Extract(AFrom: IXMLElement): TBlockInitParms; static;
   end;

   PTypesSet = ^TTypesSet;
   TTypesSet = set of 0..255;

   TComponentComparer = class(TComparer<TComponent>)
      FCompareType: integer;
      constructor Create(ACompareType: integer);
      function Compare(const L, R: TComponent): integer; override;
   end;

   TTreeNodeWithFriend = class(TTreeNode)
   public
      Friend: TTreeNodeWithFriend;
      Offset: integer;
   end;

   TNameEdit = class(TEdit)
       protected
          procedure WMKillFocus(var msg: TWMKillFocus); message WM_KILLFOCUS;
    end;

implementation

uses
   System.SysUtils, System.Rtti, CommonInterfaces, ApplicationCommon, XMLProcessor;

constructor TComponentComparer.Create(ACompareType: integer);
begin
   inherited Create;
   FCompareType := ACompareType;
end;

function TComponentComparer.Compare(const L, R: TComponent): integer;
var
   c1, c2: IGenericComparable;
begin
   if (L = nil) and (R = nil) then
      result := 0
   else if not Supports(L, IGenericComparable, c1) then
      result := -41893
   else if not Supports(R, IGenericComparable, c2) then
      result := 41893
   else
      result := c1.GetCompareValue(FCompareType) - c2.GetCompareValue(FCompareType);
end;

class function TCodeRange.New: TCodeRange;
begin
   result.IsFolded := false;
   result.FirstRow := ROW_NOT_FOUND;
   result.LastRow := ROW_NOT_FOUND;
   result.Lines := nil;
{$IFDEF USE_CODEFOLDING}
   result.FoldRange := nil;
{$ENDIF}
end;

class function TChangeLine.New: TChangeLine;
begin
   result.Text := '';
   result.Row := ROW_NOT_FOUND;
   result.Col := 0;
   result.EditCaretXY := BufferCoord(0, 0);
   result.CodeRange := TCodeRange.New;
end;

class function TFocusInfo.New: TFocusInfo;
begin
   result.Line := -1;
   result.RelativeLine := 0;
   result.SelStart := -1;
   result.SelText := '';
   result.LineText := '';
   result.FocusEdit := nil;
   result.FocusEditForm := nil;
   result.FocusEditCallBack := nil;
   result.ActiveControl := nil;
end;

class function T3Strings.Extract(const AFrom: string): T3Strings;
var
   i: integer;
   tokens: TArray<string>;
begin
   result.S0 := '';
   result.S1 := '';
   result.S2 := '';
   tokens := AFrom.Split(['|'], 3);
   i := Length(tokens);
   if i > 0 then
      result.S0 := tokens[0];
   if i > 1 then
      result.S1 := tokens[1];
   if i > 2 then
      result.S2 := tokens[2];
end;

class function TBlockInitParms.Extract(AFrom: IXMLElement): TBlockInitParms;
var
   attr: string;
   at: integer;
begin
   attr := AFrom.GetAttribute(BLOCK_TYPE_ATTR);
   at := StrToIntDef(attr, -1);
   if at = -1 then
      result.bt := TRttiEnumerationType.GetValue<TBlockType>(attr)
   else
      result.bt := TBlockType(at);
   with TXMLProcessor do
   begin
      result.x :=    GetIntegerFromXMLNode(AFrom, 'x');
      result.y :=    GetIntegerFromXMLNode(AFrom, 'y');
      result.h :=    GetIntegerFromXMLNode(AFrom, 'h');
      result.w :=    GetIntegerFromXMLNode(AFrom, 'w');
      result.brx :=  GetIntegerFromXMLNode(AFrom, 'brx');
      result.bh :=   GetIntegerFromXMLNode(AFrom, 'bh');
      result.bry :=  GetIntegerFromXMLNode(AFrom, 'bry');
      result.th :=   GetIntegerFromXMLNode(AFrom, 'th');
      result.fbrx := GetIntegerFromXMLNode(AFrom, 'fbrx');
      result.fbry := GetIntegerFromXMLNode(AFrom, 'fbry');
      result.trh :=  GetIntegerFromXMLNode(AFrom, 'trh');
      result.flh :=  GetIntegerFromXMLNode(AFrom, 'flh');
      result.bid :=  GetIntegerFromXMLNode(AFrom, ID_ATTR, ID_INVALID);
   end;
end;

procedure TNameEdit.WMKillFocus(var msg: TWMKillFocus);
begin
   inherited;
   Change;
end;

end.


