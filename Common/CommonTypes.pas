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
   Vcl.ComCtrls, SynEditTypes;

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

implementation

uses
   System.SysUtils, CommonInterfaces, ApplicationCommon;

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

end.


