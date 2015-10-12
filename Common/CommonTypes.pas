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
   Windows, SysUtils, Classes, Types, SynEditTypes;

type

   TCustomCursor = (crNormal, crIfElse, crFor, crRepeat, crWhile, crAssign, crMultiAssign,
                    crIf, crFuncCall, crInput, crOutput, crCase, crReturn, crText, crFolder);

   TErrorType = (errNone, errDeclare, errIO, errValidate, errConvert, errSyntax, errPrinter, errCompile, errImport, errGeneral);

   TBlockType = (blUnknown, blAssign, blMultAssign, blInput, blOutput, blFuncCall,
                 blWhile, blRepeat, blIf, blIfElse, blFor, blCase, blMain, blComment,
                 blReturn, blText, blFolder);

   TDataTypeKind = (tpInt, tpReal, tpString, tpBool, tpStruct, tpEnum, tpArray, tpPtr, tpOther);

   TParserMode = (prsNone, prsCondition, prsAssign, prsInput, prsOutput, prsFor, prsFuncCall,
                 prsCase, prsCaseValue, prsReturn, prsVarSize);

   TArrowPosition = (arrMiddle, arrEnd);

   TCodeRange = record
      FirstRow,
      LastRow: integer;
      IsFolded: boolean;
      Lines: TStrings;
{$IFDEF USE_CODEFOLDING}
      FoldRange: TSynEditFoldRange;
{$ENDIF}
   end;

   PNativeDataType = ^TNativeDataType;
   TNativeDataType = record
      Name: string;
      Kind: TDataTypeKind;
      OrigType: PNativeDataType;
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
   end;

   TIntegerTypes = 0..100;
   TIntegerTypesSet = set of TIntegerTypes;

   TRealTypes = 1..100;
   TRealTypesSet = set of TRealTypes;

   TBoolTypes = 2..100;
   TBoolTypesSet = set of TBoolTypes;

   TPointerTypes = 2..100;
   TPointerTypesSet = set of TPointerTypes;

   TStructTypes = 2..100;
   TStructTypesSet = set of TStructTypes;

   TEnumTypes = 2..100;
   TEnumTypesSet = set of TEnumTypes;

   TArrayTypes = 2..100;
   TArrayTypesSet = set of TArrayTypes;

   TStringTypes = 2..100;
   TStringTypesSet = set of TStringTypes;

   TOtherTypes = 2..100;
   TOtherTypesSet = set of TOtherTypes;

implementation

end.


