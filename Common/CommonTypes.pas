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
   Windows, SysUtils, Classes, Types;

type

   TCustomCursor = (crNormal, crIfElse, crFor, crRepeat, crWhile, crAssign, crMultAssign,
                    crIf, crFuncCall, crInput, crOutput, crCase, crReturn);

   TErrorType = (erNone, erDeclare, erIO, erValidate, erConvert, erSyntax, erPrinter, erCompile, erImport, erGeneral);

   TBlockType = (blUnknown, blAssign, blMultAssign, blInput, blOutput, blFuncCall,
                 blWhile, blRepeat, blIf, blIfElse, blFor, blCase, blMain, blComment,
                 blReturn, blText);

   TDataTypeKind = (tpInt, tpReal, tpString, tpBool, tpStruct, tpEnum, tpArray, tpPtr, tpOther);

   TParserMode = (prNone, prCondition, prAssign, prInput, prOutput, prFor, prFuncCall,
                 prCase, prCaseValue, prReturn, prVarSize);

   TArrowPlace = (apMiddle, apEnd);

   TCodeRange = record
      FirstLineIdx,
      LastLineIdx: integer;
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

   TPlaceHolderLine = record
      Text: string;
      Index: integer;
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


