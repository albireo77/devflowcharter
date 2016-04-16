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



{ This unit contains routines used mainly by parsers }

unit ParserCommon;

interface

uses
   Base_Block, ApplicationCommon, DeclareList, StdCtrls, UserFunction;

function IsInLoop: boolean;
function ValidateUserFunctionParms(const AFunctionName: string; AParmList: array of integer): boolean;
function GetUserFunctionType(const AFunctionName: string): integer;
function GetConstType(const AConstName: string): integer;
function GetEnumeratedType(const AValue: string): integer;
function GetTypeAsString(const AType: integer):string;
function GetType(const ATypeName: string; const ALangName: string = ''): integer;
function GetFieldType(const AVarName, AField: string): integer; overload;
function GetFieldType(const AType: integer; const AField: string): integer; overload;
function IsDeclared(const AIdentName: string): boolean;
function IsDuplicatedCase: boolean;
function GetConstValue(const AConstName: string): string;
function GetIdentInfo(const AIdentName: string): TIdentInfo;
function GetFunctionType: integer;
function FindUserFunctionVarList(const ABlock: TBlock): TVarDeclareList;
function FindUserFunctionHeader(const ABlock: TBlock): TUserFunctionHeader;
procedure GetParameterInfo(const AHeader: TUserFunctionHeader; var AResult: TIdentInfo);
procedure GetVariableInfo(const AVarList: TVarDeclareList; var AResult: TIdentInfo);
function IsStructType(const AType: integer): boolean;
function IsEnumType(const AType: integer): boolean;
function IsIntegerType(const AType: integer): boolean;
function IsRealType(const AType: integer): boolean;
function IsNumericType(const AType: integer): boolean;
function IsPointerType(const AType: integer): boolean;
function IsBoolType(const AType: integer): boolean;
function IsStringType(const AType: integer): boolean;
function IsOtherType(const AType: integer): boolean;
function GetPointerType(const AType: integer): integer;
function GetOriginalType(const AType: integer): integer;
function GetForVarType: integer;
function GetCaseVarType: integer;
procedure InitIdentInfo(var AIdentInfo: TIdentInfo);
function GetVarInfo(const AVarName: string): TIdentInfo;
function IsArrayType(const AType: integer): boolean;
function AreTypesCompatible(const AType1, AType2: integer): boolean;

const

   // generic datatype descriptors
   GENERIC_INT_TYPE  = 0;
   GENERIC_PTR_TYPE  = 99;

   DIMENSION_LEVEL_STEP = 100;

   INCORRECT_SIZE = 0;
   UNKNOWN_TYPE   = -2;
   NOT_DEFINED    = -3;

   VARIABLE     = 1;
   VARRAY       = 2;
   CONSTANT     = 3;
   ROUTINE_ID   = 4;
   ENUM_VALUE   = 5;
   UNKNOWN      = -1;

   GLOBAL    = -10;
   LOCAL     = -11;
   PARAMETER = -12;
   
implementation

uses
   Controls, SysUtils, Forms, UserDataType, Statement, CommonTypes, Case_Block,
   Main_Block, Grids, Graphics, ForDo_Block, LangDefinition, CommonInterfaces;

procedure InitIdentInfo(var AIdentInfo: TIdentInfo);
begin
   with AIdentInfo do
   begin
      Ident := '';
      IdentType := UNKNOWN;
      TType := NOT_DEFINED;
      TypeOriginal := NOT_DEFINED;
      TypePointer := NOT_DEFINED;
      Value := '';
      Size := INCORRECT_SIZE;
      DimensCount := 0;
      SizeAsString := IntToStr(INCORRECT_SIZE);
      SizeExpArrayAsString := '';
      TypeAsString := i18Manager.GetString('Unknown');
      TypeOriginalAsString := TypeAsString;
      IsInteger := false;
      IsReal := false;
      IsNumeric := false;
      IsPointer := false;
      IsStruct := false;
      IsEnum := false;
      Scope := GLOBAL;
   end;
end;

function GetForVarType: integer;
var
   lBlock: TBlock;
begin
   result := NOT_DEFINED;
   lBlock := TInfra.GetParsedBlock;
   if lBlock is TForDoBlock then
      result := GetVarInfo(TForDoBlock(lBlock).edtVariable.Text).TType;
end;

function GetCaseVarType: integer;
var
   lBlock: TBlock;
begin
   result := NOT_DEFINED;
   lBlock := TInfra.GetParsedBlock;
   if lBlock is TCaseBlock then
   begin
      result := GetVarInfo(TCaseBlock(lBlock).GetTextControl.Text).TType;
      if result = NOT_DEFINED then
         result := GENERIC_INT_TYPE;
   end;
end;

// check if active statement is inside loop
function IsInLoop: boolean;
var
   lBlock: TBlock;
begin
   result := false;
   lBlock := TInfra.GetParsedBlock;
   while lBlock <> nil do
   begin
      if lBlock.BType in LOOP_BLOCKS then
      begin
         result := true;
         break;
      end;
      lBlock := lBlock.ParentBlock;
   end;
end;

function IsDuplicatedCase: boolean;
var
   lEdit: TCustomEdit;
begin
   result := false;
   lEdit := TInfra.GetParsedEdit;
   if (lEdit <> nil) and (lEdit.Parent is TCaseBlock) then
      result := TCaseBlock(lEdit.Parent).IsDuplicatedCase(lEdit);
end;

function GetFunctionType: integer;
var
   lObject: TObject;
   lFunction: TUserFunction;
   lBlock: TBlock;
begin
   result := NOT_DEFINED;
   lBlock := TInfra.GetParsedBlock;
   if lBlock <> nil then
   begin
      lObject := TMainBlock(lBlock.TopParentBlock).OwnerFunction;
      if lObject is TUserFunction then
      begin
         lFunction := TUserFunction(lObject);
         if (lFunction.Header <> nil) and (lFunction.Header.Font.Color <> NOK_COLOR) then
            result := lFunction.Header.cbType.ItemIndex-1;
      end;
   end;
end;

// get type descriptor for given type string
function GetType(const ATypeName: string; const ALangName: string = ''): integer;
var
   i: integer;
   lLangDef: TLangDefinition;
begin
   result := UNKNOWN_TYPE;
   if ALangName = '' then
      lLangDef := GInfra.CurrentLang
   else
      lLangDef := GInfra.GetLangDefinition(ALangName);
   if lLangDef <> nil then
   begin
      for i := 0 to High(lLangDef.NativeDataTypes) do
      begin
         if TInfra.SameStrings(lLangDef.NativeDataTypes[i].Name, ATypeName) then
         begin
            result := i;
            break;
         end;
      end;
   end;
   if (result = UNKNOWN_TYPE) and (GProject <> nil) and (GProject.GlobalVars <> nil) then
   begin
      result := GProject.GlobalVars.cbType.Items.IndexOf(ATypeName);
      if result = -1 then result := UNKNOWN_TYPE;
   end;
end;

function DecodeDimension(const AType: integer): integer;
begin
   result := AType div DIMENSION_LEVEL_STEP;
end;

function DecodeType(const AType: integer): integer;
begin
   result := AType mod DIMENSION_LEVEL_STEP;
end;

function ValidateUserFunctionParms(const AFunctionName: string; AParmList: array of integer): boolean;
var
   i, lParmType, lCurrentType: integer;
   lFunction: TUserFunction;
   lIsArray: boolean;
   lParm: TParameter;
   iterp: IIterator;
begin
   result := false;
   if GProject <> nil then
   begin
      lFunction := GProject.GetUserFunction(AFunctionName);
      if (lFunction <> nil) and (lFunction.Header <> nil) and (Length(AParmList) = lFunction.Header.ParameterCount) then
      begin
         i := 0;
         iterp := lFunction.Header.GetParameterIterator;
         while iterp.HasNext do
         begin
            lParm := TParameter(iterp.Next);
            lParmType := GetType(lParm.cbType.Text);
            lCurrentType := AParmList[i];
            lIsArray := lCurrentType >= DIMENSION_LEVEL_STEP;
            if lIsArray then
               lCurrentType := DecodeType(lCurrentType);
            if lIsArray then
            begin
               if (lCurrentType <> lParmType) or (not lParm.chkTable.Checked and not IsArrayType(lParmType)) then
                  exit;
            end
            else if (not AreTypesCompatible(lParmType, lCurrentType)) or lParm.chkTable.Checked then
               exit;
            i := i + 1;
         end;
         result := true;
      end;
   end;
end;

function GetUserFunctionType(const AFunctionName: string): integer;
var
   lFunction: TUserFunction;
begin
   result := NOT_DEFINED;
   if GProject <> nil then
   begin
      lFunction := GProject.GetUserFunction(AFunctionName);
      if (lFunction <> nil) and (lFunction.Header <> nil) then
         result := GetType(lFunction.Header.cbType.Text);
   end;
end;

function GetEnumeratedType(const AValue: string): integer;
var
   iterf, iter: IIterator;
   lDataType: TUserDataType;
   lField: TField;
begin
   result := NOT_DEFINED;
   if GProject <> nil then
   begin
      iter := GProject.GetUserDataTypes;
      while iter.HasNext do
      begin
         lDataType := TUserDataType(iter.Next);
         if lDataType.Active and (lDataType.Font.Color <> NOK_COLOR) and lDataType.rbEnum.Checked then
         begin
            iterf := lDataType.GetFieldIterator;
            while iterf.HasNext do
            begin
               lField := TField(iterf.Next);
               if (lField.edtName.Font.Color <> NOK_COLOR) and TInfra.SameStrings(AValue, Trim(lField.edtName.Text)) then
               begin
                  result := GetType(lDataType.edtName.Text);
                  break;
               end;
            end;
         end;
         if result <> NOT_DEFINED then break;
      end;
   end;
end;

function FindUserFunctionHeader(const ABlock: TBlock): TUserFunctionHeader;
var
   lMBlock: TMainBlock;
begin
   result := nil;
   if ABlock <> nil then
   begin
      lMBlock := TMainBlock(ABlock.TopParentBlock);
      if lMBlock.OwnerFunction is TUserFunction then
         result := TUserFunction(lMBlock.OwnerFunction).Header;
   end;
end;

function FindUserFunctionVarList(const ABlock: TBlock): TVarDeclareList;
var
   lHeader: TUserFunctionHeader;
begin
   result := nil;
   lHeader := FindUserFunctionHeader(ABlock);
   if lHeader <> nil then
      result := lHeader.LocalVars;
end;

function GetSizeExpArrayAsString(const ATypeAsString: string; const ASizeAsString: string): string;
var
   lDataType: TUserDataType;
   lSize: string;
begin
   result := ASizeAsString;
   if GProject <> nil then
   begin
      lDataType := GProject.GetUserDataType(ATypeAsString);
      if lDataType <> nil then
      begin
         lSize := lDataType.GetDimensions;
         if lSize <> '' then
         begin
            if result <> '1' then
               result := result + ',' + lSize
            else
               result := lSize;
         end;
      end;
   end;
end;

procedure GetParameterInfo(const AHeader: TUserFunctionHeader; var AResult: TIdentInfo);
var
   iter: IIterator;
   lParam: TParameter;
   lDataType: TUserDataType;
begin
   if AHeader <> nil then
   begin
      iter := AHeader.GetParameterIterator;
      while iter.HasNext do
      begin
         lParam := TParameter(iter.Next);
         if (lParam.edtName.Font.Color <> NOK_COLOR) and TInfra.SameStrings(AResult.Ident, Trim(lParam.edtName.Text)) then
         begin
            with AResult do
            begin
               TypeAsString := lParam.cbType.Text;
               TType := GetType(TypeAsString);
               TypeOriginal := GetOriginalType(TType);
               TypeOriginalAsString := GetTypeAsString(TypeOriginal);
               TypePointer := GetPointerType(TType);
               IsInteger := IsIntegerType(TType);
               IsReal := IsRealType(TType);
               IsNumeric := IsInteger or IsReal;
               IsStruct := IsStructType(TType);
               IsEnum := IsEnumType(TType);
               IsPointer := IsPointerType(TType);
               Scope := PARAMETER;
               if lParam.chkTable.Checked then
               begin
                  Size := 0;
                  SizeAsString := '';
                  DimensCount := 1;
               end
               else
               begin
                  Size := 1;
                  SizeAsString := '1';
                  DimensCount := 0;
               end;
               if GProject <> nil then
               begin
                  lDataType := GProject.GetUserDataType(lParam.cbType.Text);
                  if lDataType <> nil then
                     Inc(DimensCount, lDataType.GetDimensionCount);
               end;
               if DimensCount > 0 then
               begin
                  IdentType := VARRAY;
                  SizeExpArrayAsString := GetSizeExpArrayAsString(TypeAsString, SizeAsString);
               end
               else
                  IdentType := VARIABLE;
            end;
            break;
         end;
      end;
   end;
end;

procedure GetVariableInfo(const AVarList: TVarDeclareList; var AResult: TIdentInfo);
var
   i: integer;
begin
   if AVarList <> nil then
   begin
      i := AVarList.sgList.Cols[VAR_NAME_COL].IndexOf(AResult.Ident);
      if i > 0 then
      begin
         with AResult do
         begin
            TypeAsString := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            SizeAsString := AVarList.sgList.Cells[VAR_SIZE_COL, i];
            Size := StrToIntDef(SizeAsString, INCORRECT_SIZE);
            TType := GetType(TypeAsString);
            DimensCount := AVarList.GetDimensionCount(Ident, true);
            TypeOriginal := GetOriginalType(TType);
            TypeOriginalAsString := GetTypeAsString(TypeOriginal);
            if DimensCount = 0 then
               IdentType := VARIABLE
            else
            begin
               IdentType := VARRAY;
               SizeExpArrayAsString := GetSizeExpArrayAsString(TypeAsString, SizeAsString);
            end;
            TypePointer := GetPointerType(TType);
            IsInteger := IsIntegerType(TType);
            IsReal := IsRealType(TType);
            IsNumeric := IsInteger or IsReal;
            IsStruct := IsStructType(TType);
            IsEnum := IsEnumType(TType);
            IsPointer := IsPointerType(TType);
            if (GProject <> nil) and (GProject.GlobalVars <> AVarList) then
               Scope := LOCAL;
         end;
      end;
   end;
end;


function GetVarInfo(const AVarName: string): TIdentInfo;
var
   lBlock: TBlock;
begin
   InitIdentInfo(result);
   result.Ident := AVarName;
   lBlock := TInfra.GetParsedBlock;
   if lBlock <> nil then
   begin
      GetParameterInfo(FindUserFunctionHeader(lBlock), result);
      if result.TType = NOT_DEFINED  then
         GetVariableInfo(FindUserFunctionVarList(lBlock), result);
   end;
   if (result.TType = NOT_DEFINED) and (GProject <> nil) then
      GetVariableInfo(GProject.GlobalVars, result);
end;

// get field type for given structural variable
function GetFieldType(const AVarName, AField: string): integer;
begin
   result := GetFieldType(GetVarInfo(AVarName).TypeOriginal, AField);
end;

// get field type for given structural type
function GetFieldType(const AType: integer; const AField: string): integer;
var
   lTypeString: string;
   lDataType: TUserDataType;
   lField: TField;
   iterf: IIterator;
begin
   result := NOT_DEFINED;
   lTypeString := GetTypeAsString(AType);
   if (lTypeString <> i18Manager.GetString('Unknown')) and (GProject <> nil) then
   begin
      lDataType := GProject.GetUserDataType(lTypeString);
      if (lDataType <> nil) and lDataType.rbStruct.Checked then
      begin
         iterf := lDataType.GetFieldIterator;
         while iterf.HasNext do
         begin
            lField := TField(iterf.Next);
            if (lField.edtName.Font.Color <> NOK_COLOR) and TInfra.SameStrings(Trim(lField.edtName.Text), AField) then
            begin
               result := GetType(lField.cbType.Text);
               break;
            end;
         end;
      end;
   end;
end;

// interface for _GetConstType template function
function GetConstType(const AConstName: string): integer;
var
   lValue: string;
begin
   result := NOT_DEFINED;
   lValue := GetConstValue(AConstName);
   if lValue <> '' then
   begin
      if Assigned(GInfra.CurrentLang.GetLiteralType) then
         result := GInfra.CurrentLang.GetLiteralType(lValue)
      else if Assigned(GInfra.DummyLang.GetLiteralType) then
         result := GInfra.DummyLang.GetLiteralType(lValue)
   end;
end;

function GetConstValue(const AConstName: string): string;
begin
   result := '';
   if (GProject <> nil) and (GProject.GlobalConsts <> nil) then
      result := GProject.GlobalConsts.GetValue(AConstName);
end;

function GetIdentInfo(const AIdentName: string): TIdentInfo;
begin
   result := GetVarInfo(AIdentName);
   if result.IdentType = UNKNOWN then
   begin
      result.TType := GetConstType(result.Ident);
      if result.TType <> NOT_DEFINED then
      begin
         result.IdentType := CONSTANT;
         result.Value := GetConstValue(result.Ident);
      end
      else
      begin
         result.TType := GetUserFunctionType(result.Ident);
         if result.TType <> NOT_DEFINED then
            result.IdentType := ROUTINE_ID
         else
         begin
            result.TType := GetEnumeratedType(result.Ident);
            if result.TType <> NOT_DEFINED then
               result.IdentType := ENUM_VALUE;
         end;
      end;
      if result.IdentType <> UNKNOWN then
      begin
         with result do
         begin
            TypeAsString := GetTypeAsString(TType);
            IsStruct := IsStructType(TType);
            IsEnum := IsEnumType(TType);
            IsReal := IsRealType(TType);
            IsInteger := IsIntegerType(TType);
            IsNumeric := IsReal or IsInteger;
            IsPointer := IsPointerType(TType);
            TypeOriginal := GetOriginalType(TType);
            TypeOriginalAsString := GetTypeAsString(TypeOriginal);
            TypePointer := GetPointerType(TType);
         end;
      end;
   end;
end;

function GetTypeAsString(const AType: integer):string;
begin
   if (GProject <> nil) and (GProject.GlobalVars <> nil) and (AType >= 0) and (AType < GProject.GlobalVars.cbType.Items.Count) then
      result := GProject.GlobalVars.cbType.Items[AType]
   else if AType = GENERIC_PTR_TYPE then
      result := 'pointer'
   else if AType >= DIMENSION_LEVEL_STEP then
      result := 'array'
   else
      result := i18Manager.GetString('Unknown');
end;

function IsDeclared(const AIdentName: string): boolean;
var
   iterf, iter: IIterator;
   lDataType: TUserDataType;
   lField: TField;
begin
   result := true;
   if GProject <> nil then
   begin
      iter := GProject.GetUserDataTypes;
      while iter.HasNext do
      begin
         lDataType := TUserDataType(iter.Next);
         if lDataType.Active and (lDataType.Font.Color <> NOK_COLOR) then
         begin
            iterf := lDataType.GetFieldIterator;
            while iterf.HasNext do
            begin
               lField := TField(iterf.Next);
               if (lField.edtName.Font.Color <> NOK_COLOR) and TInfra.SameStrings(AIdentName, Trim(lField.edtName.Text)) then exit;
            end;
         end;
      end;
   end;
   if (GetVarInfo(AIdentName).TType = NOT_DEFINED) and (GetConstType(AIdentName) = NOT_DEFINED) and
      (GetUserFunctionType(AIdentName) = NOT_DEFINED) then result := false;
end;

function GetOriginalType(const AType: integer): integer;
var
   lUserDataType: TUserDataType;
   lNativeDataType: PNativeDataType;
   lTypeName: string;
begin
   result := AType;
   if GProject <> nil then
   begin
      lTypeName := GetTypeAsString(AType);
      lNativeDataType := GInfra.GetNativeDataType(lTypeName);
      lUserDataType := GProject.GetUserDataType(lTypeName);
      if lNativeDataType <> nil then
         result := GetType(lNativeDataType.OrigType.Name)
      else if (lUserDataType <> nil) and (lUserDataType.GetDimensionCount > 0) then
         result := lUserDataType.GetOriginalType
      else if Assigned(GInfra.CurrentLang.GetOriginalType) then
         result := GetType(GInfra.CurrentLang.GetOriginalType(lTypeName));
   end;
end;

function GetPointerType(const AType: integer): integer;
var
   lLang: TLangDefinition;
begin
   result := UNKNOWN_TYPE;
   lLang := nil;
   if Assigned(GInfra.CurrentLang.GetPointerTypeName) then
      lLang := GInfra.CurrentLang
   else if Assigned(GInfra.DummyLang.GetPointerTypeName) then
      lLang := GInfra.DummyLang;
   if lLang <> nil then
      result := GetType(lLang.GetPointerTypeName(GetTypeAsString(AType)))
end;

function IsIntegerType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.IntegerTypesSet);
end;

function IsRealType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.RealTypesSet);
end;

function IsNumericType(const AType: integer): boolean;
begin
   result := IsIntegerType(AType) or IsRealType(AType);
end;

function IsPointerType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.PointerTypesSet);
end;

function IsStructType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.StructTypesSet);
end;

function IsEnumType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.EnumTypesSet);
end;

function IsArrayType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.ArrayTypesSet);
end;

function IsBoolType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.BoolTypesSet);
end;

function IsStringType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.StringTypesSet);
end;

function IsOtherType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.OtherTypesSet);
end;

function AreTypesCompatible(const AType1, AType2: integer): boolean;
var
   lIsRealType1, lIsIntegerType2: boolean;
begin
   result := AType1 = AType2;
   if not result then
   begin
      lIsRealType1 := IsRealType(AType1);
      result := lIsRealType1 and IsRealType(AType2);
      if not result then
      begin
         lIsIntegerType2 := IsIntegerType(AType2);
         result := lIsRealType1 and lIsIntegerType2;
         if not result then
         begin
            result := IsIntegerType(AType1) and lIsIntegerType2;
            if not result then
            begin
               result := IsBoolType(AType1) and IsBoolType(AType2);
               if not result then
               begin
                  result := IsStringType(AType1) and IsStringType(AType2);
                  if not result then
                     result := (AType2 = GENERIC_PTR_TYPE) and IsPointerType(AType1);
               end;
            end;
         end;
      end;
      if not result and Assigned(GInfra.CurrentLang.AreTypesCompatible) then
         result := GInfra.CurrentLang.AreTypesCompatible(AType1, AType2);
   end;
end;

end.