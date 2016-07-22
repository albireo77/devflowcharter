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

unit ParserHelper;

interface

uses
   Base_Block, ApplicationCommon, DeclareList, StdCtrls, UserFunction;

type

   TParserHelper = class(TObject)
   private
      //class function DecodeDimension(const AType: integer): integer;
      class function DecodeType(const AType: integer): integer;
   public
      class function IsInLoop: boolean;
      class function ValidateUserFunctionParms(const AFunctionName: string; AParmList: array of integer): boolean;
      class function GetUserFunctionType(const AFunctionName: string): integer; overload;
      class function GetUserFunctionType: integer; overload;
      class function GetConstType(const AConstName: string): integer;
      class function GetEnumeratedType(const AValue: string): integer;
      class function GetTypeAsString(const AType: integer):string;
      class function GetType(const ATypeName: string; const ALangName: string = ''): integer;
      class function GetFieldType(const AVarName, AField: string): integer; overload;
      class function GetFieldType(const AType: integer; const AField: string): integer; overload;
      class function IsDeclared(const AIdentName: string): boolean;
      class function IsDuplicatedCase: boolean;
      class function GetConstValue(const AConstName: string): string;
      class function GetIdentInfo(const AIdentName: string): TIdentInfo;
      class function FindUserFunctionVarList(const ABlock: TBlock): TVarDeclareList;
      class procedure GetParameterInfo(const AHeader: TUserFunctionHeader; var AResult: TIdentInfo);
      class procedure GetVariableInfo(const AVarList: TVarDeclareList; var AResult: TIdentInfo);
      class function IsStructType(const AType: integer): boolean;
      class function IsEnumType(const AType: integer): boolean;
      class function IsIntegerType(const AType: integer): boolean;
      class function IsRealType(const AType: integer): boolean;
      class function IsNumericType(const AType: integer): boolean;
      class function IsPointerType(const AType: integer): boolean;
      class function IsBoolType(const AType: integer): boolean;
      class function IsStringType(const AType: integer): boolean;
      class function IsOtherType(const AType: integer): boolean;
      class function GetPointerType(const AType: integer): integer;
      class function GetOriginalType(const AType: integer): integer;
      class function GetForVarType: integer;
      class function GetCaseVarType: integer;
      class procedure InitIdentInfo(var AIdentInfo: TIdentInfo);
      class function GetVarInfo(const AVarName: string): TIdentInfo;
      class function IsArrayType(const AType: integer): boolean;
      class function AreTypesCompatible(const AType1, AType2: integer): boolean;
      class function GetSizeExpArrayAsString(const ATypeAsString: string; const ASizeAsString: string): string;
   end;

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

class procedure TParserHelper.InitIdentInfo(var AIdentInfo: TIdentInfo);
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

class function TParserHelper.GetForVarType: integer;
var
   lBlock: TBlock;
begin
   result := NOT_DEFINED;
   lBlock := TInfra.GetParsedBlock;
   if lBlock is TForDoBlock then
      result := GetVarInfo(TForDoBlock(lBlock).edtVariable.Text).TType;
end;

class function TParserHelper.GetCaseVarType: integer;
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
class function TParserHelper.IsInLoop: boolean;
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

class function TParserHelper.IsDuplicatedCase: boolean;
var
   lEdit: TCustomEdit;
begin
   result := false;
   lEdit := TInfra.GetParsedEdit;
   if (lEdit <> nil) and (lEdit.Parent is TCaseBlock) then
      result := TCaseBlock(lEdit.Parent).IsDuplicatedCase(lEdit);
end;

// get function type for active edit control
class function TParserHelper.GetUserFunctionType: integer;
var
   lHeader: TUserFunctionHeader;
begin
   result := NOT_DEFINED;
   lHeader := TInfra.GetFunctionHeader(TInfra.GetParsedBlock);
   if (lHeader <> nil) and (lHeader.Font.Color <> NOK_COLOR) then
      result := GetType(lHeader.cbType.Text);
end;

class function TParserHelper.GetUserFunctionType(const AFunctionName: string): integer;
var
   lFunction: TUserFunction;
begin
   result := NOT_DEFINED;
   if GProject <> nil then
   begin
      lFunction := GProject.GetUserFunction(AFunctionName);
      if (lFunction <> nil) and (lFunction.Header <> nil) and (lFunction.Header.Font.Color <> NOK_COLOR) then
         result := GetType(lFunction.Header.cbType.Text);
   end;
end;

// get type descriptor for given type string
class function TParserHelper.GetType(const ATypeName: string; const ALangName: string = ''): integer;
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
      if result = -1 then
         result := UNKNOWN_TYPE;
   end;
end;

class function TParserHelper.ValidateUserFunctionParms(const AFunctionName: string; AParmList: array of integer): boolean;
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

class function TParserHelper.GetEnumeratedType(const AValue: string): integer;
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

class function TParserHelper.FindUserFunctionVarList(const ABlock: TBlock): TVarDeclareList;
var
   lHeader: TUserFunctionHeader;
begin
   result := nil;
   lHeader := TInfra.GetFunctionHeader(ABlock);
   if lHeader <> nil then
      result := lHeader.LocalVars;
end;

class function TParserHelper.GetSizeExpArrayAsString(const ATypeAsString: string; const ASizeAsString: string): string;
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

class procedure TParserHelper.GetParameterInfo(const AHeader: TUserFunctionHeader; var AResult: TIdentInfo);
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

class procedure TParserHelper.GetVariableInfo(const AVarList: TVarDeclareList; var AResult: TIdentInfo);
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

class function TParserHelper.GetVarInfo(const AVarName: string): TIdentInfo;
var
   lBlock: TBlock;
begin
   InitIdentInfo(result);
   result.Ident := AVarName;
   lBlock := TInfra.GetParsedBlock;
   if lBlock <> nil then
   begin
      GetParameterInfo(TInfra.GetFunctionHeader(lBlock), result);
      if result.TType = NOT_DEFINED  then
         GetVariableInfo(FindUserFunctionVarList(lBlock), result);
   end;
   if (result.TType = NOT_DEFINED) and (GProject <> nil) then
      GetVariableInfo(GProject.GlobalVars, result);
end;

// get field type for given structural variable
class function TParserHelper.GetFieldType(const AVarName, AField: string): integer;
begin
   result := GetFieldType(GetVarInfo(AVarName).TypeOriginal, AField);
end;

// get field type for given structural type
class function TParserHelper.GetFieldType(const AType: integer; const AField: string): integer;
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
class function TParserHelper.GetConstType(const AConstName: string): integer;
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

class function TParserHelper.GetConstValue(const AConstName: string): string;
begin
   result := '';
   if (GProject <> nil) and (GProject.GlobalConsts <> nil) then
      result := GProject.GlobalConsts.GetValue(AConstName);
end;

class function TParserHelper.GetIdentInfo(const AIdentName: string): TIdentInfo;
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

class function TParserHelper.GetTypeAsString(const AType: integer):string;
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

class function TParserHelper.IsDeclared(const AIdentName: string): boolean;
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

class function TParserHelper.GetOriginalType(const AType: integer): integer;
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

class function TParserHelper.GetPointerType(const AType: integer): integer;
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

class function TParserHelper.IsIntegerType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.IntegerTypesSet);
end;

class function TParserHelper.IsRealType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.RealTypesSet);
end;

class function TParserHelper.IsNumericType(const AType: integer): boolean;
begin
   result := IsIntegerType(AType) or IsRealType(AType);
end;

class function TParserHelper.IsPointerType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.PointerTypesSet);
end;

class function TParserHelper.IsStructType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.StructTypesSet);
end;

class function TParserHelper.IsEnumType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.EnumTypesSet);
end;

class function TParserHelper.IsArrayType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.ArrayTypesSet);
end;

class function TParserHelper.IsBoolType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.BoolTypesSet);
end;

class function TParserHelper.IsStringType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.StringTypesSet);
end;

class function TParserHelper.IsOtherType(const AType: integer): boolean;
begin
   result := (GProject <> nil) and (AType in GProject.OtherTypesSet);
end;

class function TParserHelper.AreTypesCompatible(const AType1, AType2: integer): boolean;
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

{class function TParserHelper.DecodeDimension(const AType: integer): integer;
begin
   result := AType div DIMENSION_LEVEL_STEP;
end;}

class function TParserHelper.DecodeType(const AType: integer): integer;
begin
   result := AType mod DIMENSION_LEVEL_STEP;
end;

end.