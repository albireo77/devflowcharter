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

unit Dummy_Template;

interface

implementation

uses
   Base_Block, LangDefinition, UserFunction, SysUtils, StrUtils, DeclareList, CommonInterfaces,
   UserDataType, FastcodeAnsiStringReplaceUnit, Classes, ApplicationCommon, ParserHelper;

procedure Dummy_UserDataTypesSectionGenerator(ALines: TStringList);
var
   lDataType: TUserDataType;
   lField: TField;
   lName, lSizeStr, lRecord, lEnum: string;
   iterf, iter: IIterator;
   b, lType: integer;
   lLang: TLangDefinition;
   lDataTypeList, lDataTypesTemplate, lRecordTemplate, lFieldList, lEnumTemplate: TStringList;
   lDataTypeStr, lFieldStr, lValueStr, lValueStr2: string;
begin
   lLang := GInfra.CurrentLang;
   if lLang.DataTypesTemplate <> '' then
   begin
      lDataTypeList := TStringList.Create;
      try
         iter := GProject.GetUserDataTypes;
         while iter.HasNext do
         begin
            lDataType := TUserDataType(iter.Next);
            lName := lDataType.GetName;
            if (lName <> '') and not lDataType.chkExtDeclare.Checked then
            begin
               iterf := lDataType.GetFieldIterator;
               if lDataType.rbInt.Checked then
               begin
                  if lLang.DataTypeIntMask <> '' then
                  begin
                     lDataTypeStr := FastCodeAnsiStringReplace(llang.DataTypeIntMask, '%s1', lName);
                     lDataTypeList.AddObject(lDataTypeStr, lDataType);
                  end;
               end
               else if lDataType.rbReal.Checked then
               begin
                  if lLang.DataTypeRealMask <> '' then
                  begin
                     lDataTypeStr := FastCodeAnsiStringReplace(llang.DataTypeRealMask, '%s1', lName);
                     lDataTypeList.AddObject(lDataTypeStr, lDataType);
                  end;
               end
               else if lDataType.rbOther.Checked then
               begin
                  if lLang.DataTypeOtherMask <> '' then
                  begin
                     lDataTypeStr := FastCodeAnsiStringReplace(llang.DataTypeOtherMask, '%s1', lName);
                     lValueStr := '';
                     if iterf.HasNext then
                        lValueStr := Trim(TField(iterf.Next).edtName.Text);
                     lDataTypeStr := FastCodeAnsiStringReplace(lDataTypeStr, '%s2', lValueStr);
                     lDataTypeList.AddObject(lDataTypeStr, lDataType);
                  end;
               end
               else if lDataType.rbArray.Checked then
               begin
                  if lLang.DataTypeArrayMask <> '' then
                  begin
                     lDataTypeStr := FastCodeAnsiStringReplace(llang.DataTypeArrayMask, '%s1', lName);
                     lValueStr := '';
                     lValueStr2 := '';
                     if iterf.HasNext then
                     begin
                        lField := TField(iterf.Next);
                        lValueStr := lField.cbType.Text;
                        lValueStr2 := lLang.GetArraySizes(lField.edtSize);
                     end;
                     lDataTypeStr := FastCodeAnsiStringReplace(lDataTypeStr, '%s2', lValueStr);
                     lDataTypeStr := FastCodeAnsiStringReplace(lDataTypeStr, '%s3', lValueStr2);
                     lDataTypeList.AddObject(lDataTypeStr, lDataType);
                  end;
               end
               else if lDataType.rbStruct.Checked then
               begin
                  if lLang.DataTypeRecordTemplate <> '' then
                  begin
                     lRecordTemplate := TStringList.Create;
                     try
                        lRecordTemplate.Text := FastCodeAnsiStringReplace(lLang.DataTypeRecordTemplate, '%s1', lName);
                        lFieldList := TStringList.Create;
                        try
                           while iterf.HasNext do
                           begin
                              lField := TField(iterf.Next);
                              lSizeStr := lLang.GetArraySizes(lField.edtSize);
                              if lSizeStr <> '' then
                                 lFieldStr := lLang.DataTypeRecordFieldArrayMask
                              else
                                 lFieldStr := lLang.DataTypeRecordFieldMask;
                              lFieldStr := FastCodeAnsiStringReplace(lFieldStr, '%s1', Trim(lField.edtName.Text));
                              lFieldStr := FastCodeAnsiStringReplace(lFieldStr, '%s2', lField.cbType.Text);
                              if lSizeStr <> '' then
                                 lFieldStr := FastCodeAnsiStringReplace(lFieldStr, '%s3', lSizeStr);
                              lRecord := '';
                              lEnum := '';
                              lType := TParserHelper.GetType(lField.cbType.Text);
                              if TParserHelper.IsStructType(lType) then
                                 lRecord := lLang.FunctionHeaderArgsEntryRecord
                              else if TParserHelper.IsEnumType(lType) then
                                 lEnum := lLang.FunctionHeaderArgsEntryEnum;
                              lFieldStr := FastCodeAnsiStringReplace(lFieldStr, '%s4', lRecord);
                              lFieldStr := FastCodeAnsiStringReplace(lFieldStr, '%s5', lEnum);
                              lFieldList.Add(lFieldStr);
                           end;
                           TInfra.InsertTemplateLines(lRecordTemplate, '%s2', lFieldList);
                        finally
                           lFieldList.Free;
                        end;
                        for b := 0 to lRecordTemplate.Count-1 do
                           lDataTypeList.AddObject(lRecordTemplate[b], lDataType)
                     finally
                        lRecordTemplate.Free;
                     end;
                  end;
               end
               else if lDataType.rbEnum.Checked then
               begin
                  if lLang.DataTypeEnumTemplate <> '' then
                  begin
                     lEnumTemplate := TStringList.Create;
                     try
                        lEnumTemplate.Text := FastCodeAnsiStringReplace(lLang.DataTypeEnumTemplate, '%s1', lName);
                        lValueStr := '';
                        while iterf.HasNext do
                        begin
                           lField := TField(iterf.Next);
                           lValueStr := lValueStr + Format(lLang.DataTypeEnumEntryList, [Trim(lField.edtName.Text)]);
                        end;
                        lValueStr := AnsiLeftStr(lValueStr, Length(lValueStr)-lLang.DataTypeEnumEntryListStripCount);
                        TInfra.InsertTemplateLines(lEnumTemplate, '%s2', lValueStr);
                        for b := 0 to lEnumTemplate.Count-1 do
                           lDataTypeList.AddObject(lEnumTemplate[b], lDataType)
                     finally
                        lEnumTemplate.Free;
                     end;
                  end;
               end;
            end;
         end;
         if lDataTypeList.Count > 0 then
         begin
            lDataTypesTemplate := TStringList.Create;
            try
               lDataTypesTemplate.Text := lLang.DataTypesTemplate;
               TInfra.InsertTemplateLines(lDataTypesTemplate, '%s1', lDataTypeList);
               ALines.AddStrings(lDataTypesTemplate);
            finally
               lDataTypesTemplate.Free;
            end;
         end;
      finally
         lDataTypeList.Free;
      end;
   end;
end;

function Dummy_GetLiteralType(const AValue: string): integer;
var
   i: integer;
begin
   result := UNKNOWN_TYPE;
   if TryStrToInt(AValue, i) then
      result := GENERIC_INT_TYPE;
end;

procedure Dummy_ProgramHeaderSectionGenerator(ALines: TStringList);
var
   hdrTemplate: TStringList;
   lLang: TLangDefinition;
begin
    lLang := GInfra.CurrentLang;
    if lLang.ProgramHeaderTemplate <> '' then
    begin
       hdrTemplate := TStringList.Create;
       try
          hdrTemplate.Text := lLang.ProgramHeaderTemplate;
          TInfra.InsertTemplateLines(hdrTemplate, '%s1', GProject.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s2', lLang.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s3', GProject.GetProgramHeader);
          TInfra.InsertTemplateLines(hdrTemplate, '%s4', DateTimeToStr(Now));
          TInfra.InsertTemplateLines(hdrTemplate, '%s5', ExtractFileName(lLang.DefFile));
          ALines.AddStrings(hdrTemplate);
       finally
          hdrTemplate.Free;
       end;
    end;
end;

procedure Dummy_LibSectionGenerator(ALines: TStringList);
var
   i: integer;
   libList, libTemplate: TStringList;
   lLang: TLangDefinition;
   lLibStr, lPlaceHolder, lPlaceHolder2: string;
   lIss1: boolean;
begin
   lLang := GInfra.CurrentLang;
   libList := GProject.GetLibraryList;
   try
      if (libList.Count > 0) and (lLang.LibTemplate <> '') and ((lLang.LibEntry <> '') or (lLang.LibEntryList <> '')) then
      begin
         lLibStr := '';
         libTemplate := TStringList.Create;
         try
            lIss1 := AnsiPos('%s1', lLang.LibTemplate) <> 0;
            libTemplate.Text := lLang.LibTemplate;
            for i := 0 to libList.Count-1 do
            begin
               if lIss1 then
                  lLibStr := lLibStr + Format(lLang.LibEntry, [libList[i]]) + CRLF
               else
                  lLibStr := lLibStr + Format(lLang.LibEntryList, [libList[i]]);
            end;
            if lIss1 then
            begin
               lPlaceHolder := '%s1';
               lPlaceHolder2 := '%s2';
            end
            else
            begin
               lLibStr := AnsiLeftStr(lLibStr, Length(lLibStr)-lLang.LibEntryListStripCount);
               lPlaceHolder := '%s2';
               lPlaceHolder2 := '%s1';
            end;
            TInfra.InsertTemplateLines(libTemplate, lPlaceHolder, lLibStr);
            TInfra.InsertTemplateLines(libTemplate, lPlaceHolder2, '');
            ALines.AddStrings(libTemplate);
         finally
            libTemplate.Free;
         end;
      end;
   finally
      libList.Free;
   end;
end;

procedure Dummy_ConstSectionGenerator(ALines: TStringList; AConstList: TConstDeclareList);
var
   i: integer;
   lConstStr, lConstEntry: string;
   lLang: TLangDefinition;
   lConstList, lConstTemplate: TStringList;
   lIsExtern: boolean;
begin
   lLang := GInfra.CurrentLang;
   if (AConstList <> nil) and (AConstList.sgList.RowCount > 2) and (lLang.ConstTemplate <> '') then
   begin
      lConstList := TStringList.Create;
      try
         for i := 1 to AConstList.sgList.RowCount-2 do
         begin
            lIsExtern := AConstList.IsExternal(i);
            if (lIsExtern and lLang.GenExternVarConst) or not lIsExtern then
            begin
               if lIsExtern then
                  lConstEntry := llang.ConstEntryExtern
               else
                  lConstEntry := llang.ConstEntry;
               lConstStr := FastCodeAnsiStringReplace(lConstEntry, '%s1', AConstList.sgList.Cells[CONST_NAME_COL, i]);
               lConstStr := FastCodeAnsiStringReplace(lConstStr, '%s2', AConstList.sgList.Cells[CONST_VALUE_COL, i]);
               lConstList.AddObject(lConstStr, AConstList);
            end;
         end;
         if lConstList.Count > 0 then
         begin
            lConstTemplate := TStringList.Create;
            try
               lConstTemplate.Text := lLang.ConstTemplate;
               TInfra.InsertTemplateLines(lConstTemplate, '%s1', lConstList);
               ALines.AddStrings(lConstTemplate);
            finally
               lConstTemplate.Free;
            end;
         end;
      finally
         lConstList.Free;
      end;
   end;
end;

procedure Dummy_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   lLang: TLangDefinition;
   i, b, lType: integer;
   lVarStr, lSizeStr, lInit, lInitEntry, lRecord, lEnum, lExtern, lName, lTypeStr: string;
   lVarTemplate, lVarList: TStringList;
   lIsExtern: boolean;
begin
   lLang := GInfra.CurrentLang;
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) and (lLang.VarTemplate <> '') then
   begin
      lVarList := TStringList.Create;
      try
         for i := 1 to AVarList.sgList.RowCount-2 do
         begin
            lSizeStr := '';
            lName := AVarList.sgList.Cells[VAR_NAME_COL, i];
            lTypeStr := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            lIsExtern := AVarList.IsExternal(i);
            if (lIsExtern and lLang.GenExternVarConst) or not lIsExtern then
            begin
               for b := 1 to AVarList.GetDimensionCount(lName) do
                  lSizeStr := lSizeStr + Format(lLang.VarEntryArraySize, [AVarList.GetDimension(lName, b)]);
               if lSizeStr <> '' then
               begin
                  lVarStr := FastCodeAnsiStringReplace(lLang.VarEntryArray, '%s1', lName);
                  lSizeStr := AnsiLeftStr(lSizeStr, Length(lSizeStr)-lLang.VarEntryArraySizeStripCount);
                  lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s3', lSizeStr);
               end
               else
                  lVarStr := FastCodeAnsiStringReplace(llang.VarEntry, '%s1', lName);
               lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s2', lTypeStr);
               lInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
               if lInit <> '' then
               begin
                  if lIsExtern then
                     lInitEntry := lLang.VarEntryInitExtern
                  else
                     lInitEntry := lLang.VarEntryInit;
                  lInit := FastCodeAnsiStringReplace(lInitEntry, '%s1', lInit);
               end;
               lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s4', lInit);
               lType := TParserHelper.GetType(lTypeStr);
               lRecord := '';
               lEnum := '';
               if TParserHelper.IsStructType(lType) then
                  lRecord := lLang.FunctionHeaderArgsEntryRecord
               else if TParserHelper.IsEnumType(lType) then
                  lEnum := lLang.FunctionHeaderArgsEntryEnum;
               lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s5', lRecord);
               lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s6', lEnum);
               if lIsExtern then
                  lExtern := lLang.ExternEntry
               else
                  lExtern := '';
               lVarStr := FastCodeAnsiStringReplace(lVarStr, '%s7', lExtern);
               lVarList.AddObject(lVarStr, AVarList);
            end;
         end;
         lVarTemplate := TStringList.Create;
         try
            lVarTemplate.Text := lLang.VarTemplate;
            TInfra.InsertTemplateLines(lVarTemplate, '%s1', lVarList);
            ALines.AddStrings(lVarTemplate);
         finally
            lVarTemplate.Free;
         end;
      finally
         lVarList.Free;
      end;
   end;
end;

procedure Dummy_MainProgramSectionGenerator(ALines: TStringList; ADeep: integer);
var
   lBlock: TBlock;
begin
   lBlock := GProject.GetMainBlock;
   if lBlock <> nil then
      lBlock.GenerateCode(ALines, GInfra.CurrentLang.Name, ADeep);
end;

procedure Dummy_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
var
   lFunction: TUserFunction;
   lParm: TParameter;
   lName, lArgList, lParmStr, lNoneType1, lNoneType2, lType, lRef, lArray, lRecord, lEnum: string;
   iterp, iter: IIterator;
   lLang: TLangDefinition;
   lHeaderTemplate, lVarStrList, lFunctionTemplate, lBodyTemplate, lFunctionList, lFunctionsTemplate: TStringList;
   lTypeInt: integer;
begin
   lLang := GInfra.CurrentLang;
   if lLang.FunctionsTemplate <> '' then
   begin
      lFunctionList := TStringList.Create;
      try
         iter := GProject.GetUserFunctions;
         while iter.HasNext do
         begin
            lFunction := TUserFunction(iter.Next);
            lName := lFunction.GetName;
            if (lName <> '') and not lFunction.Header.chkExtDeclare.Checked and (lLang.FunctionTemplate <> '') then
            begin
               // assemble list of function parameters
               lArgList := '';
               iterp := lFunction.Header.GetParameterIterator;
               while iterp.HasNext do
               begin
                  lParm := TParameter(iterp.Next);
                  lParmStr := FastCodeAnsiStringReplace(lLang.FunctionHeaderArgsEntryMask, '%s1', Trim(lParm.edtName.Text));
                  lParmStr := FastCodeAnsiStringReplace(lParmStr, '%s2', lParm.cbType.Text);
                  lRef := '';
                  lArray := '';
                  lRecord := '';
                  lEnum := '';
                  if lParm.chkReference.Checked then
                     lRef := lLang.FunctionHeaderArgsEntryRef;
                  if lParm.chkTable.Checked then
                     lArray := lLang.FunctionHeaderArgsEntryArray;
                  lTypeInt := TParserHelper.GetType(lParm.cbType.Text);
                  if TParserHelper.IsStructType(lTypeInt) then
                     lRecord := lLang.FunctionHeaderArgsEntryRecord
                  else if TParserHelper.IsEnumType(lTypeInt) then
                     lEnum := lLang.FunctionHeaderArgsEntryEnum;
                  lParmStr := FastCodeAnsiStringReplace(lParmStr, '%s3', lRef);
                  lParmStr := FastCodeAnsiStringReplace(lParmStr, '%s4', lArray);
                  lParmStr := FastCodeAnsiStringReplace(lParmStr, '%s5', lRecord);
                  lParmStr := FastCodeAnsiStringReplace(lParmStr, '%s6', lEnum);
                  lArgList := lArgList + lParmStr;
               end;
               lArgList := AnsiLeftStr(lArgList, Length(lArgList)-lLang.FunctionHeaderArgsStripCount);

               lHeaderTemplate := TStringList.Create;
               try
                  // assemble function header line
                  lHeaderTemplate.Text := FastCodeAnsiStringReplace(lLang.FunctionHeaderTemplate, '%s1', lName);
                  lHeaderTemplate.Text := FastCodeAnsiStringReplace(lHeaderTemplate.Text, '%s3', lArgList);
                  if lFunction.Header.cbType.ItemIndex <> 0 then
                  begin
                     lNoneType1 := lLang.FunctionHeaderTypeNotNone1;
                     lNoneType2 := lLang.FunctionHeaderTypeNotNone2;
                     lType := lFunction.Header.cbType.Text;
                  end
                  else
                  begin
                     lNoneType1 := lLang.FunctionHeaderTypeNone1;
                     lNoneType2 := lLang.FunctionHeaderTypeNone2;
                     lType := '';
                  end;
                  lHeaderTemplate.Text := FastCodeAnsiStringReplace(lHeaderTemplate.Text, '%s4', lType);
                  lHeaderTemplate.Text := FastCodeAnsiStringReplace(lHeaderTemplate.Text, '%s5', lNoneType1);
                  lHeaderTemplate.Text := FastCodeAnsiStringReplace(lHeaderTemplate.Text, '%s6', lNoneType2);

                  if ASkipBodyGen then
                  begin
                     TInfra.InsertTemplateLines(lHeaderTemplate, '%s2', nil);
                     lFunctionList.AddStrings(lHeaderTemplate);
                  end
                  else
                  begin
                     // assemble entire function section
                     if lFunction.Header.chkInclDescCode.Checked then
                        lHeaderTemplate.Text := FastCodeAnsiStringReplace(lHeaderTemplate.Text, '%s2', lFunction.Header.memDescription.Text)
                     else
                        TInfra.InsertTemplateLines(lHeaderTemplate, '%s2', nil);
                     lFunctionTemplate := TStringList.Create;
                     try
                        lFunctionTemplate.Text := lLang.FunctionTemplate;
                        TInfra.InsertTemplateLines(lFunctionTemplate, '%s1', lHeaderTemplate, lFunction.Header);
                        lVarStrList := TStringList.Create;
                        try
                           if Assigned(lLang.VarSectionGenerator) then
                              lLang.VarSectionGenerator(lVarStrList, lFunction.Header.LocalVars)
                           else
                              Dummy_VarSectionGenerator(lVarStrList, lFunction.Header.LocalVars);
                           TInfra.InsertTemplateLines(lFunctionTemplate, '%s2', lVarStrList);
                        finally
                           lVarStrList.Free;
                        end;
                        if lFunction.Body <> nil then
                        begin
                           lBodyTemplate := TStringList.Create;
                           try
                              lFunction.Body.GenerateCode(lBodyTemplate, lLang.Name, 0);
                              TInfra.InsertTemplateLines(lFunctionTemplate, '%s3', lBodyTemplate);
                           finally
                              lBodyTemplate.Free;
                           end;
                           lFunction.Body.GenerateTemplateSection(lFunctionList, lFunctionTemplate, lLang.Name, 0);
                        end;
                     finally
                        lFunctionTemplate.Free;
                     end;
                  end;
               finally
                  lHeaderTemplate.Free;
               end;
            end;
         end;
         lFunctionsTemplate := TStringList.Create;
         try
            lFunctionsTemplate.Text := lLang.FunctionsTemplate;
            TInfra.InsertTemplateLines(lFunctionsTemplate, '%s1', lFunctionList);
            ALines.AddStrings(lFunctionsTemplate);
         finally
            lFunctionsTemplate.Free;
         end;
      finally
         lFunctionList.Free;
      end;
   end;
end;

function Dummy_GetPointerTypeName(const AValue: string): string;
begin
   result := Format(GInfra.CurrentLang.PointerTypeMask, [AValue]);
end;

function Dummy_SkipFuncBodyGen: boolean;
begin
   result := false;
end;

function Dummy_GetUserTypeDesc(ADataType: TUserDataType): string;
var
   lKey: string;
begin
   result := GInfra.CurrentLang.UserTypeDesc;
   if result <> '' then
   begin
      result := FastCodeAnsiStringReplace(result, '%s1', ADataType.edtName.Text);
      if ADataType.rbReal.Checked then
         lKey := 'rbReal'
      else if ADataType.rbInt.Checked then
         lKey := 'rbInt'
      else if ADataType.rbStruct.Checked then
         lKey := 'rbStruct'
      else if ADataType.rbEnum.Checked then
         lKey := 'rbEnum'
      else if ADataType.rbArray.Checked then
         lKey := 'rbArray'
      else
         lKey := 'rbOther';
      result := FastCodeAnsiStringReplace(result, '%s2', i18Manager.GetString(lKey));
   end;
end;

function Dummy_GetMainProgramDesc: string;
begin
   result := i18Manager.GetString(GInfra.CurrentLang.ProgramLabelKey);
   result := FastCodeAnsiStringReplace(result, '%s1', GProject.Name);
end;

function Dummy_GetUserFuncDesc(AHeader: TUserFunctionHeader): string;
var
   lFuncParmList, lDesc, lName, lType, lKey: string;
   lLang: TLangDefinition;
   iterp: IIterator;
begin
   result := '';
   lDesc := '';
   lFuncParmList := '';
   lName := '';
   lType := '';
   lKey := '';
   lLang := GInfra.CurrentLang;
   if AHeader <> nil then
   begin
      iterp := AHeader.GetParameterIterator;
      while iterp.HasNext do
         lFuncParmList := lFuncParmList + TParameter(iterp.Next).cbType.Text + ',';
      lFuncParmList := AnsiLeftStr(lFuncParmList, Length(lFuncParmList)-1);
      if AHeader.cbType.ItemIndex <> 0 then
      begin
         lKey := lLang.FunctionLabelKey;
         lType := AHeader.cbType.Text;
      end
      else
         lKey := lLang.ProcedureLabelKey;
      lName := Trim(AHeader.edtName.Text);
      if AHeader.chkInclDescFlow.Checked then
         lDesc := AHeader.memDescription.Text;
   end;
   if lKey <> '' then
   begin
      result := i18Manager.GetString(lKey);
      result := FastCodeAnsiStringReplace(result, '##', CRLF);
      result := FastCodeAnsiStringReplace(result, '%s1', lName);
      result := FastCodeAnsiStringReplace(result, '%s2', lFuncParmList);
      result := FastCodeAnsiStringReplace(result, '%s3', lType);
      result := FastCodeAnsiStringReplace(result, '%s4', lDesc);
   end;
end;

initialization

   with GInfra.DummyLang do
   begin
      Name := DUMMY_LANG_ID;
      EnabledUserFunctionBody := true;
      EnabledExplorer := true;
      MainProgramSectionGenerator := Dummy_MainProgramSectionGenerator;
      UserFunctionsSectionGenerator := Dummy_UserFunctionsSectionGenerator;
      VarSectionGenerator := Dummy_VarSectionGenerator;
      ProgramHeaderSectionGenerator := Dummy_ProgramHeaderSectionGenerator;
      LibSectionGenerator := Dummy_LibSectionGenerator;
      ConstSectionGenerator := Dummy_ConstSectionGenerator;
      UserDataTypesSectionGenerator  := Dummy_UserDataTypesSectionGenerator;
      GetLiteralType := Dummy_GetLiteralType;
      GetPointerTypeName := Dummy_GetPointerTypeName;
      GetUserFuncDesc := Dummy_GetUserFuncDesc;
      GetUserTypeDesc := Dummy_GetUserTypeDesc;
      GetMainProgramDesc := Dummy_GetMainProgramDesc;
      SkipFuncBodyGen := Dummy_SkipFuncBodyGen;
   end;

   // it really sucks but this must be executed here due to initialization order
   GInfra.ReadFromRegistry;

end.