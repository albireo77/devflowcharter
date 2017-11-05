{
   Copyright (C) 2006 The devFlowcharter project.
   The initial author of this file is Michal Domagala.
    
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License        (
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
   System.SysUtils, System.StrUtils, System.Classes, Base_Block, LangDefinition,
   UserFunction, DeclareList, CommonInterfaces, UserDataType, ApplicationCommon,
   ParserHelper, CommonTypes;

procedure Dummy_UserDataTypesSectionGenerator(ALines: TStringList);
var
   dataType: TUserDataType;
   field: TField;
   name, sizeStr, lRecord, enum: string;
   b, lType: integer;
   lang: TLangDefinition;
   typesList, typesTemplate, recTemplate, fieldList, enumTemplate: TStringList;
   typeStr, fieldStr, valStr, valStr2: string;
   fields: IEnumerable<TField>;
begin
   lang := GInfra.CurrentLang;
   if not lang.DataTypesTemplate.IsEmpty then
   begin
      typesList := TStringList.Create;
      try
         for dataType in GProject.GetUserDataTypes do
         begin
            name := dataType.GetName;
            if (not name.IsEmpty) and not dataType.chkExtDeclare.Checked then
            begin
               case dataType.Kind of
                  dtInt:
                  if not lang.DataTypeIntMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeIntMask, PRIMARY_PLACEHOLDER, name);
                     typesList.AddObject(typeStr, dataType);
                  end;
                  dtReal:
                  if not lang.DataTypeRealMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeRealMask, PRIMARY_PLACEHOLDER, name);
                     typesList.AddObject(typeStr, dataType);
                  end;
                  dtOther:
                  if not lang.DataTypeOtherMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeOtherMask, PRIMARY_PLACEHOLDER, name);
                     valStr := '';
                     fields := dataType.GetFields;
                     if fields.GetEnumerator.MoveNext then
                        valStr := Trim(fields.GetEnumerator.Current.edtName.Text);
                     typeStr := ReplaceStr(typeStr, '%s2', valStr);
                     typesList.AddObject(typeStr, dataType);
                  end;
                  dtArray:
                  if not lang.DataTypeArrayMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeArrayMask, PRIMARY_PLACEHOLDER, name);
                     valStr := '';
                     valStr2 := '';
                     fields := dataType.GetFields;
                     if fields.GetEnumerator.MoveNext then
                     begin
                        field := fields.GetEnumerator.Current;
                        valStr := field.cbType.Text;
                        valStr2 := lang.GetArraySizes(field.edtSize);
                     end;
                     typeStr := ReplaceStr(typeStr, '%s2', valStr);
                     typeStr := ReplaceStr(typeStr, '%s3', valStr2);
                     typesList.AddObject(typeStr, dataType);
                  end;
                  dtRecord:
                  if not lang.DataTypeRecordTemplate.IsEmpty then
                  begin
                     recTemplate := TStringList.Create;
                     try
                        recTemplate.Text := ReplaceStr(lang.DataTypeRecordTemplate, PRIMARY_PLACEHOLDER, name);
                        fieldList := TStringList.Create;
                        try
                           for field in dataType.GetFields do
                           begin
                              sizeStr := lang.GetArraySizes(field.edtSize);
                              if sizeStr.IsEmpty then
                                 fieldStr := lang.DataTypeRecordFieldMask
                              else
                                 fieldStr := lang.DataTypeRecordFieldArrayMask;
                              fieldStr := ReplaceStr(fieldStr, PRIMARY_PLACEHOLDER, Trim(field.edtName.Text));
                              fieldStr := ReplaceStr(fieldStr, '%s2', field.cbType.Text);
                              fieldStr := ReplaceStr(fieldStr, '%s3', sizeStr);
                              lRecord := '';
                              enum := '';
                              lType := TParserHelper.GetType(field.cbType.Text);
                              if TParserHelper.IsRecordType(lType) then
                                 lRecord := lang.FunctionHeaderArgsEntryRecord
                              else if TParserHelper.IsEnumType(lType) then
                                 enum := lang.FunctionHeaderArgsEntryEnum;
                              fieldStr := ReplaceStr(fieldStr, '%s4', lRecord);
                              fieldStr := ReplaceStr(fieldStr, '%s5', enum);
                              fieldList.Add(fieldStr);
                           end;
                           TInfra.InsertTemplateLines(recTemplate, '%s2', fieldList);
                        finally
                           fieldList.Free;
                        end;
                        for b := 0 to recTemplate.Count-1 do
                           typesList.AddObject(recTemplate[b], dataType)
                     finally
                        recTemplate.Free;
                     end;
                  end;
                  dtEnum:
                  if not lang.DataTypeEnumTemplate.IsEmpty then
                  begin
                     enumTemplate := TStringList.Create;
                     try
                        enumTemplate.Text := ReplaceStr(lang.DataTypeEnumTemplate, PRIMARY_PLACEHOLDER, name);
                        valStr := '';
                        for field in dataType.GetFields do
                           valStr := valStr + Format(lang.DataTypeEnumEntryList, [Trim(field.edtName.Text)]);
                        if (lang.DataTypeEnumEntryListStripCount > 0) and not valStr.IsEmpty then
                           SetLength(valStr, valStr.Length - lang.DataTypeEnumEntryListStripCount);
                        TInfra.InsertTemplateLines(enumTemplate, '%s2', valStr);
                        for b := 0 to enumTemplate.Count-1 do
                           typesList.AddObject(enumTemplate[b], dataType)
                     finally
                        enumTemplate.Free;
                     end;
                  end;
               end;
            end;
         end;
         if typesList.Count > 0 then
         begin
            typesTemplate := TStringList.Create;
            try
               typesTemplate.Text := lang.DataTypesTemplate;
               TInfra.InsertTemplateLines(typesTemplate, PRIMARY_PLACEHOLDER, typesList);
               ALines.AddStrings(typesTemplate);
            finally
               typesTemplate.Free;
            end;
         end;
      finally
         typesList.Free;
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
   lang: TLangDefinition;
begin
    lang := GInfra.CurrentLang;
    if not lang.ProgramHeaderTemplate.IsEmpty then
    begin
       hdrTemplate := TStringList.Create;
       try
          hdrTemplate.Text := lang.ProgramHeaderTemplate;
          TInfra.InsertTemplateLines(hdrTemplate, PRIMARY_PLACEHOLDER, GProject.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s2', lang.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s3', GProject.GetProgramHeader);
          TInfra.InsertTemplateLines(hdrTemplate, '%s4', DateTimeToStr(Now));
          TInfra.InsertTemplateLines(hdrTemplate, '%s5', ExtractFileName(lang.DefFile));
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
   lang: TLangDefinition;
   libStr, p1, p2: string;
   isS1: boolean;
begin
   lang := GInfra.CurrentLang;
   libList := GProject.GetLibraryList;
   try
      if (libList.Count > 0) and (not lang.LibTemplate.IsEmpty) and ((not lang.LibEntry.IsEmpty) or (not lang.LibEntryList.IsEmpty)) then
      begin
         libStr := '';
         libTemplate := TStringList.Create;
         try
            isS1 := lang.LibTemplate.Contains(PRIMARY_PLACEHOLDER);
            libTemplate.Text := lang.LibTemplate;
            for i := 0 to libList.Count-1 do
               libStr := libStr + Format(IfThen(isS1, lang.LibEntry + sLineBreak, lang.LibEntryList), [libList[i]]);
            if isS1 then
            begin
               p1 := PRIMARY_PLACEHOLDER;
               p2 := '%s2';
            end
            else
            begin
               if (lang.LibEntryListStripCount > 0) and not libStr.IsEmpty then
                  SetLength(libStr, libStr.Length - lang.LibEntryListStripCount);
               p1 := '%s2';
               p2 := PRIMARY_PLACEHOLDER;
            end;
            TInfra.InsertTemplateLines(libTemplate, p1, libStr);
            TInfra.InsertTemplateLines(libTemplate, p2, '');
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
   constStr: string;
   lang: TLangDefinition;
   constList, constTemplate: TStringList;
   isExtern: boolean;
begin
   lang := GInfra.CurrentLang;
   if (AConstList <> nil) and (AConstList.sgList.RowCount > 2) and not lang.ConstTemplate.IsEmpty then
   begin
      constList := TStringList.Create;
      try
         for i := 1 to AConstList.sgList.RowCount-2 do
         begin
            isExtern := AConstList.IsExternal(i);
            if (isExtern and lang.GenExternVarConst) or not isExtern then
            begin
               constStr := IfThen(isExtern, lang.ConstEntryExtern, lang.ConstEntry);
               constStr := ReplaceStr(constStr, PRIMARY_PLACEHOLDER, AConstList.sgList.Cells[CONST_NAME_COL, i]);
               constStr := ReplaceStr(constStr, '%s2', AConstList.sgList.Cells[CONST_VALUE_COL, i]);
               constList.AddObject(constStr, AConstList);
            end;
         end;
         if constList.Count > 0 then
         begin
            constTemplate := TStringList.Create;
            try
               constTemplate.Text := lang.ConstTemplate;
               TInfra.InsertTemplateLines(constTemplate, PRIMARY_PLACEHOLDER, constList);
               ALines.AddStrings(constTemplate);
            finally
               constTemplate.Free;
            end;
         end;
      finally
         constList.Free;
      end;
   end;
end;

procedure Dummy_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   lang: TLangDefinition;
   i, b, lType, dcount: integer;
   varStr, varSize, varInit, initEntry, lRecord, enum, name, typeStr: string;
   varTemplate, varList: TStringList;
   isExtern: boolean;
begin
   lang := GInfra.CurrentLang;
   if (AVarList <> nil) and (AVarList.sgList.RowCount > 2) and not lang.VarTemplate.IsEmpty then
   begin
      varList := TStringList.Create;
      try
         for i := 1 to AVarList.sgList.RowCount-2 do
         begin
            varSize := '';
            name := AVarList.sgList.Cells[VAR_NAME_COL, i];
            typeStr := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            isExtern := AVarList.IsExternal(i);
            if (isExtern and lang.GenExternVarConst) or not isExtern then
            begin
               dcount := AVarList.GetDimensionCount(name);
               if dcount > 0 then
               begin
                  if dcount <> MaxInt then
                  begin
                     for b := 1 to dcount do
                        varSize := varSize + Format(lang.VarEntryArraySize, [AVarList.GetDimension(name, b)]);
                     if (lang.VarEntryArraySizeStripCount > 0) and not varSize.IsEmpty then
                        SetLength(varSize, varSize.Length - lang.VarEntryArraySizeStripCount);
                  end;
                  varStr := ReplaceStr(lang.VarEntryArray, PRIMARY_PLACEHOLDER, name);
                  varStr := ReplaceStr(varStr, '%s3', varSize);
               end
               else
                  varStr := ReplaceStr(lang.VarEntry, PRIMARY_PLACEHOLDER, name);
               varStr := ReplaceStr(varStr, '%s2', typeStr);
               varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
               if not varInit.IsEmpty then
               begin
                  initEntry := IfThen(isExtern, lang.VarEntryInitExtern, lang.VarEntryInit);
                  if not initEntry.IsEmpty then
                     varInit := ReplaceStr(initEntry, PRIMARY_PLACEHOLDER, varInit);
               end;
               varStr := ReplaceStr(varStr, '%s4', varInit);
               lType := TParserHelper.GetType(typeStr);
               lRecord := '';
               enum := '';
               if TParserHelper.IsRecordType(lType) then
                  lRecord := lang.FunctionHeaderArgsEntryRecord
               else if TParserHelper.IsEnumType(lType) then
                  enum := lang.FunctionHeaderArgsEntryEnum;
               varStr := ReplaceStr(varStr, '%s5', lRecord);
               varStr := ReplaceStr(varStr, '%s6', enum);
               varStr := ReplaceStr(varStr, '%s7', IfThen(isExtern, lang.ExternEntry));
               varList.AddObject(varStr, AVarList);
            end;
         end;
         varTemplate := TStringList.Create;
         try
            varTemplate.Text := lang.VarTemplate;
            TInfra.InsertTemplateLines(varTemplate, PRIMARY_PLACEHOLDER, varList);
            ALines.AddStrings(varTemplate);
         finally
            varTemplate.Free;
         end;
      finally
         varList.Free;
      end;
   end;
end;

procedure Dummy_MainProgramSectionGenerator(ALines: TStringList; ADeep: integer);
var
   block: TBlock;
begin
   block := GProject.GetMainBlock;
   if block <> nil then
      block.GenerateCode(ALines, GInfra.CurrentLang.Name, ADeep);
end;

procedure Dummy_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
var
   func: TUserFunction;
   param: TParameter;
   name, argList, paramStr, noneType1, noneType2, lType, ref, lArray, lRecord, enum, defValue: string;
   lang: TLangDefinition;
   headerTemplate, varList, funcTemplate, bodyTemplate, funcList, funcsTemplate: TStringList;
   intType: integer;
begin
   lang := GInfra.CurrentLang;
   if not lang.FunctionsTemplate.IsEmpty then
   begin
      funcList := TStringList.Create;
      try
         for func in GProject.GetUserFunctions do
         begin
            name := func.GetName;
            if (not name.IsEmpty) and not func.Header.chkExtDeclare.Checked and not lang.FunctionTemplate.IsEmpty then
            begin
               // assemble list of function parameters
               argList := '';
               for param in func.Header.GetParameters do
               begin
                  paramStr := ReplaceStr(lang.FunctionHeaderArgsEntryMask, PRIMARY_PLACEHOLDER, Trim(param.edtName.Text));
                  paramStr := ReplaceStr(paramStr, '%s2', param.cbType.Text);
                  ref := '';
                  lArray := '';
                  lRecord := '';
                  enum := '';
                  if param.chkReference.Checked then
                     ref := lang.FunctionHeaderArgsEntryRef;
                  if param.chkTable.Checked then
                     lArray := lang.FunctionHeaderArgsEntryArray;
                  intType := TParserHelper.GetType(param.cbType.Text);
                  if TParserHelper.IsRecordType(intType) then
                     lRecord := lang.FunctionHeaderArgsEntryRecord
                  else if TParserHelper.IsEnumType(intType) then
                     enum := lang.FunctionHeaderArgsEntryEnum;
                  defValue := Trim(param.edtDefault.Text);
                  if not defValue.IsEmpty then
                     defValue := ReplaceStr(lang.FunctionHeaderArgsEntryDefault, '%s', defValue);
                  paramStr := ReplaceStr(paramStr, '%s3', ref);
                  paramStr := ReplaceStr(paramStr, '%s4', lArray);
                  paramStr := ReplaceStr(paramStr, '%s5', lRecord);
                  paramStr := ReplaceStr(paramStr, '%s6', enum);
                  paramStr := ReplaceStr(paramStr, '%s7', defValue);
                  argList := argList + paramStr;
               end;

               if (lang.FunctionHeaderArgsStripCount > 0) and not argList.IsEmpty then
                  SetLength(argList, argList.Length - lang.FunctionHeaderArgsStripCount);

               headerTemplate := TStringList.Create;
               try
                  // assemble function header line
                  headerTemplate.Text := ReplaceStr(lang.FunctionHeaderTemplate, PRIMARY_PLACEHOLDER, name);
                  headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s3', argList);
                  if func.Header.cbType.ItemIndex <> 0 then
                  begin
                     noneType1 := lang.FunctionHeaderTypeNotNone1;
                     noneType2 := lang.FunctionHeaderTypeNotNone2;
                     lType := func.Header.cbType.Text;
                  end
                  else
                  begin
                     noneType1 := lang.FunctionHeaderTypeNone1;
                     noneType2 := lang.FunctionHeaderTypeNone2;
                     lType := '';
                  end;
                  headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s4', lType);
                  headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s5', noneType1);
                  headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s6', noneType2);

                  if ASkipBodyGen then
                  begin
                     TInfra.InsertTemplateLines(headerTemplate, '%s2', nil);
                     funcList.AddStrings(headerTemplate);
                  end
                  else
                  begin
                     // assemble entire function section
                     if func.Header.chkInclDescCode.Checked then
                        headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s2', func.Header.memDesc.Text)
                     else
                        TInfra.InsertTemplateLines(headerTemplate, '%s2', nil);
                     funcTemplate := TStringList.Create;
                     try
                        funcTemplate.Text := lang.FunctionTemplate;
                        TInfra.InsertTemplateLines(funcTemplate, PRIMARY_PLACEHOLDER, headerTemplate, func.Header);
                        varList := TStringList.Create;
                        try
                           if Assigned(lang.VarSectionGenerator) then
                              lang.VarSectionGenerator(varList, func.Header.LocalVars)
                           else
                              Dummy_VarSectionGenerator(varList, func.Header.LocalVars);
                           TInfra.InsertTemplateLines(funcTemplate, '%s2', varList);
                        finally
                           varList.Free;
                        end;
                        if func.Body <> nil then
                        begin
                           bodyTemplate := TStringList.Create;
                           try
                              func.Body.GenerateCode(bodyTemplate, lang.Name, 0);
                              TInfra.InsertTemplateLines(funcTemplate, '%s3', bodyTemplate);
                           finally
                              bodyTemplate.Free;
                           end;
                           func.Body.GenerateTemplateSection(funcList, funcTemplate, lang.Name, 0);
                        end;
                     finally
                        funcTemplate.Free;
                     end;
                  end;
               finally
                  headerTemplate.Free;
               end;
            end;
         end;
         funcsTemplate := TStringList.Create;
         try
            funcsTemplate.Text := lang.FunctionsTemplate;
            TInfra.InsertTemplateLines(funcsTemplate, PRIMARY_PLACEHOLDER, funcList);
            ALines.AddStrings(funcsTemplate);
         finally
            funcsTemplate.Free;
         end;
      finally
         funcList.Free;
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
   kind: string;
begin
   result := GInfra.CurrentLang.UserTypeDesc;
   if not result.IsEmpty then
   begin
      result := ReplaceStr(result, PRIMARY_PLACEHOLDER, ADataType.edtName.Text);
      kind := '';
      if ADataType.rgTypeBox.ItemIndex <> -1 then
         kind := ADataType.rgTypeBox.Items[ADataType.rgTypeBox.ItemIndex];
      result := ReplaceStr(result, '%s2', kind);
   end;
end;

function Dummy_GetMainProgramDesc: string;
begin
   result := i18Manager.GetString(GInfra.CurrentLang.ProgramLabelKey);
   result := ReplaceStr(result, PRIMARY_PLACEHOLDER, GProject.Name);
end;

function Dummy_GetUserFuncDesc(AHeader: TUserFunctionHeader; ALongDesc: boolean = true): string;
var
   params, desc, name, lType, key, lb: string;
   lang: TLangDefinition;
   param: TParameter;
begin
   result := '';
   desc := '';
   params := '';
   name := '';
   lType := '';
   key := '';
   lb := '';
   lang := GInfra.CurrentLang;
   if AHeader <> nil then
   begin
      name := Trim(AHeader.edtName.Text);
      if AHeader.cbType.ItemIndex <> 0 then
      begin
         key := lang.FunctionLabelKey;
         lType := AHeader.cbType.Text;
      end
      else
         key := lang.ProcedureLabelKey;
      if ALongDesc then
      begin
         for param in AHeader.GetParameters do
         begin
            if not params.IsEmpty then
               params := params + ',';
            params := params + param.cbType.Text;
         end;
         if AHeader.chkInclDescFlow.Checked then
            desc := AHeader.memDesc.Text;
         lb := sLineBreak;
      end;
   end;
   if not key.IsEmpty then
   begin
      result := i18Manager.GetString(key);
      result := ReplaceStr(result, LB_PHOLDER2, lb);
      result := ReplaceStr(result, PRIMARY_PLACEHOLDER, name);
      result := ReplaceStr(result, '%s2', params);
      result := ReplaceStr(result, '%s3', lType);
      result := ReplaceStr(result, '%s4', desc);
   end;
end;

initialization

   with GInfra.DummyLang do
   begin
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
