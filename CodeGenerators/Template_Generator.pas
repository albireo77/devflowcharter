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

unit Template_Generator;

interface

implementation

uses
   System.SysUtils, System.StrUtils, System.Classes, System.Math, Vcl.StdCtrls, Base_Block,
   LangDefinition, UserFunction, DeclareList, Interfaces, UserDataType, Infrastructure,
   ParserHelper, Types, Constants, TabComponent;

procedure Template_UserDataTypesSectionGenerator(ALines: TStringList);
var
   typeStr, fieldStr, s2, s3: string;
begin
   var lang := GInfra.CurrentLang;
   if not lang.DataTypesTemplate.IsEmpty then
   begin
      var template := TStringList.Create;
      var typesList := TStringList.Create;
      try
         for var dataType in GProject.GetUserDataTypes do
         begin
            var name := dataType.GetName;
            if (name <> '') and (lang.CodeIncludeExternDataType or not dataType.chkExternal.Checked) then
            begin
               var extModifier := dataType.GetExternModifier;
               template.Clear;
               case dataType.Kind of

                  dtInt:
                  if not lang.DataTypeIntMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeIntMask, PRIMARY_PLACEHOLDER, name);
                     template.Text := ReplaceStr(typeStr, '%s9', extModifier);
                  end;

                  dtReal:
                  if not lang.DataTypeRealMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeRealMask, PRIMARY_PLACEHOLDER, name);
                     template.Text := ReplaceStr(typeStr, '%s9', extModifier);
                  end;

                  dtOther:
                  if not lang.DataTypeOtherMask.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeOtherMask, PRIMARY_PLACEHOLDER, name);
                     typeStr := ReplaceStr(typeStr, '%s9', extModifier);
                     s2 := '';
                     var field := dataType.GetFirstField;
                     if field <> nil then
                        s2 := Trim(field.edtName.Text);
                     template.Text := ReplaceStr(typeStr, '%s2', s2);
                  end;

                  dtArray:
                  begin
                     var arrayMask := lang.DataTypeArrayMask;
                     s2 := '';
                     s3 := '';
                     var field := dataType.GetFirstField;
                     if field <> nil then
                     begin
                        s2 := field.cbType.Text;
                        s3 := lang.GetArraySizes(field.edtSize);
                        if field.edtSize.IsUnboundedArray and not lang.DataTypeUnboundedArrayMask.IsEmpty then
                           arrayMask := lang.DataTypeUnboundedArrayMask;
                     end;
                     if not arrayMask.IsEmpty then
                     begin
                        typeStr := ReplaceStr(arrayMask, PRIMARY_PLACEHOLDER, name);
                        typeStr := ReplaceStr(typeStr, '%s9', extModifier);
                        typeStr := ReplaceStr(typeStr, '%s2', s2);
                        template.Text := ReplaceStr(typeStr, '%s3', s3);
                     end;
                  end;

                  dtRecord:
                  if not lang.DataTypeRecordTemplate.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeRecordTemplate, PRIMARY_PLACEHOLDER, name);
                     template.Text := ReplaceStr(typeStr, '%s9', extModifier);
                     var fieldList := TStringList.Create;
                     try
                        for var field in dataType.GetFields do
                        begin
                           var sizeStr := lang.GetArraySizes(field.edtSize);
                           if sizeStr.IsEmpty then
                              fieldStr := lang.DataTypeRecordFieldMask
                           else
                              fieldStr := lang.DataTypeRecordFieldArrayMask;
                           fieldStr := ReplaceStr(fieldStr, PRIMARY_PLACEHOLDER, Trim(field.edtName.Text));
                           fieldStr := ReplaceStr(fieldStr, '%s2', field.cbType.Text);
                           fieldStr := ReplaceStr(fieldStr, '%s3', sizeStr);
                           var lRecord := '';
                           var enum := '';
                           var lType := TParserHelper.GetType(field.cbType.Text);
                           if TParserHelper.IsRecordType(lType) then
                              lRecord := lang.FunctionHeaderArgsEntryRecord
                           else if TParserHelper.IsEnumType(lType) then
                              enum := lang.FunctionHeaderArgsEntryEnum;
                           fieldStr := ReplaceStr(fieldStr, '%s4', lRecord);
                           fieldStr := ReplaceStr(fieldStr, '%s5', enum);
                           fieldList.Add(fieldStr);
                        end;
                        TInfra.InsertTemplateLines(template, '%s2', fieldList);
                     finally
                        fieldList.Free;
                     end;
                  end;

                  dtEnum:
                  if not lang.DataTypeEnumTemplate.IsEmpty then
                  begin
                     typeStr := ReplaceStr(lang.DataTypeEnumTemplate, PRIMARY_PLACEHOLDER, name);
                     template.Text := ReplaceStr(typeStr, '%s9', extModifier);
                     s2 := '';
                     for var field in dataType.GetFields do
                        s2 := s2 + Format(lang.DataTypeEnumEntryList, [Trim(field.edtName.Text)]);
                     if lang.DataTypeEnumEntryListStripCount > 0 then
                        SetLength(s2, s2.Length - lang.DataTypeEnumEntryListStripCount);
                     TInfra.InsertTemplateLines(template, '%s2', s2);
                  end;

               end;
               for var b := 0 to template.Count-1 do
                  typesList.AddObject(template[b], dataType)
            end;
         end;
         if typesList.Count > 0 then
         begin
            var typesTemplate := TStringList.Create;
            try
               typesTemplate.Text := lang.DataTypesTemplate;
               TInfra.InsertTemplateLines(typesTemplate, PRIMARY_PLACEHOLDER, typesList);
               ALines.AddStrings(typesTemplate);
            finally
               typesTemplate.Free;
            end;
         end;
      finally
         template.Free;
         typesList.Free;
      end;
   end;
end;

function Template_GetConstantType(const AValue: string; var AGenericType: string): integer;
begin
   AGenericType := '';
   result := IfThen(TryStrToInt(AValue, result), GENERIC_INT_TYPE, UNKNOWN_TYPE);
end;

procedure Template_ProgramHeaderSectionGenerator(ALines: TStringList);
begin
    var lang := GInfra.CurrentLang;
    if not lang.ProgramHeaderTemplate.IsEmpty then
    begin
       var hdrTemplate := TStringList.Create;
       try
          hdrTemplate.Text := lang.ProgramHeaderTemplate;
          TInfra.InsertTemplateLines(hdrTemplate, PRIMARY_PLACEHOLDER, GProject.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s2', lang.Name);
          TInfra.InsertTemplateLines(hdrTemplate, '%s3', GProject.GetProgramHeader);
          TInfra.InsertTemplateLines(hdrTemplate, '%s4', DateTimeToStr(Now));
          TInfra.InsertTemplateLines(hdrTemplate, '%s5', ExtractFileName(lang.DefFile));
          TInfra.InsertTemplateLines(hdrTemplate, '%s6', lang.DefFile);
          ALines.AddStrings(hdrTemplate);
       finally
          hdrTemplate.Free;
       end;
    end;
end;

procedure Template_LibSectionGenerator(ALines: TStringList);
begin
   var lang := GInfra.CurrentLang;
   var libList := GProject.GetLibraryList;
   try
      if (libList.Count > 0) and not lang.LibTemplate.IsEmpty then
      begin
         var libStr := '';
         var libFormat := lang.LibEntryList;
         var p1 := '%s2';
         var p2 := PRIMARY_PLACEHOLDER;
         var stripCount := lang.LibEntryListStripCount;
         if lang.LibTemplate.Contains(PRIMARY_PLACEHOLDER) then
         begin
            libFormat := lang.LibEntry + sLineBreak;
            p1 := PRIMARY_PLACEHOLDER;
            p2 := '%s2';
            stripCount := Length(sLineBreak);
         end;
         for var i := 0 to libList.Count-1 do
         begin
            var tabName := '';
            if libList.Objects[i] is TTabComponent then
               tabName := TTabComponent(libList.Objects[i]).GetName;
            var libEntry := ReplaceStr(libFormat, '%s1', tabName);
            libEntry := ReplaceStr(libEntry, '%s', libList.Strings[i]);
            libStr := libStr + libEntry;
         end;
         if not libStr.Trim.IsEmpty then
         begin
            if stripCount > 0 then
               SetLength(libStr, libStr.Length - stripCount);
            var libTemplate := TStringList.Create;
            try
               libTemplate.Text := lang.LibTemplate;
               TInfra.InsertTemplateLines(libTemplate, p1, libStr, TInfra.GetLibObject);
               TInfra.InsertTemplateLines(libTemplate, p2, '');
               ALines.AddStrings(libTemplate);
            finally
               libTemplate.Free;
            end;
         end;
      end;
   finally
      libList.Free;
   end;
end;

procedure Template_ConstSectionGenerator(ALines: TStringList; AConstList: TConstDeclareList);
begin
   var currLang := GInfra.CurrentLang;
   if (AConstList <> nil) and not currLang.ConstTemplate.IsEmpty then
   begin
      var constList := TStringList.Create;
      try
         for var i := 1 to AConstList.sgList.RowCount-2 do
         begin
            if (AConstList.GetExternalState(i) = cbChecked) and not currLang.CodeIncludeExternVarConst then
               continue;
            var constValue := AConstList.sgList.Cells[CONST_VALUE_COL, i];
            var genericTypes := '';
            var constType := '';
            var d := 0;
            var t := 0;
            if Assigned(currLang.GetConstantType) then
               t := currLang.GetConstantType(constValue, genericTypes)
            else
               t := Template_GetConstantType(constValue, genericTypes);
            if t <> UNKNOWN_TYPE then
            begin
               d := TParserHelper.DecodeArrayDimension(t);
               if d > 0 then
                  t := TParserHelper.DecodeArrayType(t);
               constType := TParserHelper.GetTypeAsString(t);
               var template := currLang.ConstTypeGeneric;
               if template.IsEmpty or genericTypes.IsEmpty or not TParserHelper.IsGenericType(constType) then
                  template := currLang.ConstTypeNotGeneric;
               if not template.IsEmpty then
               begin
                  constType := ReplaceStr(template, PRIMARY_PLACEHOLDER, constType);
                  constType := ReplaceStr(constType, '%s2', genericTypes);
               end;
            end;
            var constStr := IfThen(d > 0, currLang.ConstEntryArray, currLang.ConstEntry);
            constStr := ReplaceStr(constStr, PRIMARY_PLACEHOLDER, AConstList.sgList.Cells[CONST_NAME_COL, i]);
            constStr := ReplaceStr(constStr, '%s2', constValue);
            constStr := ReplaceStr(constStr, '%s3', AConstList.GetExternalModifier(i));
            constStr := ReplaceStr(constStr, '%s4', constType);
            if d > 0 then
               constStr := ReplaceStr(constStr, '%s5', DupeString(ReplaceStr(currLang.VarEntryArraySize, '%s', ''), d));
            constList.AddObject(constStr, AConstList);
         end;
         if constList.Count > 0 then
         begin
            var constTemplate := TStringList.Create;
            try
               constTemplate.Text := currLang.ConstTemplate;
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

procedure Template_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   lang: TLangDefinition;
   i, b, lType, dcount: integer;
   varStr, varSize, varInit, initEntry, lRecord, enum, name, typeStr: string;
   varTemplate, varList: TStringList;
   isExtern: boolean;
   dims: TArray<string>;
begin
   lang := GInfra.CurrentLang;
   if (AVarList <> nil) and not lang.VarTemplate.IsEmpty then
   begin
      varList := TStringList.Create;
      try
         for i := 1 to AVarList.sgList.RowCount-2 do
         begin
            varSize := '';
            name := AVarList.sgList.Cells[VAR_NAME_COL, i];
            isExtern := AVarList.GetExternalState(i) = cbChecked;
            typeStr := AVarList.sgList.Cells[VAR_TYPE_COL, i];
            if isExtern and not lang.CodeIncludeExternVarConst then
               continue;
            dcount := AVarList.GetDimensionCount(name);
            if dcount > 0 then
            begin
               dims := AVarList.GetDimensions(name);
               for b := 0 to High(dims) do
                  varSize := varSize + Format(lang.VarEntryArraySize, [dims[b]]);
               if lang.VarEntryArraySizeStripCount > 0 then
                  SetLength(varSize, varSize.Length - lang.VarEntryArraySizeStripCount);
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
            varStr := ReplaceStr(varStr, '%s7', AVarList.GetExternalModifier(i));
            varList.AddObject(varStr, AVarList);
         end;
         if varList.Count > 0 then
         begin
            varTemplate := TStringList.Create;
            try
               varTemplate.Text := lang.VarTemplate;
               TInfra.InsertTemplateLines(varTemplate, PRIMARY_PLACEHOLDER, varList);
               ALines.AddStrings(varTemplate);
            finally
               varTemplate.Free;
            end;
         end;
      finally
         varList.Free;
      end;
   end;
end;

procedure Template_MainFunctionSectionGenerator(ALines: TStringList; ADeep: integer);
begin
   var block := GProject.GetMainBlock;
   if block <> nil then
      block.GenerateCode(ALines, GInfra.CurrentLang.Name, ADeep);
end;

procedure Template_UserFunctionsSectionGenerator(ALines: TStringList; ASkipBodyGen: boolean);
var
   func: TUserFunction;
   param: TParameter;
   name, argList, paramStr, ref, lArray, lRecord, enum, defValue, hText, typeArray, isStatic, memDesc, h0, h1, h2: string;
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
            if (name <> '') and (lang.CodeIncludeExternFunction or not func.Header.chkExternal.Checked) and not lang.FunctionTemplate.IsEmpty then
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

               if lang.FunctionHeaderArgsStripCount > 0 then
                  SetLength(argList, argList.Length - lang.FunctionHeaderArgsStripCount);

               if func.Header.cbType.ItemIndex > 0 then
               begin
                  typeArray := IfThen(func.Header.chkArrayType.Checked, lang.FunctionHeaderTypeArray, lang.FunctionHeaderTypeNotArray);
                  h0 := func.Header.cbType.Text;
                  h1 := lang.FunctionHeaderTypeNotNone1;
                  h2 := lang.FunctionHeaderTypeNotNone2;
               end
               else
               begin
                  typeArray := '';
                  h0 := '';
                  h1 := lang.FunctionHeaderTypeNone1;
                  h2 := lang.FunctionHeaderTypeNone2;
               end;

               if func.Header.chkStatic.Visible then
                  isStatic := IfThen(func.Header.chkStatic.Checked, lang.FunctionHeaderStatic, lang.FunctionHeaderNotStatic)
               else
                  isStatic := '';

               hText := IfThen(func.Header.chkConstructor.Checked, lang.ConstructorHeaderTemplate, lang.FunctionHeaderTemplate);
               hText := ReplaceStr(hText, PRIMARY_PLACEHOLDER, name);
               hText := ReplaceStr(hText, '%s3', argList);
               hText := ReplaceStr(hText, '%s4', h0);
               hText := ReplaceStr(hText, '%s5', h1);
               hText := ReplaceStr(hText, '%s6', h2);
               hText := ReplaceStr(hText, '%s7', func.Header.GetExternModifier);
               hText := ReplaceStr(hText, '%s8', typeArray);
               hText := ReplaceStr(hText, '%s9', isStatic);

               headerTemplate := TStringList.Create;
               try
                  headerTemplate.Text := hText;
                  if ASkipBodyGen then
                  begin
                     TInfra.InsertTemplateLines(headerTemplate, '%s2', nil);
                     funcList.AddStrings(headerTemplate);
                  end
                  else
                  begin
                     memDesc := '';
                     if func.Header.chkInclDescCode.Checked then
                        memDesc := TrimRight(func.Header.memDesc.Text);
                     if memDesc.IsEmpty then
                        TInfra.InsertTemplateLines(headerTemplate, '%s2', nil)
                     else
                        headerTemplate.Text := ReplaceStr(headerTemplate.Text, '%s2', memDesc);

                     funcTemplate := TStringList.Create;
                     try
                        funcTemplate.Text := lang.FunctionTemplate;
                        TInfra.InsertTemplateLines(funcTemplate, PRIMARY_PLACEHOLDER, headerTemplate, func.Header);
                        varList := TStringList.Create;
                        try
                           if Assigned(lang.VarSectionGenerator) then
                              lang.VarSectionGenerator(varList, func.Header.LocalVars)
                           else
                              Template_VarSectionGenerator(varList, func.Header.LocalVars);
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
         if funcList.Count > 0 then
         begin
            funcsTemplate := TStringList.Create;
            try
               funcsTemplate.Text := lang.FunctionsTemplate;
               TInfra.InsertTemplateLines(funcsTemplate, PRIMARY_PLACEHOLDER, funcList);
               ALines.AddStrings(funcsTemplate);
            finally
               funcsTemplate.Free;
            end;
         end;
      finally
         funcList.Free;
      end;
   end;
end;

function Template_SkipFuncBodyGen: boolean;
begin
   result := False;
end;

procedure Template_ProgramGenerator(ALines: TStringList);
var
   programTemplate, headerTemplate, mainFuncTemplate, libTemplate, constTemplate,
   varTemplate, funcTemplate, dataTypeTemplate: TStringList;
   currLang: TLangDefinition;
   skipFuncBody: boolean;
begin

   currLang := GInfra.CurrentLang;

   if currLang.ProgramTemplate.IsEmpty then
      Exit;

   try
      // generate program header section
      headerTemplate := TStringList.Create;
      if Assigned(currLang.ProgramHeaderSectionGenerator) then
         currLang.ProgramHeaderSectionGenerator(headerTemplate)
      else
         Template_ProgramHeaderSectionGenerator(headerTemplate);

      // generate libraries section
      libTemplate := TStringList.Create;
      if Assigned(currLang.LibSectionGenerator) then
         currLang.LibSectionGenerator(libTemplate)
      else
         Template_LibSectionGenerator(libTemplate);

     // generate global constants section
     constTemplate := TStringList.Create;
     if currLang.EnabledConsts then
     begin
        if Assigned(currLang.ConstSectionGenerator) then
           currLang.ConstSectionGenerator(constTemplate, GProject.GlobalConsts)
        else
           Template_ConstSectionGenerator(constTemplate, GProject.GlobalConsts);
     end;

     // generate global variables section
     varTemplate := TStringList.Create;
     if currLang.EnabledVars then
     begin
        if Assigned(currLang.VarSectionGenerator) then
           currLang.VarSectionGenerator(varTemplate, GProject.GlobalVars)
        else
           Template_VarSectionGenerator(varTemplate, GProject.GlobalVars);
      end;

     // generate user data types section
     dataTypeTemplate := TStringList.Create;
     if currLang.EnabledUserDataTypes then
     begin
        if Assigned(currLang.UserDataTypesSectionGenerator) then
           currLang.UserDataTypesSectionGenerator(dataTypeTemplate)
        else
           Template_UserDataTypesSectionGenerator(dataTypeTemplate);
     end;

     if Assigned(currLang.SkipFuncBodyGen) then
        skipFuncBody := currLang.SkipFuncBodyGen
     else
        skipFuncBody := Template_SkipFuncBodyGen;

     // generate user functions section
     funcTemplate := TStringList.Create;
     if currLang.EnabledUserFunctionHeader then
     begin
        if Assigned(currLang.UserFunctionsSectionGenerator) then
           currLang.UserFunctionsSectionGenerator(funcTemplate, skipFuncBody)
        else
           Template_UserFunctionsSectionGenerator(funcTemplate, skipFuncBody);
     end;

      // generate main function section
      mainFuncTemplate := TStringList.Create;
      if Assigned(currLang.MainFunctionSectionGenerator) then
         currLang.MainFunctionSectionGenerator(mainFuncTemplate, 0)
      else
         Template_MainFunctionSectionGenerator(mainFuncTemplate, 0);

      programTemplate := TStringList.Create;
      programTemplate.Text := currLang.ProgramTemplate;
      TInfra.InsertTemplateLines(programTemplate, PRIMARY_PLACEHOLDER, GProject.Name);
      TInfra.InsertTemplateLines(programTemplate, '%s2', headerTemplate);
      GProject.LibSectionOffset := TInfra.InsertTemplateLines(programTemplate, '%s3', libTemplate);
      TInfra.InsertTemplateLines(programTemplate, '%s4', constTemplate);
      TInfra.InsertTemplateLines(programTemplate, '%s5', varTemplate);
      TInfra.InsertTemplateLines(programTemplate, '%s6', dataTypeTemplate);
      TInfra.InsertTemplateLines(programTemplate, '%s7', funcTemplate);
      TInfra.InsertTemplateLines(programTemplate, '%s8', mainFuncTemplate);
      ALines.AddStrings(programTemplate);
   finally
      programTemplate.Free;
      headerTemplate.Free;
      mainFuncTemplate.Free;
      libTemplate.Free;
      constTemplate.Free;
      varTemplate.Free;
      funcTemplate.Free;
      dataTypeTemplate.Free;
   end;
end;

function Template_GetPointerTypeName(const AValue: string): string;
begin
   result := Format(GInfra.CurrentLang.PointerTypeMask, [AValue]);
end;

function Template_GetUserTypeDesc(ADataType: TUserDataType): string;
begin
   result := GInfra.CurrentLang.UserTypeDesc;
   if not result.IsEmpty then
   begin
      result := ReplaceStr(result, PRIMARY_PLACEHOLDER, ADataType.edtName.Text);
      var kind := '';
      if ADataType.rgTypeBox.ItemIndex <> -1 then
         kind := ADataType.rgTypeBox.Items[ADataType.rgTypeBox.ItemIndex];
      result := ReplaceStr(result, '%s2', kind);
   end;
end;

function Template_GetMainProgramDesc: string;
begin
   result := i18Manager.GetString(GInfra.CurrentLang.ProgramLabelKey);
   result := ReplaceStr(result, PRIMARY_PLACEHOLDER, GProject.Name);
end;

function Template_GetUserFuncDesc(AHeader: TUserFunctionHeader; AFullParams: boolean = True; AIncludeDesc: boolean = True): string;
begin
   result := '';
   if AHeader <> nil then
   begin
      var params := '';
      var lang := GInfra.CurrentLang;
      var arrayType := IfThen(AHeader.chkArrayType.Checked, lang.FunctionHeaderTypeArray, lang.FunctionHeaderTypeNotArray);
      var desc := IfThen(AIncludeDesc, AHeader.memDesc.Text);
      var lb := IfThen(AIncludeDesc, sLineBreak);
      var lType := AHeader.cbType.Text;
      var key := lang.ProcedureLabelKey;
      if AHeader.chkConstructor.Checked then
         key := lang.ConstructorLabelKey
      else if AHeader.cbType.ItemIndex > 0 then
         key := lang.FunctionLabelKey;
      if (AHeader.ParameterCount > 1) and not AFullParams then
         params := '...'
      else
      begin
         for var param in AHeader.GetParameters do
         begin
            if not params.IsEmpty then
               params := params + ', ';
            params := params + param.cbType.Text + IfThen(param.chkTable.Checked, '[ ] ', ' ') + Trim(param.edtName.Text);
         end;
      end;
      if not key.IsEmpty then
      begin
         result := i18Manager.GetString(key);
         result := ReplaceStr(result, LB_PHOLDER2, lb);
         result := ReplaceStr(result, PRIMARY_PLACEHOLDER, Trim(AHeader.edtName.Text));
         result := ReplaceStr(result, '%s2', params);
         result := ReplaceStr(result, '%s3', lType);
         result := ReplaceStr(result, '%s4', desc);
         result := ReplaceStr(result, '%s5', arrayType);
         result := ReplaceStr(result, '%s6', AHeader.GetExternModifier);
      end;
   end;
end;

function Template_GetUserFuncHeaderDesc(AHeader: TUserFunctionHeader): string;
begin
   result := '';
   var lang := GInfra.CurrentLang;
   if (AHeader <> nil) and not lang.FunctionHeaderDescTemplate.IsEmpty then
   begin
       var template := TStringList.Create;
       var parms := TStringList.Create;
       try
          template.Text := ReplaceStr(lang.FunctionHeaderDescTemplate, PRIMARY_PLACEHOLDER, Trim(AHeader.edtName.Text));
          template.Text := ReplaceStr(template.Text, '%s2', AHeader.cbType.Text);

          if not lang.FunctionHeaderDescParmMask.IsEmpty then
          begin
             var i := 1;
             for var parm in AHeader.GetParameters do
             begin
                var parmString := ReplaceStr(lang.FunctionHeaderDescParmMask, PRIMARY_PLACEHOLDER, Trim(parm.edtName.Text));
                parmString := ReplaceStr(parmString, '%s2', parm.cbType.Text);
                parmString := ReplaceStr(parmString, '%s3', Trim(parm.edtDefault.Text));
                parmString := ReplaceStr(parmString, '%s4', i.ToString);
                parms.Add(parmString);
                Inc(i);
             end;
          end;
          if parms.Count > 0 then
             TInfra.InsertTemplateLines(template, '%s3', parms)
          else
             TInfra.DeleteLinesContaining(template, '%s3');

          if AHeader.chkConstructor.Checked or (AHeader.cbType.ItemIndex = 0) then
             TInfra.DeleteLinesContaining(template, '%s4')
          else
             template.Text := ReplaceStr(template.Text, '%s4', ReplaceStr(lang.FunctionHeaderDescReturnMask, PRIMARY_PLACEHOLDER, AHeader.cbType.Text));

          result := template.Text;
       finally
          parms.Free;
          template.Free;
       end;
   end;
end;

initialization

   with GInfra.TemplateLang do
   begin
      EnabledUserFunctionBody := True;
      EnabledExplorer := True;
      MainFunctionSectionGenerator := Template_MainFunctionSectionGenerator;
      UserFunctionsSectionGenerator := Template_UserFunctionsSectionGenerator;
      VarSectionGenerator := Template_VarSectionGenerator;
      ProgramHeaderSectionGenerator := Template_ProgramHeaderSectionGenerator;
      LibSectionGenerator := Template_LibSectionGenerator;
      ConstSectionGenerator := Template_ConstSectionGenerator;
      UserDataTypesSectionGenerator := Template_UserDataTypesSectionGenerator;
      ProgramGenerator := Template_ProgramGenerator;
      GetConstantType := Template_GetConstantType;
      GetPointerTypeName := Template_GetPointerTypeName;
      GetUserFuncDesc := Template_GetUserFuncDesc;
      GetUserFuncHeaderDesc := Template_GetUserFuncHeaderDesc;
      GetUserTypeDesc := Template_GetUserTypeDesc;
      GetMainProgramDesc := Template_GetMainProgramDesc;
      SkipFuncBodyGen := Template_SkipFuncBodyGen;
   end;

   // it really sucks but this must be executed here due to initialization order
   GSettings.ResetCurrentLangName;

end.
