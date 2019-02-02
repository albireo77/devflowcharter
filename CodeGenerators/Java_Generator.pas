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



{ This unit contains stuff to support Java code generation }

unit Java_Generator;

interface

implementation

uses
   System.Classes, System.SysUtils, System.StrUtils, Vcl.Graphics, Vcl.ComCtrls,
   System.Character, SynHighlighterJava, DeclareList, ApplicationCommon, UserDataType,
   UserFunction, LangDefinition, ParserHelper, CommonTypes;

const
   JAVA_STRING_DELIM = #34;
   JAVA_CHAR_DELIM   = #39;

var
   javaLang: TLangDefinition;
   FImportLines: TStringList;
   FListImpl,
   FMapImpl,
   FSetImpl,
   FDateFormatImpl,
   FQueueImpl,
   FDequeImpl,
   FReaderImpl,
   FWriterImpl,
   FInStreamImpl,
   FOutStreamImpl,
   FTemporalImpl,
   FNumberImpl: TStringList;
   JAVA_INT_TYPE,
   JAVA_LONG_TYPE,
   JAVA_FLOAT_TYPE,
   JAVA_DOUBLE_TYPE,
   JAVA_CHAR_TYPE,
   JAVA_STRING_TYPE,
   JAVA_BOOLEAN_TYPE,
   JAVA_DATE_TYPE,
   JAVA_CALENDAR_TYPE,
   JAVA_LOCAL_DATETIME_TYPE,
   JAVA_LOCAL_DATE_TYPE,
   JAVA_LOCAL_TIME_TYPE,
   JAVA_INSTANT_TYPE,
   JAVA_DURATION_TYPE,
   JAVA_PERIOD_TYPE,
   JAVA_BIGDECIMAL_TYPE,
   JAVA_BIGINTEGER_TYPE: integer;

procedure AddLibImport(const ALib: string);
var
   importLib: string;
begin
   if (FImportLines <> nil) and not ALib.IsEmpty then
   begin
      importLib := Format(javaLang.LibEntry, [ALib]);
      if (importLib <> '') and (FImportLines.IndexOf(importLib) = -1) then
         FImportLines.AddObject(importLib, TInfra.GetLibObject);
   end;
end;

function CheckForDataType(const AType: string): boolean;
var
   varList: TVarDeclareList;
   constList: TConstDeclareList;
   i: integer;
   typeStr: string;
   dataType: TUserDataType;
   field: TField;
   func: TUserFunction;
   param: TParameter;
begin

   result := false;

   // search in instance variables
   varList := GProject.GlobalVars;
   if varList <> nil then
   begin
      for i := 1 to varList.sgList.RowCount-2 do
      begin
         typeStr := varList.sgList.Cells[VAR_TYPE_COL, i];
         if typeStr = AType then
            Exit(true);
      end;
   end;

   // search in instance constants
   constList := GProject.GlobalConsts;
   if constList <> nil then
   begin
      for i := 1 to constList.sgList.RowCount-2 do
      begin
         typeStr := TParserHelper.GetTypeAsString(TParserHelper.GetConstType(constList.sgList.Cells[CONST_NAME_COL, i]));
         if typeStr = AType then
            Exit(true);
      end;
   end;

   // search in data types
   for dataType in GProject.GetUserDataTypes do
   begin
      if not dataType.GetName.IsEmpty then
      begin
         for field in dataType.GetFields do
         begin
            if field.cbType.Enabled and (field.cbType.Text = AType) then
               Exit(true);
         end;
      end;
   end;

   // search in functions
   for func in GProject.GetUserFunctions do
   begin
      if (func.Header <> nil) and not func.GetName.IsEmpty then
      begin
         if func.Header.cbType.Text = AType then
            Exit(true);
         varList := func.Header.LocalVars;
         if varList <> nil then
         begin
            for i := 1 to varList.sgList.RowCount-2 do
            begin
               typeStr := varList.sgList.Cells[VAR_TYPE_COL, i];
               if typeStr = AType then
                  Exit(true);
            end;
         end;
         for param in func.Header.GetParameters do
         begin
            if param.cbType.Text = AType then
               Exit(true);
         end;
      end;
   end;
end;

procedure Java_ExecuteBeforeGeneration;
begin
   FImportLines := nil;
end;

procedure Java_ExecuteAfterGeneration;
begin
   FImportLines := nil;
end;

procedure Java_LibSectionGenerator(ALines: TStringList);
var
   i: integer;
   libList: TStringList;
   typeName, libImport: string;
   pNativeType: PNativeDataType;
begin

   FImportLines := ALines;

   for i := 0 to High(javaLang.NativeDataTypes) do
   begin
      pNativeType := @javaLang.NativeDataTypes[i];
      if (pNativeType.Lib <> '') and CheckForDataType(pNativeType.Name) then
      begin
         libImport := pNativeType.Lib + '.' + pNativeType.Name;
         AddLibImport(libImport);
      end;
   end;

   libList := GProject.GetLibraryList;
   try
      for i := 0 to libList.Count-1 do
      begin
         typeName := '';
         if libList.Objects[i] is TTabSheet then
            typeName := TTabSheet(libList.Objects[i]).Caption;
         if not typeName.IsEmpty then
         begin
            libImport := libList.Strings[i] + '.' + typeName;
            AddLibImport(libImport);
         end;
      end;
   finally
      libList.Free;
   end;
end;

function ExtractImplementer(const ATypeName: string; const AContents: string): string;
var
   i: integer;
   implList: TStringList;
   name: string;
begin
   result := '';
   implList := nil;
   if ATypeName.EndsWith('List') then
      implList := FListImpl
   else if ATypeName.EndsWith('Map') then
      implList := FMapImpl
   else if ATypeName.EndsWith('Set') then
      implList := FSetImpl
   else if ATypeName.EndsWith('DateFormat') then
      implList := FDateFormatImpl
   else if ATypeName.EndsWith('Queue') then
      implList := FQueueImpl
   else if ATypeName.EndsWith('Deque') then
      implList := FDequeImpl
   else if ATypeName.EndsWith('Reader') then
      implList := FReaderImpl
   else if ATypeName.EndsWith('Writer') then
      implList := FWriterImpl
   else if ATypeName.EndsWith('InputStream') then
      implList := FInStreamImpl
   else if ATypeName.EndsWith('OutputStream') then
      implList := FOutStreamImpl
   else if ATypeName.EndsWith('Temporal') then
      implList := FTemporalImpl
   else if ATypeName.EndsWith('Number') then
      implList := FNumberImpl;
   if implList <> nil then
   begin
      for i := 0 to implList.Count-1 do
      begin
         name := implList.Names[i];
         if AContents.Contains(name) then
            Exit(implList.Values[name] + '.' + name);
      end;
   end;
end;

procedure Java_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   i, p1, p2, a, b: integer;
   varType, varInit, varInit2, varVal, varGeneric, varGenericType, libImport, varAccess, varSize, varName: string;
   dims: TArray<string>;
   pNativeType: PNativeDataType;
   tokens: TArray<string>;
begin
   if AVarList <> nil then
   begin
      for i := 1 to AVarList.sgList.RowCount-2 do
      begin
         varName := AVarList.sgList.Cells[VAR_NAME_COL, i];
         varType :=  AVarList.sgList.Cells[VAR_TYPE_COL, i];
         varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
         varGeneric := '';
         varSize := '';
         varAccess := '';
         p1 := 0;
         p2 := 0;
         if not varInit.IsEmpty then
         begin
            if TParserHelper.IsGenericType(varType) then
            begin
               p1 := Pos('<', varInit);
               if p1 > 0 then
               begin
                  p2 := LastDelimiter('>', varInit);
                  if p2 > p1 then
                  begin
                     varGeneric := Copy(varInit, p1, p2-p1+1);
                     varGenericType := Copy(varInit, p1+1, p2-p1-1);
                     varGenericType := ReplaceStr(varGenericType, ' ', '');
                     tokens := varGenericType.Split([',']);
                     for b := 0 to High(tokens) do
                     begin
                       for a := 0 to High(javaLang.NativeDataTypes) do
                       begin
                          pNativeType := @javaLang.NativeDataTypes[a];
                          if (pNativeType.Lib <> '') and (pNativeType.Name = tokens[b]) then
                          begin
                             libImport := pNativeType.Lib + '.' + pNativeType.Name;
                             AddLibImport(libImport);
                             break;
                          end;
                       end;
                     end;
                  end;
               end;
            end;
            libImport := ExtractImplementer(varType, varInit);
            AddLibImport(libImport);
            if p2 > p1 then
               Delete(varInit, p1+1, p2-p1-1);      // make diamond operator
            varInit := ' = ' + varInit;
         end;
         p1 := AVarList.GetDimensionCount(varName);
         if p1 > 0 then
         begin
            varInit2 := '';
            dims := AVarList.GetDimensions(varName);
            if dims <> nil then
            begin
               for p2 := 0 to High(dims) do
               begin
                  varSize := varSize + Format(javaLang.VarEntryArraySize, [dims[p2]]);
                  varInit2 := varInit2 + '[' + dims[p2] + ']';
               end;
               if varInit.IsEmpty then
                  varInit := ' = new ' + varType + varInit2;
               if javaLang.VarEntryArraySizeStripCount > 0 then
                  SetLength(varSize, varSize.Length - javaLang.VarEntryArraySizeStripCount);
            end;
         end;
         if AVarList = GProject.GlobalVars then
            varAccess := AVarList.GetExternModifier(i);
         varVal := varAccess + varType + varGeneric + varSize + ' ' + varName + varInit + ';';
         ALines.AddObject(varVal, AVarList);
      end;
   end;
end;

procedure Java_UserDataTypesSectionGenerator(ALines: TStringList);
var
   name, line, fieldSize, fieldName, fieldType, funcStrU, typeAccess, indent: string;
   dataType: TUserDataType;
   field: TField;
   i: integer;
begin
   i := 0;
   for dataType in GProject.GetUserDataTypes do
   begin
      name := dataType.GetName;
      if not name.IsEmpty then
      begin
         indent := GSettings.IndentString;
         typeAccess := IfThen(dataType.chkExternal.Checked, javaLang.DataTypeExternal, javaLang.DataTypeNonExternal);
         if dataType.Kind = dtRecord then
         begin
            if i > 0 then
               ALines.AddObject('', dataType);
            line := typeAccess + 'class ' + name + ' {';
            ALines.AddObject(line, dataType);
            if dataType.FieldCount > 0 then
            begin
               ALines.AddObject('', dataType);
               i := ALines.Count;
               ALines.AddObject('', dataType);
               for field in dataType.GetFields do
               begin
                  fieldSize := javaLang.GetArraySizes(field.edtSize);
                  fieldName := Trim(field.edtName.Text);
                  fieldType := field.cbType.Text;
                  line := indent + 'private ' + fieldType + fieldSize + ' ' + fieldName + ';';
                  ALines.InsertObject(i, line, dataType);
                  funcStrU := fieldName;
                  funcStrU[1] := funcStrU[1].ToUpper;
                  line := indent + 'public ' + fieldType + fieldSize + ' get' + funcStrU + '() {';
                  ALines.AddObject(line, dataType);
                  line := indent + indent + 'return ' + fieldName + ';';
                  ALines.AddObject(line, dataType);
                  ALines.AddObject(indent + '}', dataType);
                  line := indent + 'public void set' + funcStrU + '(' + fieldType + fieldSize + ' ' + fieldName + ') {';
                  ALines.AddObject(line, dataType);
                  line := indent + indent + 'this.' + fieldName + ' = ' + fieldName + ';';
                  ALines.AddObject(line, dataType);
                  ALines.AddObject(indent + '}', dataType);
                  i := i + 1;
               end;
            end;
            ALines.AddObject('}', dataType);
            i := 1;
         end
         else if dataType.Kind = dtEnum then
         begin
            if i > 0 then
               ALines.AddObject('', dataType);
            line := typeAccess + 'enum ' + name + ' {';
            ALines.AddObject(line, dataType);
            if dataType.FieldCount > 0 then
            begin
               line := indent;
               for field in dataType.GetFields do
                  line := line + Trim(field.edtName.Text).ToUpper + ', ';
               SetLength(line, Length(line)-2);
               ALines.AddObject(line, dataType);
            end;
            ALines.AddObject('}', dataType);
            i := 1;
         end;
      end;
   end;
end;

function Java_GetLiteralType(const AValue: string): integer;
var
   i, len, a: integer;
   i64: Int64;
   f: double;
   firstChar, lastChar: char;
   cValue: string;
begin
   result := UNKNOWN_TYPE;
   len := AValue.Length;
   if len > 0 then
   begin
      firstChar := AValue[1];
      lastChar := AValue[len];
      if firstChar = '$' then
         Exit;
      if not TryStrToInt(AValue, i) then
      begin
         if len = 1 then
            Exit;
         if not TryStrToFloat(AValue, f) then
         begin
            if (firstChar = JAVA_STRING_DELIM) and (lastChar = JAVA_STRING_DELIM) then
               result := JAVA_STRING_TYPE
            else if (len > 2) and (firstChar = JAVA_CHAR_DELIM) and (lastChar = JAVA_CHAR_DELIM) then
            begin
               cValue := Copy(AValue, 2, len-2);
               i := cValue.Length;
               if cValue[1] = '\' then
               begin
                  if (i = 2) and (LastDelimiter('0btnfr"\'#39, cValue) = 2) then
                     result := JAVA_CHAR_TYPE
                  else if (i = 6) and ((cValue[2] = 'u') or (cValue[2] = 'U')) then
                  begin
                     for a := 3 to 6 do
                     begin
                        if not CharInSet(cValue[a], ['0'..'9', 'a'..'f', 'A'..'F']) then
                           Exit;
                     end;
                     result := JAVA_CHAR_TYPE;
                  end;
               end
               else if i = 1 then
                  result := JAVA_CHAR_TYPE;
            end
            else if AValue = 'new Date()' then
               result := JAVA_DATE_TYPE
            else if AValue = 'Calendar.getInstance()' then
               result := JAVA_CALENDAR_TYPE
            else if (AValue = 'LocalDateTime.now()') or AValue.StartsWith('LocalDateTime.of(') then
               result := JAVA_LOCAL_DATETIME_TYPE
            else if (AValue = 'LocalDate.now()') or AValue.StartsWith('LocalDate.of(') then
               result := JAVA_LOCAL_DATE_TYPE
            else if (AValue = 'LocalTime.now()') or AValue.StartsWith('LocalTime.of(') then
               result := JAVA_LOCAL_TIME_TYPE
            else if AValue.StartsWith('Duration.') then
            begin
               if AValue.EndsWith('.toDays()') or AValue.EndsWith('.toHours()') or (AValue.EndsWith('.toMillis()')) or
                  AValue.EndsWith('.toMinutes()') or AValue.EndsWith('.toNanos()') then
                  result := JAVA_LONG_TYPE
               else if AValue.EndsWith('.toString()') then
                  result := JAVA_STRING_TYPE
               else
                  result := JAVA_DURATION_TYPE;
               if result <> JAVA_DURATION_TYPE then
                  AddLibImport('java.time.Duration');
            end
            else if AValue.StartsWith('Period.') then
            begin
               if AValue.EndsWith('.getDays()') or AValue.EndsWith('.getMonths()') or (AValue.EndsWith('.getYears()')) then
                  result := JAVA_INT_TYPE
               else if AValue.EndsWith('.toString()') then
                  result := JAVA_STRING_TYPE
               else
                  result := JAVA_PERIOD_TYPE;
               if result <> JAVA_PERIOD_TYPE then
                  AddLibImport('java.time.Period');
            end
            else if AValue.StartsWith('Instant.') then
            begin
               if AValue.EndsWith('.getNano()') then
                  result := JAVA_INT_TYPE
               else if AValue.EndsWith('.toEpochMilli()') then
                  result := JAVA_LONG_TYPE
               else if AValue.EndsWith('.toString()') then
                  result := JAVA_STRING_TYPE
               else
                  result := JAVA_INSTANT_TYPE;
               if result <> JAVA_INSTANT_TYPE then
                  AddLibImport('java.time.Instant');
            end
            else if AValue.Contains('System.currentTimeMillis()') then
               result := JAVA_LONG_TYPE
            else if AValue.Contains('Math.E') or AValue.Contains('Math.PI') then
               result := JAVA_DOUBLE_TYPE
            else if AValue = 'null' then
               result := JAVA_STRING_TYPE
            else if MatchStr(AValue, ['true', 'false']) then
               result := JAVA_BOOLEAN_TYPE
            else if AValue.Contains('BigDecimal') then
            begin
               if AValue.Contains('.longValue()') then
                  result := JAVA_LONG_TYPE
               else if AValue.Contains('.intValue()') then
                  result := JAVA_INT_TYPE
               else if AValue.Contains('.doubleValue()') then
                  result := JAVA_DOUBLE_TYPE
               else if AValue.Contains('.floatValue()') then
                  result := JAVA_FLOAT_TYPE
               else if AValue.EndsWith('.toString()') or AValue.EndsWith('.toPlainString()') then
                  result := JAVA_STRING_TYPE
               else
                  result := JAVA_BIGDECIMAL_TYPE;
               if result <> JAVA_BIGDECIMAL_TYPE then
                  AddLibImport('java.math.BigDecimal');
            end
            else if AValue.Contains('BigInteger') then
            begin
               if AValue.Contains('.longValue()') then
                  result := JAVA_LONG_TYPE
               else if AValue.Contains('.intValue()') then
                  result := JAVA_INT_TYPE
               else if AValue.Contains('.doubleValue()') then
                  result := JAVA_DOUBLE_TYPE
               else if AValue.Contains('.floatValue()') then
                  result := JAVA_FLOAT_TYPE
               else if AValue.Contains('.toString(') then
                  result := JAVA_STRING_TYPE
               else
                  result := JAVA_BIGINTEGER_TYPE;
               if result <> JAVA_BIGINTEGER_TYPE then
                  AddLibImport('java.math.BigInteger');
            end
            else if TryStrToInt64(AValue, i64) then
               result := JAVA_LONG_TYPE
            else
            begin
               cValue := Copy(AValue, 1, len-1);
               case lastChar of
                  'l', 'L':
                  if TryStrToInt64(cValue, i64) then
                     result := JAVA_LONG_TYPE;
                  'd', 'D':
                  if TryStrToFloat(cValue, f) then
                     result := JAVA_DOUBLE_TYPE;
                  'f', 'F':
                  if TryStrToFloat(cValue, f) then
                     result := JAVA_FLOAT_TYPE;
               end;
            end;
         end
         else
            result := JAVA_DOUBLE_TYPE;
      end
      else
         result := JAVA_INT_TYPE;
   end;
end;

procedure Java_SetHLighterAttrs;
var
   hlighter: TSynJavaSyn;
   bkgColor: TColor;
begin
   if (javaLang <> nil) and (javaLang.HighLighter is TSynJavaSyn) then
   begin
      bkgColor := GSettings.EditorBkgColor;
      hlighter := TSynJavaSyn(javaLang.HighLighter);
      hlighter.StringAttri.Foreground     := GSettings.EditorStringColor;
      hlighter.StringAttri.Background     := bkgColor;
      hlighter.NumberAttri.Foreground     := GSettings.EditorNumberColor;
      hlighter.NumberAttri.Background     := bkgColor;
      hlighter.CommentAttri.Foreground    := GSettings.EditorCommentColor;
      hlighter.CommentAttri.Background    := bkgColor;
      hlighter.KeyAttri.Foreground        := GSettings.EditorKeywordColor;
      hlighter.KeyAttri.Background        := bkgColor;
      hlighter.IdentifierAttri.Foreground := GSettings.EditorIdentColor;
      hlighter.IdentifierAttri.Background := bkgColor;
      hlighter.DocumentAttri.Foreground   := clHotLight;
      hlighter.DocumentAttri.Background   := bkgColor;
   end;
end;


initialization

   JAVA_INT_TYPE            := TParserHelper.GetType('int', JAVA_LANG_ID);
   JAVA_LONG_TYPE           := TParserHelper.GetType('long', JAVA_LANG_ID);
   JAVA_FLOAT_TYPE          := TParserHelper.GetType('float', JAVA_LANG_ID);
   JAVA_DOUBLE_TYPE         := TParserHelper.GetType('double', JAVA_LANG_ID);
   JAVA_CHAR_TYPE           := TParserHelper.GetType('char', JAVA_LANG_ID);
   JAVA_STRING_TYPE         := TParserHelper.GetType('String', JAVA_LANG_ID);
   JAVA_BOOLEAN_TYPE        := TParserHelper.GetType('boolean', JAVA_LANG_ID);
   JAVA_DATE_TYPE           := TParserHelper.GetType('Date', JAVA_LANG_ID);
   JAVA_CALENDAR_TYPE       := TParserHelper.GetType('Calendar', JAVA_LANG_ID);
   JAVA_LOCAL_DATETIME_TYPE := TParserHelper.GetType('LocalDateTime', JAVA_LANG_ID);
   JAVA_LOCAL_DATE_TYPE     := TParserHelper.GetType('LocalDate', JAVA_LANG_ID);
   JAVA_LOCAL_TIME_TYPE     := TParserHelper.GetType('LocalTime', JAVA_LANG_ID);
   JAVA_INSTANT_TYPE        := TParserHelper.GetType('Instant', JAVA_LANG_ID);
   JAVA_DURATION_TYPE       := TParserHelper.GetType('Duration', JAVA_LANG_ID);
   JAVA_PERIOD_TYPE         := TParserHelper.GetType('Period', JAVA_LANG_ID);
   JAVA_BIGDECIMAL_TYPE     := TParserHelper.GetType('BigDecimal', JAVA_LANG_ID);
   JAVA_BIGINTEGER_TYPE     := TParserHelper.GetType('BigInteger', JAVA_LANG_ID);

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
   begin
      javaLang.ExecuteBeforeGeneration :=  Java_ExecuteBeforeGeneration;
      javaLang.ExecuteAfterGeneration :=  Java_ExecuteAfterGeneration;
      javaLang.LibSectionGenerator := Java_LibSectionGenerator;
      javaLang.VarSectionGenerator := Java_VarSectionGenerator;
      javaLang.UserDataTypesSectionGenerator := Java_UserDataTypesSectionGenerator;
      javaLang.GetLiteralType := Java_GetLiteralType;
      javaLang.SetHLighterAttrs := Java_SetHLighterAttrs;
   end;

   FListImpl := TStringList.Create;
   FListImpl.AddPair('CopyOnWriteArrayList', 'java.util.concurrent');
   FListImpl.AddPair('ArrayList', 'java.util');
   FListImpl.AddPair('LinkedList', 'java.util');
   FListImpl.AddPair('Stack', 'java.util');
   FListImpl.AddPair('Vector', 'java.util');

   FMapImpl := TStringList.Create;
   FMapImpl.AddPair('ConcurrentHashMap', 'java.util.concurrent');
   FMapImpl.AddPair('ConcurrentSkipListMap', 'java.util.concurrent');
   FMapImpl.AddPair('EnumMap', 'java.util');
   FMapImpl.AddPair('WeakHashMap', 'java.util');
   FMapImpl.AddPair('LinkedHashMap', 'java.util');
   FMapImpl.AddPair('HashMap', 'java.util');
   FMapImpl.AddPair('Hashtable', 'java.util');
   FMapImpl.AddPair('Properties', 'java.util');
   FMapImpl.AddPair('TreeMap', 'java.util');

   FSetImpl := TStringList.Create;
   FSetImpl.AddPair('ConcurrentSkipListSet', 'java.util.concurrent');
   FSetImpl.AddPair('CopyOnWriteArraySet', 'java.util.concurrent');
   FSetImpl.AddPair('EnumSet', 'java.util');
   FSetImpl.AddPair('LinkedHashSet', 'java.util');
   FSetImpl.AddPair('HashSet', 'java.util');
   FSetImpl.AddPair('TreeSet', 'java.util');

   FDateFormatImpl := TStringList.Create;
   FDateFormatImpl.AddPair('SimpleDateFormat', 'java.text');

   FQueueImpl := TStringList.Create;
   FQueueImpl.AddPair('ArrayBlockingQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('ArrayDeque', 'java.util');
   FQueueImpl.AddPair('ConcurrentLinkedDeque', 'java.util.concurrent');
   FQueueImpl.AddPair('ConcurrentLinkedQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('DelayQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('LinkedBlockingDeque', 'java.util.concurrent');
   FQueueImpl.AddPair('LinkedBlockingQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('LinkedList', 'java.util');
   FQueueImpl.AddPair('LinkedTransferQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('PriorityBlockingQueue', 'java.util.concurrent');
   FQueueImpl.AddPair('PriorityQueue', 'java.util');
   FQueueImpl.AddPair('SynchronousQueue', 'java.util.concurrent');

   FDequeImpl := TStringList.Create;
   FDequeImpl.AddPair('ArrayDeque', 'java.util');
   FDequeImpl.AddPair('ConcurrentLinkedDeque', 'java.util.concurrent');
   FDequeImpl.AddPair('LinkedBlockingDeque', 'java.util.concurrent');
   FDequeImpl.AddPair('LinkedList', 'java.util');

   FReaderImpl := TStringList.Create;
   FReaderImpl.AddPair('BufferedReader', 'java.io');
   FReaderImpl.AddPair('CharArrayReader', 'java.io');
   FReaderImpl.AddPair('InputStreamReader', 'java.io');
   FReaderImpl.AddPair('PipedReader', 'java.io');
   FReaderImpl.AddPair('StringReader', 'java.io');

   FWriterImpl := TStringList.Create;
   FWriterImpl.AddPair('BufferedWriter', 'java.io');
   FWriterImpl.AddPair('CharArrayWriter', 'java.io');
   FWriterImpl.AddPair('OutputStreamWriter', 'java.io');
   FWriterImpl.AddPair('PipedWriter', 'java.io');
   FWriterImpl.AddPair('PrintWriter', 'java.io');
   FWriterImpl.AddPair('StringWriter', 'java.io');

   FInStreamImpl := TStringList.Create;
   FInStreamImpl.AddPair('AudioInputStream', 'javax.sound.sampled');
   FInStreamImpl.AddPair('ByteArrayInputStream', 'java.io');
   FInStreamImpl.AddPair('FileInputStream', 'java.io');
   FInStreamImpl.AddPair('FilterInputStream', 'java.io');
   FInStreamImpl.AddPair('ObjectInputStream', 'java.io');
   FInStreamImpl.AddPair('PipedInputStream', 'java.io');
   FInStreamImpl.AddPair('SequenceInputStream', 'java.io');
   FInStreamImpl.AddPair('BufferedInputStream', 'java.io');
   FInStreamImpl.AddPair('CheckedInputStream', 'java.util.zip');
   FInStreamImpl.AddPair('CipherInputStream', 'javax.crypto');
   FInStreamImpl.AddPair('DataInputStream', 'java.io');
   FInStreamImpl.AddPair('DeflaterInputStream', 'java.util.zip');
   FInStreamImpl.AddPair('InflaterInputStream', 'java.util.zip');
   FInStreamImpl.AddPair('DigestInputStream', 'java.security');
   FInStreamImpl.AddPair('PushbackInputStream', 'java.io');

   FOutStreamImpl := TStringList.Create;
   FOutStreamImpl.AddPair('ByteArrayOutputStream', 'java.io');
   FOutStreamImpl.AddPair('FileOutputStream', 'java.io');
   FOutStreamImpl.AddPair('FilterOutputStream', 'java.io');
   FOutStreamImpl.AddPair('ObjectOutputStream', 'java.io');
   FOutStreamImpl.AddPair('PipedOutputStream', 'java.io');
   FOutStreamImpl.AddPair('BufferedOutputStream', 'java.io');
   FOutStreamImpl.AddPair('CipherOutputStream', 'javax.crypto');
   FOutStreamImpl.AddPair('DataOutputStream', 'java.io');
   FOutStreamImpl.AddPair('InflaterOutputStream', 'java.util.zip');
   FOutStreamImpl.AddPair('DeflaterOutputStream', 'java.util.zip');
   FOutStreamImpl.AddPair('DigestOutputStream', 'java.security');
   FOutStreamImpl.AddPair('PrintStream', 'java.io');

   FTemporalImpl := TStringList.Create;
   FTemporalImpl.AddPair('Instant', 'java.time');
   FTemporalImpl.AddPair('LocalDateTime', 'java.time');
   FTemporalImpl.AddPair('LocalDate', 'java.time');
   FTemporalImpl.AddPair('LocalTime', 'java.time');
   FTemporalImpl.AddPair('OffsetDateTime', 'java.time');
   FTemporalImpl.AddPair('OffsetTime', 'java.time');
   FTemporalImpl.AddPair('ZonedDateTime', 'java.time');

   FNumberImpl := TStringList.Create;
   FNumberImpl.AddPair('AtomicInteger', 'java.util.concurrent.atomic');
   FNumberImpl.AddPair('AtomicLong', 'java.util.concurrent.atomic');
   FNumberImpl.AddPair('BigDecimal', 'java.math');
   FNumberImpl.AddPair('BigInteger', 'java.math');
   FNumberImpl.AddPair('DoubleAccumulator', 'java.util.concurrent.atomic');
   FNumberImpl.AddPair('DoubleAdder', 'java.util.concurrent.atomic');
   FNumberImpl.AddPair('LongAccumulator', 'java.util.concurrent.atomic');
   FNumberImpl.AddPair('LongAdder', 'java.util.concurrent.atomic');

finalization

   FListImpl.Free;
   FMapImpl.Free;
   FSetImpl.Free;
   FDateFormatImpl.Free;
   FQueueImpl.Free;
   FDequeImpl.Free;
   FReaderImpl.Free;
   FWriterImpl.Free;
   FInStreamImpl.Free;
   FOutStreamImpl.Free;
   FTemporalImpl.Free;
   FNumberImpl.Free;

end.
