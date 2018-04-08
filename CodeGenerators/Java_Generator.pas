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
   IMPORT_MASK = 'import %s.%s;';
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
   FOutStreamImpl: TStringList;
   JAVA_INT_TYPE,
   JAVA_DOUBLE_TYPE,
   JAVA_CHAR_TYPE,
   JAVA_STRING_TYPE,
   JAVA_BOOLEAN_TYPE,
   JAVA_BIGDECIMAL_TYPE: integer;

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

procedure Java_LibSectionGenerator(ALines: TStringList);
var
   i: integer;
   libList: TStringList;
   typeName, libImport: string;
   pNativeType: PNativeDataType;
begin

   for i := 0 to High(javaLang.NativeDataTypes) do
   begin
      pNativeType := @javaLang.NativeDataTypes[i];
      if (pNativeType.Lib <> '') and CheckForDataType(pNativeType.Name) then
         ALines.Add(Format(IMPORT_MASK, [pNativeType.Lib, pNativeType.Name]));
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
            libImport := Format(IMPORT_MASK, [libList.Strings[i], typeName]);
            if ALines.IndexOf(libImport) = -1 then
               ALines.Add(libImport);
         end;
      end;
   finally
      libList.Free;
   end;

   FImportLines := ALines;
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
      implList := FOutStreamImpl;
   if implList <> nil then
   begin
      for i := 0 to implList.Count-1 do
      begin
         name := implList.Names[i];
         if AContents.Contains(name) then
            Exit(Format(IMPORT_MASK, [implList.Values[name], name]));
      end;
   end;
end;

procedure Java_VarSectionGenerator(ALines: TStringList; AVarList: TVarDeclareList);
var
   i, p1, p2: integer;
   varType, varInit, varInit2, varVal, varGeneric, libImport, varAccess, varSize, varName: string;
   dims: TArray<string>;
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
         if not varInit.IsEmpty then
         begin
            if TParserHelper.IsGenericType(varType) then
            begin
               p1 := Pos('<', varInit);
               if p1 > 0 then
               begin
                  p2 := LastDelimiter('>', varInit);
                  if p2 > p1 then
                     varGeneric := Copy(varInit, p1, p2-p1+1);
               end;
            end;
            libImport := ExtractImplementer(varType, varInit);
            if (libImport <> '') and (FImportLines <> nil) and (FImportLines.IndexOf(libImport) = -1) then
               FImportLines.Add(libImport);
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
            varAccess := IfThen(AVarList.IsExternal(i), 'public ', 'private ');
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
         typeAccess := IfThen(dataType.chkExtDeclare.Checked, javaLang.DataTypeExternal, javaLang.DataTypeNonExternal);
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
   i: integer;
   f: double;
begin
   result := UNKNOWN_TYPE;
   if not AValue.IsEmpty then
   begin
      if not TryStrToInt(AValue, i) then
      begin
         if not TryStrToFloat(AValue, f) then
         begin
            if AValue.StartsWith(JAVA_STRING_DELIM) and AValue.EndsWith(JAVA_STRING_DELIM) then
               result := JAVA_STRING_TYPE
            else if (AValue.Length = 3) and (AValue[1] = JAVA_CHAR_DELIM) and (AValue[3] = JAVA_CHAR_DELIM) then
               result := JAVA_CHAR_TYPE
            else if TInfra.SameStrings(AValue, 'null') then
               result := GENERIC_PTR_TYPE
            else if TInfra.SameStrings(AValue, 'true') or TInfra.SameStrings(AValue, 'false') then
               result := JAVA_BOOLEAN_TYPE
            else if AValue.Contains('BigDecimal') then
               result := JAVA_BIGDECIMAL_TYPE;
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
   end;
end;


initialization

   JAVA_INT_TYPE        := TParserHelper.GetType('int', JAVA_LANG_ID);
   JAVA_DOUBLE_TYPE     := TParserHelper.GetType('double', JAVA_LANG_ID);
   JAVA_CHAR_TYPE       := TParserHelper.GetType('char', JAVA_LANG_ID);
   JAVA_STRING_TYPE     := TParserHelper.GetType('String', JAVA_LANG_ID);
   JAVA_BOOLEAN_TYPE    := TParserHelper.GetType('boolean', JAVA_LANG_ID);
   JAVA_BIGDECIMAL_TYPE := TParserHelper.GetType('BigDecimal', JAVA_LANG_ID);

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
   begin
      javaLang.ExecuteBeforeGeneration :=  Java_ExecuteBeforeGeneration;
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

end.
