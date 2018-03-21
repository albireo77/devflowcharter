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



{ This unit contains stuff to support Java language }

unit Java_Template;

interface

implementation

uses
   System.Classes, System.SysUtils, Vcl.Graphics, Vcl.ComCtrls, SynHighlighterJava, DeclareList,
   ApplicationCommon, UserDataType, UserFunction, LangDefinition, ParserHelper,
   CommonTypes;

const
   IMPORT_MASK = 'import %s.%s;';

var
   javaLang: TLangDefinition;
   FImportLinesPos: integer;
   FImportLines: TStringList;
   FListImpl,
   FMapImpl,
   FSetImpl,
   FDateFormatImpl,
   FQueueImpl,
   FDequeImpl,
   FReaderImpl,
   FWriterImpl: TStringList;

function CheckForDataType(const AType: string): boolean;
var
   varList: TVarDeclareList;
   i: integer;
   varType: string;
   dataType: TUserDataType;
   field: TField;
   func: TUserFunction;
   param: TParameter;
begin

   result := false;

   // search in global variables
   varList := GProject.GlobalVars;
   if varList <> nil then
   begin
      for i := 1 to varList.sgList.RowCount-2 do
      begin
         varType := varList.sgList.Cells[VAR_TYPE_COL, i];
         if varType = AType then
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
               varType := varList.sgList.Cells[VAR_TYPE_COL, i];
               if varType = AType then
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

   FImportLinesPos := ALines.Count;
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
   if ATypeName = 'List' then
      implList := FListImpl
   else if ATypeName = 'Map' then
      implList := FMapImpl
   else if ATypeName = 'Set' then
      implList := FSetImpl
   else if ATypeName = 'DateFormat' then
      implList := FDateFormatImpl
   else if ATypeName = 'Queue' then
      implList := FQueueImpl
   else if ATypeName = 'Deque' then
      implList := FDequeImpl
   else if ATypeName = 'Reader' then
      implList := FReaderImpl
   else if ATypeName = 'Writer' then
      implList := FWriterImpl;
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
   varType, varInit, varVal, gener, libImport: string;
begin
   if AVarList <> nil then
   begin
      for i := 1 to AVarList.sgList.RowCount-2 do
      begin
         varType :=  AVarList.sgList.Cells[VAR_TYPE_COL, i];
         varInit := AVarList.sgList.Cells[VAR_INIT_COL, i];
         gener := '';
         if not varInit.IsEmpty then
         begin
            if TParserHelper.IsGenericType(varType) then
            begin
               p1 := Pos('<', varInit);
               if p1 > 0 then
               begin
                  p2 := LastDelimiter('>', varInit);
                  if p2 > p1 then
                     gener := Copy(varInit, p1, p2-p1+1);
               end;
            end;
            libImport := ExtractImplementer(varType, varInit);
            if (libImport <> '') and (FImportLines.IndexOf(libImport) = -1) then
               FImportLines.Insert(FImportLinesPos, libImport);
            varInit := ' = ' + varInit
         end;
         varVal := varType + gener + ' ' + AVarList.sgList.Cells[VAR_NAME_COL, i] + varInit + ';';
         ALines.Add(varVal);
      end;
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

   javaLang := GInfra.GetLangDefinition(JAVA_LANG_ID);
   if javaLang <> nil then
   begin
      javaLang.LibSectionGenerator := Java_LibSectionGenerator;
      javaLang.VarSectionGenerator := Java_VarSectionGenerator;
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

finalization

   FListImpl.Free;
   FMapImpl.Free;
   FSetImpl.Free;
   FDateFormatImpl.Free;
   FQueueImpl.Free;
   FDequeImpl.Free;
   FReaderImpl.Free;
   FWriterImpl.Free;

end.
