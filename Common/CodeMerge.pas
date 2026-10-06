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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1
   301, USA.
}



unit CodeMerge;

// Keeps what the user changed in code editor when code is generated again from flowchart.
// Every generated line is identified by flowchart object it belongs to rather than by its text,
// and every change the user made is attached to such line: text appended to it or put in its place,
// its removal and lines the user wrote in front of it. Freshly generated code is then walked line
// by line and each line brings along what the user attached to it, so user changes follow their
// lines when blocks move, change or disappear and come back (undo of removal).

interface

uses
   System.Classes, System.SysUtils, System.Generics.Collections;

type

   TLinesDiff = record
      Map1: TArray<integer>;    // for each line of the first list, index of the matching line in the second one
      Map2: TArray<integer>;    // for each line of the second list, index of the matching line in the first one
   end;

   // project ids of flowchart objects lines are bound to, keyed by object reference turned into a number,
   // as objects of previously generated lines may be freed already and must not be touched
   TObjectIds = TDictionary<NativeInt, integer>;

   TUserEditKind = (ueKept, ueAppended, ueChanged, ueDeleted);

   // what the user made of one previously generated line
   TUserEdit = record
      Kind: TUserEditKind;
      Text: string;                    // text appended for ueAppended, whole line for ueChanged
      Leading: TArray<string>;         // lines the user wrote in front of it
   end;

   TDetachedLine = record
      Text: string;                    // line as generated last time
      Edit: TUserEdit;
   end;

   TDetachedObject = record
      Obj: NativeInt;
      Lines: TArray<TDetachedLine>;
   end;

   // User changes of lines whose flowchart object no longer generates code (e.g. removed block),
   // kept for when the object generates code again (e.g. removal is undone)
   TDetachedLines = class
   private
      FItems: TDictionary<integer, TDetachedObject>;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Purge(AIsAlive: TFunc<integer, NativeInt, boolean>);
   end;

function DiffLines(ALines1, ALines2: TStrings): TLinesDiff;
function MergeLines(ABase, AEditor, ANew: TStrings; ABaseIds: TObjectIds = nil; ANewIds: TObjectIds = nil;
                    ADetached: TDetachedLines = nil): TStringList;

implementation

uses
   Constants;

type
   // Myers' diff in linear space: finds the longest sequence of lines common to two lists
   // in time proportional to their length times the number of differing lines
   TLineMatcher = record
      Ids1, Ids2: TArray<integer>;   // lines turned into numbers so that comparing them is cheap
      Map1, Map2: TArray<integer>;
      Offset: integer;
      procedure Pair(AIndex1, AIndex2: integer);
      procedure Match(AFrom1, ATo1, AFrom2, ATo2: integer);
      function FindSplit(AFrom1, ATo1, AFrom2, ATo2: integer; out ASplit1, ASplit2: integer): boolean;
   end;

   TCodeMerger = class
   private
      FBase, FEditor, FNew: TArray<string>;
      FBaseObjects, FEditorObjects, FNewObjects: TArray<TObject>;
      FEdits: TArray<TUserEdit>;
      FTrailing: TArray<string>;
      FIdBase, FIdNew: TArray<integer>;    // identity: matching line of the other list
      procedure PairGap(AFromBase, AToBase, AFromEditor, AToEditor: integer; var AMapBase, AMapEditor: TArray<integer>);
      procedure FindUserEdits;
      procedure Rejoin(ANewIds: TObjectIds; ADetached: TDetachedLines);
      procedure MatchRuns(const ABaseRows, ANewRows: TArray<integer>);
      procedure FindIdentity;
      function Emit(ABaseIds: TObjectIds; ADetached: TDetachedLines): TStringList;
   public
      constructor Create(ABase, AEditor, ANew: TStrings);
      function Merge(ABaseIds, ANewIds: TObjectIds; ADetached: TDetachedLines): TStringList;
   end;

procedure TLineMatcher.Pair(AIndex1, AIndex2: integer);
begin
   Map1[Offset+AIndex1] := Offset + AIndex2;
   Map2[Offset+AIndex2] := Offset + AIndex1;
end;

procedure TLineMatcher.Match(AFrom1, ATo1, AFrom2, ATo2: integer);
begin
   while (AFrom1 < ATo1) and (AFrom2 < ATo2) and (Ids1[AFrom1] = Ids2[AFrom2]) do
   begin
      Pair(AFrom1, AFrom2);
      Inc(AFrom1);
      Inc(AFrom2);
   end;
   while (AFrom1 < ATo1) and (AFrom2 < ATo2) and (Ids1[ATo1-1] = Ids2[ATo2-1]) do
   begin
      Dec(ATo1);
      Dec(ATo2);
      Pair(ATo1, ATo2);
   end;
   if (AFrom1 = ATo1) or (AFrom2 = ATo2) then
      Exit;
   var split1, split2: integer;
   if FindSplit(AFrom1, ATo1, AFrom2, ATo2, split1, split2) and
      not ((split1 = AFrom1) and (split2 = AFrom2)) and not ((split1 = ATo1) and (split2 = ATo2)) then
   begin
      Match(AFrom1, split1, AFrom2, split2);
      Match(split1, ATo1, split2, ATo2);
   end;
end;

// Runs the search for the shortest edit path from both ends at once and returns the point
// where the two meet, so that each half can be matched on its own
function TLineMatcher.FindSplit(AFrom1, ATo1, AFrom2, ATo2: integer; out ASplit1, ASplit2: integer): boolean;
begin
   result := False;
   ASplit1 := AFrom1;
   ASplit2 := AFrom2;
   var len1 := ATo1 - AFrom1;
   var len2 := ATo2 - AFrom2;
   var maxD := (len1 + len2 + 1) div 2;
   var vOffset := maxD;
   var vLength := 2*maxD + 2;
   var v1, v2: TArray<integer>;
   SetLength(v1, vLength);
   SetLength(v2, vLength);
   for var i := 0 to vLength-1 do
   begin
      v1[i] := -1;
      v2[i] := -1;
   end;
   v1[vOffset+1] := 0;
   v2[vOffset+1] := 0;
   var delta := len1 - len2;
   var front := Odd(delta);
   var k1Start := 0;
   var k1End := 0;
   var k2Start := 0;
   var k2End := 0;
   for var d := 0 to maxD-1 do
   begin
      var k1 := k1Start - d;
      while k1 <= d - k1End do
      begin
         var k1Offset := vOffset + k1;
         var x1: integer;
         if (k1 = -d) or ((k1 <> d) and (v1[k1Offset-1] < v1[k1Offset+1])) then
            x1 := v1[k1Offset+1]
         else
            x1 := v1[k1Offset-1] + 1;
         var y1 := x1 - k1;
         while (x1 < len1) and (y1 < len2) and (Ids1[AFrom1+x1] = Ids2[AFrom2+y1]) do
         begin
            Inc(x1);
            Inc(y1);
         end;
         v1[k1Offset] := x1;
         if x1 > len1 then
            Inc(k1End, 2)
         else if y1 > len2 then
            Inc(k1Start, 2)
         else if front then
         begin
            var k2Offset := vOffset + delta - k1;
            if (k2Offset >= 0) and (k2Offset < vLength) and (v2[k2Offset] <> -1) and (x1 >= len1 - v2[k2Offset]) then
            begin
               ASplit1 := AFrom1 + x1;
               ASplit2 := AFrom2 + y1;
               Exit(True);
            end;
         end;
         Inc(k1, 2);
      end;
      var k2 := k2Start - d;
      while k2 <= d - k2End do
      begin
         var k2Offset := vOffset + k2;
         var x2: integer;
         if (k2 = -d) or ((k2 <> d) and (v2[k2Offset-1] < v2[k2Offset+1])) then
            x2 := v2[k2Offset+1]
         else
            x2 := v2[k2Offset-1] + 1;
         var y2 := x2 - k2;
         while (x2 < len1) and (y2 < len2) and (Ids1[ATo1-x2-1] = Ids2[ATo2-y2-1]) do
         begin
            Inc(x2);
            Inc(y2);
         end;
         v2[k2Offset] := x2;
         if x2 > len1 then
            Inc(k2End, 2)
         else if y2 > len2 then
            Inc(k2Start, 2)
         else if not front then
         begin
            var k1Offset := vOffset + delta - k2;
            if (k1Offset >= 0) and (k1Offset < vLength) and (v1[k1Offset] <> -1) then
            begin
               var x1 := v1[k1Offset];
               if x1 >= len1 - x2 then
               begin
                  ASplit1 := AFrom1 + x1;
                  ASplit2 := AFrom2 + vOffset + x1 - k1Offset;
                  Exit(True);
               end;
            end;
         end;
         Inc(k2, 2);
      end;
   end;
end;

function LineId(AIds: TDictionary<string, integer>; const ALine: string): integer;
begin
   if not AIds.TryGetValue(ALine, result) then
   begin
      result := AIds.Count;
      AIds.Add(ALine, result);
   end;
end;

// Matches lines of two lists which are the same in both of them. Leading and trailing
// lines common to both lists are matched right away so that the expensive part is
// computed for the changed lines only
function DiffArrays(const ALines1, ALines2: TArray<string>): TLinesDiff;
begin
   var cnt1 := Length(ALines1);
   var cnt2 := Length(ALines2);
   SetLength(result.Map1, cnt1);
   SetLength(result.Map2, cnt2);
   for var i := 0 to cnt1-1 do
      result.Map1[i] := ROW_NOT_FOUND;
   for var i := 0 to cnt2-1 do
      result.Map2[i] := ROW_NOT_FOUND;
   var head := 0;
   while (head < cnt1) and (head < cnt2) and (ALines1[head] = ALines2[head]) do
   begin
      result.Map1[head] := head;
      result.Map2[head] := head;
      Inc(head);
   end;
   var tail := 0;
   while (head+tail < cnt1) and (head+tail < cnt2) and (ALines1[cnt1-tail-1] = ALines2[cnt2-tail-1]) do
   begin
      result.Map1[cnt1-tail-1] := cnt2-tail-1;
      result.Map2[cnt2-tail-1] := cnt1-tail-1;
      Inc(tail);
   end;
   var n := cnt1 - head - tail;
   var m := cnt2 - head - tail;
   if (n < 1) or (m < 1) then
      Exit;
   var matcher: TLineMatcher;
   matcher.Map1 := result.Map1;
   matcher.Map2 := result.Map2;
   matcher.Offset := head;
   SetLength(matcher.Ids1, n);
   SetLength(matcher.Ids2, m);
   var ids := TDictionary<string, integer>.Create;
   try
      for var i := 0 to n-1 do
         matcher.Ids1[i] := LineId(ids, ALines1[head+i]);
      for var i := 0 to m-1 do
         matcher.Ids2[i] := LineId(ids, ALines2[head+i]);
   finally
      ids.Free;
   end;
   matcher.Match(0, n, 0, m);
end;

function DiffLines(ALines1, ALines2: TStrings): TLinesDiff;
begin
   result := DiffArrays(ALines1.ToStringArray, ALines2.ToStringArray);
end;

function ObjectsOf(ALines: TStrings): TArray<TObject>;
begin
   SetLength(result, ALines.Count);
   for var i := 0 to ALines.Count-1 do
      result[i] := ALines.Objects[i];
end;

// Line with the object it is bound to, so that lines of different objects never match
function Token(const AText: string; AObject: TObject): string;
begin
   result := AText;
   if AObject <> nil then
      result := result + #0 + IntToHex(Int64(NativeInt(AObject)), 16);
end;

function Extends(const ALine, ABase: string): boolean;
begin
   result := (ALine.Length > ABase.Length) and ALine.StartsWith(ABase) and not ABase.Trim.IsEmpty;
end;

function JoinLines(const A, B: TArray<string>): TArray<string>;
begin
   result := A;
   if Length(B) > 0 then
   begin
      SetLength(result, Length(A) + Length(B));
      for var i := 0 to High(B) do
         result[Length(A)+i] := B[i];
   end;
end;

constructor TDetachedLines.Create;
begin
   inherited Create;
   FItems := TDictionary<integer, TDetachedObject>.Create;
end;

destructor TDetachedLines.Destroy;
begin
   FItems.Free;
   inherited Destroy;
end;

procedure TDetachedLines.Clear;
begin
   FItems.Clear;
end;

// Forgets objects which will never generate code again, as they are freed
procedure TDetachedLines.Purge(AIsAlive: TFunc<integer, NativeInt, boolean>);
begin
   var ids := FItems.Keys.ToArray;
   for var id in ids do
   begin
      if not AIsAlive(id, FItems[id].Obj) then
         FItems.Remove(id);
   end;
end;

constructor TCodeMerger.Create(ABase, AEditor, ANew: TStrings);
begin
   inherited Create;
   FBase := ABase.ToStringArray;
   FEditor := AEditor.ToStringArray;
   FNew := ANew.ToStringArray;
   FBaseObjects := ObjectsOf(ABase);
   FEditorObjects := ObjectsOf(AEditor);
   FNewObjects := ObjectsOf(ANew);
end;

// Pairs lines left unmatched within one gap between matched lines, keeping their order: line bound
// to an object pairs with editor line of the same object, unbound one with editor line extending it.
// Equal numbers of unbound lines left between such pairs are taken for lines changed in place
procedure TCodeMerger.PairGap(AFromBase, AToBase, AFromEditor, AToEditor: integer; var AMapBase, AMapEditor: TArray<integer>);
begin
   var baseRows: TArray<integer> := nil;
   var editorRows: TArray<integer> := nil;
   for var b := AFromBase to AToBase-1 do
   begin
      if AMapBase[b] = ROW_NOT_FOUND then
         baseRows := baseRows + [b];
   end;
   for var e := AFromEditor to AToEditor-1 do
   begin
      if AMapEditor[e] = ROW_NOT_FOUND then
         editorRows := editorRows + [e];
   end;
   if (baseRows = nil) or (editorRows = nil) then
      Exit;
   var pairsBase: TArray<integer> := nil;
   var pairsEditor: TArray<integer> := nil;
   var cursor := 0;
   for var b in baseRows do
   begin
      for var k := cursor to High(editorRows) do
      begin
         var e := editorRows[k];
         var fits := False;
         if FBaseObjects[b] <> nil then
            fits := FEditorObjects[e] = FBaseObjects[b]
         else if FEditorObjects[e] = nil then
            fits := Extends(FEditor[e], FBase[b]);
         if fits then
         begin
            AMapBase[b] := e;
            AMapEditor[e] := b;
            pairsBase := pairsBase + [b];
            pairsEditor := pairsEditor + [e];
            cursor := k + 1;
            break;
         end;
      end;
   end;
   var ib := 0;
   var ie := 0;
   for var p := 0 to Length(pairsBase) do
   begin
      var limitBase := AToBase;
      var limitEditor := AToEditor;
      if p < Length(pairsBase) then
      begin
         limitBase := pairsBase[p];
         limitEditor := pairsEditor[p];
      end;
      var restBase: TArray<integer> := nil;
      var restEditor: TArray<integer> := nil;
      while (ib < Length(baseRows)) and (baseRows[ib] < limitBase) do
      begin
         if AMapBase[baseRows[ib]] = ROW_NOT_FOUND then
            restBase := restBase + [baseRows[ib]];
         Inc(ib);
      end;
      while (ie < Length(editorRows)) and (editorRows[ie] < limitEditor) do
      begin
         if AMapEditor[editorRows[ie]] = ROW_NOT_FOUND then
            restEditor := restEditor + [editorRows[ie]];
         Inc(ie);
      end;
      if (restBase <> nil) and (Length(restBase) = Length(restEditor)) then
      begin
         var unbound := True;
         for var k := 0 to High(restBase) do
         begin
            if (FBaseObjects[restBase[k]] <> nil) or (FEditorObjects[restEditor[k]] <> nil) then
            begin
               unbound := False;
               break;
            end;
         end;
         if unbound then
         begin
            for var k := 0 to High(restBase) do
            begin
               AMapBase[restBase[k]] := restEditor[k];
               AMapEditor[restEditor[k]] := restBase[k];
            end;
         end;
      end;
      if p < Length(pairsBase) then
      begin
         Inc(ib);                      // the paired lines themselves
         Inc(ie);
      end;
   end;
end;

// Finds what the user made of each previously generated line, by comparing it with editor content
procedure TCodeMerger.FindUserEdits;
begin
   var baseTokens: TArray<string>;
   var editorTokens: TArray<string>;
   SetLength(baseTokens, Length(FBase));
   SetLength(editorTokens, Length(FEditor));
   for var i := 0 to High(FBase) do
      baseTokens[i] := Token(FBase[i], FBaseObjects[i]);
   for var i := 0 to High(FEditor) do
      editorTokens[i] := Token(FEditor[i], FEditorObjects[i]);
   var diff := DiffArrays(baseTokens, editorTokens);
   var mapBase := diff.Map1;
   var mapEditor := diff.Map2;
   var exact: TArray<boolean>;
   SetLength(exact, Length(FBase));
   for var b := 0 to High(FBase) do
      exact[b] := mapBase[b] <> ROW_NOT_FOUND;
   var fromBase := 0;
   var fromEditor := 0;
   for var b := 0 to Length(FBase) do
   begin
      if (b = Length(FBase)) or exact[b] then
      begin
         var toEditor := Length(FEditor);
         if b < Length(FBase) then
            toEditor := mapBase[b];
         PairGap(fromBase, b, fromEditor, toEditor, mapBase, mapEditor);
         fromBase := b + 1;
         fromEditor := toEditor + 1;
      end;
   end;
   SetLength(FEdits, Length(FBase));
   for var b := 0 to High(FBase) do
   begin
      var e := mapBase[b];
      FEdits[b].Leading := nil;
      FEdits[b].Text := '';
      if e = ROW_NOT_FOUND then
         FEdits[b].Kind := ueDeleted
      else if exact[b] then
         FEdits[b].Kind := ueKept
      else if Extends(FEditor[e], FBase[b]) then
      begin
         FEdits[b].Kind := ueAppended;
         FEdits[b].Text := FEditor[e].Substring(FBase[b].Length);
      end
      else
      begin
         FEdits[b].Kind := ueChanged;
         FEdits[b].Text := FEditor[e];
      end;
   end;
   var pending: TArray<string> := nil;
   for var e := 0 to High(FEditor) do
   begin
      var b := mapEditor[e];
      if b = ROW_NOT_FOUND then
         pending := pending + [FEditor[e]]      // written by the user
      else
      begin
         FEdits[b].Leading := JoinLines(FEdits[b].Leading, pending);
         pending := nil;
      end;
   end;
   FTrailing := pending;
end;

// Lines of objects which generate code again join previously generated lines with what the user made of them
procedure TCodeMerger.Rejoin(ANewIds: TObjectIds; ADetached: TDetachedLines);
begin
   if (ADetached = nil) or (ANewIds = nil) or (ADetached.FItems.Count = 0) then
      Exit;
   var baseObjects := TDictionary<NativeInt, boolean>.Create;
   try
      for var obj in FBaseObjects do
      begin
         if obj <> nil then
            baseObjects.AddOrSetValue(NativeInt(obj), True);
      end;
      for var obj in FNewObjects do
      begin
         var id: integer;
         var detached: TDetachedObject;
         if (obj = nil) or baseObjects.ContainsKey(NativeInt(obj)) or not ANewIds.TryGetValue(NativeInt(obj), id) or
            not ADetached.FItems.TryGetValue(id, detached) then
            continue;
         ADetached.FItems.Remove(id);
         if detached.Obj <> NativeInt(obj) then
            continue;                  // id given to another object meanwhile
         baseObjects.AddOrSetValue(NativeInt(obj), True);
         for var line in detached.Lines do
         begin
            FBase := FBase + [line.Text];
            FBaseObjects := FBaseObjects + [obj];
            FEdits := FEdits + [line.Edit];
         end;
      end;
   finally
      baseObjects.Free;
   end;
end;

// Gives identity to lines of two runs: lines matching by text, then equal numbers of lines left between them
procedure TCodeMerger.MatchRuns(const ABaseRows, ANewRows: TArray<integer>);
begin
   var baseTexts: TArray<string>;
   var newTexts: TArray<string>;
   SetLength(baseTexts, Length(ABaseRows));
   SetLength(newTexts, Length(ANewRows));
   for var i := 0 to High(ABaseRows) do
      baseTexts[i] := FBase[ABaseRows[i]];
   for var i := 0 to High(ANewRows) do
      newTexts[i] := FNew[ANewRows[i]];
   var diff := DiffArrays(baseTexts, newTexts);
   var fromBase := 0;
   var fromNew := 0;
   for var a := 0 to Length(ABaseRows) do
   begin
      if (a = Length(ABaseRows)) or (diff.Map1[a] <> ROW_NOT_FOUND) then
      begin
         var b := Length(ANewRows);
         if a < Length(ABaseRows) then
         begin
            b := diff.Map1[a];
            FIdBase[ABaseRows[a]] := ANewRows[b];
            FIdNew[ANewRows[b]] := ABaseRows[a];
         end;
         if a - fromBase = b - fromNew then
         begin
            for var k := 0 to a-fromBase-1 do
            begin
               FIdBase[ABaseRows[fromBase+k]] := ANewRows[fromNew+k];
               FIdNew[ANewRows[fromNew+k]] := ABaseRows[fromBase+k];
            end;
         end;
         fromBase := a + 1;
         fromNew := b + 1;
      end;
   end;
end;

// Matches freshly generated lines with previously generated ones: bound lines by their object,
// runs of unbound lines by bound lines next to them and whatever is left by text
procedure TCodeMerger.FindIdentity;

   function RunAfter(const AObjects: TArray<TObject>; AStart: integer): TArray<integer>;
   begin
      result := nil;
      var i := AStart;
      while (i >= 0) and (i < Length(AObjects)) and (AObjects[i] = nil) do
      begin
         result := result + [i];
         Inc(i);
      end;
   end;

   function RunBefore(const AObjects: TArray<TObject>; AEnd: integer): TArray<integer>;
   begin
      result := nil;
      var i := AEnd - 1;
      while (i >= 0) and (i < Length(AObjects)) and (AObjects[i] = nil) do
      begin
         result := [i] + result;
         Dec(i);
      end;
   end;

begin
   SetLength(FIdBase, Length(FBase));
   SetLength(FIdNew, Length(FNew));
   for var i := 0 to High(FIdBase) do
      FIdBase[i] := ROW_NOT_FOUND;
   for var i := 0 to High(FIdNew) do
      FIdNew[i] := ROW_NOT_FOUND;
   var baseRows := TDictionary<NativeInt, TArray<integer>>.Create;
   var newRows := TDictionary<NativeInt, TArray<integer>>.Create;
   try
      for var i := 0 to High(FBaseObjects) do
      begin
         if FBaseObjects[i] <> nil then
         begin
            var rows: TArray<integer> := nil;
            baseRows.TryGetValue(NativeInt(FBaseObjects[i]), rows);
            baseRows.AddOrSetValue(NativeInt(FBaseObjects[i]), rows + [i]);
         end;
      end;
      for var i := 0 to High(FNewObjects) do
      begin
         if FNewObjects[i] <> nil then
         begin
            var rows: TArray<integer> := nil;
            newRows.TryGetValue(NativeInt(FNewObjects[i]), rows);
            newRows.AddOrSetValue(NativeInt(FNewObjects[i]), rows + [i]);
         end;
      end;
      for var pair in newRows do
      begin
         var rows: TArray<integer>;
         if baseRows.TryGetValue(pair.Key, rows) then
            MatchRuns(rows, pair.Value);
      end;
   finally
      newRows.Free;
      baseRows.Free;
   end;
   var claimed: TArray<boolean>;
   SetLength(claimed, Length(FBase));
   var n := 0;
   while n < Length(FNew) do
   begin
      if FNewObjects[n] <> nil then
      begin
         Inc(n);
         continue;
      end;
      var runNew := RunAfter(FNewObjects, n);
      var runBase: TArray<integer> := nil;
      if n = 0 then
         runBase := RunAfter(FBaseObjects, 0)
      else if FIdNew[n-1] <> ROW_NOT_FOUND then
         runBase := RunAfter(FBaseObjects, FIdNew[n-1]+1);
      var next := runNew[High(runNew)] + 1;
      if (runBase = nil) and (next < Length(FNew)) and (FIdNew[next] <> ROW_NOT_FOUND) then
         runBase := RunBefore(FBaseObjects, FIdNew[next]);
      if runBase <> nil then
      begin
         var unclaimed := True;
         for var b in runBase do
         begin
            if claimed[b] then
            begin
               unclaimed := False;
               break;
            end;
         end;
         if unclaimed then
         begin
            for var b in runBase do
               claimed[b] := True;
            MatchRuns(runBase, runNew);
         end;
      end;
      n := next;
   end;
   // unbound lines still without counterpart are matched by text, in order
   var restBase: TArray<integer> := nil;
   var restNew: TArray<integer> := nil;
   for var i := 0 to High(FBase) do
   begin
      if (FBaseObjects[i] = nil) and (FIdBase[i] = ROW_NOT_FOUND) then
         restBase := restBase + [i];
   end;
   for var i := 0 to High(FNew) do
   begin
      if (FNewObjects[i] = nil) and (FIdNew[i] = ROW_NOT_FOUND) then
         restNew := restNew + [i];
   end;
   var restBaseTexts: TArray<string>;
   var restNewTexts: TArray<string>;
   SetLength(restBaseTexts, Length(restBase));
   SetLength(restNewTexts, Length(restNew));
   for var i := 0 to High(restBase) do
      restBaseTexts[i] := FBase[restBase[i]];
   for var i := 0 to High(restNew) do
      restNewTexts[i] := FNew[restNew[i]];
   var diff := DiffArrays(restBaseTexts, restNewTexts);
   for var a := 0 to High(restBase) do
   begin
      var b := diff.Map1[a];
      if b <> ROW_NOT_FOUND then
      begin
         FIdBase[restBase[a]] := restNew[b];
         FIdNew[restNew[b]] := restBase[a];
      end;
   end;
end;

// Builds the result from freshly generated lines and what the user made of their counterparts.
// User lines of previously generated lines which are gone move on to the next line still there,
// unless the object they belong to no longer generates code, then they are kept for its return
function TCodeMerger.Emit(ABaseIds: TObjectIds; ADetached: TDetachedLines): TStringList;
begin
   var newObjects := TDictionary<NativeInt, boolean>.Create;
   try
      for var obj in FNewObjects do
      begin
         if obj <> nil then
            newObjects.AddOrSetValue(NativeInt(obj), True);
      end;
      var carry: TArray<string> := nil;
      for var b := 0 to High(FBase) do
      begin
         if FIdBase[b] <> ROW_NOT_FOUND then
         begin
            FEdits[b].Leading := JoinLines(carry, FEdits[b].Leading);
            carry := nil;
            continue;
         end;
         var obj := FBaseObjects[b];
         var id: integer;
         if (obj <> nil) and (ADetached <> nil) and (ABaseIds <> nil) and not newObjects.ContainsKey(NativeInt(obj)) and
            ABaseIds.TryGetValue(NativeInt(obj), id) then
         begin
            var detached: TDetachedObject;
            if not ADetached.FItems.TryGetValue(id, detached) then
            begin
               detached.Obj := NativeInt(obj);
               detached.Lines := nil;
            end;
            var line: TDetachedLine;
            line.Text := FBase[b];
            line.Edit := FEdits[b];
            detached.Lines := detached.Lines + [line];
            ADetached.FItems.AddOrSetValue(id, detached);
         end
         else
         begin
            carry := JoinLines(carry, FEdits[b].Leading);
            if (obj = nil) and (FEdits[b].Kind = ueChanged) then
               carry := carry + [FEdits[b].Text];      // text the user put in place of an unbound line is the user's own
         end;
      end;
      FTrailing := JoinLines(carry, FTrailing);
   finally
      newObjects.Free;
   end;
   result := TStringList.Create;
   for var n := 0 to High(FNew) do
   begin
      var line := FNew[n];
      var obj := FNewObjects[n];
      var b := FIdNew[n];
      if b = ROW_NOT_FOUND then
      begin
         result.AddObject(line, obj);
         continue;
      end;
      for var userLine in FEdits[b].Leading do
         result.Add(userLine);
      var changed := FBase[b] <> line;
      case FEdits[b].Kind of
         ueKept:
            result.AddObject(line, obj);
         ueAppended:
            result.AddObject(line + FEdits[b].Text, obj);
         ueChanged:
         begin
            // user's version wins while generator keeps the line, or when it already is the new line with text appended
            if not changed or Extends(FEdits[b].Text, line) then
               result.AddObject(FEdits[b].Text, obj)
            else
               result.AddObject(line, obj);
         end;
         ueDeleted:
         begin
            if changed then
               result.AddObject(line, obj);    // generator changed the line the user removed, so it is back
         end;
      end;
   end;
   for var userLine in FTrailing do
      result.Add(userLine);
end;

function TCodeMerger.Merge(ABaseIds, ANewIds: TObjectIds; ADetached: TDetachedLines): TStringList;
begin
   FindUserEdits;
   Rejoin(ANewIds, ADetached);
   FindIdentity;
   result := Emit(ABaseIds, ADetached);
end;

// Three way merge of code in editor. ABase is code as the generator produced it last time, AEditor is
// current content of editor and ANew is freshly generated code. ABaseIds and ANewIds give project ids of
// objects lines are bound to, so that user changes of an object which stops generating code can be kept
// in ADetached and brought back when the object generates code again; with no ADetached they are dropped
function MergeLines(ABase, AEditor, ANew: TStrings; ABaseIds: TObjectIds = nil; ANewIds: TObjectIds = nil;
                    ADetached: TDetachedLines = nil): TStringList;
begin
   var merger := TCodeMerger.Create(ABase, AEditor, ANew);
   try
      result := merger.Merge(ABaseIds, ANewIds, ADetached);
   finally
      merger.Free;
   end;
end;

end.
