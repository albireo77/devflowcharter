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

// Keeps text the user appended after the end of generated lines in code editor (e.g. trailing comments)
// when code is generated again from flowchart. Everything else in editor is replaced by freshly generated
// code, so that code always matches flowchart. Every generated line is identified by flowchart object it
// belongs to rather than by its text, so appended text follows its line when blocks move, change or
// disappear and come back (undo of removal).

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

   TDetachedLine = record
      Text: string;                    // line as generated last time
      Tail: string;                    // text the user appended to it
   end;

   TDetachedObject = record
      Obj: NativeInt;
      Lines: TArray<TDetachedLine>;
      Sequence: integer;               // order in which objects were detached
      TextMatchable: boolean;          // whether a new object with the same lines may take it over
   end;

   // Text appended to lines whose flowchart object no longer generates code (e.g. removed block),
   // kept for when the object generates code again (e.g. removal is undone)
   TDetachedLines = class
   private
      FItems: TDictionary<integer, TDetachedObject>;
      FSequence: integer;
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
   System.Math, Constants;

const
   MAX_PAIR_CELLS = 2000000;
   MAX_CHAR_CELLS = 1000000;

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
      FTails: TArray<string>;              // text the user appended to each previously generated line
      FUnpaired: TDictionary<NativeInt, TArray<integer>>;   // editor lines paired with no previously generated line, by object
      FIdBase, FIdNew: TArray<integer>;    // identity: matching line of the other list
      procedure PairRows(const ABaseRows, AEditorRows: TArray<integer>; ABound: boolean; var APairs: TArray<integer>);
      procedure FindTails;
      procedure Rejoin(ANewIds: TObjectIds; ADetached: TDetachedLines);
      procedure MatchRuns(const ABaseRows, ANewRows: TArray<integer>);
      procedure FindIdentity;
      function Emit(ABaseIds: TObjectIds; ADetached: TDetachedLines): TStringList;
   public
      constructor Create(ABase, AEditor, ANew: TStrings);
      destructor Destroy; override;
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

function Extends(const ALine, ABase: string): boolean;
begin
   result := (ALine.Length > ABase.Length) and ALine.StartsWith(ABase) and not ABase.Trim.IsEmpty;
end;

// How well editor line fits previously generated line: 3 - the same, 2 - with text appended, 1 - changed
// by the user (only line bound to flowchart object, which stays bound to it when changed), 0 - not at all
function Score(const AEditorLine, ABaseLine: string; ABound: boolean): integer;
begin
   if AEditorLine = ABaseLine then
      result := 3
   else if Extends(AEditorLine, ABaseLine) then
      result := 2
   else if ABound then
      result := 1
   else
      result := 0;
end;

// Text the user appended after the end of generated line ABaseLine within line AEditorLine, whose generated
// part the user also changed. Characters of generated line are aligned within edited one and text after the
// position its last character got is returned; nothing if that character cannot be aligned
function AppendedAfter(const AEditorLine, ABaseLine: string): string;
begin
   result := '';
   var base := ABaseLine.TrimRight;
   var n := base.Length;
   var m := AEditorLine.Length;
   if base.Trim.IsEmpty or (m = 0) or (Int64(n)*m > MAX_CHAR_CELLS) then
      Exit;
   var w := m + 1;
   var common: TArray<integer>;        // length of longest common subsequence of base from i on and line from j on
   SetLength(common, (n+1)*w);
   for var i := n-1 downto 0 do
   begin
      for var j := m-1 downto 0 do
      begin
         if base.Chars[i] = AEditorLine.Chars[j] then
            common[i*w+j] := 1 + common[(i+1)*w+j+1]
         else
            common[i*w+j] := Max(common[(i+1)*w+j], common[i*w+j+1]);
      end;
   end;
   var bi := 0;
   var ej := 0;
   var endPos := -1;
   while (bi < n) and (ej < m) do
   begin
      if (base.Chars[bi] = AEditorLine.Chars[ej]) and (common[bi*w+ej] = 1 + common[(bi+1)*w+ej+1]) then
      begin
         if bi = n-1 then
            endPos := ej;
         Inc(bi);
         Inc(ej);
      end
      else if common[(bi+1)*w+ej] >= common[bi*w+ej+1] then
         Inc(bi)
      else
         Inc(ej);
   end;
   if endPos >= 0 then
      result := AEditorLine.Substring(endPos+1);
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
   FUnpaired := TDictionary<NativeInt, TArray<integer>>.Create;
end;

destructor TCodeMerger.Destroy;
begin
   FUnpaired.Free;
   inherited Destroy;
end;

// Pairs previously generated lines with editor lines of the same object, keeping their order, so that
// identical lines, lines with text appended and changed lines weigh as much as possible (see Score)
procedure TCodeMerger.PairRows(const ABaseRows, AEditorRows: TArray<integer>; ABound: boolean; var APairs: TArray<integer>);
begin
   var n := Length(ABaseRows);
   var m := Length(AEditorRows);
   if (n = 0) or (m = 0) then
      Exit;
   if Int64(n)*m > MAX_PAIR_CELLS then
   begin
      // too many lines to weigh every pairing, so each line takes the first editor line fitting it
      var cursor := 0;
      for var b in ABaseRows do
      begin
         for var k := cursor to m-1 do
         begin
            if Score(FEditor[AEditorRows[k]], FBase[b], ABound) > 0 then
            begin
               APairs[b] := AEditorRows[k];
               cursor := k + 1;
               break;
            end;
         end;
      end;
      Exit;
   end;
   var w := m + 1;
   var weights: TArray<integer>;        // best total weight of pairing base rows from i on and editor rows from j on
   SetLength(weights, (n+1)*w);
   for var i := n-1 downto 0 do
   begin
      for var j := m-1 downto 0 do
      begin
         var best := Max(weights[(i+1)*w+j], weights[i*w+j+1]);
         var fit := Score(FEditor[AEditorRows[j]], FBase[ABaseRows[i]], ABound);
         if fit > 0 then
            best := Max(best, fit + weights[(i+1)*w+j+1]);
         weights[i*w+j] := best;
      end;
   end;
   var bi := 0;
   var ej := 0;
   while (bi < n) and (ej < m) do
   begin
      var fit := Score(FEditor[AEditorRows[ej]], FBase[ABaseRows[bi]], ABound);
      if (fit > 0) and (weights[bi*w+ej] = fit + weights[(bi+1)*w+ej+1]) then
      begin
         APairs[ABaseRows[bi]] := AEditorRows[ej];
         Inc(bi);
         Inc(ej);
      end
      else if weights[bi*w+ej] = weights[(bi+1)*w+ej] then
         Inc(bi)
      else
         Inc(ej);
   end;
end;

// Finds text the user appended to each previously generated line, pairing lines of the same flowchart
// object (or lines bound to none) in editor and in previously generated code
procedure TCodeMerger.FindTails;
begin
   var baseGroups := TDictionary<NativeInt, TArray<integer>>.Create;
   var editorGroups := TDictionary<NativeInt, TArray<integer>>.Create;
   try
      for var b := 0 to High(FBase) do
      begin
         var rows: TArray<integer> := nil;
         baseGroups.TryGetValue(NativeInt(FBaseObjects[b]), rows);
         baseGroups.AddOrSetValue(NativeInt(FBaseObjects[b]), rows + [b]);
      end;
      for var e := 0 to High(FEditor) do
      begin
         var rows: TArray<integer> := nil;
         editorGroups.TryGetValue(NativeInt(FEditorObjects[e]), rows);
         editorGroups.AddOrSetValue(NativeInt(FEditorObjects[e]), rows + [e]);
      end;
      var pairs: TArray<integer>;
      SetLength(pairs, Length(FBase));
      for var b := 0 to High(pairs) do
         pairs[b] := ROW_NOT_FOUND;
      for var group in baseGroups do
      begin
         var editorRows: TArray<integer>;
         if editorGroups.TryGetValue(group.Key, editorRows) then
            PairRows(group.Value, editorRows, group.Key <> 0, pairs);   // key 0: lines bound to no object
      end;
      var used: TArray<boolean>;
      SetLength(used, Length(FEditor));
      SetLength(FTails, Length(FBase));
      for var b := 0 to High(FBase) do
      begin
         FTails[b] := '';
         var e := pairs[b];
         if e <> ROW_NOT_FOUND then
         begin
            used[e] := True;
            if Extends(FEditor[e], FBase[b]) then
               FTails[b] := FEditor[e].Substring(FBase[b].Length)
            else if FEditor[e] <> FBase[b] then
               FTails[b] := AppendedAfter(FEditor[e], FBase[b]);   // line the user also changed
         end;
      end;
      for var e := 0 to High(FEditor) do
      begin
         if not used[e] and not FEditor[e].Trim.IsEmpty then
         begin
            var rows: TArray<integer> := nil;
            FUnpaired.TryGetValue(NativeInt(FEditorObjects[e]), rows);
            FUnpaired.AddOrSetValue(NativeInt(FEditorObjects[e]), rows + [e]);
         end;
      end;
   finally
      editorGroups.Free;
      baseGroups.Free;
   end;
end;

// Lines of objects which generate code again join previously generated lines with text appended to them.
// An object comes back by its id (e.g. undo of removal) or, when it is a copy of an object detached since
// the last time new objects appeared (e.g. block moved by drag and drop, which cuts it and pastes a copy),
// by having the same lines
procedure TCodeMerger.Rejoin(ANewIds: TObjectIds; ADetached: TDetachedLines);

   procedure Take(AObject: TObject; const ADetachedObject: TDetachedObject);
   begin
      for var line in ADetachedObject.Lines do
      begin
         FBase := FBase + [line.Text];
         FBaseObjects := FBaseObjects + [AObject];
         FTails := FTails + [line.Tail];
      end;
   end;

begin
   if (ADetached = nil) or (ANewIds = nil) or (ADetached.FItems.Count = 0) then
      Exit;
   var baseObjects := TDictionary<NativeInt, boolean>.Create;
   var newRows := TDictionary<NativeInt, TArray<integer>>.Create;
   try
      for var obj in FBaseObjects do
      begin
         if obj <> nil then
            baseObjects.AddOrSetValue(NativeInt(obj), True);
      end;
      var appeared: TArray<TObject> := nil;   // objects new to the code, in order of their lines
      for var i := 0 to High(FNewObjects) do
      begin
         var obj := FNewObjects[i];
         if (obj = nil) or baseObjects.ContainsKey(NativeInt(obj)) then
            continue;
         var rows: TArray<integer> := nil;
         if not newRows.TryGetValue(NativeInt(obj), rows) then
            appeared := appeared + [obj];
         newRows.AddOrSetValue(NativeInt(obj), rows + [i]);
      end;
      for var obj in appeared do
      begin
         var id: integer;
         var detached: TDetachedObject;
         if ANewIds.TryGetValue(NativeInt(obj), id) and ADetached.FItems.TryGetValue(id, detached) then
         begin
            ADetached.FItems.Remove(id);
            if detached.Obj = NativeInt(obj) then    // otherwise id given to another object meanwhile
            begin
               baseObjects.AddOrSetValue(NativeInt(obj), True);
               Take(obj, detached);
            end;
         end;
      end;
      for var obj in appeared do
      begin
         if baseObjects.ContainsKey(NativeInt(obj)) then
            continue;
         var rows := newRows[NativeInt(obj)];
         var found := False;
         var bestId := 0;
         var best: TDetachedObject;
         for var pair in ADetached.FItems do
         begin
            var detached := pair.Value;
            if not detached.TextMatchable or (Length(detached.Lines) <> Length(rows)) or (found and (detached.Sequence >= best.Sequence)) then
               continue;
            var same := True;
            for var k := 0 to High(rows) do
            begin
               if detached.Lines[k].Text.TrimLeft <> FNew[rows[k]].TrimLeft then
               begin
                  same := False;
                  break;
               end;
            end;
            if same then
            begin
               found := True;
               bestId := pair.Key;
               best := detached;
            end;
         end;
         if found then
         begin
            ADetached.FItems.Remove(bestId);
            baseObjects.AddOrSetValue(NativeInt(obj), True);
            Take(obj, best);
         end;
      end;
      if appeared <> nil then
      begin
         // objects detached before can come back by their id only
         for var id in ADetached.FItems.Keys.ToArray do
         begin
            var detached := ADetached.FItems[id];
            detached.TextMatchable := False;
            ADetached.FItems[id] := detached;
         end;
      end;
   finally
      newRows.Free;
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

// Builds the result from freshly generated lines with text the user appended to their counterparts.
// Text appended to lines of objects which no longer generate code is kept for their return
function TCodeMerger.Emit(ABaseIds: TObjectIds; ADetached: TDetachedLines): TStringList;
begin
   var newObjects := TDictionary<NativeInt, boolean>.Create;
   try
      for var obj in FNewObjects do
      begin
         if obj <> nil then
            newObjects.AddOrSetValue(NativeInt(obj), True);
      end;
      for var b := 0 to High(FBase) do
      begin
         if FIdBase[b] <> ROW_NOT_FOUND then
            continue;
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
               detached.Sequence := ADetached.FSequence;
               detached.TextMatchable := True;
               Inc(ADetached.FSequence);
            end;
            var line: TDetachedLine;
            line.Text := FBase[b];
            line.Tail := FTails[b];
            detached.Lines := detached.Lines + [line];
            ADetached.FItems.AddOrSetValue(id, detached);
         end;
      end;
   finally
      newObjects.Free;
   end;
   var used: TArray<boolean>;
   SetLength(used, Length(FEditor));
   result := TStringList.Create;
   for var n := 0 to High(FNew) do
   begin
      var line := FNew[n];
      var obj := FNewObjects[n];
      var b := FIdNew[n];
      var tail := '';
      if b <> ROW_NOT_FOUND then
         tail := FTails[b];
      if line.Trim.IsEmpty then
         tail := ''                    // text is never appended to blank line
      else if tail.IsEmpty then
      begin
         // editor line which already is this line with text appended, e.g. when generated text changed
         // in a way previously generated code does not show (project name unknown when it was generated)
         var rows: TArray<integer>;
         if FUnpaired.TryGetValue(NativeInt(obj), rows) then
         begin
            for var e in rows do
            begin
               if not used[e] and Extends(FEditor[e], line) then
               begin
                  tail := FEditor[e].Substring(line.Length);
                  used[e] := True;
                  break;
               end;
            end;
         end;
      end;
      result.AddObject(line + tail, obj);
   end;
end;

function TCodeMerger.Merge(ABaseIds, ANewIds: TObjectIds; ADetached: TDetachedLines): TStringList;
begin
   FindTails;
   Rejoin(ANewIds, ADetached);
   FindIdentity;
   result := Emit(ABaseIds, ADetached);
end;

// Freshly generated code ANew with text the user appended to lines in editor. ABase is code as the generator
// produced it last time and AEditor is current content of editor. ABaseIds and ANewIds give project ids of
// objects lines are bound to, so that text appended to lines of an object which stops generating code can be
// kept in ADetached and brought back when the object generates code again; with no ADetached it is dropped
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
