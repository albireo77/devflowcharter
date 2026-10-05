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



unit Editor_Form;

interface

uses
{$IFDEF USE_CODEFOLDING}
   SynEditCodeFolding,
{$ENDIF}
   Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Clipbrd,
   Vcl.Menus, System.Types, System.SysUtils, System.Classes, SynEdit, SynExportRTF,
   SynEditPrint, Types, SynHighlighterPas, SynHighlighterCpp, SynMemo, SynExportHTML,
   OmniXML, Base_Form, Interfaces, SynEditExport, SynEditHighlighter, SynHighlighterPython,
   SynHighlighterJava, Base_Block;

type

  TEditorForm = class(TBaseForm)
    pmPopMenu: TPopupMenu;
    miUndo: TMenuItem;
    miCut: TMenuItem;
    N1: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miRemove: TMenuItem;
    N2: TMenuItem;
    miSelectAll: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    memCodeEditor: TSynMemo;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    SynEditPrint1: TSynEditPrint;
    miRedo: TMenuItem;
    MainMenu1: TMainMenu;
    miProgram: TMenuItem;
    miCompile: TMenuItem;
    miPrint: TMenuItem;
    stbEditorBar: TStatusBar;
    miSave: TMenuItem;
    miEdit: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    miGoto: TMenuItem;
    SaveDialog2: TSaveDialog;
    SaveDialog1: TSaveDialog;
    SynExporterRTF1: TSynExporterRTF;
    miPasteComment: TMenuItem;
    N3: TMenuItem;
    miView: TMenuItem;
    miRegenerate: TMenuItem;
    miStatusBar: TMenuItem;
    miGutter: TMenuItem;
    miScrollbars: TMenuItem;
    N4: TMenuItem;
    miHelp: TMenuItem;
    miCopyRichText: TMenuItem;
    SynExporterHTML1: TSynExporterHTML;
    miCodeFolding: TMenuItem;
    miCollapseAll: TMenuItem;
    miUnCollapseAll: TMenuItem;
    miIndentGuides: TMenuItem;
    miCodeFoldingEnable: TMenuItem;
    miRichText: TMenuItem;
    N5: TMenuItem;
    miFindProj: TMenuItem;
    N6: TMenuItem;
    SynPythonSyn1: TSynPythonSyn;
    SynJavaSyn1: TSynJavaSyn;
    procedure FormShow(Sender: TObject);
    procedure pmPopMenuPopup(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure FindDialogShow(Sender: TObject);
    procedure FindDialogClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miCompileClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure OnShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure memCodeEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure memCodeEditorGutterClick(Sender: TObject;
      Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
    procedure miRegenerateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memCodeEditorDblClick(Sender: TObject);
    procedure memCodeEditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure memCodeEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure miHelpClick(Sender: TObject);
    procedure memCodeEditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure memCodeEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function SelectCodeRange(AObject: TObject; ADoSelect: boolean = True): TCodeRange;
    procedure UnSelectCodeRange(AObject: TObject);
    procedure AfterTranslation(AList: TStringList); override;
    procedure ResetForm; override;
    procedure SetSaveDialog(ASaveDialog: TSaveDialog);
    procedure miGotoClick(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miRichTextClick(Sender: TObject);
    procedure memCodeEditorChange(Sender: TObject);
    procedure miFindProjClick(Sender: TObject);
    procedure OnChangeEditor;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  private
    { Private declarations }
    FCloseBracketPos: TPoint;
    FCloseBracketPosP: PPoint;
    FFocusEditor: boolean;
    FDialog: TFindDialog;
    FWithFocus: IWithFocus;
    FGeneratedLines: TStringList;
    FUndoObject: TObject;
    FUndoBase: TStringList;
    FUndoLines: TStringList;
    function BuildBracketHint(startLine, endLine: integer): string;
    function CharToPixels(const P: TBufferCoord): TPoint;
    function GetAllLines: TStrings;
    procedure PasteComment(const AText: string);
    procedure DisplayLines(ALines: TStringList; AReset: boolean);
    procedure SetGeneratedLines(ALines: TStrings);
    function ReplaceGeneratedLine(const AChangeLine: TChangeLine): string;
    procedure StoreUndoSection(AObject: TObject; AEditorLines, ANewLines: TStrings);
    function RestoreUndoSection(ALines: TStringList): boolean;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure SetFormAttributes;
    procedure ExecuteCopyToClipboard(AIfRichText: boolean);
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
    function GetIndentLevel(idx: integer; ALines: TStrings): integer;
    procedure RefreshEditorForObject(AObject: TObject);
    procedure UpdateEditorForBlock(ABlock: TBlock; const AChangeLine: TChangeLine);
    procedure MergeGeneratedSection(const ACodeRange: TCodeRange; ANewLines: TStringList);
    procedure SetCaretPos(const ALine: TChangeLine);
    procedure SaveToFile(const APath: string);
    procedure InsertLibraryEntry(const ALibrary: string);
{$IFDEF USE_CODEFOLDING}
    procedure RemoveFoldRange(var AFoldRange: TSynEditFoldRange);
    function FindFoldRangeInCodeRange(const ACodeRange: TCodeRange; ACount: integer): TSynEditFoldRange;
    procedure ReloadFoldRegions;
{$ENDIF}
  end;

  TEditorHintWindow = class(THintWindow)
     constructor Create (AOwner: TComponent); override;
     procedure ActivateHintData(ARect: TRect; const AHint: string; AData: Pointer); override;
     function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

var
   EditorForm: TEditorForm;

implementation

uses
   System.StrUtils, System.Math, System.UITypes, System.Contnrs, System.Generics.Collections, WinApi.Windows,
   Infrastructure, Goto_Form, Main_Block, Help_Form, Comment, OmniXMLUtils, Main_Form,
   SynEditTypes, ParserHelper, Constants;

{$R *.dfm}

type
   TLinesDiff = record
      Map1: TArray<integer>;    // for each line of the first list, index of the matching line in the second one
      Map2: TArray<integer>;    // for each line of the second list, index of the matching line in the first one
      Tails: TArray<string>;    // for each line of the first list, text the user appended to it
   end;

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

function CompareIntegers(AList: TStringList; idx1, idx2: integer): integer;
begin
   result := AList[idx1].ToInteger - AList[idx2].ToInteger;
end;

constructor TEditorHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Font.Assign(EditorForm.memCodeEditor.Font);
end;

procedure TEditorHintWindow.ActivateHintData(ARect: TRect; const AHint: string; AData: Pointer);
begin
   if AData <> nil then
      ARect.SetLocation(PPoint(AData)^);
   ARect.Offset(0, -ARect.Height);
   inherited ActivateHintData(ARect, AHint, AData);
end;

// implementation taken from base class (THintWindow)
function TEditorHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
begin
  Result := System.Types.Rect(0, 0, MaxWidth, 0);
  //code below removed to allow use of custom font (e.g. font of underlying control) instead of Screen.HintFont
  //if Screen.ActiveCustomForm <> nil then
  //begin
  //  Canvas.Font := Screen.HintFont;
  //  Canvas.Font.Height := Muldiv(Canvas.Font.Height, Screen.ActiveCustomForm.CurrentPPI, Screen.PixelsPerInch);
  //end;
  DrawText(Canvas.Handle, AHint, -1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;

destructor TEditorForm.Destroy;
begin
   FGeneratedLines.Free;
   FUndoBase.Free;
   FUndoLines.Free;
   inherited Destroy;
end;

procedure TEditorForm.FormCreate(Sender: TObject);
begin
   FGeneratedLines := TStringList.Create;
   FUndoBase := TStringList.Create;
   FUndoLines := TStringList.Create;
   GInfra.SetHLighters;
   SetFormAttributes;
   Application.OnShowHint := OnShowHint;
{$IFDEF USE_CODEFOLDING}
   ReloadFoldRegions;
{$ENDIF}
end;

procedure TEditorForm.OnShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
   if (HintInfo.HintControl = memCodeEditor) and (FCloseBracketPosP <> nil) then
   begin
      HintInfo.HintWindowClass := TEditorHintWindow;
      HintInfo.HintData := FCloseBracketPosP;
      FCloseBracketPosP := nil;
   end;
end;

function TEditorForm.BuildBracketHint(startLine, endLine: integer): string;
begin
   result := '';
   if (endLine < 0) or (endLine >= memCodeEditor.Lines.Count) or (startLine >= endLine) or (startLine < 0) then
      Exit;
   var lines := TStringList.Create;
   lines.TrailingLineBreak := False;
   try
      if (endLine - startLine) > (memCodeEditor.LinesInWindow div 2) then
      begin
         lines.Add(memCodeEditor.Lines[startLine]);
         lines.Add(memCodeEditor.Lines[startLine+1]);
         lines.Add(TInfra.ExtractIndentString(memCodeEditor.Lines[startLine+1]) + '...');
      end
      else
      begin
         for var i := startLine to endLine do
            lines.Add(memCodeEditor.Lines[i]);
      end;
      var min := -1;
      for var i := 0 to lines.Count-1 do
      begin
         if lines[i].Trim.IsEmpty then
            continue;
         var len := TInfra.ExtractIndentString(lines[i]).Length;
         if (min = -1) or (len < min) then
            min := len;
      end;
      if min = -1 then
         min := 0;
      for var i := 0 to lines.Count-1 do
         lines[i] := Copy(lines[i], min+1);
      result := lines.Text;
   finally
      lines.Free;
   end;
end;

procedure TEditorForm.SetFormAttributes;
begin
   with GSettings do
   begin
      memCodeEditor.Font.Color := EditorFontColor;
      memCodeEditor.Color := EditorBkgColor;
      memCodeEditor.ActiveLineColor := EditorALineColor;
      memCodeEditor.SelectedColor.Background := EditorSelectColor;
      memCodeEditor.Gutter.Color := EditorGutterColor;
      memCodeEditor.Gutter.Font.Color := EditorFontColor;
      memCodeEditor.Gutter.BorderColor := EditorBkgColor;
      memCodeEditor.Gutter.Visible := EditorShowGutter;
      if IndentChar = TAB_CHAR then
      begin
         memCodeEditor.Options := memCodeEditor.Options - [eoTabsToSpaces];
         memCodeEditor.TabWidth := 3;
      end
      else
      begin
         memCodeEditor.Options := memCodeEditor.Options + [eoTabsToSpaces];
         memCodeEditor.TabWidth := IndentLength;
      end;
      memCodeEditor.Font.Size := EditorFontSize;
      memCodeEditor.Gutter.Font.Size := Max(EDITOR_DEFAULT_GUTTER_FONT_SIZE, EditorFontSize - 2);
      memCodeEditor.RightEdge := EditorRightEdgeColumn;
      memCodeEditor.RightEdgeColor := EditorRightEdgeColor;
      stbEditorBar.Visible := EditorShowStatusBar;
      miStatusBar.Checked := EditorShowStatusBar;
      miGutter.Checked := EditorShowGutter;
      miScrollbars.Checked := EditorShowScrollbars;
      miRichText.Enabled := GInfra.CurrentLang.Highlighter <> nil;
      miRichText.Checked := if miRichText.Enabled then EditorShowRichText else False;
      memCodeEditor.ScrollBars := if EditorShowScrollbars then TScrollStyle.ssBoth else TScrollStyle.ssNone;
      memCodeEditor.Height := if stbEditorBar.Visible then (ClientHeight - stbEditorBar.Height) else ClientHeight;
      memCodeEditor.Highlighter := if EditorShowRichText then GInfra.CurrentLang.Highlighter else nil;
{$IFDEF USE_CODEFOLDING}
      miCodeFoldingEnable.Enabled := Length(GInfra.CurrentLang.FoldRegions) > 0;
{$ELSE}
      miCodeFoldingEnable.Enabled := False;
{$ENDIF}
      miCodeFoldingEnable.Checked := EditorCodeFolding and miCodeFoldingEnable.Enabled;
      miCollapseAll.Enabled := miCodeFoldingEnable.Checked;
      miUnCollapseAll.Enabled := miCodeFoldingEnable.Checked;
      miIndentGuides.Enabled := miCodeFoldingEnable.Checked;
      miIndentGuides.Checked := miIndentGuides.Enabled and EditorIndentGuides;
{$IFDEF USE_CODEFOLDING}
      with memCodeEditor.CodeFolding do
      begin
         Enabled := miCodeFoldingEnable.Checked;
         HighlighterFoldRegions := False;
         FolderBarColor := EditorGutterColor;
         FolderBarLinesColor := EditorFontColor;
         IndentGuides := miIndentGuides.Checked;
      end;
{$ENDIF}
   end;
   GInfra.SetLangHiglighterAttributes;
end;

procedure TEditorForm.ResetForm;
begin
{$IFDEF USE_CODEFOLDING}
   memCodeEditor.AllFoldRanges.DestroyAll;
{$ENDIF}
   memCodeEditor.ClearAll;
   memCodeEditor.Highlighter := nil;
   FGeneratedLines.Clear;
   FUndoBase.Clear;
   FUndoLines.Clear;
   FUndoObject := nil;
   FFocusEditor := True;
   FCloseBracketPosP := nil;
   FWithFocus := nil;
   Width := 425;
   Height := 558;
   FDialog := nil;
   inherited ResetForm;
end;

procedure TEditorForm.AfterTranslation(AList: TStringList);
begin
   if stbEditorBar.Panels[1].Text <> '' then
      stbEditorBar.Panels[1].Text := AList.Values['Modified'];
   stbEditorBar.Panels[2].Text := AList.Values[if memCodeEditor.InsertMode then 'InsertMode' else 'OverwriteMode'];
   inherited AfterTranslation(AList);
end;

procedure TEditorForm.PasteComment(const AText: string);
begin
   if AText.IsEmpty then
      Exit;
   var ctext := '';
   Clipboard.Open;
   if Clipboard.HasFormat(CF_TEXT) then
      ctext := Clipboard.AsText;
   memCodeEditor.BeginUpdate;
   var strings := TStringList.Create;
   try
      strings.Text := AText;
      var count := strings.Count - 1;
      var bc := memCodeEditor.CaretXY;
      var afterLine := True;
      for var i := 0 to count do
      begin
         if bc.Char <= memCodeEditor.Lines[bc.Line-1+i].Length then
         begin
            afterLine := False;
            break;
         end;
      end;
      var beginComment := GInfra.CurrentLang.CommentBegin;
      var endComment := GInfra.CurrentLang.CommentEnd;
      for var i := 0 to count do
      begin
         var line := ' ' + strings[i].Trim;
         memCodeEditor.CaretXY := BufferCoord(bc.Char, bc.Line + i);
         if afterLine then
         begin
            line := beginComment + line;
            if not endComment.IsEmpty then
               line := line + ' ' + endComment;
         end
         else
         begin
            if i = 0 then
            begin
               line := beginComment + line;
               if (i = count) and not endComment.IsEmpty then
                  line := line + ' ' + endComment;
            end
            else if i = count then
            begin
               if endComment.IsEmpty then
                  line := beginComment + line
               else
                  line := line + ' ' + endComment;
            end
            else if endComment.IsEmpty then
               line := beginComment + line;
            memCodeEditor.Lines.Insert(memCodeEditor.CaretY-1, '');
         end;
         Clipboard.AsText := line;
         memCodeEditor.PasteFromClipboard;
      end;
   finally
      strings.Free;
      memCodeEditor.EndUpdate;
      if not ctext.IsEmpty then
         Clipboard.AsText := ctext;
      Clipboard.Close;
   end;
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
function DiffLines(ALines1, ALines2: TStrings): TLinesDiff;
begin
   var cnt1 := ALines1.Count;
   var cnt2 := ALines2.Count;
   SetLength(result.Map1, cnt1);
   SetLength(result.Map2, cnt2);
   SetLength(result.Tails, cnt1);
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

// Pairs generated lines with editor lines the user created out of them by appending text,
// like a trailing comment. Such a line counts as matching the generated one and the appended
// text is kept aside so that it can be put back when the generator changes the line
procedure MatchExtendedLines(ABase, AEditor: TStrings; var ADiff: TLinesDiff);
begin
   var e := 0;
   for var b := 0 to ABase.Count-1 do
   begin
      if ADiff.Map1[b] <> ROW_NOT_FOUND then
      begin
         e := ADiff.Map1[b] + 1;
         Continue;
      end;
      var baseLine := ABase[b];
      if baseLine.Trim.IsEmpty then
         Continue;                      // every line starts with a blank one so it must be skipped
      var j := e;
      while j < AEditor.Count do
      begin
         if ADiff.Map2[j] <> ROW_NOT_FOUND then
            break;                      // line belongs to another generated one so do not pair across it
         var editorLine := AEditor[j];
         if (editorLine.Length > baseLine.Length) and editorLine.StartsWith(baseLine) then
         begin
            ADiff.Map1[b] := j;
            ADiff.Map2[j] := b;
            ADiff.Tails[b] := editorLine.Substring(baseLine.Length);
            e := j + 1;
            break;
         end;
         Inc(j);
      end;
   end;
end;

function SameLines(ALines1: TStrings; AFrom1, ATo1: integer; ALines2: TStrings; AFrom2, ATo2: integer): boolean;
begin
   result := (ATo1-AFrom1) = (ATo2-AFrom2);
   if result then
   begin
      for var i := 0 to ATo1-AFrom1-1 do
      begin
         if ALines1[AFrom1+i] <> ALines2[AFrom2+i] then
            Exit(False);
      end;
   end;
end;

procedure AddLines(AResult: TStringList; ALines: TStrings; AFrom, ATo: integer; AWithObjects: boolean);
begin
   for var i := AFrom to ATo-1 do
   begin
      var obj: TObject := nil;
      if AWithObjects then
         obj := ALines.Objects[i];
      AResult.AddObject(ALines[i], obj);
   end;
end;

// Resolves lines enclosed by two lines which are identical in previously generated code,
// in editor and in freshly generated code. ABaseFrom..ABaseTo are previously generated lines,
// AEditorFrom..AEditorTo are lines currently in editor and ANewFrom..ANewTo are freshly
// generated lines; each of these ranges may be empty
procedure MergeChunk(AResult: TStringList; ABase, AEditor, ANew: TStrings; const ADiff: TLinesDiff;
                     ABaseFrom, ABaseTo, AEditorFrom, AEditorTo, ANewFrom, ANewTo: integer);
begin
   if ABaseFrom >= ABaseTo then
   begin
      // no previously generated line here, so generator and user both only added lines; keep them all
      AddLines(AResult, ANew, ANewFrom, ANewTo, True);
      AddLines(AResult, AEditor, AEditorFrom, AEditorTo, False);
      Exit;
   end;
   if SameLines(ANew, ANewFrom, ANewTo, ABase, ABaseFrom, ABaseTo) then
   begin
      // generator repeated what it generated before, so everything here comes from the user
      var sameCount := (AEditorTo-AEditorFrom) = (ANewTo-ANewFrom);
      for var i := AEditorFrom to AEditorTo-1 do
      begin
         var obj: TObject := nil;
         if sameCount then
            obj := ANew.Objects[ANewFrom+i-AEditorFrom];   // user only edited the lines so keep them bound to flowchart
         AResult.AddObject(AEditor[i], obj);
      end;
      Exit;
   end;
   var alignedCount := (ABaseTo-ABaseFrom) = (ANewTo-ANewFrom);
   var keepUserLines := not SameLines(AEditor, AEditorFrom, AEditorTo, ABase, ABaseFrom, ABaseTo);   // false when user changed nothing here
   for var i := ABaseFrom to ABaseTo-1 do
   begin
      if ADiff.Map1[i] = ROW_NOT_FOUND then
      begin
         keepUserLines := False;       // user and generator changed the same line so the generated one wins
         break;
      end;
   end;
   if keepUserLines and alignedCount then
   begin
      // each freshly generated line takes the place of the line it replaces, so lines the user
      // added here stay where the user put them
      for var i := AEditorFrom to AEditorTo-1 do
      begin
         var b := ADiff.Map2[i];
         if b = ROW_NOT_FOUND then
            AResult.AddObject(AEditor[i], nil)
         else
         begin
            var n := ANewFrom + b - ABaseFrom;
            AResult.AddObject(ANew[n] + ADiff.Tails[b], ANew.Objects[n]);   // put back what the user appended to the replaced line
         end;
      end;
      Exit;
   end;
   for var i := ANewFrom to ANewTo-1 do
   begin
      var tail := '';
      if alignedCount then
         tail := ADiff.Tails[ABaseFrom+i-ANewFrom];   // put back what the user appended to the replaced line
      AResult.AddObject(ANew[i] + tail, ANew.Objects[i]);
   end;
   if keepUserLines then
   begin
      for var i := AEditorFrom to AEditorTo-1 do
      begin
         if ADiff.Map2[i] = ROW_NOT_FOUND then
            AResult.AddObject(AEditor[i], nil);   // user only added lines here so they survive regeneration
      end;
   end;
end;

// Three way merge of code in editor. ABase is code as the generator produced it last time,
// AEditor is the current content of editor and ANew is freshly generated code. Lines added,
// modified or removed by the user are carried over to the result unless the generator
// has changed the very same lines in the meantime
function MergeLines(ABase, AEditor, ANew: TStrings): TStringList;
begin
   result := TStringList.Create;
   var diffEditor := DiffLines(ABase, AEditor);
   MatchExtendedLines(ABase, AEditor, diffEditor);
   var diffNew := DiffLines(ABase, ANew);
   var baseFrom := 0;
   var editorFrom := 0;
   var newFrom := 0;
   for var i := 0 to ABase.Count-1 do
   begin
      var e := diffEditor.Map1[i];
      var n := diffNew.Map1[i];
      if (e <> ROW_NOT_FOUND) and (n <> ROW_NOT_FOUND) then    // line left intact by user and by generator
      begin
         MergeChunk(result, ABase, AEditor, ANew, diffEditor, baseFrom, i, editorFrom, e, newFrom, n);
         result.AddObject(ANew[n] + diffEditor.Tails[i], ANew.Objects[n]);
         baseFrom := i + 1;
         editorFrom := e + 1;
         newFrom := n + 1;
      end;
   end;
   MergeChunk(result, ABase, AEditor, ANew, diffEditor, baseFrom, ABase.Count, editorFrom, AEditor.Count, newFrom, ANew.Count);
end;

function LastSectionRow(ALines: TStrings; AObject: TObject; AFirstRow: integer): integer;
begin
   if AObject is TBlock then
      result := TBlock(AObject).FindLastRow(AFirstRow, ALines)
   else
      result := TInfra.FindLastRow(AObject, AFirstRow, ALines);
end;

procedure CopyRows(ALines: TStrings; AFirstRow, ALastRow: integer; ADestLines: TStringList);
begin
   for var i := AFirstRow to ALastRow do
      ADestLines.AddObject(ALines[i], ALines.Objects[i]);
end;

procedure ReplaceRows(ALines: TStrings; AFirstRow, ALastRow: integer; ANewLines: TStrings);
begin
   for var i := ALastRow downto AFirstRow do
      ALines.Delete(i);
   for var i := ANewLines.Count-1 downto 0 do
      ALines.InsertObject(AFirstRow, ANewLines[i], ANewLines.Objects[i]);
end;

// Copies lines which AObject generated, together with whatever the user put between them
function CopySection(ALines: TStrings; AObject: TObject; ADestLines: TStringList): boolean;
begin
   var firstRow := ALines.IndexOfObject(AObject);
   result := firstRow <> ROW_NOT_FOUND;
   if result then
      CopyRows(ALines, firstRow, LastSectionRow(ALines, AObject, firstRow), ADestLines);
end;

// Removing a flowchart object can be undone, so its code section disappears from editor only
// for the time being. Remembers that section as the generator made it and as the user left it,
// and takes it out of editor as a whole, user changes included, so that all of it can be
// brought back when the removal is undone
procedure TEditorForm.StoreUndoSection(AObject: TObject; AEditorLines, ANewLines: TStrings);
begin
   FUndoObject := AObject;
   FUndoBase.Clear;
   FUndoLines.Clear;
   if (AObject = nil) or (AEditorLines = nil) or (ANewLines = nil) then
      Exit;
   if ANewLines.IndexOfObject(AObject) <> ROW_NOT_FOUND then
      Exit;                            // object still generates code so nothing goes away
   var firstRow := AEditorLines.IndexOfObject(AObject);
   if (firstRow = ROW_NOT_FOUND) or not CopySection(FGeneratedLines, AObject, FUndoBase) then
   begin
      FUndoBase.Clear;
      Exit;
   end;
   var lastRow := LastSectionRow(AEditorLines, AObject, firstRow);
   CopyRows(AEditorLines, firstRow, lastRow, FUndoLines);
   for var i := lastRow downto firstRow do
      AEditorLines.Delete(i);
end;

// Code section of a removed object showed up again because its removal has been undone, so
// user changes which went away with it are merged back into the regenerated lines
function TEditorForm.RestoreUndoSection(ALines: TStringList): boolean;
begin
   result := False;
   if (FUndoObject = nil) or FUndoBase.IsEmpty or FUndoLines.IsEmpty then
      Exit;
   var firstRow := ALines.IndexOfObject(FUndoObject);
   if firstRow = ROW_NOT_FOUND then
      Exit;
   var lastRow := LastSectionRow(ALines, FUndoObject, firstRow);
   var section := TStringList.Create;
   try
      CopyRows(ALines, firstRow, lastRow, section);
      var merged := MergeLines(FUndoBase, FUndoLines, section);
      try
         ReplaceRows(ALines, firstRow, lastRow, merged);
      finally
         merged.Free;
      end;
   finally
      section.Free;
   end;
   FUndoBase.Clear;
   FUndoLines.Clear;
   result := True;
end;

procedure TEditorForm.SetGeneratedLines(ALines: TStrings);
begin
   FGeneratedLines.Clear;
   for var i := 0 to ALines.Count-1 do
      FGeneratedLines.AddObject(ALines[i], ALines.Objects[i]);   // objects are only ever compared, never used
end;

// Blocks update the line they generated directly in editor instead of regenerating the whole
// program. Replaces the previously generated line behind the line about to be updated with its
// new text so that both stay in step, and returns text the user had appended to that line so
// that the caller can put it back. The previously generated line is found through the object
// the code range belongs to, at the same place within its section as the updated line in editor
function TEditorForm.ReplaceGeneratedLine(const AChangeLine: TChangeLine): string;
begin
   result := '';
   var codeRange := AChangeLine.CodeRange;
   if (codeRange.Lines = nil) or (AChangeLine.Row < codeRange.FirstRow) or (AChangeLine.Row > codeRange.LastRow) then
      Exit;
   var editorLine := codeRange.Lines[AChangeLine.Row];
   if editorLine = AChangeLine.Text then
      Exit;                            // nothing changes, e.g. line without placeholder is taken from editor as it is
   var obj := codeRange.Lines.Objects[codeRange.FirstRow];
   var firstRow := FGeneratedLines.IndexOfObject(obj);
   if firstRow = ROW_NOT_FOUND then
      Exit;
   var lastRow := LastSectionRow(FGeneratedLines, obj, firstRow);
   var row := firstRow + AChangeLine.Row - codeRange.FirstRow;
   if (AChangeLine.Row = codeRange.LastRow) and (AChangeLine.Row > codeRange.FirstRow) then
      row := lastRow;                  // placeholder is in the last line of the template, see TInfra.GetChangeLine
   if row > lastRow then
      Exit;
   var generatedLine := FGeneratedLines[row];
   if (editorLine.Length > generatedLine.Length) and editorLine.StartsWith(generatedLine) and not generatedLine.Trim.IsEmpty then
      result := editorLine.Substring(generatedLine.Length);
   FGeneratedLines[row] := AChangeLine.Text;
end;

// Multi-line blocks regenerate their whole section in editor instead of the whole program.
// Merges what the user changed within that section into its freshly generated lines ANewLines
// the same way regeneration of the whole program does, and keeps generated lines in step
procedure TEditorForm.MergeGeneratedSection(const ACodeRange: TCodeRange; ANewLines: TStringList);
begin
   if (ACodeRange.Lines = nil) or (ACodeRange.FirstRow < 0) or (ACodeRange.LastRow < ACodeRange.FirstRow) then
      Exit;
   var obj := ACodeRange.Lines.Objects[ACodeRange.FirstRow];
   var firstRow := FGeneratedLines.IndexOfObject(obj);
   if firstRow = ROW_NOT_FOUND then
      Exit;
   var lastRow := LastSectionRow(FGeneratedLines, obj, firstRow);
   var base: TStringList := nil;
   var section: TStringList := nil;
   try
      base := TStringList.Create;
      section := TStringList.Create;
      CopyRows(FGeneratedLines, firstRow, lastRow, base);
      CopyRows(ACodeRange.Lines, ACodeRange.FirstRow, ACodeRange.LastRow, section);
      ReplaceRows(FGeneratedLines, firstRow, lastRow, ANewLines);
      var merged := MergeLines(base, section, ANewLines);
      try
         ANewLines.Assign(merged);
      finally
         merged.Free;
      end;
   finally
      section.Free;
      base.Free;
   end;
end;

procedure TEditorForm.DisplayLines(ALines: TStringList; AReset: boolean);
begin
   if (ALines = nil) or ALines.IsEmpty then
      Exit;
   if GSettings.IndentChar = TAB_CHAR then
      TInfra.IndentSpacesToTabs(ALines);
   var merged: TStringList := nil;
   try
      if not FGeneratedLines.IsEmpty then
      begin
         var editorLines := GetAllLines;    // must be read before fold ranges are destroyed
         try
            if GClpbrd.UndoObject <> FUndoObject then
               StoreUndoSection(GClpbrd.UndoObject, editorLines, ALines);
            if editorLines.Count > 0 then
               merged := MergeLines(FGeneratedLines, editorLines, ALines);
         finally
            editorLines.Free;
         end;
      end;
      SetGeneratedLines(ALines);
      var lines: TStrings := ALines;
      if merged <> nil then
      begin
         RestoreUndoSection(merged);
         lines := merged;
      end;
{$IFDEF USE_CODEFOLDING}
      memCodeEditor.AllFoldRanges.DestroyAll;
{$ENDIF}
      if AReset then
         memCodeEditor.Marks.Clear;
      memCodeEditor.Highlighter := nil;
      memCodeEditor.Lines.Assign(lines);
   finally
      merged.Free;
   end;
   if GSettings.EditorShowRichText then
      memCodeEditor.Highlighter := GInfra.CurrentLang.HighLighter;
   OnChangeEditor;
   if FFocusEditor then
   begin
      if memCodeEditor.CanFocus then
         memCodeEditor.SetFocus;
   end
   else
      FFocusEditor := True;
   memCodeEditor.ClearUndo;
   memCodeEditor.Modified := not AReset;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
   var programLines := GInfra.GenerateProgram;
   try
      DisplayLines(programLines, True);
      GProject.SetChanged;
   finally
      programLines.Free;
   end;
end;

procedure TEditorForm.pmPopMenuPopup(Sender: TObject);
begin
   FWithFocus := nil;
   miFindProj.Enabled := False;
   miCut.Enabled := memCodeEditor.SelAvail;
   miCopy.Enabled := miCut.Enabled;
   miCopyRichText.Enabled := miCopy.Enabled and (memCodeEditor.Highlighter <> nil);
   miRemove.Enabled := miCut.Enabled;
   miPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
   miPasteComment.Enabled := miPaste.Enabled;
   miUndo.Enabled := memCodeEditor.CanUndo;
   miRedo.Enabled := memCodeEditor.CanRedo;
   var pnt := memCodeEditor.ScreenToClient(Mouse.CursorPos);
   var dispCoord := memCodeEditor.PixelsToRowColumn(pnt.X, pnt.Y);
   if dispCoord.Row > 0 then
   begin
      var obj := memCodeEditor.Lines.Objects[dispCoord.Row-1];
      miFindProj.Enabled := TInfra.IsValidControl(obj) and Supports(obj, IWithFocus, FWithFocus) and FWithFocus.CanBeFocused;
   end;
end;

procedure TEditorForm.ExecuteCopyToClipboard(AIfRichText: boolean);
begin
   if AIfRichText then
   begin
      SynExporterRTF1.Highlighter := memCodeEditor.Highlighter;
      with memCodeEditor do
         SynExporterRTF1.ExportRange(Lines, BlockBegin, BlockEnd);
      SynExporterRTF1.CopyToClipboard;
      SynExporterRTF1.Highlighter := nil;
   end
   else
      Clipboard.AsText := memCodeEditor.SelText;
end;

procedure TEditorForm.miUndoClick(Sender: TObject);
begin
   if Sender = miUndo then
      memCodeEditor.Undo
   else if Sender = miRedo then
      memCodeEditor.Redo
   else if Sender = miCut then
      memCodeEditor.CutToClipboard
   else if Sender = miCopy then
      ExecuteCopyToClipboard(False)
   else if Sender = miCopyRichText then
      ExecuteCopyToClipboard(True)
   else if Sender = miPaste then
      memCodeEditor.PasteFromClipboard
   else if Sender = miRemove then
      memCodeEditor.ClearSelection
   else if Sender = miSelectAll then
      memCodeEditor.SelectAll
   else if (Sender = miPasteComment) and Clipboard.HasFormat(CF_TEXT) then
      PasteComment(Clipboard.AsText);
end;

procedure TEditorForm.ReplaceDialogReplace(Sender: TObject);
begin
   var txt := '';
   Clipboard.Open;
   try
      if Clipboard.HasFormat(CF_TEXT) then
         txt := Clipboard.AsText;
      Clipboard.AsText := ReplaceDialog.ReplaceText;
      if frReplaceAll in ReplaceDialog.Options then
      begin
         memCodeEditor.SelStart := 0;
         while True do
         begin
            var i := TInfra.PosText(ReplaceDialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart+1, frMatchCase in ReplaceDialog.Options);
            if i = 0 then
               Exit;
            memCodeEditor.SelStart := i - 1;
            memCodeEditor.SelLength := ReplaceDialog.FindText.Length;
            memCodeEditor.ClearSelection;
            memCodeEditor.PasteFromClipboard;
            memCodeEditor.SelStart := memCodeEditor.SelStart + ReplaceDialog.ReplaceText.Length;
         end;
      end;
      if memCodeEditor.SelAvail then
      begin
         memCodeEditor.ClearSelection;
         memCodeEditor.PasteFromClipboard;
      end;
   finally
      if not txt.IsEmpty then
         Clipboard.AsText := txt;
      Clipboard.Close;
   end;
end;

procedure TEditorForm.FindDialogShow(Sender: TObject);
begin
   FDialog := TFindDialog(Sender);
end;

procedure TEditorForm.FindDialogClose(Sender: TObject);
begin
   FDialog := nil;
   memCodeEditor.Repaint;
end;

procedure TEditorForm.ReplaceDialogFind(Sender: TObject);
var
   i, startPos, len: integer;
   dialog: TFindDialog;
begin
   dialog := TFindDialog(Sender);
   len := dialog.FindText.Length;
   memCodeEditor.Repaint;
   if frDown in dialog.Options then
      i := TInfra.PosText(dialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart + memCodeEditor.SelLength + 1, frMatchCase in dialog.Options)
   else
   begin
      startPos := 1;
      while True do
      begin
         i := TInfra.PosText(dialog.FindText, memCodeEditor.Text, startPos, frMatchCase in dialog.Options);
         if (i > 0) and (i <= memCodeEditor.SelStart) then
            startPos := i + len
         else
         begin
            if startPos > 1 then
               i := startPos - len;
            if i >= memCodeEditor.SelStart then
               i := 0;
            break;
         end;
      end;
   end;
   if i > 0 then
   begin
      memCodeEditor.SetFocus;
      memCodeEditor.SelStart := i - 1;
      memCodeEditor.SelLength := len;
   end;
end;

procedure TEditorForm.InsertLibraryEntry(const ALibrary: string);
begin
   var libEntry := ALibrary;
   if not GInfra.CurrentLang.LibEntry.IsEmpty then
      libEntry := Format(GInfra.CurrentLang.LibEntry, [ALibrary])
   else if not GInfra.CurrentLang.LibEntryList.IsEmpty then       // this functionality is disabled for libs in LibEntryList
      Exit;
   var found := False;
   for var a := 0 to memCodeEditor.Lines.Count-1 do
   begin
      if memCodeEditor.Lines[a].TrimLeft.StartsWith(libEntry, not GInfra.CurrentLang.CaseSensitiveSyntax) then
      begin
         found := True;
         break;
      end;
   end;
   if not found then
   begin
      var libObj := TInfra.GetLibObject;
      var i := memCodeEditor.Lines.IndexOfObject(libObj);
      if i <> -1 then
      begin
         var indent := TInfra.ExtractIndentString(memCodeEditor.Lines[i]);
         memCodeEditor.Lines.InsertObject(i, indent + libEntry, libObj);
      end
      else if GProject.LibSectionOffset >= 0 then
      begin
         if not GInfra.CurrentLang.LibTemplate.IsEmpty then
         begin
            var lines := TStringList.Create;
            try
               lines.Text := GInfra.CurrentLang.LibTemplate;
               TInfra.InsertTemplateLines(lines, PRIMARY_PLACEHOLDER, libEntry, libObj);
               for var a := lines.Count-1 downto 0 do
                  memCodeEditor.Lines.InsertObject(GProject.LibSectionOffset, lines.Strings[a], lines.Objects[a]);
            finally
               lines.Free;
            end;
         end
         else
            memCodeEditor.Lines.InsertObject(GProject.LibSectionOffset, libEntry, libObj);
      end;
   end;
end;

procedure TEditorForm.miCompileClick(Sender: TObject);
begin
    SetSaveDialog(SaveDialog1);
    var main := GProject.GetMain;
    var command := GInfra.CurrentLang.CompilerCommand;
    var commandNoMain := GInfra.CurrentLang.CompilerCommandNoMain;
    if (not command.IsEmpty) or ((main = nil) and not commandNoMain.IsEmpty) then
    begin
       if SaveDialog1.Execute then
       begin
          SaveToFile(SaveDialog1.FileName);
          var fileName := ExtractFileName(SaveDialog1.FileName);
          var fileNameNoExt := fileName;
          var p := Pos('.', fileNameNoExt);
          if p > 0 then
             SetLength(fileNameNoExt, p-1);
          if main = nil then
          begin
             if commandNoMain.IsEmpty then
                commandNoMain := '%s3';
             command := ReplaceText(commandNoMain, '%s3', command);
          end;
          command := ReplaceText(command, '%s1', fileName);
          command := ReplaceText(command, '%s2', fileNameNoExt);
          if not TInfra.CreateDOSProcess(command, ExtractFileDir(SaveDialog1.FileName)) then
             TInfra.ShowErrorBox('CompileFail', [], errCompile);
       end;
    end
    else
       TInfra.ShowErrorBox('CompilerNotFound', [GInfra.CurrentLang.Name], errCompile)
end;

procedure TEditorForm.miPrintClick(Sender: TObject);
begin
   if not TInfra.IsPrinter then
      TInfra.ShowErrorBox('NoPrinter', [], errPrinter)
   else if (GProject <> nil) and MainForm.PrintDialog.Execute then
   begin
      with SynEditPrint1 do
      begin
         SynEdit := memCodeEditor;
         Title := GProject.Name;
         DocTitle := GProject.Name;
         LineNumbers := GSettings.EditorShowGutter;
         Copies := MainForm.PrintDialog.Copies;
         Print;
      end;
   end;
end;

procedure TEditorForm.miSaveClick(Sender: TObject);
begin
   SetSaveDialog(SaveDialog2);
   if Assigned(memCodeEditor.Highlighter) then
      SaveDialog2.Filter := SaveDialog2.Filter + '|' + trnsManager.GetJoinedString('|', EDITOR_DIALOG_FILTER_KEYS);
   if SaveDialog2.Execute then
   begin
      var synExport: TSynCustomExporter := nil;
      if SaveDialog2.FilterIndex > 1 then
      begin
         var filterKey := EDITOR_DIALOG_FILTER_KEYS[SaveDialog2.FilterIndex-2];
         if RTF_FILES_FILTER_KEY = filterKey then
            synExport := SynExporterRTF1
         else if HTML_FILES_FILTER_KEY = filterKey then
            synExport := SynExporterHTML1;
      end;
      if synExport <> nil then
      begin
         synExport.Highlighter := memCodeEditor.Highlighter;
         var lines := GetAllLines;
         try
            synExport.ExportAll(lines);
            synExport.SaveToFile(SaveDialog2.FileName);
         finally
            synExport.Highlighter := nil;
            lines.Free;
         end;
      end
      else
         SaveToFile(SaveDialog2.FileName);
   end;
end;

procedure TEditorForm.miFindClick(Sender: TObject);
var
   dialog: TFindDialog;
begin
   if Sender = miFind then
      dialog := FindDialog
   else
   begin
      dialog := ReplaceDialog;
      ReplaceDialog.ReplaceText := '';
   end;
   if memCodeEditor.SelAvail then
      dialog.FindText := memCodeEditor.SelText.Trim;
   dialog.Execute;
end;

procedure TEditorForm.memCodeEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
   if Changes * [scAll, scCaretX, scCaretY] <> [] then
   begin
      var p := memCodeEditor.CaretXY;
      stbEditorBar.Panels[0].Text := trnsManager.GetFormattedString('StatusBarInfo', [p.Line, p.Char]);
   end;
   if scModified in Changes then
   begin
      if memCodeEditor.Modified then
      begin
         stbEditorBar.Panels[1].Text := trnsManager.GetString('Modified');
         GProject.SetChanged;
      end
      else
         stbEditorBar.Panels[1].Text := '';
   end;
   if scInsertMode in Changes then
      stbEditorBar.Panels[2].Text := trnsManager.GetString(if memCodeEditor.InsertMode then 'InsertMode' else 'OverwriteMode');
end;

procedure TEditorForm.memCodeEditorGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
const
   MARK_FIRST_INDEX = 0;   // index of first bookmark image in MainForm.ImageList1
   MARK_LAST_INDEX = 4;    // index of last bookmark image in MainForm.ImageList1
   MAX_MARKS = MARK_LAST_INDEX - MARK_FIRST_INDEX + 1;
var
   i, a: integer;
   found: boolean;
begin
   if not Assigned(Mark) then
   begin
      if memCodeEditor.Marks.Count < MAX_MARKS then
      begin
         for i := MARK_FIRST_INDEX to MARK_LAST_INDEX do
         begin
            found := True;
            for a := 0 to memCodeEditor.Marks.Count-1 do
            begin
               if i = memCodeEditor.Marks[a].ImageIndex then
               begin
                  found := False;
                  break;
               end;
            end;
            if found then break;
         end;
         if not found then
            i := MARK_FIRST_INDEX;
         Mark := TSynEditMark.Create(memCodeEditor);
         Mark.ImageIndex := i;
         memCodeEditor.Marks.Add(Mark);
         Mark.Line := Line;
         Mark.Visible := True;
      end;
   end
   else
      memCodeEditor.Marks.Remove(Mark);
end;

procedure TEditorForm.miRegenerateClick(Sender: TObject);
begin
   OnShow(Self);
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   GotoForm.Close;
   memCodeEditor.TopLine := 0;
   memCodeEditor.SelStart := 0;
end;

procedure TEditorForm.memCodeEditorDblClick(Sender: TObject);
begin
   if memCodeEditor.SelAvail then
   begin
      var hnd := FindDialog.Handle;
      if hnd = 0 then
         hnd := ReplaceDialog.Handle;
      hnd := FindWindowEx(hnd, 0, 'Edit', nil);
      if hnd <> 0 then
         SetWindowText(hnd, PChar(memCodeEditor.SelText));
   end;
end;

procedure TEditorForm.memCodeEditorDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   var exportable: IExportable := nil;
   if State = dsDragEnter then
      memCodeEditor.SetFocus;
   if not ((Source is TComment) or Supports(Source, IExportable, exportable)) then
      Accept := False
   else with memCodeEditor do
      CaretXY := DisplayToBufferPos(PixelsToRowColumn(X, Y));
end;

procedure TEditorForm.memCodeEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
   var exportable: IExportable := nil;
   if Source is TComment then
      PasteComment(TComment(Source).Text)
   else if Supports(Source, IExportable, exportable) then
   begin
      memCodeEditor.BeginUpdate;
      var lines := TStringList.Create;
      try
         exportable.ExportCode(lines);
         var pos := memCodeEditor.PixelsToRowColumn(X, Y);
         for var i := 0 to lines.Count-1 do
            memCodeEditor.Lines.Insert(pos.Row + i - 1, StringOfChar(SPACE_CHAR, pos.Column - 1) + lines.Strings[i]);
      finally
         lines.Free;
         memCodeEditor.EndUpdate;
      end;
   end;
end;

procedure TEditorForm.miHelpClick(Sender: TObject);
begin
   HelpForm.Visible := not HelpForm.Visible;
end;

procedure TEditorForm.memCodeEditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
const
   Brackets = ['{', '[', '(', '<', '}', ']', ')', '>'];
var
   i, f, len: integer;
   c: char;
   hAttr: TSynHighlighterAttributes;
   p: TBufferCoord;
   s, s1: string;
   pos: TPoint;
   fontStyle: TFontStyles;
   brushColor, fontColor: TColor;
begin
   if FDialog <> nil then
   begin
      len := FDialog.FindText.Length;
      brushColor := Canvas.Brush.Color;
      fontStyle := Canvas.Font.Style;
      Canvas.Brush.Color := clYellow;
      for i := 0 to memCodeEditor.Lines.Count-1 do
      begin
         s := memCodeEditor.Lines[i];
         f := TInfra.PosText(FDialog.FindText, s, 1, frMatchCase in FDialog.Options);
         while f > 0 do
         begin
            p := BufferCoord(f, i+1);
            memCodeEditor.GetHighlighterAttriAtRowCol(p, s1, hAttr);
            s1 := Copy(s, f, len);
            pos := CharToPixels(p);
            if hAttr <> nil then
               Canvas.Font.Style := hAttr.Style;
            Canvas.TextOut(pos.X, pos.Y, s1);
            f := TInfra.PosText(FDialog.FindText, s, f+len, frMatchCase in FDialog.Options);
         end;
      end;
      Canvas.Brush.Color := brushColor;
      Canvas.Font.Style := fontStyle;
   end;
   i := memCodeEditor.SelStart;
   c := #0;
   if (i >= 0) and (i < memCodeEditor.Text.Length) then
      c := memCodeEditor.Text[i+1];
   if memCodeEditor.SelAvail or (memCodeEditor.Font.Color = MATCH_BRACKET_COLOR) or not CharInSet(c, Brackets) then
      Exit;
   p := memCodeEditor.CaretXY;
   s := c;
   if memCodeEditor.GetHighlighterAttriAtRowCol(p, s, hAttr) and (memCodeEditor.Highlighter.SymbolAttribute = hAttr) then
   begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Font.Assign(memCodeEditor.Font);
      Canvas.Font.Style := hAttr.Style;
      pos := CharToPixels(p);
      if TransientType = ttAfter then
      begin
         fontColor := MATCH_BRACKET_COLOR;
         brushColor := clNone;
      end
      else
      begin
         fontColor := hAttr.Foreground;
         brushColor := hAttr.Background;
      end;
      if fontColor = clNone then
         fontColor := memCodeEditor.Font.Color;
      if brushColor = clNone then
         brushColor := memCodeEditor.ActiveLineColor;
      Canvas.Brush.Color := brushColor;
      Canvas.Font.Color := fontColor;
      Canvas.TextOut(pos.X, pos.Y, s);
      P := memCodeEditor.GetMatchingBracketEx(p);
      if (p.Char > 0) and (p.Line > 0) then
      begin
         i := memCodeEditor.RowColToCharIndex(p);
         s := memCodeEditor.Text[i+1];
         pos := CharToPixels(p);
         if p.Line <> memCodeEditor.CaretY then
            Canvas.Brush.Color := memCodeEditor.Color;
         Canvas.Font.Color := fontColor;
         Canvas.TextOut(pos.X, pos.Y, s);
      end;
   end;
end;

function TEditorForm.CharToPixels(const P: TBufferCoord): TPoint;
begin
   result := memCodeEditor.RowColumnToPixels(memCodeEditor.BufferToDisplayPos(P));
end;

procedure TEditorForm.memCodeEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   p, p1: TBufferCoord;
   w, w1, h, scope: string;
   hAttr: TSynHighlighterAttributes;
   gCheck, lCheck: boolean;
   idInfo: TIdentInfo;
   obj: TObject;
   block: TBlock;
   i: integer;
begin
   h := '';
   FCloseBracketPosP := nil;
   memCodeEditor.ShowHint := False;
   memCodeEditor.Hint := '';
   p := memCodeEditor.DisplayToBufferPos(memCodeEditor.PixelsToRowColumn(X, Y));
   p1 := memCodeEditor.GetMatchingBracketEx(p);
   if (p1.Line > 0) and (p1.Line < p.Line) then
   begin
      i := p1.Line - 1;
      if (memCodeEditor.Lines[i].Trim.Length < 2) and (i > 0) and not memCodeEditor.Lines[i-1].Trim.IsEmpty then
         Dec(i);
      h := BuildBracketHint(i, p.Line-2);
      if not h.IsEmpty then
      begin
         FCloseBracketPos := memCodeEditor.ClientToScreen(CharToPixels(p))
                             - Point(3, memCodeEditor.LineHeight-memCodeEditor.Canvas.TextHeight('I')+1);
         FCloseBracketPosP := @FCloseBracketPos;
         memCodeEditor.Hint := h;
         memCodeEditor.ShowHint := True;
         Exit;
      end;
   end;
   w := memCodeEditor.GetWordAtRowCol(p);
   w1 := w;
   if w1.IsEmpty or (memCodeEditor.GetHighlighterAttriAtRowCol(p, w1, hAttr) and (memCodeEditor.Highlighter.StringAttribute = hAttr)) then
      Exit;
   block := nil;
   gCheck := True;
   lCheck := True;
   idInfo := TIdentInfo.New;
   obj := memCodeEditor.Lines.Objects[p.Line-1];
   idInfo.Ident := w;
   if TInfra.IsValidControl(obj) and (obj is TBlock) then
      block := TBlock(obj);
   TParserHelper.GetParameterInfo(GProject.FindFunctionHeader(block), idInfo);
   if idInfo.TType <> NOT_DEFINED then
   begin
      lCheck := False;
      gCheck := False;
   end;
   if lCheck then
   begin
      TParserHelper.GetVariableInfo(TParserHelper.FindUserFunctionVarList(block), idInfo);
      if idInfo.TType <> NOT_DEFINED then
         gCheck := False;
   end;
   if gCheck then
      idInfo := TParserHelper.GetIdentInfo(w);
   case idInfo.Scope of
      LOCAL: scope := trnsManager.GetString('VarLocal');
      PARAMETER: scope := trnsManager.GetString('VarParm');
   else
      scope := '';
   end;
   case idInfo.IdentType of
      VARRAY:
      begin
         h := trnsManager.GetFormattedString('HintArray', [scope, idInfo.DimensCount, w, idInfo.SizeAsString, idInfo.TypeAsString]);
         if (idInfo.SizeExpArrayAsString <> idInfo.SizeAsString) and not idInfo.SizeExpArrayAsString.IsEmpty then
            h := h + sLineBreak + trnsManager.GetFormattedString('HintArrayExp', [idInfo.TypeAsString, sLineBreak, scope, idInfo.DimensCount, w, idInfo.SizeExpArrayAsString, idInfo.TypeOriginalAsString]);
      end;
      VARIABLE:   h := trnsManager.GetFormattedString('HintVar', [scope, w, idInfo.TypeAsString]);
      CONSTANT:   h := trnsManager.GetFormattedString('HintConst', [w, idInfo.Value]);
      ROUTINE_ID: h := trnsManager.GetFormattedString('HintRoutine', [w, idInfo.TypeAsString]);
      ENUM_VALUE: h := trnsManager.GetFormattedString('HintEnum', [w, idInfo.TypeAsString]);
   end;
   if not h.IsEmpty then
   begin
      memCodeEditor.Hint := h;
      memCodeEditor.ShowHint := True;
   end;
end;

function TEditorForm.GetAllLines: TStrings;
begin
{$IFDEF USE_CODEFOLDING}
   result := memCodeEditor.GetUncollapsedStrings;
{$ELSE}
   result := TStringList.Create;
   result.Assign(memCodeEditor.Lines);
{$ENDIF}
end;

procedure TEditorForm.SaveToFile(const APath: string);
begin
   with GetAllLines do
   try
      SaveToFile(APath, GInfra.CurrentLang.GetFileEncoding);
   finally
      Free;
   end;
end;

function TEditorForm.SelectCodeRange(AObject: TObject; ADoSelect: boolean = True): TCodeRange;
begin
   result := TCodeRange.New;
   var lines := GetAllLines;
   result.FirstRow := lines.IndexOfObject(AObject);
   lines.Free;
   if result.FirstRow <> ROW_NOT_FOUND then
   begin
{$IFDEF USE_CODEFOLDING}
      result.FirstRow := memCodeEditor.Lines.IndexOfObject(AObject);
      if result.FirstRow = ROW_NOT_FOUND then
      begin
         for var i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
         begin
            result.Lines := memCodeEditor.AllFoldRanges[i].CollapsedLines;
            result.FirstRow := result.Lines.IndexOfObject(AObject);
            if result.FirstRow <> ROW_NOT_FOUND then
            begin
               ADoSelect := False;
               result.IsFolded := True;
               result.FoldRange := memCodeEditor.AllFoldRanges[i];
               break;
            end
            else
               result.Lines := nil;
         end;
      end
      else
         result.Lines := memCodeEditor.Lines;
{$ELSE}
      result.Lines := memCodeEditor.Lines;
{$ENDIF}
      if result.Lines <> nil then
      begin
         result.LastRow := if AObject is TBlock then
                              TBlock(AObject).FindLastRow(result.FirstRow, result.Lines)
                           else
                              TInfra.FindLastRow(AObject, result.FirstRow, result.Lines);
         if ADoSelect then
         begin
            with memCodeEditor do
            begin
               CaretXY := BufferCoord(result.Lines[result.LastRow].Length+1, result.LastRow+1);;
               EnsureCursorPosVisible;
               SelStart := RowColToCharIndex(CaretXY);
               SelEnd := RowColToCharIndex(BufferCoord(1, result.FirstRow+1));
            end;
         end;
{$IFDEF USE_CODEFOLDING}
         if not result.IsFolded and not ADoSelect then
         begin
            for var i := result.FirstRow to result.LastRow do
            begin
               if result.Lines.Objects[i] = AObject then
               begin
                  var foldRange := memCodeEditor.CollapsableFoldRangeForLine(i+1);
                  if (foldRange <> nil) and foldRange.Collapsed then
                  begin
                     result.FoldRange := foldRange;
                     break;
                  end
               end;
            end;
         end;
{$ENDIF}
      end;
   end;
end;

procedure TEditorForm.SetCaretPos(const ALine: TChangeLine);
begin
   if ALine.CodeRange.Lines = memCodeEditor.Lines then
   begin
      var col := ALine.Col + ALine.EditCaretXY.Char;
      var line := ALine.Row + ALIne.EditCaretXY.Line + 1;
      if (line > ALine.CodeRange.FirstRow) and (line <= ALine.CodeRange.LastRow+1) and (line <= ALine.CodeRange.Lines.Count) then
      begin
         memCodeEditor.CaretXY := BufferCoord(col, line);
         memCodeEditor.EnsureCursorPosVisible;
      end;
   end;
end;

procedure TEditorForm.UnSelectCodeRange(AObject: TObject);
begin
   if memCodeEditor.SelAvail and memCodeEditor.CanFocus then
   begin
      var codeRange := SelectCodeRange(AObject, False);
      if (codeRange.FirstRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart).Line-1) and
         (codeRange.LastRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelEnd).Line-1) then
            memCodeEditor.SelStart := memCodeEditor.SelEnd;
   end;
end;

{$IFDEF USE_CODEFOLDING}
procedure TEditorForm.RemoveFoldRange(var AFoldRange: TSynEditFoldRange);
begin
   var idx := memCodeEditor.AllFoldRanges.AllRanges.IndexOf(AFoldRange);
   if idx <> -1 then
      memCodeEditor.AllFoldRanges.AllRanges.Delete(idx);
   AFoldRange.Free;
   AFoldRange := nil;
end;

function TEditorForm.FindFoldRangeInCodeRange(const ACodeRange: TCodeRange; ACount: integer): TSynEditFoldRange;
begin
   result := nil;
   if ACodeRange.Lines = memCodeEditor.Lines then
   begin
      for var i := ACodeRange.FirstRow to ACodeRange.FirstRow+ACount do
      begin
         result := memCodeEditor.CollapsableFoldRangeForLine(i+1);
         if result <> nil then
            break;
      end;
   end;
end;
{$ENDIF}

procedure TEditorForm.UpdateEditorForBlock(ABlock: TBlock; const AChangeLine: TChangeLine);
begin
   if ABlock.ShouldUpdateEditor then
   begin
      var chLine := AChangeLine;
      chLine.Text := chLine.Text + ReplaceGeneratedLine(AChangeLine);   // put back what the user appended to this line
      if chLine.Change then
         memCodeEditor.Modified := True;
   end;
   SetCaretPos(AChangeLine);
end;

procedure TEditorForm.RefreshEditorForObject(AObject: TObject);
begin
   FFocusEditor := False;
   var gotoLine := False;
   var topLine := memCodeEditor.TopLine;
   var caretXY := memCodeEditor.CaretXY;
   var scrollEnabled := memCodeEditor.ScrollBars <> TScrollStyle.ssNone;
   if scrollEnabled then
      memCodeEditor.BeginUpdate;
   var programLines := GInfra.GenerateProgram;
   memCodeEditor.LockDrawing;
   try
      DisplayLines(programLines, False);
      if AObject <> nil then
      begin
         var codeRange := SelectCodeRange(AObject, False);
         var line := codeRange.FirstRow + 1;
         if (line > 0) and not codeRange.IsFolded then
         begin
            gotoLine := (line < topLine) or (line > topLine + memCodeEditor.LinesInWindow);
            if gotoLine then
               memCodeEditor.GotoLineAndCenter(line);
         end;
      end;
   finally
      programLines.Free;
      if not gotoLine then
      begin
         memCodeEditor.CaretXY := caretXY;
         memCodeEditor.TopLine := topLine;
      end;
      memCodeEditor.UnlockDrawing;
      if scrollEnabled then
         memCodeEditor.EndUpdate;
   end;
end;

function TEditorForm.GetIndentLevel(idx: integer; ALines: TStrings): integer;
begin
   result := 0;
   if (idx >= 0) and (idx < ALines.Count) then
   begin
      var line := ALines[idx];
      for var i := 1 to line.Length do
      begin
         if line[i] = GSettings.IndentChar then
            result := i
         else
            break;
      end;
      if GSettings.IndentLength > 0 then
         result := result div GSettings.IndentLength;
   end;
end;

procedure TEditorForm.ExportToXML(ANode: IXMLNode);
begin
   if Visible then
   begin
      SetNodeAttrBool(ANode, 'src_win_show', True);
      SetNodeAttrInt(ANode, 'src_win_x', Left);
      SetNodeAttrInt(ANode, 'src_win_y', Top);
      SetNodeAttrInt(ANode, 'src_win_w', Width);
      SetNodeAttrInt(ANode, 'src_win_h', Height);
      SetNodeAttrInt(ANode, 'src_win_sel_start', memCodeEditor.SelStart);
      if memCodeEditor.SelAvail then
         SetNodeAttrInt(ANode, 'src_win_sel_length', memCodeEditor.SelLength);
      var i := 0;
      for i := 0 to memCodeEditor.Marks.Count-1 do
      begin
         var mark := memCodeEditor.Marks[i];
         var node := AppendNode(ANode, 'src_win_mark');
         SetNodeAttrInt(node, 'line', mark.Line);
         SetNodeAttrInt(node, 'index', mark.ImageIndex);
      end;
      if memCodeEditor.TopLine > 1 then
         SetNodeAttrInt(ANode, 'src_top_line', memCodeEditor.TopLine);
      if WindowState = wsMinimized then
         SetNodeAttrBool(ANode, 'src_win_min', True);
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         var node: IXMLNode := nil;
         for i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
         begin
            var foldRange := memCodeEditor.AllFoldRanges[i];
            if foldRange.Collapsed then
            begin
               if node = nil then
                  node := AppendNode(ANode, 'fold_ranges');
               AppendNode(node, 'fold_range').Text := memCodeEditor.GetRealLineNumber(foldRange.FromLine).ToString;
            end;
         end;
      end;
{$ENDIF}
      var lines := GetAllLines;
      try
         for i := 0 to lines.Count-1 do
         begin
            var node := AppendNode(ANode, 'text_line');
            var withId: IWithId := nil;
            SetCDataChild(node, lines[i]);
            if TInfra.IsValidControl(lines.Objects[i]) and Supports(lines.Objects[i], IWithId, withId) then
               SetNodeAttrInt(node, ID_ATTR, withId.Id);
         end;
      finally
         lines.Free;
      end;
   end;
end;

procedure TEditorForm.ImportFromXML(ANode: IXMLNode);
begin
   if GetNodeAttrBool(ANode, 'src_win_show', False) and GInfra.CurrentLang.EnabledCodeGenerator then
   begin
      Position := poDesigned;
      SetBounds(GetNodeAttrInt(ANode, 'src_win_x'),
                GetNodeAttrInt(ANode, 'src_win_y'),
                GetNodeAttrInt(ANode, 'src_win_w'),
                GetNodeAttrInt(ANode, 'src_win_h'));
      if GetNodeAttrBool(ANode, 'src_win_min', False) then
         WindowState := wsMinimized;
      var showEvent := OnShow;
      OnShow := nil;
      try
         Show;
      finally
         OnShow := showEvent;
      end;
      ANode.OwnerDocument.PreserveWhiteSpace := True;
      memCodeEditor.Lines.BeginUpdate;
      var lineNodes := FilterNodes(ANode, 'text_line');
      var lineNode := lineNodes.NextNode;
      while lineNode <> nil do
      begin
         memCodeEditor.Lines.AddObject(lineNode.Text, GProject.FindObject(GetNodeAttrInt(lineNode, ID_ATTR, ID_UNDEFINED)));
         lineNode := lineNodes.NextNode;
      end;
      memCodeEditor.Lines.EndUpdate;
      if GSettings.EditorShowRichText then
         memCodeEditor.Highlighter := GInfra.CurrentLang.HighLighter;
      memCodeEditor.ClearUndo;
      memCodeEditor.SetFocus;
      memCodeEditor.SelStart := GetNodeAttrInt(ANode, 'src_win_sel_start');
      memCodeEditor.SelLength := GetNodeAttrInt(ANode, 'src_win_sel_length', 0);
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         memCodeEditor.ReScanForFoldRanges;
         var node := FindNode(ANode, 'fold_ranges');
         if node <> nil then
         begin
            var foldLines := TStringList.Create;
            try
               var rangeNodes := FilterNodes(node, 'fold_range');
               var rangeNode := rangeNodes.NextNode;
               while rangeNode <> nil do
               begin
                  if StrToIntDef(rangeNode.Text, 0) > 0 then
                     foldLines.Add(rangeNode.Text);
                  rangeNode := rangeNodes.NextNode;
               end;
               foldLines.CustomSort(@CompareIntegers);
               for var i := foldLines.Count-1 downto 0 do
               begin
                  var foldRange := memCodeEditor.CollapsableFoldRangeForLine(foldLines[i].ToInteger);
                  if (foldRange <> nil) and not foldRange.Collapsed then
                  begin
                     memCodeEditor.Collapse(foldRange);
                     memCodeEditor.Refresh;
                  end;
               end;
            finally
               foldLines.Free;
            end;
         end;
      end;
{$ENDIF}
      ANode.OwnerDocument.PreserveWhiteSpace := False;
      var i := GetNodeAttrInt(ANode, 'src_top_line', 0);
      if i > 0 then
         memCodeEditor.TopLine := i;
      var markNodes := FilterNodes(ANode, 'src_win_mark');
      var markNode := markNodes.NextNode;
      while markNode <> nil do
      begin
         var mark := TSynEditMark.Create(memCodeEditor);
         mark.ImageIndex := GetNodeAttrInt(markNode, 'index', 0);
         memCodeEditor.Marks.Add(mark);
         mark.Line := GetNodeAttrInt(markNode, 'line', 0);
         mark.Visible := True;
         markNode := markNodes.NextNode;
      end;
      var genLines := GInfra.GenerateProgram;
      try
         if GSettings.IndentChar = TAB_CHAR then
            TInfra.IndentSpacesToTabs(genLines);
         SetGeneratedLines(genLines);   // flowchart is unchanged since save so this is what the loaded code is based on
      finally
         genLines.Free;
      end;
   end;
end;

procedure TEditorForm.SetSaveDialog(ASaveDialog: TSaveDialog);
begin
   with ASaveDialog do
   begin
      DefaultExt := GInfra.CurrentLang.DefaultExt;
      Filter := trnsManager.GetFormattedString('SourceFilesFilter', [GInfra.CurrentLang.Name, DefaultExt, DefaultExt]);
      FileName := if GProject.Name.IsEmpty then trnsManager.GetString('Unknown') else GProject.Name;
   end;
end;

procedure TEditorForm.miGotoClick(Sender: TObject);
begin
   GotoForm.Show;
end;

{$IFDEF USE_CODEFOLDING}
procedure TEditorForm.ReloadFoldRegions;
begin
   memCodeEditor.CodeFolding.FoldRegions.Clear;
   for var fr in GInfra.CurrentLang.FoldRegions do
      memCodeEditor.CodeFolding.FoldRegions.Add(fr.RegionType, fr.AddClose, fr.NoSubFolds, fr.WholeWords, PChar(fr.Open), PChar(fr.Close));
   memCodeEditor.InitCodeFolding;
end;
{$ENDIF}

procedure TEditorForm.miCollapseAllClick(Sender: TObject);
begin
{$IFDEF USE_CODEFOLDING}
   if Sender = miCollapseAll then
      memCodeEditor.CollapseAll
   else if Sender = miUnCollapseAll then
      memCodeEditor.UncollapseAll;
{$ENDIF}
end;

procedure TEditorForm.miRichTextClick(Sender: TObject);
begin
   if Sender = miRichText then
      memCodeEditor.Highlighter := if miRichText.Checked then GInfra.CurrentLang.Highlighter else nil
   else if Sender = miCodeFoldingEnable then
   begin
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled and not miCodeFoldingEnable.Checked then
         memCodeEditor.UnCollapseAll;
      memCodeEditor.CodeFolding.Enabled := miCodeFoldingEnable.Checked;
      memCodeEditor.CodeFolding.HighlighterFoldRegions := False;
      miIndentGuides.Enabled := memCodeEditor.CodeFolding.Enabled;
      miCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      miUnCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      if not miIndentGuides.Enabled then
         miIndentGuides.Checked := False;
      memCodeEditor.CodeFolding.IndentGuides := miIndentGuides.Checked;
      memCodeEditor.Gutter.RightOffset := IfThen(memCodeEditor.CodeFolding.Enabled, 21);
{$ENDIF}
   end
   else if Sender = miStatusBar then
   begin
      stbEditorBar.Visible := miStatusBar.Checked;
      memCodeEditor.Height := if stbEditorBar.Visible then (ClientHeight - stbEditorBar.Height) else ClientHeight;
   end
   else if Sender = miScrollbars then
      memCodeEditor.ScrollBars := if miScrollbars.Checked then TScrollStyle.ssBoth else TScrollStyle.ssNone
   else if Sender = miGutter then
      memCodeEditor.Gutter.Visible := miGutter.Checked
   else if Sender = miIndentGuides then
   begin
{$IFDEF USE_CODEFOLDING}
      memCodeEditor.CodeFolding.IndentGuides := miIndentGuides.Checked;
{$ENDIF}
   end;
   GSettings.LoadFromEditor;
end;

procedure TEditorForm.memCodeEditorChange(Sender: TObject);
begin
{$IFDEF USE_CODEFOLDING}
   if memCodeEditor.CodeFolding.Enabled then
      memCodeEditor.ReScanForFoldRanges;
{$ENDIF}
end;

procedure TEditorForm.OnChangeEditor;
begin
   memCodeEditorChange(memCodeEditor);
end;

procedure TEditorForm.miFindProjClick(Sender: TObject);
begin
   if (FWithFocus <> nil) and FWithFocus.CanBeFocused then
   begin
      var focusInfo := TFocusInfo.New;
      var point := memCodeEditor.ScreenToClient(pmPopMenu.PopupPoint);
      var displ := memCodeEditor.PixelsToRowColumn(point.X, point.Y);
      if displ.Row > 0 then
      begin
         var selStart := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart);
         selStart.Line := selStart.Line - 1;
         var sLine := memCodeEditor.Lines[selStart.Line];
         focusInfo.SelStart := Max(selStart.Char - sLine.Length + sLine.TrimLeft.Length, 1);
         if memCodeEditor.SelAvail then
         begin
            var selEnd := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart + memCodeEditor.SelLength);
            selEnd.Line := selEnd.Line - 1;
            if selStart.Line <> selEnd.Line then
            begin
               var selText := '';
               for var i := selStart.Line to selEnd.Line do
               begin
                  sline := memCodeEditor.Lines[i];
                  if i = selStart.Line then
                     sLine := RightStr(sline, sline.Length - selStart.Char + 1)
                  else if i = selEnd.Line then
                     sline := LeftStr(sline, selEnd.Char - 1);
                  selText := selText + sLine.TrimLeft + IfThen(i <> selEnd.Line, #10);
               end;
               focusInfo.SelText := selText;
            end
            else
               focusInfo.SelText := MidStr(sLine, selStart.Char, memCodeEditor.SelLength).TrimLeft;
            focusInfo.Line := selStart.Line;
         end
         else
            focusInfo.Line := displ.Row - 1;
         focusInfo.LineText := memCodeEditor.Lines[focusInfo.Line].TrimLeft;
         var codeRange := SelectCodeRange(memCodeEditor.Lines.Objects[focusInfo.Line], False);
         if codeRange.FirstRow <> ROW_NOT_FOUND then
            focusInfo.RelativeLine := focusInfo.Line - codeRange.FirstRow;
      end;
      FWithFocus.RetrieveFocus(focusInfo);
   end;
   FWithFocus := nil;
end;

procedure TEditorForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
{}
end;

end.


