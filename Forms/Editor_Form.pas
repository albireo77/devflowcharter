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
   Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, System.Types,
   Vcl.Dialogs, Vcl.ComCtrls, Vcl.Clipbrd, Vcl.Menus, System.SysUtils, System.Classes,
   SynEdit, SynExportRTF, SynEditPrint, Types, SynHighlighterPas, SynHighlighterCpp,
   SynMemo, SynExportHTML, OmniXML, Base_Form, Interfaces, SynEditExport, SynEditHighlighter,
   SynHighlighterPython, SynHighlighterJava, Base_Block;

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
    procedure Localize(AList: TStringList); override;
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
    function BuildBracketHint(startLine, endLine: integer): string;
    function CharToPixels(P: TBufferCoord): TPoint;
    function GetAllLines: TStrings;
    procedure PasteComment(const AText: string);
    procedure DisplayLines(ALines: TStringList; AReset: boolean);
  public
    { Public declarations }
    procedure SetFormAttributes;
    procedure ExecuteCopyToClipboard(AIfRichText: boolean);
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
    function GetIndentLevel(idx: integer; ALines: TStrings): integer;
    procedure RefreshEditorForObject(AObject: TObject);
    procedure UpdateEditorForBlock(ABlock: TBlock; const AChangeLine: TChangeLine);
    procedure SetCaretPos(const ALine: TChangeLine);
    procedure SaveToFile(const APath: string);
    procedure InsertLibraryEntry(const ALibrary: string);
{$IFDEF USE_CODEFOLDING}
    procedure RemoveFoldRange(var AFoldRange: TSynEditFoldRange);
    function FindFoldRangesInCodeRange(const ACodeRange: TCodeRange; ACount: integer): TSynEditFoldRanges;
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
   System.StrUtils, System.UITypes, System.Math, WinApi.Windows, Infrastructure,
   Goto_Form, Settings, LangDefinition, Main_Block, Help_Form, Comment, OmniXMLUtils,
   Main_Form, SynEditTypes, ParserHelper, Constants, System.Character;

{$R *.dfm}

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

procedure TEditorForm.FormCreate(Sender: TObject);
begin
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
var
   i, min, len: integer;
   lines: TStringList;
begin
   result := '';
   if (endLine < 0) or (endLine >= memCodeEditor.Lines.Count) or (startLine >= endLine) or (startLine < 0) then
      Exit;
   lines := TStringList.Create;
   try
      if (endLine - startLine) > (memCodeEditor.LinesInWindow div 2) then
      begin
         lines.Add(memCodeEditor.Lines[startLine]);
         lines.Add(memCodeEditor.Lines[startLine+1]);
         lines.Add(TInfra.ExtractIndentString(memCodeEditor.Lines[startLine+1]) + '...');
      end
      else
      begin
         for i := startLine to endLine do
            lines.Add(memCodeEditor.Lines[i]);
      end;
      min := -1;
      for i := 0 to lines.Count-1 do
      begin
         if lines[i].Trim.IsEmpty then
            continue;
         len := TInfra.ExtractIndentString(lines[i]).Length;
         if (min = -1) or (len < min) then
            min := len;
      end;
      if min = -1 then
         min := 0;
      for i := 0 to lines.Count-1 do
         lines[i] := Copy(lines[i], min+1);
      for i := 0 to lines.Count-1 do
      begin
         if i <> 0 then
            result := result + sLineBreak;
         result := result + lines[i];
      end;
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
      if miRichText.Enabled then
         miRichText.Checked := EditorShowRichText
      else
         miRichText.Checked := False;
      if EditorShowScrollbars then
         memCodeEditor.ScrollBars := ssBoth
      else
         memCodeEditor.ScrollBars := ssNone;
      if stbEditorBar.Visible then
         memCodeEditor.Height := ClientHeight - stbEditorBar.Height
      else
         memCodeEditor.Height := ClientHeight;
      if EditorShowRichText then
         memCodeEditor.Highlighter := GInfra.CurrentLang.Highlighter
      else
         memCodeEditor.Highlighter := nil;
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
   FFocusEditor := True;
   FCloseBracketPosP := nil;
   FWithFocus := nil;
   Width := 425;
   Height := 558;
   FDialog := nil;
   inherited ResetForm;
end;

procedure TEditorForm.Localize(AList: TStringList);
begin
   if stbEditorBar.Panels[1].Text <> '' then
      stbEditorBar.Panels[1].Text := AList.Values['Modified'];
   stbEditorBar.Panels[2].Text := AList.Values[IfThen(memCodeEditor.InsertMode, 'InsertMode', 'OverwriteMode')];
   inherited Localize(AList);
end;

procedure TEditorForm.PasteComment(const AText: string);
var
   line, bline, beginComment, endComment: string;
   bc: TBufferCoord;
   strings: TStringList;
   i, count: integer;
   afterLine: boolean;
begin
   if AText.IsEmpty then
      Exit;
   line := '';
   bline := '';
   strings := TStringList.Create;
   try
      for i := 1 to AText.Length do
      begin
         if not AText[i].IsControl then
         begin
            line := line + AText[i];
            if i = AText.Length then
               strings.Add(line);
         end
         else if AText[i] = #10 then
         begin
            strings.Add(line);
            line := '';
         end;
      end;
      Clipboard.Open;
      if Clipboard.HasFormat(CF_TEXT) then
         bline := Clipboard.AsText;
      bc := memCodeEditor.CaretXY;
      beginComment := GInfra.CurrentLang.CommentBegin;
      endComment := GInfra.CurrentLang.CommentEnd;
      count := strings.Count - 1;
      afterLine := True;
      for i := 0 to count do
      begin
         if memCodeEditor.CaretX <= memCodeEditor.Lines[memCodeEditor.CaretY-1+i].Length then
         begin
            afterLine := False;
            break;
         end;
      end;
      for i := 0 to count do
      begin
         line := ' ' + strings[i].Trim;
         memCodeEditor.CaretY := bc.Line + i;
         memCodeEditor.CaretX := bc.Char;
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
               if (count = i) and not endComment.IsEmpty then
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
      if not bline.IsEmpty then
         Clipboard.AsText := bline;
      Clipboard.Close;
      strings.Free;
   end;
end;

procedure TEditorForm.DisplayLines(ALines: TStringList; AReset: boolean);
begin
   if (ALines = nil) or (ALines.Count = 0) then
      Exit;
{$IFDEF USE_CODEFOLDING}
   memCodeEditor.AllFoldRanges.DestroyAll;
{$ENDIF}
   if AReset then
      memCodeEditor.Marks.Clear;
   memCodeEditor.Highlighter := nil;
   if GSettings.IndentChar = TAB_CHAR then
      TInfra.IndentSpacesToTabs(ALines);
   memCodeEditor.Lines.Assign(ALines);
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
            var i := TInfra.FindText(ReplaceDialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart+1, frMatchCase in ReplaceDialog.Options);
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
      i := TInfra.FindText(dialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart + memCodeEditor.SelLength + 1, frMatchCase in dialog.Options)
   else
   begin
      startPos := 1;
      while True do
      begin
         i := TInfra.FindText(dialog.FindText, memCodeEditor.Text, startPos, frMatchCase in dialog.Options);
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
var
   libEntry, indent: string;
   i, a: integer;
   found: boolean;
   lines: TStringList;
   libObj: TObject;
begin
   if not GInfra.CurrentLang.LibEntry.IsEmpty then
      libEntry := Format(GInfra.CurrentLang.LibEntry, [ALibrary])
   else if not GInfra.CurrentLang.LibEntryList.IsEmpty then       // this functionality is disabled for libs in LibEntryList
      Exit
   else
      libEntry := ALibrary;
   found := False;
   for i := 0 to memCodeEditor.Lines.Count-1 do
   begin
      if memCodeEditor.Lines[i].TrimLeft.StartsWith(libEntry, not GInfra.CurrentLang.CaseSensitiveSyntax) then
      begin
         found := True;
         break;
      end;
   end;
   if not found then
   begin
      libObj := TInfra.GetLibObject;
      i := memCodeEditor.Lines.IndexOfObject(libObj);
      if i <> -1 then
      begin
         indent := TInfra.ExtractIndentString(memCodeEditor.Lines[i]);
         memCodeEditor.Lines.InsertObject(i, indent + libEntry, libObj);
      end
      else
      begin
         i := GProject.LibSectionOffset;
         if i >= 0 then
         begin
            if not GInfra.CurrentLang.LibTemplate.IsEmpty then
            begin
               lines := TStringList.Create;
               try
                  lines.Text := GInfra.CurrentLang.LibTemplate;
                  TInfra.InsertTemplateLines(lines, PRIMARY_PLACEHOLDER, libEntry, libObj);
                  for a := lines.Count-1 downto 0 do
                     memCodeEditor.Lines.InsertObject(i, lines.Strings[a], lines.Objects[a]);
               finally
                  lines.Free;
               end;
            end
            else
               memCodeEditor.Lines.InsertObject(i, libEntry, libObj);
         end;
      end;
   end;
end;

procedure TEditorForm.miCompileClick(Sender: TObject);
var
   command, commandNoMain, fileName, fileNameNoExt: string;
   lPos: integer;
   mainBlock: TMainBlock;
begin
    SetSaveDialog(SaveDialog1);
    mainBlock := GProject.GetMainBlock;
    command := GInfra.CurrentLang.CompilerCommand;
    commandNoMain := GInfra.CurrentLang.CompilerCommandNoMain;
    if (not command.IsEmpty) or ((mainBlock = nil) and not commandNoMain.IsEmpty) then
    begin
       if SaveDialog1.Execute then
       begin
          SaveToFile(SaveDialog1.FileName);
          fileName := ExtractFileName(SaveDialog1.FileName);
          fileNameNoExt := fileName;
          lPos := Pos('.', fileNameNoExt);
          if lPos <> 0 then
             SetLength(fileNameNoExt, lPos-1);
          if mainBlock = nil then
          begin
             if commandNoMain.IsEmpty then
                commandNoMain := '%s3';
             command := ReplaceText(commandNoMain, '%s3', command);
          end;
          command := ReplaceText(command, '%s1', fileName);
          command := ReplaceText(command, '%s2', fileNameNoExt);
          if not TInfra.CreateDOSProcess(command, ExtractFileDir(SaveDialog1.FileName)) then
             TInfra.ShowErrorBox(i18Manager.GetString('CompileFail'), errCompile);
       end;
    end
    else
       TInfra.ShowErrorBox('CompilerNotFound', [GInfra.CurrentLang.Name], errCompile)
end;

procedure TEditorForm.miPrintClick(Sender: TObject);
begin
   if not TInfra.IsPrinter then
      TInfra.ShowErrorBox(i18Manager.GetString('NoPrinter'), errPrinter)
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
var
   synExport: TSynCustomExporter;
   lines: TStrings;
   filterKey: string;
begin
   SetSaveDialog(SaveDialog2);
   if Assigned(memCodeEditor.Highlighter) then
      SaveDialog2.Filter := SaveDialog2.Filter + '|' + i18Manager.GetJoinedString('|', EDITOR_DIALOG_FILTER_KEYS);
   if SaveDialog2.Execute then
   begin
      synExport := nil;
      if SaveDialog2.FilterIndex > 1 then
      begin
         filterKey := EDITOR_DIALOG_FILTER_KEYS[SaveDialog2.FilterIndex-2];
         if RTF_FILES_FILTER_KEY = filterKey then
            synExport := SynExporterRTF1
         else if HTML_FILES_FILTER_KEY = filterKey then
            synExport := SynExporterHTML1;
      end;
      if synExport <> nil then
      begin
         synExport.Highlighter := memCodeEditor.Highlighter;
         lines := GetAllLines;
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
      stbEditorBar.Panels[0].Text := i18Manager.GetFormattedString('StatusBarInfo', [p.Line, p.Char]);
   end;
   if scModified in Changes then
   begin
      if memCodeEditor.Modified then
      begin
         stbEditorBar.Panels[1].Text := i18Manager.GetString('Modified');
         GProject.SetChanged;
      end
      else
         stbEditorBar.Panels[1].Text := '';
   end;
   if scInsertMode in Changes then
      stbEditorBar.Panels[2].Text := i18Manager.GetString(IfThen(memCodeEditor.InsertMode, 'InsertMode', 'OverwriteMode'));
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

procedure TEditorForm.memCodeEditorDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if State = dsDragEnter then
      memCodeEditor.SetFocus;
   if not ((Source is TComment) or (Source is TBlock)) then
      Accept := False
   else
      memCodeEditor.CaretXY := memCodeEditor.DisplayToBufferPos(memCodeEditor.PixelsToRowColumn(X, Y));
end;

procedure TEditorForm.memCodeEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   pos: TDisplayCoord;
   tmpList: TStringList;
   i: integer;
begin
   pos := memCodeEditor.PixelsToRowColumn(X, Y);
   if Source is TComment then
   begin
      memCodeEditor.CaretXY := memCodeEditor.DisplayToBufferPos(pos);
      PasteComment(TComment(Source).Text);
   end
   else if Source is TBlock then
   begin
      Clipboard.Open;
      tmpList := TStringList.Create;
      try
         TBlock(Source).GenerateCode(tmpList, GInfra.CurrentLang.Name, 0);
         memCodeEditor.BeginUpdate;
         for i := 0 to tmpList.Count-1 do
         begin
            Clipboard.AsText := tmpList.Strings[i];
            memCodeEditor.CaretY := pos.Row + i;
            memCodeEditor.CaretX := pos.Column;
            memCodeEditor.Lines.Insert(memCodeEditor.CaretY-1, '');
            memCodeEditor.PasteFromClipboard;
         end;
         memCodeEditor.EndUpdate;
      finally
         tmpList.Free;
         Clipboard.Close;
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
         f := TInfra.FindText(FDialog.FindText, s, 1, frMatchCase in FDialog.Options);
         while f > 0 do
         begin
            p := BufferCoord(f, i+1);
            memCodeEditor.GetHighlighterAttriAtRowCol(p, s1, hAttr);
            s1 := Copy(s, f, len);
            pos := CharToPixels(p);
            if hAttr <> nil then
               Canvas.Font.Style := hAttr.Style;
            Canvas.TextOut(pos.X, pos.Y, s1);
            f := TInfra.FindText(FDialog.FindText, s, f+len, frMatchCase in FDialog.Options);
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

function TEditorForm.CharToPixels(P: TBufferCoord): TPoint;
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
   TParserHelper.GetParameterInfo(TInfra.GetFunctionHeader(block), idInfo);
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
      LOCAL: scope := i18Manager.GetString('VarLocal');
      PARAMETER: scope := i18Manager.GetString('VarParm');
   else
      scope := '';
   end;
   case idInfo.IdentType of
      VARRAY:
      begin
         h := i18Manager.GetFormattedString('HintArray', [scope, idInfo.DimensCount, w, idInfo.SizeAsString, idInfo.TypeAsString]);
         if (idInfo.SizeExpArrayAsString <> idInfo.SizeAsString) and not idInfo.SizeExpArrayAsString.IsEmpty then
            h := h + sLineBreak + i18Manager.GetFormattedString('HintArrayExp', [idInfo.TypeAsString, sLineBreak, scope, idInfo.DimensCount, w, idInfo.SizeExpArrayAsString, idInfo.TypeOriginalAsString]);
      end;
      VARIABLE:   h := i18Manager.GetFormattedString('HintVar', [scope, w, idInfo.TypeAsString]);
      CONSTANT:   h := i18Manager.GetFormattedString('HintConst', [w, idInfo.Value]);
      ROUTINE_ID: h := i18Manager.GetFormattedString('HintRoutine', [w, idInfo.TypeAsString]);
      ENUM_VALUE: h := i18Manager.GetFormattedString('HintEnum', [w, idInfo.TypeAsString]);
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
var
   i: integer;
   lines: TStrings;
begin
   result := TCodeRange.New;
   lines := GetAllLines;
   result.FirstRow := lines.IndexOfObject(AObject);
   lines.Free;
   if result.FirstRow <> ROW_NOT_FOUND then
   begin
{$IFDEF USE_CODEFOLDING}
      result.FirstRow := memCodeEditor.Lines.IndexOfObject(AObject);
      if result.FirstRow = ROW_NOT_FOUND then
      begin
         for i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
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
         if AObject is TBlock then
            result.LastRow := TBlock(AObject).FindLastRow(result.FirstRow, result.Lines)
         else
            result.LastRow := TInfra.FindLastRow(AObject, result.FirstRow, result.Lines);
         with memCodeEditor do
         begin
            if ADoSelect and CanFocus then
            begin
               SelStart := RowColToCharIndex(BufferCoord(result.Lines[result.LastRow].Length+1, result.LastRow+1));
               SelEnd := RowColToCharIndex(BufferCoord(1, result.FirstRow+1));
            end;
{$IFDEF USE_CODEFOLDING}
            if not result.IsFolded and not ADoSelect then
            begin
               for i := result.FirstRow to result.LastRow do
               begin
                  if result.Lines.Objects[i] = AObject then
                  begin
                     var foldRange := CollapsableFoldRangeForLine(i+1);
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
end;

procedure TEditorForm.SetCaretPos(const ALine: TChangeLine);
begin
   if ALine.CodeRange.Lines = memCodeEditor.Lines then
   begin
      var c := ALine.Col + ALine.EditCaretXY.Char;
      var line := ALine.Row + ALIne.EditCaretXY.Line + 1;
      if (line > ALine.CodeRange.FirstRow) and (line <= ALine.CodeRange.LastRow+1) and (line <= ALine.CodeRange.Lines.Count) then
      begin
         memCodeEditor.CaretXY := BufferCoord(c, line);
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

function TEditorForm.FindFoldRangesInCodeRange(const ACodeRange: TCodeRange; ACount: integer): TSynEditFoldRanges;
begin
   result := TSynEditFoldRanges.Create;
   if ACodeRange.Lines = memCodeEditor.Lines then
   begin
      for var i := ACodeRange.FirstRow to ACodeRange.FirstRow+ACount do
      begin
         var foldRange := memCodeEditor.CollapsableFoldRangeForLine(i+1);
         if (foldRange <> nil) and (result.Ranges.IndexOf(foldRange) = -1) then
            result.AddF(foldRange);
      end;
   end;
end;
{$ENDIF}

procedure TEditorForm.UpdateEditorForBlock(ABlock: TBlock; const AChangeLine: TChangeLine);
begin
   if ABlock.ShouldUpdateEditor and AChangeLine.Change then
      memCodeEditor.Modified := True;
   SetCaretPos(AChangeLine);
end;

procedure TEditorForm.RefreshEditorForObject(AObject: TObject);
var
   topLine, line: integer;
   codeRange: TCodeRange;
   caretXY: TBufferCoord;
   gotoLine: boolean;
   scrollEnabled: boolean;
begin
   FFocusEditor := False;
   gotoLine := False;
   topLine := memCodeEditor.TopLine;
   caretXY := memCodeEditor.CaretXY;
   scrollEnabled := memCodeEditor.ScrollBars <> TScrollStyle.ssNone;
   if scrollEnabled then
      memCodeEditor.BeginUpdate;
   var programLines := GInfra.GenerateProgram;
   memCodeEditor.LockDrawing;
   try
      DisplayLines(programLines, False);
      if AObject <> nil then
      begin
         codeRange := SelectCodeRange(AObject, False);
         line := codeRange.FirstRow + 1;
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
      if memCodeEditor.Marks.Count > 0 then
      begin
         for var i := 0 to memCodeEditor.Marks.Count-1 do
         begin
            var mark := memCodeEditor.Marks[i];
            var node := AppendNode(ANode, 'src_win_mark');
            SetNodeAttrInt(node, 'line', mark.Line);
            SetNodeAttrInt(node, 'index', mark.ImageIndex);
         end;
      end;
      if memCodeEditor.TopLine > 1 then
         SetNodeAttrInt(ANode, 'src_top_line', memCodeEditor.TopLine);
      if WindowState = wsMinimized then
         SetNodeAttrBool(ANode, 'src_win_min', True);
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         var node: IXMLNode := nil;
         for var i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
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
         for var i := 0 to lines.Count-1 do
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
         memCodeEditor.Lines.AddObject(lineNode.Text, GProject.FindObject(GetNodeAttrInt(lineNode, ID_ATTR, ID_INVALID)));
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
   end;
end;

procedure TEditorForm.SetSaveDialog(ASaveDialog: TSaveDialog);
begin
   with ASaveDialog do
   begin
      DefaultExt := GInfra.CurrentLang.DefaultExt;
      Filter := i18Manager.GetFormattedString('SourceFilesFilter', [GInfra.CurrentLang.Name, DefaultExt, DefaultExt]);
      if GProject.Name.IsEmpty then
         FileName := i18Manager.GetString('Unknown')
      else
         FileName := GProject.Name;
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
   for var i := 0 to High(GInfra.CurrentLang.FoldRegions) do
   begin
      with GInfra.CurrentLang.FoldRegions[i] do
         memCodeEditor.CodeFolding.FoldRegions.Add(RegionType, AddClose, NoSubFolds, WholeWords, PChar(Open), PChar(Close));
   end;
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
   begin
      if miRichText.Checked then
         memCodeEditor.Highlighter := GInfra.CurrentLang.Highlighter
      else
         memCodeEditor.Highlighter := nil;
   end
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
      if stbEditorBar.Visible then
         memCodeEditor.Height := ClientHeight - stbEditorBar.Height
      else
         memCodeEditor.Height := ClientHeight;
   end
   else if Sender = miScrollbars then
   begin
      if miScrollbars.Checked then
         memCodeEditor.ScrollBars := ssBoth
      else
         memCodeEditor.ScrollBars := ssNone;
   end
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


