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
   SynEdit, SynExportRTF, SynEditPrint, CommonTypes, SynHighlighterPas, SynHighlighterCpp,
   SynMemo, SynExportHTML, OmniXML, Base_Form, CommonInterfaces, SynEditExport, SynEditHighlighter,
   SynHighlighterPython, SynHighlighterJava;

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
    function SelectCodeRange(AObject: TObject; ADoSelect: boolean = true): TCodeRange;
    procedure UnSelectCodeRange(AObject: TObject);
    procedure Localize(AList: TStringList); override;
    procedure ResetForm; override;
    procedure SetSaveDialog(Sender: TSaveDialog);
    procedure miGotoClick(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miRichTextClick(Sender: TObject);
    procedure memCodeEditorChange(Sender: TObject);
    procedure miFindProjClick(Sender: TObject);
    procedure OnChangeEditor;
  private
    { Private declarations }
    FCloseBracketPos: TPoint;
    FCloseBracketHint,
    FFocusEditor: boolean;
    FDialog: TFindDialog;
    FFocusControl: IFocusable;
    procedure PasteComment(const AText: string);
    function BuildBracketHint(startLine, endLine: integer): string;
    function CharToPixels(P: TBufferCoord): TPoint;
    procedure GenerateCode(APreserveBookMarks: boolean = false);
    function GetAllLines: TStrings;
  public
    { Public declarations }
    procedure SetFormAttributes;
    procedure ExecuteCopyToClipboard(AIfRichText: boolean);
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
    function GetIndentLevel(idx: integer; ALines: TStrings): integer;
    procedure RefreshEditorForObject(AObject: TObject);
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
  end;

var
   EditorForm: TEditorForm;

implementation

uses
   System.StrUtils, System.UITypes, WinApi.Messages, System.Math, WinApi.Windows,
   ApplicationCommon, Goto_Form, Settings, LangDefinition, Main_Block, Help_Form,
   Comment, XMLProcessor, Main_Form, Base_Block, SynEditTypes, ParserHelper,
   System.Character;

{$R *.dfm}

constructor TEditorHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Font.Assign(EditorForm.memCodeEditor.Font);
end;

procedure TEditorHintWindow.ActivateHintData(ARect: TRect; const AHint: string; AData: Pointer);
begin
   if (AData <> nil) and not TPoint(AData^).IsZero then
      ARect.SetLocation(TPoint(AData^));
   ARect.Offset(0, -ARect.Height);
   inherited ActivateHintData(ARect, AHint, AData);
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
   if (HintInfo.HintControl = memCodeEditor) and FCloseBracketHint then
   begin
      FCloseBracketHint := false;
      HintInfo.HintWindowClass := TEditorHintWindow;
      HintInfo.HintData := @FCloseBracketPos;
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
         miRichText.Checked := false;
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
      miCodeFoldingEnable.Enabled := false;
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
         HighlighterFoldRegions := false;
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
   FFocusEditor := true;
   FCloseBracketHint := false;
   FCloseBracketPos := TPoint.Zero;
   FFocusControl := nil;
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
      afterLine := true;
      for i := 0 to count do
      begin
         if memCodeEditor.CaretX <= memCodeEditor.Lines[memCodeEditor.CaretY-1+i].Length then
         begin
            afterLine := false;
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

procedure TEditorForm.GenerateCode(APreserveBookMarks: boolean = false);
var
   lang: TLangDefinition;
   skipFuncBody: boolean;
   newLines: TStringList;
begin

   newLines := TStringList.Create;
   try

      lang := nil;
      skipFuncBody := false;
      if Assigned(GInfra.CurrentLang.SkipFuncBodyGen) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.TemplateLang.SkipFuncBodyGen) then
         lang := GInfra.TemplateLang;
      if lang <> nil then
         skipFuncBody := lang.SkipFuncBodyGen;


      lang := nil;
      if Assigned(GInfra.CurrentLang.ExecuteBeforeGeneration) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.TemplateLang.ExecuteBeforeGeneration) then
         lang := GInfra.TemplateLang;
      if lang <> nil then
         lang.ExecuteBeforeGeneration;

      try
         lang := nil;
         if Assigned(GInfra.CurrentLang.FileContentsGenerator) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.TemplateLang.FileContentsGenerator) then
            lang := GInfra.TemplateLang;
         if (lang <> nil) and not lang.FileContentsGenerator(newLines, skipFuncBody) then
         begin
            TInfra.ShowFormattedErrorBox('NoProgTempl', [sLineBreak, GInfra.CurrentLang.Name, GInfra.CurrentLang.DefFile, FILE_CONTENTS_TAG], errValidate);
            Exit;
         end;
      finally
         lang := nil;
         if Assigned(GInfra.CurrentLang.ExecuteAfterGeneration) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.TemplateLang.ExecuteAfterGeneration) then
            lang := GInfra.TemplateLang;
         if lang <> nil then
            lang.ExecuteAfterGeneration;
      end;

      with memCodeEditor do
      begin
{$IFDEF USE_CODEFOLDING}
         AllFoldRanges.DestroyAll;
{$ENDIF}
         if not APreserveBookMarks then
            Marks.Clear;
         Highlighter := nil;
         if GSettings.IndentChar = TAB_CHAR then
            TInfra.IndentSpacesToTabs(newLines);
         Lines.Assign(newLines);
         if GSettings.EditorShowRichText then
            Highlighter := GInfra.CurrentLang.HighLighter;
         if Assigned(OnChange) then
            OnChange(memCodeEditor);
         if FFocusEditor then
         begin
            if CanFocus then
               SetFocus;
         end
         else
            FFocusEditor := true;
         ClearUndo;
         Modified := false;
      end;

   finally
      newLines.Free;
   end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
   GenerateCode;
end;

procedure TEditorForm.pmPopMenuPopup(Sender: TObject);
var
   pnt: TPoint;
   dispCoord: TDisplayCoord;
   obj: TObject;
begin
   FFocusControl := nil;
   miFindProj.Enabled := false;
   miCut.Enabled := memCodeEditor.SelAvail;
   miCopy.Enabled := miCut.Enabled;
   miCopyRichText.Enabled := miCopy.Enabled and (memCodeEditor.Highlighter <> nil);
   miRemove.Enabled := miCut.Enabled;
   miPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
   miPasteComment.Enabled := miPaste.Enabled;
   miUndo.Enabled := memCodeEditor.CanUndo;
   miRedo.Enabled := memCodeEditor.CanRedo;
   pnt := memCodeEditor.ScreenToClient(Mouse.CursorPos);
   dispCoord := memCodeEditor.PixelsToRowColumn(pnt.X, pnt.Y);
   if dispCoord.Row > 0 then
   begin
      obj := memCodeEditor.Lines.Objects[dispCoord.Row-1];
      miFindProj.Enabled := TInfra.IsValidControl(obj) and Supports(obj, IFocusable, FFocusControl) and FFocusControl.CanBeFocused;
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
      ExecuteCopyToClipboard(false)
   else if Sender = miCopyRichText then
      ExecuteCopyToClipboard(true)
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
var
   i: integer;
   txt: string;
begin
   txt := '';
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
            i := TInfra.FindText(ReplaceDialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart+1, frMatchCase in ReplaceDialog.Options);
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
   found := false;
   for i := 0 to memCodeEditor.Lines.Count-1 do
   begin
      if memCodeEditor.Lines[i].TrimLeft.StartsWith(libEntry, not GInfra.CurrentLang.CaseSensitiveSyntax) then
      begin
         found := true;
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
         i := GProject.GetLibSectionOffset;
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
       TInfra.ShowFormattedErrorBox('CompilerNotFound', [GInfra.CurrentLang.Name], errCompile)
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
begin
   SetSaveDialog(SaveDialog2);
   if SaveDialog2.Execute then
   begin
      if (SaveDialog2.FilterIndex > 1) and Assigned(memCodeEditor.Highlighter) then
      begin
         case SaveDialog2.FilterIndex of
            2: synExport := SynExporterRTF1;
            3: synExport := SynExporterHTML1;
         else
            synExport := nil;
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
var
   p: TBufferCoord;
begin
   if Changes * [scAll, scCaretX, scCaretY] <> [] then
   begin
      p := memCodeEditor.CaretXY;
      stbEditorBar.Panels[0].Text := i18Manager.GetFormattedString('StatusBarInfo', [p.Line, p.Char]);
   end;
   if scModified in Changes then
      stbEditorBar.Panels[1].Text := IfThen(memCodeEditor.Modified, i18Manager.GetString('Modified'));
   if scInsertMode in Changes then
      stbEditorBar.Panels[2].Text := i18Manager.GetString(IfThen(memCodeEditor.InsertMode, 'InsertMode', 'OverwriteMode'));
end;

procedure TEditorForm.memCodeEditorGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
const
   MARK_FIRST_INDEX = 0;   // index of first bookmark image in MainForm.ImageList1
   MARK_LAST_INDEX = 4;   // index of last bookmark image in MainForm.ImageList1
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
            found := true;
            for a := 0 to memCodeEditor.Marks.Count-1 do
               if i = memCodeEditor.Marks[a].ImageIndex then
               begin
                  found := false;
                  break;
               end;
            if found then break;
         end;
         if not found then
            i := MARK_FIRST_INDEX;
         Mark := TSynEditMark.Create(memCodeEditor);
         Mark.ImageIndex := i;
         memCodeEditor.Marks.Add(Mark);
         Mark.Line := Line;
         Mark.Visible := true;
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
var
   hnd: THandle;
begin
   if memCodeEditor.SelAvail then
   begin
      hnd := FindDialog.Handle;
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
      Accept := false
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
   pos: TPoint;
   i: integer;
begin
   h := '';
   FCloseBracketHint := false;
   FCloseBracketPos := TPoint.Zero;
   memCodeEditor.ShowHint := false;
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
         with memCodeEditor do
         begin
            pos := ClientToScreen(CharToPixels(p));
            Dec(pos.Y, LineHeight-Canvas.TextHeight('I')+1);
            Dec(pos.X, 3);
         end;
         FCloseBracketPos := pos;
         FCloseBracketHint := true;
         memCodeEditor.Hint := h;
         memCodeEditor.ShowHint := true;
         Exit;
      end;
   end;
   w := memCodeEditor.GetWordAtRowCol(p);
   w1 := w;
   if w1.IsEmpty or (memCodeEditor.GetHighlighterAttriAtRowCol(p, w1, hAttr) and (memCodeEditor.Highlighter.StringAttribute = hAttr)) then
      Exit;
   block := nil;
   gCheck := true;
   lCheck := true;
   idInfo := TIdentInfo.New;
   obj := memCodeEditor.Lines.Objects[p.Line-1];
   idInfo.Ident := w;
   if TInfra.IsValidControl(obj) and (obj is TBlock) then
      block := TBlock(obj);
   TParserHelper.GetParameterInfo(TInfra.GetFunctionHeader(block), idInfo);
   if idInfo.TType <> NOT_DEFINED then
   begin
      lCheck := false;
      gCheck := false;
   end;
   if lCheck then
   begin
      TParserHelper.GetVariableInfo(TParserHelper.FindUserFunctionVarList(block), idInfo);
      if idInfo.TType <> NOT_DEFINED then
         gCheck := false;
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
      memCodeEditor.ShowHint := true;
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
{$IFDEF USE_CODEFOLDING}
   with memCodeEditor.GetUncollapsedStrings do
   try
      SaveToFile(APath, GInfra.CurrentLang.GetFileEncoding);
   finally
      Free;
   end;
{$ELSE}
   memCodeEditor.Lines.SaveToFile(APath, GInfra.CurrentLang.GetFileEncoding);
{$ENDIF}
end;

function TEditorForm.SelectCodeRange(AObject: TObject; ADoSelect: boolean = true): TCodeRange;
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
               ADoSelect := false;
               result.IsFolded := true;
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
var
   c, line: integer;
begin
   if ALine.CodeRange.Lines = memCodeEditor.Lines then
   begin
      c := ALine.Col + ALine.EditCaretXY.Char;
      line := ALine.Row + ALIne.EditCaretXY.Line + 1;
      if (line > ALine.CodeRange.FirstRow) and (line <= ALine.CodeRange.LastRow+1) and (line <= ALine.CodeRange.Lines.Count) then
      begin
         memCodeEditor.CaretXY := BufferCoord(c, line);
         memCodeEditor.EnsureCursorPosVisible;
      end;
   end;
end;

procedure TEditorForm.UnSelectCodeRange(AObject: TObject);
var
   codeRange: TCodeRange;
begin
   if memCodeEditor.SelAvail and memCodeEditor.CanFocus then
   begin
      codeRange := SelectCodeRange(AObject, false);
      if (codeRange.FirstRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart).Line-1) and
         (codeRange.LastRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelEnd).Line-1) then
            memCodeEditor.SelStart := memCodeEditor.SelEnd;
   end;
end;

{$IFDEF USE_CODEFOLDING}
procedure TEditorForm.RemoveFoldRange(var AFoldRange: TSynEditFoldRange);
var
   idx: integer;
begin
   idx := memCodeEditor.AllFoldRanges.AllRanges.IndexOf(AFoldRange);
   if idx <> -1 then
      memCodeEditor.AllFoldRanges.AllRanges.Delete(idx);
   AFoldRange.Free;
   AFoldRange := nil;
end;

function TEditorForm.FindFoldRangesInCodeRange(const ACodeRange: TCodeRange; ACount: integer): TSynEditFoldRanges;
var
   i: integer;
   foldRange: TSynEditFoldRange;
begin
   result := TSynEditFoldRanges.Create;
   if ACodeRange.Lines = memCodeEditor.Lines then
   begin
      for i := ACodeRange.FirstRow to ACodeRange.FirstRow+ACount do
      begin
         foldRange := memCodeEditor.CollapsableFoldRangeForLine(i+1);
         if (foldRange <> nil) and (result.Ranges.IndexOf(foldRange) = -1) then
            result.AddF(foldRange);
      end;
   end;
end;
{$ENDIF}

procedure TEditorForm.RefreshEditorForObject(AObject: TObject);
var
   topLine, line: integer;
   codeRange: TCodeRange;
   caretXY: TBufferCoord;
   gotoLine: boolean;
   scrollEnabled: boolean;
begin
   FFocusEditor := false;
   gotoLine := false;
   topLine := memCodeEditor.TopLine;
   caretXY := memCodeEditor.CaretXY;
   scrollEnabled := memCodeEditor.ScrollBars <> TScrollStyle.ssNone;
   if scrollEnabled then
      memCodeEditor.BeginUpdate;
   SendMessage(memCodeEditor.Handle, WM_SETREDRAW, WPARAM(False), 0);
   try
      GenerateCode(true);
      if AObject <> nil then
      begin
         codeRange := SelectCodeRange(AObject, false);
         line := codeRange.FirstRow + 1;
         if (line > 0) and not codeRange.IsFolded then
         begin
            gotoLine := (line < topLine) or (line > topLine + memCodeEditor.LinesInWindow);
            if gotoLine then
               memCodeEditor.GotoLineAndCenter(line);
         end;
      end;
   finally
      if not gotoLine then
      begin
         memCodeEditor.CaretXY := caretXY;
         memCodeEditor.TopLine := topLine;
      end;
      SendMessage(memCodeEditor.Handle, WM_SETREDRAW, WPARAM(True), 0);
      memCodeEditor.Invalidate;
      if scrollEnabled then
         memCodeEditor.EndUpdate;
   end;
end;

function TEditorForm.GetIndentLevel(idx: integer; ALines: TStrings): integer;
var
   line: string;
   i: integer;
begin
   result := 0;
   if (idx >= 0) and (idx < ALines.Count) then
   begin
      line := ALines[idx];
      for i := 1 to line.Length do
      begin
         if line[i] = GSettings.IndentChar then
            result := i
         else
            break;
      end;
      result := result div GSettings.IndentLength;
   end;
end;

procedure TEditorForm.ExportSettingsToXMLTag(ATag: IXMLElement);
var
   i: integer;
   tag2: IXMLElement;
   idObject: IIdentifiable;
   lines: TStrings;
begin
   if Visible then
   begin
      ATag.SetAttribute('src_win_show', 'true');
      ATag.SetAttribute('src_win_x', Left.ToString);
      ATag.SetAttribute('src_win_y', Top.ToString);
      ATag.SetAttribute('src_win_w', Width.ToString);
      ATag.SetAttribute('src_win_h', Height.ToString);
      ATag.SetAttribute('src_win_sel_start', memCodeEditor.SelStart.ToString);
      if memCodeEditor.SelAvail then
         ATag.SetAttribute('src_win_sel_length', memCodeEditor.SelLength.ToString);
      if memCodeEditor.Marks.Count > 0 then
      begin
         for i := 0 to memCodeEditor.Marks.Count-1 do
         begin
            tag2 := ATag.OwnerDocument.CreateElement('src_win_mark');
            tag2.SetAttribute('line', memCodeEditor.Marks[i].Line.ToString);
            tag2.SetAttribute('index', memCodeEditor.Marks[i].ImageIndex.ToString);
            ATag.AppendChild(tag2);
         end;
      end;
      if memCodeEditor.TopLine > 1 then
         ATag.SetAttribute('src_top_line', memCodeEditor.TopLine.ToString);
      if WindowState = wsMinimized then
         ATag.SetAttribute('src_win_min', 'true');
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         var tag1: IXMLElement := nil;
         for i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
         begin
            var foldRange := memCodeEditor.AllFoldRanges[i];
            if foldRange.Collapsed then
            begin
               if tag1 = nil then
               begin
                  tag1 := ATag.OwnerDocument.CreateElement('fold_ranges');
                  ATag.AppendChild(tag1);
               end;
               tag2 := ATag.OwnerDocument.CreateElement('fold_range');
               TXMLProcessor.AddText(tag2, memCodeEditor.GetRealLineNumber(foldRange.FromLine).ToString);
               tag1.AppendChild(tag2);
            end;
         end;
      end;
{$ENDIF}
      lines := GetAllLines;
      try
         for i := 0 to lines.Count-1 do
         begin
            tag2 := ATag.OwnerDocument.CreateElement('text_line');
            TXMLProcessor.AddCDATA(tag2, lines[i]);
            if TInfra.IsValidControl(lines.Objects[i]) and Supports(lines.Objects[i], IIdentifiable, idObject) then
               tag2.SetAttribute(ID_ATTR, idObject.Id.ToString);
            ATag.AppendChild(tag2);
         end;
      finally
         lines.Free;
      end;
      ATag.SetAttribute('modified', memCodeEditor.Modified.ToString);
   end;
end;

procedure TEditorForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   i: integer;
   tag1: IXMLElement;
   mark: TSynEditMark;
   showEvent: TNotifyEvent;
begin
   if TXMLProcessor.GetBool(ATag, 'src_win_show') and GInfra.CurrentLang.EnabledCodeGenerator then
   begin
      Position := poDesigned;
      SetBounds(TXMLProcessor.GetInt(ATag, 'src_win_x', 50),
                TXMLProcessor.GetInt(ATag, 'src_win_y', 50),
                TXMLProcessor.GetInt(ATag, 'src_win_w', 425),
                TXMLProcessor.GetInt(ATag, 'src_win_h', 558));
      if TXMLProcessor.GetBool(ATag, 'src_win_min') then
         WindowState := wsMinimized;
      showEvent := OnShow;
      OnShow := nil;
      try
         Show;
      finally
         OnShow := showEvent;
      end;
      ATag.OwnerDocument.PreserveWhiteSpace := true;
      tag1 := TXMLProcessor.FindChildTag(ATag, 'text_line');
      memCodeEditor.Lines.BeginUpdate;
      while tag1 <> nil do
      begin
         memCodeEditor.Lines.AddObject(tag1.Text, GProject.FindObject(TXMLProcessor.GetInt(tag1, ID_ATTR, ID_INVALID)));
         tag1 := TXMLProcessor.FindNextTag(tag1);
      end;
      memCodeEditor.Lines.EndUpdate;
      if GSettings.EditorShowRichText then
         memCodeEditor.Highlighter := GInfra.CurrentLang.HighLighter;
      memCodeEditor.ClearUndo;
      memCodeEditor.SetFocus;
      memCodeEditor.Modified := TXMLProcessor.GetBool(ATag, 'modified');
      memCodeEditor.SelStart := TXMLProcessor.GetInt(ATag, 'src_win_sel_start');
      memCodeEditor.SelLength := TXMLProcessor.GetInt(ATag, 'src_win_sel_length');
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         memCodeEditor.ReScanForFoldRanges;
         tag1 := TXMLProcessor.FindChildTag(ATag, 'fold_ranges');
         if tag1 <> nil then
         begin
            var foldLines := TStringList.Create;
            try
               var tag2 := TXMLProcessor.FindChildTag(tag1, 'fold_range');
               while tag2 <> nil do
               begin
                  if StrToIntDef(tag2.Text, 0) > 0 then
                     foldLines.Add(tag2.Text);
                  tag2 := TXMLProcessor.FindNextTag(tag2);
               end;
               foldLines.CustomSort(@CompareIntegers);
               for i := foldLines.Count-1 downto 0 do
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
      ATag.OwnerDocument.PreserveWhiteSpace := false;
      i := TXMLProcessor.GetInt(ATag, 'src_top_line');
      if i > 0 then
         memCodeEditor.TopLine := i;
      tag1 := TXMLProcessor.FindChildTag(ATag, 'src_win_mark');
      while tag1 <> nil do
      begin
         mark := TSynEditMark.Create(memCodeEditor);
         mark.ImageIndex := TXMLProcessor.GetInt(tag1, 'index');
         memCodeEditor.Marks.Add(mark);
         mark.Line := TXMLProcessor.GetInt(tag1, 'line');
         mark.Visible := true;
         tag1 := TXMLProcessor.FindNextTag(tag1);
      end;
   end;
end;

procedure TEditorForm.SetSaveDialog(Sender: TSaveDialog);
begin
   with Sender do
   begin
      DefaultExt := GInfra.CurrentLang.DefaultExt;
      Filter := i18Manager.GetFormattedString('SourceFilesFilter', [GInfra.CurrentLang.Name, DefaultExt, DefaultExt]);
      if (Sender = SaveDialog2) and Assigned(memCodeEditor.Highlighter) then
      begin
         Filter := Filter + '|' +
                   i18Manager.GetString('RTFFilesFilter') + '|' +
                   i18Manager.GetString('HTMLFilesFilter');
      end;
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
var
   i: integer;
begin
   memCodeEditor.CodeFolding.FoldRegions.Clear;
   for i := 0 to High(GInfra.CurrentLang.FoldRegions) do
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
      memCodeEditor.CodeFolding.HighlighterFoldRegions := false;
      miIndentGuides.Enabled := memCodeEditor.CodeFolding.Enabled;
      miCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      miUnCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      if not miIndentGuides.Enabled then
         miIndentGuides.Checked := false;
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
var
   point: TPoint;
   displ: TDisplayCoord;
   selStart, selEnd: TBufferCoord;
   focusInfo: TFocusInfo;
   codeRange: TCodeRange;
   i: integer;
   selText, sline: string;
begin
   if (FFocusControl <> nil) and FFocusControl.CanBeFocused then
   begin
      focusInfo := TFocusInfo.New;
      point := memCodeEditor.ScreenToClient(pmPopMenu.PopupPoint);
      displ := memCodeEditor.PixelsToRowColumn(point.X, point.Y);
      if displ.Row > 0 then
      begin
         if memCodeEditor.SelAvail then
         begin
            selStart := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart);
            selStart.Line := selStart.Line - 1;
            selEnd := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart + memCodeEditor.SelLength);
            selEnd.Line := selEnd.Line - 1;
            sLine := memCodeEditor.Lines[selStart.Line];
            focusInfo.SelStart := selStart.Char - sLine.Length + sLine.TrimLeft.Length;
            if selStart.Line <> selEnd.Line then
            begin
               selText := '';
               for i := selStart.Line to selEnd.Line do
               begin
                  sline := memCodeEditor.Lines[i];
                  if i = selStart.Line then
                     selText := RightStr(sline, sline.Length - selStart.Char + 1) + sLineBreak
                  else if i = selEnd.Line then
                  begin
                     sline := LeftStr(sline, selEnd.Char-1);
                     selText := selText + sline.TrimLeft;
                  end
                  else
                     selText := selText + sline.TrimLeft + sLineBreak;
               end;
               focusInfo.SelText := selText;
            end
            else
               focusInfo.SelText := MidStr(sLine.TrimLeft, focusInfo.SelStart, memCodeEditor.SelLength);
            focusInfo.Line := selStart.Line;
         end
         else
            focusInfo.Line := displ.Row - 1;
         focusInfo.LineText := memCodeEditor.Lines[focusInfo.Line].TrimLeft;
         codeRange := SelectCodeRange(memCodeEditor.Lines.Objects[focusInfo.Line], false);
         if codeRange.FirstRow <> ROW_NOT_FOUND then
            focusInfo.RelativeLine := focusInfo.Line - codeRange.FirstRow;
      end;
      FFocusControl.RetrieveFocus(focusInfo);
   end;
   FFocusControl := nil;
end;

end.


