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
   WinApi.Windows, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
   Vcl.Dialogs, Vcl.ComCtrls, Vcl.Clipbrd, Vcl.Menus, System.SysUtils, System.Classes,
   SynEdit, SynExportRTF, SynEditPrint, CommonTypes, SynHighlighterPas, SynHighlighterCpp,
   SynMemo, SynExportHTML, OmniXML, Base_Form, CommonInterfaces, SynEditExport, SynEditHighlighter,
  SynHighlighterPython;

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
    procedure memCodeEditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure memCodeEditorGutterClick(Sender: TObject;
      Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
    procedure miRegenerateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memCodeEditorDblClick(Sender: TObject);
    procedure memCodeEditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure memCodeEditorDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure miHelpClick(Sender: TObject);
    procedure memCodeEditorPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure memCodeEditorMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    function SelectCodeRange(const AObject: TObject; ADoSelect: boolean = true): TCodeRange;
    procedure UnSelectCodeRange(const AObject: TObject);
    procedure Localize(const AList: TStringList); override;
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
    procedure GenerateCode(const APreserveBookMarks: boolean = false);
  public
    { Public declarations }
    procedure SetFormAttributes;
    procedure ExecuteCopyToClipboard(const AIfRichText: boolean);
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
    function GetIndentLevel(const idx: integer; ALines: TStrings = nil): integer;
    procedure RefreshEditorForObject(const AObject: TObject);
    function GetAllLines: TStrings;
    procedure SetCaretPos(const ALine: TChangeLine);
{$IFDEF USE_CODEFOLDING}
    procedure RemoveFoldRange(var AFoldRange: TSynEditFoldRange);
    function FindFoldRangesInCodeRange(const ACodeRange: TCodeRange; const ACount: integer): TSynEditFoldRanges;
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
   System.StrUtils, System.Contnrs, System.UITypes, System.Types, WinApi.Messages, ApplicationCommon,
   Goto_Form, Settings, LangDefinition, Main_Block, Help_Form, Comment, XMLProcessor,
   Main_Form, Base_Block, SynEditTypes, ParserHelper;

const
   InfoPanel2: array[boolean] of string = ('OverwriteMode', 'InsertMode');

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
      exit;
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
         if Trim(lines[i]) = '' then
            continue;
         len := Length(TInfra.ExtractIndentString(lines[i]));
         if (min = -1) or (len < min) then
            min := len;
      end;
      if min = -1 then
         min := 0;
      for i := 0 to lines.Count-1 do
         lines[i] := Copy(lines[i], min+1, MAXINT);
      for i := 0 to lines.Count-1 do
      begin
         if i <> 0 then
            result := result + CRLF;
         result := result + lines[i];
      end;
   finally
      lines.Free;
   end;
end;

procedure TEditorForm.SetFormAttributes;
var
   fontSize: integer;
begin
   with GSettings do
   begin
      memCodeEditor.Font.Color := EditorFontColor;
      memCodeEditor.Color := EditorBkgColor;
      memCodeEditor.ActiveLineColor := EditorALineColor;
      memCodeEditor.SelectedColor.Background := EditorSelectColor;
      memCodeEditor.Gutter.Color := EditorGutterColor;
      memCodeEditor.Gutter.Font.Color := Font.Color;
      memCodeEditor.Gutter.Visible := EditorShowGutter;
      memCodeEditor.TabWidth := IndentLength;
      memCodeEditor.Font.Size := EditorFontSize;
      fontSize := memCodeEditor.Font.Size - 2;
      if fontSize < EDITOR_DEFAULT_GUTTER_FONT_SIZE then
         fontSize := EDITOR_DEFAULT_GUTTER_FONT_SIZE;
      memCodeEditor.Gutter.Font.Size := fontSize;
      stbEditorBar.Visible := EditorShowStatusBar;
      miStatusBar.Checked := EditorShowStatusBar;
      miGutter.Checked := EditorShowGutter;
      miScrollbars.Checked := EditorShowScrollbars;
      miRichText.Enabled := GInfra.CurrentLang.Highlighter <> nil;
      miRichText.Checked := EditorShowRichText;
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

procedure TEditorForm.Localize(const AList: TStringList);
begin
   if stbEditorBar.Panels[1].Text <> '' then
      stbEditorBar.Panels[1].Text := AList.Values['Modified'];
   stbEditorBar.Panels[2].Text := AList.Values[InfoPanel2[memCodeEditor.InsertMode]];
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
      exit;
   line := '';
   bline := '';
   strings := TStringList.Create;
   try
      for i := 1 to AText.Length do
      begin
         if not CharInSet(AText[i], [#13, #10]) then
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
         line := ' ' + Trim(strings[i]);
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

procedure TEditorForm.GenerateCode(const APreserveBookMarks: boolean = false);
var
   lang: TLangDefinition;
   skipFuncBody: boolean;
   newLines: TStringList;
begin

   skipFuncBody := false;
   lang := nil;

   newLines := TStringList.Create;
   try

      if Assigned(GInfra.CurrentLang.SkipFuncBodyGen) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.SkipFuncBodyGen) then
         lang := GInfra.DummyLang;
      if lang <> nil then
         skipFuncBody := lang.SkipFuncBodyGen;

      // execute code generation routines for current language
      if Assigned(GInfra.CurrentLang.PreGenerationActivities) then
         GInfra.CurrentLang.PreGenerationActivities;

      lang := nil;
      if Assigned(GInfra.CurrentLang.ProgramHeaderSectionGenerator) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.ProgramHeaderSectionGenerator) then
         lang := GInfra.DummyLang;
      if lang <> nil then
         lang.ProgramHeaderSectionGenerator(newLines);

      lang := nil;
      if Assigned(GInfra.CurrentLang.LibSectionGenerator) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.LibSectionGenerator) then
         lang := GInfra.DummyLang;
      if lang <> nil then
         lang.LibSectionGenerator(newLines);

      if GInfra.CurrentLang.EnabledConsts then
      begin
         lang := nil;
         if Assigned(GInfra.CurrentLang.ConstSectionGenerator) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.ConstSectionGenerator) then
            lang := GInfra.DummyLang;
         if lang <> nil then
            lang.ConstSectionGenerator(newLines, GProject.GlobalConsts);
         newLines.Add('');
      end;

      if GInfra.CurrentLang.EnabledUserDataTypes then
      begin
         lang := nil;
         if Assigned(GInfra.CurrentLang.UserDataTypesSectionGenerator) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.UserDataTypesSectionGenerator) then
            lang := GInfra.DummyLang;
         if lang <> nil then
            lang.UserDataTypesSectionGenerator(newLines);
      end;

      if GInfra.CurrentLang.EnabledVars then
      begin
         lang := nil;
         if Assigned(GInfra.CurrentLang.VarSectionGenerator) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.VarSectionGenerator) then
            lang := GInfra.DummyLang;
         if lang <> nil then
            lang.VarSectionGenerator(newLines, GProject.GlobalVars);
         newLines.Add('');
      end;

      if GInfra.CurrentLang.EnabledUserFunctionHeader then
      begin
         lang := nil;
         if Assigned(GInfra.CurrentLang.UserFunctionsSectionGenerator) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.UserFunctionsSectionGenerator) then
            lang := GInfra.DummyLang;
         if lang <> nil then
            lang.UserFunctionsSectionGenerator(newLines, skipFuncBody);
      end;

      lang := nil;
      if Assigned(GInfra.CurrentLang.MainProgramSectionGenerator) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.MainProgramSectionGenerator) then
         lang := GInfra.DummyLang;
      if lang <> nil then
         lang.MainProgramSectionGenerator(newLines, 0);

      with memCodeEditor do
      begin
{$IFDEF USE_CODEFOLDING}
         AllFoldRanges.ClearAll;
{$ENDIF}
         if not APreserveBookMarks then
            Marks.Clear;
         Highlighter := nil;
         Lines.Assign(newLines);
         if GSettings.EditorShowRichText then
            Highlighter := GInfra.CurrentLang.HighLighter;
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
      miFindProj.Enabled := TInfra.IsValid(obj) and Supports(obj, IFocusable, FFocusControl) and FFocusControl.CanBeFocused;
   end;
end;

procedure TEditorForm.ExecuteCopyToClipboard(const AIfRichText: boolean);
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
               exit;
            memCodeEditor.SelStart := i - 1;
            memCodeEditor.SelLength := Length(ReplaceDialog.FindText);
            memCodeEditor.ClearSelection;
            memCodeEditor.PasteFromClipboard;
            memCodeEditor.SelStart := memCodeEditor.SelStart + Length(ReplaceDialog.ReplaceText);
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
   len := Length(dialog.FindText);
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
    if (command <> '') or ((mainBlock = nil) and (commandNoMain <> '')) then
    begin
       if SaveDialog1.Execute then
       begin
          GetAllLines.SaveToFile(SaveDialog1.FileName, GInfra.CurrentLang.GetFileEncoding);
          fileName := ExtractFileName(SaveDialog1.FileName);
          fileNameNoExt := fileName;
          lPos := Pos('.', fileNameNoExt);
          if lPos <> 0 then
             SetLength(fileNameNoExt, lPos-1);
          if mainBlock = nil then
          begin
             if commandNoMain = '' then
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
   strings: TStrings;
begin
   SetSaveDialog(SaveDialog2);
   if SaveDialog2.Execute then
   begin
      strings := GetAllLines;
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
            synExport.ExportAll(strings);
            synExport.SaveToFile(SaveDialog2.FileName);
            synExport.Highlighter := nil;
         end;
      end
      else
         strings.SaveToFile(SaveDialog2.FileName, GInfra.CurrentLang.GetFileEncoding);
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
      dialog.FindText := Trim(memCodeEditor.SelText);
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
   begin
      if memCodeEditor.Modified then
         stbEditorBar.Panels[1].Text := i18Manager.GetString('Modified')
      else
         stbEditorBar.Panels[1].Text := '';
   end;
   if scInsertMode in Changes then
      stbEditorBar.Panels[2].Text := i18Manager.GetString(InfoPanel2[memCodeEditor.InsertMode]);
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
   highAttr: TSynHighlighterAttributes;
   p: TBufferCoord;
   s, s1: string;
   pos: TPoint;
   fontStyle: TFontStyles;
   brushColor, fontColor: TColor;
begin
   if FDialog <> nil then
   begin
      len := Length(FDialog.FindText);
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
            memCodeEditor.GetHighlighterAttriAtRowCol(p, s1, highAttr);
            s1 := Copy(s, f, len);
            pos := CharToPixels(p);
            Canvas.Font.Style := highAttr.Style;
            Canvas.TextOut(pos.X, pos.Y, s1);
            f := TInfra.FindText(FDialog.FindText, s, f+len, frMatchCase in FDialog.Options);
         end;
      end;
      Canvas.Brush.Color := brushColor;
      Canvas.Font.Style := fontStyle;
   end;
   i := memCodeEditor.SelStart;
   c := #0;
   if (i >= 0) and (i < Length(memCodeEditor.Text)) then
      c := memCodeEditor.Text[i+1];
   if (memCodeEditor.Highlighter = nil) or memCodeEditor.SelAvail or (GSettings.EditorBracketColor = memCodeEditor.Font.Color) or not
      CharInSet(c, Brackets) then exit;
   p := memCodeEditor.CaretXY;
   s := c;
   memCodeEditor.GetHighlighterAttriAtRowCol(p, s, highAttr);
   if memCodeEditor.Highlighter.SymbolAttribute = highAttr then
   begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Font.Assign(memCodeEditor.Font);
      Canvas.Font.Style := highAttr.Style;
      pos := CharToPixels(p);
      if TransientType = ttAfter then
      begin
         fontColor := GSettings.EditorBracketColor;
         brushColor := clNone;
      end
      else
      begin
         fontColor := highAttr.Foreground;
         brushColor := highAttr.Background;
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
   w, h, scope: string;
   highAttr: TSynHighlighterAttributes;
   show, gCheck, lCheck: boolean;
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
   show := false;
   p := memCodeEditor.DisplayToBufferPos(memCodeEditor.PixelsToRowColumn(X, Y));
   p1 := memCodeEditor.GetMatchingBracketEx(p);
   if (p1.Line > 0) and (p1.Line < p.Line) then
   begin
      i := p1.Line - 1;
      if (Length(Trim(memCodeEditor.Lines[i])) < 2) and (i > 0) and (Trim(memCodeEditor.Lines[i-1]) <> '') then
         Dec(i);
      h := BuildBracketHint(i, p.Line-2);
      if h <> '' then
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
         exit;
      end;
   end;
   w := memCodeEditor.GetWordAtRowCol(p);
   if w <> '' then
   begin
      show := true;
      if memCodeEditor.Highlighter <> nil then
      begin
         memCodeEditor.GetHighlighterAttriAtRowCol(p, w, highAttr);
         if memCodeEditor.Highlighter.StringAttribute = highAttr then
            show := false;
      end;
   end;
   if show then
   begin
      block := nil;
      gCheck := true;
      lCheck := true;
      idInfo.New;
      obj := memCodeEditor.Lines.Objects[p.Line-1];
      idInfo.Ident := w;
      if TInfra.IsValid(obj) and (obj is TBlock) then
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
         LOCAL: scope := 'VarLocal';
         PARAMETER: scope := 'VarParm';
      else
         scope := 'VarGlobal';
      end;
      scope := i18Manager.GetString(scope);
      case idInfo.IdentType of
         VARRAY:
         begin
            h := i18Manager.GetFormattedString('HintArray', [scope, idInfo.DimensCount, w, idInfo.SizeAsString, idInfo.TypeAsString]);
            if (idInfo.SizeExpArrayAsString <> '') and (idInfo.SizeExpArrayAsString <> idInfo.SizeAsString) then
               h := h + CRLF + i18Manager.GetFormattedString('HintArrayExp', [idInfo.TypeAsString, CRLF, scope, idInfo.DimensCount, w, idInfo.SizeExpArrayAsString, idInfo.TypeOriginalAsString]);
         end;
         VARIABLE:   h := i18Manager.GetFormattedString('HintVar', [scope, w, idInfo.TypeAsString]);
         CONSTANT:   h := i18Manager.GetFormattedString('HintConst', [w, idInfo.Value]);
         ROUTINE_ID: h := i18Manager.GetFormattedString('HintRoutine', [w, idInfo.TypeAsString]);
         ENUM_VALUE: h := i18Manager.GetFormattedString('HintEnum', [w, idInfo.TypeAsString]);
      end;
      if h <> '' then
      begin
         memCodeEditor.Hint := h;
         memCodeEditor.ShowHint := true;
      end;
   end;
end;

function TEditorForm.GetAllLines: TStrings;
begin
{$IFDEF USE_CODEFOLDING}
   result := memCodeEditor.GetUncollapsedStrings;
{$ELSE}
   result := memCodeEditor.Lines;
{$ENDIF}
end;

function TEditorForm.SelectCodeRange(const AObject: TObject; ADoSelect: boolean = true): TCodeRange;
var
   i: integer;
{$IFDEF USE_CODEFOLDING}
   foldRange: TSynEditFoldRange;
{$ENDIF}
begin
   TInfra.InitCodeRange(result);
   result.Lines := GetAllLines;
   result.FirstRow := result.Lines.IndexOfObject(AObject);
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
{$ENDIF}
      if result.Lines <> nil then
      begin
         result.LastRow := result.FirstRow;
         for i := result.FirstRow+1 to result.Lines.Count-1 do
         begin
            if result.Lines.Objects[i] = AObject then
               result.LastRow := i;
         end;
         with memCodeEditor do
         begin
            if ADoSelect and CanFocus then
            begin
               SelStart := RowColToCharIndex(BufferCoord(Length(result.Lines[result.LastRow])+1, result.LastRow+1));
               SelEnd := RowColToCharIndex(BufferCoord(1, result.FirstRow+1));
            end;
{$IFDEF USE_CODEFOLDING}
            if not result.IsFolded and not ADoSelect then
            begin
               for i := result.FirstRow to result.LastRow do
               begin
                  if result.Lines.Objects[i] = AObject then
                  begin
                     foldRange := CollapsableFoldRangeForLine(i+1);
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
   end
   else
      result.Lines := nil;
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

procedure TEditorForm.UnSelectCodeRange(const AObject: TObject);
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

function TEditorForm.FindFoldRangesInCodeRange(const ACodeRange: TCodeRange; const ACount: integer): TSynEditFoldRanges;
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

procedure TEditorForm.RefreshEditorForObject(const AObject: TObject);
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

function TEditorForm.GetIndentLevel(const idx: integer; ALines: TStrings = nil): integer;
var
   line: string;
   i: integer;
begin
   result := 0;
   if ALines = nil then
      ALines := GetAllLines;
   if (idx >= 0) and (idx < ALines.Count) then
   begin
      line := ALines[idx];
      for i := 1 to Length(line) do
      begin
         if line[i] = INDENT_CHAR then
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
{$IFDEF USE_CODEFOLDING}
   foldRange: TSynEditFoldRange;
   tag1: IXMLElement;
{$ENDIF}
begin
   if Visible then
   begin
      ATag.SetAttribute('src_win_show', '1');
      ATag.SetAttribute('src_win_x', IntToStr(Left));
      ATag.SetAttribute('src_win_y', IntToStr(Top));
      ATag.SetAttribute('src_win_w', IntToStr(Width));
      ATag.SetAttribute('src_win_h', IntToStr(Height));
      ATag.SetAttribute('src_win_sel_start', IntToStr(memCodeEditor.SelStart));
      if memCodeEditor.SelAvail then
         ATag.SetAttribute('src_win_sel_length', IntToStr(memCodeEditor.SelLength));
      if memCodeEditor.Marks.Count > 0 then
      begin
         for i := 0 to memCodeEditor.Marks.Count-1 do
         begin
            tag2 := ATag.OwnerDocument.CreateElement('src_win_mark');
            tag2.SetAttribute('line', IntToStr(memCodeEditor.Marks[i].Line));
            tag2.SetAttribute('index', IntToStr(memCodeEditor.Marks[i].ImageIndex));
            ATag.AppendChild(tag2);
         end;
      end;
      if memCodeEditor.TopLine > 1 then
         ATag.SetAttribute('src_top_line', IntToStr(memCodeEditor.TopLine));
      if WindowState = wsMinimized then
         ATag.SetAttribute('src_win_min', '1');
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         tag1 := nil;
         for i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
         begin
            foldRange := memCodeEditor.AllFoldRanges[i];
            if foldRange.Collapsed then
            begin
               if tag1 = nil then
               begin
                  tag1 := ATag.OwnerDocument.CreateElement('fold_ranges');
                  ATag.AppendChild(tag1);
               end;
               tag2 := ATag.OwnerDocument.CreateElement('fold_range');
               TXMLProcessor.AddText(tag2, IntToStr(memCodeEditor.GetRealLineNumber(foldRange.FromLine)));
               tag1.AppendChild(tag2);
            end;
         end;
      end;
{$ENDIF}
      lines := GetAllLines;
      for i := 0 to lines.Count-1 do
      begin
         tag2 := ATag.OwnerDocument.CreateElement('text_line');
         TXMLProcessor.AddCDATA(tag2, lines[i]);
         if TInfra.IsValid(lines.Objects[i]) and Supports(lines.Objects[i], IIdentifiable, idObject) then
            tag2.SetAttribute(ID_ATTR, IntToStr(idObject.Id));
         ATag.AppendChild(tag2);
      end;
      ATag.SetAttribute('modified', BoolToStr(memCodeEditor.Modified, true));
   end;
end;

procedure TEditorForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
var
   rect: TRect;
   i: integer;
   tag1: IXMLElement;
   mark: TSynEditMark;
{$IFDEF USE_CODEFOLDING}
   foldRange: TSynEditFoldRange;
   foldLines: TStringList;
   tag2: IXMLElement;
{$ENDIF}
begin
   if (ATag.GetAttribute('src_win_show') = '1') and GInfra.CurrentLang.EnabledCodeGenerator then
   begin
      rect.Left := StrToIntDef(ATag.GetAttribute('src_win_x'), 50);
      rect.Top := StrToIntDef(ATag.GetAttribute('src_win_y'), 50);
      rect.Right := StrToIntDef(ATag.GetAttribute('src_win_w'), 425);
      rect.Bottom := StrToIntDef(ATag.GetAttribute('src_win_h'), 558);
      Position := poDesigned;
      SetBounds(rect.Left, rect.Top, rect.Right, rect.Bottom);
      if ATag.GetAttribute('src_win_min') = '1' then
         WindowState := wsMinimized;
      OnShow := nil;
      Show;
      OnShow := FormShow;
      ATag.OwnerDocument.PreserveWhiteSpace := true;
      tag1 := TXMLProcessor.FindChildTag(ATag, 'text_line');
      memCodeEditor.Lines.BeginUpdate;
      while tag1 <> nil do
      begin
         memCodeEditor.Lines.AddObject(tag1.Text, GProject.FindObject(StrToIntDef(tag1.GetAttribute(ID_ATTR), ID_INVALID)));
         tag1 := TXMLProcessor.FindNextTag(tag1);
      end;
      memCodeEditor.Lines.EndUpdate;
      if GSettings.EditorShowRichText then
         memCodeEditor.Highlighter := GInfra.CurrentLang.HighLighter;
      memCodeEditor.ClearUndo;
      memCodeEditor.SetFocus;
      memCodeEditor.Modified := ATag.GetAttribute('modified') = 'True';
      memCodeEditor.SelStart := StrToIntDef(ATag.GetAttribute('src_win_sel_start'), 0);
      memCodeEditor.SelLength := StrToIntDef(ATag.GetAttribute('src_win_sel_length'), 0);
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         memCodeEditor.ReScanForFoldRanges;
         tag1 := TXMLProcessor.FindChildTag(ATag, 'fold_ranges');
         if tag1 <> nil then
         begin
            foldLines := TStringList.Create;
            try
               tag2 := TXMLProcessor.FindChildTag(tag1, 'fold_range');
               while tag2 <> nil do
               begin
                  if StrToIntDef(tag2.Text, 0) > 0 then
                     foldLines.Add(tag2.Text);
                  tag2 := TXMLProcessor.FindNextTag(tag2);
               end;
               foldLines.CustomSort(@CompareIntegers);
               for i := foldLines.Count-1 downto 0 do
               begin
                  foldRange := memCodeEditor.CollapsableFoldRangeForLine(StrToInt(foldLines[i]));
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
      i := StrToIntDef(ATag.GetAttribute('src_top_line'), 0);
      if i > 0 then
         memCodeEditor.TopLine := i;
      tag1 := TXMLProcessor.FindChildTag(ATag, 'src_win_mark');
      while tag1 <> nil do
      begin
         mark := TSynEditMark.Create(memCodeEditor);
         mark.ImageIndex := StrToInt(tag1.GetAttribute('index'));
         memCodeEditor.Marks.Add(mark);
         mark.Line := StrToInt(tag1.GetAttribute('line'));
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
      if GProject.Name <> '' then
         FileName := GProject.Name
      else
         FileName := i18Manager.GetString('Unknown');
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
      if (not miCodeFoldingEnable.Checked) and memCodeEditor.CodeFolding.Enabled then
         memCodeEditor.UnCollapseAll;
      memCodeEditor.CodeFolding.Enabled := miCodeFoldingEnable.Checked;
      memCodeEditor.CodeFolding.HighlighterFoldRegions := false;
      miIndentGuides.Enabled := memCodeEditor.CodeFolding.Enabled;
      miCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      miUnCollapseAll.Enabled := memCodeEditor.CodeFolding.Enabled;
      if not miIndentGuides.Enabled then
         miIndentGuides.Checked := false;
      memCodeEditor.CodeFolding.IndentGuides := miIndentGuides.Checked;
      if memCodeEditor.CodeFolding.Enabled then
         memCodeEditor.Gutter.RightOffset := 21
      else
         memCodeEditor.Gutter.RightOffset := 0;
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
   dispCoord: TDisplayCoord;
   bufCoord: TBufferCoord;
   focusInfo: TFocusInfo;
   codeRange: TCodeRange;
begin
   if (FFocusControl <> nil) and FFocusControl.CanBeFocused then
   begin
      TInfra.InitFocusInfo(focusInfo);
      point := memCodeEditor.ScreenToClient(pmPopMenu.PopupPoint);
      dispCoord := memCodeEditor.PixelsToRowColumn(point.X, point.Y);
      if dispCoord.Row > 0 then
      begin
         focusInfo.Line := dispCoord.Row - 1;
         focusInfo.LineText := TrimLeft(memCodeEditor.Lines[focusInfo.Line]);
         codeRange := SelectCodeRange(memCodeEditor.Lines.Objects[focusInfo.Line], false);
         if codeRange.FirstRow <> ROW_NOT_FOUND then
            focusInfo.RelativeLine := focusInfo.Line - codeRange.FirstRow;
         bufCoord := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart);
         if bufCoord.Line = dispCoord.Row then
         begin
            focusInfo.SelStart := bufCoord.Char - Length(memCodeEditor.Lines[dispCoord.Row-1]) + Length(focusInfo.LineText);
            focusInfo.SelText := MidStr(focusInfo.LineText, focusInfo.SelStart, memCodeEditor.SelLength);
         end;
      end;
      FFocusControl.RetrieveFocus(focusInfo);
   end;
   FFocusControl := nil;
end;

end.

