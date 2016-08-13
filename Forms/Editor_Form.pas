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



unit Editor_Form;

interface

uses
{$IFDEF USE_CODEFOLDING}
   SynEditCodeFolding,
{$ENDIF}
   Windows, Controls, Forms, StdCtrls, ExtCtrls, Graphics, Dialogs, ComCtrls,
   Menus, Clipbrd, SysUtils, SynExportRTF, SynEditPrint, CommonTypes, SynHighlighterPas,
   SynHighlighterCpp, Classes, SynEdit, SynMemo, SynExportHTML, OmniXML, Base_Form,
   CommonInterfaces;

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
    procedure FormShow(Sender: TObject);
    procedure pmPopMenuPopup(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
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
    FFocusControl: IFocusable;
    procedure PasteComment(const AText: string);
    function BuildBracketHint(startLine, endLine: integer): string;
  public
    { Public declarations }
    procedure SetFormAttributes;
    procedure ExecuteCopyToClipboard(const AIfRichText: boolean);
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
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
   ApplicationCommon, Goto_Form, Settings, LangDefinition, Main_Block, Help_Form, Comment,
   XMLProcessor, StrUtils, Main_Form, Base_Block, SynEditTypes, SynEditExport, SynEditHighlighter,
   ParserHelper;

const
   InfoPanel2: array[boolean] of string = ('OverwriteMode', 'InsertMode');

{$R *.dfm}

constructor TEditorHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Font.Assign(EditorForm.memCodeEditor.Font);
end;

procedure TEditorHintWindow.ActivateHintData(ARect: TRect; const AHint: string; AData: Pointer);
var
   x, y, h: integer;
   lPoint: TPoint;
begin
   if (AData <> nil) and not InvalidPoint(TPoint(AData^)) then
   begin
      lPoint := TPoint(AData^);
      x := lPoint.X - ARect.Left;
      y := lPoint.Y - ARect.Top;
      ARect.Left := lPoint.X;
      Inc(ARect.Right, x);
      ARect.Top := lPoint.Y;
      Inc(ARect.Bottom, y);
   end;
   h := ARect.Bottom - ARect.Top;
   Dec(ARect.Top, h);
   Dec(ARect.Bottom, h);
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
   i, min, l: integer;
   lLines: TStringList;
begin
   result := '';
   if (endLine < 0) or (endLine >= memCodeEditor.Lines.Count) or (startLine >= endLine) or (startLine < 0) then
      exit;
   min := 0;
   lLines := TStringList.Create;
   try
      if (endLine - startLine) > (memCodeEditor.LinesInWindow div 2) then
      begin
         lLines.Add(memCodeEditor.Lines[startLine]);
         lLines.Add(memCodeEditor.Lines[startLine+1]);
         lLines.Add(TInfra.ExtractIndentString(memCodeEditor.Lines[startLine+1]) + '...');
      end
      else
      begin
         for i := startLine to endLine do
            lLines.Add(memCodeEditor.Lines[i]);
      end;
      for i := 0 to lLines.Count-1 do
      begin
         if Trim(lLines[i]) = '' then
            continue;
         l := Length(TInfra.ExtractIndentString(lLines[i]));
         if i = 0 then
            min := l
         else if l < min then
            min := l;
      end;
      for i := 0 to lLines.Count-1 do
         lLines[i] := AnsiRightStr(lLines[i], Length(lLines[i])-min);
      for i := 0 to lLines.Count-1 do
      begin
         if i <> 0 then
            result := result + CRLF;
         result := result + lLines[i];
      end;
   finally
      lLines.Free;
   end;
end;

procedure TEditorForm.SetFormAttributes;
var
   lFontSize: integer;
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
      lFontSize := memCodeEditor.Font.Size - 2;
      if lFontSize < EDITOR_DEFAULT_GUTTER_FONT_SIZE then
         lFontSize := EDITOR_DEFAULT_GUTTER_FONT_SIZE;
      memCodeEditor.Gutter.Font.Size := lFontSize;
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
   FCloseBracketPos := Point(-1, -1);
   FFocusControl := nil;
   Width := 425;
   Height := 558;
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
   lLine: string;
   lPos: TBufferCoord;
   lStrings: TStringList;
   i, len: integer;
begin
   len := Length(AText);
   lStrings := TStringList.Create;
   try
      for i := 1 to len do
      begin
         if not (AText[i] in [#13, #10]) then
         begin
            lLine := lLine + AText[i];
            if i = len then
               lStrings.Add(lLine);
         end
         else if AText[i] = #10 then
         begin
            lStrings.Add(lLine);
            lLine := '';
         end;
      end;
      lLine := '';
      try        // this try..except block handles strange "Cannot open clipboard" error
         Clipboard.Open;
         if Clipboard.HasFormat(CF_TEXT) then
            lLine := Clipboard.AsText;
         lPos := memCodeEditor.CaretXY;
         for i := 0 to lStrings.Count-1 do
         begin
            Clipboard.AsText := GInfra.CurrentLang.CommentBegin + ' ' +
                                Trim(lStrings[i]) + ' ' +
                                GInfra.CurrentLang.CommentEnd;
            with memCodeEditor do
            begin
               CaretY := lPos.Line + i;
               CaretX := lPos.Char;
               if CaretX <= Length(Lines[CaretY-1]) then
                  Lines.Insert(CaretY-1, '');
               PasteFromClipboard;
            end;
         end;
      except
      end;
   finally
      lStrings.Free;
      if lLine <> '' then
         Clipboard.AsText := lLine;
      Clipboard.Close;
   end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
var
   lLang: TLangDefinition;
   lSkipFuncBody: boolean;
   lLines: TStringList;
begin

{$IFDEF USE_CODEFOLDING}
   memCodeEditor.AllFoldRanges.ClearAll;
{$ENDIF}
   memCodeEditor.ClearAll;
   memCodeEditor.Highlighter := nil;


   lSkipFuncBody := false;
   lLang := nil;

   lLines := TStringList.Create;
   try

      if Assigned(GInfra.CurrentLang.SkipFuncBodyGen) then
         lLang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.SkipFuncBodyGen) then
         lLang := GInfra.DummyLang;
      if lLang <> nil then
         lSkipFuncBody := lLang.SkipFuncBodyGen;

      // execute code generation routines for current language
      if Assigned(GInfra.CurrentLang.PreGenerationActivities) then
         GInfra.CurrentLang.PreGenerationActivities;

      lLang := nil;
      if Assigned(GInfra.CurrentLang.ProgramHeaderSectionGenerator) then
         lLang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.ProgramHeaderSectionGenerator) then
         lLang := GInfra.DummyLang;
      if lLang <> nil then
         lLang.ProgramHeaderSectionGenerator(lLines);

      lLang := nil;
      if Assigned(GInfra.CurrentLang.LibSectionGenerator) then
         lLang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.LibSectionGenerator) then
         lLang := GInfra.DummyLang;
      if lLang <> nil then
         lLang.LibSectionGenerator(lLines);

      if GInfra.CurrentLang.EnabledConsts then
      begin
         lLang := nil;
         if Assigned(GInfra.CurrentLang.ConstSectionGenerator) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.ConstSectionGenerator) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            lLang.ConstSectionGenerator(lLines, GProject.GlobalConsts);
         lLines.Add('');
      end;

      if GInfra.CurrentLang.EnabledUserDataTypes then
      begin
         lLang := nil;
         if Assigned(GInfra.CurrentLang.UserDataTypesSectionGenerator) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.UserDataTypesSectionGenerator) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            lLang.UserDataTypesSectionGenerator(lLines);
      end;

      if GInfra.CurrentLang.EnabledVars then
      begin
         lLang := nil;
         if Assigned(GInfra.CurrentLang.VarSectionGenerator) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.VarSectionGenerator) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            lLang.VarSectionGenerator(lLines, GProject.GlobalVars);
         lLines.Add('');
      end;

      if GInfra.CurrentLang.EnabledUserFunctionHeader then
      begin
         lLang := nil;
         if Assigned(GInfra.CurrentLang.UserFunctionsSectionGenerator) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.UserFunctionsSectionGenerator) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            lLang.UserFunctionsSectionGenerator(lLines, lSkipFuncBody);
      end;

      lLang := nil;
      if Assigned(GInfra.CurrentLang.MainProgramSectionGenerator) then
         lLang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.MainProgramSectionGenerator) then
         lLang := GInfra.DummyLang;
      if lLang <> nil then
         lLang.MainProgramSectionGenerator(lLines, 0);

      memCodeEditor.Lines.Assign(lLines);
      if GSettings.EditorShowRichText then
         memCodeEditor.Highlighter := GInfra.CurrentLang.HighLighter;
      memCodeEditor.OnChange(memCodeEditor);
      if FFocusEditor then
      begin
         if memCodeEditor.CanFocus then
            memCodeEditor.SetFocus;
      end
      else
         FFocusEditor := true;
      memCodeEditor.ClearUndo;
      memCodeEditor.Modified := false;

   finally
      lLines.Free;
   end;

end;

procedure TEditorForm.pmPopMenuPopup(Sender: TObject);
var
   lPoint: TPoint;
   lCoord: TDisplayCoord;
   lObject: TObject;
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
   lPoint := memCodeEditor.ScreenToClient(Mouse.CursorPos);
   lCoord := memCodeEditor.PixelsToRowColumn(lPoint.X, lPoint.Y);
   if lCoord.Row > 0 then
   begin
      lObject := memCodeEditor.Lines.Objects[lCoord.Row-1];
      miFindProj.Enabled := TInfra.IsValid(lObject) and Supports(lObject, IFocusable, FFocusControl) and FFocusControl.CanBeFocused;
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
   idx: Integer;
   lText: string;
begin
   lText := '';
   Clipboard.Open;
   try
      if Clipboard.HasFormat(CF_TEXT) then
         lText := Clipboard.AsText;
      Clipboard.AsText := ReplaceDialog.ReplaceText;
      if frReplaceAll in ReplaceDialog.Options then
      begin
         memCodeEditor.SelStart := 0;
         while True do
         begin
            idx := TInfra.FindText(ReplaceDialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart, frMatchCase in ReplaceDialog.Options);
            if idx = -1 then exit;
            memCodeEditor.SelStart := idx;
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
      if lText <> '' then
         Clipboard.AsText := lText;
      Clipboard.Close;
   end;
end;

procedure TEditorForm.ReplaceDialogFind(Sender: TObject);
var
   idx, lStartPos, lLength: Integer;
   lDialog: TFindDialog;
begin
   lDialog := TFindDialog(Sender);
   lLength := Length(lDialog.FindText);
   idx := -1;
   if frDown in lDialog.Options then
      idx := TInfra.FindText(lDialog.FindText, memCodeEditor.Text, memCodeEditor.SelStart + memCodeEditor.SelLength, frMatchCase in lDialog.Options)
   else
   begin
      lStartPos := 0;
      while True do
      begin
         idx := TInfra.FindText(lDialog.FindText, memCodeEditor.Text, lStartPos, frMatchCase in lDialog.Options);
         if (idx <> -1) and (idx < memCodeEditor.SelStart) then
            lStartPos := idx + lLength
         else
         begin
            if lStartPos > 0 then
               idx := lStartPos - lLength;
            if idx >= memCodeEditor.SelStart then
               idx := -1;
            break;
         end;
      end;
   end;
   if idx <> -1 then
   begin
      memCodeEditor.SetFocus;
      memCodeEditor.SelStart := idx;
      memCodeEditor.SelLength := lLength;
   end;
end;

procedure TEditorForm.miCompileClick(Sender: TObject);
var
   lCommand, lCommandNoMain, lFileName, lFileNameNoExt: string;
   lPos: integer;
   lMainBlock: TMainBlock;
begin
    SetSaveDialog(SaveDialog1);
    lMainBlock := GProject.GetMainBlock;
    lCommand := GInfra.CurrentLang.CompilerCommand;
    lCommandNoMain := GInfra.CurrentLang.CompilerCommandNoMain;
    if (lCommand <> '') or ((lMainBlock = nil) and (lCommandNoMain <> '')) then
    begin
       if SaveDialog1.Execute then
       begin
          GetAllLines.SaveToFile(SaveDialog1.FileName);
          lFileName := ExtractFileName(SaveDialog1.FileName);
          lFileNameNoExt := lFileName;
          lPos := AnsiPos('.', lFileNameNoExt);
          if lPos <> 0 then
             lFileNameNoExt := AnsiLeftStr(lFileNameNoExt, lPos-1);

          if lMainBlock = nil then
          begin
             if lCommandNoMain = '' then
                lCommandNoMain := '%s3';
             lCommand := AnsiReplaceText(lCommandNoMain, '%s3', lCommand);
          end;

          lCommand := AnsiReplaceText(lCommand, '%s1', lFileName);
          lCommand := AnsiReplaceText(lCommand, '%s2', lFileNameNoExt);

          if not TInfra.IsWin9x then
             lCommand := 'cmd.exe /k ' + lCommand;

          if not TInfra.CreateDOSProcess(lCommand, ExtractFileDir(SaveDialog1.FileName)) then
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
   SynExporter: TSynCustomExporter;
   lStrings: TStrings;
begin
   SetSaveDialog(SaveDialog2);
   if SaveDialog2.Execute then
   begin
      lStrings := GetAllLines;
      if (SaveDialog2.FilterIndex > 1) and Assigned(memCodeEditor.Highlighter) then
      begin
         case SaveDialog2.FilterIndex of
            2: SynExporter := SynExporterRTF1;
            3: SynExporter := SynExporterHTML1;
         else
            SynExporter := nil;
         end;
         if SynExporter <> nil then
         begin
            SynExporter.Highlighter := memCodeEditor.Highlighter;
            SynExporter.ExportAll(lStrings);
            SynExporter.SaveToFile(SaveDialog2.FileName);
            SynExporter.Highlighter := nil;
         end;
      end
      else
         lStrings.SaveToFile(SaveDialog2.FileName);
   end;
end;

procedure TEditorForm.miFindClick(Sender: TObject);
var
   lDialog: TFindDialog;
begin
   if Sender = miFind then
      lDialog := FindDialog
   else
   begin
      lDialog := ReplaceDialog;
      ReplaceDialog.ReplaceText := '';
   end;
   if memCodeEditor.SelAvail then
      lDialog.FindText := Trim(memCodeEditor.SelText);
   lDialog.Execute;
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
   lFound: boolean;
begin
   if not Assigned(Mark) then
   begin
      if memCodeEditor.Marks.Count < MAX_MARKS then
      begin
         for i := MARK_FIRST_INDEX to MARK_LAST_INDEX do
         begin
            lFound := true;
            for a := 0 to memCodeEditor.Marks.Count-1 do
               if i = memCodeEditor.Marks[a].ImageIndex then
               begin
                  lFound := false;
                  break;
               end;
            if lFound then break;
         end;
         if not lFound then
            i := MARK_FIRST_INDEX;
{$IFDEF USE_CODEFOLDING}
         Mark := TSynEditMark.Create(memCodeEditor);
{$ELSE}
         Mark := TSynEditMark.Create;
{$ENDIF}
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
  hndle: THandle;
begin
   if memCodeEditor.SelAvail then
   begin
      hndle := FindDialog.Handle;
      if hndle = 0 then
         hndle := ReplaceDialog.Handle;
      hndle := FindWindowEx(hndle, 0, 'Edit', nil);
      if hndle <> 0 then
         SetWindowText(hndle, PChar(memCodeEditor.SelText));
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

procedure TEditorForm.memCodeEditorDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
   lPos: TDisplayCoord;
   lTmpList: TStringList;
   i: integer;
begin
   lPos := memCodeEditor.PixelsToRowColumn(X, Y);
   if Source is TComment then
   begin
      memCodeEditor.CaretXY := memCodeEditor.DisplayToBufferPos(lPos);
      PasteComment(TComment(Source).Text);
   end
   else if Source is TBlock then
   begin
      Clipboard.Open;
      lTmpList := TStringList.Create;
      try
         TBlock(Source).GenerateCode(lTmpList, GInfra.CurrentLang.Name, 0);
         memCodeEditor.BeginUpdate;
         for i := 0 to lTmpList.Count-1 do
         begin
            Clipboard.AsText := lTmpList.Strings[i];
            memCodeEditor.CaretY := lPos.Row + i;
            memCodeEditor.CaretX := lPos.Column;
            memCodeEditor.Lines.Insert(memCodeEditor.CaretY-1, '');
            memCodeEditor.PasteFromClipboard;
         end;
         memCodeEditor.EndUpdate;
      finally
         lTmpList.Free;
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
   idx: integer;
   lChar: char;
   lAttr: TSynHighlighterAttributes;
   P: TBufferCoord;
   S: string;
   lPos: TPoint;
begin
   idx := memCodeEditor.SelStart;
   lChar := #0;
   if (idx >= 0) and (idx < Length(memCodeEditor.Text)) then
      lChar := memCodeEditor.Text[idx+1];
   if (memCodeEditor.Highlighter = nil) or memCodeEditor.SelAvail or (GSettings.EditorBracketColor = memCodeEditor.Font.Color) or not
      (lChar in Brackets) then exit;
   P := memCodeEditor.CaretXY;
   S := lChar;
   memCodeEditor.GetHighlighterAttriAtRowCol(P, S, lAttr);
   if memCodeEditor.Highlighter.SymbolAttribute = lAttr then
   begin
      memCodeEditor.Canvas.Brush.Style := bsSolid;
      memCodeEditor.Canvas.Font.Assign(memCodeEditor.Font);
      memCodeEditor.Canvas.Font.Style := lAttr.Style;
      lPos := memCodeEditor.RowColumnToPixels(memCodeEditor.BufferToDisplayPos(P));
      if TransientType = ttAfter then
      begin
         memCodeEditor.Canvas.Font.Color := GSettings.EditorBracketColor;
         memCodeEditor.Canvas.Brush.Color := clNone;
      end
      else
      begin
         memCodeEditor.Canvas.Font.Color := lAttr.Foreground;
         memCodeEditor.Canvas.Brush.Color := lAttr.Background;
      end;
      if memCodeEditor.Canvas.Font.Color = clNone then
         memCodeEditor.Canvas.Font.Color := memCodeEditor.Font.Color;
      if memCodeEditor.Canvas.Brush.Color = clNone then
         memCodeEditor.Canvas.Brush.Color := memCodeEditor.ActiveLineColor;
      memCodeEditor.Canvas.TextOut(lPos.X, lPos.Y, S);
      P := memCodeEditor.GetMatchingBracketEx(P);
      if (P.Char > 0) and (P.Line > 0) then
      begin
         idx := memCodeEditor.RowColToCharIndex(P);
         S := memCodeEditor.Text[idx+1];
         lPos := memCodeEditor.RowColumnToPixels(memCodeEditor.BufferToDisplayPos(P));
         if P.Line <> memCodeEditor.CaretY then
            memCodeEditor.Canvas.Brush.Color := memCodeEditor.Color;
         memCodeEditor.Canvas.TextOut(lPos.X, lPos.Y, S);
      end;
      memCodeEditor.Canvas.Brush.Style := bsSolid;
   end;
end;

procedure TEditorForm.memCodeEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   P, P1: TBufferCoord;
   lWord, lScope, lHint: string;
   lAttr: TSynHighlighterAttributes;
   lShow, lGlobalCheck, lLocalCheck: boolean;
   lIdent: TIdentInfo;
   lObject: TObject;
   lBlock: TBlock;
   lPos: TPoint;
   i: integer;
begin
   lHint := '';
   FCloseBracketHint := false;
   FCloseBracketPos := Point(-1, -1);
   memCodeEditor.ShowHint := false;
   memCodeEditor.Hint := '';
   lShow := false;
   P := memCodeEditor.DisplayToBufferPos(memCodeEditor.PixelsToRowColumn(X, Y));
   P1 := memCodeEditor.GetMatchingBracketEx(P);
   if (P1.Line > 0) and (P1.Line < P.Line) then
   begin
      i := P1.Line - 1;
      if (Length(Trim(memCodeEditor.Lines[i])) < 2) and (i > 0) and (Trim(memCodeEditor.Lines[i-1]) <> '') then
         Dec(i);
      lHint := BuildBracketHint(i, P.Line-2);
      if lHint <> '' then
      begin
         with memCodeEditor do
         begin
            lPos := ClientToScreen(RowColumnToPixels(BufferToDisplayPos(P)));
            Dec(lPos.Y, LineHeight-Canvas.TextHeight('I')+1);
            Dec(lPos.X, 3);
         end;
         FCloseBracketPos := lPos;
         FCloseBracketHint := true;
         memCodeEditor.Hint := lHint;
         memCodeEditor.ShowHint := true;
         exit;
      end;
   end;
   lWord := memCodeEditor.GetWordAtRowCol(P);
   if lWord <> '' then
   begin
      lShow := true;
      if memCodeEditor.Highlighter <> nil then
      begin
         memCodeEditor.GetHighlighterAttriAtRowCol(P, lWord, lAttr);
         if memCodeEditor.Highlighter.StringAttribute = lAttr then
            lShow := false;
      end;
   end;
   if lShow then
   begin
      lBlock := nil;
      lGlobalCheck := true;
      lLocalCheck := true;
      TParserHelper.InitIdentInfo(lIdent);
      lObject := memCodeEditor.Lines.Objects[P.Line-1];
      lIdent.Ident := lWord;
      if TInfra.IsValid(lObject) and (lObject is TBlock) then
         lBlock := TBlock(lObject);
      TParserHelper.GetParameterInfo(TInfra.GetFunctionHeader(lBlock), lIdent);
      if lIdent.TType <> NOT_DEFINED then
      begin
         lLocalCheck := false;
         lGlobalCheck := false;
      end;
      if lLocalCheck then
      begin
         TParserHelper.GetVariableInfo(TParserHelper.FindUserFunctionVarList(lBlock), lIdent);
         if lIdent.TType <> NOT_DEFINED then
            lGlobalCheck := false;
      end;
      if lGlobalCheck then
         lIdent := TParserHelper.GetIdentInfo(lWord);
      case lIdent.Scope of
         LOCAL: lScope := 'VarLocal';
         PARAMETER: lScope := 'VarParm';
      else
         lScope := 'VarGlobal';
      end;
      lScope := i18Manager.GetString(lScope);
      case lIdent.IdentType of
         VARRAY:
         begin
            lHint := i18Manager.GetFormattedString('HintArray', [lScope, lIdent.DimensCount, lWord, lIdent.SizeAsString, lIdent.TypeAsString]);
            if (lIdent.SizeExpArrayAsString <> '') and (lIdent.SizeExpArrayAsString <> lIdent.SizeAsString) then
               lHint := lHint + CRLF + i18Manager.GetFormattedString('HintArrayExp', [lIdent.TypeAsString, CRLF, lScope, lIdent.DimensCount, lWord, lIdent.SizeExpArrayAsString, lIdent.TypeOriginalAsString]);
         end;
         VARIABLE:   lHint := i18Manager.GetFormattedString('HintVar', [lScope, lWord, lIdent.TypeAsString]);
         CONSTANT:   lHint := i18Manager.GetFormattedString('HintConst', [lWord, lIdent.Value]);
         ROUTINE_ID: lHint := i18Manager.GetFormattedString('HintRoutine', [lWord, lIdent.TypeAsString]);
         ENUM_VALUE: lHint := i18Manager.GetFormattedString('HintEnum', [lWord, lIdent.TypeAsString]);
      end;
      if lHint <> '' then
      begin
         memCodeEditor.Hint := lHint;
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
   lRange: TSynEditFoldRange;
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
                     lRange := CollapsableFoldRangeForLine(i+1);
                     if (lRange <> nil) and lRange.Collapsed then
                     begin
                        result.FoldRange := lRange;
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
   lChar, lLine: integer;
begin
   if ALine.CodeRange.Lines = memCodeEditor.Lines then
   begin
      lChar := ALine.Col + ALine.EditCaretXY.Char;
      lLine := ALine.Row + ALIne.EditCaretXY.Line + 1;
      if (lLine > ALine.CodeRange.FirstRow) and (lLine <= ALine.CodeRange.LastRow+1) and (lLine <= ALine.CodeRange.Lines.Count) then
      begin
         memCodeEditor.CaretXY := BufferCoord(lChar, lLine);
         memCodeEditor.EnsureCursorPosVisible;
      end;
   end;
end;

procedure TEditorForm.UnSelectCodeRange(const AObject: TObject);
var
   lCodeRange: TCodeRange;
begin
   if memCodeEditor.SelAvail and memCodeEditor.CanFocus then
   begin
      lCodeRange := SelectCodeRange(AObject, false);
      if (lCodeRange.FirstRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart).Line-1) and
         (lCodeRange.LastRow = memCodeEditor.CharIndexToRowCol(memCodeEditor.SelEnd).Line-1) then
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
   lRange: TSynEditFoldRange;
begin
   result := TSynEditFoldRanges.Create;
   if ACodeRange.Lines = memCodeEditor.Lines then
   begin
      for i := ACodeRange.FirstRow to ACodeRange.FirstRow+ACount do
      begin
         lRange := memCodeEditor.CollapsableFoldRangeForLine(i+1);
         if (lRange <> nil) and (result.Ranges.IndexOf(lRange) = -1) then
            result.AddF(lRange);
      end;
   end;
end;
{$ENDIF}

procedure TEditorForm.RefreshEditorForObject(const AObject: TObject);
var
   lTopLine, lLine: integer;
   lRange: TCodeRange;
   lScrollStyle: TScrollStyle;
begin
   FFocusEditor := false;
   lTopLine := memCodeEditor.TopLine;
   lScrollStyle := memCodeEditor.ScrollBars;
   memCodeEditor.ScrollBars := ssNone;
   try
      FormShow(Self);
      if AObject <> nil then
      begin
         lRange := SelectCodeRange(AObject, false);
         lLine := lRange.FirstRow + 1;
         if (lLine > 0) and not lRange.IsFolded then
         begin
            if (lLine < lTopLine) or (lLine > lTopLine + memCodeEditor.LinesInWindow) then
               memCodeEditor.GotoLineAndCenter(lLine)
            else
               memCodeEditor.TopLine := lTopLine;
         end
      end
      else
         memCodeEditor.TopLine := lTopLine;
   finally
      memCodeEditor.ScrollBars := lScrollStyle;
   end;
end;

function TEditorForm.GetIndentLevel(const idx: integer; ALines: TStrings = nil): integer;
var
   lLine: string;
   i: integer;
begin
   result := 0;
   if ALines = nil then
      ALines := GetAllLines;
   if (idx >= 0) and (idx < ALines.Count) then
   begin
      lLine := ALines[idx];
      for i := 1 to Length(lLine) do
      begin
         if lLine[i] = INDENT_CHAR then
            result := i
         else
            break;
      end;
      result := result div GSettings.IndentLength;
   end;
end;

procedure TEditorForm.ExportSettingsToXMLTag(const root: IXMLElement);
var
   i: integer;
   tag2, tag1: IXMLElement;
   idObject: IIdentifiable;
   lLines: TStrings;
{$IFDEF USE_CODEFOLDING}
   lFoldRange: TSynEditFoldRange;
{$ENDIF}
begin
   if Visible then
   begin
      root.SetAttribute('src_win_show', '1');
      root.SetAttribute('src_win_x', IntToStr(Left));
      root.SetAttribute('src_win_y', IntToStr(Top));
      root.SetAttribute('src_win_w', IntToStr(Width));
      root.SetAttribute('src_win_h', IntToStr(Height));
      root.SetAttribute('src_win_sel_start', IntToStr(memCodeEditor.SelStart));
      if memCodeEditor.SelAvail then
         root.SetAttribute('src_win_sel_length', IntToStr(memCodeEditor.SelLength));
      if memCodeEditor.Marks.Count > 0 then
      begin
         for i := 0 to memCodeEditor.Marks.Count-1 do
         begin
            tag2 := root.OwnerDocument.CreateElement('src_win_mark');
            tag2.SetAttribute('line', IntToStr(memCodeEditor.Marks[i].Line));
            tag2.SetAttribute('index', IntToStr(memCodeEditor.Marks[i].ImageIndex));
            root.AppendChild(tag2);
         end;
      end;
      if memCodeEditor.TopLine > 1 then
         root.SetAttribute('src_top_line', IntToStr(memCodeEditor.TopLine));
      if WindowState = wsMinimized then
         root.SetAttribute('src_win_min', '1');
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         tag1 := nil;
         for i := 0 to memCodeEditor.AllFoldRanges.AllCount-1 do
         begin
            lFoldRange := memCodeEditor.AllFoldRanges[i];
            if lFoldRange.Collapsed then
            begin
               if tag1 = nil then
               begin
                  tag1 := root.OwnerDocument.CreateElement('fold_ranges');
                  root.AppendChild(tag1);
               end;
               tag2 := root.OwnerDocument.CreateElement('fold_range');
               TXMLProcessor.AddText(tag2, IntToStr(memCodeEditor.GetRealLineNumber(lFoldRange.FromLine)));
               tag1.AppendChild(tag2);
            end;
         end;
      end;
{$ENDIF}
      lLines := GetAllLines;
      for i := 0 to lLines.Count-1 do
      begin
         tag2 := root.OwnerDocument.CreateElement('text_line');
         TXMLProcessor.AddCDATA(tag2, lLines[i]);
         if TInfra.IsValid(lLines.Objects[i]) and Supports(lLines.Objects[i], IIdentifiable, idObject) then
            tag2.SetAttribute(ID_ATTR, IntToStr(idObject.Id));
         root.AppendChild(tag2);
      end;
      root.SetAttribute('modified', BoolToStr(memCodeEditor.Modified, true));
   end;
end;

procedure TEditorForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   lRect: TRect;
   i: integer;
   tag1, tag2: IXMLElement;
   lMark: TSynEditMark;
   lFoldedLines: TStringList;
{$IFDEF USE_CODEFOLDING}
   lFoldRange: TSynEditFoldRange;
{$ENDIF}
begin
   if (root.GetAttribute('src_win_show') = '1') and GInfra.CurrentLang.EnabledCodeGenerator then
   begin
      lRect.Left := StrToIntDef(root.GetAttribute('src_win_x'), 50);
      lRect.Top := StrToIntDef(root.GetAttribute('src_win_y'), 50);
      lRect.Right := StrToIntDef(root.GetAttribute('src_win_w'), 425);
      lRect.Bottom := StrToIntDef(root.GetAttribute('src_win_h'), 558);
      Position := poDesigned;
      SetBounds(lRect.Left, lRect.Top, lRect.Right, lRect.Bottom);
      if root.GetAttribute('src_win_min') = '1' then
         WindowState := wsMinimized;
      OnShow := nil;
      Show;
      OnShow := FormShow;
      root.OwnerDocument.PreserveWhiteSpace := true;
      tag1 := TXMLProcessor.FindChildTag(root, 'text_line');
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
      memCodeEditor.Modified := root.GetAttribute('modified') = 'True';
      memCodeEditor.SelStart := StrToIntDef(root.GetAttribute('src_win_sel_start'), 0);
      memCodeEditor.SelLength := StrToIntDef(root.GetAttribute('src_win_sel_length'), 0);
{$IFDEF USE_CODEFOLDING}
      if memCodeEditor.CodeFolding.Enabled then
      begin
         memCodeEditor.ReScanForFoldRanges;
         tag1 := TXMLProcessor.FindChildTag(root, 'fold_ranges');
         if tag1 <> nil then
         begin
            lFoldedLines := TStringList.Create;
            try
               tag2 := TXMLProcessor.FindChildTag(tag1, 'fold_range');
               while tag2 <> nil do
               begin
                  if StrToIntDef(tag2.Text, 0) > 0 then
                     lFoldedLines.Add(tag2.Text);
                  tag2 := TXMLProcessor.FindNextTag(tag2);
               end;
               lFoldedLines.CustomSort(@CompareIntegers);
               for i := lFoldedLines.Count-1 downto 0 do
               begin
                  lFoldRange := memCodeEditor.CollapsableFoldRangeForLine(StrToInt(lFoldedLines[i]));
                  if (lFoldRange <> nil) and not lFoldRange.Collapsed then
                  begin
                     memCodeEditor.Collapse(lFoldRange);
                     memCodeEditor.Refresh;
                  end;
               end;
            finally
               lFoldedLines.Free;
            end;
         end;
      end;
{$ENDIF}
      root.OwnerDocument.PreserveWhiteSpace := false;
      i := StrToIntDef(root.GetAttribute('src_top_line'), 0);
      if i > 0 then
         memCodeEditor.TopLine := i;
      tag1 := TXMLProcessor.FindChildTag(root, 'src_win_mark');
      while tag1 <> nil do
      begin
{$IFDEF USE_CODEFOLDING}
         lMark := TSynEditMark.Create(memCodeEditor);
{$ELSE}
         lMark := TSynEditMark.Create;
{$ENDIF}
         lMark.ImageIndex := StrToInt(tag1.GetAttribute('index'));
         memCodeEditor.Marks.Add(lMark);
         lMark.Line := StrToInt(tag1.GetAttribute('line'));
         lMark.Visible := true;
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
   lPoint: TPoint;
   lCoord: TDisplayCoord;
   lSelCoord: TBufferCoord;
   lFocusInfo: TFocusInfo;
   lCodeRange: TCodeRange;
begin
   if (FFocusControl <> nil) and FFocusControl.CanBeFocused then
   begin
      TInfra.InitFocusInfo(lFocusInfo);
      lPoint := memCodeEditor.ScreenToClient(pmPopMenu.PopupPoint);
      lCoord := memCodeEditor.PixelsToRowColumn(lPoint.X, lPoint.Y);
      if lCoord.Row > 0 then
      begin
         lFocusInfo.Line := lCoord.Row - 1;
         lFocusInfo.LineText := TrimLeft(memCodeEditor.Lines[lFocusInfo.Line]);
         lCodeRange := SelectCodeRange(memCodeEditor.Lines.Objects[lFocusInfo.Line], false);
         if lCodeRange.FirstRow <> ROW_NOT_FOUND then
            lFocusInfo.RelativeLine := lFocusInfo.Line - lCodeRange.FirstRow;
         lSelCoord := memCodeEditor.CharIndexToRowCol(memCodeEditor.SelStart);
         if lSelCoord.Line = lCoord.Row then
         begin
            lFocusInfo.SelStart := lSelCoord.Char - Length(memCodeEditor.Lines[lCoord.Row-1]) + Length(lFocusInfo.LineText);
            lFocusInfo.SelText := AnsiMidStr(lFocusInfo.LineText, lFocusInfo.SelStart, memCodeEditor.SelLength);
         end;
      end;
      FFocusControl.RetrieveFocus(lFocusInfo);
   end;
   FFocusControl := nil;
end;

end.

