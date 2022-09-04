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



{ This unit contains implementation of main form }

unit Main_Form;

interface

uses
  WinApi.Windows, Vcl.Graphics, Vcl.Forms, System.SysUtils, Vcl.Menus, Vcl.ImgList,
  System.Classes, Vcl.Dialogs, WinApi.Messages, Vcl.ComCtrls, System.ImageList,
  Base_Form, History, Interfaces, OmniXML, Vcl.Controls;

type

  TClockPos = (cp12, cp3, cp6, cp9);

  TMainForm = class(TBaseForm)
    pmPages: TPopupMenu;
    ExportDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    miInstr: TMenuItem;
    miMultiInstr: TMenuItem;
    miIfElse: TMenuItem;
    miWhile: TMenuItem;
    miFor: TMenuItem;
    miRepeat: TMenuItem;
    miFontStyle: TMenuItem;
    miFontSize: TMenuItem;
    miStyleBold: TMenuItem;
    miStyleItalic: TMenuItem;
    miStyleUnderline: TMenuItem;
    miStyleNormal: TMenuItem;
    miInsert: TMenuItem;
    miFont: TMenuItem;
    miRemove: TMenuItem;
    miInput: TMenuItem;
    miOutput: TMenuItem;
    mmMainMenu: TMainMenu;
    miFile: TMenuItem;
    miProject: TMenuItem;
    miAbout: TMenuItem;
    miToolbox: TMenuItem;
    miDeclarations: TMenuItem;
    miGenerate: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miClose: TMenuItem;
    N2: TMenuItem;
    miExit: TMenuItem;
    N3: TMenuItem;
    miPrint: TMenuItem;
    miSettings: TMenuItem;
    PrintDialog: TPrintDialog;
    N4: TMenuItem;
    miComment: TMenuItem;
    miRoutineCall: TMenuItem;
    miInstrs: TMenuItem;
    miLoop: TMenuItem;
    miUndoRemove: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miReopen: TMenuItem;
    N5: TMenuItem;
    miIf: TMenuItem;
    miExplorer: TMenuItem;
    miOptions: TMenuItem;
    miSubRoutines: TMenuItem;
    ImageList1: TImageList;
    miCut: TMenuItem;
    N1: TMenuItem;
    N6: TMenuItem;
    miDataTypes: TMenuItem;
    miFrame: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miFoldUnfold: TMenuItem;
    miAddBranch: TMenuItem;
    miCase: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    miRemoveBranch: TMenuItem;
    miStyleStrikeOut: TMenuItem;
    miReturn: TMenuItem;
    miUnfoldAll: TMenuItem;
    miPrint2: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    miAddMain: TMenuItem;
    N13: TMenuItem;
    miNewFlowchart: TMenuItem;
    miNavigator: TMenuItem;
    N12: TMenuItem;
    miText: TMenuItem;
    N14: TMenuItem;
    miForAsc: TMenuItem;
    miForDesc: TMenuItem;
    miMemo: TMenuItem;
    miMemoVScroll: TMenuItem;
    miMemoEdit: TMenuItem;
    N15: TMenuItem;
    miMemoHScroll: TMenuItem;
    miMemoWordWrap: TMenuItem;
    miNewFunction: TMenuItem;
    miFolder: TMenuItem;
    pgcPages: TPageControl;
    pmTabs: TPopupMenu;
    miAddPage: TMenuItem;
    miRemovePage: TMenuItem;
    miRenamePage: TMenuItem;
    pmEdits: TPopupMenu;
    miUndo: TMenuItem;
    N16: TMenuItem;
    miCut1: TMenuItem;
    miCopy1: TMenuItem;
    miPaste1: TMenuItem;
    miRemove1: TMenuItem;
    N17: TMenuItem;
    miInsertFunc: TMenuItem;
    miIsHeader: TMenuItem;
    miPasteText: TMenuItem;
    N18: TMenuItem;
    miMemoAlignRight: TMenuItem;
    miInsertBranch: TMenuItem;
    stbStatusBar: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miGenerateClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OnException(Sender: TObject; E: Exception);
    procedure miUndoRemoveClick(Sender: TObject);
    procedure pmPagesPopup(Sender: TObject);
    procedure miCommentClick(Sender: TObject);
    procedure miInstrClick(Sender: TObject);
    procedure miStyleBoldClick(Sender: TObject);
    procedure miFontSizeClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miRemoveClick(Sender: TObject);
    procedure miSubRoutinesClick(Sender: TObject);
    procedure miExportClick(Sender: TObject);
    procedure miImportClick(Sender: TObject);
    procedure miFrameClick(Sender: TObject);
    procedure miFoldUnfoldClick(Sender: TObject);
    procedure miAddBranchClick(Sender: TObject);
    procedure miRemoveBranchClick(Sender: TObject);
    procedure miUnfoldAllClick(Sender: TObject);
    procedure Localize(AList: TStringList); override;
    procedure ResetForm; override;
    procedure SetProjectMenu(AEnabled: boolean);
    procedure miPrint2Click(Sender: TObject);
    procedure miProjectClick(Sender: TObject);
    procedure miAddMainClick(Sender: TObject);
    procedure miNewFlowchartClick(Sender: TObject);
    procedure miForAscClick(Sender: TObject);
    procedure miMemoEditClick(Sender: TObject);
    procedure miMemoVScrollClick(Sender: TObject);
    procedure miNewFunctionClick(Sender: TObject);
    procedure pgcPagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure pmTabsPopup(Sender: TObject);
    procedure miRemovePageClick(Sender: TObject);
    procedure pgcPagesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pgcPagesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pgcPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure miRenamePageClick(Sender: TObject);
    procedure miAddPageClick(Sender: TObject);
    procedure pgcPagesChange(Sender: TObject);
    procedure pmEditsPopup(Sender: TObject);
    procedure pmEditsMenuClick(Sender: TObject);
    procedure FuncMenuClick(Sender: TObject);
    procedure miIsHeaderClick(Sender: TObject);
    procedure miPasteTextClick(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  private
    { Private declarations }
    FClockPos: TClockPos;
    FHistoryMenu: THistoryMenu;
    function BuildFuncMenu: integer;
    function CreateNewProject: boolean;
    function CloseExistingProject: boolean;
    function CanCloseExistingProject: boolean;
    procedure PopupMenuClosed;
    procedure PPIDialog;
  public
    { Public declarations }
    function ConfirmSave: integer;
    function GetMainBlockNextTopLeft: TPoint;
    procedure AcceptFile(const AFilePath: string);
    procedure SetChanged;
    procedure UpdateTabsColor(AColor: TColor);
  end;

  TPopupListEx = class(TPopupList)
  protected
     procedure WndProc(var msg: TMessage); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
   Vcl.StdCtrls, Vcl.Clipbrd, System.StrUtils, System.UITypes, System.Types, System.Generics.Defaults,
   System.Generics.Collections, System.Math, Toolbox_Form, Infrastructure, About_Form,
   Main_Block, ParseGlobals, LocalizationManager, XMLProcessor, UserFunction, ForDo_Block,
   Return_Block, Project, Declarations_Form, Base_Block, Comment, Case_Block, Navigator_Form,
   Types, LangDefinition, EditMemo_Form, BlockFactory, BlockTabSheet, MemoEx, Constants;

var
   ByCaptionMenuItemComparer: IComparer<TMenuItem>;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
const
   CursorIDs: array[TCustomCursor] of PChar = (' ', 'IFELSE', 'FOR', 'REPEAT', 'WHILE', 'ASSIGN',
              'MULTIASSIGN', 'IF', 'SUBROUTINE', 'INPUT', 'OUTPUT', 'CASE', 'RETURN', 'TEXT', 'FOLDER');
begin
   var lCursor := crNormal;
   repeat
      Inc(lCursor);
      Screen.Cursors[Ord(lCursor)] := LoadCursor(HInstance, CursorIDs[lCursor]);
   until lCursor = High(TCustomCursor);
   InitialiseVariables;
   SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, Ord(True), nil, 0);
   Application.DefaultFont.Name := APPLICATION_DEFAULT_FONT_NAME;
   Application.DefaultFont.Size := APPLICATION_DEFAULT_FONT_SIZE;
   Application.HintHidePause := HINT_PAUSE;
   Application.OnException := OnException;
   Application.Title := PROGRAM_NAME;
   Caption := PROGRAM_NAME;
   FHistoryMenu := THistoryMenu.Create(miReopen, miOpen.OnClick);
   FHistoryMenu.Load;
   pgcPages.DoubleBuffered := true;
   FClockPos := Low(TClockPos);
   for var i := FLOWCHART_MIN_FONT_SIZE to FLOWCHART_MAX_FONT_SIZE do
   begin
      var menuItem := TMenuItem.Create(miFontSize);
      menuItem.Caption := i.ToString;
      menuItem.OnClick := miFontSizeClick;
      miFontSize.Add(menuItem);
   end;
end;

procedure TMainForm.KeyDown(var Key: Word; Shift: TShiftState);
var
   selectedBlock: TBlock;

   procedure ExecuteClick(AMenuItem: TMenuItem);
   begin
      pmPages.PopupComponent := selectedBlock;
      AMenuItem.Click;
   end;

begin
   if GProject = nil then
      Exit;
   if Key in TO_MAIN_FORM_KEYS then
   begin
      selectedBlock := GProject.FindSelectedBlock;
      if selectedBlock <> nil then
      begin
         case Key of
            vkDelete: ExecuteClick(miRemove);
            vkF12:
            begin
               if selectedBlock is TGroupBlock then
                  ExecuteClick(miFoldUnfold);
            end;
            vkF11:
            begin
               ExecuteClick(miFrame);
               var p := selectedBlock.ScreenToClient(Mouse.CursorPos);
               selectedBlock.OnMouseMove(selectedBlock, Shift, p.X, p.Y);
            end;
            vkF10:
            begin
               if selectedBlock is TGroupBlock then
               begin
                  var groupBlock := TGroupBlock(selectedBlock);
                  if groupBlock.Expanded and groupBlock.HasFoldedBlocks then
                     ExecuteClick(miUnfoldAll);
               end;
            end;
         end;
         Key := 0;
      end;
   end
   else
      GProject.ActivePage.Box.BoxKeyDown(Self, Key, Shift);
end;

procedure TMainForm.ResetForm;
begin
   miUndoRemove.Enabled := false;
   Caption := PROGRAM_NAME;
   FClockPos := Low(TClockPos);
   SetProjectMenu(false);
   miInsertFunc.Clear;
   while pgcPages.PageCount > 0 do
      pgcPages.Pages[0].Free;
end;

procedure TMainForm.SetProjectMenu(AEnabled: boolean);
begin
   var mMenu := Menu;
   Menu := nil;
   var clang := GInfra.CurrentLang;
   miSave.Enabled := AEnabled;
   miSaveAs.Enabled := AEnabled;
   miClose.Enabled := AEnabled;
   miPrint.Enabled := AEnabled;
   miToolbox.Enabled := AEnabled;
   miNavigator.Enabled := AEnabled;
   miSubRoutines.Enabled := AEnabled and clang.EnabledUserFunctionHeader;
   miDataTypes.Enabled := AEnabled and clang.EnabledUserDataTypes;
   miDeclarations.Enabled := AEnabled and (clang.EnabledConsts or clang.EnabledVars);
   miExplorer.Enabled := AEnabled and clang.EnabledExplorer;
   miGenerate.Enabled := AEnabled and clang.EnabledCodeGenerator;
   miAddMain.Enabled := AEnabled and clang.EnabledMainProgram;
   Menu := mMenu;
end;

procedure TMainForm.Localize(AList: TStringList);
begin
   if GProject <> nil then
   begin
      GProject.RepaintFlowcharts;
      GProject.RefreshStatements;
   end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
   SetProjectMenu(false);
   // will display PPI dialog when main form is already visible
   TThread.ForceQueue(nil, PPIDialog);
end;

procedure TMainForm.PPIDialog;
begin
   if TInfra.PPI > MAX_SUPPORTED_PPI then
      TInfra.ShowWarningBox('OverMaxPPI', [TInfra.PPI, MAX_SUPPORTED_PPI, sLineBreak]);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FHistoryMenu.Save;
   GClpbrd.UndoObject.Free;
   GProject.Free;
   GProject := nil;
   FHistoryMenu.Free;
end;

function TMainForm.GetMainBlockNextTopLeft: TPoint;
const
   nextPos: array[TClockPos] of TClockPos = (cp3, cp6, cp9, cp12);
   xShift: array[TClockPos] of integer = (0, 20, 0, -20);
   yShift: array[TClockPos] of integer = (20, 40, 60, 40);
begin
   result.X := ((pgcPages.ClientWidth - MAIN_BLOCK_DEF_WIDTH) div 2) + xShift[FClockPos];
   result.Y := yShift[FClockPos];
   FClockPos := nextPos[FClockPos];
end;

function TMainForm.CanCloseExistingProject: boolean;
begin
   result := true;
   if (GProject <> nil) and GProject.IsChanged then
   begin
      case ConfirmSave of
         IDYES: miSave.Click;
         IDCANCEL: result := false;
      end;
   end;
end;

function TMainForm.CloseExistingProject: boolean;
begin
   result := CanCloseExistingProject;
   if result then
      TInfra.Reset;
end;

procedure TMainForm.miCloseClick(Sender: TObject);
begin
   CloseExistingProject;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := CanCloseExistingProject;
end;

function TMainForm.CreateNewProject: boolean;
begin
   result := CloseExistingProject;
   if result then
   begin
      GProject := TProject.GetInstance;
      SetProjectMenu(true);
   end;
end;

procedure TMainForm.miNewClick(Sender: TObject);
begin
   if CreateNewProject then
   begin
      var mBlock := TMainBlock.Create(GProject.MainPage, GetMainBlockNextTopLeft);
      mBlock.OnResize(mBlock);
      TUserFunction.Create(nil, mBlock);
      GProject.ChangingOn := true;
   end;
end;

procedure TMainForm.miOpenClick(Sender: TObject);
begin
   if CreateNewProject then
   begin
      var tmpCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      var filePath := IfThen(Sender <> miOpen, StripHotKey(TMenuItem(Sender).Caption));
      filePath := TXMLProcessor.ImportFromXMLFile(GProject.ImportFromXMLTag, impAll, filePath);
      GProject.ChangingOn := true;
      GProject.SetNotChanged;
      Screen.Cursor := tmpCursor;
      SetFocus;
      if not filePath.IsEmpty then
         AcceptFile(filePath)
      else
         TInfra.Reset;
   end;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
begin
   TInfra.ExportToFile(GProject);
end;

procedure TMainForm.miGenerateClick(Sender: TObject);
begin
   var form := TInfra.GetEditorForm;
   if form.Showing then
   begin
      if form.WindowState = wsMinimized then
         form.WindowState := wsNormal;
      form.OnShow(form);
   end
   else
      form.Show;
end;

procedure TMainForm.miSettingsClick(Sender: TObject);
begin
   TInfra.GetSettingsForm.ShowModal;
end;

procedure TMainForm.miExitClick(Sender: TObject);
begin
   Close;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
    if GProject.IsNew then
       miSaveAs.Click
    else
    begin
       var filePath := ReplaceText(Caption, PROJECT_CAPTION, '');
       filePath := ReplaceText(filePath, '*', '');
       GProject.ExportToXMLFile(filePath);
    end;
end;

procedure TMainForm.miPrintClick(Sender: TObject);
begin
   var bitmap := TBitmap.Create;
   try
      GProject.ExportToGraphic(bitmap);
      TInfra.PrintBitmap(bitmap);
   finally
      bitmap.Free;
   end;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
   AboutForm.ShowModal;
end;

procedure TMainForm.OnException(Sender: TObject; E: Exception);
begin
   Application.ShowException(E);
   if GProject <> nil then
      miSaveAs.Click;
   TInfra.Reset;
end;

procedure TMainForm.miUndoRemoveClick(Sender: TObject);
var
   block: TBlock;
   xmlObj: IXMLable;
begin
   if (GClpbrd.UndoObject is TBlock) and (TBlock(GClpbrd.UndoObject).ParentBlock <> nil) then
   begin
      block := TBlock(GClpbrd.UndoObject);
      if not block.ParentBlock.CanFocus or
         not block.ParentBlock.Expanded or ((block is TReturnBlock) and (block.ParentBranch.FindInstanceOf(TReturnBlock) <> -1)) then Exit;
      block.ParentBranch.UndoRemove(block);
      block.ParentBlock.ResizeWithDrawLock;
      block.SetVisible(true);
      NavigatorForm.Invalidate;
   end
   else if Supports(GClpbrd.UndoObject, IXMLable, xmlObj) then
      xmlObj.Active := true;
   TInfra.UpdateCodeEditor(GClpbrd.UndoObject);
   if (GClpbrd.UndoObject is TUserFunction) and (GClpbrd.Instance = TUserFunction(GClpbrd.UndoObject).Body) then
      GClpbrd.Instance := nil;
   GClpbrd.UndoObject := nil;
   miUndoRemove.Enabled := false;
end;

procedure TMainForm.pmPagesPopup(Sender: TObject);
var
   comp: TComponent;
   block: TBlock;
   lFont: TFont;
   isFunction, expanded: boolean;
   memo: TMemoEx;
   memoEx: IMemoEx;
   i: integer;
   menuItem: TMenuItem;
begin
   lFont := nil;
   memo := nil;
   miFont.Visible := False;
   miCopy.Visible := False;
   miCut.Visible := False;
   miRemove.Visible := False;
   miInsert.Visible := False;
   miPaste.Enabled := False;
   miPasteText.Visible := False;
   miInstrs.Enabled := False;
   miLoop.Enabled := False;
   miFrame.Visible := False;
   miFrame.Checked := False;
   miExport.Visible := False;
   miImport.Visible := True;
   miFoldUnfold.Visible := False;
   miAddBranch.Visible := False;
   miRemoveBranch.Visible := False;
   miInsertBranch.Visible := False;
   miText.Enabled := False;
   miFolder.Enabled := False;
   miFoldUnfold.Caption := i18Manager.GetString('miFoldTrue');
   miReturn.Enabled := False;
   miUnfoldAll.Visible := False;
   miPrint2.Visible := False;
   miNewFlowchart.Visible := GInfra.CurrentLang = GInfra.TemplateLang;
   miNewFunction.Visible := GInfra.CurrentLang.EnabledUserFunctionHeader and GInfra.CurrentLang.EnabledUserFunctionBody;
   miForAsc.Visible := False;
   miForDesc.Visible := False;
   miMemo.Visible := False;
   miStyleBold.Checked := False;
   miStyleItalic.Checked := False;
   miStyleUnderline.Checked := False;
   miStyleStrikeOut.Checked := False;
   miStyleNormal.Checked := False;
   for i := 0 to miFontSize.Count-1 do
      miFontSize.Items[i].Checked := False;
   miIsHeader.Visible := False;
   miMemoVScroll.Checked := False;
   miMemoHScroll.Checked := False;
   miMemoWordWrap.Checked := False;
   miMemoAlignRight.Checked := False;

   isFunction := GClpbrd.UndoObject is TUserFunction;
   miPaste.Enabled := TInfra.IsValidControl(GClpbrd.Instance) or isFunction;

   comp := pmPages.PopupComponent;

   if Supports(comp, IMemoEx, memoEx) then
   begin
      memo := memoEx.GetMemoEx;
      miMemo.Visible := (memo <> nil) and memo.Visible;
   end;

   if comp is TBlock then
   begin
       block := TBlock(comp);
       lFont := block.GetFont;
       if block.Ired >= 0 then
       begin
          miMemo.Visible := False;
          miInsert.Visible := True;
          miInstrs.Enabled := True;
          miLoop.Enabled := True;
          miText.Enabled := True;
          miFolder.Enabled := True;
          miReturn.Enabled := block.CanInsertReturnBlock;
          if (GClpbrd.Instance is TComment) or ((GClpbrd.Instance is TReturnBlock) and not miReturn.Enabled) then
             miPaste.Enabled := False;
       end
       else if block.IsMouseAtSelectPos then
       begin
          miRemove.Visible := True;
          miFont.Visible := True;
          if not (block is TReturnBlock) then
             miExport.Visible := True;
          miPrint2.Visible := True;
          miCut.Visible := True;
          if not (block is TMainBlock) then
             miCopy.Visible := True;
          if block is TGroupBlock then
          begin
             miFoldUnfold.Visible := True;
             expanded := TGroupBlock(block).Expanded;
             if expanded and (block is TCaseBlock) then
                miAddBranch.Visible := True;
             miFoldUnfold.Caption := i18Manager.GetString('miFold' + BoolToStr(expanded, true));
          end;
          if (block is TGroupBlock) and TGroupBlock(block).Expanded and TGroupBlock(block).HasFoldedBlocks then
             miUnfoldAll.Visible := True;
          if block is TForDoBlock then
          begin
             miForAsc.Visible := True;
             miForDesc.Visible := True;
             miForDesc.Checked := TForDoBlock(block).DescOrder;
             miForAsc.Checked := not miForDesc.Checked;
          end;
       end
       else
       begin
          miMemo.Visible := False;
          miInsert.Visible := True;
          miComment.Enabled := True;
          if (GClpbrd.Instance is TBlock) and not isFunction then
             miPaste.Enabled := False;
       end;
       miFrame.Visible := True;
       miFrame.Checked := block.Frame;
       if (block is TCaseBlock) and (block.Ired > PRIMARY_BRANCH_IDX) then
       begin
          miRemoveBranch.Visible := True;
          miInsertBranch.Visible := True;
       end;
   end
   else if comp is TComment then
   begin
      miPasteText.Visible := Clipboard.HasFormat(CF_TEXT);
      lFont := TComment(comp).Font;
      miRemove.Visible := True;
      miFont.Visible := True;
      miCopy.Visible := True;
      miCut.Visible := TComment(comp).SelLength > 0;
      miIsHeader.Visible := True;
      miIsHeader.Checked := TComment(comp).IsHeader;
      if GClpbrd.Instance is TBlock then
         miPaste.Enabled := False;
   end
   else
   begin
      miInsert.Visible := True;
      if GClpbrd.Instance is TMainBlock then
         miPaste.Enabled := True
      else if GClpbrd.Instance is TBlock then
         miPaste.Enabled := False;
   end;
   if lFont <> nil then
   begin
      miStyleBold.Checked := fsBold in lFont.Style;
      miStyleItalic.Checked := fsItalic in lFont.Style;
      miStyleUnderline.Checked := fsUnderline in lFont.Style;
      miStyleStrikeOut.Checked := fsStrikeOut in lFont.Style;
      miStyleNormal.Checked := lFont.Style = [];
      menuItem := miFontSize.Find(lFont.Size.ToString);
      if menuItem <> nil then
         menuItem.Checked := True;
   end;
   if miMemo.Visible then
   begin
      miMemoVScroll.Checked := memo.HasVScroll;
      miMemoHScroll.Checked := memo.HasHScroll;
      miMemoWordWrap.Checked := memo.WordWrap;
      miMemoAlignRight.Checked := memo.Alignment = taRightJustify;
   end;
end;

procedure TMainForm.miCommentClick(Sender: TObject);
begin
   if GProject <> nil then
   begin
      var page := GProject.ActivePage;
      var p := page.Box.ScreenToClient(page.Box.PopupMenu.PopupPoint);
      TComment.Create(page, p.X, p.Y, 150, 50);
      GProject.SetChanged;
   end;
end;

procedure TMainForm.miInstrClick(Sender: TObject);
var
   newBlock, currBlock, srcBlock: TBlock;
   branch: TBranch;
   lParent: TGroupBlock;
   tmpCursor: TCursor;
   comment: TComment;
   p: TPoint;
   blockType: TBlockType;
   page: TBlockTabSheet;
   func: TUserFunction;
begin

   srcBlock := nil;
   lParent := nil;
   func := nil;
   comment := nil;

   if GClpbrd.UndoObject is TUserFunction then
      func := TUserFunction(GClpbrd.UndoObject)
   else if GClpbrd.Instance is TComment then
      comment := TComment(GClpbrd.Instance);

   if (Sender = miPaste) and ((func <> nil) or (comment <> nil)) then
   begin
      page := GProject.ActivePage;
      p := page.Box.ScreenToClient(page.Box.PopupMenu.PopupPoint);
      if func <> nil then
      begin
         if func.Body <> nil then
         begin
            func.Body.Page := page;
            TInfra.MoveWin(func.Body, p);
         end;
         miUndoRemoveClick(miUndoRemove);
      end
      else if comment <> nil then
         comment.Clone(page, p);
      GProject.SetChanged;
      NavigatorForm.Invalidate;
      Exit;
   end;

   if pmPages.PopupComponent is TBlock then
   begin
      currBlock := TBlock(pmPages.PopupComponent);
      if (currBlock.Ired > 0) and (currBlock is TGroupBlock) then
         lParent := TGroupBlock(currBlock)
      else if currBlock.Ired = 0 then
         lParent := currBlock.ParentBlock;
   end;

   if lParent <> nil then
   begin

      branch := lParent.GetBranch(lParent.Ired);
      if branch <> nil then
         currBlock := nil
      else
         branch := currBlock.ParentBranch;

      if branch <> nil then
      begin
         branch.ParentBlock.TopParentBlock.LockDrawing;
         try
            newBlock := nil;
            blockType := blUnknown;
            if Sender = miInstr then
               blockType := blInstr
            else if Sender = miMultiInstr then
               blockType := blMultiInstr
            else if Sender = miIfElse then
               blockType := blIfElse
            else if Sender = miWhile then
               blockType := blWhile
            else if Sender = miFor then
               blockType := blFor
            else if Sender = miRepeat then
               blockType := blRepeat
            else if Sender = miInput then
               blockType := blInput
            else if Sender = miOutput then
               blockType := blOutput
            else if Sender = miRoutineCall then
               blockType := blFuncCall
            else if Sender = miIf then
               blockType := blIf
            else if Sender = miCase then
               blockType := blCase
            else if Sender = miReturn then
               blockType := blReturn
            else if Sender = miText then
               blockType := blText
            else if Sender = miFolder then
               blockType := blFolder
            else if (Sender = miPaste) and TInfra.IsValidControl(GClpbrd.Instance) and (GClpbrd.Instance is TBlock) then
            begin
               srcBlock := TBlock(GClpbrd.Instance);
               tmpCursor := Screen.Cursor;
               Screen.Cursor := crHourGlass;
               newBlock := srcBlock.Clone(branch);
               Screen.Cursor := tmpCursor;
            end;
            if blockType <> blUnknown then
               newBlock := TBlockFactory.Create(branch, blockType);
            if newBlock <> nil then
            begin
               branch.InsertAfter(newBlock, currBlock);
               lParent.ResizeHorz(true);
               lParent.ResizeVert(true);
               if not newBlock.Visible then
               begin
                  newBlock.Show;
                  newBlock.PerformRefreshStatements;
               end;
               if srcBlock <> nil then
                  newBlock.CloneComments(srcBlock);
               TInfra.UpdateCodeEditor(newBlock);
            end;
         finally
            branch.ParentBlock.TopParentBlock.UnLockDrawing;
         end;
      end;
   end;
   NavigatorForm.Invalidate;
end;

function TMainForm.ConfirmSave: integer;
begin
   result := mrCancel;
   if GProject <> nil then
      result := TInfra.ShowQuestionBox('ConfirmClose', [GProject.Name]);
end;

procedure TMainForm.SetChanged;
begin
   if (GProject <> nil) and not EndsText('*', Caption) then
      Caption := Caption + '*';
end;

procedure TMainForm.miStyleBoldClick(Sender: TObject);
var
   comp: TComponent;
   fontStyles: TFontStyles;
   fontStyle: TFontStyle;
   block: TBlock;
   comment: TComment;
begin
   block := nil;
   comment := nil;
   comp := pmPages.PopupComponent;
   if (comp is TBlock) or (comp is TComment) then
   begin
      if comp is TBlock then
      begin
         block := TBlock(comp);
         fontStyles := block.GetFont.Style;
      end
      else if comp is TComment then
      begin
         comment := TComment(comp);
         fontStyles := comment.Font.Style;
      end;

      if Sender = miStyleBold then
         fontStyle := fsBold
      else if Sender = miStyleItalic then
         fontStyle := fsItalic
      else if Sender = miStyleUnderline then
         fontStyle := fsUnderline
      else if Sender = miStyleStrikeOut then
         fontStyle := fsStrikeOut;

      if Sender = miStyleNormal then
         fontStyles := []
      else if fontStyle in fontStyles then
         Exclude(fontStyles, fontStyle)
      else
         Include(fontStyles, fontStyle);

      if block <> nil then
      begin
         block.SetFontStyle(fontStyles);
         if (Sender = miStyleStrikeOut) and not block.SkipUpdateEditor then
            TInfra.UpdateCodeEditor;
      end
      else if comment <> nil then
         comment.Font.Style := fontStyles;
      GProject.SetChanged;
   end;
end;

procedure TMainForm.miFontSizeClick(Sender: TObject);
begin
   var comp := pmPages.PopupComponent;
   if (comp is TBlock) or (comp is TComment) then
   begin
      var fontSize := StripHotKey(TMenuItem(Sender).Caption).ToInteger;
      if comp is TBlock then
         TBlock(comp).SetFontSize(fontSize)
      else if comp is TComment then
         TComment(comp).Font.Size := fontSize;
      GProject.SetChanged;
   end;
end;

procedure TMainForm.miCopyClick(Sender: TObject);
begin
   var comp := pmPages.PopupComponent;
   if (comp is TCustomEdit) and (TCustomEdit(comp).SelLength > 0) then
   begin
      if Sender = miCopy then
         TCustomEdit(comp).CopyToClipboard
      else if Sender = miCut then
         TCustomEdit(comp).CutToClipboard;
   end
   else if (comp is TBlock) or (comp is TComment) then
   begin
      if Sender = miCut then
         miRemove.Click;
      GClpbrd.Instance := TControl(comp);
      Clipboard.Clear;
   end;
end;

procedure TMainForm.miRemoveClick(Sender: TObject);
begin
   var comp := pmPages.PopupComponent;
   if (comp = GClpbrd.Instance) or (GClpbrd.UndoObject = GClpbrd.Instance) then
      GClpbrd.Instance := nil;
   if comp is TBlock then
      TBlock(comp).Remove
   else if comp is TComment then
   begin
      if TComment(comp).SelLength > 0 then
         TComment(comp).SelText := ''
      else
         comp.Free;
   end;
   GProject.SetChanged;
end;

procedure TMainForm.miSubRoutinesClick(Sender: TObject);
begin
   var form: TForm := nil;
   if Sender = miSubRoutines then
      form := TInfra.GetFunctionsForm
   else if Sender = miToolbox then
      form := ToolboxForm
   else if Sender = miDeclarations then
      form := DeclarationsForm
   else if Sender = miExplorer then
      form := TInfra.GetExplorerForm
   else if Sender = miDataTypes then
      form := TInfra.GetDataTypesForm
   else if Sender = miNavigator then
      form := NavigatorForm;
   if form <> nil then
      form.Visible := not form.Visible
end;

procedure TMainForm.miExportClick(Sender: TObject);
var
   exp: IExportable;
begin
   if Supports(pmPages.PopupComponent, IExportable, exp) then
      TInfra.ExportToFile(exp);
end;

procedure TMainForm.miImportClick(Sender: TObject);
var
   comp: TComponent;
   impProc: TXMLImportProc;
begin
   if GProject <> nil then
   begin
      comp := pmPages.PopupComponent;
      if (comp is TBlock) and (TBlock(comp).Ired >= 0) then
         impProc := TBlock(comp).ImportFromXMLTag
      else
         impProc := GProject.ImportUserFunctionsFromXML;
      if not TXMLProcessor.ImportFromXMLFile(impProc, impSelectPopup).IsEmpty then
         TInfra.UpdateCodeEditor;
   end;
end;

procedure TMainForm.miFrameClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TBlock then
      TBlock(pmPages.PopupComponent).ChangeFrame;
end;

procedure TMainForm.miFoldUnfoldClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      var block := TGroupBlock(pmPages.PopupComponent);
      block.DeSelect;
      block.ExpandFold(true);
   end;
end;

procedure TMainForm.miAddBranchClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      var caseBlock := TCaseBlock(pmPages.PopupComponent);
      var i := IfThen(Sender = miInsertBranch, caseBlock.Ired, -1);
      TInfra.UpdateCodeEditor(caseBlock.InsertNewBranch(i));
   end;
end;

procedure TMainForm.miRemoveBranchClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      var res := IDYES;
      if GSettings.ConfirmRemove then
         res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
      if res = IDYES then
      begin
         var caseBlock := TCaseBlock(pmPages.PopupComponent);
         caseBlock.RemoveBranch(caseBlock.Ired);
         TInfra.UpdateCodeEditor(caseBlock.Branch);
      end;
   end;
end;

procedure TMainForm.miUnfoldAllClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      var block := TGroupBlock(pmPages.PopupComponent);
      block.DeSelect;
      block.TopParentBlock.LockDrawing;
      try
         block.ExpandAll;
      finally
         block.TopParentBlock.UnLockDrawing;
      end;
   end;
end;

procedure TMainForm.AcceptFile(const AFilePath: string);
begin
   Caption := PROJECT_CAPTION + AFilePath;
   GProject.Name := ChangeFileExt(ExtractFilename(AFilePath), '');
   FHistoryMenu.AddFile(AFilePath);
end;

procedure TMainForm.miPrint2Click(Sender: TObject);
begin
   if pmPages.PopupComponent is TBlock then
   begin
      var block := TBlock(pmPages.PopupComponent);
      var bitmap := TBitmap.Create;
      try
         block.ExportToGraphic(bitmap);
         TInfra.PrintBitmap(bitmap);
      finally
         bitmap.Free;
      end;
   end;
end;

procedure TMainForm.miProjectClick(Sender: TObject);
begin
   miAddMain.Enabled := GInfra.CurrentLang.EnabledMainProgram and (GProject <> nil) and (GProject.GetMainBlock = nil);
   miUndoRemove.Enabled := GClpbrd.UndoObject <> nil;
end;

procedure TMainForm.miAddMainClick(Sender: TObject);
begin
   if GProject <> nil then
   begin
      var body := TMainBlock.Create(GProject.ActivePage, GetMainBlockNextTopLeft);
      TUserFunction.Create(nil, body);
      TInfra.UpdateCodeEditor(body);
   end;
end;

procedure TMainForm.miNewFlowchartClick(Sender: TObject);
begin
   if GProject <> nil then
   begin
      var page := GProject.ActivePage;
      var mainBlock := TMainBlock.Create(page, page.Box.ScreenToClient(page.Box.PopupMenu.PopupPoint));
      mainBlock.OnResize(mainBlock);
      TUserFunction.Create(nil, mainBlock);
      GProject.SetChanged;
   end;
end;

procedure TMainForm.miForAscClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TForDoBlock then
      TForDoBlock(pmPages.PopupComponent).DescOrder := Sender = miForDesc;
end;

procedure TMainForm.miMemoEditClick(Sender: TObject);
var
   memoEx: IMemoEx;
   memo: TMemoEx;
begin
   if Supports(pmPages.PopupComponent, IMemoEx, memoEx) then
   begin
      memo := memoEx.GetMemoEx;
      if memo <> nil then
      begin
         MemoEditorForm.Source := memo;
         MemoEditorForm.ShowModal;
      end;
   end;
end;

procedure TMainForm.miMemoVScrollClick(Sender: TObject);
var
   memoEx: IMemoEx;
   memo: TMemoEx;
begin
   if Supports(pmPages.PopupComponent, IMemoEx, memoEx) then
   begin
      memo := memoEx.GetMemoEx;
      if memo <> nil then
      begin
         if Sender = miMemoVScroll then
            memo.HasVScroll := miMemoVScroll.Checked
         else if Sender = miMemoHScroll then
            memo.HasHScroll := miMemoHScroll.Checked
         else if Sender = miMemoWordWrap then
            memo.WordWrap := miMemoWordWrap.Checked
         else if Sender = miMemoAlignRight then
         begin
            if miMemoAlignRight.Checked then
               memo.Alignment := taRightJustify
            else
               memo.Alignment := taLeftJustify;
         end;
      end;
   end;
end;

procedure TMainForm.miNewFunctionClick(Sender: TObject);
begin
   if GProject <> nil then
   begin
      var box := GProject.ActivePage.Box;
      TInfra.GetFunctionsForm.AddUserFunction(box.ScreenToClient(box.PopupMenu.PopupPoint));
   end;
end;

procedure TMainForm.pgcPagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
   if (GProject <> nil) and (htOnItem in pgcPages.GetHitTestInfoAt(MousePos.X, MousePos.Y)) then
   begin
      var page := TInfra.GetPageFromXY(pgcPages, MousePos.X, MousePos.Y);
      if page <> nil then
      begin
         pmTabs.PopupComponent := page;
         pgcPages.ActivePageIndex := page.PageIndex;
      end;
      var p := pgcPages.ClientToScreen(MousePos);
      pmTabs.Popup(p.X, p.Y);
   end
end;

procedure TMainForm.pmTabsPopup(Sender: TObject);
begin
   miRemovePage.Enabled := false;
   miRenamePage.Enabled := false;
   miAddPage.Enabled := false;
   if pmTabs.PopupComponent is TBlockTabSheet then
   begin
      miRemovePage.Enabled := not TBlockTabSheet(pmTabs.PopupComponent).IsMain;
      miRenamePage.Enabled := miRemovePage.Enabled;
      miAddPage.Enabled := true;
   end;
end;

procedure TMainForm.miRemovePageClick(Sender: TObject);
begin
   var res := IDYES;
   if GSettings.ConfirmRemove then
      res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
   if res = IDYES then
   begin
      pmTabs.PopupComponent.Free;
      NavigatorForm.Invalidate;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TMainForm.pgcPagesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then
   begin
      var page := TInfra.GetPageFromXY(pgcPages, X, Y);
      if page <> nil then
         page.BeginDrag(false, 3);
   end;
end;

procedure TMainForm.pgcPagesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if not (Source is TBlockTabSheet) then
      Accept := false;
end;

procedure TMainForm.pgcPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
   var page := TInfra.GetPageFromXY(pgcPages, X, Y);
   if page <> nil then
   begin
      var idx := page.PageIndex;
      page.PageIndex := TTabSheet(Source).PageIndex;
      TTabSheet(Source).PageIndex := idx;
      GProject.SetChanged;
   end;
end;

procedure TMainForm.miRenamePageClick(Sender: TObject);
begin
   if pmTabs.PopupComponent is TTabSheet then
   begin
      var page := TTabSheet(pmTabs.PopupComponent);
      var lCaption := InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), page.Caption).Trim;
      if (lCaption <> '') and (TInfra.FindDuplicatedPage(page, lCaption) = nil) then
      begin
         page.Caption := lCaption;
         GProject.UpdateHeadersBody(page);
         GProject.SetChanged;
      end;
   end;
end;

procedure TMainForm.miAddPageClick(Sender: TObject);
begin
   var lCaption := InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), '').Trim;
   if (lCaption <> '') and (GProject.GetPage(lCaption, false) = nil) then
   begin
      var page := GProject.GetPage(lCaption);
      page.PageControl.ActivePage := page;
      NavigatorForm.Invalidate;
      GProject.SetChanged;
   end;
end;

procedure TMainForm.pgcPagesChange(Sender: TObject);
begin
   NavigatorForm.Invalidate;
end;

procedure TMainForm.pmEditsPopup(Sender: TObject);
begin
   miUndo.Enabled := false;
   miCut1.Enabled := false;
   miCopy1.Enabled := false;
   miPaste1.Enabled := false;
   miRemove1.Enabled := false;
   miInsertFunc.Visible := false;
   if pmEdits.PopupComponent is TCustomEdit then
   begin
      var edit := TCustomEdit(pmEdits.PopupComponent);
      miUndo.Enabled := edit.CanUndo;
      miCut1.Enabled := not edit.SelText.IsEmpty;
      miCopy1.Enabled := miCut1.Enabled;
      miRemove1.Enabled := miCut1.Enabled;
      miPaste1.Enabled := Clipboard.HasFormat(CF_TEXT);
      miInsertFunc.Visible := BuildFuncMenu > 0;
   end;
end;

procedure TMainForm.pmEditsMenuClick(Sender: TObject);
begin
   if pmEdits.PopupComponent is TCustomEdit then
   begin
      var edit := TCustomEdit(pmEdits.PopupComponent);
      if Sender = miUndo then
         edit.Undo
      else if Sender = miCut1 then
         edit.CutToClipboard
      else if Sender = miCopy1 then
         edit.CopyToClipboard
      else if Sender = miPaste1 then
         edit.PasteFromClipboard
      else if Sender = miRemove1 then
         edit.SelText := '';
   end;
end;

procedure TMainForm.FuncMenuClick(Sender: TObject);
var
   funcName, lBrackets, backup, lLibrary: string;
   edit: TCustomEdit;
   menuItem: TMenuItem;
   cursorPos, selPos: integer;
begin
   if (Sender is TMenuItem) and (pmEdits.PopupComponent is TCustomEdit) then
   begin
      edit := TCustomEdit(pmEdits.PopupComponent);
      menuItem := TMenuItem(Sender);
      funcName := StripHotKey(menuItem.Caption);
      backup := '';
      selPos := edit.SelStart;
      if menuItem.Tag <> 0 then
      begin
         with PNativeFunction(menuItem.Tag)^ do
         begin
            if not Caption.IsEmpty then
               funcName := Name;
            cursorPos := BracketsCursorPos;
            lBrackets := Brackets;
            lLibrary := Lib;
         end;
      end
      else
      begin
         cursorPos := GInfra.CurrentLang.FuncBracketsCursorPos;
         lBrackets := GInfra.CurrentLang.FuncBrackets;
         lLibrary := menuItem.Name;
      end;
      if Clipboard.HasFormat(CF_TEXT) then
         backup := Clipboard.AsText;
      Clipboard.AsText := funcName + lBrackets;
      edit.PasteFromClipboard;
      if cursorPos <> 0 then
         edit.SelStart := cursorPos + Length(funcName) + selPos;
      if not backup.IsEmpty then
         Clipboard.AsText := backup;
      if GSettings.UpdateEditor and not lLibrary.IsEmpty then
         TInfra.GetEditorForm.InsertLibraryEntry(lLibrary);
   end;
end;

function TMainForm.BuildFuncMenu: integer;
var
   i: integer;
   lName: string;
   func: TUserFunction;
   nativeFunc: PNativeFunction;
   lang: TLangDefinition;
   mItem: TMenuItem;
   mItems: array of TMenuItem;
begin
   miInsertFunc.Clear;
   for func in GProject.GetUserFunctions do
   begin
      lName := func.GetName;
      if not lName.IsEmpty then
      begin
         mItem := TMenuItem.Create(miInsertFunc);
         mItem.Caption := lName;
         if Assigned(GInfra.CurrentLang.GetUserFuncDesc) then
            lang := GInfra.CurrentLang
         else
            lang := GInfra.TemplateLang;
         mItem.Hint := lang.GetUserFuncDesc(func.Header);
         if func.Header <> nil then
            mItem.Name := Trim(func.Header.edtLibrary.Text);
         mItem.OnClick := FuncMenuClick;
         mItems := mItems + [mItem];
      end;
   end;
   for i := 0 to High(GInfra.CurrentLang.NativeFunctions) do
   begin
      mItem := TMenuItem.Create(miInsertFunc);
      nativeFunc := @GInfra.CurrentLang.NativeFunctions[i];
      mItem.Caption := IfThen(nativeFunc.Caption.IsEmpty, nativeFunc.Name, nativeFunc.Caption);
      mItem.Hint := nativeFunc.Hint;
      mItem.Tag := NativeInt(nativeFunc);
      mItem.OnClick := FuncMenuClick;
      mItems := mItems + [mItem];
   end;
   result := Length(mItems);
   if result > 0 then
   begin
      if result > 1 then
         TArray.Sort<TMenuItem>(mItems, ByCaptionMenuItemComparer);
      miInsertFunc.Add(mItems);
   end;
end;

procedure TMainForm.miIsHeaderClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TComment then
   begin
      TComment(pmPages.PopupComponent).IsHeader := not miIsHeader.Checked;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TMainForm.miPasteTextClick(Sender: TObject);
begin
   if Clipboard.HasFormat(CF_TEXT) and (pmPages.PopupComponent is TCustomEdit) then
      TCustomEdit(pmPages.PopupComponent).PasteFromClipboard;
end;

procedure TMainForm.PopupMenuClosed;
begin
   if pmPages.PopupComponent is TBlock then
      TBlock(pmPages.PopupComponent).OnMouseLeave(false);
end;

procedure TMainForm.UpdateTabsColor(AColor: TColor);
begin
   for var i := 0 to pgcPages.PageCount-1 do
      TBlockTabSheet(pgcPages.Pages[i]).Box.Color := AColor;
end;

procedure TPopupListEx.WndProc(var msg: TMessage);
begin
   if (msg.Msg = WM_UNINITMENUPOPUP) and (Screen.ActiveForm is TMainForm) then
   begin
      var mform := TMainForm(Screen.ActiveForm);
      if msg.WParam = mform.pmPages.Handle then
         TThread.ForceQueue(nil, mForm.PopupMenuClosed);
   end;
   inherited;
end;

initialization

   PopupList.Free;
   PopupList := TPopupListEx.Create;

   ByCaptionMenuItemComparer := TDelegatedComparer<TMenuItem>.Create(
      function(const L, R: TMenuItem): integer
      begin
         result := TComparer<string>.Default.Compare(StripHotKey(L.Caption), StripHotKey(R.Caption));
      end
   );

end.


