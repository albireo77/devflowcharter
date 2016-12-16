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
  Windows, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Menus, Printers,
  ImgList, Clipbrd, Types, OmniXML, SysUtils, Classes, ShellApi, StrUtils, Base_Form,
  Messages, History, Dialogs, ComCtrls, CommonInterfaces;

type

  TClockPos = (cp12, cp3, cp6, cp9);

  TMainForm = class(TBaseForm)
    pmPages: TPopupMenu;
    ExportDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    miAssign: TMenuItem;
    miMultipleAssign: TMenuItem;
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
    miSize8: TMenuItem;
    miSize10: TMenuItem;
    miSize12: TMenuItem;
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
    miInstr: TMenuItem;
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
    miExpFold: TMenuItem;
    miAddBranch: TMenuItem;
    miCase: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    miRemoveBranch: TMenuItem;
    miStyleStrikeOut: TMenuItem;
    miReturn: TMenuItem;
    miExpandAll: TMenuItem;
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
    procedure miAssignClick(Sender: TObject);
    procedure miStyleBoldClick(Sender: TObject);
    procedure miSize8Click(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miRemoveClick(Sender: TObject);
    procedure miSubRoutinesClick(Sender: TObject);
    procedure miExportClick(Sender: TObject);
    procedure miImportClick(Sender: TObject);
    procedure miFrameClick(Sender: TObject);
    procedure miExpFoldClick(Sender: TObject);
    procedure miAddBranchClick(Sender: TObject);
    procedure miRemoveBranchClick(Sender: TObject);
    procedure miExpandAllClick(Sender: TObject);
    procedure Localize(const AList: TStringList); override;
    procedure ResetForm; override;
    procedure SetMenu(const AEnabled: boolean);
    procedure miPrint2Click(Sender: TObject);
    procedure miProjectClick(Sender: TObject);
    procedure miAddMainClick(Sender: TObject);
    procedure SetScrollBars;
    procedure miNewFlowchartClick(Sender: TObject);
    procedure ScrollV(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure ScrollH(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miForAscClick(Sender: TObject);
    procedure miMemoEditClick(Sender: TObject);
    procedure miMemoVScrollClick(Sender: TObject);
    procedure PerformFormsRepaint;
    procedure miNewFunctionClick(Sender: TObject);
    procedure AutoScrollInView(AControl: TControl); override;
    procedure pgcPagesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure pmTabsPopup(Sender: TObject);
    procedure miRemovePageClick(Sender: TObject);
    procedure pgcPagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pgcPagesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pgcPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure miRenamePageClick(Sender: TObject);
    procedure miAddPageClick(Sender: TObject);
    procedure pgcPagesChange(Sender: TObject);
    procedure pmEditsPopup(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miCut1Click(Sender: TObject);
    procedure miCopy1Click(Sender: TObject);
    procedure miPaste1Click(Sender: TObject);
    procedure FuncMenuClick(Sender: TObject);
    procedure miRemove1Click(Sender: TObject);
    procedure miIsHeaderClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure miPasteTextClick(Sender: TObject);
  private
    { Private declarations }
    FClockPos: TClockPos;
    FHistoryMenu: THistoryMenu;
    function BuildFuncMenu(AParent: TMenuItem): integer;
    procedure DestroyFuncMenu;
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
    function ConfirmSave: integer;
    function GetDisplayedRect: TRect;
    function GetMainBlockNextTopLeft: TPoint;
    procedure AcceptFile(const AFilePath: string);
  end;

var
  MainForm: TMainForm;
  FFuncMenu: array of TMenuItem;

implementation

uses
   Toolbox_Form, ApplicationCommon, About_Form, Main_Block, ParseGlobals, LocalizationManager,
   XMLProcessor, UserFunction, ForDo_Block, Return_Block, Project, Declarations_Form,
   Base_Block, Comment, Case_Block, Navigator_Form, CommonTypes, LangDefinition,
   EditMemo_Form, BlockFactory, BlockTabSheet;

type
   TDerivedControl = class(TControl);

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
const
   CursorIdsArray: array[TCustomCursor] of PChar = (' ', 'IFELSE', 'FOR', 'REPEAT',
                   'WHILE', 'ASSIGN', 'MULTIASSIGN', 'IF', 'SUBROUTINE', 'INPUT', 'OUTPUT',
                   'CASE', 'RETURN', 'TEXT', 'FOLDER');
var
   lCursor: TCustomCursor;
begin
   lCursor := crNormal;
   repeat
      Inc(lCursor);
      Screen.Cursors[Ord(lCursor)] := LoadCursor(HInstance, CursorIdsArray[lCursor]);
   until lCursor = High(TCustomCursor);
   InitialiseVariables;
   DecimalSeparator := '.';
   SystemParametersInfo(SPI_SETDRAGFULLWINDOWS, Ord(True), nil, 0);
   Application.HintHidePause := HINT_PAUSE;
   Application.OnException := OnException;
   Application.Title := PROGRAM_NAME;
   Caption := PROGRAM_NAME;
   FHistoryMenu := THistoryMenu.Create(miReopen, miOpen.OnClick);
   FHistoryMenu.Load;
   pgcPages.DoubleBuffered := true;
   FClockPos := Low(TClockPos);
end;

procedure TMainForm.ScrollV(var Msg: TWMVScroll);
begin
   inherited;
   PerformFormsRepaint;
end;

procedure TMainForm.ScrollH(var Msg: TWMHScroll);
begin
   inherited;
   PerformFormsRepaint;
end;

procedure TMainForm.ResetForm;
begin
   miUndoRemove.Enabled := false;
   VertScrollBar.Range := ClientHeight;
   HorzScrollBar.Range := ClientWidth;
   VertScrollbar.Position := 0;
   HorzScrollBar.Position := 0;
   Caption := PROGRAM_NAME;
   FClockPos := Low(TClockPos);
   SetMenu(false);
   DestroyFuncMenu;
   while pgcPages.PageCount > 0 do
      pgcPages.Pages[0].Free;
end;

procedure TMainForm.SetMenu(const AEnabled: boolean);
begin
   miSave.Enabled      := AEnabled;
   miSaveAs.Enabled    := AEnabled;
   miClose.Enabled     := AEnabled;
   miPrint.Enabled     := AEnabled;
   miToolbox.Enabled   := AEnabled;
   miNavigator.Enabled := AEnabled;
   if AEnabled then
   begin
      miSubRoutines.Enabled := GInfra.CurrentLang.EnabledUserFunctionHeader;
      miDataTypes.Enabled := GInfra.CurrentLang.EnabledUserDataTypes;
      miDeclarations.Enabled := GInfra.CurrentLang.EnabledConsts or GInfra.CurrentLang.EnabledVars;
      miExplorer.Enabled := GInfra.CurrentLang.EnabledExplorer;
      miGenerate.Enabled := GInfra.CurrentLang.EnabledCodeGenerator;
      miAddMain.Enabled := GInfra.CurrentLang.EnabledMainProgram;
   end
   else
   begin
      miDeclarations.Enabled := false;
      miSubRoutines.Enabled := false;
      miDataTypes.Enabled := false;
      miGenerate.Enabled := false;
      miExplorer.Enabled := false;
      miAddMain.Enabled := false;
   end;
end;

// don't remove this method
procedure TMainForm.AutoScrollInView(AControl: TControl);
begin
   //inherited AutoScrollInView(AControl);
end;

procedure TMainForm.Localize(const AList: TStringList);
begin
   if GProject <> nil then
   begin
      GProject.RepaintFlowcharts;
      GProject.RefreshStatements;
   end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
   SetMenu(false);
   Color := GSettings.DesktopColor;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FHistoryMenu.Save;
   GClpbrd.UndoObject.Free;
   GProject.Free;
   GProject := nil;
   if GSettings <> nil then
      GSettings.WriteToRegistry;
   GSettings.Free;
   GSettings := nil;
   FHistoryMenu.Free;
   i18Manager.Free;
   i18Manager := nil;
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

procedure TMainForm.miNewClick(Sender: TObject);
var
   lBlock: TMainBlock;
begin
   if GChange = 1 then
   begin
      case ConfirmSave of
         IDYES: miSave.Click;
         IDCANCEL: exit;
      end;
   end;
   TInfra.SetInitialSettings;
   GProject := TProject.GetInstance;
   lBlock := TMainBlock.Create(GProject.GetMainPage, GetMainBlockNextTopLeft);
   lBlock.OnResize(lBlock);
   TUserFunction.Create(nil, lBlock);
   ExportDialog.FileName := '';
end;

procedure TMainForm.miOpenClick(Sender: TObject);
var
   tmpCursor: TCursor;
   filePath: string;
begin
    if (GChange = 1) and (GProject <> nil) then
    begin
       case ConfirmSave of
          IDYES: miSave.Click;
          IDCANCEL: exit;
       end;
    end;
    filePath := '';
    if Sender <> miOpen then
       filePath := StripHotKey(TMenuItem(Sender).Caption);
    TInfra.SetInitialSettings;
    tmpCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    GProject := TProject.GetInstance;
    filePath := TXMLProcessor.ImportFromXMLFile(GProject.ImportFromXMLTag, filePath);
    Screen.Cursor := tmpCursor;
    if filePath <> '' then
       AcceptFile(filePath)
    else
       TInfra.SetInitialSettings;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
begin
   TInfra.ExportToFile(GProject);
end;

procedure TMainForm.miGenerateClick(Sender: TObject);
var
   form: TForm;
begin
   form := TInfra.GetEditorForm;
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

procedure TMainForm.miCloseClick(Sender: TObject);
begin
   if GChange = 1 then
   begin
      case ConfirmSave of
         IDYES: miSave.Click;
         IDCANCEL: exit;
      end;
   end;
   TInfra.SetInitialSettings;
end;

procedure TMainForm.miSaveClick(Sender: TObject);
begin
    if Caption = PROGRAM_NAME then
       miSaveAs.Click
    else if (GProject <> nil) and (GProject.ExportToXMLFile(AnsiReplaceText(Caption, MAIN_FORM_CAPTION, '')) = errNone) then
       GChange := 0;
end;

procedure TMainForm.miPrintClick(Sender: TObject);
var
   bitmap: TBitmap;
begin
   bitmap := TBitmap.Create;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if (GChange = 1) and (GProject <> nil) then
   begin
      case ConfirmSave of
         IDYES: miSave.Click;
         IDCANCEL: CanClose := false;
      end;
   end;
end;

procedure TMainForm.OnException(Sender: TObject; E: Exception);
var
   msg: array[0..255] of char;
begin
   if (ExceptAddr = nil) or (ExceptionErrorMessage(E, ExceptAddr, msg, SizeOf(msg)) = 0) then
      msg := '';
   if msg <> '' then
   begin
      if GProject <> nil then
      begin
         TInfra.ShowFormattedErrorBox('OtherException', [msg], errGeneral);
         miSaveAs.Click;
      end
      else
         TInfra.ShowErrorBox(msg, errGeneral);
   end;
   TInfra.SetInitialSettings;
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
         not block.ParentBlock.Expanded or ((block is TReturnBlock) and (block.ParentBranch.FindInstanceOf(TReturnBlock) <> -1)) then exit;
      block.ParentBranch.UndoRemove(block);
      block.ParentBlock.ResizeWithDrawLock;
      block.SetVisible(true);
      NavigatorForm.Invalidate;
   end
   else if Supports(GClpbrd.UndoObject, IXMLable, xmlObj) then
      xmlObj.Active := true;
   TInfra.UpdateCodeEditor(GClpbrd.UndoObject);
   GClpbrd.UndoObject := nil;
   miUndoRemove.Enabled := false;
end;

procedure TMainForm.pmPagesPopup(Sender: TObject);
var
   comp: TComponent;
   block: TBlock;
   lFont: TFont;
   isFunction: boolean;
begin
   lFont := nil;
   miFont.Visible := False;
   miCopy.Visible := False;
   miCut.Visible := False;
   miRemove.Visible := False;
   miInsert.Visible := False;
   miPaste.Enabled := False;
   miPasteText.Visible := False;
   miInstr.Enabled := False;
   miLoop.Enabled := False;
   miFrame.Visible := False;
   miFrame.Checked := False;
   miExport.Visible := False;
   miImport.Visible := True;
   miExpFold.Visible := False;
   miAddBranch.Visible := False;
   miRemoveBranch.Visible := False;
   miText.Enabled := False;
   miFolder.Enabled := False;
   miExpFold.Caption := i18Manager.GetString('miFoldBlock');
   miReturn.Enabled := False;
   miExpandAll.Visible := False;
   miPrint2.Visible := False;
   miNewFlowchart.Visible := GInfra.CurrentLang = GInfra.DummyLang;
   miNewFunction.Visible := GInfra.CurrentLang.EnabledUserFunctionHeader and GInfra.CurrentLang.EnabledUserFunctionBody;
   miForAsc.Visible := False;
   miForDesc.Visible := False;
   miMemo.Visible := False;
   miStyleBold.Checked := False;
   miStyleItalic.Checked := False;
   miStyleUnderline.Checked := False;
   miStyleStrikeOut.Checked := False;
   miStyleNormal.Checked := False;
   miSize8.Checked := False;
   miSize10.Checked := False;
   miSize12.Checked := False;
   miIsHeader.Visible := False;

   comp := pmPages.PopupComponent;
   isFunction := TInfra.IsValid(GClpbrd.UndoObject) and (GClpbrd.UndoObject is TUserFunction);

   miPaste.Enabled := TInfra.IsValid(GClpbrd.Instance) or isFunction;

   if comp is TBlock then
   begin
       block := TBlock(comp);
       lFont := block.GetFont;
       if block.Ired >= 0 then
       begin
          miInsert.Visible := True;
          miInstr.Enabled := True;
          miLoop.Enabled := True;
          miText.Enabled := True;
          miFolder.Enabled := True;
          miReturn.Enabled := block.CanInsertReturnBlock;
          if (GClpbrd.Instance is TComment) or ((GClpbrd.Instance is TReturnBlock) and not miReturn.Enabled) then
             miPaste.Enabled := False;
       end
       else if block.IsCursorSelect then
       begin
          miRemove.Visible := True;
          miFont.Visible := True;
          miMemoEdit.Visible := block.GetFrontMemo <> nil;
          if not (block is TReturnBlock) then
             miExport.Visible := True;
          miPrint2.Visible := True;
          miCut.Visible := true;
          if not (block is TMainBlock) then
             miCopy.Visible := True;
          if block is TGroupBlock then
          begin
             miExpFold.Visible := true;
             if TGroupBlock(block).Expanded then
             begin
                miExpFold.Caption := i18Manager.GetString('miFoldBlock');
                if block is TCaseBlock then
                   miAddBranch.Visible := True;
             end
             else
                miExpFold.Caption := i18Manager.GetString('miExpandBlock');
          end;
          if (block is TGroupBlock) and TGroupBlock(block).Expanded and TGroupBlock(block).HasFoldedBlocks then
             miExpandAll.Visible := true;
          if block is TForDoBlock then
          begin
             miForAsc.Visible := true;
             miForDesc.Visible := true;
             miForAsc.Checked := TForDoBlock(block).Order = ordAsc;
             miForDesc.Checked := not miForAsc.Checked;
          end;
          miMemo.Visible := block.GetFrontMemo <> nil;
          if miMemo.Visible then
          begin
             miMemoVScroll.Checked := block.MemoVScroll;
             miMemoHScroll.Checked := block.MemoHScroll;
             miMemoWordWrap.Checked := block.MemoWordWrap;
          end;
       end
       else
       begin
          miInsert.Visible := True;
          miComment.Enabled := True;
          if (GClpbrd.Instance is TBlock) and not isFunction then
             miPaste.Enabled := False;
       end;
       miFrame.Visible := True;
       miFrame.Checked := block.Frame;
       if (block is TCaseBlock) and (block.Ired > PRIMARY_BRANCH_IND) then
          miRemoveBranch.Visible := True;
   end
   else if comp is TComment then
   begin
      miPasteText.Visible := Clipboard.HasFormat(CF_TEXT);
      lFont := TComment(comp).Font;
      miRemove.Visible := True;
      miFont.Visible := True;
      miCopy.Visible := True;
      miCut.Visible := TComment(comp).SelLength > 0;
      miIsHeader.Visible := true;
      miIsHeader.Checked := TComment(comp).IsHeader;
      if GClpbrd.Instance is TBlock then
         miPaste.Enabled := false;
   end
   else
   begin
      miInsert.Visible := True;
      if GClpbrd.Instance is TMainBlock then
         miPaste.Enabled := true
      else if GClpbrd.Instance is TBlock then
         miPaste.Enabled := false;
   end;
   if lFont <> nil then
   begin
      miStyleBold.Checked := fsBold in lFont.Style;
      miStyleItalic.Checked := fsItalic in lFont.Style;
      miStyleUnderline.Checked := fsUnderline in lFont.Style;
      miStyleStrikeOut.Checked := fsStrikeOut in lFont.Style;
      miStyleNormal.Checked := lFont.Style = [];
      case lFont.Size of
         8:   miSize8.Checked := true;
         10:  miSize10.Checked := true;
         12:  miSize12.Checked := true;
      end;
   end;
end;

procedure TMainForm.miCommentClick(Sender: TObject);
var
   ptn: TPoint;
   page: TBlockTabSheet;
begin
   if GProject <> nil then
   begin
      page := GProject.GetActivePage;
      ptn := page.ScreenToClient(pmPages.PopupPoint);
      TComment.Create(page, ptn.X, ptn.Y, 150, 50);
      GChange := 1;
   end;
end;

procedure TMainForm.miAssignClick(Sender: TObject);
var
   newBlock, currBlock, srcBlock: TBlock;
   branch: TBranch;
   lParent: TGroupBlock;
   tmpCursor: TCursor;
   comment: TComment;
   topLeft: TPoint;
   blockType: TBlockType;
   lock: boolean;
   page: TBlockTabSheet;
   func: TUserFunction;
begin

   srcBlock := nil;
   lParent := nil;
   func := nil;
   comment := nil;

   if TInfra.IsValid(GClpbrd.UndoObject) and (GClpbrd.UndoObject is TUserFunction) then
      func := TUserFunction(GClpbrd.UndoObject)
   else if GClpbrd.Instance is TComment then
      comment := TComment(GClpbrd.Instance);

   if (Sender = miPaste) and ((func <> nil) or (comment <> nil)) then
   begin
      page := GProject.GetActivePage;
      topLeft := page.ScreenToClient(pmPages.PopupPoint);
      if func <> nil then
      begin
         if func.Body <> nil then
         begin
            func.Body.Page := page;
            func.Body.SetBounds(topLeft.X, topLeft.Y, func.Body.Width, func.Body.Height);
         end;
         miUndoRemoveClick(miUndoRemove);
      end
      else if comment <> nil then
         comment.Clone(page, @topLeft);
      GChange := 1;
      NavigatorForm.Invalidate;
      exit;
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
         lock := branch.ParentBlock.LockDrawing;
         try
            newBlock := nil;
            blockType := blUnknown;
            if Sender = miAssign then
               blockType := blAssign
            else if Sender = miMultipleAssign then
               blockType := blMultAssign
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
            else if (Sender = miPaste) and TInfra.IsValid(GClpbrd.Instance) and (GClpbrd.Instance is TBlock) then
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
                  newBlock.RefreshStatements;
               end;
               if srcBlock <> nil then
                  newBlock.CloneComments(srcBlock);
               TInfra.UpdateCodeEditor(newBlock);
            end;
         finally
            if lock then
               branch.ParentBlock.UnLockDrawing;
         end;
      end;
   end;
   NavigatorForm.Invalidate;
end;

function TMainForm.ConfirmSave: integer;
begin
   result := IDCANCEL;
   if GProject <> nil then
      result := TInfra.ShowFormattedQuestionBox('ConfirmClose', [GProject.Name]);
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
      GChange := 1;
   end;
end;

procedure TMainForm.miSize8Click(Sender: TObject);
var
   comp: TComponent;
   fontSize: integer;
begin
   comp := pmPages.PopupComponent;
   if (comp is TBlock) or (comp is TComment) then
   begin
      if Sender = miSize10 then
         fontSize := 10
      else if Sender = miSize12 then
         fontSize := 12
      else
         fontSize := 8;
      if comp is TBlock then
         TBlock(comp).SetFontSize(fontSize)
      else if comp is TComment then
         TComment(comp).Font.Size := fontSize;
      GChange := 1;
   end;
end;

procedure TMainForm.miCopyClick(Sender: TObject);
var
   comp: TComponent;
begin
   comp := pmPages.PopupComponent;
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
var
   comp: TComponent;
begin
   comp := pmPages.PopupComponent;
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
   GChange := 1;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   w: Word;
begin
   if WheelDelta < 0 then
      w := VK_DOWN
   else
      w := VK_UP;
   FormKeyDown(Self, w, [ssCtrl]);
end;

procedure TMainForm.miSubRoutinesClick(Sender: TObject);
begin
   if Sender = miSubRoutines then
      TInfra.GetFunctionsForm.Visible := not TInfra.GetFunctionsForm.Visible
   else if Sender = miToolbox then
      ToolboxForm.Visible := not ToolboxForm.Visible
   else if Sender = miDeclarations then
      DeclarationsForm.Visible := not DeclarationsForm.Visible
   else if Sender = miExplorer then
      TInfra.GetExplorerForm.Visible := not TInfra.GetExplorerForm.Visible
   else if Sender = miDataTypes then
      TInfra.GetDataTypesForm.Visible := not TInfra.GetDataTypesForm.Visible
   else if Sender = miNavigator then
      NavigatorForm.Visible := not NavigatorForm.Visible
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
   ptn: TPoint;
   func: TUserFunction;
   impProc: TImportProc;
   impFunc: boolean;
begin
   if GProject <> nil then
   begin
      impFunc := false;
      comp := pmPages.PopupComponent;
      if (comp is TBlock) and (TBlock(comp).Ired >= 0) then
         impProc := TBlock(comp).ImportFromXMLTag
      else
      begin
         impProc := GProject.ImportUserFunctionsFromXML;
         impFunc := true;
      end;
      if TXMLProcessor.ImportFromXMLFile(impProc) <> '' then
      begin
         if impFunc then
         begin
            func := GProject.LastUserFunction;
            if (func <> nil) and func.Active and (func.Body <> nil) and func.Body.Visible then
            begin
               ptn := GProject.GetActivePage.ScreenToClient(pmPages.PopupPoint);
               func.Body.Left := ptn.X;
               func.Body.Top := ptn.Y;
               func.Body.Page.Form.SetScrollBars;
            end;
         end;
         TInfra.UpdateCodeEditor;
      end;
   end;
end;

procedure TMainForm.miFrameClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TBlock then
      TBlock(pmPages.PopupComponent).ChangeFrame;
end;

procedure TMainForm.miExpFoldClick(Sender: TObject);
var
   block: TGroupBlock;
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      block := TGroupBlock(pmPages.PopupComponent);
      block.ClearSelection;
      block.ExpandFold(true);
   end;
end;

procedure TMainForm.miAddBranchClick(Sender: TObject);
var
   ptn: TPoint;
   caseBlock: TCaseBlock;
   branch: TBranch;
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      caseBlock := TCaseBlock(pmPages.PopupComponent);
      ptn := Point(caseBlock.GetBranch(caseBlock.BranchCount).GetMostRight+60, caseBlock.Height-32);
      branch := caseBlock.AddBranch(ptn, true);
      TInfra.UpdateCodeEditor(branch);
   end;
end;

procedure TMainForm.miRemoveBranchClick(Sender: TObject);
var
   res: integer;
   caseBlock: TCaseBlock;
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      res := IDYES;
      if GSettings.ConfirmRemove then
         res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
      if res = IDYES then
      begin
         caseBlock := TCaseBlock(pmPages.PopupComponent);
         caseBlock.RemoveBranch;
         TInfra.UpdateCodeEditor(caseBlock.Branch);
      end;
   end;
end;

procedure TMainForm.miExpandAllClick(Sender: TObject);
var
   block: TGroupBlock;
   lock: boolean;
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      block := TGroupBlock(pmPages.PopupComponent);
      block.ClearSelection;
      lock := block.LockDrawing;
      try
         block.ExpandAll;
      finally
         if lock then
            block.UnLockDrawing;
      end;
   end;
end;

procedure TMainForm.AcceptFile(const AFilePath: string);
begin
   Caption := MAIN_FORM_CAPTION + AFilePath;
   GProject.Name := ChangeFileExt(ExtractFilename(AFilePath), '');
   FHistoryMenu.AddFile(AFilePath);
   GChange := 0;
end;

procedure TMainForm.ExportSettingsToXMLTag(const root: IXMLElement);
begin
   root.SetAttribute('scrollrange_h', IntToStr(HorzScrollBar.Range));
   root.SetAttribute('scrollrange_v', IntToStr(VertScrollBar.Range));
   root.SetAttribute('scroll_h', IntToStr(HorzScrollBar.Position));
   root.SetAttribute('scroll_v', IntToStr(VertScrollBar.Position));
end;

procedure TMainForm.ImportSettingsFromXMLTag(const root: IXMLElement);
var
   val: integer;
begin
   val := StrToIntDef(root.GetAttribute('scrollrange_h'), -1);
   if val > -1 then
      HorzScrollBar.Range := val;
   val := StrToIntDef(root.GetAttribute('scroll_h'), -1);
   if val > -1 then
      HorzScrollBar.Position := val;
   val := StrToIntDef(root.GetAttribute('scrollrange_v'), -1);
   if val > -1 then
      VertScrollBar.Range := val;
   val := StrToIntDef(root.GetAttribute('scroll_v'), -1);
   if val > -1 then
      VertScrollBar.Position := val;
end;

procedure TMainForm.miPrint2Click(Sender: TObject);
var
   bitmap: TBitmap;
   block: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      block := TBlock(pmPages.PopupComponent);
      bitmap := TBitmap.Create;
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
var
   body: TMainBlock;
begin
   if GProject <> nil then
   begin
      body := TMainBlock.Create(GProject.GetActivePage, GetMainBlockNextTopLeft);
      TUserFunction.Create(nil, body);
      TInfra.UpdateCodeEditor(body);
   end;
end;

procedure TMainForm.SetScrollBars;
var
   ptn: TPoint;
begin
   if GProject <> nil then
   begin
      ptn := GProject.GetBottomRight;
      if ptn.X > ClientWidth then
         HorzScrollBar.Range := ptn.X
      else
         HorzScrollBar.Range := ClientWidth;
      if ptn.Y > ClientHeight then
         VertScrollBar.Range := ptn.Y
      else
         VertScrollBar.Range := ClientHeight;
      NavigatorForm.Invalidate;
   end;
end;

function TMainForm.GetDisplayedRect: TRect;
begin
   with result do
   begin
      Left := HorzScrollBar.Position;
      Top := VertScrollBar.Position;
      Right := Left + ClientWidth;
      Bottom := Top + ClientHeight;
   end;
end;

procedure TMainForm.miNewFlowchartClick(Sender: TObject);
var
   mainBlock: TMainBlock;
   page: TBlockTabSheet;
begin
   if GProject <> nil then
   begin
      page := GProject.GetActivePage;
      mainBlock := TMainBlock.Create(page, page.ScreenToClient(pmPages.PopupPoint));
      mainBlock.OnResize(mainBlock);
      TUserFunction.Create(nil, mainBlock);
      GChange := 1;
   end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const
   STEP = 15;
   MULTP = 20;
var
   y: integer;
begin
   y := 0;
   if Sender = Self then
   begin
      if ssCtrl in Shift then
      begin
         case Key of
            VK_DOWN, VK_RIGHT: y := STEP;
            VK_UP, VK_LEFT:    y := -STEP;
         end;
      end;
   end
   else
   begin
      case Key of
         VK_DOWN, VK_RIGHT: y := STEP;
         VK_UP, VK_LEFT:    y := -STEP;
      end;
   end;
   case Key of
      VK_NEXT:  y := STEP * MULTP;
      VK_PRIOR: y := -STEP * MULTP;
   end;
   if y <> 0 then
   begin
      if Key in [VK_LEFT, VK_RIGHT] then
         HorzScrollBar.Position := HorzScrollBar.Position + y
      else
         VertScrollBar.Position := VertScrollBar.Position + y;
      Key := 0;
      PerformFormsRepaint;
   end;
end;

procedure TMainForm.miForAscClick(Sender: TObject);
var
   block: TForDoBlock;
begin
   if pmPages.PopupComponent is TForDoBlock then
   begin
      block := TForDoBlock(pmPages.PopupComponent);
      if Sender = miForAsc then
         block.Order := ordAsc
      else
         block.Order := ordDesc;
   end;
end;

procedure TMainForm.miMemoEditClick(Sender: TObject);
var
   block: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      block := TBlock(pmPages.PopupComponent);
      if block.GetFrontMemo <> nil then
      begin
         MemoEditorForm.SourceBlock := block;
         MemoEditorForm.ShowModal;
      end;
   end;

end;

procedure TMainForm.miMemoVScrollClick(Sender: TObject);
var
   block: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      block := TBlock(pmPages.PopupComponent);
      if Sender = miMemoVScroll then
         block.MemoVScroll := miMemoVScroll.Checked
      else if Sender = miMemoHScroll then
         block.MemoHScroll := miMemoHScroll.Checked
      else if Sender = miMemoWordWrap then
         block.MemoWordWrap := miMemoWordWrap.Checked;
   end;
end;

procedure TMainForm.PerformFormsRepaint;
begin
   if GSettings.EnableDBuffering or NavigatorForm.Visible then
      Repaint;
   NavigatorForm.Invalidate;
end;

procedure TMainForm.miNewFunctionClick(Sender: TObject);
begin
   if GProject <> nil then
      TInfra.GetFunctionsForm.AddUserFunction(GProject.GetActivePage.ScreenToClient(pmPages.PopupPoint));
end;

procedure TMainForm.pgcPagesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
   ptn: TPoint;
   idx: integer;
begin
   if (GProject <> nil) and (htOnItem in pgcPages.GetHitTestInfoAt(MousePos.X, MousePos.Y)) then
   begin
      idx := TInfra.GetPageIndex(pgcPages, MousePos.X, MousePos.Y);
      if idx <> -1 then
      begin
         pmTabs.PopupComponent := pgcPages.Pages[idx];
         pgcPages.ActivePageIndex := idx;
      end;
      ptn := pgcPages.ClientToScreen(MousePos);
      pmTabs.Popup(ptn.X, ptn.Y);
   end
end;

procedure TMainForm.pmTabsPopup(Sender: TObject);
var
   page: TTabSheet;
begin
   miRemovePage.Enabled := false;
   miRenamePage.Enabled := false;
   miAddPage.Enabled := false;
   if pmTabs.PopupComponent is TTabSheet then
   begin
      page := TTabSheet(pmTabs.PopupComponent);
      miRemovePage.Enabled := page <> GProject.GetMainPage;
      miRenamePage.Enabled := miRemovePage.Enabled;
      miAddPage.Enabled := true;
   end;
end;

procedure TMainForm.miRemovePageClick(Sender: TObject);
var
   res: integer;
begin
   res := IDYES;
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
var
   idx: integer;
begin
   if Button = mbLeft then
   begin
      idx := TInfra.GetPageIndex(pgcPages, X, Y);
      if idx <> -1 then
         pgcPages.Pages[idx].BeginDrag(false, 3);
   end;
end;

procedure TMainForm.pgcPagesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if not (Source is TBlockTabSheet) then
      Accept := false;
end;

procedure TMainForm.pgcPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   idx: integer;
begin
   idx := TInfra.GetPageIndex(pgcPages, X, Y);
   if idx <> -1 then
   begin
      pgcPages.Pages[idx].PageIndex := TTabSheet(Source).PageIndex;
      TTabSheet(Source).PageIndex := idx;
      GChange := 1;
   end;
end;

procedure TMainForm.miRenamePageClick(Sender: TObject);
var
   lCaption: TCaption;
   page: TTabSheet;
begin
   if pmTabs.PopupComponent is TTabSheet then
   begin
      page := TTabSheet(pmTabs.PopupComponent);
      lCaption := Trim(InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), page.Caption));
      if (lCaption <> '') and (TInfra.FindDuplicatedPage(page, lCaption) = nil) then
      begin
         page.Caption := lCaption;
         GProject.UpdateHeadersBody(page);
         GChange := 1;
      end;
   end;
end;

procedure TMainForm.miAddPageClick(Sender: TObject);
var
   lCaption: TCaption;
   page: TTabSheet;
begin
   lCaption := Trim(InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), ''));
   if (lCaption <> '') and (GProject.GetPage(lCaption, false) = nil) then
   begin
      page := GProject.GetPage(lCaption);
      page.PageControl.ActivePage := page;
      NavigatorForm.Invalidate;
      GChange := 1; 
   end;
end;

procedure TMainForm.pgcPagesChange(Sender: TObject);
begin
   NavigatorForm.Invalidate;
end;

procedure TMainForm.pmEditsPopup(Sender: TObject);
var
   edit: TCustomEdit;
begin
   miUndo.Enabled := false;
   miCut1.Enabled := false;
   miCopy1.Enabled := false;
   miPaste1.Enabled := false;
   miRemove1.Enabled := false;
   miInsertFunc.Visible := false;
   if pmEdits.PopupComponent is TCustomEdit then
   begin
      edit := TCustomEdit(pmEdits.PopupComponent);
      miUndo.Enabled := edit.CanUndo;
      miCut1.Enabled := edit.SelText <> '';
      miCopy1.Enabled := miCut1.Enabled;
      miRemove1.Enabled := miCut1.Enabled;
      miPaste1.Enabled := Clipboard.HasFormat(CF_TEXT);
      miInsertFunc.Visible := BuildFuncMenu(miInsertFunc) > 0;
   end;
end;

procedure TMainForm.miUndoClick(Sender: TObject);
begin
   TCustomEdit(pmEdits.PopupComponent).Undo;
end;

procedure TMainForm.miCut1Click(Sender: TObject);
begin
   TCustomEdit(pmEdits.PopupComponent).CutToClipboard;
end;

procedure TMainForm.miCopy1Click(Sender: TObject);
begin
   TCustomEdit(pmEdits.PopupComponent).CopyToClipboard;
end;

procedure TMainForm.miPaste1Click(Sender: TObject);
begin
   TCustomEdit(pmEdits.PopupComponent).PasteFromClipboard;
end;

procedure TMainForm.miRemove1Click(Sender: TObject);
begin
   TCustomEdit(pmEdits.PopupComponent).SelText := '';
end;

procedure TMainForm.FuncMenuClick(Sender: TObject);
var
   funcName, backup: string;
   edit: TCustomEdit;
begin
   if (Sender is TMenuItem) and (pmEdits.PopupComponent is TCustomEdit) then
   begin
      edit := TCustomEdit(pmEdits.PopupComponent);
      funcName := StripHotKey(TMenuItem(Sender).Caption);
      funcName := funcName + GInfra.CurrentLang.FuncBrackets;
      backup := '';
      if Clipboard.HasFormat(CF_TEXT) then
         backup := Clipboard.AsText;
      Clipboard.AsText := funcName;
      edit.PasteFromClipboard;
      if GInfra.CurrentLang.FuncBrackets <> '' then
         edit.SelStart := edit.SelStart + GInfra.CurrentLang.FuncBracketsCursorPos;
      if backup <> '' then
         Clipboard.AsText := backup;
   end;
end;

procedure TMainForm.DestroyFuncMenu;
var
   i: integer;
begin
   for i := 0 to Length(FFuncMenu)-1 do
      FFuncMenu[i].Free;
   FFuncMenu := nil;
end;

function TMainForm.BuildFuncMenu(AParent: TMenuItem): integer;
var
   i: integer;
   it: IIterator;
   funcList: TStringList;
   lName: string;
begin
   result := 0;
   if AParent <> nil then
   begin
      DestroyFuncMenu;
      funcList := TStringList.Create;
      try
         funcList.Sorted := true;
         funcList.Duplicates := dupIgnore;
         funcList.AddStrings(GInfra.CurrentLang.NativeFunctions);
         it := GProject.GetUserFunctions;
         while it.HasNext do
         begin
            lName := TUserFunction(it.Next).GetName;
            if lName <> '' then
               funcList.Add(lName);
         end;
         SetLength(FFuncMenu, funcList.Count);
         for i := 0 to funcList.Count-1 do
         begin
            FFuncMenu[i] := TMenuItem.Create(AParent);
            FFuncMenu[i].Caption := funcList[i];
            FFuncMenu[i].OnClick := FuncMenuClick;
         end;
         result := funcList.Count;
      finally
         funcList.Free;
      end;
      if result > 0 then
         AParent.Add(FFuncMenu);
   end;
end;

procedure TMainForm.miIsHeaderClick(Sender: TObject);
begin
   if pmPages.PopupComponent is TComment then
   begin
      TComment(pmPages.PopupComponent).IsHeader := miIsHeader.Checked;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TMainForm.miPasteTextClick(Sender: TObject);
begin
   if Clipboard.HasFormat(CF_TEXT) and (pmPages.PopupComponent is TCustomEdit) then
      TCustomEdit(pmPages.PopupComponent).PasteFromClipboard;
end;

end.

