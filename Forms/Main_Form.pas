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
  Messages, History, Dialogs, ComCtrls;

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
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
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
  private
    { Private declarations }
    FHistoryMenu: THistoryMenu;
    FClockPos: TClockPos;
    function BuildFuncMenu(AParent: TMenuItem): integer;
    procedure DestroyFuncMenu;
    function GetOutFileFilter: string;
    procedure AcceptFile(const AFilePath: string);
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(const root: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(const root: IXMLElement); override;
    function ConfirmSave: integer;
    function GetDisplayedRect: TRect;
    function GetMainBlockNextTopLeft: TPoint;
  end;

var
  MainForm: TMainForm;
  FFuncMenu: array of TMenuItem;

implementation

uses
   Toolbox_Form, ApplicationCommon, About_Form, Main_Block, ParseGlobals, LocalizationManager,
   XMLProcessor, UserFunction, ForDo_Block, Return_Block, Project, Declarations_Form,
   Base_Block, Comment, Case_Block, jpeg, CommonInterfaces, Navigator_Form, CommonTypes,
   LangDefinition, EditMemo_Form, BlockFactory, BlockTabSheet, pngimage;

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
   pgcPages.DoubleBuffered := true;
   FHistoryMenu.Load;
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
   NextPos: array[TClockPos] of TClockPos = (cp3, cp6, cp9, cp12);
   xShift: array[TClockPos] of integer = (0, 20, 0, -20);
   yShift: array[TClockPos] of integer = (20, 40, 60, 40);
begin
   result.X := ((Width - MAIN_BLOCK_DEF_WIDTH) div 2) + xShift[FClockPos];
   result.Y := yShift[FClockPos];
   FClockPos := NextPos[FClockPos];
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
   lTmpCursor: TCursor;
   lFile: string;
begin
    if (GChange = 1) and (GProject <> nil) then
    begin
       case ConfirmSave of
          IDYES: miSave.Click;
          IDCANCEL: exit;
       end;
    end;
    lFile := '';
    if Sender <> miOpen then
       lFile := StripHotKey(TMenuItem(Sender).Caption);
    TInfra.SetInitialSettings;
    lTmpCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    GProject := TProject.GetInstance;
    lFile := TXMLProcessor.ImportFromXMLFile(GProject.ImportFromXMLTag, lFile);
    Screen.Cursor := lTmpCursor;
    if lFile <> '' then
       AcceptFile(lFile)
    else
       TInfra.SetInitialSettings;
end;

procedure TMainForm.AcceptFile(const AFilePath: string);
begin
   Caption := MAIN_FORM_CAPTION + AFilePath;
   GProject.Name := ChangeFileExt(ExtractFilename(AFilePath), '');
   FHistoryMenu.Add(AFilePath);
   GChange := 0;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
var
   lGraphic: TGraphic;
   lFilePath: string;
begin
    ExportDialog.FileName := GProject.Name;
    ExportDialog.Filter := GetOutFileFilter;
    ExportDialog.FilterIndex := 1;
    if ExportDialog.Execute then
    begin
       lFilePath := ExportDialog.Filename;
       if ExportDialog.FilterIndex = 1 then
       begin
          if GProject.ExportToXMLFile(lFilePath) = errNone then
             AcceptFile(lFilePath);
       end
       else
       begin
          case ExportDialog.FilterIndex of
             3: lGraphic := TPNGObject.Create;
             4: lGraphic := TJPEGImage.Create;
          else
                lGraphic := TBitmap.Create;
          end;
          try
             GProject.ExportToGraphic(lGraphic);
             lGraphic.SaveToFile(lFilePath);
          finally
             lGraphic.Free;
          end;
       end
    end;
end;

procedure TMainForm.miGenerateClick(Sender: TObject);
var
   lForm: TForm;
begin
   lForm := TInfra.GetEditorForm;
   if lForm.Showing then
   begin
      if lForm.WindowState = wsMinimized then
         lForm.WindowState := wsNormal;
      lForm.OnShow(lForm);
   end
   else
      lForm.Show;
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
   lBitmap: TBitmap;
begin
   lBitmap := TBitmap.Create;
   try
      GProject.ExportToGraphic(lBitmap);
      TInfra.PrintBitmap(lBitmap);
   finally
      lBitmap.Free;
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
   lBlock: TBlock;
   lActiveObject: IXMLable;
begin
   if (GClpbrd.UndoObject is TBlock) and (TBlock(GClpbrd.UndoObject).ParentBlock <> nil) then
   begin
      lBlock := TBlock(GClpbrd.UndoObject);
      if not lBlock.ParentBlock.CanFocus or
         not lBlock.ParentBlock.Expanded or ((lBlock is TReturnBlock) and (lBlock.ParentBranch.FindInstanceOf(TReturnBlock) <> -1)) then exit;
      lBlock.ParentBranch.UndoRemove(lBlock);
      lBlock.ParentBlock.ResizeWithDrawLock;
      lBlock.SetVisible(true);
      NavigatorForm.Invalidate;
   end
   else if Supports(GClpbrd.UndoObject, IXMLable, lActiveObject) then
      lActiveObject.Active := true;
   TInfra.UpdateCodeEditor(GClpbrd.UndoObject);
   GClpbrd.UndoObject := nil;
   miUndoRemove.Enabled := false;
end;

procedure TMainForm.pmPagesPopup(Sender: TObject);
var
   lComponent: TComponent;
   lBlock: TBlock;
   lFont: TFont;
   lIsFunction: boolean;
begin
   lFont := nil;
   miFont.Visible := False;
   miCopy.Visible := False;
   miCut.Visible := False;
   miRemove.Visible := False;
   miInsert.Visible := False;
   miPaste.Enabled := False;
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

   lComponent := pmPages.PopupComponent;
   lIsFunction := TInfra.IsValid(GClpbrd.UndoObject) and (GClpbrd.UndoObject is TUserFunction);

   miPaste.Enabled := TInfra.IsValid(GClpbrd.Instance) or lIsFunction;

   if lComponent is TBlock then
   begin
       lBlock := TBlock(lComponent);
       lFont := lBlock.GetFont;
       if lBlock.Ired >= 0 then
       begin
          miInsert.Visible := True;
          miInstr.Enabled := True;
          miLoop.Enabled := True;
          miText.Enabled := True;
          miFolder.Enabled := True;
          miReturn.Enabled := lBlock.CanInsertReturnBlock;
          if (GClpbrd.Instance is TComment) or ((GClpbrd.Instance is TReturnBlock) and not miReturn.Enabled) then
             miPaste.Enabled := False;
       end
       else if lBlock.IsCursorSelect then
       begin
          miRemove.Visible := True;
          miFont.Visible := True;
          miMemoEdit.Visible := lBlock.GetFrontMemo <> nil;
          if not (lBlock is TReturnBlock) then
             miExport.Visible := True;
          miPrint2.Visible := True;
          miCut.Visible := true;
          if not (lBlock is TMainBlock) then
             miCopy.Visible := True;
          if lBlock is TGroupBlock then
          begin
             miExpFold.Visible := true;
             if TGroupBlock(lBlock).Expanded then
             begin
                miExpFold.Caption := i18Manager.GetString('miFoldBlock');
                if lBlock is TCaseBlock then
                   miAddBranch.Visible := True;
             end
             else
                miExpFold.Caption := i18Manager.GetString('miExpandBlock');
          end;
          if (lBlock is TGroupBlock) and TGroupBlock(lBlock).Expanded and TGroupBlock(lBlock).HasFoldedBlocks then
             miExpandAll.Visible := true;
          if lBlock is TForDoBlock then
          begin
             miForAsc.Visible := true;
             miForDesc.Visible := true;
             miForAsc.Checked := TForDoBlock(lBlock).Order = ordAsc;
             miForDesc.Checked := not miForAsc.Checked;
          end;
          miMemo.Visible := lBlock.GetFrontMemo <> nil;
          if miMemo.Visible then
          begin
             miMemoVScroll.Checked := lBlock.MemoVScroll;
             miMemoHScroll.Checked := lBlock.MemoHScroll;
             miMemoWordWrap.Checked := lBlock.MemoWordWrap;
          end;
       end
       else
       begin
          miInsert.Visible := True;
          miComment.Enabled := True;
          if (GClpbrd.Instance is TBlock) and not lIsFunction then
             miPaste.Enabled := False;
       end;
       miFrame.Visible := True;
       miFrame.Checked := lBlock.Frame;
       if (lBlock is TCaseBlock) and (lBlock.Ired > PRIMARY_BRANCH_IND) then
          miRemoveBranch.Visible := True;
   end
   else if lComponent is TComment then
   begin
      lFont := TComment(lComponent).Font;
      miRemove.Visible := True;
      miFont.Visible := True;
      miCopy.Visible := True;
      miIsHeader.Visible := true;
      miIsHeader.Checked := TComment(lComponent).IsHeader;
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
   lPoint: TPoint;
   lPage: TBlockTabSheet;
begin
   if GProject <> nil then
   begin
      lPage := GProject.GetActivePage;
      lPoint := lPage.ScreenToClient(pmPages.PopupPoint);
      TComment.Create(lPage, lPoint.X, lPoint.Y, 150, 50);
      GChange := 1;
   end;
end;

procedure TMainForm.miAssignClick(Sender: TObject);
var
   lNewBlock, lCurrentBlock, lSourceBlock: TBlock;
   lBranch: TBranch;
   lParent: TGroupBlock;
   lTmpCursor: TCursor;
   lComment: TComment;
   lTopLeft: TPoint;
   lBlockType: TBlockType;
   lLocked: boolean;
   lPage: TBlockTabSheet;
   lFunction: TUserFunction;
begin

   lSourceBlock := nil;
   lParent := nil;
   lFunction := nil;
   lComment := nil;

   if TInfra.IsValid(GClpbrd.UndoObject) and (GClpbrd.UndoObject is TUserFunction) then
      lFunction := TUserFunction(GClpbrd.UndoObject)
   else if GClpbrd.Instance is TComment then
      lComment := TComment(GClpbrd.Instance);

   if (Sender = miPaste) and ((lFunction <> nil) or (lComment <> nil)) then
   begin
      lPage := GProject.GetActivePage;
      lTopLeft := lPage.ScreenToClient(pmPages.PopupPoint);
      if lFunction <> nil then
      begin
         if lFunction.Body <> nil then
         begin
            lFunction.Body.Page := lPage;
            lFunction.Body.SetBounds(lTopLeft.X, lTopLeft.Y, lFunction.Body.Width, lFunction.Body.Height);
         end;
         miUndoRemoveClick(miUndoRemove);
      end
      else if lComment <> nil then
         lComment.Clone(lPage, @lTopLeft);
      GChange := 1;
      NavigatorForm.Invalidate;
      exit;
   end;

   if pmPages.PopupComponent is TBlock then
   begin
      lCurrentBlock := TBlock(pmPages.PopupComponent);
      if (lCurrentBlock.Ired > 0) and (lCurrentBlock is TGroupBlock) then
         lParent := TGroupBlock(lCurrentBlock)
      else if lCurrentBlock.Ired = 0 then
         lParent := lCurrentBlock.ParentBlock;
   end;

   if lParent <> nil then
   begin
   
      lBranch := lParent.GetBranch(lParent.Ired);
      if lBranch <> nil then
         lCurrentBlock := nil
      else
         lBranch := lCurrentBlock.ParentBranch;

      if lBranch <> nil then
      begin
         lLocked := lBranch.ParentBlock.LockDrawing;
         try
            lNewBlock := nil;
            lBlockType := blUnknown;
            if Sender = miAssign then
               lBlockType := blAssign
            else if Sender = miMultipleAssign then
               lBlockType := blMultAssign
            else if Sender = miIfElse then
               lBlockType := blIfElse
            else if Sender = miWhile then
               lBlockType := blWhile
            else if Sender = miFor then
               lBlockType := blFor
            else if Sender = miRepeat then
               lBlockType := blRepeat
            else if Sender = miInput then
               lBlockType := blInput
            else if Sender = miOutput then
               lBlockType := blOutput
            else if Sender = miRoutineCall then
               lBlockType := blFuncCall
            else if Sender = miIf then
               lBlockType := blIf
            else if Sender = miCase then
               lBlockType := blCase
            else if Sender = miReturn then
               lBlockType := blReturn
            else if Sender = miText then
               lBlockType := blText
            else if Sender = miFolder then
               lBlockType := blFolder
            else if (Sender = miPaste) and TInfra.IsValid(GClpbrd.Instance) and (GClpbrd.Instance is TBlock) then
            begin
               lSourceBlock := TBlock(GClpbrd.Instance);
               lTmpCursor := Screen.Cursor;
               Screen.Cursor := crHourGlass;
               lNewBlock := lSourceBlock.Clone(lBranch);
               Screen.Cursor := lTmpCursor;
            end;
            if lBlockType <> blUnknown then
               lNewBlock := TBlockFactory.Create(lBranch, lBlockType);
            if lNewBlock <> nil then
            begin
               lBranch.InsertAfter(lNewBlock, lCurrentBlock);
               lParent.ResizeHorz(true);
               lParent.ResizeVert(true);
               if not lNewBlock.Visible then
               begin
                  lNewBlock.Show;
                  lNewBlock.RefreshStatements;
               end;
               if lSourceBlock <> nil then
                  lNewBlock.CloneComments(lSourceBlock);
               TInfra.UpdateCodeEditor(lNewBlock);
            end;
         finally
            if lLocked then
               lBranch.ParentBlock.UnLockDrawing;
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
   lComponent: TComponent;
   lFontStyles: TFontStyles;
   lFontStyle: TFontStyle;
   lBlock: TBlock;
   lComment: TComment;
begin
   lBlock := nil;
   lComment := nil;
   lComponent := pmPages.PopupComponent;
   if (lComponent is TBlock) or (lComponent is TComment) then
   begin
      if lComponent is TBlock then
      begin
         lBlock := TBlock(lComponent);
         lFontStyles := lBlock.GetFont.Style;
      end
      else if lComponent is TComment then
      begin
         lComment := TComment(lComponent);
         lFontStyles := lComment.Font.Style;
      end;

      if Sender = miStyleBold then
         lFontStyle := fsBold
      else if Sender = miStyleItalic then
         lFontStyle := fsItalic
      else if Sender = miStyleUnderline then
         lFontStyle := fsUnderline
      else if Sender = miStyleStrikeOut then
         lFontStyle := fsStrikeOut;

      if Sender = miStyleNormal then
         lFontStyles := []
      else if lFontStyle in lFontStyles then
         Exclude(lFontStyles, lFontStyle)
      else
         Include(lFontStyles, lFontStyle);
         
      if lBlock <> nil then
      begin
         lBlock.SetFontStyle(lFontStyles);
         if (Sender = miStyleStrikeOut) and not lBlock.SkipUpdateEditor then
            TInfra.UpdateCodeEditor;
      end
      else if lComment <> nil then
         lComment.Font.Style := lFontStyles;
      GChange := 1;
   end;
end;

procedure TMainForm.miSize8Click(Sender: TObject);
var
   lComponent: TComponent;
   lFontSize: integer;
begin
   lComponent := pmPages.PopupComponent;
   if (lComponent is TBlock) or (lComponent is TComment) then
   begin
      if Sender = miSize10 then
         lFontSize := 10
      else if Sender = miSize12 then
         lFontSize := 12
      else
         lFontSize := 8;
      if lComponent is TBlock then
         TBlock(lComponent).SetFontSize(lFontSize)
      else if lComponent is TComment then
         TComment(lComponent).Font.Size := lFontSize;
      GChange := 1;
   end;
end;

procedure TMainForm.miCopyClick(Sender: TObject);
var
   lComponent: TComponent;
begin
   lComponent := pmPages.PopupComponent;
   if (lComponent is TBlock) or (lComponent is TComment) then
   begin
      if Sender = miCut then
         miRemove.Click;
      GClpbrd.Instance := TControl(lComponent);
   end;
end;

procedure TMainForm.miRemoveClick(Sender: TObject);
var
   lComponent: TComponent;
begin
   lComponent := pmPages.PopupComponent;
   if (lComponent = GClpbrd.Instance) or (GClpbrd.UndoObject = GClpbrd.Instance) then
      GClpbrd.Instance := nil;
   if lComponent is TBlock then
      TBlock(lComponent).Remove
   else if lComponent is TComment then
      lComponent.Free;
   GChange := 1;
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
   lWord: Word;
begin
   lWord := VK_DOWN;
   FormKeyDown(Self, lWord, [ssCtrl]);
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
   lWord: Word;
begin
   lWord := VK_UP;
   FormKeyDown(Self, lWord, [ssCtrl]);
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

function TMainForm.GetOutFileFilter: string;
begin
   result := i18Manager.GetString('XMLFilesFilter') + '|' +
             i18Manager.GetString('BMPFilesFilter') + '|' +
             i18Manager.GetString('PNGFilesFilter') + '|' +
             i18Manager.GetString('JPGFilesFilter');
end;

procedure TMainForm.miExportClick(Sender: TObject);
var
   lBlock: TBlock;
   lGraphic: TGraphic;
   lExportProc: TExportProc;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      lBlock := TBlock(pmPages.PopupComponent);
      ExportDialog.Filename := '';
      ExportDialog.Filter := GetOutFileFilter;
      ExportDialog.FilterIndex := 1;
      if ExportDialog.Execute then
      begin
         if ExportDialog.FilterIndex = 1 then
         begin
            if lBlock is TMainBlock then
               lExportProc := TUserFunction(TMainBlock(lBlock).UserFunction).ExportToXMLTag
            else
               lExportProc := lBlock.ExportToXMLTag;
            TXMLProcessor.ExportToXMLFile(lExportProc, ExportDialog.Filename);
         end
         else
         begin
            case ExportDialog.FilterIndex of
               3: lGraphic := TPNGObject.Create;
               4: lGraphic := TJPEGImage.Create;
            else
                  lGraphic := TBitmap.Create;
            end;
            try
               lBlock.ClearSelection;
               lblock.ExportToGraphic(lGraphic);
               lGraphic.SaveToFile(ExportDialog.Filename);
            finally
               lGraphic.Free;
            end;
         end;
      end;
   end;
end;

procedure TMainForm.miImportClick(Sender: TObject);
var
   lComponent: TComponent;
   lPoint: TPoint;
   lFunction: TUserFunction;
   lImportProc: TImportProc;
   lImportFunc: boolean;
begin
   if GProject <> nil then
   begin
      lImportFunc := false;
      lComponent := pmPages.PopupComponent;
      if (lComponent is TBlock) and (TBlock(lComponent).Ired >= 0) then
         lImportProc := TBlock(lComponent).ImportFromXMLTag
      else
      begin
         lImportProc := GProject.ImportUserFunctionsFromXML;
         lImportFunc := true;
      end;
      if TXMLProcessor.ImportFromXMLFile(lImportProc) <> '' then
      begin
         if lImportFunc then
         begin
            lFunction := GProject.LastUserFunction;
            if (lFunction <> nil) and lFunction.Active and (lFunction.Body <> nil) and lFunction.Body.Visible then
            begin
               lPoint := GProject.GetActivePage.ScreenToClient(pmPages.PopupPoint);
               lFunction.Body.Left := lPoint.X;
               lFunction.Body.Top := lPoint.Y;
               lFunction.Body.Page.Form.SetScrollBars;
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
   lBlock: TGroupBlock;
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      lBlock := TGroupBlock(pmPages.PopupComponent);
      lBlock.ClearSelection;
      lBlock.ExpandFold(true);
   end;
end;

procedure TMainForm.miAddBranchClick(Sender: TObject);
var
   lPoint: TPoint;
   lCaseBlock: TCaseBlock;
   lBranch: TBranch;
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      lCaseBlock := TCaseBlock(pmPages.PopupComponent);
      lPoint := Point(lCaseBlock.GetBranch(lCaseBlock.BranchCount).GetMostRight+60, lCaseBlock.Height-32);
      lBranch := lCaseBlock.AddBranch(lPoint, true);
      TInfra.UpdateCodeEditor(lBranch);
   end;
end;

procedure TMainForm.miRemoveBranchClick(Sender: TObject);
var
   res: integer;
   lCaseBlock: TCaseBlock;
begin
   if pmPages.PopupComponent is TCaseBlock then
   begin
      res := IDYES;
      if GSettings.ConfirmRemove then
         res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
      if res = IDYES then
      begin
         lCaseBlock := TCaseBlock(pmPages.PopupComponent);
         lCaseBlock.RemoveBranch;
         TInfra.UpdateCodeEditor(lCaseBlock.Branch);
      end;
   end;
end;

procedure TMainForm.miExpandAllClick(Sender: TObject);
var
   lBlock: TGroupBlock;
   lLocked: boolean;
begin
   if pmPages.PopupComponent is TGroupBlock then
   begin
      lBlock := TGroupBlock(pmPages.PopupComponent);
      lBlock.ClearSelection;
      lLocked := lBlock.LockDrawing;
      try
         lBlock.ExpandAll;
      finally
         if lLocked then
            lBlock.UnLockDrawing;
      end;
   end;
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
   lBitmap: TBitmap;
   lBlock: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      lBlock := TBlock(pmPages.PopupComponent);
      lBitmap := TBitmap.Create;
      try
         lBlock.ClearSelection;
         lBlock.ExportToGraphic(lBitmap);
         TInfra.PrintBitmap(lBitmap);
      finally
         lBitmap.Free;
      end;
   end;
end;

procedure TMainForm.miProjectClick(Sender: TObject);
begin
   miAddMain.Enabled := GInfra.CurrentLang.EnabledMainProgram and
                        (GProject <> nil) and (GProject.GetMainBlock = nil);
   miUndoRemove.Enabled := GClpbrd.UndoObject <> nil;
end;

procedure TMainForm.miAddMainClick(Sender: TObject);
var
   lBody: TMainBlock;
begin
   if GProject <> nil then
   begin
      lBody := TMainBlock.Create(GProject.GetActivePage, GetMainBlockNextTopLeft);
      TUserFunction.Create(nil, lBody);
      TInfra.UpdateCodeEditor(lBody);
   end;
end;

procedure TMainForm.SetScrollBars;
var
   lPoint: TPoint;
begin
   if GProject <> nil then
   begin
      lPoint := GProject.GetBottomRight;
      if lPoint.X > ClientWidth then
         HorzScrollBar.Range := lPoint.X
      else
         HorzScrollBar.Range := ClientWidth;
      if lPoint.Y > ClientHeight then
         VertScrollBar.Range := lPoint.Y
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
   lBlock: TMainBlock;
   lPage: TBlockTabSheet;
begin
   if GProject <> nil then
   begin
      lPage := GProject.GetActivePage;
      lBlock := TMainBlock.Create(lPage, lPage.ScreenToClient(pmPages.PopupPoint));
      lBlock.OnResize(lBlock);
      TUserFunction.Create(nil, lBlock);
      GChange := 1;
   end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const
   STEP = 15;
   MULTP = 20;
var
   lPos: integer;
   lScrollBar: TControlScrollBar;
begin
   lPos := 0;
   if Sender = Self then
   begin
      if ssCtrl in Shift then
      begin
         case Key of
            VK_DOWN, VK_RIGHT: lPos := STEP;
            VK_UP, VK_LEFT:    lPos := -STEP;
         end;
      end;
   end
   else
   begin
      case Key of
         VK_DOWN, VK_RIGHT: lPos := STEP;
         VK_UP, VK_LEFT:    lPos := -STEP;
      end;
   end;
   case Key of
      VK_NEXT:  lPos := STEP * MULTP;
      VK_PRIOR: lPos := -STEP * MULTP;
   end;
   if lPos <> 0 then
   begin
      if Key in [VK_LEFT, VK_RIGHT] then
         lScrollBar := HorzScrollBar
      else
         lScrollBar := VertScrollBar;
      lScrollBar.Position := lScrollBar.Position + lPos;
      Key := 0;
      PerformFormsRepaint;
   end;
end;

procedure TMainForm.miForAscClick(Sender: TObject);
var
   lBlock: TForDoBlock;
begin
   if pmPages.PopupComponent is TForDoBlock then
   begin
      lBlock := TForDoBlock(pmPages.PopupComponent);
      if Sender = miForAsc then
         lBlock.Order := ordAsc
      else
         lBlock.Order := ordDesc;
   end;
end;

procedure TMainForm.miMemoEditClick(Sender: TObject);
var
   lBlock: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      lBlock := TBlock(pmPages.PopupComponent);
      if lBlock.GetFrontMemo <> nil then
      begin
         MemoEditorForm.SourceBlock := lBlock;
         MemoEditorForm.ShowModal;
      end;
   end;

end;

procedure TMainForm.miMemoVScrollClick(Sender: TObject);
var
   lBlock: TBlock;
begin
   if pmPages.PopupComponent is TBlock then
   begin
      lBlock := TBlock(pmPages.PopupComponent);
      if Sender = miMemoVScroll then
         lBlock.MemoVScroll := miMemoVScroll.Checked
      else if Sender = miMemoHScroll then
         lBlock.MemoHScroll := miMemoHScroll.Checked
      else if Sender = miMemoWordWrap then
         lBlock.MemoWordWrap := miMemoWordWrap.Checked;
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
   lPoint: TPoint;
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
      lPoint := pgcPages.ClientToScreen(MousePos);
      pmTabs.Popup(lPoint.X, lPoint.Y);
   end
end;

procedure TMainForm.pmTabsPopup(Sender: TObject);
var
   lPage: TTabSheet;
begin
   miRemovePage.Enabled := false;
   miRenamePage.Enabled := false;
   miAddPage.Enabled := false;
   if pmTabs.PopupComponent is TTabSheet then
   begin
      lPage := TTabSheet(pmTabs.PopupComponent);
      miRemovePage.Enabled := lPage <> GProject.GetMainPage;
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
   lPage: TTabSheet;
begin
   if pmTabs.PopupComponent is TTabSheet then
   begin
      lPage := TTabSheet(pmTabs.PopupComponent);
      lCaption := Trim(InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), lPage.Caption));
      if (lCaption <> '') and (TInfra.FindDuplicatedPage(lPage, lCaption) = nil) then
      begin
         lPage.Caption := lCaption;
         GProject.UpdateHeadersBody(lPage);
         GChange := 1;
      end;
   end;
end;

procedure TMainForm.miAddPageClick(Sender: TObject);
var
   lCaption: TCaption;
   lPage: TTabSheet;
begin
   lCaption := Trim(InputBox(i18Manager.GetString('Page'), i18Manager.GetString('EnterPage'), ''));
   if (lCaption <> '') and (GProject.GetPage(lCaption, false) = nil) then
   begin
      lPage := GProject.GetPage(lCaption);
      lPage.PageControl.ActivePage := lPage;
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
   lEdit: TCustomEdit;
begin
   miUndo.Enabled := false;
   miCut1.Enabled := false;
   miCopy1.Enabled := false;
   miPaste1.Enabled := false;
   miRemove1.Enabled := false;
   miInsertFunc.Visible := false;
   if pmEdits.PopupComponent is TCustomEdit then
   begin
      lEdit := TCustomEdit(pmEdits.PopupComponent);
      miUndo.Enabled := lEdit.CanUndo;
      miCut1.Enabled := lEdit.SelText <> '';
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
   lFuncName, lBackup: string;
   lEdit: TCustomEdit;
begin
   if (Sender is TMenuItem) and (pmEdits.PopupComponent is TCustomEdit) then
   begin
      lEdit := TCustomEdit(pmEdits.PopupComponent);
      lFuncName := StripHotKey(TMenuItem(Sender).Caption);
      lFuncName := lFuncName + GInfra.CurrentLang.FuncBrackets;
      lBackup := '';
      if Clipboard.HasFormat(CF_TEXT) then
         lBackup := Clipboard.AsText;
      Clipboard.AsText := lFuncName;
      lEdit.PasteFromClipboard;
      if GInfra.CurrentLang.FuncBrackets <> '' then
         lEdit.SelStart := lEdit.SelStart + GInfra.CurrentLang.FuncBracketsCursorPos;
      if lBackup <> '' then
         Clipboard.AsText := lBackup;
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
   lFuncList: TStringList;
   lName: string;
begin
   result := 0;
   if AParent <> nil then
   begin
      DestroyFuncMenu;
      lFuncList := TStringList.Create;
      try
         lFuncList.Sorted := true;
         lFuncList.Duplicates := dupIgnore;
         lFuncList.AddStrings(GInfra.CurrentLang.NativeFunctions);
         it := GProject.GetUserFunctions;
         while it.HasNext do
         begin
            lName := TUserFunction(it.Next).GetName;
            if lName <> '' then
               lFuncList.Add(lName);
         end;
         SetLength(FFuncMenu, lFuncList.Count);
         for i := 0 to lFuncList.Count-1 do
         begin
            FFuncMenu[i] := TMenuItem.Create(AParent);
            FFuncMenu[i].Caption := lFuncList[i];
            FFuncMenu[i].OnClick := FuncMenuClick;
         end;
         result := lFuncList.Count;
      finally
         lFuncList.Free;
      end;
      if result > 0 then
         AParent.Add(FFuncMenu);
   end;
end;

procedure TMainForm.miIsHeaderClick(Sender: TObject);
var
   lComment: TComment;
begin
   if pmPages.PopupComponent is TComment then
   begin
      TComment(pmPages.PopupComponent).IsHeader := miIsHeader.Checked;
      TInfra.UpdateCodeEditor;
   end;
end;

end.

