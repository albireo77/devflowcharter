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



{ This unit contains definition of two base classes: TBlock and TGroupBlock }

unit Base_Block;

interface

uses
   WinApi.Windows, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics,
   WinApi.Messages, System.SysUtils, System.Classes, Vcl.ComCtrls, System.UITypes,
   System.Contnrs, Statement, OmniXML, BaseIterator, CommonInterfaces, CommonTypes,
   BlockTabSheet;

const
   PRIMARY_BRANCH_IND = 1;
   LAST_LINE = -1;
   
type

   TInitParms = record
      Width: integer;
      Height: integer;
      BottomHook: integer;
      TopHook: integer;
      BottomPoint: TPoint;
      P2X: integer;
      BranchPoint: TPoint;
      IPoint: TPoint;
      HeightAffix: integer;
   end;

   TGroupBlock = class;
   TBranch = class;

   TBlock = class(TCustomControl, IIdentifiable, IFocusable, IExportable)
      private
         FParentBlock: TGroupBlock;
         FParentBranch: TBranch;
         FId: integer;
         FMemoVScroll,
         FMemoHScroll: boolean;
      protected
         FType: TBlockType;
         FStatement: TStatement;
         FTopParentBlock: TGroupBlock;
         FHResize,
         FVResize,
         FRefreshMode,
         FFrame,
         FMouseLeave: boolean;
         FShape: TColorShape;
         procedure MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); virtual;
         procedure MyOnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
         procedure MyOnDragDrop(Sender, Source: TObject; X, Y: Integer);
         procedure MyOnChange(Sender: TObject);
         procedure MyOnDblClick(Sender: TObject);
         procedure OnChangeMemo(Sender: TObject); virtual;
         procedure SetMemoVScroll(AValue: boolean);
         procedure UpdateMemoVScroll;
         procedure SetMemoHScroll(AValue: boolean);
         procedure UpdateMemoHScroll;
         procedure ResetMemoScrollBars(const AStyle: TScrollStyle; const AMemo: TMemo);
         procedure SetMemoWordWrap(AValue: boolean);
         function GetMemoWordWrap: boolean;
         procedure WMMouseLeave(var Msg: TMessage); message WM_MOUSELEAVE;
         procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
         procedure WMExitSizeMove(var Msg: TWMMove); message WM_EXITSIZEMOVE;
         procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
         procedure OnMouseLeave; virtual;
         procedure Paint; override;
         procedure DrawI;
         procedure DrawTextLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
         procedure DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
         function GetId: integer;
         function PerformEditorUpdate: boolean;
         procedure SelectBlock(const APoint: TPoint);
         procedure SetCursor(const APoint: TPoint);
         procedure SetFrame(const AValue: boolean);
         procedure PutTextControls; virtual;
         procedure DrawArrowLine(const ABeginPoint, AEndPoint: TPoint; const AArrowPos: TArrowPosition = arrEnd; const AColor: TColor = clBlack);
         function GetEllipseTextRect(const APoint: TPoint; const AText: string): TRect;
         function DrawEllipsedText(const APoint: TPoint; const AText: string): TRect;
         procedure MoveComments(x, y: integer);
         function GetUndoObject: TObject; virtual;
         function IsInFront(const AControl: TWinControl): boolean;
         procedure SetPage(APage: TBlockTabSheet); virtual;
         function GetPage: TBlockTabSheet; virtual;
         procedure CreateParams(var Params: TCreateParams); override;
         procedure OnWindowPosChanged(x, y: integer); virtual;
         function ProcessComments: boolean;
         function IsForeParent(const AParent: TObject): boolean;
         function GetErrorMsg(AEdit: TCustomEdit): string;
         procedure SaveInXML2(const ATag: IXMLElement);
      public
         BottomPoint: TPoint;    // points to arrow at the bottom of the block
         IPoint: TPoint;          // points to I mark
         BottomHook: integer;
         TopHook: TPoint;
         Ired: Integer;           // indicates active arrow; -1: none, 0: bottom, 1: branch1, 2: branch2 and so on...
         memoWidth,
         memoHeight: integer;
         property Frame: boolean read FFrame write SetFrame;
         property MemoVScroll: boolean read FMemoVScroll write SetMemoVScroll;
         property MemoHScroll: boolean read FMemoHScroll write SetMemoHScroll;
         property MemoWordWrap: boolean read GetMemoWordWrap write SetMemoWordWrap;
         property TopParentBlock: TGroupBlock read FTopParentBlock;
         property Page: TBlockTabSheet read GetPage write SetPage;
         property ParentBlock: TGroupBlock read FParentBlock;
         property BType: TBlockType read FType default blUnknown;
         property ParentBranch: TBranch read FParentBranch;
         property Id: integer read GetId;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AId: integer = ID_INVALID);
         destructor Destroy; override;
         function Clone(const ABranch: TBranch): TBlock; virtual;
         procedure ChangeColor(const AColor: TColor); virtual;
         procedure SetFontStyle(const AStyle: TFontStyles);
         procedure SetFontSize(const ASize: integer);
         function GetFont: TFont;
         procedure SetFont(const AFont: TFont);
         procedure RefreshStatements;
         procedure PopulateComboBoxes; virtual;
         function GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer; virtual;
         function GetFromXML(const ATag: IXMLElement): TErrorType; virtual;
         procedure SaveInXML(const ATag: IXMLElement); virtual;
         function FillTemplate(const ALangId: string; const ATemplate: string = ''): string; virtual;
         function FillCodedTemplate(const ALangId: string): string; virtual;
         function GetDescTemplate(const ALangId: string): string; virtual;
         function GetTextControl: TCustomEdit; virtual;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; virtual;
         function IsCursorSelect: boolean;
         function IsCursorResize: boolean;
         function CanInsertReturnBlock: boolean; virtual;
         procedure ExportToXMLTag(ATag: IXMLElement);
         function ImportFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
         procedure ExportToGraphic(const AGraphic: TGraphic); virtual;
         procedure UpdateEditor(AEdit: TCustomEdit); virtual;
         function SkipUpdateEditor: boolean;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; virtual;
         function CanBeFocused: boolean; virtual;
         procedure GenerateDefaultTemplate(const ALines: TStringList; const ALangId: string; const ADeep: integer);
         procedure GenerateTemplateSection(const ALines: TStringList; const ATemplate: TStringList; const ALangId: string; const ADeep: integer); overload; virtual;
         procedure GenerateTemplateSection(const ALines: TStringList; const ATemplate: string; const ALangId: string; const ADeep: integer); overload;
         function GetFrontMemo: TMemo; virtual;
         function FocusOnTextControl(AInfo: TFocusInfo): boolean;
         procedure ClearSelection;
         procedure ChangeFrame;
         procedure RepaintAll;
         function Next: TBlock;
         function Prev: TBlock;
         function CountErrWarn: TErrWarnCount; virtual;
         function LockDrawing: boolean;
         procedure UnLockDrawing;
         function GetFocusColor: TColor;
         function Remove: boolean; virtual;
         function CanBeRemoved: boolean;
         function IsBoldDesc: boolean; virtual;
         function GetComments(const AInFront: boolean = false): IIterator;
         function GetPinComments: IIterator;
         procedure SetVisible(const AVisible: boolean; const ASetComments: boolean = true); virtual;
         procedure BringAllToFront;
         function PinComments: integer;
         function UnPinComments: integer; virtual;
         procedure CloneComments(const ASource: TBlock);
         procedure ImportCommentsFromXML(const ATag: IXMLElement);
         procedure CloneFrom(ABlock: TBlock); virtual;
         function GetExportFileName: string; virtual;
         function ExportToXMLFile(const AFile: string): TErrorType; virtual;
      published
         property Color;
         property OnMouseDown;
         property OnResize;
   end;

   TGroupBlock = class(TBlock)    // block which can aggregate child blocks
      private
         function GetBranchCount: integer;
      protected
         FBlockImportMode: boolean;
         FMemoFolder: TMemo;
         FInitParms: TInitParms;
         FDrawingFlag: boolean;
         FBranchArray: array of TBranch;
         FTrueLabel, FFalseLabel: string;
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure SetWidth(const AMinX: integer); virtual;
         procedure OnMouseLeave; override;
         procedure LinkBlocks(const idx: integer = PRIMARY_BRANCH_IND-1);
         procedure Paint; override;
         function ExtractBranchIndex(const AStr: string): integer;
         procedure PutTextControls; override;
         function GetDiamondPoint: TPoint; virtual;
      public
         Branch: TBranch;     // primary branch to order child blocks
         Expanded: boolean;
         FFoldParms: TInitParms;
         property BlockImportMode: boolean read FBlockImportMode write FBlockImportMode;
         property BranchCount: integer read GetBranchCount;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AHook: TPoint; const AId: integer = ID_INVALID);
         destructor Destroy; override;
         procedure ResizeHorz(const AContinue: boolean); virtual;
         procedure ResizeVert(const AContinue: boolean); virtual;
         function GenerateNestedCode(const ALines: TStringList; const ABranchInd, ADeep: integer; const ALangId: string): integer;
         procedure ExpandFold(const AResize: boolean); virtual;
         function GetBranch(const idx: integer): TBranch;
         procedure ChangeColor(const AColor: TColor); override;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; override;
         function AddBranch(const AHook: TPoint; const AResizeInd: boolean; const ABranchId: integer = ID_INVALID; const ABranchStmntId: integer = ID_INVALID): TBranch; virtual;
         procedure ExpandAll;
         function HasFoldedBlocks: boolean;
         procedure PopulateComboBoxes; override;
         function GetFrontMemo: TMemo; override;
         function CanInsertReturnBlock: boolean; override;
         function GetFromXML(const ATag: IXMLElement): TErrorType; override;
         procedure SaveInXML(const ATag: IXMLElement); override;
         procedure GenerateTemplateSection(const ALines: TStringList; const ATemplate: TStringList; const ALangId: string; const ADeep: integer); override;
         function GetBlocks(const AIndex: integer = PRIMARY_BRANCH_IND-1): IIterator;
         function GetBranches(const AStart: integer = PRIMARY_BRANCH_IND-1): IIterator;
         procedure ResizeWithDrawLock;
         function GetFoldedText: string;
         procedure SetFoldedText(const AText: string);
         function CountErrWarn: TErrWarnCount; override;
         procedure SetVisible(const AVisible: boolean; const ASetComments: boolean = true); override;
         function CanBeFocused: boolean; override;
         function UnPinComments: integer; override;
         procedure CloneFrom(ABlock: TBlock); override;
   end;

   TBranch = class(TObjectList, IIdentifiable)
      private
         FParentBlock: TGroupBlock;
         FRmvBlockIdx: integer;
         FId: integer;
         function GetIndex: integer;
         function GetHeight: integer;
         function GetItems(AIndex: integer): TBlock;
         procedure SetItems(AIndex: integer; ABlock: TBlock);
         function GetFirst: TBlock;
         function GetLast: TBlock;
         function GetId: integer;
         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;
         function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
      public
         Hook: TPoint;           // hook determines position of blocks within a branch in parent window coordinates
         Statement: TStatement;
         property ParentBlock: TGroupBlock read FParentBlock;
         property Index: integer read GetIndex;
         property Height: integer read GetHeight;
         property Items[Index: integer]: TBlock read GetItems write SetItems; default;
         property First: TBlock read GetFirst;
         property Last: TBlock read GetLast;
         property Id: integer read GetId;
         constructor Create(const AParent: TGroupBlock; const AHook: TPoint; const AId: integer = ID_INVALID);
         destructor Destroy; override;
         procedure InsertAfter(ANewBlock, APosBlock: TBlock);
         procedure Insert(AIndex: integer; ABlock: TBlock);
         function IndexOf(ABlock: TBlock): integer;
         function Add(ABlock: TBlock): integer;
         function Remove(ABlock: TObject): integer;
         procedure UndoRemove(ABlock: TBlock);
         function GetMostRight: integer;
   end;

   TBranchIterator = class(TBaseIterator);
   TBlockIterator = class(TBaseIterator);
   TCommentIterator = class(TBaseIterator);

implementation

uses
   System.StrUtils, Vcl.Menus, System.Types, System.Math, Main_Block, Return_Block,
   ApplicationCommon, BlockFactory, UserFunction, XMLProcessor, Navigator_Form,
   LangDefinition, FlashThread, Comment;

type
   THackControl = class(TControl);
   THackCustomEdit = class(TCustomEdit);

constructor TBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AId: integer = ID_INVALID);
begin

   if ABranch <> nil then
   begin
      FParentBlock := ABranch.ParentBlock;
      FTopParentBlock := FParentBlock.TopParentBlock;
      inherited Create(FParentBlock);
      Parent := FParentBlock;
      FParentBranch := ABranch;
   end
   else                                     // current object is TMainBlock class
   begin
      FTopParentBlock := TGroupBlock(Self);
      inherited Create(Page.Form);
      Parent := Page;
   end;

   ParentFont  := true;
   ParentColor := true;
   Ctl3D       := false;
   Color       := Page.Brush.Color;
   Font.Name   := GSettings.FlowchartFontName;
   PopupMenu   := Page.Form.pmPages;
   DoubleBuffered := GSettings.EnableDBuffering;
   ControlStyle := ControlStyle + [csOpaque];
   ParentBackground := false;
   Canvas.TextFlags := Canvas.TextFlags or ETO_OPAQUE;
   SetBounds(ALeft, ATop, AWidth, AHeight);

   FId := GProject.Register(Self, AId);
   FStatement := TStatement.Create(Self);
   FMouseLeave := true;
   FShape := shpRectangle;
   FStatement.Color := GSettings.GetShapeColor(FShape);
   Ired := -1;
   memoWidth := 280;
   memoHeight := 182;

   OnMouseDown := MyOnMouseDown;
   OnMouseUp   := MyOnMouseUp;
   OnMouseMove := MyOnMouseMove;
   OnCanResize := MyOnCanResize;
   OnDblClick  := MyOnDblClick;
   OnDragOver  := MyOnDragOver;
   OnDragDrop  := MyOnDragDrop;
end;

constructor TGroupBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AHook: TPoint; const AId: integer = ID_INVALID);
begin

   inherited Create(ABranch, ALeft, ATop, AWidth, AHeight, AId);

   FStatement.Width := 65;

   FMemoFolder := TMemo.Create(Self);
   with FMemoFolder do
   begin
      Parent := Self;
      Visible := false;
      SetBounds(3, 3, 134, 55);
      Ctl3D := false;
      Color := GSettings.GetShapeColor(shpFolder);
      Font.Assign(FStatement.Font);
      OnMouseDown := Self.OnMouseDown;
      Font.Color := clNavy;
      OnChange := Self.OnChangeMemo;
   end;

   Expanded := true;

   FFoldParms.Width := 140;
   FFoldParms.Height := 91;

   FShape := shpDiamond;
   FStatement.Color := GSettings.GetShapeColor(FShape);

   FTrueLabel := i18Manager.GetString('CaptionTrue');
   FFalseLabel := i18Manager.GetString('CaptionFalse');

   Branch := AddBranch(AHook, false);
end;

procedure TBlock.CloneFrom(ABlock: TBlock);
var
   edit, editSrc: TCustomEdit;
begin
   if ABlock <> nil then
   begin
      Visible := ABlock.Visible;
      SetFont(ABlock.Font);
      editSrc := ABlock.GetTextControl;
      edit := GetTextControl;
      if edit <> nil then
      begin
         if editSrc <> nil then
         begin
            edit.Text := editSrc.Text;
            edit.BoundsRect := editSrc.BoundsRect;
            edit.Visible := editSrc.Visible;
            edit.SelStart := editSrc.SelStart;
         end;
         if edit.CanFocus then
            edit.SetFocus;
      end;
   end;
end;

procedure TGroupBlock.CloneFrom(ABlock: TBlock);
var
   grpBlock: TGroupBlock;
   newBlock, prevBlock, block: TBlock;
   lBranch, lBranch2: TBranch;
   i: integer;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TGroupBlock then
   begin
      grpBlock := TGroupBlock(ABlock);
      FMemoFolder.Text := grpBlock.FMemoFolder.Text;
      if not grpBlock.Expanded then
      begin
         Expanded := false;
         FFoldParms := grpBlock.FFoldParms;
         Constraints.MinWidth := 140;
         Constraints.MinHeight := 54;
         Width := grpBlock.Width;
         Height := grpBlock.Height;
         FMemoFolder.SetBounds(3, 3, Width-6, Height-36);
         FMemoFolder.Anchors := [akRight, akLeft, akBottom, akTop];
         BottomPoint.X := Width div 2;
         BottomPoint.Y := Height - 30;
         IPoint.X := (Width div 2) + 30;
         IPoint.Y := FMemoFolder.Height + 15;
         TopHook.X := Width div 2;
         BottomHook := Width div 2;
         FMemoFolder.Visible := true;
      end
      else
      begin
         FFoldParms.Width := grpBlock.FFoldParms.Width;
         FFoldParms.Height := grpBlock.FFoldParms.Height;
      end;
      for i := PRIMARY_BRANCH_IND to High(grpBlock.FBranchArray) do
      begin
         lBranch := grpBlock.FBranchArray[i];
         lBranch2 := GetBranch(i);
         if lBranch2 = nil then
            lBranch2 := AddBranch(lBranch.Hook, false);
         block := lBranch.First;
         prevBlock := nil;
         while block <> nil do
         begin
            newBlock := block.Clone(lBranch2);
            lBranch2.InsertAfter(newBlock, prevBlock);
            prevBlock := lBranch2.Last;
            block := block.Next;
         end;
      end;
   end;
end;

destructor TBlock.Destroy;
var
   iter: IIterator;
begin
   iter := GetPinComments;
   while iter.HasNext do
      iter.Next.Free;
   if Self = GClpbrd.Instance then
      GClpbrd.Instance := nil;
   if Self = GClpbrd.UndoObject then
      GClpbrd.UndoObject := nil;
   GProject.UnRegister(Self);
   inherited Destroy;
end;

destructor TGroupBlock.Destroy;
var
   i: integer;
begin
   Hide;
   Page.Form.SetScrollBars;
   for i := 0 to High(FBranchArray) do
      FBranchArray[i].Free;
   FBranchArray := nil;
   inherited Destroy;
end;

procedure TBlock.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TBlock.WMExitSizeMove(var Msg: TWMMove);
begin
   inherited;
   UpdateMemoVScroll;
   UpdateMemoHScroll;
end;

procedure TBlock.CloneComments(const ASource: TBlock);
var
   iter: IIterator;
   newComment: TComment;
   unPin: boolean;
   lPage: TBlockTabSheet;
begin
   if ASource <> nil then
   begin
      lPage := Page;
      unPin := ASource.PinComments > 0;
      try
         iter := ASource.GetPinComments;
         while iter.HasNext do
         begin
            newComment := TComment(iter.Next).Clone(lPage);
            newComment.PinControl := Self;
         end;
         UnPinComments;
      finally
         if unPin then
            ASource.UnPinComments;
      end;
   end;
end;

procedure TBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   pnt: TPoint;
begin
   pnt := Point(X, Y);
   SelectBlock(pnt);
   SetCursor(pnt);
   if Rect(BottomPoint.X-5, BottomPoint.Y, BottomPoint.X+5, Height).Contains(pnt) then
   begin
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1), arrEnd, clRed);
      Ired := 0;
      Cursor := TCursor(GCustomCursor);
   end
   else if Ired = 0 then
   begin
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1));
      Ired := -1;
      Cursor := crDefault;
   end;
end;

procedure TGroupBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   i: integer;
   pnt: TPoint;
begin
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         pnt := FBranchArray[i].Hook;
         if Rect(pnt.X-5, TopHook.Y, pnt.X+5, pnt.Y).Contains(Point(X, Y)) then
         begin
            DrawArrowLine(Point(pnt.X, TopHook.Y), pnt, arrEnd, clRed);
            Ired := i;
            Cursor := TCursor(GCustomCursor);
            break;
         end
         else if Ired = i then
         begin
            DrawArrowLine(Point(pnt.X, TopHook.Y), pnt);
            Ired := -1;
            Cursor := crDefault;
            break;
         end;
      end;
   end;
   inherited MyOnMouseMove(Sender, Shift, X, Y);
end;

procedure TBlock.SetCursor(const APoint: TPoint);
begin
   if FFrame and Rect(Width-5, 0, Width, Height-5).Contains(APoint) then
      Cursor := crSizeWE
   else if FFrame and Rect(0, Height-5, Width-5, Height).Contains(APoint) then
      Cursor := crSizeNS
   else if FFrame and Rect(Width-5, Height-5, Width, Height).Contains(APoint) then
      Cursor := crSizeNWSE
   else if IsCursorResize then
      Cursor := crDefault;
end;

function TBlock.IsForeParent(const AParent: TObject): boolean;
var
   lParent: TWinControl;
begin
   result := false;
   if AParent <> nil then
   begin
      lParent := Parent;
      while lParent is TBlock do
      begin
         if lParent = AParent then
         begin
            result := true;
            break;
         end;
         lParent := lParent.Parent;
      end;
   end;
end;

procedure TBlock.MyOnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
   isShift: boolean;
   shiftState: TShiftState;
begin
   isShift := GetAsyncKeyState(VK_SHIFT) <> 0;
   if isShift then
      shiftState := [ssShift]
   else
      shiftState := [];
   MyOnMouseMove(Sender, shiftState, X, Y);
   if (Ired < 0) or (not (Source is TBlock)) or (Source is TMainBlock) or (Source is TReturnBlock) or ((not isShift) and ((Source = Self) or IsForeParent(Source))) then
      Accept := false;
end;

procedure TBlock.MyOnDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   lPage, srcPage: TBlockTabSheet;
   menuItem: TMenuItem;
   inst: TControl;
   uobj: TObject;
   lock: boolean;
begin
   if Source is TBlock then
   begin
      lock := false;
      srcPage := TBlock(Source).Page;
      srcPage.Form.pmPages.PopupComponent := TBlock(Source);
      if GetAsyncKeyState(VK_SHIFT) <> 0 then
         menuItem := srcPage.Form.miCopy
      else
      begin
         menuItem := srcPage.Form.miCut;
         lock := TBlock(Source).LockDrawing;
      end;
      inst := GClpbrd.Instance;
      uobj := GClpbrd.UndoObject;
      GClpbrd.Instance := nil;
      GClpbrd.UndoObject := nil;
      lPage := Page;
      try
         menuItem.OnClick(menuItem);
         lPage.Form.pmPages.PopupComponent := Self;
         lPage.Form.miPaste.OnClick(lPage.Form.miPaste);
      finally
         GClpbrd.Instance := inst;
         GClpbrd.UndoObject := uobj;
         if lock then
            TBlock(Source).UnLockDrawing;
      end;
   end;
end;

procedure TBlock.WMMouseLeave(var Msg: TMessage);
begin
   inherited;
   if FMouseLeave then
      OnMouseLeave
   else
      FMouseLeave := true;
end;

procedure TBlock.OnMouseLeave;
begin
   if Cursor <> crDefault then
      Cursor := crDefault;
   ClearSelection;
   if Ired = 0 then
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1));
   Ired := -1;
   if FVResize or FHResize then
      SendMessage(Handle, WM_NCHITTEST, 0, 0);
end;

procedure TGroupBlock.OnMouseLeave;
var
   pnt: TPoint;
   lBranch: TBranch;
begin
   lBranch := GetBranch(Ired);
   if lBranch <> nil then
   begin
      pnt := lBranch.Hook;
      DrawArrowLine(Point(pnt.X, TopHook.Y), pnt);
   end;
   inherited OnMouseLeave;
end;

procedure TBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if FHResize and Resize then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

procedure TGroupBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if FHResize and Resize then
   begin
      if Expanded then
         Inc(BottomPoint.X, NewWidth-Width)
      else
      begin
         BottomPoint.X := NewWidth div 2;
         TopHook.X := BottomPoint.X;
         IPoint.X := BottomPoint.X + 30;
      end;
   end;
   if FVResize and Resize then
   begin
      if Expanded then
         Inc(Branch.Hook.Y, NewHeight-Height)
      else
      begin
         IPoint.Y := NewHeight - 21;
         BottomPoint.Y := NewHeight - 30;
      end;
   end;
end;

procedure TBlock.MyOnDblClick(Sender: TObject);
begin
   if IsCursorSelect then
      ChangeFrame;
end;

procedure TBlock.ChangeFrame;
begin
   Frame := not Frame;
end;

function TGroupBlock.GenerateNestedCode(const ALines: TStringList; const ABranchInd, ADeep: integer; const ALangId: string): integer;
var
   block: TBlock;
   lBranch: TBranch;
begin
   result := 0;
   lBranch := GetBranch(ABranchInd);
   if lBranch <> nil then
   begin
      block := lBranch.First;
      while block <> nil do
      begin
         result := result + block.GenerateCode(ALines, ALangId, ADeep);
         block := block.Next;
      end;
   end;
end;

function TBlock.GetTextControl: TCustomEdit;
begin
   result := FStatement;
end;

procedure TBlock.MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   menuItem: TMenuItem;
   lPage: TBlockTabSheet;
begin
   if Button = mbLeft then
   begin
      if Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10).Contains(Point(X, Y)) then
         BeginDrag(false, 3)
      else if not IsCursorResize then
      begin          // drag entire flowchart
         ReleaseCapture;
         FTopParentBlock.BringAllToFront;
         SendMessage(FTopParentBlock.Handle, WM_SYSCOMMAND, $F012, 0);
         FTopParentBlock.OnResize(FTopParentBlock);
         if Ired >= 0 then
         begin
            lPage := Page;
            menuItem := nil;
            case GCustomCursor of
               crInstr:       menuItem := lPage.Form.miInstr;
               crMultiInstr:  menuItem := lPage.Form.miMultiInstr;
               crIfElse:      menuItem := lPage.Form.miIfElse;
               crWhile:       menuItem := lPage.Form.miWhile;
               crFor:         menuItem := lPage.Form.miFor;
               crRepeat:      menuItem := lPage.Form.miRepeat;
               crInput:       menuItem := lPage.Form.miInput;
               crOutput:      menuItem := lPage.Form.miOutput;
               crFuncCall:    menuItem := lPage.Form.miRoutineCall;
               crIf:          menuItem := lPage.Form.miIf;
               crCase:        menuItem := lPage.Form.miCase;
               crFolder:      menuItem := lPage.Form.miFolder;
               crText:        menuItem := lPage.Form.miText;
               crReturn:
               begin
                  if CanInsertReturnBlock then
                     menuItem := lPage.Form.miReturn;
               end;
            end;
            if menuItem <> nil then
            begin
               PopupMenu.PopupComponent := Self;
               menuItem.OnClick(menuItem);
            end;
         end;
      end;
   end;
end;

function TBlock.Clone(const ABranch: TBranch): TBlock;
begin
{}
end;

procedure TBlock.NCHitTest(var Msg: TWMNCHitTest);
var
   lock: boolean;
begin
   inherited;
   if GetAsyncKeyState(VK_LBUTTON) <> 0 then
   begin
      FMouseLeave := false;
      case Cursor of
         crSizeWE:
         begin
            Msg.Result := HTRIGHT;
            FHResize := true;
            BringToFront;
         end;
         crSizeNS:
         begin
            Msg.Result := HTBOTTOM;
            FVResize := true;
            BringToFront;
         end;
         crSizeNWSE:
         begin
            Msg.Result := HTBOTTOMRIGHT;
            FHResize := true;
            FVResize := true;
            BringToFront;
         end;
      end;
   end
   else if FHResize or FVResize then
   begin
      lock := LockDrawing;
      try
         if FHResize then
         begin
            if FParentBlock <> nil then
               FParentBlock.ResizeHorz(true);
            FHResize := false;
         end;
         if FVResize then
         begin
            if Self is TGroupBlock then
               TGroupBlock(Self).LinkBlocks;
            if FParentBlock <> nil then
               FParentBlock.ResizeVert(true);
            FVResize := false;
         end;
      finally
         if lock then
            UnLockDrawing;
      end;
      GChange := 1;
      if FParentBlock = nil then
         BringAllToFront;
      NavigatorForm.Invalidate;
   end;
end;

procedure TBlock.MyOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pnt: TPoint;
begin
   if Button = mbRight then
   begin
      pnt := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      FMouseLeave := false;
      PopupMenu.Popup(pnt.X, pnt.Y);
   end;
end;

procedure TBlock.SetFrame(const AValue: boolean);
begin
   if FFrame <> AValue then
   begin
      FFrame := AValue;
      GChange := 1;
      ClearSelection;
      Invalidate;
      if FFrame then
         TInfra.GetEditorForm.SelectCodeRange(Self)
      else
         TInfra.GetEditorForm.UnSelectCodeRange(Self);
   end;
end;

procedure TGroupBlock.ResizeWithDrawLock;
var
   lock: boolean;
begin
   lock := LockDrawing;
   try
      ResizeHorz(true);
      ResizeVert(true);
   finally
      if lock then
         UnlockDrawing;
   end;
end;

procedure TGroupBlock.ResizeVert(const AContinue: boolean);
begin
   Height := Branch.Height + Branch.Hook.Y + FInitParms.HeightAffix;
   LinkBlocks;
   if AContinue and (FParentBlock <> nil) then
      FParentBlock.ResizeVert(AContinue);
end;

procedure TGroupBlock.ResizeHorz(const AContinue: boolean);
var
   xLeft, xRight: integer;
   block: TBlock;
begin

   Branch.Hook.X := FInitParms.BranchPoint.X;
   TopHook.X := Branch.Hook.X;
   LinkBlocks;

   block := Branch.First;
   if block = nil then   // case if primary branch is empty
   begin
      Width := FInitParms.Width;
      BottomHook := FInitParms.BottomHook;
      BottomPoint.X := FInitParms.BottomPoint.X;
   end
   else
   begin
      // resize in left direction
      xLeft := 30;   // 30 - left margin
      repeat
         if block.Left < xLeft then
            xLeft := block.Left;
         block := block.Next;
      until block = nil;

      Branch.Hook.X := Branch.Hook.X + 30 - xLeft;
      TopHook.X := Branch.Hook.X;
      LinkBlocks;
      block := Branch.Last;
      BottomHook := block.Left + block.BottomPoint.X;

      // resize in right direction
      xRight := 0;
      block := Branch.First;
      repeat
         if block.BoundsRect.Right > xRight then
            xRight := block.BoundsRect.Right;
         block := block.Next;
      until block = nil;

      SetWidth(xRight);  // set final width
   end;

   if FParentBlock <> nil then
   begin
      if AContinue then
         FParentBlock.ResizeHorz(AContinue);
   end
   else if GSettings.ShowFuncLabels then
      RepaintAll;

end;

procedure TGroupBlock.SetWidth(const AMinX: integer);
begin
{}
end;

procedure TGroupBlock.LinkBlocks(const idx: integer = PRIMARY_BRANCH_IND-1);
var
   block, blockPrev: TBlock;
   i, first, last: integer;
   topLeft: TPoint;
begin
   if GetBranch(idx) <> nil then
   begin
      first := idx;
      last := idx;
   end
   else
   begin
      first := PRIMARY_BRANCH_IND;
      last := High(FBranchArray);
   end;
   for i := first to last do
   begin
      block := FBranchArray[i].First;
      if block <> nil then
      begin
         topLeft := Point(FBranchArray[i].Hook.X-block.TopHook.X, FBranchArray[i].Hook.Y+1);
         block.SetBounds(topLeft.X, topLeft.Y, block.Width, block.Height);
         block := block.Next;
         while block <> nil do
         begin
            blockPrev := block.Prev;
            topLeft := Point(blockPrev.BottomPoint.X+blockPrev.Left-block.TopHook.X, blockPrev.BoundsRect.Bottom);
            block.SetBounds(topLeft.X, topLeft.Y, block.Width, block.Height);
            block := block.Next;
         end;
      end;
   end;
end;

function TGroupBlock.GetFoldedText: string;
begin
   result := FMemoFolder.Text;
end;

procedure TGroupBlock.SetFoldedText(const AText: string);
begin
   FMemoFolder.Text := AText;
end;

procedure TBlock.SetPage(APage: TBlockTabSheet);
begin
end;

function TBlock.GetPage: TBlockTabSheet;
begin
   result := FTopParentBlock.Page;
end;

procedure TBlock.SetFontStyle(const AStyle: TFontStyles);
var
   i: integer;
begin
   Font.Style := AStyle;
   Refresh;
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i] is TBlock then
         TBlock(Controls[i]).SetFontStyle(AStyle)
      else
         THackControl(Controls[i]).Font.Style := AStyle;
   end;
end;

procedure TBlock.SetFontSize(const ASize: integer);
var
   i: integer;
begin
   Font.Size := ASize;
   Refresh;
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i] is TBlock then
         TBlock(Controls[i]).SetFontSize(ASize)
      else
         TInfra.SetFontSize(Controls[i], ASize);
   end;
   PutTextControls;
end;

function TBlock.GetFont: TFont;
begin
   result := Font;
end;

procedure TBlock.SetFont(const AFont: TFont);
var
   i: integer;
begin
   Font.Assign(AFont);
   Refresh;
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i] is TBlock then
      begin
         TBlock(Controls[i]).SetFontStyle(AFont.Style);
         TBlock(Controls[i]).SetFontSize(AFont.Size);
      end
      else
      begin
         THackControl(Controls[i]).Font.Style := AFont.Style;
         TInfra.SetFontSize(Controls[i], AFont.Size);
      end;
   end;
   PutTextControls;
end;

function TBlock.GetComments(const AInFront: boolean = false): IIterator;
var
   comment: TComment;
   iterc: IIterator;
   isFront: boolean;
   lPage: TTabSheet;
   objList: TObjectList;
begin
   objList := TObjectList.Create(false);
   if Visible then
   begin
      lPage := Page;
      iterc := GProject.GetComments;
      while iterc.HasNext do
      begin
         comment := TComment(iterc.Next);
         if comment.Page = lPage then
         begin
            if AInFront then
               isFront := IsInFront(comment)
            else
               isFront := true;
            if isFront and (comment.PinControl = nil) and ClientRect.Contains(ParentToClient(comment.BoundsRect.TopLeft, lPage)) then
               objList.Add(comment);
         end
      end;
   end;
   result := TCommentIterator.Create(objList);
end;

procedure TBlock.BringAllToFront;
var
   iter: IIterator;
begin
   BringToFront;
   iter := GetComments;
   while iter.HasNext do
      TComment(iter.Next).BringToFront;
end;

function TBlock.IsInFront(const AControl: TWinControl): boolean;
var
   hnd: THandle;
begin
   result := false;
   if AControl <> nil then
   begin
      hnd := GetWindow(AControl.Handle, GW_HWNDLAST);
      while hnd <> 0 do
      begin
         if hnd = FTopParentBlock.Handle then
         begin
            result := true;
            break;
         end
         else if hnd = AControl.Handle then
            break;
         hnd := GetNextWindow(hnd, GW_HWNDPREV);
      end;
   end;
end;

procedure TBlock.MoveComments(x, y: integer);
var
   iter: IIterator;
   comment: TComment;
begin
   if (x <> 0) and (y <> 0) and (Left <> 0) and (Top <> 0) and ((x <> Left) or (y <> Top)) then
   begin
      iter := GetComments(true);
      while iter.HasNext do
      begin
         comment := TComment(iter.Next);
         if comment.Visible then
         begin
            comment.SetBounds(comment.Left+x-Left, comment.Top+y-Top, comment.Width, comment.Height);
            comment.BringToFront;
         end;
      end;
   end;
end;

procedure TBlock.OnWindowPosChanged(x, y: integer);
begin
   MoveComments(x, y);
end;

procedure TBlock.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
begin
   OnWindowPosChanged(Msg.WindowPos.x, Msg.WindowPos.y);
   inherited;
end;

function TBlock.GetPinComments: IIterator;
var
   comment: TComment;
   iterc: IIterator;
   objList: TObjectList;
begin
   objList := TObjectList.Create(false);
   iterc := GProject.GetComments;
   while iterc.HasNext do
   begin
      comment := TComment(iterc.Next);
      if comment.PinControl = Self then
         objList.Add(comment);
   end;
   result := TCommentIterator.Create(objList);
end;

procedure TBlock.PutTextControls;
begin
end;

function TGroupBlock.GetDiamondPoint: TPoint;
begin
   result := Point(-1, -1);
end;

procedure TGroupBlock.PutTextControls;
var
   topLeft, pnt: TPoint;
   textControl: TCustomEdit;
begin
   textControl := GetTextControl;
   pnt := GetDiamondPoint;
   if (textControl <> nil) and not InvalidPoint(pnt) then
   begin
      topLeft.X := textControl.Height + pnt.X - 60 + 4;
      topLeft.Y := Trunc((60-textControl.Height)/2) + pnt.Y + 2;
      textControl.SetBounds(topLeft.X, topLeft.Y, 120-2*textControl.Height-7, textControl.Height);
   end
end;

procedure TBlock.RefreshStatements;
var
   i: integer;
   b, b1: boolean;
begin
    b := NavigatorForm.InvalidateInd;
    NavigatorForm.InvalidateInd := false;
    b1 := FRefreshMode;
    FRefreshMode := true;
    try
       for i := 0 to ControlCount-1 do
       begin
          if Controls[i] is TStatement then
             TStatement(Controls[i]).DoEnter
          else if (Controls[i] is TMemo) and Assigned(TMemo(Controls[i]).OnChange) then
             TMemo(Controls[i]).OnChange(Controls[i])
          else if (Controls[i] is TEdit) and Assigned(TEdit(Controls[i]).OnChange) then
             TEdit(Controls[i]).OnChange(Controls[i])
          else if (Controls[i] is TBlock) and (Controls[i] <> GClpbrd.UndoObject) then
             TBlock(Controls[i]).RefreshStatements;
       end;
    finally
       FRefreshMode := b1;
    end;
    NavigatorForm.InvalidateInd := b;
end;

function TBlock.GetId: integer;
begin
   result := FId;
end;

procedure TBlock.ChangeColor(const AColor: TColor);
var
   iter: IIterator;
   comment: TComment;
   lEdit: THackCustomEdit;
   lColor: TColor;
begin
   Color := AColor;
   iter := GetComments;
   while iter.HasNext do
   begin
      comment := TComment(iter.Next);
      if comment.Visible then
         comment.Color := AColor;
   end;
   lEdit := THackCustomEdit(GetTextControl);
   if lEdit <> nil then
   begin
      lColor := GSettings.GetShapeColor(FShape);
      if lColor = GSettings.DesktopColor then
         lEdit.Color := AColor
      else
         lEdit.Color := lColor;
   end;
end;

procedure TBlock.ClearSelection;
var
   lColor: TColor;
begin
   lColor := Page.Brush.Color;
   if Color <> lColor then
      ChangeColor(lColor);
   NavigatorForm.Invalidate;
end;

procedure TGroupBlock.ChangeColor(const AColor: TColor);
var
   i: integer;
   block: TBlock;
   lColor: TColor;
begin
   inherited ChangeColor(AColor);
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         block := FBranchArray[i].First;
         while block <> nil do
         begin
            block.ChangeColor(AColor);
            block := block.Next;
         end;
      end;
   end;
   lColor := GSettings.GetShapeColor(shpFolder);
   if lColor = GSettings.DesktopColor then
      FMemoFolder.Color := AColor
   else
      FMemoFolder.Color := lColor;
end;

procedure TBlock.SelectBlock(const APoint: TPoint);
begin
   if Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10).Contains(APoint) then
   begin
      if Color <> GSettings.HighlightColor then
      begin
         ChangeColor(GSettings.HighlightColor);
         if GSettings.EditorAutoSelectBlock then
            TInfra.GetEditorForm.SelectCodeRange(Self);
         NavigatorForm.Invalidate;
      end;
   end
   else if Color <> Page.Brush.Color then
   begin
      ChangeColor(Page.Brush.Color);
      if GSettings.EditorAutoSelectBlock and not FFrame then
         TInfra.GetEditorForm.UnSelectCodeRange(Self);
      NavigatorForm.Invalidate;
   end;
end;

procedure TGroupBlock.ExpandAll;
var
   i: integer;
   block: TBlock;
begin
   if not Expanded then
      ExpandFold(true);
   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      block := FBranchArray[i].First;
      while block <> nil do
      begin
         if block is TGroupBlock then
            TGroupBlock(block).ExpandAll;
         block := block.Next;
      end;
   end;
end;

procedure TBlock.RepaintAll;
var
   i: integer;
begin
   Repaint;
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i] is TBlock then
         TBlock(Controls[i]).RepaintAll
      else
         Controls[i].Repaint;
   end;
end;

function TGroupBlock.HasFoldedBlocks: boolean;
var
   block: TBlock;
   i: integer;
begin
   result := not Expanded;
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         block := FBranchArray[i].First;
         while block <> nil do
         begin
            if block is TGroupBlock then
            begin
               result := TGroupBlock(block).HasFoldedBlocks;
               if result then break;
            end;
            block := block.Next;
         end;
         if result then break;
      end;
   end;
end;

procedure TBlock.Paint;
begin
   inherited;
   with Canvas do
   begin
      Font.Assign(Self.Font);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      PatBlt(Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom, PATCOPY);
      Pen.Color := clBlack;
      Pen.Width := 1;
      if FFrame then
      begin
         Pen.Style := psDashDot;
         PolyLine([TPoint.Zero, Point(Width-1, 0), Point(Width-1, Height-1), Point(0, Height-1), TPoint.Zero]);
         Pen.Style := psSolid;
      end;
   end;
end;

procedure TBlock.DrawI;
var
   fontSize: integer;
begin
   if Page.DrawI then
   begin
      fontSize := Canvas.Font.Size;
      Canvas.Font.Size := 8;
      DrawTextLabel(IPoint.X, IPoint.Y, '|');
      Canvas.Font.Size := fontSize;
   end;
end;

procedure TBlock.DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
var
   fontName: string;
   fontSize: integer;
   fontStyles: TFontStyles;
begin
   if GSettings.ShowBlockLabels and not AText.IsEmpty then
   begin
      fontName := Canvas.Font.Name;
      fontStyles := Canvas.Font.Style;
      Canvas.Font.Name := GInfra.CurrentLang.LabelFontName;
      Canvas.Font.Style := [fsBold];
      fontSize := Canvas.Font.Size;
      Canvas.Font.Size := GInfra.CurrentLang.LabelFontSize;
      DrawTextLabel(x, y, AText, rightJust, downJust);
      Canvas.Font.Name := fontName;
      Canvas.Font.Size := fontSize;
      Canvas.Font.Style := fontStyles;
   end;
end;

procedure TBlock.DrawTextLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
var
   fontStyles: TFontStyles;
begin
   if not AText.IsEmpty then
   begin
      fontStyles := Canvas.Font.Style;
      Canvas.Font.Style := [];
      if fsBold in fontStyles then
         Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.Brush.Style := bsClear;
      if rightJust then
      begin
         x := x - Canvas.TextWidth(AText);
         if x < 0 then
            x := 0;
      end;
      if downJust then
      begin
         y := y - Canvas.TextHeight('X');
         if y < 0 then
            y := 0;
      end;
      Canvas.TextOut(x, y, AText);
      Canvas.Font.Style := fontStyles;
   end;
end;

procedure TBlock.DrawArrowLine(const ABeginPoint, AEndPoint: TPoint; const AArrowPos: TArrowPosition = arrEnd; const AColor: TColor = clBlack);
const
   MX: array[boolean, boolean] of integer = ((10, -10), (-5, -5));
   MY: array[boolean, boolean] of integer = ((5, 5), (10, -10));
   MD: array[boolean, boolean] of integer = ((0, -10), (10, 0));
var
   isVert, toBtmRigth: boolean;
   x, y: integer;
   pnt: TPoint;
begin
   isVert := ABeginPoint.X = AEndPoint.X;
   pnt := AEndPoint;
   if isVert then
      toBtmRigth := AEndPoint.Y > ABeginPoint.Y
   else
      toBtmRigth := AEndPoint.X > ABeginPoint.X;
   if AArrowPos = arrMiddle then
   begin
      if isVert then
         pnt.Y := pnt.Y + (ABeginPoint.Y-AEndPoint.Y) div 2
      else
         pnt.X := pnt.X + (ABeginPoint.X-AEndPoint.X) div 2;
   end;
   x := MX[isVert, toBtmRigth];
   y := MY[isVert, toBtmRigth];
   with Canvas do
   begin
      Brush.Style := bsSolid;
      Pen.Color := AColor;
      Brush.Color := AColor;
      MoveTo(ABeginPoint.X, ABeginPoint.Y);
      LineTo(AEndPoint.X, AEndPoint.Y);
      Polygon([Point(pnt.X+x, pnt.Y+y),
               Point(pnt.X+x+MD[isVert, false], pnt.Y+y+MD[isVert, true]),
               pnt,
               Point(pnt.X+x, pnt.Y+y)]);
   end;
end;

function TBlock.GetEllipseTextRect(const APoint: TPoint; const AText: string): TRect;
const
   MIN_HALF_HEIGHT = 15;
   MIN_HALF_WIDTH = 30;
var
   a, b: integer;
   ar, br, cx, cy: single;
   R: TRect;
   wndExt, viewPort: TSize;
begin
   GetWindowExtEx(Canvas.Handle, wndExt);
   GetViewportExtEx(Canvas.Handle, viewPort);
   cx := viewPort.cx / wndExt.cx;
   cy := viewPort.cy / wndExt.cy;
   R := TRect.Empty;
   DrawText(Canvas.Handle, PChar(AText), -1, R, DT_CALCRECT);
   ar := R.Height * cy / Sqrt(2);
   br := R.Width * cx / Sqrt(2);
   if ar < MIN_HALF_HEIGHT then
   begin
      if IsZero(ar) then
         br := MIN_HALF_WIDTH
      else
         br := MIN_HALF_HEIGHT * br / ar;
      ar := MIN_HALF_HEIGHT;
   end;
   {if br < MIN_HALF_WIDTH then
   begin
      if br = 0 then
         ar := MIN_HALF_HEIGHT
      else
         ar := MIN_HALF_WIDTH * ar / br;
      br := MIN_HALF_WIDTH;
   end;}
   a := Round(ar);
   b := Round(br);
   result := Rect(APoint.X-b, APoint.Y-2*a, APoint.X+b, APoint.Y);
end;

function TBlock.DrawEllipsedText(const APoint: TPoint; const AText: string): TRect;
var
   lColor: TColor;
begin
   result := TRect.Empty;
   if not InvalidPoint(APoint) then
   begin
      result := GetEllipseTextRect(APoint, AText);
      Canvas.Brush.Style := bsClear;
      lColor := GSettings.GetShapeColor(shpEllipse);
      if lColor <> GSettings.DesktopColor then
         Canvas.Brush.Color := lColor;
      Canvas.Ellipse(result);
      DrawText(Canvas.Handle, PChar(AText), -1, result, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
   end;
end;

function TBlock.GetFrontMemo: TMemo;
begin
   result := nil;
end;

function TGroupBlock.GetFrontMemo: TMemo;
begin
   result := nil;
   if not Expanded then
      result := FMemoFolder;
end;

function TBlock.CountErrWarn: TErrWarnCount;
var
   textControl: TCustomEdit;
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   textControl := GetTextControl;
   if textControl <> nil then
   begin
      if THackControl(textControl).Font.Color = NOK_COLOR then
         result.ErrorCount := 1
      else if THackControl(textControl).Font.Color = WARN_COLOR then
         result.WarningCount := 1;
   end;
end;

function TGroupBlock.CountErrWarn: TErrWarnCount;
var
   iter: IIterator;
   errWarnCount: TErrWarnCount;
begin
   result := inherited CountErrWarn;
   iter := GetBlocks;
   while iter.HasNext do
   begin
      errWarnCount := TBlock(iter.Next).CountErrWarn;
      Inc(result.ErrorCount, errWarnCount.ErrorCount);
      Inc(result.WarningCount, errWarnCount.WarningCount);
   end;
end;

procedure TGroupBlock.Paint;
var
   pnt: TPoint;
   brushStyle: TBrushStyle;
   lColor, lColor2: TColor;
   w: integer;
begin
   inherited;
   brushStyle := Canvas.Brush.Style;
   lColor := Canvas.Brush.Color;
   w := Canvas.Pen.Width;
   if Expanded then
   begin
      pnt := GetDiamondPoint;
      if not InvalidPoint(pnt) then
      begin
         Canvas.Brush.Style := bsClear;
         lColor2 := GSettings.GetShapeColor(FShape);
         if lColor2 <> GSettings.DesktopColor then
            Canvas.Brush.Color := lColor2;
         Canvas.Polygon([Point(pnt.X-60, pnt.Y+30),
                         Point(pnt.X, pnt.Y+60),
                         Point(pnt.X+60, pnt.Y+30),
                         pnt,
                         Point(pnt.X-60, pnt.Y+30)]);
         end;
   end
   else if FMemoFolder <> nil then
   begin
      if FTopParentBlock <> Self then
         DrawArrowLine(Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
      Canvas.Pen.Width := 2;
      Canvas.Brush.Style := bsClear;
      lColor2 := GSettings.GetShapeColor(shpFolder);
      if lColor2 <> GSettings.DesktopColor then
         Canvas.Brush.Color := lColor2;
      Canvas.Polygon([Point(1, 1),
                      Point(Width-1, 1),
                      Point(Width-1, FMemoFolder.Height+5),
                      Point(1, FMemoFolder.Height+5),
                      Point(1, 0)]);
   end;
   Canvas.Brush.Style := brushStyle;
   Canvas.Brush.Color := lColor;
   Canvas.Pen.Width := w;
end;

// return value indicates if drawing was in fact locked by this call
// it may not since it's already locked by other block before
function TBlock.LockDrawing: boolean;
begin
   result := false;
   if not FTopParentBlock.FDrawingFlag then
   begin
      FTopParentBlock.FDrawingFlag := true;
      result := true;
      SendMessage(FTopParentBlock.Handle, WM_SETREDRAW, WPARAM(False), 0);
   end;
end;

procedure TBlock.UnLockDrawing;
begin
   if FTopParentBlock.FDrawingFlag then
   begin
      SendMessage(FTopParentBlock.Handle, WM_SETREDRAW, WPARAM(True), 0);
      GProject.RepaintFlowcharts;
      GProject.RepaintComments;
      RedrawWindow(Page.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE);
      FTopParentBlock.FDrawingFlag := false;
   end;
end;

function TBlock.CanInsertReturnBlock: boolean;
begin
   result := (Ired = 0) and (FParentBranch <> nil) and (FParentBranch.Last = Self);
end;

function TGroupBlock.CanInsertReturnBlock: boolean;
var
   lBranch: TBranch;
begin
   if Ired = 0 then
      result := (FParentBranch <> nil) and (FParentBranch.Last = Self)
   else
   begin
      lBranch := GetBranch(Ired);
      result := (lBranch <> nil) and (lBranch.Last = nil);
   end;
end;

procedure TBlock.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
var
   pnt: TPoint;
   lPage: TTabSheet;
begin
   inherited;
   lPage := Page;
   pnt := ClientToParent(TPoint.Zero, lPage);
   if FHResize then
      Msg.MinMaxInfo.ptMaxTrackSize.X := lPage.ClientWidth - pnt.X;
   if FVResize then
      Msg.MinMaxInfo.ptMaxTrackSize.Y := lPage.ClientHeight - pnt.Y;
end;

function TBlock.IsCursorSelect: boolean;
begin
   result := Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10).Contains(ScreenToClient(Mouse.CursorPos));
end;

function TBlock.IsCursorResize: boolean;
begin
   result := -Cursor in [-crSizeWE, -crSizeNS, -crSizeNWSE];
end;

procedure TBlock.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
   Msg.Result := 1;
end;

function TGroupBlock.GetBranch(const idx: integer): TBranch;
begin
   result := nil;
   if (idx >= PRIMARY_BRANCH_IND) and (idx <= High(FBranchArray)) then
      result := FBranchArray[idx];
end;

procedure TBlock.MyOnChange(Sender: TObject);
begin
   NavigatorForm.DoInvalidate;
end;

procedure TBlock.OnChangeMemo(Sender: TObject);
begin
   UpdateMemoVScroll;
   UpdateMemoHScroll;
   NavigatorForm.DoInvalidate;
end;

procedure TBlock.SetMemoVScroll(AValue: boolean);
begin
   if AValue <> FMemoVScroll then
   begin
      FMemoVScroll := AValue;
      UpdateMemoVScroll;
   end;
end;

procedure TBlock.UpdateMemoVScroll;
var
   pos, count, lineCount: integer;
   memo: TMemo;
   oldFont: HFont;
   hnd: THandle;
   txtMetric: TTextMetric;
begin
   memo := GetFrontMemo;
   if memo <> nil then
   begin
      pos := memo.SelStart;
      if FMemoVScroll then
      begin
         hnd := GetDC(memo.Handle);
         try
            oldFont := SelectObject(hnd, memo.Font.Handle);
            try
               GetTextMetrics(hnd, txtMetric);
               lineCount := (memo.ClientHeight - 4)  div (txtMetric.tmHeight + txtMetric.tmExternalLeading)
            finally
               SelectObject(hnd, oldFont);
            end;
         finally
            ReleaseDC(memo.Handle, hnd);
         end;
         count := memo.Lines.Count;
         if EndsText(sLineBreak, memo.Text) then
            count := count + 1;
         if count > lineCount then
         begin
            if memo.ScrollBars = TScrollStyle.ssNone then
               memo.ScrollBars := TScrollStyle.ssVertical
            else if memo.ScrollBars = TScrollStyle.ssHorizontal then
               memo.ScrollBars := TScrollStyle.ssBoth;
         end
         else
            ResetMemoScrollBars(TScrollStyle.ssHorizontal, memo);
      end
      else
         ResetMemoScrollBars(TScrollStyle.ssHorizontal, memo);
      memo.SelStart := pos;
   end;
end;

procedure TBlock.SetMemoHScroll(AValue: boolean);
begin
   if AValue <> FMemoHScroll then
   begin
      FMemoHScroll := AValue;
      if FMemoHScroll then
         MemoWordWrap := false;
      UpdateMemoHScroll;
   end;
end;

procedure TBlock.UpdateMemoHScroll;
var
   pos, cnt, w, i: integer;
   memo: TMemo;
   lCanvas: TCanvas;
   margns: longint;
begin
   memo := GetFrontMemo;
   if memo <> nil then
   begin
      pos := memo.SelStart;
      if FMemoHScroll and not memo.WordWrap then
      begin
         w := 0;
         lCanvas := TCanvas.Create;
         try
            lCanvas.Font.Assign(memo.Font);
            lCanvas.Handle := GetDC(memo.Handle);
            for i := 0 to memo.Lines.Count-1 do
            begin
               cnt := lCanvas.TextWidth(memo.Lines[i]);
               if cnt > w then
                  w := cnt;
            end;
            margns := SendMessage(memo.Handle, EM_GETMARGINS, 0, 0);
            if w > (memo.ClientWidth - HiWord(margns) - LoWord(margns) - 3) then
            begin
               if memo.ScrollBars = TScrollStyle.ssNone then
                  memo.ScrollBars := TScrollStyle.ssHorizontal
               else if memo.ScrollBars = TScrollStyle.ssVertical then
                  memo.ScrollBars := TScrollStyle.ssBoth;
            end
            else
               ResetMemoScrollBars(TScrollStyle.ssVertical, memo);
         finally
            ReleaseDC(memo.Handle, lCanvas.Handle);
            lCanvas.Free;
         end;
      end
      else
         ResetMemoScrollBars(TScrollStyle.ssVertical, memo);
      memo.SelStart := pos;
   end;
end;

procedure TBlock.ResetMemoScrollBars(const AStyle: TScrollStyle; const AMemo: TMemo);
var
   sStyle: TScrollStyle;
begin
   if AStyle = TScrollStyle.ssVertical then
      sStyle := TScrollStyle.ssHorizontal
   else
      sStyle := TScrollStyle.ssVertical;
   if AMemo.ScrollBars = TScrollStyle.ssBoth then
      AMemo.ScrollBars := AStyle
   else if AMemo.ScrollBars = sStyle then
      AMemo.ScrollBars := TScrollStyle.ssNone;
end;

procedure TBlock.SetMemoWordWrap(AValue: boolean);
var
   memo: TMemo;
begin
   memo := GetFrontMemo;
   if (memo <> nil) and (AValue <> memo.WordWrap) then
   begin
      memo.WordWrap := AValue;
      if memo.WordWrap then
         MemoHScroll := false;
   end;
end;

function TBlock.GetMemoWordWrap: boolean;
var
   memo: TMemo;
begin
   memo := GetFrontMemo;
   result := (memo <> nil) and memo.WordWrap;
end;

function TBlock.CanBeFocused: boolean;
var
   lParent: TGroupBlock;
   func: TUserFunction;
begin
   result := true;
   lParent := FParentBlock;
   while lParent <> nil do
   begin
      if not lParent.Expanded then
      begin
         result := false;
         break;
      end;
      lParent := lParent.ParentBlock;
   end;
   if result then
   begin
      func := TUserFunction(TMainBlock(FTopParentBlock).UserFunction);
      if func <> nil then
      begin
         result := func.Active;
         if result and (func.Header <> nil) then
            result := func.Header.chkBodyVisible.Checked;
      end;
      if result and (FParentBranch <> nil) and (FParentBranch.IndexOf(Self) = -1) then
         result := false;
   end;
end;

function TGroupBlock.CanBeFocused: boolean;
begin
   result := Expanded;
   if result then
      result := inherited CanBeFocused;
end;

function TBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   lPage: TBlockTabSheet;
begin
   if AInfo.FocusEdit = nil then
      AInfo.FocusEdit := GetTextControl;
   lPage := Page;
   AInfo.FocusEditForm := lPage.Form;
   lPage.PageControl.ActivePage := lPage;
   result := FocusOnTextControl(AInfo);
end;

function TBlock.FocusOnTextControl(AInfo: TFocusInfo): boolean;
var
   idx, idx2, i: integer;
   txt: string;
   memo: TMemo;
begin
   result := false;
   if ContainsControl(AInfo.FocusEdit) and AInfo.FocusEdit.CanFocus then
   begin
      Page.Show;
      FTopParentBlock.BringAllToFront;
      Page.Form.ScrollInView(AInfo.FocusEdit);
      idx2 := 0;
      if AInfo.FocusEdit is TMemo then
      begin
         memo := TMemo(AInfo.FocusEdit);
         if AInfo.RelativeLine < memo.Lines.Count then
         begin
            txt := memo.Lines[AInfo.RelativeLine];
            if AInfo.RelativeLine > 0 then
            begin
               for i := 0 to AInfo.RelativeLine-1 do
                  idx2 := idx2 + (memo.Lines[i] + sLineBreak).Length;
            end;
         end
         else
            txt := memo.Text;
      end
      else
         txt := AInfo.FocusEdit.Text;
      idx := Pos(txt, AInfo.LineText);
      if idx <> 0 then
         AInfo.SelStart := AInfo.SelStart - idx + idx2
      else
      begin
         idx := Pos(AInfo.SelText, txt);
         if idx <> 0 then
            AInfo.SelStart := idx - 1 + idx2;
      end;
      AInfo.FocusEditCallBack := UpdateEditor;
      TFlashThread.Create(AInfo);
      result := true;
   end;
end;

function TBlock.GetErrorMsg(AEdit: TCustomEdit): string;
var
   lColor: TColor;
   i: integer;
begin
   result := '';
   if AEdit <>  nil then
   begin
      lColor := THackControl(AEdit).Font.Color;
      if TInfra.IsNOkColor(lColor) then
      begin
         result := AEdit.Hint;
         i := LastDelimiter(sLineBreak, result);
         if i > 0 then
            result := ' - ' + Copy(result, i+1, MaxInt);
      end;
   end;
end;

function TBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   errMsg, descTemplate: string;
   textControl: TCustomEdit;
begin
   result := AParentNode;
   textControl := GetTextControl;
   if textControl <> nil then
   begin
      errMsg := GetErrorMsg(textControl);
      descTemplate := GetDescTemplate(GInfra.CurrentLang.Name);
      result := AParentNode.Owner.AddChildObject(AParentNode, FillTemplate(GInfra.CurrentLang.Name, descTemplate) + errMsg, textControl);
      if not errMsg.IsEmpty then
      begin
         AParentNode.MakeVisible;
         AParentNode.Expand(false);
      end;
   end;
end;

function TGroupBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   block: TBlock;
begin
   result := inherited GenerateTree(AParentNode);
   block := FBranchArray[PRIMARY_BRANCH_IND].First;
   while block <> nil do
   begin
      block.GenerateTree(result);
      block := block.Next;
   end;
end;

function TGroupBlock.AddBranch(const AHook: TPoint; const AResizeInd: boolean; const ABranchId: integer = ID_INVALID; const ABranchStmntId: integer = ID_INVALID): TBranch;
var
   len: integer;
begin
   result := TBranch.Create(Self, AHook, ABranchId);
   len := Length(FBranchArray);
   if len = 0 then
      len := 1;
   SetLength(FBranchArray, len+1);
   FBranchArray[len] := result;
end;

function TBlock.CanBeRemoved: boolean;
begin
   result := Visible;
end;

function TBlock.GetUndoObject: TObject;
begin
   result := Self;
end;

function TBlock.Remove: boolean;
begin
   result := CanBeRemoved;
   if result then
   begin
      GClpbrd.UndoObject.Free;
      ClearSelection;
      SetVisible(false);
      if FParentBranch <> nil then
      begin
         FParentBranch.Remove(Self);
         if FParentBlock <> nil then
            FParentBlock.ResizeWithDrawLock;
      end;
      GClpbrd.UndoObject := GetUndoObject;
      TInfra.UpdateCodeEditor;
      NavigatorForm.Repaint;
   end;
end;

function TBlock.IsBoldDesc: boolean;
begin
   result := false;
end;

function TBlock.PinComments: integer;
var
   iter: IIterator;
   comment: TComment;
   pnt: TPoint;
begin
   pnt := ClientToParent(ClientRect.TopLeft, Page);
   iter := GetComments;
   while iter.HasNext do
   begin
      comment := TComment(iter.Next);
      comment.PinControl := Self;
      comment.Visible := false;
      comment.SetBounds(comment.Left - pnt.X, comment.Top - pnt.Y, comment.Width, comment.Height);
   end;
   result := iter.Count;
end;

function TBlock.UnPinComments: integer;
var
   iter: IIterator;
   comment: TComment;
   pnt: TPoint;
begin
   pnt := ClientToParent(ClientRect.TopLeft, Page);
   iter := GetPinComments;
   while iter.HasNext do
   begin
      comment := TComment(iter.Next);
      comment.PinControl := nil;
      comment.SetBounds(comment.Left + pnt.X, comment.Top + pnt.Y, comment.Width, comment.Height);
      comment.Visible := true;
      comment.BringToFront;
   end;
   result := iter.Count;
end;

function TGroupBlock.UnPinComments: integer;
begin
   result := 0;
   if Expanded then
      result := inherited UnPinComments;
end;

procedure TBlock.SetVisible(const AVisible: boolean; const ASetComments: boolean = true);
begin
   if Visible <> AVisible then
   begin
      if ASetComments then
      begin
         if AVisible then
            UnPinComments
         else
            PinComments;
      end;
      Visible := AVisible;
   end;
end;

function TBlock.ExportToXMLFile(const AFile: string): TErrorType;
begin
   result := TXMLProcessor.ExportToXMLFile(ExportToXMLTag, AFile);
end;

procedure TGroupBlock.SetVisible(const AVisible: boolean; const ASetComments: boolean = true);
begin
   inherited SetVisible(AVisible, Expanded);
end;

function TBlock.ProcessComments: boolean;
begin
   result := (FParentBlock = nil) or not FParentBlock.BlockImportMode;
end;

procedure TGroupBlock.ExpandFold(const AResize: boolean);
var
   tmpWidth, tmpHeight, i: integer;
   block: TBlock;
   textControl: TCustomEdit;
begin
   GChange := 1;
   Expanded := not Expanded;
   textControl := GetTextControl;
   if textControl <> nil then
      textControl.Visible := Expanded;
   FMemoFolder.Visible := not Expanded;

   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      block := FBranchArray[i].First;
      while block <> nil do
      begin
         block.Visible := Expanded;
         block := block.Next;
      end;
   end;

   if Expanded then
   begin
      tmpWidth := Width;
      tmpHeight := Height;
      Width := FFoldParms.Width;
      Height := FFoldParms.Height;
      FFoldParms.Width := tmpWidth;
      FFoldParms.Height := tmpHeight;
      BottomHook := FFoldParms.BottomHook;
      TopHook.X := FFoldParms.TopHook;
      BottomPoint.X := FFoldParms.BottomPoint.X;
      BottomPoint.Y := FFoldParms.BottomPoint.Y;
      Branch.Hook.X := FFoldParms.BranchPoint.X;
      IPoint.X := FFoldParms.IPoint.X;
      IPoint.Y := FFoldParms.IPoint.Y;
      Constraints.MinWidth := FInitParms.Width;
      Constraints.MinHeight := FInitParms.Height;
      RefreshStatements;
   end
   else
   begin
      if ProcessComments then
         PinComments;
      tmpWidth := FFoldParms.Width;
      tmpHeight := FFoldParms.Height;
      FFoldParms.Width := Width;
      FFoldParms.Height := Height;
      FFoldParms.BottomHook := BottomHook;
      FFoldParms.TopHook := TopHook.X;
      FFoldParms.BottomPoint.X := BottomPoint.X;
      FFoldParms.BottomPoint.Y := BottomPoint.Y;
      FFoldParms.BranchPoint.X := Branch.Hook.X;
      FFoldParms.IPoint.X := IPoint.X;
      FFoldParms.IPoint.Y := IPoint.Y;
      Constraints.MinWidth := 140;
      Constraints.MinHeight := 54;
      Width := tmpWidth;
      Height := tmpHeight;
      FMemoFolder.SetBounds(3, 3, Width-6, Height-36);
      FMemoFolder.Anchors := [akRight, akLeft, akBottom, akTop];
      BottomPoint.X := Width div 2;
      BottomPoint.Y := Height - 30;
      IPoint.X := (Width div 2) + 30;
      IPoint.Y := FMemoFolder.Height + 15;
      TopHook.X := Width div 2;
      BottomHook := Width div 2;
   end;

   if AResize and (FParentBlock <> nil) then
   begin
      FParentBlock.ResizeWithDrawLock;
      NavigatorForm.Invalidate;
   end;
   
   UnPinComments;
end;

procedure TGroupBlock.SaveInXML(const ATag: IXMLElement);
var
   brx, fw, fh, i: integer;
   txt: string;
   tag1, tag2: IXMLElement;
   lBranch: TBranch;
   it: IIterator;
   unPin: boolean;
begin
   SaveInXML2(ATag);
   if ATag <> nil then
   begin
      unPin := false;
      if Expanded then
      begin
         fw := FFoldParms.Width;
         fh := FFoldParms.Height;
         brx := Branch.Hook.X;
         unPin := PinComments > 0;
      end
      else
      begin
         fw := Width;
         fh := Height;
         brx := FFoldParms.BranchPoint.X;
         ATag.SetAttribute('h', FFoldParms.Height.ToString);
         ATag.SetAttribute('w', FFoldParms.Width.ToString);
         ATag.SetAttribute('bh', FFoldParms.BottomHook.ToString);
      end;

      try
         ATag.SetAttribute('brx', brx.ToString);
         ATag.SetAttribute('bry', Branch.Hook.Y.ToString);
         ATag.SetAttribute('fw', fw.ToString);
         ATag.SetAttribute('fh', fh.ToString);
         ATag.SetAttribute(FOLDED_ATTR, (not Expanded).ToString);

         txt := GetFoldedText;
         if not txt.IsEmpty then
         begin
            tag1 := ATag.OwnerDocument.CreateElement(FOLD_TEXT_ATTR);
            TXMLProcessor.AddCDATA(tag1, txt);
            ATag.AppendChild(tag1);
         end;

         it := GetPinComments;
         while it.HasNext do
            TComment(it.Next).ExportToXMLTag2(ATag);

         for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
         begin
            lBranch := FBranchArray[i];

            tag2 := ATag.OwnerDocument.CreateElement(BRANCH_TAG);
            ATag.AppendChild(tag2);

            tag2.SetAttribute(ID_ATTR, lBranch.Id.ToString);

            if lBranch.Statement <> nil then
               tag2.SetAttribute(BRANCH_STMNT_ATTR, lBranch.Statement.Id.ToString);

            tag1 := ATag.OwnerDocument.CreateElement('x');
            TXMLProcessor.AddText(tag1, lBranch.hook.X.ToString);
            tag2.AppendChild(tag1);

            tag1 := ATag.OwnerDocument.CreateElement('y');
            TXMLProcessor.AddText(tag1, lBranch.hook.Y.ToString);
            tag2.AppendChild(tag1);

            it := GetBlocks(lBranch.Index);
            while it.HasNext do
               TXMLProcessor.ExportBlockToXML(TBlock(it.Next), tag2);
         end;
      finally
         if unPin then
            UnPinComments;
      end;
   end;
end;

procedure TBlock.SaveInXML(const ATag: IXMLElement);
var
   it: IIterator;
begin
   SaveInXML2(ATag);
   if (ATag <> nil) and (PinComments > 0) then
   begin
      it := GetPinComments;
      while it.HasNext do
         TComment(it.Next).ExportToXMLTag2(ATag);
      UnPinComments;
   end;
end;

procedure TBlock.SaveInXML2(const ATag: IXMLElement);
var
   txtControl: TCustomEdit;
   txt: string;
   tag: IXMLElement;
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute(BLOCK_TYPE_ATTR, Ord(BType).ToString);
      ATag.SetAttribute(FRAME_ATTR, FFrame.ToString);
      ATag.SetAttribute('x', Left.ToString);
      ATag.SetAttribute('y', Top.ToString);
      ATag.SetAttribute('h', Height.ToString);
      ATag.SetAttribute('w', Width.ToString);
      ATag.SetAttribute('bh', BottomHook.ToString);
      ATag.SetAttribute('brx', BottomPoint.X.ToString);
      ATag.SetAttribute(ID_ATTR, FId.ToString);
      ATag.SetAttribute('memW', memoWidth.ToString);
      ATag.SetAttribute('memH', memoHeight.ToString);
      ATag.SetAttribute('mem_vscroll', FMemoVScroll.ToString);
      ATag.SetAttribute('mem_hscroll', FMemoHScroll.ToString);
      ATag.SetAttribute('mem_wordwrap', MemoWordWrap.ToString);
      ATag.SetAttribute(FONT_SIZE_ATTR, Font.Size.ToString);
      ATag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      txtControl := GetTextControl;
      if (txtControl <> nil) and (txtControl.Text <> '') then
      begin
         txt := ReplaceStr(txtControl.Text, sLineBreak, LB_PHOLDER);
         tag := ATag.OwnerDocument.CreateElement(TEXT_TAG);
         TXMLProcessor.AddCDATA(tag, txt);
         ATag.AppendChild(tag);
      end;
   end;
end;

procedure TBlock.ImportCommentsFromXML(const ATag: IXMLElement);
var
   tag: IXMLElement;
   comment: TComment;
begin
   if ProcessComments then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, COMMENT_ATTR);
      while tag <> nil do
      begin
         comment := TComment.CreateDefault(Page);
         comment.ImportFromXMLTag(tag, Self);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      UnPinComments;
   end;
end;

function TBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   tag: IXMLElement;
   textControl: TCustomEdit;
   i: integer;
begin
   result := errNone;
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, TEXT_TAG);
      textControl := GetTextControl;
      if (tag <> nil) and (textControl <> nil) then
      begin
         FRefreshMode := true;
         textControl.Text := ReplaceStr(tag.Text, LB_PHOLDER, sLineBreak);
         FRefreshMode := false;
      end;

      i := StrToIntDef(ATag.GetAttribute(FONT_SIZE_ATTR), 8);
      if i in [8, 10, 12] then
         SetFontSize(i);

      i := StrToIntDef(ATag.GetAttribute(FONT_STYLE_ATTR), 0);
      SetFontStyle(TInfra.DecodeFontStyle(i));
      
      Frame := TXMLProcessor.GetBoolFromAttr(ATag, FRAME_ATTR);
      memoWidth := StrToIntDef(ATag.GetAttribute('memW'), 280);
      memoHeight := StrToIntDef(ATag.GetAttribute('memH'), 182);
      MemoVScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_vscroll');
      MemoHScroll := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_hscroll');
      MemoWordWrap := TXMLProcessor.GetBoolFromAttr(ATag, 'mem_wordwrap');

      ImportCommentsFromXML(ATag);
   end;
end;

function TGroupBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   tag1, tag2: IXMLElement;
   bId, idx, bStmntId, hx, hy: integer;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      tag1 := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if tag1 <> nil then
      begin
         idx := PRIMARY_BRANCH_IND;
         repeat
            tag2 := TXMLProcessor.FindChildTag(tag1, 'x');
            if tag2 <> nil then
               hx := StrToIntDef(tag2.Text, 0);
            tag2 := TXMLProcessor.FindChildTag(tag1, 'y');
            if tag2 <> nil then
               hy := StrToIntDef(tag2.Text, 0);
            bId := StrToIntDef(tag1.GetAttribute(ID_ATTR), ID_INVALID);
            bStmntId := StrToIntDef(tag1.GetAttribute(BRANCH_STMNT_ATTR), ID_INVALID);
            if GetBranch(idx) = nil then
               AddBranch(Point(hx, hy), false, bId, bStmntId);
            tag2 := TXMLProcessor.FindChildTag(tag1, BLOCK_TAG);
            if tag2 <> nil then
            begin
               TXMLProcessor.ImportFlowchartFromXMLTag(tag2, Self, nil, result, idx);
               if result <> errNone then break;
            end;
            idx := idx + 1;
            tag1 := TXMLProcessor.FindNextTag(tag1);
         until tag1 = nil;
      end;
      tag2 := TXMLProcessor.FindChildTag(ATag, FOLD_TEXT_ATTR);
      if tag2 <> nil then
         SetFoldedText(tag2.Text);
      FFoldParms.Width := StrToIntDef(ATag.GetAttribute('fw'), 140);
      FFoldParms.Height := StrToIntDef(ATag.GetAttribute('fh'), 91);
      if TXMLProcessor.GetBoolFromAttr(ATag, FOLDED_ATTR) then
         ExpandFold(false);
   end;
end;

procedure TBlock.ExportToXMLTag(ATag: IXMLElement);
var
   block: TBlock;
begin
   TXMLProcessor.ExportBlockToXML(Self, ATag);
   if ParentBranch <> nil then
   begin
      block := ParentBranch.First;
      while block <> nil do
      begin
         if block = Self then
            break;
         block := block.Next;
      end;
      if block <> nil then
      begin
         block := block.Next;
         while block <> nil do
         begin
            if not block.Frame then
               break;
            TXMLProcessor.ExportBlockToXML(block, ATag);
            block := block.Next;
         end;
      end;
   end;
end;

function TBlock.ImportFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
var
   block, newBlock: TBlock;
   lParent: TGroupBlock;
   tag: IXMLElement;
begin
   result := errValidate;
   tag := TXMLProcessor.FindChildTag(ATag, BLOCK_TAG);
   if (tag = nil) or (tag.GetAttribute(BLOCK_TYPE_ATTR) = Ord(blMain).ToString) then
      Gerr_text := i18Manager.GetString('BadImportTag')
   else
   begin
      lParent := nil;
      block := Self;
      if (Ired = 0) and (FParentBlock <> nil) then
         lParent := FParentBlock
      else if Self is TGroupBlock then
      begin
         lParent := TGroupBlock(Self);
         block := nil;
      end;
      if lParent <> nil then
      begin
         lParent.BlockImportMode := true;
         try
            newBlock := TXMLProcessor.ImportFlowchartFromXMLTag(tag, lParent, block, result, Ired);
         finally
            lParent.BlockImportMode := false;
         end;
         if result = errNone then
         begin
            lParent.ResizeWithDrawLock;
            newBlock.ImportCommentsFromXML(tag);
         end;
      end;
   end;
end;

procedure TBlock.PopulateComboBoxes;
begin
end;

function TBlock.GetExportFileName: string;
begin
   result := '';
end;

function TBlock.GetFocusColor: TColor;
var
   edit: TCustomEdit;
begin
   edit := GetTextControl;
   if (edit <> nil) and edit.HasParent then
      result := THackControl(edit).Font.Color
   else
      result := OK_COLOR;
end;

procedure TBlock.UpdateEditor(AEdit: TCustomEdit);
var
   chLine: TChangeLine;
begin
   if (AEdit <> nil) and PerformEditorUpdate then
   begin
      if not GInfra.CurrentLang.GetTemplate(ClassType).IsEmpty then
      begin
         chLine := TInfra.GetChangeLine(Self, AEdit);
         if chLine.Row <> ROW_NOT_FOUND then
         begin
            chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, AEdit.Text);
            chLine.Text := TInfra.StripInstrEnd(chLine.Text);
            if GSettings.UpdateEditor and not SkipUpdateEditor then
               TInfra.ChangeLine(chLine);
            TInfra.GetEditorForm.SetCaretPos(chLine);
         end;
      end
      else
         TInfra.UpdateCodeEditor(Self);
   end;
end;

function TBlock.PerformEditorUpdate: boolean;
begin
   result := TInfra.GetEditorForm.Visible and (not FRefreshMode) and not (fsStrikeOut in Font.Style);
end;

function TBlock.GetDescTemplate(const ALangId: string): string;
begin
   result := '';
end;

function TBlock.FillTemplate(const ALangId: string; const ATemplate: string = ''): string;
var
   textControl: TCustomEdit;
   s, template: string;
   lang: TLangDefinition;
begin
   result := '';
   template := '';
   if ATemplate.IsEmpty then
   begin
      lang := GInfra.GetLangDefinition(ALangId);
      if lang <> nil then
      begin
         if not lang.GetTemplate(ClassType).IsEmpty then
            template := lang.GetTemplateExpr(ClassType);
      end;
   end
   else
      template := ATemplate;
   if not template.IsEmpty then
   begin
      textControl := GetTextControl;
      if textControl <> nil then
         s := Trim(textControl.Text)
      else
         s := '';
      result := ReplaceStr(template, PRIMARY_PLACEHOLDER, s);
   end
   else
      result := FillCodedTemplate(ALangId);
end;

function TBlock.FillCodedTemplate(const ALangId: string): string;
var
   textControl: TCustomEdit;
begin
   result := '';
   textControl := GetTextControl;
   if textControl <> nil then
      result := Trim(textControl.Text);
end;

procedure TBlock.ExportToGraphic(const AGraphic: TGraphic);
var
   bitmap: TBitmap;
   iterc: IIterator;
   comment: TComment;
   pnt: TPoint;
   lPage: TBlockTabSheet;
begin
   ClearSelection;
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   bitmap.Width := Width + 2;
   bitmap.Height := Height + 2;
   lPage := Page;
   lPage.DrawI := false;
   bitmap.Canvas.Lock;
   try
      PaintTo(bitmap.Canvas.Handle, 1, 1);
      iterc := GetComments;
      while iterc.HasNext do
      begin
         comment := TComment(iterc.Next);
         pnt := ParentToClient(comment.BoundsRect.TopLeft, Page);
         comment.PaintTo(bitmap.Canvas.Handle, pnt.X, pnt.Y);
      end;
   finally
      bitmap.Canvas.Unlock;
      lPage.DrawI := true;
   end;
   if AGraphic <> bitmap then
   begin
      AGraphic.Assign(bitmap);
      bitmap.Free;
   end;
end;

procedure TGroupBlock.PopulateComboBoxes;
var
   i: integer;
   block: TBlock;
begin
   inherited PopulateComboBoxes;
   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      block := FBranchArray[i].First;
      while block <> nil do
      begin
         block.PopulateComboBoxes;
         block := block.Next;
      end;
   end;
end;

function TGroupBlock.GetBranchCount: integer;
begin
   result := Length(FBranchArray) - 1;
end;

function TGroupBlock.GetBlocks(const AIndex: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   first, last, i, a: integer;
   block: TBlock;
   objList: TObjectList;
begin
   objList := TObjectList.Create(false);
   if GetBranch(AIndex) <> nil then
   begin
      first := AIndex;
      last := AIndex;
   end
   else
   begin
      if AIndex < PRIMARY_BRANCH_IND then
      begin
         first := PRIMARY_BRANCH_IND;
         last := High(FBranchArray);
      end
      else
      begin
         first := 0;
         last := -1;
      end;
   end;
   a := 0;
   for i := first to last do
      a := a + FBranchArray[i].Count;
   if objList.Capacity < a then
      objList.Capacity := a;
   for i := first to last do
   begin
      block := FBranchArray[i].First;
      while block <> nil do
      begin
         objList.Add(block);
         block := block.Next;
      end;
   end;
   result := TBlockIterator.Create(objList);
end;

function TGroupBlock.GetBranches(const AStart: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   i, first, last: integer;
   objList: TObjectList;
begin
   objList := TObjectList.Create(false);
   if GetBranch(AStart) <> nil then
      first := AStart
   else
   begin
      if AStart < PRIMARY_BRANCH_IND then
         first := PRIMARY_BRANCH_IND
      else
         first := 0;           // case if lower index is greater than upper bound of branch_collection
   end;
   if first = 0 then
      last := -1
   else
      last := High(FBranchArray);
   for i := first to last do
      objList.Add(FBranchArray[i]);
   result := TBranchIterator.Create(objList);
end;

function TBlock.SkipUpdateEditor: boolean;
var
   funcHeader: TUserFunctionHeader;
begin
   funcHeader := TInfra.GetFunctionHeader(Self);
   result := (funcHeader <> nil) and (TInfra.IsNOkColor(funcHeader.Font.Color) or funcHeader.chkExtDeclare.Checked);
end;

function TBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   tmpList: TStringList;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   tmpList := TStringList.Create;
   try
      GenerateDefaultTemplate(tmpList, ALangId, ADeep);
      TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
      result := tmpList.Count;
   finally
      tmpList.Free;
   end;
end;

procedure TBlock.GenerateDefaultTemplate(const ALines: TStringList; const ALangId: string; const ADeep: integer);
var
   langDef: TLangDefinition;
   template, txt: string;
   textControl: TCustomEdit;
begin
   langDef := GInfra.GetLangDefinition(ALangId);
   if langDef <> nil then
   begin
      txt := '';
      textControl := GetTextControl;
      if textControl is TMemo then
         txt := textControl.Text
      else if textControl <> nil then
         txt := Trim(textControl.Text);
      template := langDef.GetTemplate(Self.ClassType);
      if template.IsEmpty then
         template := PRIMARY_PLACEHOLDER;
      template := ReplaceStr(template, PRIMARY_PLACEHOLDER, txt);
      GenerateTemplateSection(ALines, template, ALangId, ADeep);
   end;
end;

function TGroupBlock.ExtractBranchIndex(const AStr: string): integer;
var
   i, b: integer;
   val: string;
begin
   result := Pos('%b', AStr);
   if result <> 0 then
   begin
      val := '';
      for i := result+2 to Length(AStr) do
      begin
         if TryStrToInt(AStr[i], b) then
            val := val + AStr[i]
         else
            break;
      end;
      result := StrToIntDef(val, 0);
      if result > High(FBranchArray) then
         result := 0;
   end;
end;

procedure TBlock.GenerateTemplateSection(const ALines: TStringList; const ATemplate: string; const ALangId: string; const ADeep: integer);
var
   lines: TStringList;
begin
   lines := TStringList.Create;
   try
      lines.Text := ATemplate;
      GenerateTemplateSection(ALines, lines, ALangId, ADeep);
   finally
      lines.Free;
   end;
end;

procedure TBlock.GenerateTemplateSection(const ALines: TStringList; const ATemplate: TStringList; const ALangId: string; const ADeep: integer);
var
   line: string;
   i: integer;
   obj: TObject;
begin
   i := ALines.Count + ATemplate.Count;
   if ALines.Capacity < i then
      ALines.Capacity := i;
   for i := 0 to ATemplate.Count-1 do
   begin
      line := DupeString(GSettings.IndentString, ADeep) + ATemplate[i];
      line := ReplaceStr(line, INDENT_XML_CHAR, GSettings.IndentString);
      line := TInfra.StripInstrEnd(line);
      obj := ATemplate.Objects[i];
      if obj = nil then
         obj := Self;
      ALines.AddObject(line, obj);
   end;
end;

procedure TGroupBlock.GenerateTemplateSection(const ALines: TStringList; const ATemplate: TStringList; const ALangId: string; const ADeep: integer);

   function CountLeadIndentChars(const AString: string): integer;
   var
      i: integer;
   begin
      result := 0;
      for i := 1 to Length(AString) do
      begin
         if AString[i] = INDENT_XML_CHAR then
            result := result + 1
         else
            break;
      end;
   end;

var
   i, b: integer;
   line: string;
   obj: TObject;
begin
   for i := 0 to ATemplate.Count-1 do
   begin
      b := ExtractBranchIndex(ATemplate[i]);
      if b > 0 then
      begin
         if (ALines.Count > 0) and (ALines.Objects[ALines.Count-1] = nil) then
            ALines.Objects[ALines.Count-1] := FBranchArray[b];
         GenerateNestedCode(ALines, b, ADeep+CountLeadIndentChars(ATemplate[i]), ALangId);
      end
      else
      begin
         line := DupeString(GSettings.IndentString, ADeep) + ATemplate[i];
         line := ReplaceStr(line, INDENT_XML_CHAR, GSettings.IndentString);
         obj := ATemplate.Objects[i];
         if obj = nil then
            obj := Self;
         ALines.AddObject(line, obj);
      end;
   end;
end;

function TBlock.Next: TBlock;
var
   idx: integer;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      idx := FParentBranch.IndexOf(Self);
      if (idx <> -1) and (FParentBranch.Last <> Self) then
         result := FParentBranch.Items[idx+1];
   end;
end;

function TBlock.Prev: TBlock;
var
   idx: integer;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      idx := FParentBranch.IndexOf(Self);
      if (idx <> -1) and (FParentBranch.First <> Self) then
         result := FParentBranch.Items[idx-1];
   end;
end;

constructor TBranch.Create(const AParent: TGroupBlock; const AHook: TPoint; const AId: integer = ID_INVALID);
begin
   inherited Create;
   FParentBlock := AParent;
   Hook := AHook;
   FRmvBlockIdx := -1;
   Statement := nil;
   OwnsObjects := false;
   FId := GProject.Register(Self, AId);
end;

destructor TBranch.Destroy;
begin
   Statement.Free;
   OwnsObjects := true;
   GProject.UnRegister(Self);
   inherited Destroy;
end;

function TBranch.GetMostRight: integer;
var
   i: integer;
begin
   result := Hook.X;
   for i := 0 to Count-1 do
   begin
      if Items[i].BoundsRect.Right > result then
         result := Items[i].BoundsRect.Right;
   end;
end;

procedure TBranch.InsertAfter(ANewBlock, APosBlock: TBlock);
begin
   Insert(IndexOf(APosBlock)+1, ANewBlock);
end;

function TBranch.GetHeight: integer;
var
   i: integer;
begin
   result := 0;
   for i := 0 to Count-1 do
      Inc(result, Items[i].Height);
end;

function TBranch.GetIndex: integer;
var
   iter: IIterator;
begin
   result := -1;
   if FParentBlock <> nil then
   begin
      iter := FParentBlock.GetBranches;
      result := iter.GetObjectIndex(Self);
      if result <> -1 then
         Inc(result);
   end;
end;

function TBranch.GetItems(AIndex: integer): TBlock;
begin
  result := TBlock(inherited Items[AIndex]);
end;

procedure TBranch.SetItems(AIndex: integer; ABlock: TBlock);
begin
  inherited Items[AIndex] := ABlock;
end;

function TBranch.GetFirst: TBlock;
begin
   result := nil;
   if Count > 0 then
      result := TBlock(inherited First);
end;

function TBranch.GetLast: TBlock;
begin
   result := nil;
   if Count > 0 then
      result := TBlock(inherited Last);
end;

function TBranch.Add(ABlock: TBlock): integer;
begin
  result := inherited Add(ABlock);
end;

function TBranch.IndexOf(ABlock: TBlock): integer;
begin
  result := inherited IndexOf(ABlock);
end;

procedure TBranch.Insert(AIndex: integer; ABlock: TBlock);
begin
   if AIndex >= 0 then
      inherited Insert(AIndex, ABlock);
end;

procedure TBranch.UndoRemove(ABlock: TBlock);
begin
   if (ABlock <> nil) and (Self = ABlock.ParentBranch) and (FRmvBlockIdx >= 0) then
   begin
      Insert(FRmvBlockIdx, ABlock);
      FRmvBlockIdx := -1;
   end;
end;

function TBranch.Remove(ABlock: TObject): integer;
begin
   result := inherited Remove(ABlock);
   FRmvBlockIdx := result;
end;

function TBranch.GetId: integer;
begin
   result := FId;
end;

function TBranch.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
   if GetInterface(IID, Obj) then
      result := 0
   else
      result := E_NOINTERFACE;
end;

function TBranch._AddRef: Integer; stdcall;    // no reference counting
begin
   result := -1;
end;

function TBranch._Release: Integer; stdcall;
begin
   result := -1;
end;

end.
