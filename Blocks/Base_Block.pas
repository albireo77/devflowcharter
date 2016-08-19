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
   Windows, Forms, StdCtrls, ExtCtrls, Controls, Graphics, Messages, SysUtils,
   Classes, ComCtrls, Statement, OmniXML, BaseIterator, CommonInterfaces,
   CommonTypes, Contnrs, BlockTabSheet;

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

   TBlock = class(TCustomControl, IIdentifiable, IFocusable)
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
         FRefreshMode: boolean;
         FFrame: boolean;
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
         procedure ResetMemoScrollBars(const AScrollStyle: TScrollStyle; const AMemo: TMemo);
         procedure SetMemoWordWrap(AValue: boolean);
         function GetMemoWordWrap: boolean;
         procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
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
      public
         BottomPoint: TPoint;    // points to arrow at the bottom of the block
         IPoint: TPoint;          // points to I mark
         BottomHook: integer;
         TopHook: TPoint;
         Ired: Integer;           // indicates active arrow; -1: none, 0: bottom, 1: branch1, 2: branch2 and so on...
         HResizeInd: boolean;
         VResizeInd: boolean;
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
         function GetDescription: string; virtual;
         function GetTextControl: TCustomEdit; virtual;
         function GenerateTree(const AParentNode: TTreeNode): TTreeNode; virtual;
         function IsCursorSelect: boolean;
         function IsCursorResize: boolean;
         function CanInsertReturnBlock: boolean; virtual;
         procedure ExportToXMLTag(const ATag: IXMLElement);
         function ImportFromXMLTag(const ATag: IXMLElement): TErrorType;
         procedure ExportToGraphic(const AImage: TGraphic); virtual;
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
   Main_Block, ApplicationCommon, BlockFactory, StrUtils, UserFunction, Menus,
   XMLProcessor, Navigator_Form, LangDefinition, FastcodeAnsiStringReplaceUnit,
   FlashThread, Comment;

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

   Ired       := -1;
   memoWidth  := 280;
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
      Color := GSettings.FoldColor;
      Font.Assign(FStatement.Font);
      OnMouseDown := Self.OnMouseDown;
      Font.Color := clNavy;
      OnChange := Self.OnChangeMemo;
   end;

   Expanded := true;

   FFoldParms.Width := 140;
   FFoldParms.Height := 91;

   FTrueLabel := i18Manager.GetString('CaptionTrue');
   FFalseLabel := i18Manager.GetString('CaptionFalse');

   Branch := AddBranch(AHook, false);
end;

procedure TBlock.CloneFrom(ABlock: TBlock);
var
   lEdit, lEditSource: TCustomEdit;
begin
   if ABlock <> nil then
   begin
      Visible := ABlock.Visible;
      SetFont(ABlock.Font);
      lEditSource := ABlock.GetTextControl;
      lEdit := GetTextControl;
      if lEdit <> nil then
      begin
         if lEditSource <> nil then
         begin
            lEdit.Text := lEditSource.Text;
            lEdit.BoundsRect := lEditSource.BoundsRect;
            lEdit.Visible := lEditSource.Visible;
            lEdit.SelStart := lEditSource.SelStart;
         end;
         if lEdit.CanFocus then
            lEdit.SetFocus;
      end;
   end;
end;

procedure TGroupBlock.CloneFrom(ABlock: TBlock);
var
   lGroupBlock: TGroupBlock;
   lNewBlock, lPrevBlock, lBlock: TBlock;
   lBranch, lBranch2: TBranch;
   i: integer;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TGroupBlock then
   begin
      lGroupBlock := TGroupBlock(ABlock);
      FMemoFolder.Text := lGroupBlock.FMemoFolder.Text;
      if not lGroupBlock.Expanded then
      begin
         Expanded := false;
         FFoldParms := lGroupBlock.FFoldParms;
         Constraints.MinWidth := 140;
         Constraints.MinHeight := 54;
         Width := lGroupBlock.Width;
         Height := lGroupBlock.Height;
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
         FFoldParms.Width := lGroupBlock.FFoldParms.Width;
         FFoldParms.Height := lGroupBlock.FFoldParms.Height;
      end;
      for i := PRIMARY_BRANCH_IND to High(lGroupBlock.FBranchArray) do
      begin
         lBranch := lGroupBlock.FBranchArray[i];
         lBranch2 := GetBranch(i);
         if lBranch2 = nil then
            lBranch2 := AddBranch(lBranch.Hook, false);
         lBlock := lBranch.First;
         lPrevBlock := nil;
         while lBlock <> nil do
         begin
            lNewBlock := lBlock.Clone(lBranch2);
            lBranch2.InsertAfter(lNewBlock, lPrevBlock);
            lPrevBlock := lBranch2.Last;
            lBlock := lBlock.Next;
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
   lNewComment: TComment;
   lUnPin: boolean;
   lPage: TBlockTabSheet;
begin
   if ASource <> nil then
   begin
      lPage := Page;
      lUnPin := ASource.PinComments > 0;
      try
         iter := ASource.GetPinComments;
         while iter.HasNext do
         begin
            lNewComment := TComment(iter.Next).Clone(lPage);
            lNewComment.PinControl := Self;
         end;
         UnPinComments;
      finally
         if lUnPin then
            ASource.UnPinComments;
      end;
   end;
end;

procedure TBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   SelectBlock(Point(X, Y));
   SetCursor(Point(X, Y));
   if PtInRect(Rect(BottomPoint.X-5, BottomPoint.Y, BottomPoint.X+5, Height), Point(X, Y)) then
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
   lPoint: TPoint;
begin
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         lPoint := FBranchArray[i].Hook;
         if PtInRect(Rect(lPoint.X-5, TopHook.Y, lPoint.X+5, lPoint.Y), Point(X, Y)) then
         begin
            DrawArrowLine(Point(lPoint.X, TopHook.Y), lPoint, arrEnd, clRed);
            Ired := i;
            Cursor := TCursor(GCustomCursor);
            break;
         end
         else if Ired = i then
         begin
            DrawArrowLine(Point(lPoint.X, TopHook.Y), lPoint);
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
   if FFrame and PtInRect(Rect(Width-5, 0, Width, Height-5), APoint) then
      Cursor := crSizeWE
   else if FFrame and PtInRect(Rect(0, Height-5, Width-5, Height), APoint) then
      Cursor := crSizeNS
   else if FFrame and PtInRect(Rect(Width-5, Height-5, Width, Height), APoint) then
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
   if (Ired < 0) or (not (Source is TBlock)) or (Source is TMainBlock) or ((not isShift) and ((Source = Self) or IsForeParent(Source))) then
      Accept := false;
end;

procedure TBlock.MyOnDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   lPage, lSourcePage: TBlockTabSheet;
   lMItem: TMenuItem;
begin
   if Source is TBlock then
   begin
      lSourcePage := TBlock(Source).Page;
      lSourcePage.Form.pmPages.PopupComponent := TBlock(Source);
      if GetAsyncKeyState(VK_SHIFT) <> 0 then
         lMItem := lSourcePage.Form.miCopy
      else
         lMItem := lSourcePage.Form.miCut;
      lMItem.OnClick(lMitem);
      lPage := Page;
      lPage.Form.pmPages.PopupComponent := Self;
      lPage.Form.miPaste.OnClick(lPage.Form.miPaste);
   end;
end;

procedure TBlock.OnMouseLeave;
begin
   if Cursor <> crDefault then
      Cursor := crDefault;
   ClearSelection;
   if Ired = 0 then
      DrawArrowLine(BottomPoint, Point(BottomPoint.X, Height-1));
   Ired := -1;
   if VResizeInd or HResizeInd then
      SendMessage(Handle, WM_NCHITTEST, 0, 0);
end;

procedure TGroupBlock.OnMouseLeave;
var
   lPoint: TPoint;
   lBranch: TBranch;
begin
   lBranch := GetBranch(Ired);
   if lBranch <> nil then
   begin
      lPoint := lBranch.Hook;
      DrawArrowLine(Point(lPoint.X, TopHook.Y), lPoint);
   end;
   inherited OnMouseLeave;
end;

procedure TBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if HResizeInd and Resize then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

procedure TGroupBlock.MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   Resize := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if HResizeInd and Resize then
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
   if VResizeInd and Resize then
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
   lBlock: TBlock;
   lBranch: TBranch;
begin
   result := 0;
   lBranch := GetBranch(ABranchInd);
   if lBranch <> nil then
   begin
      lBlock := lBranch.First;
      while lBlock <> nil do
      begin
         result := result + lBlock.GenerateCode(ALines, ALangId, ADeep);
         lBlock := lBlock.Next;
      end;
   end;
end;

function TBlock.GetTextControl: TCustomEdit;
begin
   result := FStatement;
end;

procedure TBlock.MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   lMitem: TMenuItem;
   lPage: TBlockTabSheet;
begin
   if Button = mbLeft then
   begin
      if PtInRect(Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10), Point(X, Y)) then
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
            lMitem := nil;
            case GCustomCursor of
               crAssign:      lMitem := lPage.Form.miAssign;
               crMultiAssign: lMitem := lPage.Form.miMultipleAssign;
               crIfElse:      lMitem := lPage.Form.miIfElse;
               crWhile:       lMitem := lPage.Form.miWhile;
               crFor:         lMitem := lPage.Form.miFor;
               crRepeat:      lMitem := lPage.Form.miRepeat;
               crInput:       lMitem := lPage.Form.miInput;
               crOutput:      lMitem := lPage.Form.miOutput;
               crFuncCall:    lMitem := lPage.Form.miRoutineCall;
               crIf:          lMitem := lPage.Form.miIf;
               crCase:        lMitem := lPage.Form.miCase;
               crFolder:      lMitem := lPage.Form.miFolder;
               crText:        lMitem := lPage.Form.miText;
               crReturn:
               begin
                  if CanInsertReturnBlock then
                     lMitem := lPage.Form.miReturn;
               end;
            end;
            if lMitem <> nil then
            begin
               PopupMenu.PopupComponent := Self;
               lMitem.OnClick(lMitem);
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
   lLocked: boolean;
begin
   inherited;
   if GetAsyncKeyState(VK_LBUTTON) <> 0 then
   begin
      case Cursor of
         crSizeWE:
         begin
            Msg.Result := HTRIGHT;
            HResizeInd := true;
            BringToFront;
         end;
         crSizeNS:
         begin
            Msg.Result := HTBOTTOM;
            VResizeInd := true;
            BringToFront;
         end;
         crSizeNWSE:
         begin
            Msg.Result := HTBOTTOMRIGHT;
            HResizeInd := true;
            VResizeInd := true;
            BringToFront;
         end;
      end;
   end
   else if HResizeInd or VResizeInd then
   begin
      lLocked := LockDrawing;
      try
         if HResizeInd then
         begin
            if FParentBlock <> nil then
               FParentBlock.ResizeHorz(true);
            HResizeInd := false;
         end;
         if VResizeInd then
         begin
            if Self is TGroupBlock then
               TGroupBlock(Self).LinkBlocks;
            if FParentBlock <> nil then
               FParentBlock.ResizeVert(true);
            VResizeInd := false;
         end;
      finally
         if lLocked then
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
   lPoint: TPoint;
begin
   if Button = mbRight then
   begin
      lPoint := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      PopupMenu.Popup(lPoint.X, lPoint.Y);
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
   lLocked: boolean;
begin
   lLocked := LockDrawing;
   try
      ResizeHorz(true);
      ResizeVert(true);
   finally
      if lLocked then
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
   lLeftX, lRightX: Integer;
   lBlock: TBlock;
begin

   Branch.Hook.X := FInitParms.BranchPoint.X;
   TopHook.X := Branch.Hook.X;
   LinkBlocks;

   lBlock := Branch.First;
   if lBlock = nil then   // case if primary branch is empty
   begin
      Width := FInitParms.Width;
      BottomHook := FInitParms.BottomHook;
      BottomPoint.X := FInitParms.BottomPoint.X;
   end
   else
   begin
      // resize in left direction
      lLeftX := 30;   // 30 - left margin
      repeat
         if lBlock.Left < lLeftX then
            lLeftX := lBlock.Left;
         lBlock := lBlock.Next;
      until lBlock = nil;

      Branch.Hook.X := Branch.Hook.X + 30 - lLeftX;
      TopHook.X := Branch.Hook.X;
      LinkBlocks;
      lBlock := Branch.Last;
      BottomHook := lBlock.Left + lBlock.BottomPoint.X;

      // resize in right direction
      lRightX := 0;
      lBlock := Branch.First;
      repeat
         if lBlock.BoundsRect.Right > lRightX then
            lRightX := lBlock.BoundsRect.Right;
         lBlock := lBlock.Next;
      until lBlock = nil;

      SetWidth(lRightX);  // set final width
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
   lBlock, lBlockPrev: TBlock;
   i, lStart, lStop: integer;
   lTopLeft: TPoint;
begin
   if GetBranch(idx) <> nil then
   begin
      lStart := idx;
      lStop := idx;
   end
   else
   begin
      lStart := PRIMARY_BRANCH_IND;
      lStop := High(FBranchArray);
   end;
   for i := lStart to lStop do
   begin
      lBlock := FBranchArray[i].First;
      if lBlock <> nil then
      begin
         lTopLeft := Point(FBranchArray[i].Hook.X-lBlock.TopHook.X, FBranchArray[i].Hook.Y+1);
         lBlock.SetBounds(lTopLeft.X, lTopLeft.Y, lBlock.Width, lBlock.Height);
         lBlock := lBlock.Next;
         while lBlock <> nil do
         begin
            lBlockPrev := lBlock.Prev;
            lTopLeft := Point(lBlockPrev.BottomPoint.X+lBlockPrev.Left-lBlock.TopHook.X, lBlockPrev.BoundsRect.Bottom);
            lBlock.SetBounds(lTopLeft.X, lTopLeft.Y, lBlock.Width, lBlock.Height);
            lBlock := lBlock.Next;
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

procedure TBlock.CMMouseLeave(var Msg: TMessage);
begin
   OnMouseLeave;
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
   lComment: TComment;
   iterc: IIterator;
   lInFront: boolean;
   lPage: TTabSheet;
   lList: TObjectList;
begin
   lList := TObjectList.Create(false);
   if Visible then
   begin
      lPage := Page;
      iterc := GProject.GetComments;
      while iterc.HasNext do
      begin
         lComment := TComment(iterc.Next);
         if lComment.Page = lPage then
         begin
            if AInFront then
               lInFront := IsInFront(lComment)
            else
               lInFront := true;
            if lInFront and (lComment.PinControl = nil) and PtInRect(ClientRect, TInfra.ParentToClient(Self, lComment.BoundsRect.TopLeft, lPage)) then
               lList.Add(lComment);
         end
      end;
   end;
   result := TCommentIterator.Create(lList);
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
   lWnd: THandle;
begin
   result := false;
   if AControl <> nil then
   begin
      lWnd := GetWindow(AControl.Handle, GW_HWNDLAST);
      while lWnd <> 0 do
      begin
         if lWnd = FTopParentBlock.Handle then
         begin
            result := true;
            break;
         end
         else if lWnd = AControl.Handle then
            break;
         lWnd := GetNextWindow(lWnd, GW_HWNDPREV);
      end;
   end;
end;

procedure TBlock.MoveComments(x, y: integer);
var
   iter: IIterator;
   lComment: TComment;
begin
   if (x <> 0) and (y <> 0) and (Left <> 0) and (Top <> 0) and ((x <> Left) or (y <> Top)) then
   begin
      iter := GetComments(true);
      while iter.HasNext do
      begin
         lComment := TComment(iter.Next);
         if lComment.Visible then
         begin
            lComment.SetBounds(lComment.Left+x-Left, lComment.Top+y-Top, lComment.Width, lComment.Height);
            lComment.BringToFront;
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
   OnWindowPosChanged(Msg.WindowPos^.x, Msg.WindowPos^.y);
   inherited;
end;

function TBlock.GetPinComments: IIterator;
var
   lComment: TComment;
   iterc: IIterator;
   lList: TObjectList;
begin
   lList := TObjectList.Create(false);
   iterc := GProject.GetComments;
   while iterc.HasNext do
   begin
      lComment := TComment(iterc.Next);
      if lComment.PinControl = Self then
         lList.Add(lComment);
   end;
   result := TCommentIterator.Create(lList);
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
   lTopLeft, lPoint: TPoint;
   lTextControl: TCustomEdit;
begin
   lTextControl := GetTextControl;
   lPoint := GetDiamondPoint;
   if (lTextControl <> nil) and not InvalidPoint(lPoint) then
   begin
      lTopLeft.X := lTextControl.Height + lPoint.X - 60 + 4;
      lTopLeft.Y := Trunc((60-lTextControl.Height)/2) + lPoint.Y + 2;
      lTextControl.SetBounds(lTopLeft.X, lTopLeft.Y, 120-2*lTextControl.Height-7, lTextControl.Height);
   end
end;

procedure TBlock.RefreshStatements;
var
   i: integer;
   lBool, lBool2: boolean;
begin
    lBool := NavigatorForm.InvalidateInd;
    NavigatorForm.InvalidateInd := false;
    lBool2 := FRefreshMode;
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
       FRefreshMode := lBool2;
    end;
    NavigatorForm.InvalidateInd := lBool;
end;

function TBlock.GetId: integer;
begin
   result := FId;
end;

procedure TBlock.ChangeColor(const AColor: TColor);
var
   iter: IIterator;
   lComment: TComment;
begin
   Color := AColor;
   iter := GetComments;
   while iter.HasNext do
   begin
      lComment := TComment(iter.Next);
      if lComment.Visible then
         lComment.Color := AColor;
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
   lBlock: TBlock;
begin
   inherited ChangeColor(AColor);
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         lBlock := FBranchArray[i].First;
         while lBlock <> nil do
         begin
            lBlock.ChangeColor(AColor);
            lBlock := lBlock.Next;
         end;
      end;
   end;
   if GSettings.FoldColor = GSettings.DesktopColor then
      FMemoFolder.Color := AColor
   else
      FMemoFolder.Color := GSettings.FoldColor;
end;

procedure TBlock.SelectBlock(const APoint: TPoint);
begin
   if PtInRect(Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10), APoint) then
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
   lBlock: TBlock;
begin
   if not Expanded then
      ExpandFold(true);
   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      lBlock := FBranchArray[i].First;
      while lBlock <> nil do
      begin
         if lBlock is TGroupBlock then
            TGroupBlock(lBlock).ExpandAll;
         lBlock := lBlock.Next;
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
   lBlock: TBlock;
   i: integer;
begin
   result := not Expanded;
   if Expanded then
   begin
      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         lBlock := FBranchArray[i].First;
         while lBlock <> nil do
         begin
            if lBlock is TGroupBlock then
            begin
               result := TGroupBlock(lblock).HasFoldedBlocks;
               if result then break;
            end;
            lBlock := lBlock.Next;
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
         PolyLine([Point(0, 0), Point(Width-1, 0), Point(Width-1, Height-1), Point(0, Height-1), Point(0, 0)]);
         Pen.Style := psSolid;
      end;
   end;
end;

procedure TBlock.DrawI;
var
   lFontSize: integer;
begin
   if TMainBlock(FTopParentBlock).ShowI then
   begin
      lFontSize := Canvas.Font.Size;
      Canvas.Font.Size := 8;
      DrawTextLabel(IPoint.X, IPoint.Y, '|');
      Canvas.Font.Size := lFontSize;
   end;
end;

procedure TBlock.DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
var
   lFontName: string;
   lFontSize: integer;
   lFontStyles: TFontStyles;
begin
   if GSettings.ShowBlockLabels and (AText <> '') then
   begin
      lFontName := Canvas.Font.Name;
      lFontStyles := Canvas.Font.Style;
      Canvas.Font.Name := GInfra.CurrentLang.LabelFontName;
      Canvas.Font.Style := [fsBold];
      lFontSize := Canvas.Font.Size;
      Canvas.Font.Size := GInfra.CurrentLang.LabelFontSize;
      DrawTextLabel(x, y, AText, rightJust, downJust);
      Canvas.Font.Name := lFontName;
      Canvas.Font.Size := lFontSize;
      Canvas.Font.Style := lFontStyles;
   end;
end;

procedure TBlock.DrawTextLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
var
   lFontStyles: TFontStyles;
begin
   if AText <> '' then
   begin
      lFontStyles := Canvas.Font.Style;
      Canvas.Font.Style := [];
      if fsBold in lFontStyles then
         Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.Brush.Style := bsClear;
      if rightJust then
         x := x - Canvas.TextWidth(AText);
      if downJust then
         y := y - Canvas.TextHeight('X');
      Canvas.TextOut(x, y, AText);
      Canvas.Font.Style := lFontStyles;
   end;
end;

procedure TBlock.DrawArrowLine(const ABeginPoint, AEndPoint: TPoint; const AArrowPos: TArrowPosition = arrEnd; const AColor: TColor = clBlack);
const
   MX: array[boolean, boolean] of integer = ((10, -10), (-5, -5));
   MY: array[boolean, boolean] of integer = ((5, 5), (10, -10));
   MD: array[boolean, boolean] of integer = ((0, -10), (10, 0));
var
   lIsVertical, lToBottomRight: boolean;
   x, y: integer;
   lArrPoint: TPoint;
begin
   lIsVertical := ABeginPoint.X = AEndPoint.X;
   lArrPoint := AEndPoint;
   if lIsVertical then
      lToBottomRight := AEndPoint.Y > ABeginPoint.Y
   else
      lToBottomRight := AEndPoint.X > ABeginPoint.X;
   if AArrowPos = arrMiddle then
   begin
      if lIsVertical then
         lArrPoint.Y := lArrPoint.Y + (ABeginPoint.Y-AEndPoint.Y) div 2
      else
         lArrPoint.X := lArrPoint.X + (ABeginPoint.X-AEndPoint.X) div 2;
   end;
   x := MX[lIsVertical, lToBottomRight];
   y := MY[lIsVertical, lToBottomRight];
   with Canvas do
   begin
      Brush.Style := bsSolid;
      Pen.Color := AColor;
      Brush.Color := AColor;
      MoveTo(ABeginPoint.X, ABeginPoint.Y);
      LineTo(AEndPoint.X, AEndPoint.Y);
      Polygon([Point(lArrPoint.X+x, lArrPoint.Y+y),
               Point(lArrPoint.X+x+MD[lIsVertical, false], lArrPoint.Y+y+MD[lIsVertical, true]),
               lArrPoint,
               Point(lArrPoint.X+x, lArrPoint.Y+y)]);
   end;
end;

function TBlock.GetEllipseTextRect(const APoint: TPoint; const AText: string): TRect;
const
   MIN_HALF_HEIGHT = 15;
   MIN_HALF_WIDTH = 30;
var
   a, b: integer;
   ar, br, cx, cy: real;
   R: TRect;
   lExt, lViewPort: TSize;
begin
   GetWindowExtEx(Canvas.Handle, lExt);
   GetViewportExtEx(Canvas.Handle, lViewPort);
   cx := lViewPort.cx / lExt.cx;
   cy := lViewPort.cy / lExt.cy;
   R := Rect(0, 0, 0, 0);
   DrawText(Canvas.Handle, PChar(AText), -1, R, DT_CALCRECT);
   ar := (R.Bottom - R.Top) * cy / Sqrt(2);
   br := (R.Right - R.Left) * cx / Sqrt(2);
   if ar < MIN_HALF_HEIGHT then
   begin
      if ar = 0 then
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
begin
   result := Rect(0, 0, 0, 0);
   if not InvalidPoint(APoint) then
   begin
      result := GetEllipseTextRect(APoint, AText);
      Canvas.Brush.Style := bsClear;
      if GSettings.EllipseColor <> GSettings.DesktopColor then
         Canvas.Brush.Color := GSettings.EllipseColor;
      Canvas.Ellipse(result.Left, result.Top, result.Right, result.Bottom);
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
   lTextControl: TCustomEdit;
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   lTextControl := GetTextControl;
   if lTextControl <> nil then
   begin
      if THackControl(lTextControl).Font.Color = NOK_COLOR then
         result.ErrorCount := 1
      else if THackControl(lTextControl).Font.Color = WARN_COLOR then
         result.WarningCount := 1;
   end;
end;

function TGroupBlock.CountErrWarn: TErrWarnCount;
var
   iter: IIterator;
   lErrWarnCount: TErrWarnCount;
begin
   result := inherited CountErrWarn;
   iter := GetBlocks;
   while iter.HasNext do
   begin
      lErrWarnCount := TBlock(iter.Next).CountErrWarn;
      Inc(result.ErrorCount, lErrWarnCount.ErrorCount);
      Inc(result.WarningCount, lErrWarnCount.WarningCount);
   end;
end;

procedure TGroupBlock.Paint;
var
   lTop: TPoint;
   lStyle: TBrushStyle;
   lColor: TColor;
   lWidth: integer;
begin
   inherited;
   lStyle := Canvas.Brush.Style;
   lColor := Canvas.Brush.Color;
   lWidth := Canvas.Pen.Width;
   if Expanded then
   begin
      lTop := GetDiamondPoint;
      if not InvalidPoint(lTop) then
      begin
         Canvas.Brush.Style := bsClear;
         if GSettings.DiamondColor <> GSettings.DesktopColor then
            Canvas.Brush.Color := GSettings.DiamondColor;
         Canvas.Polygon([Point(lTop.X-60, lTop.Y+30),
                         Point(lTop.X, lTop.Y+60),
                         Point(lTop.X+60, lTop.Y+30),
                         lTop,
                         Point(lTop.X-60, lTop.Y+30)]);
         end;
   end
   else if FMemoFolder <> nil then
   begin
      if FTopParentBlock <> Self then
         DrawArrowLine(Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
      Canvas.Pen.Width := 2;
      Canvas.Brush.Style := bsClear;
      if GSettings.FoldColor <> GSettings.DesktopColor then
         Canvas.Brush.Color := GSettings.FoldColor;
      Canvas.Polygon([Point(1, 1),
                      Point(Width-1, 1),
                      Point(Width-1, FMemoFolder.Height+5),
                      Point(1, FMemoFolder.Height+5),
                      Point(1, 0)]);
   end;
   Canvas.Brush.Style := lStyle;
   Canvas.Brush.Color := lColor;
   Canvas.Pen.Width := lWidth;
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
   lPoint: TPoint;
   lPage: TTabSheet;
begin
   inherited;
   lPage := Page;
   lPoint := TInfra.ClientToParent(Self, Point(0, 0), lPage);
   if HResizeInd then
      Msg.MinMaxInfo.ptMaxTrackSize.X := lPage.ClientWidth - lPoint.X;
   if VResizeInd then
      Msg.MinMaxInfo.ptMaxTrackSize.Y := lPage.ClientHeight - lPoint.Y;
end;

function TBlock.IsCursorSelect: boolean;
begin
   result := PtInRect(Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10), ScreenToClient(Mouse.CursorPos));
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
   lPos, lCount, lLineCount: integer;
   lMemo: TMemo;
   lOldFont: HFont;
   lHandle: THandle;
   lTextMetric: TTextMetric;
begin
   lMemo := GetFrontMemo;
   if lMemo <> nil then
   begin
      lPos := lMemo.SelStart;
      if FMemoVScroll then
      begin
         lHandle := GetDC(lMemo.Handle);
         try
            lOldFont := SelectObject(lHandle, lMemo.Font.Handle);
            try
               GetTextMetrics(lHandle, lTextMetric);
               lLineCount := (lMemo.ClientHeight - 4)  div (lTextMetric.tmHeight + lTextMetric.tmExternalLeading)
            finally
               SelectObject(lHandle, lOldFont);
            end;
         finally
            ReleaseDC(lMemo.Handle, lHandle);
         end;
         lCount := lMemo.Lines.Count;
         if AnsiEndsText(CRLF, lMemo.Text) then
            lCount := lCount + 1;
         if lCount > lLineCount then
         begin
            if lMemo.ScrollBars = ssNone then
               lMemo.ScrollBars := ssVertical
            else if lMemo.ScrollBars = ssHorizontal then
               lMemo.ScrollBars := ssBoth;
         end
         else
            ResetMemoScrollBars(ssHorizontal, lMemo);
      end
      else
         ResetMemoScrollBars(ssHorizontal, lMemo);
      lMemo.SelStart := lPos;
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
   lPos, lCount, lWidth, i: integer;
   lMemo: TMemo;
   lCanvas: TCanvas;
   lMargins: longint;
begin
   lMemo := GetFrontMemo;
   if lMemo <> nil then
   begin
      lPos := lMemo.SelStart;
      if FMemoHScroll and not lMemo.WordWrap then
      begin
         lWidth := 0;
         lCanvas := TCanvas.Create;
         try
            lCanvas.Font.Assign(lMemo.Font);
            lCanvas.Handle := GetDC(lMemo.Handle);
            for i := 0 to lMemo.Lines.Count-1 do
            begin
               lCount := lCanvas.TextWidth(lMemo.Lines[i]);
               if lCount > lWidth then
                  lWidth := lCount;
            end;
            lMargins := SendMessage(lMemo.Handle, EM_GETMARGINS, 0, 0);
            if lWidth > (lMemo.ClientWidth - HiWord(lMargins) - LoWord(lMargins) - 3) then
            begin
               if lMemo.ScrollBars = ssNone then
                  lMemo.ScrollBars := ssHorizontal
               else if lMemo.ScrollBars = ssVertical then
                  lMemo.ScrollBars := ssBoth;
            end
            else
               ResetMemoScrollBars(ssVertical, lMemo);
         finally
            ReleaseDC(lMemo.Handle, lCanvas.Handle);
            lCanvas.Free;
         end;
      end
      else
         ResetMemoScrollBars(ssVertical, lMemo);
      lMemo.SelStart := lPos;
   end;
end;

procedure TBlock.ResetMemoScrollBars(const AScrollStyle: TScrollStyle; const AMemo: TMemo);
var
   lScrollStyle: TScrollStyle;
begin
   if AScrollStyle = ssVertical then
      lScrollStyle := ssHorizontal
   else
      lScrollStyle := ssVertical;
   if AMemo.ScrollBars = ssBoth then
      AMemo.ScrollBars := AScrollStyle
   else if AMemo.ScrollBars = lScrollStyle then
      AMemo.ScrollBars := ssNone;
end;

procedure TBlock.SetMemoWordWrap(AValue: boolean);
var
   lMemo: TMemo;
begin
   lMemo := GetFrontMemo;
   if (lMemo <> nil) and (AValue <> lMemo.WordWrap) then
   begin
      lMemo.WordWrap := AValue;
      if lMemo.WordWrap then
         MemoHScroll := false;
   end;
end;

function TBlock.GetMemoWordWrap: boolean;
var
   lMemo: TMemo;
begin
   lMemo := GetFrontMemo;
   result := (lMemo <> nil) and lMemo.WordWrap;
end;

function TBlock.CanBeFocused: boolean;
var
   lParent: TGroupBlock;
   lFunction: TUserFunction;
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
      lFunction := TUserFunction(TMainBlock(FTopParentBlock).UserFunction);
      if lFunction <> nil then
      begin
         result := lFunction.Active;
         if result and (lFunction.Header <> nil) then
            result := lFunction.Header.chkBodyVisible.Checked;
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
   lText: string;
   lMemo: TMemo;
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
         lMemo := TMemo(AInfo.FocusEdit);
         if AInfo.RelativeLine < lMemo.Lines.Count then
         begin
            lText := lMemo.Lines[AInfo.RelativeLine];
            if AInfo.RelativeLine > 0 then
            begin
               for i := 0 to AInfo.RelativeLine-1 do
                  idx2 := idx2 + Length(lMemo.Lines[i] + CRLF);
            end;
         end
         else
            lText := lMemo.Text;
      end
      else
         lText := AInfo.FocusEdit.Text;
      idx := AnsiPos(lText, AInfo.LineText);
      if idx <> 0 then
         AInfo.SelStart := AInfo.SelStart - idx + idx2
      else
      begin
         idx := AnsiPos(AInfo.SelText, lText);
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
         i := TInfra.RPos(#10, result);
         if i <> 0 then
            result := ' - ' + AnsiRightStr(result, Length(result)-i);
      end;
   end;
end;

function TBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lErrMsg: string;
   lTextControl: TCustomEdit;
begin
   result := AParentNode;
   lTextControl := GetTextControl;
   if lTextControl <> nil then
   begin
      lErrMsg := GetErrorMsg(lTextControl);
      result := AParentNode.Owner.AddChildObject(AParentNode, GetDescription + lErrMsg, lTextControl);
      if lErrMsg <> '' then
      begin
         AParentNode.MakeVisible;
         AParentNode.Expand(false);
      end;
   end;
end;

function TGroupBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lBlock: TBlock;
begin
   result := inherited GenerateTree(AParentNode);
   lBlock := FBranchArray[PRIMARY_BRANCH_IND].First;
   while lBlock <> nil do
   begin
      lBlock.GenerateTree(result);
      lBlock := lBlock.Next;
   end;
end;

function TGroupBlock.AddBranch(const AHook: TPoint; const AResizeInd: boolean; const ABranchId: integer = ID_INVALID; const ABranchStmntId: integer = ID_INVALID): TBranch;
var
   l: integer;
begin
   result := TBranch.Create(Self, AHook, ABranchId);
   l := Length(FBranchArray);
   if l = 0 then l := 1;
   SetLength(FBranchArray, l+1);
   FBranchArray[l] := result;
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
   lComment: TComment;
   lPoint: TPoint;
begin
   lPoint := TInfra.ClientToParent(Self, ClientRect.TopLeft, Page);
   iter := GetComments;
   while iter.HasNext do
   begin
      lComment := TComment(iter.Next);
      lComment.PinControl := Self;
      lComment.Visible := false;
      lComment.SetBounds(lComment.Left - lPoint.X, lComment.Top - lPoint.Y, lComment.Width, lComment.Height);
   end;
   result := iter.Count;
end;

function TBlock.UnPinComments: integer;
var
   iter: IIterator;
   lComment: TComment;
   lPoint: TPoint;
begin
   lPoint := TInfra.ClientToParent(Self, ClientRect.TopLeft, Page);
   iter := GetPinComments;
   while iter.HasNext do
   begin
      lComment := TComment(iter.Next);
      lComment.PinControl := nil;
      lComment.SetBounds(lComment.Left + lPoint.X, lComment.Top + lPoint.Y, lComment.Width, lComment.Height);
      lComment.Visible := true;
      lComment.BringToFront;
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
   lTmpWidth, lTmpHeight, i: integer;
   lBlock: TBlock;
   lTextControl: TCustomEdit;
begin
   GChange := 1;
   Expanded := not Expanded;
   lTextControl := GetTextControl;
   if lTextControl <> nil then
      lTextControl.Visible := Expanded;
   FMemoFolder.Visible := not Expanded;

   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      lBlock := FBranchArray[i].First;
      while lBlock <> nil do
      begin
         lBlock.Visible := Expanded;
         lBlock := lBlock.Next;
      end;
   end;

   if Expanded then
   begin
      lTmpWidth := Width;
      lTmpHeight := Height;
      Width := FFoldParms.Width;
      Height := FFoldParms.Height;
      FFoldParms.Width := lTmpWidth;
      FFoldParms.Height := lTmpHeight;
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
      lTmpWidth := FFoldParms.Width;
      lTmpHeight := FFoldParms.Height;
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
      Width := lTmpWidth;
      Height := lTmpHeight;
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
   lText: string;
   tag1, tag2: IXMLElement;
   lBranch: TBranch;
   iter: IIterator;
   lUnPin: boolean;
   lTextControl: TCustomEdit;
begin
   lUnPin := false;
   if ATag <> nil then
   begin
      ATag.SetAttribute(BLOCK_TYPE_ATTR, IntToStr(Ord(BType)));
      ATag.SetAttribute(FRAME_ATTR, BoolToStr(FFrame, true));
      ATag.SetAttribute('x', IntToStr(Left));
      ATag.SetAttribute('y', IntToStr(Top));
      ATag.SetAttribute('h', IntToStr(Height));
      ATag.SetAttribute('w', IntToStr(Width));
      ATag.SetAttribute('bh', IntToStr(BottomHook));
      ATag.SetAttribute('brx', IntToStr(BottomPoint.X));
      ATag.SetAttribute(ID_ATTR, IntToStr(FId));
      ATag.SetAttribute('memW', IntToStr(memoWidth));
      ATag.SetAttribute('memH', IntToStr(memoHeight));
      ATag.SetAttribute('mem_vscroll', BoolToStr(FMemoVScroll, true));
      ATag.SetAttribute('mem_hscroll', BoolToStr(FMemoHScroll, true));
      ATag.SetAttribute('mem_wordwrap', BoolToStr(MemoWordWrap, true));
      ATag.SetAttribute(FONT_SIZE_ATTR, IntToStr(Font.Size));
      ATag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      lTextControl := GetTextControl;
      if (lTextControl <> nil) and (lTextControl.Text <> '') then
      begin
         lText := AnsiReplaceStr(lTextControl.Text, CRLF, CRLF_PLACEHOLDER);
         tag1 := ATag.OwnerDocument.CreateElement(TEXT_TAG);
         TXMLProcessor.AddCDATA(tag1, lText);
         ATag.AppendChild(tag1);
      end;

      if Expanded then
      begin
         fw := FFoldParms.Width;
         fh := FFoldParms.Height;
         brx := Branch.Hook.X;
         lUnPin := PinComments > 0;
      end
      else
      begin
         fw := Width;
         fh := Height;
         brx := FFoldParms.BranchPoint.X;
         ATag.SetAttribute('h', IntToStr(FFoldParms.Height));
         ATag.SetAttribute('w', IntToStr(FFoldParms.Width));
         ATag.SetAttribute('bh', IntToStr(FFoldParms.BottomHook));
      end;

      try

        ATag.SetAttribute('brx', IntToStr(brx));
        ATag.SetAttribute('bry', IntToStr(Branch.Hook.Y));
        ATag.SetAttribute('fw', IntToStr(fw));
        ATag.SetAttribute('fh', IntToStr(fh));
        ATag.SetAttribute(FOLDED_ATTR, BoolToStr(not Expanded, true));

        lText := GetFoldedText;
        if lText <> '' then
        begin
           tag1 := ATag.OwnerDocument.CreateElement(FOLD_TEXT_ATTR);
           TXMLProcessor.AddCDATA(tag1, lText);
           ATag.AppendChild(tag1);
        end;

        iter := GetPinComments;
        while iter.HasNext do
           TComment(iter.Next).ExportToXMLTag2(ATag);

        for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
        begin
           lBranch := FBranchArray[i];

           tag2 := ATag.OwnerDocument.CreateElement(BRANCH_TAG);
           ATag.AppendChild(tag2);

           tag2.SetAttribute(ID_ATTR, IntToStr(lBranch.Id));

           if lBranch.Statement <> nil then
              tag2.SetAttribute(BRANCH_STMNT_ATTR, IntToStr(lBranch.Statement.Id));

           tag1 := ATag.OwnerDocument.CreateElement('x');
           TXMLProcessor.AddText(tag1, IntToStr(lBranch.hook.X));
           tag2.AppendChild(tag1);

           tag1 := ATag.OwnerDocument.CreateElement('y');
           TXMLProcessor.AddText(tag1, IntToStr(lBranch.hook.Y));
           tag2.AppendChild(tag1);

           iter := GetBlocks(lBranch.Index);
           while iter.HasNext do
              TXMLProcessor.ExportBlockToXML(TBlock(iter.Next), tag2);
        end;

      finally
         if lUnPin then
            UnPinComments;
      end;
   end;
end;

procedure TBlock.SaveInXML(const ATag: IXMLElement);
var
   lTextControl: TCustomEdit;
   lText: string;
   lTag: IXMLElement;
   iter: IIterator;
   lUnPin: boolean;
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute(BLOCK_TYPE_ATTR, IntToStr(Ord(BType)));
      ATag.SetAttribute(FRAME_ATTR, BoolToStr(FFrame, true));
      ATag.SetAttribute('x', IntToStr(Left));
      ATag.SetAttribute('y', IntToStr(Top));
      ATag.SetAttribute('h', IntToStr(Height));
      ATag.SetAttribute('w', IntToStr(Width));
      ATag.SetAttribute('bh', IntToStr(BottomHook));
      ATag.SetAttribute('brx', IntToStr(BottomPoint.X));
      ATag.SetAttribute(ID_ATTR, IntToStr(FId));
      ATag.SetAttribute('memW', IntToStr(memoWidth));
      ATag.SetAttribute('memH', IntToStr(memoHeight));
      ATag.SetAttribute('mem_vscroll', BoolToStr(FMemoVScroll, true));
      ATag.SetAttribute('mem_hscroll', BoolToStr(FMemoHScroll, true));
      ATag.SetAttribute('mem_wordwrap', BoolToStr(MemoWordWrap, true));
      ATag.SetAttribute(FONT_SIZE_ATTR, IntToStr(Font.Size));
      ATag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      lTextControl := GetTextControl;
      if (lTextControl <> nil) and (lTextControl.Text <> '') then
      begin
         lText := AnsiReplaceStr(lTextControl.Text, CRLF, CRLF_PLACEHOLDER);
         lTag := ATag.OwnerDocument.CreateElement(TEXT_TAG);
         TXMLProcessor.AddCDATA(lTag, lText);
         ATag.AppendChild(lTag);
      end;
      lUnPin := PinComments > 0;
      if lUnPin then
      begin
         iter := GetPinComments;
         while iter.HasNext do
            TComment(iter.Next).ExportToXMLTag2(ATag);
         UnPinComments;
      end;
   end;
end;

procedure TBlock.ImportCommentsFromXML(const ATag: IXMLElement);
var
   tag: IXMLElement;
   lComment: TComment;
begin
   if ProcessComments then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, COMMENT_ATTR);
      while tag <> nil do
      begin
         lComment := TComment.CreateDefault(Page);
         lComment.ImportFromXMLTag(tag, Self);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      UnPinComments;
   end;
end;

function TBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   tag: IXMLElement;
   lTextControl: TCustomEdit;
   lValue: integer;
begin
   result := errNone;
   if ATag <> nil then
   begin
      tag := TXMLProcessor.FindChildTag(ATag, TEXT_TAG);
      lTextControl := GetTextControl;
      if (tag <> nil) and (lTextControl <> nil) then
      begin
         FRefreshMode := true;
         lTextControl.Text := AnsiReplaceStr(tag.Text, CRLF_PLACEHOLDER, CRLF);
         FRefreshMode := false;
      end;

      lValue := StrToIntDef(ATag.GetAttribute(FONT_SIZE_ATTR), 8);
      if lValue in [8, 10, 12] then
         SetFontSize(lValue);

      lValue := StrToIntDef(ATag.GetAttribute(FONT_STYLE_ATTR), 0);
      SetFontStyle(TInfra.DecodeFontStyle(lValue));
      
      Frame := ATag.GetAttribute(FRAME_ATTR) = 'True';
      memoWidth := StrToIntDef(ATag.GetAttribute('memW'), 280);
      memoHeight := StrToIntDef(ATag.GetAttribute('memH'), 182);
      MemoVScroll := StrToBoolDef(ATag.GetAttribute('mem_vscroll'), false);
      MemoHScroll := StrToBoolDef(ATag.GetAttribute('mem_hscroll'), false);
      MemoWordWrap := StrToBoolDef(ATag.GetAttribute('mem_wordwrap'), false);

      ImportCommentsFromXML(ATag);
   end;
end;

function TGroupBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   lTag1, lTag2: IXMLElement;
   lBranchId, lBranchIdx, lBranchStmntId, hx, hy: integer;
begin
   result := inherited GetFromXML(ATag);
   if ATag <> nil then
   begin
      lTag1 := TXMLProcessor.FindChildTag(ATag, BRANCH_TAG);
      if lTag1 <> nil then
      begin
         lBranchIdx := PRIMARY_BRANCH_IND;
         repeat
            lTag2 := TXMLProcessor.FindChildTag(lTag1, 'x');
            if lTag2 <> nil then
               hx := StrToIntDef(lTag2.Text, 0);
            lTag2 := TXMLProcessor.FindChildTag(lTag1, 'y');
            if lTag2 <> nil then
               hy := StrToIntDef(lTag2.Text, 0);
            lBranchId := StrToIntDef(lTag1.GetAttribute(ID_ATTR), ID_INVALID);
            lBranchStmntId := StrToIntDef(lTag1.GetAttribute(BRANCH_STMNT_ATTR), ID_INVALID);
            if GetBranch(lBranchIdx) = nil then
               AddBranch(Point(hx, hy), false, lBranchId, lBranchStmntId);
            lTag2 := TXMLProcessor.FindChildTag(lTag1, BLOCK_TAG);
            if lTag2 <> nil then
            begin
               TXMLProcessor.ImportFlowchartFromXMLTag(lTag2, Self, nil, result, lBranchIdx);
               if result <> errNone then break;
            end;
            lBranchIdx := lBranchIdx + 1;
            lTag1 := TXMLProcessor.FindNextTag(lTag1);
         until lTag1 = nil;
      end;
      lTag2 := TXMLProcessor.FindChildTag(ATag, FOLD_TEXT_ATTR);
      if lTag2 <> nil then
         SetFoldedText(lTag2.Text);
      FFoldParms.Width := StrToIntDef(ATag.GetAttribute('fw'), 140);
      FFoldParms.Height := StrToIntDef(ATag.GetAttribute('fh'), 91);
      if ATag.GetAttribute(FOLDED_ATTR) = 'True' then
         ExpandFold(false);
   end;
end;

procedure TBlock.ExportToXMLTag(const ATag: IXMLElement);
var
   lBlock: TBlock;
begin
   TXMLProcessor.ExportBlockToXML(Self, ATag);
   if ParentBranch <> nil then
   begin
      lBlock := ParentBranch.First;
      while lBlock <> nil do
      begin
         if lBlock = Self then break;
         lBlock := lBlock.Next;
      end;
      if lBlock <> nil then
      begin
         lBlock := lBlock.Next;
         while lBlock <> nil do
         begin
            if not lBlock.Frame then
               break;
            TXMLProcessor.ExportBlockToXML(lBlock, ATag);
            lBlock := lBlock.Next;
         end;
      end;
   end;
end;

function TBlock.ImportFromXMLTag(const ATag: IXMLElement): TErrorType;
var
   lBlock, lNewBlock: TBlock;
   lParent: TGroupBlock;
   tag: IXMLElement;
begin
   result := errValidate;
   tag := TXMLProcessor.FindChildTag(ATag, BLOCK_TAG);
   if (tag = nil) or (tag.GetAttribute(BLOCK_TYPE_ATTR) = IntToStr(Ord(blMain))) then
      Gerr_text := i18Manager.GetString('BadImportTag')
   else
   begin
      lParent := nil;
      lBlock := Self;
      if (Ired = 0) and (FParentBlock <> nil) then
         lParent := FParentBlock
      else if Self is TGroupBlock then
      begin
         lParent := TGroupBlock(Self);
         lBlock := nil;
      end;
      if lParent <> nil then
      begin
         lParent.BlockImportMode := true;
         try
            lNewBlock := TXMLProcessor.ImportFlowchartFromXMLTag(tag, lParent, lBlock, result, Ired);
         finally
            lParent.BlockImportMode := false;
         end;
         if result = errNone then
         begin
            lParent.ResizeWithDrawLock;
            lNewBlock.ImportCommentsFromXML(tag);
         end;
      end;
   end;
end;

procedure TBlock.PopulateComboBoxes;
begin
end;

function TBlock.GetFocusColor: TColor;
var
   lEdit: TCustomEdit;
begin
   lEdit := GetTextControl;
   if lEdit <> nil then
      result := THackControl(lEdit).Font.Color
   else
      result := OK_COLOR;
end;

procedure TBlock.UpdateEditor(AEdit: TCustomEdit);
var
   lLine: TChangeLine;
begin
   if (AEdit <> nil) and PerformEditorUpdate then
   begin
      lLine := TInfra.GetChangeLine(Self, AEdit);
      if lLine.Row <> ROW_NOT_FOUND then
      begin
         lLine.Text := FastCodeAnsiStringReplace(lLine.Text, PRIMARY_PLACEHOLDER, AEdit.Text);
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(lLine);
         TInfra.GetEditorForm.SetCaretPos(lLine);
      end;
   end;
end;

function TBlock.PerformEditorUpdate: boolean;
begin
   result := TInfra.GetEditorForm.Visible and (not FRefreshMode) and not (fsStrikeOut in Font.Style);
end;

function TBlock.GetDescription: string;
var
   lTextControl: TCustomEdit;
begin
   lTextControl := GetTextControl;
   if lTextControl <> nil then
      result := FastCodeAnsiStringReplace(GInfra.CurrentLang.GetTemplateExpr(ClassType), PRIMARY_PLACEHOLDER, Trim(lTextControl.Text));
end;

procedure TBlock.ExportToGraphic(const AImage: TGraphic);
var
   lBitmap: TBitmap;
   iterc: IIterator;
   lComment: TComment;
   lPoint: TPoint;
begin
   if AImage is TBitmap then
      lBitmap := TBitmap(AImage)
   else
      lBitmap := TBitmap.Create;
   lBitmap.Width := Width + 2;
   lBitmap.Height := Height + 2;
   TMainBlock(FTopParentBlock).ShowI := false;
   lBitmap.Canvas.Lock;
   try
      PaintTo(lBitmap.Canvas.Handle, 1, 1);
      iterc := GetComments;
      while iterc.HasNext do
      begin
         lComment := TComment(iterc.Next);
         lPoint := TInfra.ParentToClient(Self, lComment.BoundsRect.TopLeft, Page);
         lComment.PaintTo(lBitmap.Canvas.Handle, lPoint.X, lPoint.Y);
      end;
   finally
      lBitmap.Canvas.Unlock;
      TMainBlock(FTopParentBlock).ShowI := true;
   end;
   if AImage <> lBitmap then
   begin
      AImage.Assign(lBitmap);
      lBitmap.Free;
   end;
end;

procedure TGroupBlock.PopulateComboBoxes;
var
   i: integer;
   lBlock: TBlock;
begin
   inherited PopulateComboBoxes;
   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      lBlock := FBranchArray[i].First;
      while lBlock <> nil do
      begin
         lBlock.PopulateComboBoxes;
         lBlock := lBlock.Next;
      end;
   end;
end;

function TGroupBlock.GetBranchCount: integer;
begin
   result := Length(FBranchArray) - 1;
end;

function TGroupBlock.GetBlocks(const AIndex: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   lFBranchIdx, lLBranchIdx, i, a: integer;
   lBlock: TBlock;
   lList: TObjectList;
begin
   lList := TObjectList.Create(false);
   if GetBranch(AIndex) <> nil then
   begin
      lFBranchIdx := AIndex;
      lLBranchIdx := AIndex;
   end
   else
   begin
      if AIndex < PRIMARY_BRANCH_IND then
      begin
         lFBranchIdx := PRIMARY_BRANCH_IND;
         lLBranchIdx := High(FBranchArray);
      end
      else
      begin
         lFBranchIdx := 0;
         lLBranchIdx := -1;
      end;
   end;
   a := 0;
   for i := lFBranchIdx to lLBranchIdx do
      a := a + FBranchArray[i].Count;
   if lList.Capacity < a then
      lList.Capacity := a;
   for i := lFBranchIdx to lLBranchIdx do
   begin
      lBlock := FBranchArray[i].First;
      while lBlock <> nil do
      begin
         lList.Add(lBlock);
         lBlock := lBlock.Next;
      end;
   end;
   result := TBlockIterator.Create(lList);
end;

function TGroupBlock.GetBranches(const AStart: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   i, lFBranchIdx, lLBranchIdx: integer;
   lList: TObjectList;
begin
   lList := TObjectList.Create(false);
   if GetBranch(AStart) <> nil then
      lFBranchIdx := AStart
   else
   begin
      if AStart < PRIMARY_BRANCH_IND then
         lFBranchIdx := PRIMARY_BRANCH_IND
      else
         lFBranchIdx := 0;           // case if lower index is greater than upper bound of branch_collection
   end;
   if lFBranchIdx = 0 then
      lLBranchIdx := -1
   else
      lLBranchIdx := High(FBranchArray);
   for i := lFBranchIdx to lLBranchIdx do
      lList.Add(FBranchArray[i]);
   result := TBranchIterator.Create(lList);
end;

function TBlock.SkipUpdateEditor: boolean;
var
   lHeader: TUserFunctionHeader;
begin
   lHeader := TInfra.GetFunctionHeader(Self);
   result := (lHeader <> nil) and (TInfra.IsNOkColor(lHeader.Font.Color) or lHeader.chkExtDeclare.Checked);
end;

function TBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lTmpList: TStringList;
begin
   result := 0;
   if fsStrikeOut in Font.Style then
      exit;
   lTmpList := TStringList.Create;
   try
      GenerateDefaultTemplate(lTmpList, ALangId, ADeep);
      TInfra.InsertLinesIntoList(ALines, lTmpList, AFromLine);
      result := lTmpList.Count;
   finally
      lTmpList.Free;
   end;
end;

procedure TBlock.GenerateDefaultTemplate(const ALines: TStringList; const ALangId: string; const ADeep: integer);
var
   lLangDef: TLangDefinition;
   lTemplate, lText: string;
   lTextControl: TCustomEdit;
begin
   lLangDef := GInfra.GetLangDefinition(ALangId);
   if lLangDef <> nil then
   begin
      lText := '';
      lTemplate := lLangDef.GetTemplate(Self.ClassType);
      lTextControl := GetTextControl;
      if lTextControl is TMemo then
         lText := lTextControl.Text
      else if lTextControl <> nil then
         lText := Trim(lTextControl.Text);
      lTemplate := FastCodeAnsiStringReplace(lTemplate, PRIMARY_PLACEHOLDER, lText);
      GenerateTemplateSection(ALines, lTemplate, ALangId, ADeep);
   end;
end;

function TGroupBlock.ExtractBranchIndex(const AStr: string): integer;
var
   i, b: integer;
   lInt: string;
begin
   result := AnsiPos('%b', AStr);
   if result <> 0 then
   begin
      lInt := '';
      for i := result+2 to Length(AStr) do
      begin
         if TryStrToInt(AStr[i], b) then
            lInt := lInt + AStr[i]
         else
            break;
      end;
      result := StrToIntDef(lInt, 0);
      if result > High(FBranchArray) then
         result := 0;
   end;
end;

procedure TBlock.GenerateTemplateSection(const ALines: TStringList; const ATemplate: string; const ALangId: string; const ADeep: integer);
var
   lStringList: TStringList;
begin
   lStringList := TStringList.Create;
   try
      lStringList.Text := ATemplate;
      GenerateTemplateSection(ALines, lStringList, ALangId, ADeep);
   finally
      lStringList.Free;
   end;
end;

procedure TBlock.GenerateTemplateSection(const ALines: TStringList; const ATemplate: TStringList; const ALangId: string; const ADeep: integer);
var
   lLine: string;
   i, lLineCount: integer;
   lObject: TObject;
begin
   lLineCount := ALines.Count + ATemplate.Count;
   if ALines.Capacity < lLineCount then
      ALines.Capacity := lLineCount;
   for i := 0 to ATemplate.Count-1 do
   begin
      lLine := DupeString(GSettings.IndentString, ADeep) + ATemplate[i];
      lLine := FastCodeAnsiStringReplace(lLine, INDENT_XML_CHAR, GSettings.IndentString);
      lObject := ATemplate.Objects[i];
      if lObject = nil then
         lObject := Self;
      ALines.AddObject(lLine, lObject);
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
   lLine: string;
   lObject: TObject;
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
         lLine := DupeString(GSettings.IndentString, ADeep) + ATemplate[i];
         lLine := FastCodeAnsiStringReplace(lLine, INDENT_XML_CHAR, GSettings.IndentString);
         lObject := ATemplate.Objects[i];
         if lObject = nil then
            lObject := Self;
         ALines.AddObject(lLine, lObject);
      end;
   end;
end;

function TBlock.Next: TBlock;
var
   lIndex: integer;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      lIndex := FParentBranch.IndexOf(Self);
      if (lIndex <> -1) and (FParentBranch.Last <> Self) then
         result := FParentBranch.Items[lIndex+1];
   end;
end;

function TBlock.Prev: TBlock;
var
   lIndex: integer;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      lIndex := FParentBranch.IndexOf(Self);
      if (lIndex <> -1) and (FParentBranch.First <> Self) then
         result := FParentBranch.Items[lIndex-1];
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
