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
   Classes, ComCtrls, Statement, OmniXML, Main_Form, BaseIterator, CommonInterfaces,
   CommonTypes, Contnrs;

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
   THackControl = class(TControl);
                                                            // base class derived from TCustomPanel
   TBlock = class(TCustomPanel, IIdentifiable, IFocusable)     // <-- the most important line in entire project; all started here
      private
         FParentBlock: TGroupBlock;
         FParentBranch: TBranch;
         FId: integer;
         FMemoVScroll,
         FMemoHScroll: boolean;
      protected
         FParentForm: TMainForm;
         FType: TBlockType;
         FStatement: TStatement;
         FTopParentBlock: TGroupBlock;
         FRefreshMode: boolean;
         procedure MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); virtual;
         procedure MyOnClick(Sender: TObject);
         procedure MyOnChange(Sender: TObject);
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
         procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
         procedure OnMouseLeave; virtual;
         procedure Paint; override;
         procedure DrawI;
         procedure DrawTextLabel(const x, y: integer; const AText: string);
         function GetId: integer;
         function PerformEditorUpdate: boolean;
         procedure SelectBlock(const APoint: TPoint);
         procedure SetCursor(const APoint: TPoint);
         procedure PutTextControls; virtual;
         function GetDefaultWidth: integer; virtual; abstract;
      public
         BottomPoint: TPoint;    // points to arrow at the bottom of the block
         IPoint: TPoint;          // points to I mark
         BottomHook: integer;
         TopHook: TPoint;
         Ired: Integer;           // indicates active arrow; -1: none, 0: bottom, 1: branch1, 2: branch2 and so on...
         HResizeInd: boolean;
         VResizeInd: boolean;
         FrameInd: boolean;
         memoWidth,
         memoHeight: integer;
         property MemoVScroll: boolean read FMemoVScroll write SetMemoVScroll;
         property MemoHScroll: boolean read FMemoHScroll write SetMemoHScroll;
         property MemoWordWrap: boolean read GetMemoWordWrap write SetMemoWordWrap;
         property TopParentBlock: TGroupBlock read FTopParentBlock default nil;
         property ParentForm: TMainForm read FParentForm default nil;
         property ParentBlock: TGroupBlock read FParentBlock default nil;
         property BType: TBlockType read FType default blUnknown;
         property ParentBranch: TBranch read FParentBranch default nil;
         property Id: integer read GetId;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AId: integer = ID_INVALID);
         destructor Destroy; override;
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
         function ExportToXMLFile(const filename: string): TErrorType;
         procedure ExportToXMLTag(const rootTag: IXMLElement);
         function ImportFromXMLFile(const filename: string): TErrorType;
         function ImportFromXMLTag(const root: IXMLElement): TErrorType;
         procedure ExportToGraphic(const AImage: TGraphic); virtual;
         procedure UpdateEditor(AEdit: TCustomEdit); virtual;
         function SkipUpdateEditor: boolean;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; virtual;
         function CanBeFocused: boolean;
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
         procedure Remove;
         function CanBeRemoved: boolean;
         function IsBoldDesc: boolean; virtual;
      published
         property Color;
         property OnMouseDown;
         property OnResize;
   end;

   TGroupBlock = class(TBlock)    // block which can aggregate child blocks
      private
         function GetBranchCount: integer;
      protected
         FMemoFolder: TMemo;
         FInitParms: TInitParms;
         FDrawingFlag: boolean;
         FBranchArray: array of TBranch;
         FTrueLabel, FFalseLabel: string;
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure SetWidth(const AMinX: integer); virtual; abstract;
         procedure OnMouseLeave; override;
         procedure LinkChildBlocks(const idx: integer = PRIMARY_BRANCH_IND-1);
         procedure Paint; override;
         function ExtractBranchIndex(const AStr: string): integer;
         procedure PutTextControls; override;
         function GetDiamondPoint: TPoint; virtual;
         procedure PlaceBranchStatement(const ABranchIdx: integer); virtual; abstract;
      public
         Branch: TBranch;     // primary branch to order child blocks
         Expanded: boolean;
         FFoldParms: TInitParms;
         property BranchCount: integer read GetBranchCount default 0;
         constructor Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AHook: TPoint; const AId: integer = ID_INVALID); overload;
         constructor Create(const ASource: TGroupBlock); overload;
         destructor Destroy; override;
         procedure ResizeHorz(const AContinue: boolean); virtual;
         procedure ResizeVert(const AContinue: boolean); virtual;
         function GenerateNestedCode(const ALines: TStringList; const ABranchInd, ADeep: integer; const ALangId: string): integer;
         procedure ExpandFold(const AResizeInd: boolean); virtual;
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
         function GetBlockIterator(const AIndex: integer = PRIMARY_BRANCH_IND-1): IIterator;
         function GetBranchIterator(const AStart: integer = PRIMARY_BRANCH_IND-1): IIterator;
         procedure ResizeWithDrawLock;
         function GetFoldedText: string;
         procedure SetFoldedText(const AText: string);
         function CountErrWarn: TErrWarnCount; override;
   end;

   TBranch = class(TObjectList, IIdentifiable)
      private
         FParentBlock: TGroupBlock;
         FParentWinControl: TWinControl;
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
         property ParentBlock: TGroupBlock read FParentBlock default nil;
         property ParentWinControl: TWinControl read FParentWinControl default nil;
         property Index: integer read GetIndex default 0;
         property Height: integer read GetHeight default 0;
         property Items[Index: integer]: TBlock read GetItems write SetItems; default;
         property First: TBlock read GetFirst;
         property Last: TBlock read GetLast;
         property Id: integer read GetId;
         constructor Create(const AParent: TWinControl; const AHook: TPoint; const AId: integer = ID_INVALID);
         destructor Destroy; override;
         procedure InsertAfter(ANewBlock, APosBlock: TBlock);
         procedure Insert(AIndex: integer; ABlock: TBlock);
         function IndexOf(ABlock: TBlock): integer;
         function Add(ABlock: TBlock): integer;
         function Remove(ABlock: TObject): integer;
         procedure UndoRemove(ABlock: TBlock);
   end;

   TBranchIterator = class(TBaseIterator);

   TBlockIterator = class(TBaseIterator);

implementation

uses
   Main_Block, ApplicationCommon, BlockFactory, StrUtils, SourceEditor_Form,
   UserFunction, Menus, XMLProcessor, Navigator_Form, LangDefinition,
   FastcodeAnsiStringReplaceUnit, FlashThread;

type
   THackCustomEdit = class(TCustomEdit);

constructor TBlock.Create(const ABranch: TBranch; const ALeft, ATop, AWidth, AHeight: Integer; const AId: integer = ID_INVALID);
begin

   if ABranch <> nil then
   begin
      inherited Create(ABranch.ParentWinControl);
      Parent := ABranch.ParentWinControl;
      FParentBranch := ABranch;
      FParentBlock := TGroupBlock(Parent);
      FTopParentBlock := FParentBlock.TopParentBlock;
      FParentForm := FParentBlock.ParentForm;
   end
   else                                     // the current object is in fact of TMainBlock class
   begin
      if FParentForm = nil then
         FParentForm := TInfra.GetMainForm;
      inherited Create(FParentForm);
      Parent := FParentForm;
      FTopParentBlock := TGroupBlock(Self);
   end;

   ParentFont  := true;
   ParentColor := true;
   BevelOuter  := bvNone;
   Ctl3D       := false;
   Color       := FParentForm.Color;
   Font.Name   := GSettings.FlowchartFontName;
   Canvas.Font := Font;
   PopupMenu   := FParentForm.PopupMenu;
   DoubleBuffered := GSettings.EnableDBuffering;
   ControlStyle := ControlStyle + [csOpaque];
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
   OnClick     := MyOnClick;
   OnCanResize := MyOnCanResize;
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
      DoubleBuffered := true;
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

constructor TGroupBlock.Create(const ASource: TGroupBlock);
var
   lBlock: TBlock;
   lNewBlock, lPrevBlock: TBlock;
   lBranch, lBranch2: TBranch;
   i: integer;
   lTextControl, lSourceTextControl: TCustomEdit;
begin
   Visible := ASource.Visible;
   SetFont(ASource.Font);
   lSourceTextControl := ASource.GetTextControl;
   lTextControl := GetTextControl;
   if (lSourceTextControl <> nil) and (lTextControl <> nil) then
      lTextControl.Text := lSourceTextControl.Text;
   FMemoFolder.Text := ASource.FMemoFolder.Text;

   for i := PRIMARY_BRANCH_IND to High(ASource.FBranchArray) do
   begin
      lBranch := ASource.FBranchArray[i];
      lBranch2 := GetBranch(i);
      if lBranch2 = nil then
         lBranch2 := AddBranch(lBranch.Hook, false);
      lBlock := lBranch.First;
      lPrevBlock := nil;
      while lBlock <> nil do
      begin
         lNewBlock := TBlockFactory.CloneBlock(lBranch2, lBlock);
         lBranch2.InsertAfter(lNewBlock, lPrevBlock);
         lPrevBlock := lBranch2.Last;
         lBlock := lBlock.Next;
      end;
   end;

   if not ASource.Expanded then
   begin
      Expanded := false;
      FFoldParms := ASource.FFoldParms;
      Constraints.MinWidth := 140;
      Constraints.MinHeight := 54;
      Width := ASource.Width;
      Height := ASource.Height;
      FMemoFolder.SetBounds(3, 3, Width-6, Height-36);
      FMemoFolder.Anchors := [akRight, akLeft, akBottom, akTop];
      BottomPoint.X := Width div 2;
      BottomPoint.Y := Height - 30;
      IPoint.X := (Width div 2) + 30;
      IPoint.Y := FMemoFolder.Height + 15;
      TopHook.X := Width div 2;
      BottomHook := Width div 2;
      if lTextControl <> nil then
         lTextControl.Visible := false;
      FMemoFolder.Visible := true;
   end
   else
   begin
      FFoldParms.Width := ASource.FFoldParms.Width;
      FFoldParms.Height := ASource.FFoldParms.Height;
   end;
end;

destructor TBlock.Destroy;
begin
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
   for i := Low(FBranchArray) to High(FBranchArray) do
      FBranchArray[i].Free;
   FBranchArray := nil;
   inherited Destroy;
end;

procedure TBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   SelectBlock(Point(X, Y));
   SetCursor(Point(X, Y));
   if PtInRect(Rect(BottomPoint.X-5, BottomPoint.Y, BottomPoint.X+5, Height), Point(X, Y)) then
   begin
      TInfra.DrawArrowLine(Canvas, BottomPoint, Point(BottomPoint.X, Height-1), arrEnd, clRed);
      Ired := 0;
      Cursor := TCursor(GCustomCursor);
   end
   else if Ired = 0 then
   begin
      TInfra.DrawArrowLine(Canvas, BottomPoint, Point(BottomPoint.X, Height-1));
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
            TInfra.DrawArrowLine(Canvas, Point(lPoint.X, TopHook.Y), lPoint, arrEnd, clRed);
            Ired := i;
            Cursor := TCursor(GCustomCursor);
            break;
         end
         else if Ired = i then
         begin
            TInfra.DrawArrowLine(Canvas, Point(lPoint.X, TopHook.Y), lPoint);
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
   if FrameInd and PtInRect(Rect(Width-5, 0, Width, Height-5), APoint) then
      Cursor := crSizeWE
   else if FrameInd and PtInRect(Rect(0, Height-5, Width-5, Height), APoint) then
      Cursor := crSizeNS
   else if FrameInd and PtInRect(Rect(Width-5, Height-5, Width, Height), APoint) then
      Cursor := crSizeNWSE
   else if IsCursorResize then
      Cursor := crDefault;
end;

procedure TBlock.OnMouseLeave;
begin
   if Cursor <> crDefault then
      Cursor := crDefault;
   ClearSelection;
   if Ired = 0 then
      TInfra.DrawArrowLine(Canvas, BottomPoint, Point(BottomPoint.X, Height-1));
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
      TInfra.DrawArrowLine(Canvas, Point(lPoint.X, TopHook.Y), lPoint);
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

procedure TBlock.MyOnClick(Sender: TObject);
begin
   if IsCursorSelect and (GetAsyncKeyState(VK_SHIFT) = 0) then
      ChangeFrame;
end;

procedure TBlock.ChangeFrame;
begin
   FrameInd := not FrameInd;
   GChange := 1;
   Invalidate;
   ClearSelection;
   if FrameInd then
      SourceEditorForm.SelectCodeRange(Self)
   else
      SourceEditorForm.UnSelectCodeRange(Self);
end;

function TGroupBlock.GenerateNestedCode(const ALines: TStringList; const ABranchInd, ADeep: integer; const ALangId: string): integer;
var
   lBlock: TBlock;
begin
   result := 0;
   if (ABranchInd >= PRIMARY_BRANCH_IND) and (ABranchInd <= High(FBranchArray)) then
   begin
      lBlock := FBranchArray[ABranchInd].First;
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
begin
   if Button = mbLeft then
   begin
      if ssShift in Shift then
      begin
         if Color <> FParentForm.Color then
            BeginDrag(true);
      end
      else if (not PtInRect(Rect(IPoint.X-5, IPoint.Y, IPoint.X+5, IPoint.Y+10), Point(X, Y))) and not IsCursorResize then
      begin          // drag entire flowchart
         ReleaseCapture;
         FTopParentBlock.BringToFront;
         SendMessage(FTopParentBlock.Handle, WM_SYSCOMMAND, $F012, 0);
         FTopParentBlock.OnResize(FTopParentBlock);
         if Ired >= 0 then
         begin
            lMitem := nil;
            case GCustomCursor of
               crAssign:      lMitem := FParentForm.miAssign;
               crMultiAssign: lMitem := FParentForm.miMultipleAssign;
               crIfElse:      lMitem := FParentForm.miIfElse;
               crWhile:       lMitem := FParentForm.miWhile;
               crFor:         lMitem := FParentForm.miFor;
               crRepeat:      lMitem := FParentForm.miRepeat;
               crInput:       lMitem := FParentForm.miInput;
               crOutput:      lMitem := FParentForm.miOutput;
               crFuncCall:    lMitem := FParentForm.miRoutineCall;
               crIf:          lMitem := FParentForm.miIf;
               crCase:        lMitem := FParentForm.miCase;
               crFolder:      lMitem := FParentForm.miFolder;
               crText:        lMitem := FParentForm.miText;
               crReturn:
               begin
                  if CanInsertReturnBlock then
                     lMitem := FParentForm.miReturn;
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
               TGroupBlock(Self).LinkChildBlocks;
            if FParentBlock <> nil then
               FParentBlock.ResizeVert(true);
            VResizeInd := false;
         end;
      finally
         if lLocked then
            UnLockDrawing;
      end;
      GChange := 1;
      if Self is TMainBlock then
         TMainBlock(Self).BringAllToFront;
      NavigatorForm.Repaint;
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
   LinkChildBlocks;

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
   LinkChildBlocks;

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
      LinkChildBlocks;
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
   else if GSettings.ShowFlowchartLabels then
      RepaintAll;

end;

procedure TGroupBlock.LinkChildBlocks(const idx: integer = PRIMARY_BRANCH_IND-1);
var
   lBlock, lBlockPrev: TBlock;
   i, lStart, lStop, lLeft, lTop: integer;
begin

   if (idx >= PRIMARY_BRANCH_IND) and (idx <= High(FBranchArray)) then
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
         lLeft := FBranchArray[i].hook.X - lBlock.TopHook.X;
         lTop := FBranchArray[i].hook.Y + 1;
         lBlock.SetBounds(lLeft, lTop, lBlock.Width, lBlock.Height);
         lBlock := lBlock.Next;
         while lBlock <> nil do
         begin
            lBlockPrev := lBlock.Prev;
            lLeft := lBlockPrev.BottomPoint.X + lBlockPrev.Left - lBlock.TopHook.X;
            lTop :=  lBlockPrev.BoundsRect.Bottom;
            lBlock.SetBounds(lLeft, lTop, lBlock.Width, lBlock.Height);
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

procedure TBlock.SetFontStyle(const AStyle: TFontStyles);
var
   i: Integer;
begin
   Font.Style := AStyle;
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
   i: Integer;
begin
   Font.Size := ASize;
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
    lBool := NavigatorForm.RepaintInd;
    NavigatorForm.RepaintInd := false;
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
    NavigatorForm.RepaintInd := lBool;
end;

function TBlock.GetId: integer;
begin
   result := FId;
end;

procedure TBlock.ChangeColor(const AColor: TColor);
begin
   Color := AColor;
end;

procedure TBlock.ClearSelection;
begin
   if Color <> FParentForm.Color then
      ChangeColor(FParentForm.Color);
   NavigatorForm.Repaint;
end;

procedure TGroupBlock.ChangeColor(const AColor: TColor);
var
   i: integer;
   lBlock: TBlock;
begin

   inherited ChangeColor(AColor);

   for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
   begin
      lBlock := FBranchArray[i].First;
      while lBlock <> nil do
      begin
         lBlock.ChangeColor(AColor);
         lBlock := lBlock.Next;
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
            SourceEditorForm.SelectCodeRange(Self);
         NavigatorForm.Repaint;
      end;
   end
   else if Color <> FParentForm.Color then
   begin
      ChangeColor(FParentForm.Color);
      if GSettings.EditorAutoSelectBlock and not FrameInd then
         SourceEditorForm.UnSelectCodeRange(Self);
      NavigatorForm.Repaint;
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
      Pen.Color := clBlack;
      Pen.Width := 1;
      if FrameInd then
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

procedure TBlock.DrawTextLabel(const x, y: integer; const AText: string);
var
   lFontStyles: TFontStyles;
begin
   lFontStyles := Canvas.Font.Style;
   Canvas.Font.Style := [];
   Canvas.Brush.Style := bsClear;
   Canvas.TextOut(x, y, AText);
   Canvas.Font.Style := lFontStyles;
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
   iter := GetBlockIterator;
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
   with Canvas do
   begin
      lStyle := Brush.Style;
      lColor := Brush.Color;
      lWidth := Pen.Width;
      if Expanded then
      begin
         lTop := GetDiamondPoint;
         if not InvalidPoint(lTop) then
         begin
            Brush.Style := bsClear;
            if GSettings.DiamondColor <> GSettings.DesktopColor then
               Brush.Color := GSettings.DiamondColor;
            Polygon([Point(lTop.X-60, lTop.Y+30),
                     Point(lTop.X, lTop.Y+60),
                     Point(lTop.X+60, lTop.Y+30),
                     lTop,
                     Point(lTop.X-60, lTop.Y+30)]);
         end;
      end
      else if FMemoFolder <> nil then
      begin
         if FTopParentBlock <> Self then
            TInfra.DrawArrowLine(Canvas, Point(BottomPoint.X, Height-31), Point(BottomPoint.X, Height-1));
         Pen.Width := 2;
         Brush.Style := bsClear;
         if GSettings.FoldColor <> GSettings.DesktopColor then
            Brush.Color := GSettings.FoldColor;
         Polygon([Point(1, 1),
                  Point(Width-1, 1),
                  Point(Width-1, FMemoFolder.Height+5),
                  Point(1, FMemoFolder.Height+5),
                  Point(1, 0)]);
      end;
      Brush.Style := lStyle;
      Brush.Color := lColor;
      Pen.Width := lWidth;
   end;
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
      RedrawWindow(FParentForm.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE);
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
begin
   inherited;
   lPoint := ClientToParent(Point(0, 0), FParentForm);
   if HResizeInd then
      Msg.MinMaxInfo.ptMaxTrackSize.X := FParentForm.ClientWidth - lPoint.X;
   if VResizeInd then
      Msg.MinMaxInfo.ptMaxTrackSize.Y := FParentForm.ClientHeight - lPoint.Y;
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
   NavigatorForm.ExecuteRepaint;
end;

procedure TBlock.OnChangeMemo(Sender: TObject);
begin
   UpdateMemoVScroll;
   UpdateMemoHScroll;
   NavigatorForm.ExecuteRepaint;
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

procedure TBlock.WMExitSizeMove(var Msg: TMessage);
begin
   inherited;
   UpdateMemoVScroll;
   UpdateMemoHScroll;
end;

function TBlock.CanBeFocused: boolean;
var
   lTextControl: TCustomEdit;
begin
   lTextControl := GetTextControl;
   result := (lTextControl <> nil) and lTextControl.CanFocus;
end;

function TBlock.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   if AInfo.FocusEdit = nil then
      AInfo.FocusEdit := GetTextControl;
   AInfo.FocusEditForm := FParentForm;
   result := FocusOnTextControl(AInfo);
end;

function TBlock.FocusOnTextControl(AInfo: TFocusInfo): boolean;
var
   idx, idx2, i: integer;
   lText: string;
   lMemo: TMemo;
begin
   result := false;
   if (AInfo.FocusEdit <> nil) and ContainsControl(AInfo.FocusEdit) and AInfo.FocusEdit.CanFocus then
   begin
      FParentForm.Show;
      TMainBlock(FTopParentBlock).BringAllToFront;
      FParentForm.ScrollInView(AInfo.FocusEdit);
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
      TFlashThread.Create(AInfo);
      result := true;
   end;
end;

function TBlock.GenerateTree(const AParentNode: TTreeNode): TTreeNode;
var
   lStrErr: string;
   idx: integer;
   lTextControl: TCustomEdit;
   lColor: TColor;
begin
   result := AParentNode;
   lTextControl := GetTextControl;
   if lTextControl <> nil then
   begin
      lStrErr := '';
      lColor := THackControl(lTextControl).Font.Color;
      if TInfra.IsRestricted(lColor) then
      begin
         lStrErr := lTextControl.Hint;
         idx := TInfra.RPos(#10, lStrErr);
         if idx <> 0 then
            lStrErr := ' - ' + AnsiRightStr(lStrErr, Length(lStrErr)-idx);
      end;
      result := AParentNode.Owner.AddChildObject(AParentNode, GetDescription + lStrErr, lTextControl);
      if TInfra.IsRestricted(lColor) then
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
begin
   result := TBranch.Create(Self, AHook, ABranchId);
   if Length(FBranchArray) = 0 then
      SetLength(FBranchArray, 1);
   SetLength(FBranchArray, Length(FBranchArray)+1);
   FBranchArray[High(FBranchArray)] := result;
end;

function TBlock.CanBeRemoved: boolean;
begin
   result := Visible;
end;

procedure TBlock.Remove;
begin
   if CanBeRemoved then
   begin
      FParentForm.PopupMenu.PopupComponent := Self;
      FParentForm.miRemove.Click;
   end;
end;

function TBlock.IsBoldDesc: boolean;
begin
   result := false;
end;

procedure TGroupBlock.ExpandFold(const AResizeInd: boolean);
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
         lBlock := lBlock.next;
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
   if AResizeInd and (FParentBlock <> nil) then
   begin
      FParentBlock.ResizeWithDrawLock;
      NavigatorForm.Repaint;
   end;
end;

function TBlock.GetFromXML(const ATag: IXMLElement): TErrorType;
var
   lTag: IXMLElement;
   lTextControl: TCustomEdit;
   lValue: integer;
begin
   result := errNone;
   if ATag <> nil then
   begin
      lTag := TXMLProcessor.FindChildTag(ATag, 'text');
      lTextControl := GetTextControl;
      if (lTag <> nil) and (lTextControl <> nil) then
      begin
         FRefreshMode := true;
         lTextControl.Text := AnsiReplaceStr(lTag.Text, '#!', CRLF);
         FRefreshMode := false;
      end;
      lValue := StrToIntDef(ATag.GetAttribute('fontsize'), 8);
      if lValue in [8, 10, 12] then
         SetFontSize(lValue);
      lValue := StrToIntDef(ATag.GetAttribute('fontstyle'), 0);
      SetFontStyle(TInfra.DecodeFontStyle(lValue));
      FrameInd := ATag.GetAttribute('frame') = 'True';
      memoWidth := StrToIntDef(ATag.GetAttribute('memW'), 280);
      memoHeight := StrToIntDef(ATag.GetAttribute('memH'), 182);
      MemoVScroll := StrToBoolDef(ATag.GetAttribute('mem_vscroll'), false);
      MemoHScroll := StrToBoolDef(ATag.GetAttribute('mem_hscroll'), false);
      MemoWordWrap := StrToBoolDef(ATag.GetAttribute('mem_wordwrap'), false);
   end;
end;

procedure TGroupBlock.SaveInXML(const ATag: IXMLElement);
var
   brx, fw, fh, i: integer;
   lText: string;
   tag1, tag2: IXMLElement;
   lBranch: TBranch;
   iter: IIterator;
begin
   inherited SaveInXML(ATag);
   if ATag <> nil then
   begin
      if Expanded then
      begin
         fw := FFoldParms.Width;
         fh := FFoldParms.Height;
         brx := Branch.Hook.X;
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
      ATag.SetAttribute('brx', IntToStr(brx));
      ATag.SetAttribute('bry', IntToStr(Branch.Hook.Y));
      ATag.SetAttribute('fw', IntToStr(fw));
      ATag.SetAttribute('fh', IntToStr(fh));
      ATag.SetAttribute('folded', BoolToStr(not Expanded, true));
      lText := GetFoldedText;
      if lText <> '' then
      begin
         tag1 := ATag.OwnerDocument.CreateElement('foldtext');
         TXMLProcessor.AddCDATA(tag1, lText);
         ATag.AppendChild(tag1);
      end;

      for i := PRIMARY_BRANCH_IND to High(FBranchArray) do
      begin
         lBranch := FBranchArray[i];

         tag2 := ATag.OwnerDocument.CreateElement('branch');
         ATag.AppendChild(tag2);

         tag2.SetAttribute(ID_ATTR_NAME, IntToStr(lBranch.Id));

         if lBranch.Statement <> nil then
            tag2.SetAttribute('bstmnt_hash', IntToStr(lBranch.Statement.Id));

         tag1 := ATag.OwnerDocument.CreateElement('x');
         TXMLProcessor.AddText(tag1, IntToStr(lBranch.hook.X));
         tag2.AppendChild(tag1);

         tag1 := ATag.OwnerDocument.CreateElement('y');
         TXMLProcessor.AddText(tag1, IntToStr(lBranch.hook.Y));
         tag2.AppendChild(tag1);

         iter := GetBlockIterator(lBranch.Index);
         while iter.HasNext do
            TXMLProcessor.ExportBlockToXML(TBlock(iter.Next), tag2);
      end;

   end;
end;

procedure TBlock.SaveInXML(const ATag: IXMLElement);
var
   lTextControl: TCustomEdit;
   lText: string;
   lTag: IXMLElement;
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute('type', IntToStr(Ord(BType)));
      ATag.SetAttribute('frame', BoolToStr(FrameInd, true));
      ATag.SetAttribute('x', IntToStr(Left));
      ATag.SetAttribute('y', IntToStr(Top));
      ATag.SetAttribute('h', IntToStr(Height));
      ATag.SetAttribute('w', IntToStr(Width));
      ATag.SetAttribute('bh', IntToStr(BottomHook));
      ATag.SetAttribute('brx', IntToStr(BottomPoint.X));
      ATag.SetAttribute(ID_ATTR_NAME, IntToStr(FId));
      ATag.SetAttribute('memW', IntToStr(memoWidth));
      ATag.SetAttribute('memH', IntToStr(memoHeight));
      ATag.SetAttribute('mem_vscroll', BoolToStr(FMemoVScroll, true));
      ATag.SetAttribute('mem_hscroll', BoolToStr(FMemoHScroll, true));
      ATag.SetAttribute('mem_wordwrap', BoolToStr(MemoWordWrap, true));
      lTextControl := GetTextControl;
      if lTextControl is TControl then
      begin
         ATag.SetAttribute('fontsize', IntToStr(THackControl(lTextControl).Font.Size));
         ATag.SetAttribute('fontstyle', TInfra.EncodeFontStyle(THackControl(lTextControl).Font.Style));
         if lTextControl.Text <> '' then
         begin
            lText := AnsiReplaceStr(lTextControl.Text, CRLF, '#!');
            lTag := ATag.OwnerDocument.CreateElement('text');
            TXMLProcessor.AddCDATA(lTag, lText);
            ATag.AppendChild(lTag);
         end;
      end;
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
      lTag1 := TXMLProcessor.FindChildTag(ATag, 'branch');
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
            lBranchId := StrToIntDef(lTag1.GetAttribute(ID_ATTR_NAME), ID_INVALID);
            lBranchStmntId := StrToIntDef(lTag1.GetAttribute('bstmnt_hash'), ID_INVALID);
            if GetBranch(lBranchIdx) = nil then
               AddBranch(Point(hx, hy), false, lBranchId, lBranchStmntId);
            lTag2 := TXMLProcessor.FindChildTag(lTag1, 'block');
            if lTag2 <> nil then
            begin
               TXMLProcessor.ImportFlowchartFromXMLTag(lTag2, Self, nil, result, lBranchIdx);
               if result <> errNone then break;
            end;
            lBranchIdx := lBranchIdx + 1;
            lTag1 := TXMLProcessor.FindNextTag(lTag1);
         until lTag1 = nil;
      end;
      lTag2 := TXMLProcessor.FindChildTag(ATag, 'foldtext');
      if lTag2 <> nil then
         SetFoldedText(lTag2.Text);
      FFoldParms.Width := StrToIntDef(ATag.GetAttribute('fw'), 140);
      FFoldParms.Height := StrToIntDef(ATag.GetAttribute('fh'), 91);
      if ATag.GetAttribute('folded') = 'True' then
         ExpandFold(false);
   end;
end;

function TBlock.ExportToXMLFile(const filename: string): TErrorType;
begin
   result := TXMLProcessor.ExportToXMLFile(filename, ExportToXMLTag);
end;

procedure TBlock.ExportToXMLTag(const rootTag: IXMLElement);
var
   lBlock: TBlock;
begin
   TXMLProcessor.ExportBlockToXML(Self, rootTag);
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
            if not lBlock.FrameInd then break;
            TXMLProcessor.ExportBlockToXML(lBlock, rootTag);
            lBlock := lBlock.Next;
         end;
      end;
   end;
end;

function TBlock.ImportFromXMLFile(const filename: string): TErrorType;
begin
   result := TXMLProcessor.ImportFromXMLFile(filename, ImportFromXMLTag);
end;

function TBlock.ImportFromXMLTag(const root: IXMLElement): TErrorType;
var
   lBlock: TBlock;
   lParent: TGroupBlock;
   lBlockTag: IXMLElement;
begin
   result := errValidate;
   lBlockTag := TXMLProcessor.FindChildTag(root, 'block');
   if (lBlockTag = nil) or (lBlockTag.GetAttribute('type') = IntToStr(Ord(blMain))) then
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
         TXMLProcessor.ImportFlowchartFromXMLTag(lBlockTag, lParent, lBlock, result, Ired);
         if result = errNone then
            lParent.ResizeWithDrawLock;
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
         TInfra.SetEditorCaretPos(lLine);
      end;
   end;
end;

function TBlock.PerformEditorUpdate: boolean;
begin
   result := SourceEditorForm.Visible and not FRefreshMode;
end;

function TBlock.GetDescription: string;
var
   lTextControl: TCustomEdit;
begin
   lTextControl := GetTextControl;
   if lTextControl <> nil then
      result := i18Manager.GetFormattedString(BLOCK_TO_DESCKEY_MAP[FType], [AnsiReplaceStr(Trim(lTextControl.Text), CRLF, ' ')]);
end;

procedure TBlock.ExportToGraphic(const AImage: TGraphic);
var
   lBitmap: TBitmap;
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

function TGroupBlock.GetBlockIterator(const AIndex: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   lFBranchIdx, lLBranchIdx, i, a: integer;
   lBlock: TBlock;
   lIterator: TBlockIterator;
begin
   lIterator := TBlockIterator.Create;
   if (AIndex >= PRIMARY_BRANCH_IND) and (AIndex <= High(FBranchArray)) then
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
   if a > 0 then
   begin
      SetLength(lIterator.FArray, a);
      a := 0;
      for i := lFBranchIdx to lLBranchIdx do
      begin
         lBlock := FBranchArray[i].First;
         while lBlock <> nil do
         begin
            lIterator.FArray[a] := lBlock;
            lBlock := lBlock.Next;
            a := a + 1;
         end;
      end;
   end;
   result := lIterator;
end;

function TGroupBlock.GetBranchIterator(const AStart: integer = PRIMARY_BRANCH_IND-1): IIterator;
var
   i, lFBranchIdx, lLBranchIdx, a: integer;
   lIterator: TBranchIterator;
begin
   lIterator := TBranchIterator.Create;
   if (AStart >= PRIMARY_BRANCH_IND) and (AStart <= High(FBranchArray)) then
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
   a := lLBranchIdx - lFBranchIdx + 1;
   if a > 0 then
   begin
      SetLength(lIterator.FArray, a);
      a := 0;
      for i := lFBranchIdx to lLBranchIdx do
      begin
         lIterator.FArray[a] := FBranchArray[i];
         a := a + 1;
      end;
   end;
   result := lIterator;
end;

function TBlock.SkipUpdateEditor: boolean;
var
   lHeader: TUserFunctionHeader;
begin
   lHeader := nil;
   if TMainBlock(FTopParentBlock).OwnerUserFunction <> nil then
      lHeader := TUserFunction(TMainBlock(FTopParentBlock).OwnerUserFunction).Header;
   result := (lHeader <> nil) and (TInfra.IsRestricted(lHeader.Font.Color) or lHeader.chkExtDeclare.Checked);
end;

function TBlock.GenerateCode(const ALines: TStringList; const ALangId: string; const ADeep: integer; const AFromLine: integer = LAST_LINE): integer;
var
   lTmpList: TStringList;
begin
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
   i: integer;
   lObject: TObject;
begin
   if ALines.Capacity < ALines.Count + ATemplate.Count then
      ALines.Capacity := ALines.Count + ATemplate.Count;
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

constructor TBranch.Create(const AParent: TWinControl; const AHook: TPoint; const AId: integer = ID_INVALID);
begin
   inherited Create;
   if AParent is TGroupBlock then
      FParentBlock := TGroupBlock(AParent)
   else
      FParentBlock := nil;
   FParentWinControl := AParent;
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
      iter := FParentBlock.GetBranchIterator;
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