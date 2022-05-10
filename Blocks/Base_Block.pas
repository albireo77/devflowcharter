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
   WinApi.Windows, Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics, WinApi.Messages, System.Classes,
   Vcl.ComCtrls, Generics.Collections, Statement, OmniXML, BaseEnumerator,
   Interfaces, Types, BlockTabSheet, Comment, MemoEx, YaccLib;

const
   PRIMARY_BRANCH_IDX = 1;
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

   TBlock = class(TCustomControl, IWithId, IWithFocus, IExportable, IMemoEx)
      private
         FParentBlock: TGroupBlock;
         FParentBranch: TBranch;
         FId: integer;
         function IsInSelect(const APoint: TPoint): boolean;
         procedure RefreshStatements;
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
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AParserMode: TYYMode; AAlignment: TAlignment);
         procedure MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); virtual;
         procedure MyOnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
         procedure MyOnDragDrop(Sender, Source: TObject; X, Y: Integer);
         procedure MyOnChange(Sender: TObject);
         procedure MyOnDblClick(Sender: TObject);
         procedure OnChangeMemo(Sender: TObject); virtual;
         procedure WMMouseLeave(var Msg: TMessage); message WM_MOUSELEAVE;
         procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
         procedure WMExitSizeMove(var Msg: TWMMove); message WM_EXITSIZEMOVE;
         procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
         procedure Paint; override;
         procedure DrawI;
         function DrawTextLabel(x, y: integer; const AText: string; ARightJust: boolean = false; ADownJust: boolean = false; APrint: boolean = true): TRect;
         procedure DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
         function GetId: integer;
         function PerformEditorUpdate: boolean;
         procedure SelectBlock(const APoint: TPoint);
         procedure SetCursor(const APoint: TPoint);
         procedure SetFrame(AValue: boolean);
         procedure PutTextControls; virtual;
         procedure DrawArrowTo(const aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrowTo(toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(const aFrom, aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(fromX, fromY, toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(fromX, fromY: integer; const aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(const aFrom: TPoint; toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         function GetEllipseTextRect(ax, ay: integer; const AText: string): TRect;
         function DrawEllipsedText(ax, ay: integer; const AText: string): TRect;
         procedure MoveComments(x, y: integer);
         function GetUndoObject: TObject; virtual;
         function IsInFront(AControl: TWinControl): boolean;
         procedure SetPage(APage: TBlockTabSheet); virtual;
         function GetPage: TBlockTabSheet; virtual;
         procedure CreateParams(var Params: TCreateParams); override;
         procedure OnWindowPosChanged(x, y: integer); virtual;
         function ProcessComments: boolean;
         function IsForeParent(AParent: TObject): boolean;
         function GetErrorMsg(AEdit: TCustomEdit): string;
         procedure SaveInXML2(ATag: IXMLElement);
         procedure ExitSizeMove;
         procedure SetBrushColor(AShape: TColorShape);
         function GetBlockParms: TBlockParms; virtual;
         function GetBlockTemplate(const ALangId: string): string;
         function GetBlockTemplateExpr(const ALangId: string): string;
         function FindTemplate(const ALangId: string; const ATemplate: string): string;
      public
         BottomPoint: TPoint;    // points to arrow at the bottom of the block
         IPoint: TPoint;          // points to I mark
         BottomHook: integer;
         TopHook: TPoint;
         Ired: Integer;           // indicates active arrow; -1: none, 0: bottom, 1: branch1, 2: branch2...
         property Frame: boolean read FFrame write SetFrame;
         property TopParentBlock: TGroupBlock read FTopParentBlock;
         property Page: TBlockTabSheet read GetPage write SetPage;
         property ParentBlock: TGroupBlock read FParentBlock;
         property BType: TBlockType read FType default blUnknown;
         property ParentBranch: TBranch read FParentBranch;
         property Id: integer read GetId;
         destructor Destroy; override;
         function Clone(ABranch: TBranch): TBlock;
         procedure ChangeColor(AColor: TColor); virtual;
         procedure SetFontStyle(const AStyle: TFontStyles);
         procedure SetFontSize(ASize: integer);
         function GetFont: TFont;
         procedure SetFont(AFont: TFont);
         procedure PerformRefreshStatements;
         procedure PopulateComboBoxes; virtual;
         function GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer; virtual;
         function GetFromXML(ATag: IXMLElement): TError; virtual;
         procedure SaveInXML(ATag: IXMLElement); virtual;
         function FillTemplate(const ALangId: string; const ATemplate: string = ''): string; virtual;
         function FillCodedTemplate(const ALangId: string): string; virtual;
         function GetDescTemplate(const ALangId: string): string; virtual;
         function GetTextControl: TCustomEdit; virtual;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; virtual;
         function IsCursorSelect: boolean;
         function IsCursorResize: boolean;
         function CanInsertReturnBlock: boolean; virtual;
         procedure ExportToXMLTag(ATag: IXMLElement);
         function ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
         procedure ExportToGraphic(AGraphic: TGraphic); virtual;
         procedure UpdateEditor(AEdit: TCustomEdit); virtual;
         function SkipUpdateEditor: boolean;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; virtual;
         function CanBeFocused: boolean; virtual;
         function GetTreeNodeText(ANodeOffset: integer = 0): string; virtual;
         function FindLastRow(AStart: integer; ALines: TStrings): integer; virtual;
         procedure GenerateDefaultTemplate(ALines: TStringList; const ALangId: string; ADeep: integer);
         procedure GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer); overload; virtual;
         procedure GenerateTemplateSection(ALines: TStringList; const ATemplate: string; const ALangId: string; ADeep: integer); overload;
         function GetMemoEx: TMemoEx; virtual;
         function FocusOnTextControl(AInfo: TFocusInfo): boolean;
         procedure ClearSelection;
         procedure ChangeFrame;
         procedure RepaintAll;
         function Next: TBlock;
         function Prev: TBlock;
         function CountErrWarn: TErrWarnCount; virtual;
         function LockDrawing: boolean;
         procedure UnLockDrawing;
         procedure LockDrawingComments;
         procedure UnLockDrawingComments;
         function GetFocusColor: TColor;
         function Remove(ANode: TTreeNodeWithFriend = nil): boolean; virtual;
         function CanRemove: boolean;
         function IsBoldDesc: boolean; virtual;
         function GetComments(AInFront: boolean = false): IEnumerable<TComment>;
         function GetPinComments: IEnumerable<TComment>;
         procedure SetVisible(AVisible: boolean; ASetComments: boolean = true); virtual;
         procedure BringAllToFront;
         function PinComments: integer;
         function UnPinComments: integer; virtual;
         procedure CloneComments(ASource: TBlock);
         procedure ImportCommentsFromXML(ATag: IXMLElement);
         procedure CloneFrom(ABlock: TBlock); virtual;
         function GetExportFileName: string; virtual;
         function ExportToXMLFile(const AFile: string): TError; virtual;
         procedure OnMouseLeave(AClearRed: boolean = true); virtual;
         function FindSelectedBlock: TBlock; virtual;
      published
         property Color;
         property OnMouseDown;
         property OnResize;
         property OnMouseMove;
   end;

   TGroupBlock = class(TBlock)    // block which can aggregate child blocks
      protected
         FBlockImportMode,
         FDrawingFlag: boolean;
         FMemoFolder: TMemoEx;
         FInitParms: TInitParms;
         FBranchList: TObjectList<TBranch>;
         FTrueLabel,
         FFalseLabel: string;
         FFixedBranches: integer;
         FDiamond: TDiamond;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AAlignment: TAlignment; AParserMode: TYYMode = yymUndefined);
         procedure MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
         procedure MyOnCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean); override;
         procedure SetWidth(AMinX: integer); virtual;
         procedure LinkAllBlocks;
         procedure LinkBlocks(ABranch: TBranch);
         procedure Paint; override;
         function ExtractBranchIndex(const AStr: string): integer;
         function GetDiamondTop: TPoint; virtual;
         procedure AfterRemovingBranch; virtual;
         function GetBlockParms: TBlockParms; override;
      public
         Branch: TBranch;     // primary branch to order child blocks
         Expanded: boolean;
         FFoldParms: TInitParms;
         property BlockImportMode: boolean read FBlockImportMode write FBlockImportMode;
         destructor Destroy; override;
         procedure ResizeHorz(AContinue: boolean); virtual;
         procedure ResizeVert(AContinue: boolean); virtual;
         function GenerateNestedCode(ALines: TStringList; ABranchInd, ADeep: integer; const ALangId: string): integer;
         procedure ExpandFold(AResize: boolean); virtual;
         function GetBranch(idx: integer): TBranch;
         function FindLastRow(AStart: integer; ALines: TStrings): integer; override;
         procedure ChangeColor(AColor: TColor); override;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; override;
         function AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchTextId: integer = ID_INVALID): TBranch; virtual;
         procedure ExpandAll;
         function HasFoldedBlocks: boolean;
         procedure PopulateComboBoxes; override;
         function GetMemoEx: TMemoEx; override;
         function CanInsertReturnBlock: boolean; override;
         function GetFromXML(ATag: IXMLElement): TError; override;
         procedure SaveInXML(ATag: IXMLElement); override;
         procedure GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer); override;
         function GetAllBlocks: IEnumerable<TBlock>;
         procedure ResizeWithDrawLock;
         function GetFoldedText: string;
         procedure SetFoldedText(const AText: string);
         function CountErrWarn: TErrWarnCount; override;
         procedure SetVisible(AVisible: boolean; ASetComments: boolean = true); override;
         function CanBeFocused: boolean; override;
         function UnPinComments: integer; override;
         procedure CloneFrom(ABlock: TBlock); override;
         procedure OnMouseLeave(AClearRed: boolean = true); override;
         function GetBranchIndexByControl(AControl: TControl): integer;
         function RemoveBranch(AIndex: integer): boolean;
         function Remove(ANode: TTreeNodeWithFriend = nil): boolean; override;
         function FindSelectedBlock: TBlock; override;
   end;

   TBranch = class(TList<TBlock>, IWithId)
      private
         FParentBlock: TGroupBlock;
         FRemovedBlockIdx: integer;
         FId: integer;
         procedure ResetRemovedBlockIdx;
         function GetHeight: integer;
         function GetId: integer;
         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;
         function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
      public
         Hook: TPoint;           // hook determines position of blocks within a branch in parent window coordinates
         Statement: TStatement;
         property ParentBlock: TGroupBlock read FParentBlock;
         property Height: integer read GetHeight;
         property Id: integer read GetId;
         constructor Create(AParentBlock: TGroupBlock; const AHook: TPoint; AId: integer = ID_INVALID);
         destructor Destroy; override;
         procedure InsertAfter(ANewBlock, ABlock: TBlock);
         function FindInstanceOf(AClass: TClass): integer;
         function Remove(ABlock: TBlock): integer;
         procedure UndoRemove(ABlock: TBlock);
         function GetMostRight: integer;
   end;

implementation

uses
   System.StrUtils, Vcl.Menus, System.Types, System.Math, System.Rtti, System.TypInfo,
   System.SysUtils, System.UITypes, Main_Block, Return_Block, Infrastructure, BlockFactory,
   UserFunction, XMLProcessor, Navigator_Form, LangDefinition, FlashThread, Main_Form, Constants;

type
   TControlHack = class(TControl);
   TCustomEditHack = class(TCustomEdit);

constructor TBlock.Create(ABranch: TBranch;
                          const ABlockParms: TBlockParms;
                          AShape: TColorShape;
                          AParserMode: TYYMode;
                          AAlignment: TAlignment);
begin

   FType := ABlockParms.bt;

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
      Parent := Page.Box;
   end;

   ParentFont  := true;
   ParentColor := true;
   Ctl3D       := false;
   Color       := Page.Box.Color;
   Font.Name   := GSettings.FlowchartFontName;
   PopupMenu   := Page.Form.pmPages;
   DoubleBuffered := GSettings.EnableDBuffering;
   ControlStyle := ControlStyle + [csOpaque];
   ParentBackground := false;
   Canvas.TextFlags := Canvas.TextFlags or ETO_OPAQUE;
   Canvas.Font.Assign(Font);
   SetBounds(ABlockParms.x, ABlockParms.y, ABlockParms.w, ABlockParms.h);

   Ired := -1;
   FId := GProject.Register(Self, ABlockParms.bid);
   FMouseLeave := true;
   FShape := AShape;
   FStatement := TStatement.Create(Self, AParserMode, AAlignment);
   FStatement.EditorAction := UpdateEditor;
   FStatement.Color := GSettings.GetShapeColor(FShape);

   OnMouseDown := MyOnMouseDown;
   OnMouseUp   := MyOnMouseUp;
   OnMouseMove := MyOnMouseMove;
   OnCanResize := MyOnCanResize;
   OnDblClick  := MyOnDblClick;
   OnDragOver  := MyOnDragOver;
   OnDragDrop  := MyOnDragDrop;
end;

constructor TGroupBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AAlignment: TAlignment; AParserMode: TYYMode = yymUndefined);
begin

   inherited Create(ABranch, ABlockParms, AShape, AParserMode, AAlignment);

   FStatement.Width := 65;

   FMemoFolder := TMemoEx.Create(Self);
   with FMemoFolder do
   begin
      Parent := Self;
      Visible := false;
      SetBounds(4, 4, 132, 55);
      DoubleBuffered := true;
      Color := GSettings.GetShapeColor(shpFolder);
      Font.Assign(FStatement.Font);
      OnMouseDown := Self.OnMouseDown;
      Font.Color := clNavy;
      OnChange := Self.OnChangeMemo;
   end;

   Expanded := true;

   FFixedBranches := 1;

   FFoldParms.Width := 140;
   FFoldParms.Height := 91;

   FTrueLabel := i18Manager.GetString('CaptionTrue');
   FFalseLabel := i18Manager.GetString('CaptionFalse');

   FBranchList := TObjectList<TBranch>.Create;
   FBranchList.Add(nil);

   Branch := AddBranch(ABlockParms.br);
end;

procedure TBlock.CloneFrom(ABlock: TBlock);
begin
   if ABlock <> nil then
   begin
      Visible := ABlock.Visible;
      SetFont(ABlock.Font);
      FFrame :=  ABlock.FFrame;
      var editSrc := ABlock.GetTextControl;
      var edit := GetTextControl;
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
   br, br2: TBranch;
   i: integer;
begin
   inherited CloneFrom(ABlock);
   if ABlock is TGroupBlock then
   begin
      grpBlock := TGroupBlock(ABlock);
      FMemoFolder.CloneFrom(grpBlock.FMemoFolder);
      if not grpBlock.Expanded then
      begin
         Expanded := false;
         FFoldParms := grpBlock.FFoldParms;
         Constraints.MinWidth := 140;
         Constraints.MinHeight := 54;
         Width := grpBlock.Width;
         Height := grpBlock.Height;
         FMemoFolder.SetBounds(4, 4, Width-8, Height-36);
         FMemoFolder.Anchors := [akRight, akLeft, akBottom, akTop];
         BottomHook := Width div 2;
         BottomPoint.X := BottomHook;
         BottomPoint.Y := Height - 28;
         IPoint.X := BottomHook + 30;
         IPoint.Y := FMemoFolder.Height + 15;
         TopHook.X := BottomHook;
         FMemoFolder.Visible := true;
      end
      else
      begin
         FFoldParms.Width := grpBlock.FFoldParms.Width;
         FFoldParms.Height := grpBlock.FFoldParms.Height;
      end;
      for i := PRIMARY_BRANCH_IDX to grpBlock.FBranchList.Count-1 do
      begin
         br := grpBlock.FBranchList[i];
         br2 := GetBranch(i);
         if br2 = nil then
            br2 := AddBranch(br.Hook);
         prevBlock := nil;
         for block in br do
         begin
            newBlock := block.Clone(br2);
            br2.InsertAfter(newBlock, prevBlock);
            prevBlock := br2.Last;
         end;
      end;
   end;
end;

destructor TBlock.Destroy;
begin
   for var comment in GetPinComments do
      comment.Free;
   if Self = GClpbrd.Instance then
      GClpbrd.Instance := nil;
   if Self = GClpbrd.UndoObject then
      GClpbrd.UndoObject := nil;
   GProject.UnRegister(Self);
   inherited Destroy;
end;

destructor TGroupBlock.Destroy;
begin
   Hide;
   Page.Box.SetScrollBars;
   FBranchList.Free;
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
   ExitSizeMove;
   var memo := GetMemoEx;
   if memo <> nil then
      memo.UpdateScrolls;
end;

procedure TBlock.ExitSizeMove;
begin
   if FHResize or FVResize then
   begin
      var lock := LockDrawing;
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
               TGroupBlock(Self).LinkAllBlocks;
            if FParentBlock <> nil then
               FParentBlock.ResizeVert(true);
            FVResize := false;
         end;
      finally
         if lock then
            UnLockDrawing;
      end;
      GProject.SetChanged;
      if FParentBlock = nil then
         BringAllToFront;
      NavigatorForm.Invalidate;
   end;
end;

procedure TBlock.CloneComments(ASource: TBlock);
begin
   if ASource <> nil then
   begin
      var lPage := Page;
      var unPin := ASource.PinComments > 0;
      try
         for var comment in ASource.GetPinComments do
         begin
            var newComment := comment.Clone(lPage);
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
begin
   var p := Point(X, Y);
   SelectBlock(p);
   SetCursor(p);
   if Rect(BottomPoint.X-5, BottomPoint.Y, BottomPoint.X+5, Height).Contains(p) then
   begin
      DrawArrow(BottomPoint, BottomPoint.X, Height-1, arrEnd, clRed);
      Ired := 0;
      Cursor := TCursor(GCustomCursor);
   end
   else if Ired = 0 then
   begin
      DrawArrow(BottomPoint, BottomPoint.X, Height-1);
      Ired := -1;
      Cursor := crDefault;
   end;
end;

function TBlock.GetBlockParms: TBlockParms;
begin
   result := TBlockParms.New(FType, Left, Top, Width, Height);
end;

function TGroupBlock.GetBlockParms: TBlockParms;
begin
   result := TBlockParms.New(FType, Left, Top, Width, Height, Branch.Hook.X, Branch.Hook.Y, BottomHook);
end;

procedure TGroupBlock.MyOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   if Expanded then
   begin
      for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
      begin
         var p := FBranchList[i].Hook;
         if Rect(p.X-5, TopHook.Y, p.X+5, p.Y).Contains(Point(X, Y)) then
         begin
            DrawArrow(p.X, TopHook.Y, p, arrEnd, clRed);
            Ired := i;
            Cursor := TCursor(GCustomCursor);
            break;
         end
         else if Ired = i then
         begin
            DrawArrow(p.X, TopHook.Y, p);
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

function TBlock.IsForeParent(AParent: TObject): boolean;
begin
   result := false;
   if AParent <> nil then
   begin
      var lParent := Parent;
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
   isShift := GetAsyncKeyState(vkShift) <> 0;
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
   srcPage: TBlockTabSheet;
   mForm: TMainForm;
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
      if GetAsyncKeyState(vkShift) <> 0 then
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
      mForm := Page.Form;
      try
         menuItem.OnClick(menuItem);
         mForm.pmPages.PopupComponent := Self;
         mForm.miPaste.OnClick(mForm.miPaste);
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

procedure TBlock.OnMouseLeave(AClearRed: boolean = true);
begin
   Cursor := crDefault;
   ClearSelection;
   if Ired = 0 then
      DrawArrow(BottomPoint, BottomPoint.X, Height-1);
   if AClearRed then
      Ired := -1;
   if FVResize or FHResize then
      SendMessage(Handle, WM_NCHITTEST, 0, 0);
end;

procedure TGroupBlock.OnMouseLeave(AClearRed: boolean = true);
begin
   var br := GetBranch(Ired);
   if br <> nil then
      DrawArrow(br.Hook.X, TopHook.Y, br.Hook);
   inherited OnMouseLeave(AClearRed);
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
         BottomPoint.Y := NewHeight - 28;
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

function TGroupBlock.GenerateNestedCode(ALines: TStringList; ABranchInd, ADeep: integer; const ALangId: string): integer;
begin
   result := 0;
   var br := GetBranch(ABranchInd);
   if br <> nil then
   begin
      for var block in br do
          result := result + block.GenerateCode(ALines, ALangId, ADeep);
   end;
end;

function TBlock.GetTextControl: TCustomEdit;
begin
   result := FStatement;
end;

function TBlock.IsInSelect(const APoint: TPoint): boolean;
begin
   result := Bounds(IPoint.X-5, IPoint.Y, 10, 10).Contains(APoint);
end;

procedure TBlock.MyOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then
   begin
      if IsInSelect(Point(X, Y)) then
         BeginDrag(false, 3)
      else if not IsCursorResize then
      begin          // drag entire flowchart
         ReleaseCapture;
         FTopParentBlock.BringAllToFront;
         SendMessage(FTopParentBlock.Handle, WM_SYSCOMMAND, $F012, 0);
         FTopParentBlock.OnResize(FTopParentBlock);
         if Ired >= 0 then
         begin
            var mForm := Page.Form;
            var menuItem: TMenuItem := nil;
            case GCustomCursor of
               crInstr:       menuItem := mForm.miInstr;
               crMultiInstr:  menuItem := mForm.miMultiInstr;
               crIfElse:      menuItem := mForm.miIfElse;
               crWhile:       menuItem := mForm.miWhile;
               crFor:         menuItem := mForm.miFor;
               crRepeat:      menuItem := mForm.miRepeat;
               crInput:       menuItem := mForm.miInput;
               crOutput:      menuItem := mForm.miOutput;
               crFuncCall:    menuItem := mForm.miRoutineCall;
               crIf:          menuItem := mForm.miIf;
               crCase:        menuItem := mForm.miCase;
               crFolder:      menuItem := mForm.miFolder;
               crText:        menuItem := mForm.miText;
               crReturn:
               begin
                  if CanInsertReturnBlock then
                     menuItem := mForm.miReturn;
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

function TBlock.Clone(ABranch: TBranch): TBlock;
begin
   result := nil;
   var ctx := TRttiContext.Create;
   var blockType := ctx.GetType(ClassInfo);
   for var method in blockType.GetMethods do
   begin
      var params := method.GetParameters;
      if method.IsConstructor
         and (method.Visibility = mvPublic)
         and (Length(params) = 2)
         and (params[0].ParamType = ctx.GetType(TBranch))
         and (params[1].ParamType = ctx.GetType(TypeInfo(TBlockParms))) then
      begin
         result := method.Invoke(blockType.AsInstance.MetaclassType, [TValue.From<TBranch>(ABranch), TValue.From<TBlockParms>(GetBlockParms)]).AsType<TBlock>;
         result.CloneFrom(Self);
         break;
      end;
   end;
   if result = nil then
      raise Exception.Create('public constructor (TBranch, TBlockParms) not found in class ' + ClassName);
end;

procedure TBlock.NCHitTest(var Msg: TWMNCHitTest);
begin
   inherited;
   if (GetAsyncKeyState(vkLButton) <> 0) and not Mouse.IsDragging then
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
   end;
end;

procedure TBlock.MyOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbRight then
   begin
      var p := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      FMouseLeave := false;
      PopupMenu.Popup(p.X, p.Y);
   end;
end;

procedure TBlock.SetFrame(AValue: boolean);
begin
   if FFrame <> AValue then
   begin
      FFrame := AValue;
      GProject.SetChanged;
      ClearSelection;
      Invalidate;
      if FFrame then
         TInfra.GetEditorForm.SelectCodeRange(Self)
      else
         TInfra.GetEditorForm.UnSelectCodeRange(Self);
   end;
end;

procedure TGroupBlock.ResizeWithDrawLock;
begin
   var lock := LockDrawing;
   try
      ResizeHorz(true);
      ResizeVert(true);
   finally
      if lock then
         UnlockDrawing;
   end;
end;

procedure TGroupBlock.ResizeVert(AContinue: boolean);
begin
   Height := Branch.Height + Branch.Hook.Y + FInitParms.HeightAffix;
   LinkAllBlocks;
   if AContinue and (FParentBlock <> nil) then
      FParentBlock.ResizeVert(AContinue);
end;

procedure TGroupBlock.ResizeHorz(AContinue: boolean);
begin

   Branch.Hook.X := FInitParms.BranchPoint.X;
   TopHook.X := Branch.Hook.X;
   LinkAllBlocks;

   if Branch.Count = 0 then   // case if primary branch is empty
   begin
      Width := FInitParms.Width;
      BottomHook := FInitParms.BottomHook;
      BottomPoint.X := FInitParms.BottomPoint.X;
   end
   else
   begin
      // resize in left direction
      var xLeft := 30;   // 30 - left margin
      for var block in Branch do
          xLeft := Min(xLeft, block.Left);

      Branch.Hook.X := Branch.Hook.X + 30 - xLeft;
      TopHook.X := Branch.Hook.X;
      LinkAllBlocks;
      BottomHook := Branch.Last.Left + Branch.Last.BottomPoint.X;

      // resize in right direction
      var xRight := 0;
      for var block in Branch do
          xRight := Max(xRight, block.BoundsRect.Right);

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

procedure TGroupBlock.SetWidth(AMinX: integer);
begin
{}
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
begin
   Font.Style := AStyle;
   Canvas.Font.Style := AStyle;
   for var i := 0 to ControlCount-1 do
   begin
      var control := Controls[i];
      if control is TBlock then
         TBlock(control).SetFontStyle(AStyle)
      else
         TControlHack(control).Font.Style := AStyle;
   end;
   Refresh;
end;

procedure TBlock.SetFontSize(ASize: integer);
begin
   Font.Size := ASize;
   Canvas.Font.Size := ASize;
   for var i := 0 to ControlCount-1 do
   begin
      var control := Controls[i];
      if control is TBlock then
         TBlock(control).SetFontSize(ASize)
      else
         TInfra.SetFontSize(control, ASize);
   end;
   PutTextControls;
   Refresh;
end;

function TBlock.GetFont: TFont;
begin
   result := Font;
end;

procedure TBlock.SetFont(AFont: TFont);
begin
   Font.Assign(AFont);
   Canvas.Font.Assign(AFont);
   Refresh;
   for var i := 0 to ControlCount-1 do
   begin
      var control := Controls[i];
      if control is TBlock then
      begin
         TBlock(control).SetFontStyle(AFont.Style);
         TBlock(control).SetFontSize(AFont.Size);
      end
      else
      begin
         TControlHack(control).Font.Style := AFont.Style;
         TInfra.SetFontSize(control, AFont.Size);
      end;
   end;
   PutTextControls;
end;

function TBlock.GetComments(AInFront: boolean = false): IEnumerable<TComment>;
begin
   var list := TList<TComment>.Create;
   if Visible then
   begin
      var lPage := Page;
      for var comment in GProject.GetComments do
      begin
         if comment.Page = lPage then
         begin
            var isFront := true;
            if AInFront then
               isFront := IsInFront(comment);
            if isFront and (comment.PinControl = nil) and ClientRect.Contains(ParentToClient(comment.BoundsRect.TopLeft, lPage.Box)) then
               list.Add(comment);
         end
      end;
   end;
   result := TEnumeratorFactory<TComment>.Create(list);
end;

procedure TBlock.BringAllToFront;
begin
   BringToFront;
   for var comment in GetComments do
      comment.BringToFront;
end;

function TBlock.IsInFront(AControl: TWinControl): boolean;
begin
   result := false;
   if AControl <> nil then
   begin
      var hnd := GetWindow(AControl.Handle, GW_HWNDLAST);
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
begin
   if (x <> 0) and (y <> 0) and (Left <> 0) and (Top <> 0) and ((x <> Left) or (y <> Top)) then
   begin
      for var comment in GetComments(true) do
      begin
         if comment.Visible then
            TInfra.MoveWinTopZ(comment, comment.Left+x-Left, comment.Top+y-Top);
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

function TBlock.GetPinComments: IEnumerable<TComment>;
begin
   var list := TList<TComment>.Create;
   for var comment in GProject.GetComments do
   begin
      if comment.PinControl = Self then
         list.Add(comment);
   end;
   result := TEnumeratorFactory<TComment>.Create(list);
end;

procedure TBlock.PutTextControls;
begin
end;

function TGroupBlock.GetDiamondTop: TPoint;
begin
   result := TPoint.Zero;
end;

procedure TBlock.PerformRefreshStatements;
begin
    var b := NavigatorForm.InvalidateIndicator;
    NavigatorForm.InvalidateIndicator := false;
    RefreshStatements;
    NavigatorForm.InvalidateIndicator := b;
end;

procedure TBlock.RefreshStatements;
begin
    var b1 := FRefreshMode;
    FRefreshMode := true;
    try
       for var i := 0 to ControlCount-1 do
       begin
          var control := Controls[i];
          if control is TCustomEdit then
             TCustomEditHack(control).Change
          else if (control is TBlock) and (control <> GClpbrd.UndoObject) then
             TBlock(control).RefreshStatements;
       end;
    finally
       FRefreshMode := b1;
    end;
end;

function TBlock.GetId: integer;
begin
   result := FId;
end;

procedure TBlock.ChangeColor(AColor: TColor);
begin
   Color := AColor;
   for var comment in GetComments do
   begin
      if comment.Visible then
         comment.Color := AColor;
   end;
   var edit := TCustomEditHack(GetTextControl);
   if edit <> nil then
   begin
      var lColor := GSettings.GetShapeColor(FShape);
      if lColor = GSettings.DesktopColor then
         edit.Color := AColor
      else
         edit.Color := lColor;
   end;
end;

procedure TBlock.ClearSelection;
begin
   var lColor := Page.Box.Color;
   if Color <> lColor then
      ChangeColor(lColor);
   NavigatorForm.Invalidate;
end;

function TBlock.FindSelectedBlock: TBlock;
begin
   result := nil;
   if Color = GSettings.HighlightColor then
      result := Self;
end;

function TGroupBlock.FindSelectedBlock: TBlock;
begin
   result := inherited FindSelectedBlock;
   if result = nil then
   begin
      for var block in GetAllBlocks do
      begin
         result := block.FindSelectedBlock;
         if result <> nil then
            break;
      end;
   end;
end;

procedure TGroupBlock.ChangeColor(AColor: TColor);
begin
   inherited ChangeColor(AColor);
   if Expanded then
   begin
      for var block in GetAllBlocks do
         block.ChangeColor(AColor);
   end;
   var lColor := GSettings.GetShapeColor(shpFolder);
   if lColor = GSettings.DesktopColor then
      FMemoFolder.Color := AColor
   else
      FMemoFolder.Color := lColor;
end;

procedure TBlock.SelectBlock(const APoint: TPoint);
begin
   if IsInSelect(APoint) then
   begin
      if Color <> GSettings.HighlightColor then
      begin
         ChangeColor(GSettings.HighlightColor);
         if GSettings.EditorAutoSelectBlock then
            TInfra.GetEditorForm.SelectCodeRange(Self);
         NavigatorForm.Invalidate;
      end;
   end
   else if Color <> Page.Box.Color then
   begin
      ChangeColor(Page.Box.Color);
      if GSettings.EditorAutoSelectBlock and not FFrame then
         TInfra.GetEditorForm.UnSelectCodeRange(Self);
      NavigatorForm.Invalidate;
   end;
end;

procedure TGroupBlock.ExpandAll;
begin
   if not Expanded then
      ExpandFold(true);
   for var block in GetAllBlocks do
   begin
      if block is TGroupBlock then
         TGroupBlock(block).ExpandAll;
   end;
end;

procedure TBlock.RepaintAll;
begin
   Repaint;
   for var i := 0 to ControlCount-1 do
   begin
      var control :=  Controls[i];
      if control is TBlock then
         TBlock(control).RepaintAll
      else
         control.Repaint;
   end;
end;

function TGroupBlock.HasFoldedBlocks: boolean;
begin
   result := not Expanded;
   if Expanded then
   begin
      for var block in GetAllBlocks do
      begin
         if block is TGroupBlock then
         begin
            result := TGroupBlock(block).HasFoldedBlocks;
            if result then
               break;
         end;
      end;
   end;
end;

procedure TBlock.Paint;
begin
   inherited;
   var r := ClientRect;
   with Canvas do
   begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Color;
      Pen.Style := psClear;
      Pen.Color := GSettings.PenColor;
      if FFrame then
         Pen.Style := psDashDot
      else
         r.Inflate(0, 0, 1, 1);
      Rectangle(r);
      Pen.Style := psSolid;
      Font.Color := Pen.Color;
   end;
end;

procedure TBlock.SetBrushColor(AShape: TColorShape);
begin
   Canvas.Brush.Style := bsClear;
   var lColor := GSettings.GetShapeColor(AShape);
   if lColor <> GSettings.DesktopColor then
      Canvas.Brush.Color := lColor;
end;

procedure TBlock.DrawI;
begin
   if Page.DrawI then
   begin
      Canvas.PenPos := IPoint;
      Canvas.LineTo(IPoint.X, IPoint.Y+10);
   end;
end;

procedure TBlock.DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = false; downJust: boolean = false);
begin
   if GSettings.ShowBlockLabels and not AText.IsEmpty then
   begin
      var f := Canvas.Font;
      var fontName := f.Name;
      var fontStyles := f.Style;
      var fontSize := f.Size;
      f.Name := GInfra.CurrentLang.LabelFontName;
      f.Style := [fsBold];
      f.Size := GInfra.CurrentLang.LabelFontSize;
      DrawTextLabel(x, y, AText, rightJust, downJust);
      f.Name := fontName;
      f.Style := fontStyles;
      f.Size := fontSize;
   end;
end;

function TBlock.DrawTextLabel(x, y: integer; const AText: string; ARightJust: boolean = false; ADownJust: boolean = false; APrint: boolean = true): TRect;
begin
   var te := TSize.Create(0, 0);
   if not AText.IsEmpty then
   begin
      var fontStyles := Canvas.Font.Style;
      var fontColor := Canvas.Font.Color;
      Canvas.Font.Style := [];
      Canvas.Font.Color := GSettings.PenColor;
      if fsBold in fontStyles then
         Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      var brushStyle := Canvas.Brush.Style;
      Canvas.Brush.Style := bsClear;
      te := Canvas.TextExtent(AText);
      if ARightJust then
         x := Max(x-te.Width, 0);
      if ADownJust then
         y := Max(y-te.Height, 0);
      if APrint then
         Canvas.TextOut(x, y, AText);
      Canvas.Brush.Style := brushStyle;
      Canvas.Font.Style := fontStyles;
      Canvas.Font.Color := fontColor;
   end;
   result := TRect.Create(Point(x, y), te.Width, te.Height);
end;

procedure TBlock.DrawArrow(const aFrom, aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(aFrom.X, aFrom.Y, aTo.X, aTo.Y, AArrowPos, AColor);
end;

procedure TBlock.DrawArrow(fromX, fromY: integer; const aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(fromX, fromY, aTo.X, aTo.Y, AArrowPos, AColor);
end;

procedure TBlock.DrawArrow(const aFrom: TPoint; toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(aFrom.X, aFrom.Y, toX, toY, AArrowPos, AColor);
end;

// this method draw arrow line from current pen position
procedure TBlock.DrawArrowTo(toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(Canvas.PenPos, toX, toY, AArrowPos, AColor);
end;

// this method draw arrow line from current pen position
procedure TBlock.DrawArrowTo(const aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(Canvas.PenPos, aTo, AArrowPos, AColor);
end;

procedure TBlock.DrawArrow(fromX, fromY, toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
const
   MX: array[boolean, boolean] of integer = ((10, -10), (-5, -5));
   MY: array[boolean, boolean] of integer = ((5, 5), (10, -10));
   MD: array[boolean, boolean] of integer = ((0, -10), (10, 0));
var
   isVert, toBottomRight: boolean;
   aX, aY: integer;
   p: TPoint;
begin
   if AColor = clNone then
      AColor := GSettings.PenColor;
   aX := toX;
   aY := toY;
   isVert := fromX = toX;
   if AArrowPos = arrMiddle then
   begin
      if isVert then
         Inc(aY, (fromY-toY) div 2)
      else
         Inc(aX, (fromX-toX) div 2);
   end;
   if isVert then
      toBottomRight := toY > fromY
   else
      toBottomRight := toX > fromX;
   p := Point(aX+MX[isVert, toBottomRight], aY+MY[isVert, toBottomRight]);
   Canvas.Brush.Style := bsSolid;
   Canvas.Pen.Color := AColor;
   Canvas.Brush.Color := AColor;
   Canvas.Polygon([p,
                   Point(p.X+MD[isVert, false], p.Y+MD[isVert, true]),
                   Point(aX, aY),
                   p]);
   Canvas.MoveTo(fromX, fromY);
   Canvas.LineTo(toX, toY);
end;

function TBlock.GetEllipseTextRect(ax, ay: integer; const AText: string): TRect;
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
   result := Rect(ax-b, ay-2*a, ax+b, ay);
end;

function TBlock.DrawEllipsedText(ax, ay: integer; const AText: string): TRect;
begin
   result := GetEllipseTextRect(ax, ay, AText);
   SetBrushColor(shpEllipse);
   Canvas.Ellipse(result);
   DrawText(Canvas.Handle, PChar(AText), -1, result, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
end;

function TBlock.GetMemoEx: TMemoEx;
begin
   result := nil;
end;

function TGroupBlock.GetMemoEx: TMemoEx;
begin
   result := FMemoFolder;
end;

function TBlock.CountErrWarn: TErrWarnCount;
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   var textControl := GetTextControl;
   if textControl <> nil then
   begin
      if TControlHack(textControl).Font.Color = NOK_COLOR then
         result.ErrorCount := 1
      else if TControlHack(textControl).Font.Color = WARN_COLOR then
         result.WarningCount := 1;
   end;
end;

function TGroupBlock.CountErrWarn: TErrWarnCount;
begin
   result := inherited CountErrWarn;
   for var block in GetAllBlocks do
   begin
      var errWarnCount := block.CountErrWarn;
      Inc(result.ErrorCount, errWarnCount.ErrorCount);
      Inc(result.WarningCount, errWarnCount.WarningCount);
   end;
end;

procedure TGroupBlock.Paint;
begin
   inherited;
   var brushStyle := Canvas.Brush.Style;
   var lColor := Canvas.Brush.Color;
   var w := Canvas.Pen.Width;
   if Expanded then
   begin
      var edit := GetTextControl;
      if (edit <> nil) and (FShape = shpDiamond) then
      begin
         FDiamond := TDiamond.New(GetDiamondTop, edit);
         TInfra.MoveWin(edit, FDiamond.Top.X - edit.Width div 2,
                              FDiamond.Top.Y - edit.Height div 2 + FDiamond.Height div 2);
         SetBrushColor(shpDiamond);
         Canvas.Polygon(FDiamond.Polygon);
      end;
   end
   else
   begin
      SetBrushColor(shpFolder);
      Canvas.Pen.Width := 2;
      var r := FMemoFolder.BoundsRect;
      r.Inflate(3, 3, 4, 4);
      Canvas.Rectangle(r);
      Canvas.Pen.Width := 1;
      if FTopParentBlock <> Self then
         DrawArrow(BottomPoint, BottomPoint.X, Height-1);
      r.Inflate(-2, -2, -3, -3);
      Canvas.Rectangle(r);
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
      FTopParentBlock.LockDrawingComments;
   end;
end;

procedure TBlock.UnLockDrawing;
begin
   if FTopParentBlock.FDrawingFlag then
   begin
      SendMessage(FTopParentBlock.Handle, WM_SETREDRAW, WPARAM(True), 0);
      FTopParentBlock.UnLockDrawingComments;
      GProject.RepaintFlowcharts;
      GProject.RepaintComments;
      RedrawWindow(Page.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE);
      FTopParentBlock.FDrawingFlag := false;
   end;
end;

procedure TBlock.LockDrawingComments;
begin
   for var comment in GetComments(true) do
   begin
      if comment.Visible then
         SendMessage(comment.Handle, WM_SETREDRAW, WPARAM(False), 0);
   end;
end;

procedure TBlock.UnLockDrawingComments;
begin
   for var comment in GetComments(true) do
   begin
      if comment.Visible then
         SendMessage(comment.Handle, WM_SETREDRAW, WPARAM(True), 0);
   end;
end;

function TBlock.CanInsertReturnBlock: boolean;
begin
   result := (Ired = 0) and (FParentBranch <> nil) and (FParentBranch.Count > 0) and (FParentBranch.Last = Self);
end;

function TGroupBlock.CanInsertReturnBlock: boolean;
begin
   if Ired = 0 then
      result := (FParentBranch <> nil) and (FParentBranch.Count > 0) and (FParentBranch.Last = Self)
   else
   begin
      var br := GetBranch(Ired);
      result := (br <> nil) and (br.Count = 0);
   end;
end;

procedure TBlock.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
   inherited;
   var box := Page.Box;
   var p := ClientToParent(TPoint.Zero, box);
   if FHResize then
      Msg.MinMaxInfo.ptMaxTrackSize.X := box.ClientWidth - p.X;
   if FVResize then
      Msg.MinMaxInfo.ptMaxTrackSize.Y := box.ClientHeight - p.Y;
end;

function TBlock.IsCursorSelect: boolean;
begin
   result := IsInSelect(ScreenToClient(Mouse.CursorPos));
end;

function TBlock.IsCursorResize: boolean;
begin
   result := -Cursor in [-crSizeWE, -crSizeNS, -crSizeNWSE];
end;

procedure TBlock.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
   Msg.Result := 1;
end;

function TGroupBlock.GetBranch(idx: integer): TBranch;
begin
   result := nil;
   if (idx >= PRIMARY_BRANCH_IDX) and (idx < FBranchList.Count) then
      result := FBranchList[idx];
end;

function TBlock.FindLastRow(AStart: integer; ALines: TStrings): integer;
begin
   result := TInfra.FindLastRow(Self, AStart, ALines);
end;

function TGroupBlock.FindLastRow(AStart: integer; ALines: TStrings): integer;
begin
   result := inherited FindLastRow(AStart, ALines);
   for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
   begin
      var br := FBranchList[i];
      if br.Count > 0 then
         result := Max(result, br.Last.FindLastRow(AStart, ALines));
   end;
end;

procedure TBlock.MyOnChange(Sender: TObject);
begin
   if NavigatorForm.InvalidateIndicator then
      NavigatorForm.Invalidate;
end;

procedure TBlock.OnChangeMemo(Sender: TObject);
begin
   var memo := GetMemoEx;
   if memo <> nil then
      memo.UpdateScrolls;
   if NavigatorForm.InvalidateIndicator then
      NavigatorForm.Invalidate;
end;

function TBlock.CanBeFocused: boolean;
begin
   result := true;
   var lParent := FParentBlock;
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
      var func := TUserFunction(TMainBlock(FTopParentBlock).UserFunction);
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
begin
   if AInfo.FocusEdit = nil then
      AInfo.FocusEdit := GetTextControl;
   var lPage := Page;
   AInfo.FocusEditForm := lPage.Form;
   lPage.PageControl.ActivePage := lPage;
   result := FocusOnTextControl(AInfo);
end;

function TBlock.FocusOnTextControl(AInfo: TFocusInfo): boolean;
var
   idx, idx2, i: integer;
   txt: string;
   memo: TCustomMemo;
   box: TScrollBoxEx;
begin
   result := false;
   if ContainsControl(AInfo.FocusEdit) and AInfo.FocusEdit.CanFocus then
   begin
      box := Page.Box;
      box.Show;
      FTopParentBlock.BringAllToFront;
      box.ScrollInView(AInfo.FocusEdit);
      idx2 := 0;
      if AInfo.FocusEdit is TCustomMemo then
      begin
         memo := TCustomMemo(AInfo.FocusEdit);
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
begin
   result := '';
   if (AEdit <> nil) and TInfra.IsNOkColor(TControlHack(AEdit).Font.Color) then
   begin
      result := AEdit.Hint;
      var i := LastDelimiter(sLineBreak, result);
      if i > 0 then
         result := ' - ' + Copy(result, i+1);
   end;
end;

function TBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
begin
   result := AParentNode;
   var textControl := GetTextControl;
   if textControl <> nil then
   begin
      var nodeText := GetTreeNodeText;
      result := AParentNode.Owner.AddChildObject(AParentNode, nodeText, textControl);
      if TInfra.IsNOkColor(TControlHack(textControl).Font.Color) then
      begin
         AParentNode.MakeVisible;
         AParentNode.Expand(false);
      end;
   end;
end;

function TGroupBlock.GenerateTree(AParentNode: TTreeNode): TTreeNode;
begin
   result := inherited GenerateTree(AParentNode);
   for var block in FBranchList[PRIMARY_BRANCH_IDX] do
       block.GenerateTree(result);
end;

function TBlock.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := '';
   var textControl := GetTextControl;
   if textControl <> nil then
   begin
      var errMsg := GetErrorMsg(textControl);
      var descTemplate := GetDescTemplate(GInfra.CurrentLang.Name);
      result := FillTemplate(GInfra.CurrentLang.Name, descTemplate) + errMsg;
   end;
end;

function TGroupBlock.GetBranchIndexByControl(AControl: TControl): integer;
begin
   result := BRANCH_IDX_NOT_FOUND;
   if FBranchList = nil then
      Exit;
   for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
   begin
      if FBranchList[i].Statement = AControl then
      begin
         result := i;
         break;
      end;
   end;
end;

function TGroupBlock.AddBranch(const AHook: TPoint; ABranchId: integer = ID_INVALID; ABranchTextId: integer = ID_INVALID): TBranch;
begin
   result := TBranch.Create(Self, AHook, ABranchId);
   FBranchList.Add(result);
end;

function TBlock.CanRemove: boolean;
begin
   result := Visible;
end;

function TBlock.GetUndoObject: TObject;
begin
   result := Self;
end;

function TBlock.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := CanRemove;
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

function TGroupBlock.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := CanRemove;
   if result then
   begin
      if ANode <> nil then
         result := RemoveBranch(GetBranchIndexByControl(ANode.Data))
      else
         result := false;
      if not result then
         result := inherited Remove(ANode);
   end;
end;

function TBlock.IsBoldDesc: boolean;
begin
   result := false;
end;

function TBlock.PinComments: integer;
begin
   result := 0;
   var lPage := Page;
   var p := ClientToParent(TPoint.Zero, lPage.Box);
   for var comment in GetComments do
   begin
      comment.Visible := false;
      TInfra.MoveWin(comment, comment.Left - p.X, comment.Top - p.Y);
      comment.PinControl := Self;
      comment.Parent := lPage;
      Inc(result);
   end;
end;

function TBlock.UnPinComments: integer;
begin
   result := 0;
   var box := Page.Box;
   var p := ClientToParent(TPoint.Zero, box);
   for var comment in GetPinComments do
   begin
      TInfra.MoveWin(comment, comment.Left + p.X, comment.Top + p.Y);
      comment.Parent := box;
      comment.Visible := true;
      comment.PinControl := nil;
      comment.BringToFront;
      Inc(result);
   end;
end;

function TGroupBlock.UnPinComments: integer;
begin
   result := 0;
   if Expanded then
      result := inherited UnPinComments;
end;

procedure TBlock.SetVisible(AVisible: boolean; ASetComments: boolean = true);
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

function TBlock.ExportToXMLFile(const AFile: string): TError;
begin
   result := TXMLProcessor.ExportToXMLFile(ExportToXMLTag, AFile);
end;

procedure TGroupBlock.SetVisible(AVisible: boolean; ASetComments: boolean = true);
begin
   inherited SetVisible(AVisible, Expanded);
end;

function TBlock.ProcessComments: boolean;
begin
   result := (FParentBlock = nil) or not FParentBlock.BlockImportMode;
end;

function TGroupBlock.RemoveBranch(AIndex: integer): boolean;
begin
   result := false;
   var br := GetBranch(AIndex);
   if (br <> nil) and (AIndex > FFixedBranches) then
   begin
      var obj: TObject := nil;
      if (GClpbrd.UndoObject is TBlock) and (TBlock(GClpbrd.UndoObject).ParentBranch = br) then
         obj := GClpbrd.UndoObject;
      if FBranchList.Remove(br) <> -1 then
      begin
         obj.Free;
         AfterRemovingBranch;
         result := true;
      end;
   end;
end;

procedure TGroupBlock.AfterRemovingBranch;
begin
   ResizeWithDrawLock;
   NavigatorForm.Invalidate;
end;

procedure TGroupBlock.ExpandFold(AResize: boolean);
var
   tmpWidth, tmpHeight, i: integer;
   block: TBlock;
   textControl: TCustomEdit;
begin
   GProject.SetChanged;
   Expanded := not Expanded;
   textControl := GetTextControl;
   if textControl <> nil then
      textControl.Visible := Expanded;
   FMemoFolder.Visible := not Expanded;

   for block in GetAllBlocks do
      block.Visible := Expanded;

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
      PerformRefreshStatements;
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
      FMemoFolder.SetBounds(4, 4, Width-8, Height-36);
      FMemoFolder.Anchors := [akRight, akLeft, akBottom, akTop];
      BottomHook := Width div 2;
      BottomPoint.X := BottomHook;
      BottomPoint.Y := Height - 28;
      IPoint.X := BottomHook + 30;
      IPoint.Y := FMemoFolder.Height + 15;
      TopHook.X := BottomHook;
   end;

   if AResize and (FParentBlock <> nil) then
   begin
      FParentBlock.ResizeWithDrawLock;
      NavigatorForm.Invalidate;
   end;
   
   UnPinComments;
end;

procedure TGroupBlock.SaveInXML(ATag: IXMLElement);
var
   brx, fw, fh, i: integer;
   txt: string;
   tag1, tag2: IXMLElement;
   br: TBranch;
   unPin: boolean;
   comment: TComment;
   block: TBlock;
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

         for comment in GetPinComments do
            comment.ExportToXMLTag2(ATag);

         for i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
         begin
            br := FBranchList[i];

            tag2 := ATag.OwnerDocument.CreateElement(BRANCH_TAG);
            ATag.AppendChild(tag2);

            tag2.SetAttribute(ID_ATTR, br.Id.ToString);

            if br.Statement <> nil then
               tag2.SetAttribute(BRANCH_STMNT_ATTR, br.Statement.Id.ToString);

            tag1 := ATag.OwnerDocument.CreateElement('x');
            TXMLProcessor.AddText(tag1, br.hook.X.ToString);
            tag2.AppendChild(tag1);

            tag1 := ATag.OwnerDocument.CreateElement('y');
            TXMLProcessor.AddText(tag1, br.hook.Y.ToString);
            tag2.AppendChild(tag1);

            for block in br do
                TXMLProcessor.ExportBlockToXML(block, tag2);
         end;
      finally
         if unPin then
            UnPinComments;
      end;
   end;
end;

procedure TBlock.SaveInXML(ATag: IXMLElement);
begin
   SaveInXML2(ATag);
   if (ATag <> nil) and (PinComments > 0) then
   begin
      for var comment in GetPinComments do
         comment.ExportToXMLTag2(ATag);
      UnPinComments;
   end;
end;

procedure TBlock.SaveInXML2(ATag: IXMLElement);
var
   txtControl: TCustomEdit;
   txt: string;
   tag: IXMLElement;
   memo: TMemoEx;
begin
   if ATag <> nil then
   begin
      ATag.SetAttribute(BLOCK_TYPE_ATTR, TRttiEnumerationType.GetName(BType));
      ATag.SetAttribute(FRAME_ATTR, FFrame.ToString);
      ATag.SetAttribute('x', Left.ToString);
      ATag.SetAttribute('y', Top.ToString);
      ATag.SetAttribute('h', Height.ToString);
      ATag.SetAttribute('w', Width.ToString);
      ATag.SetAttribute('bh', BottomHook.ToString);
      ATag.SetAttribute('brx', BottomPoint.X.ToString);
      ATag.SetAttribute(ID_ATTR, FId.ToString);
      memo := GetMemoEx;
      if memo <> nil then
         memo.SaveInXML(ATag);
      ATag.SetAttribute(FONT_SIZE_ATTR, Font.Size.ToString);
      ATag.SetAttribute(FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style).ToString);
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

procedure TBlock.ImportCommentsFromXML(ATag: IXMLElement);
begin
   if ProcessComments then
   begin
      var tag := TXMLProcessor.FindChildTag(ATag, COMMENT_ATTR);
      while tag <> nil do
      begin
         var comment := TComment.CreateDefault(Page);
         comment.ImportFromXMLTag(tag, Self);
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      UnPinComments;
   end;
end;

function TBlock.GetFromXML(ATag: IXMLElement): TError;
var
   tag: IXMLElement;
   textControl: TCustomEdit;
   i: integer;
   memo: TMemoEx;
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

      i := TXMLProcessor.GetIntFromAttr(ATag, FONT_SIZE_ATTR);
      if i in FLOWCHART_VALID_FONT_SIZES then
         SetFontSize(i);

      i := TXMLProcessor.GetIntFromAttr(ATag, FONT_STYLE_ATTR);
      SetFontStyle(TInfra.DecodeFontStyle(i));

      Frame := TXMLProcessor.GetBoolFromAttr(ATag, FRAME_ATTR);

      memo := GetMemoEx;
      if memo <> nil then
         memo.GetFromXML(ATag);

      ImportCommentsFromXML(ATag);
   end;
end;

function TGroupBlock.GetFromXML(ATag: IXMLElement): TError;
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
         idx := PRIMARY_BRANCH_IDX;
         repeat
            tag2 := TXMLProcessor.FindChildTag(tag1, 'x');
            if tag2 <> nil then
               hx := StrToIntDef(tag2.Text, 0);
            tag2 := TXMLProcessor.FindChildTag(tag1, 'y');
            if tag2 <> nil then
               hy := StrToIntDef(tag2.Text, 0);
            bId := TXMLProcessor.GetIntFromAttr(tag1, ID_ATTR, ID_INVALID);
            bStmntId := TXMLProcessor.GetIntFromAttr(tag1, BRANCH_STMNT_ATTR, ID_INVALID);
            if GetBranch(idx) = nil then
               AddBranch(Point(hx, hy), bId, bStmntId);
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
      FFoldParms.Width := TXMLProcessor.GetIntFromAttr(ATag, 'fw', 140);
      FFoldParms.Height := TXMLProcessor.GetIntFromAttr(ATag, 'fh', 91);
      if TXMLProcessor.GetBoolFromAttr(ATag, FOLDED_ATTR) then
         ExpandFold(false);
   end;
end;

procedure TBlock.ExportToXMLTag(ATag: IXMLElement);
begin
   TXMLProcessor.ExportBlockToXML(Self, ATag);
   var block := Next;
   while (block <> nil) and block.Frame do
   begin
      TXMLProcessor.ExportBlockToXML(block, ATag);
      block := block.Next;
   end;
end;

function TBlock.ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   block, newBlock: TBlock;
   lParent: TGroupBlock;
   tag: IXMLElement;
   bt: TBlockType;
begin
   result := errValidate;
   tag := TXMLProcessor.FindChildTag(ATag, BLOCK_TAG);
   if tag <> nil then
      bt := TRttiEnumerationType.GetValue<TBlockType>(tag.GetAttribute(BLOCK_TYPE_ATTR));
   if (tag = nil) or (bt in [blMain, blUnknown, blComment]) then
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
begin
   var edit := GetTextControl;
   if (edit <> nil) and edit.HasParent then
      result := TControlHack(edit).Font.Color
   else
      result := OK_COLOR;
end;

procedure TBlock.UpdateEditor(AEdit: TCustomEdit);
begin
   if (AEdit <> nil) and PerformEditorUpdate then
   begin
      var chLine := TInfra.GetChangeLine(Self, AEdit);
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, Trim(AEdit.Text));
         chLine.Text := TInfra.StripInstrEnd(chLine.Text);
         if GSettings.UpdateEditor and not SkipUpdateEditor then
            TInfra.ChangeLine(chLine);
         TInfra.GetEditorForm.SetCaretPos(chLine);
      end;
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

function TBlock.GetBlockTemplate(const ALangId: string): string;
begin
   result := '';
   var lang := GInfra.GetLangDefinition(ALangId);
   if lang <> nil then
      result := lang.GetBlockTemplate(FType);
end;

function TBlock.GetBlockTemplateExpr(const ALangId: string): string;
begin
   result := '';
   var template := GetBlockTemplate(ALangId);
   if template.IsEmpty then
      template := PRIMARY_PLACEHOLDER;
   var templateLines := TStringList.Create;
   try
      templateLines.Text := template;
      for var line in templateLines do
      begin
         if line.Contains(PRIMARY_PLACEHOLDER) then
         begin
            result := line;
            break;
         end;
      end;
   finally
      templateLines.Free;
   end;
end;

function TBlock.FindTemplate(const ALangId: string; const ATemplate: string): string;
begin
   result := '';
   if not ATemplate.IsEmpty then
      result := ATemplate
   else if not GetBlockTemplate(ALangId).IsEmpty then
      result := GetBlockTemplateExpr(ALangId);
end;

function TBlock.FillTemplate(const ALangId: string; const ATemplate: string = ''): string;
begin
   result := FillCodedTemplate(ALangId);
   var template := FindTemplate(ALangId, ATemplate);
   if not template.IsEmpty then
      result := ReplaceStr(template, PRIMARY_PLACEHOLDER, result);
end;

function TBlock.FillCodedTemplate(const ALangId: string): string;
begin
   result := '';
   var textControl := GetTextControl;
   if textControl <> nil then
      result := Trim(textControl.Text);
end;

procedure TBlock.ExportToGraphic(AGraphic: TGraphic);
begin
   ClearSelection;
   var bitmap: TBitmap := nil;
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   bitmap.Width := Width + 2;
   bitmap.Height := Height + 2;
   var lPage := Page;
   lPage.DrawI := false;
   bitmap.Canvas.Lock;
   try
      PaintTo(bitmap.Canvas.Handle, 1, 1);
      for var comment in GetComments do
      begin
         var pnt := ParentToClient(comment.BoundsRect.TopLeft, lPage.Box);
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
begin
   inherited PopulateComboBoxes;
   for var block in GetAllBlocks do
      block.PopulateComboBoxes;
end;

function TGroupBlock.GetAllBlocks: IEnumerable<TBlock>;
begin
   var list := TList<TBlock>.Create;
   for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
   begin
      for var block in FBranchList[i] do
          list.Add(block);
   end;
   result := TEnumeratorFactory<TBlock>.Create(list);
end;

procedure TGroupBlock.LinkBlocks(ABranch: TBranch);
var
   p: TPoint;
begin
   if ABranch <> nil then
   begin
      var blockPrev: TBlock := nil;
      for var block in ABranch do
      begin
         if blockPrev <> nil then
            p := Point(blockPrev.BottomPoint.X+blockPrev.Left-block.TopHook.X, blockPrev.BoundsRect.Bottom)
         else
            p := Point(ABranch.Hook.X-block.TopHook.X, ABranch.Hook.Y+1);
         TInfra.MoveWin(block, p);
         blockPrev := block;
      end;
   end;
end;

procedure TGroupBlock.LinkAllBlocks;
begin
   for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
      LinkBlocks(FBranchList[i]);
end;

function TBlock.SkipUpdateEditor: boolean;
begin
   var funcHeader := TInfra.GetFunctionHeader(Self);
   result := (funcHeader <> nil) and (TInfra.IsNOkColor(funcHeader.Font.Color) or (funcHeader.chkExternal.Checked and not GInfra.CurrentLang.CodeIncludeExternFunction));
end;

function TBlock.GenerateCode(ALines: TStringList; const ALangId: string; ADeep: integer; AFromLine: integer = LAST_LINE): integer;
begin
   if fsStrikeOut in Font.Style then
      Exit(0);
   var tmpList := TStringList.Create;
   try
      GenerateDefaultTemplate(tmpList, ALangId, ADeep);
      TInfra.InsertLinesIntoList(ALines, tmpList, AFromLine);
      result := tmpList.Count;
   finally
      tmpList.Free;
   end;
end;

procedure TBlock.GenerateDefaultTemplate(ALines: TStringList; const ALangId: string; ADeep: integer);
begin
   var txt := '';
   var textControl := GetTextControl;
   if textControl is TCustomMemo then
      txt := textControl.Text
   else if textControl <> nil then
      txt := Trim(textControl.Text);
   var template := GetBlockTemplate(ALangId);
   if template.IsEmpty then
      template := PRIMARY_PLACEHOLDER;
   template := ReplaceStr(template, PRIMARY_PLACEHOLDER, txt);
   GenerateTemplateSection(ALines, template, ALangId, ADeep);
end;

function TGroupBlock.ExtractBranchIndex(const AStr: string): integer;
begin
   result := Pos('%b', AStr);
   if result <> 0 then
   begin
      var val := '';
      var b := 0;
      for var i := result+2 to AStr.Length do
      begin
         if TryStrToInt(AStr[i], b) then
            val := val + AStr[i]
         else
            break;
      end;
      result := StrToIntDef(val, 0);
      if result >= FBranchList.Count then
         result := 0;
   end;
end;

procedure TBlock.GenerateTemplateSection(ALines: TStringList; const ATemplate: string; const ALangId: string; ADeep: integer);
begin
   var lines := TStringList.Create;
   try
      lines.Text := ATemplate;
      GenerateTemplateSection(ALines, lines, ALangId, ADeep);
   finally
      lines.Free;
   end;
end;

procedure TBlock.GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer);
begin
   var c := ALines.Count + ATemplate.Count;
   if ALines.Capacity < c then
      ALines.Capacity := c;
   for var i := 0 to ATemplate.Count-1 do
   begin
      var obj := ATemplate.Objects[i];
      if obj = nil then
         obj := Self;
      ALines.AddObject(GSettings.IndentString(ADeep) + TInfra.StripInstrEnd(TInfra.ReplaceXMLIndents(ATemplate[i])), obj);
   end;
end;

procedure TGroupBlock.GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer);

   function CountLeadXMLIndents(const AString: string): integer;
   begin
      result := 0;
      for var a := 1 to AString.Length do
      begin
         if AString[a] = INDENT_XML_CHAR then
            result := result + 1
         else
            break;
      end;
   end;

begin
   for var i := 0 to ATemplate.Count-1 do
   begin
      var templateLine := ATemplate[i];
      var b := ExtractBranchIndex(templateLine);
      if b > 0 then
      begin
         if (ALines.Count > 0) and (ALines.Objects[ALines.Count-1] = nil) then
            ALines.Objects[ALines.Count-1] := FBranchList[b];
         GenerateNestedCode(ALines, b, ADeep + CountLeadXMLIndents(templateLine), ALangId);
      end
      else
      begin
         var obj := ATemplate.Objects[i];
         if obj = nil then
            obj := Self;
         ALines.AddObject(GSettings.IndentString(ADeep) + TInfra.ReplaceXMLIndents(templateLine), obj);
      end;
   end;
end;

function TBlock.Next: TBlock;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      var idx := FParentBranch.IndexOf(Self);
      if (idx <> -1) and (FParentBranch.Last <> Self) then
         result := FParentBranch.Items[idx+1];
   end;
end;

function TBlock.Prev: TBlock;
begin
   result := nil;
   if FParentBranch <> nil then
   begin
      var idx := FParentBranch.IndexOf(Self);
      if idx > 0 then
         result := FParentBranch.Items[idx-1];
   end;
end;

constructor TBranch.Create(AParentBlock: TGroupBlock; const AHook: TPoint; AId: integer = ID_INVALID);
begin
   inherited Create;
   FParentBlock := AParentBlock;
   Hook := AHook;
   ResetRemovedBlockIdx;
   FId := GProject.Register(Self, AId);
end;

destructor TBranch.Destroy;
begin
   Statement.Free;
   for var i := 0 to Count-1 do
      Items[i].Free;
   GProject.UnRegister(Self);
   inherited Destroy;
end;

procedure TBranch.ResetRemovedBlockIdx;
begin
   FRemovedBlockIdx := -1;	// it must be negative value
end;

function TBranch.GetMostRight: integer;
begin
   result := Hook.X;
   for var i := 0 to Count-1 do
   begin
      var br := Items[i].BoundsRect.Right;
      if br > result then
         result := br;
   end;
end;

procedure TBranch.InsertAfter(ANewBlock, ABlock: TBlock);
begin
   Insert(IndexOf(ABlock)+1, ANewBlock);
end;

function TBranch.GetHeight: integer;
begin
   result := 0;
   for var i := 0 to Count-1 do
      Inc(result, Items[i].Height);
end;

function TBranch.FindInstanceOf(AClass: TClass): integer;
begin
   result := -1;
   for var i := 0 to Count-1 do
   begin
      if Items[i].ClassType = AClass then
      begin
         result := i;
         break;
      end;
   end;
end;

procedure TBranch.UndoRemove(ABlock: TBlock);
begin
   if (ABlock <> nil) and (Self = ABlock.ParentBranch) and (FRemovedBlockIdx >= 0) then
   begin
      Insert(FRemovedBlockIdx, ABlock);
      ResetRemovedBlockIdx;
   end;
end;

function TBranch.Remove(ABlock: TBlock): integer;
begin
   result := inherited Remove(ABlock);
   FRemovedBlockIdx := result;
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
