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
         FRedArrow: integer;                // indicates active arrow; -1: none, 0: bottom, 1: branch1, 2: branch2...
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AParserMode: TYYMode; AAlignment: TAlignment);
         procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
         procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
         procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
         procedure DragDrop(Source: TObject; X, Y: Integer); override;
         procedure MyOnChange(Sender: TObject);
         procedure DblClick; override;
         procedure WMMouseLeave(var Msg: TMessage); message WM_MOUSELEAVE;
         procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
         procedure NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
         procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
         procedure WMExitSizeMove(var Msg: TWMMove); message WM_EXITSIZEMOVE;
         procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
         procedure Paint; override;
         procedure DrawI;
         function DrawTextLabel(x, y: integer; const AText: string; ARightJust: boolean = False; ADownJust: boolean = False; APrint: boolean = True): TRect;
         procedure DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = False; downJust: boolean = False);
         function GetId: integer;
         function ShouldFocusEditor: boolean;
         procedure Select;
         function IsAtSelectPos(const APoint: TPoint): boolean;
         procedure SetCursor(const APoint: TPoint);
         procedure SetFrame(AValue: boolean);
         procedure PutTextControls; virtual;
         procedure DrawArrowTo(toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(const aFrom, aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
         procedure DrawArrow(fromX, fromY, toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone); overload;
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
         function IsAncestor(AParent: TObject): boolean;
         function GetErrorMsg(AEdit: TCustomEdit): string;
         procedure SaveInXML2(ANode: IXMLNode);
         procedure ExitSizeMove;
         procedure SetBrushColor(AShape: TColorShape);
         function GetBlockParms: TBlockParms; virtual;
         function GetBlockTemplate(const ALangId: string): string;
         function GetBlockTemplateExpr(const ALangId: string): string;
         function FindTemplate(const ALangId, ATemplate: string): string;
      public
         BottomPoint: TPoint;    // points to arrow at the bottom of the block
         IPoint: TPoint;          // points to I mark
         BottomHook: integer;
         TopHook: TPoint;
         property RedArrow: integer read FRedArrow;
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
         function GetFromXML(ANode: IXMLNode): TError; virtual;
         procedure SaveInXML(ANode: IXMLNode); virtual;
         function FillTemplate(const ALangId, ATemplate: string): string; virtual;
         function FillCodedTemplate(const ALangId: string): string; virtual;
         function GetDescTemplate(const ALangId: string): string; virtual;
         function GetTextControl: TCustomEdit; virtual;
         function GenerateTree(AParentNode: TTreeNode): TTreeNode; virtual;
         function IsMouseAtSelectPos: boolean;
         function IsCursorResize: boolean;
         function CanInsertReturnBlock: boolean; virtual;
         procedure ExportToXML(ANode: IXMLNode);
         function ImportFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
         procedure ExportToGraphic(AGraphic: TGraphic); virtual;
         procedure UpdateEditor(AEdit: TCustomEdit); virtual;
         procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
         function RetrieveFocus(AInfo: TFocusInfo): boolean; virtual;
         function CanBeFocused: boolean; virtual;
         function GetTreeNodeText(ANodeOffset: integer = 0): string; virtual;
         function FindLastRow(AStart: integer; ALines: TStrings): integer; virtual;
         procedure GenerateDefaultTemplate(ALines: TStringList; const ALangId: string; ADeep: integer);
         procedure GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer); overload; virtual;
         procedure GenerateTemplateSection(ALines: TStringList; const ATemplate: string; const ALangId: string; ADeep: integer); overload;
         function GetMemoEx: TMemoEx; virtual;
         function FocusOnTextControl(AInfo: TFocusInfo): boolean;
         procedure DeSelect;
         procedure ChangeFrame;
         procedure RepaintAll;
         function Next: TBlock;
         function Prev: TBlock;
         function CountErrWarn: TErrWarnCount; virtual;
         function GetFocusColor: TColor;
         function Remove(ANode: TTreeNodeWithFriend = nil): boolean; virtual;
         function CanRemove: boolean;
         function IsBoldDesc: boolean; virtual;
         function GetComments(AInFront: boolean = False): IEnumerable<TComment>;
         function GetPinComments: IEnumerable<TComment>;
         procedure SetVisible(AVisible: boolean; ASetComments: boolean = True); virtual;
         procedure BringAllToFront;
         function PinComments: integer;
         procedure UnPinComments; virtual;
         procedure CloneComments(ASource: TBlock);
         procedure ImportCommentsFromXML(ANode: IXMLNode);
         procedure CloneFrom(ABlock: TBlock); virtual;
         function GetExportFileName: string; virtual;
         function ExportToXMLFile(const AFile: string): TError; virtual;
         procedure OnMouseLeave(AClearRedArrow: boolean = True); virtual;
         procedure LockDrawing;
         procedure UnlockDrawing;
         function FindSelectedBlock: TBlock; virtual;
         function ShouldUpdateEditor: boolean;
   end;

   TGroupBlock = class(TBlock)    // block which can aggregate child blocks
      protected
         FBlockImportMode: boolean;
         FMemoFolder: TMemoEx;
         FInitParms: TInitParms;
         FBranchList: TObjectList<TBranch>;
         FTrueLabel,
         FFalseLabel: string;
         FFixedBranches: integer;
         FDiamond: TDiamond;
         constructor Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AAlignment: TAlignment; AParserMode: TYYMode = yymUndefined);
         procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
         function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
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
         function GetFromXML(ANode: IXMLNode): TError; override;
         procedure SaveInXML(ANode: IXMLNode); override;
         procedure GenerateTemplateSection(ALines: TStringList; ATemplate: TStringList; const ALangId: string; ADeep: integer); override;
         function GetAllBlocks: IEnumerable<TBlock>;
         procedure ResizeWithDrawLock;
         function GetFoldedText: string;
         procedure SetFoldedText(const AText: string);
         function CountErrWarn: TErrWarnCount; override;
         procedure SetVisible(AVisible: boolean; ASetComments: boolean = True); override;
         function CanBeFocused: boolean; override;
         procedure UnPinComments; override;
         procedure CloneFrom(ABlock: TBlock); override;
         procedure OnMouseLeave(AClearRedArrow: boolean = True); override;
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
   UserFunction, XMLProcessor, Navigator_Form, LangDefinition, FlashThread, Main_Form,
   OmniXMLUtils, Constants;

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

   ParentFont  := True;
   ParentColor := True;
   Ctl3D       := False;
   Color       := Page.Box.Color;
   Font.Name   := GSettings.FlowchartFontName;
   PopupMenu   := Page.Form.pmPages;
   DoubleBuffered := GSettings.EnableDBuffering;
   ControlStyle := ControlStyle + [csOpaque];
   ParentBackground := False;
   Canvas.TextFlags := Canvas.TextFlags or ETO_OPAQUE;
   Canvas.Font.Assign(Font);
   SetBounds(ABlockParms.x, ABlockParms.y, ABlockParms.w, ABlockParms.h);

   FRedArrow := -1;
   FId := GProject.Register(Self, ABlockParms.bid);
   FMouseLeave := True;
   FShape := AShape;
   FStatement := TStatement.Create(Self, AParserMode, AAlignment);
   FStatement.EditorAction := UpdateEditor;
   FStatement.Color := GSettings.GetShapeColor(FShape);
end;

constructor TGroupBlock.Create(ABranch: TBranch; const ABlockParms: TBlockParms; AShape: TColorShape; AAlignment: TAlignment; AParserMode: TYYMode = yymUndefined);
begin

   inherited Create(ABranch, ABlockParms, AShape, AParserMode, AAlignment);

   FStatement.Width := 65;

   FMemoFolder := TMemoEx.Create(Self);
   FMemoFolder.Parent := Self;
   FMemoFolder.Visible := False;
   FMemoFolder.SetBounds(4, 4, 132, 55);
   FMemoFolder.DoubleBuffered := True;
   FMemoFolder.Color := GSettings.GetShapeColor(shpFolder);
   FMemoFolder.Font.Assign(FStatement.Font);
   FMemoFolder.Font.Color := clNavy;

   Expanded := True;

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
         Expanded := False;
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
         FMemoFolder.Visible := True;
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
      FTopParentBlock.LockDrawing;
      try
         if FHResize then
         begin
            if FParentBlock <> nil then
               FParentBlock.ResizeHorz(True);
            FHResize := False;
         end;
         if FVResize then
         begin
            if Self is TGroupBlock then
               TGroupBlock(Self).LinkAllBlocks;
            if FParentBlock <> nil then
               FParentBlock.ResizeVert(True);
            FVResize := False;
         end;
      finally
            FTopParentBlock.UnLockDrawing;
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

procedure TBlock.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   var p := Point(X, Y);
   if IsAtSelectPos(p) then
      Select
   else
      DeSelect;
   SetCursor(p);

   if Rect(BottomPoint.X-5, BottomPoint.Y, BottomPoint.X+5, Height).Contains(p) then
   begin
      DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1), arrEnd, clRed);
      FRedArrow := 0;
      Cursor := TCursor(GCustomCursor);
   end
   else if FRedArrow = 0 then
   begin
      DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
      FRedArrow := -1;
      Cursor := crDefault;
   end;
end;

procedure TGroupBlock.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   if Expanded then
   begin
      for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
      begin
         var p := FBranchList[i].Hook;
         if Rect(p.X-5, TopHook.Y, p.X+5, p.Y).Contains(Point(X, Y)) then
         begin
            DrawArrow(Point(p.X, TopHook.Y), p, arrEnd, clRed);
            FRedArrow := i;
            Cursor := TCursor(GCustomCursor);
            break;
         end
         else if FRedArrow = i then
         begin
            DrawArrow(Point(p.X, TopHook.Y), p);
            FRedArrow := -1;
            Cursor := crDefault;
            break;
         end;
      end;
   end;
   inherited;
end;

function TBlock.GetBlockParms: TBlockParms;
begin
   result := TBlockParms.New(FType, Left, Top, Width, Height);
end;

function TGroupBlock.GetBlockParms: TBlockParms;
begin
   result := TBlockParms.New(FType, Left, Top, Width, Height, Branch.Hook.X, Branch.Hook.Y, BottomHook);
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

function TBlock.IsAncestor(AParent: TObject): boolean;
begin
   result := False;
   if AParent <> nil then
   begin
      var lParent := Parent;
      while lParent is TBlock do
      begin
         if lParent = AParent then
         begin
            result := True;
            break;
         end;
         lParent := lParent.Parent;
      end;
   end;
end;

procedure TBlock.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   var shiftState: TShiftState := [];
   var isShift := GetAsyncKeyState(vkShift) <> 0;
   if isShift then
      shiftState := [ssShift];
   MouseMove(shiftState, X, Y);
   if (FRedArrow < 0) or (not (Source is TBlock)) or (Source is TMainBlock) or (Source is TReturnBlock) or ((not isShift) and ((Source = Self) or IsAncestor(Source))) then
      Accept := False;
end;

procedure TBlock.DragDrop(Source: TObject; X, Y: Integer);
var
   srcPage: TBlockTabSheet;
   mForm: TMainForm;
   menuItem: TMenuItem;
   inst: TControl;
   uobj: TObject;
   shiftPressed: boolean;
begin
   if Source is TBlock then
   begin
      srcPage := TBlock(Source).Page;
      srcPage.Form.pmPages.PopupComponent := TBlock(Source);
      shiftPressed := GetAsyncKeyState(vkShift) <> 0;
      if shiftPressed then
         menuItem := srcPage.Form.miCopy
      else
      begin
         menuItem := srcPage.Form.miCut;
         TBlock(Source).TopParentBlock.LockDrawing;
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
         if not shiftPressed then
            TBlock(Source).TopParentBlock.UnLockDrawing;
      end;
   end;
end;

procedure TBlock.WMMouseLeave(var Msg: TMessage);
begin
   inherited;
   if FMouseLeave then
      OnMouseLeave
   else
      FMouseLeave := True;
end;

procedure TBlock.OnMouseLeave(AClearRedArrow: boolean = True);
begin
   Cursor := crDefault;
   DeSelect;
   if FRedArrow = 0 then
      DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
   if AClearRedArrow then
      FRedArrow := -1;
   if FVResize or FHResize then
      SendMessage(Handle, WM_NCHITTEST, 0, 0);
end;

procedure TGroupBlock.OnMouseLeave(AClearRedArrow: boolean = True);
begin
   var br := GetBranch(FRedArrow);
   if br <> nil then
      DrawArrow(Point(br.Hook.X, TopHook.Y), br.Hook);
   inherited OnMouseLeave(AClearRedArrow);
end;

function TBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if FHResize and result then
   begin
      BottomPoint.X := NewWidth div 2;
      TopHook.X := BottomPoint.X;
      IPoint.X := BottomPoint.X + 30;
   end;
end;

function TGroupBlock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
   result := (NewWidth >= Constraints.MinWidth) and (NewHeight >= Constraints.MinHeight);
   if FHResize and result then
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
   if FVResize and result then
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

procedure TBlock.DblClick;
begin
   inherited;
   if IsMouseAtSelectPos then
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

function TBlock.IsAtSelectPos(const APoint: TPoint): boolean;
begin
   result := Bounds(IPoint.X-5, IPoint.Y, 10, 10).Contains(APoint);
end;

procedure TBlock.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   inherited;
   if Button = mbLeft then
   begin
      if IsAtSelectPos(Point(X, Y)) then
         BeginDrag(False, 3)
      else if not IsCursorResize then
      begin
         var menuItem: TMenuItem := nil;
         if FRedArrow >= 0 then
         begin
            var mForm := Page.Form;
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
         end;
         if menuItem <> nil then
         begin
            PopupMenu.PopupComponent := Self;
            menuItem.OnClick(menuItem);
         end
         else          // drag entire flowchart
         begin
            var br := FTopParentBlock.BoundsRect;
            var b := br.Bottom;
            var r := br.Right;
            ReleaseCapture;
            FTopParentBlock.BringAllToFront;
            SendMessage(FTopParentBlock.Handle, WM_SYSCOMMAND, $F012, 0);
            br := FTopParentBlock.BoundsRect;
            if (b <> br.Bottom) or (r <> br.Right) then
               FTopParentBlock.Resize
            else
               FTopParentBlock.BringAllToFront;
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
      FMouseLeave := False;
      case Cursor of
         crSizeWE:
         begin
            Msg.Result := HTRIGHT;
            FHResize := True;
            BringToFront;
         end;
         crSizeNS:
         begin
            Msg.Result := HTBOTTOM;
            FVResize := True;
            BringToFront;
         end;
         crSizeNWSE:
         begin
            Msg.Result := HTBOTTOMRIGHT;
            FHResize := True;
            FVResize := True;
            BringToFront;
         end;
      end;
   end;
end;

procedure TBlock.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbRight then
   begin
      var p := ClientToScreen(Point(X, Y));
      PopupMenu.PopupComponent := Self;
      FMouseLeave := False;
      PopupMenu.Popup(p.X, p.Y);
   end;
end;

procedure TBlock.SetFrame(AValue: boolean);
begin
   if FFrame <> AValue then
   begin
      FFrame := AValue;
      GProject.SetChanged;
      DeSelect;
      Invalidate;
      if FFrame then
         TInfra.GetEditorForm.SelectCodeRange(Self)
      else
         TInfra.GetEditorForm.UnSelectCodeRange(Self);
   end;
end;

procedure TGroupBlock.ResizeWithDrawLock;
begin
   FTopParentBlock.LockDrawing;
   try
      ResizeHorz(True);
      ResizeVert(True);
   finally
      FTopParentBlock.UnlockDrawing;
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

function TBlock.GetComments(AInFront: boolean = False): IEnumerable<TComment>;
begin
   var list := TList<TComment>.Create;
   if Visible then
   begin
      for var comment in GProject.GetComments do
      begin
         if comment.Page = Page then
         begin
            var isFront := True;
            if AInFront then
               isFront := IsInFront(comment);
            if isFront and (comment.PinControl = nil) and ClientRect.Contains(ParentToClient(comment.BoundsRect.TopLeft, Page.Box)) then
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
   result := False;
   if AControl <> nil then
   begin
      var hnd := GetWindow(AControl.Handle, GW_HWNDLAST);
      while hnd <> 0 do
      begin
         if hnd = FTopParentBlock.Handle then
         begin
            result := True;
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
      for var comment in GetComments(True) do
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
    NavigatorForm.InvalidateIndicator := False;
    RefreshStatements;
    NavigatorForm.InvalidateIndicator := b;
end;

procedure TBlock.RefreshStatements;
begin
    var b1 := FRefreshMode;
    FRefreshMode := True;
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

function TBlock.FindSelectedBlock: TBlock;
begin
   result := nil;
   if Color = GSettings.SelectColor then
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

procedure TBlock.Select;
begin
   if Color <> GSettings.SelectColor then
   begin
      ChangeColor(GSettings.SelectColor);
      if GSettings.EditorAutoSelectBlock then
         TInfra.GetEditorForm.SelectCodeRange(Self);
      NavigatorForm.Invalidate;
   end;
end;

procedure TBlock.DeSelect;
begin
   if Color = GSettings.SelectColor then
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
      ExpandFold(True);
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
   Canvas.Brush.Style := bsSolid;
   Canvas.Brush.Color := Color;
   Canvas.Pen.Style := psClear;
   Canvas.Pen.Color := GSettings.PenColor;
   if FFrame then
      Canvas.Pen.Style := psDashDot
   else
      r.Inflate(0, 0, 1, 1);
   Canvas.Rectangle(r);
   Canvas.Pen.Style := psSolid;
   Canvas.Font.Color := Canvas.Pen.Color;
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

procedure TBlock.DrawBlockLabel(x, y: integer; const AText: string; rightJust: boolean = False; downJust: boolean = False);
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

function TBlock.DrawTextLabel(x, y: integer; const AText: string; ARightJust: boolean = False; ADownJust: boolean = False; APrint: boolean = True): TRect;
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

// this method draw arrow line from current pen position
procedure TBlock.DrawArrowTo(toX, toY: integer; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(Canvas.PenPos, Point(toX, toY), AArrowPos, AColor);
end;

procedure TBlock.DrawArrow(const aFrom, aTo: TPoint; AArrowPos: TArrowPosition = arrEnd; AColor: TColor = clNone);
begin
   DrawArrow(aFrom.X, aFrom.Y, aTo.X, aTo.Y, AArrowPos, AColor);
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
                   Point(p.X+MD[isVert, False], p.Y+MD[isVert, True]),
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
   cx := viewPort.cx;
   cy := viewPort.cy;
   if wndExt.cx <> 0 then
      cx := cx / wndExt.cx;
   if wndExt.cy <> 0 then
      cy := cy / wndExt.cy;
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
         DrawArrow(BottomPoint, Point(BottomPoint.X, Height-1));
      r.Inflate(-2, -2, -3, -3);
      Canvas.Rectangle(r);
   end;
   Canvas.Brush.Style := brushStyle;
   Canvas.Brush.Color := lColor;
   Canvas.Pen.Width := w;
end;

function TBlock.CanInsertReturnBlock: boolean;
begin
   result := (FRedArrow = 0) and (FParentBranch <> nil) and (FParentBranch.Count > 0) and (FParentBranch.Last = Self);
end;

function TGroupBlock.CanInsertReturnBlock: boolean;
begin
   if FRedArrow = 0 then
      result := (FParentBranch <> nil) and (FParentBranch.Count > 0) and (FParentBranch.Last = Self)
   else
   begin
      var br := GetBranch(FRedArrow);
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

function TBlock.IsMouseAtSelectPos: boolean;
begin
   result := IsAtSelectPos(ScreenToClient(Mouse.CursorPos));
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

function TBlock.CanBeFocused: boolean;
begin
   result := True;
   var lParent := FParentBlock;
   while lParent <> nil do
   begin
      if not lParent.Expanded then
      begin
         result := False;
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
         result := False;
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
   result := False;
   if ContainsControl(AInfo.FocusEdit) and AInfo.FocusEdit.CanFocus then
   begin
      box := Page.Box;
      box.Show;
      FTopParentBlock.BringAllToFront;
      box.ScrollInView(AInfo.FocusEdit);
      box.Repaint;
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
      result := True;
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
         AParentNode.Expand(False);
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
      DeSelect;
      SetVisible(False);
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
         result := False;
      if not result then
         result := inherited Remove(ANode);
   end;
end;

function TBlock.IsBoldDesc: boolean;
begin
   result := False;
end;

function TBlock.PinComments: integer;
begin
   result := 0;
   var p := ClientToParent(TPoint.Zero, Page.Box);
   for var comment in GetComments do
   begin
      comment.Visible := False;
      TInfra.MoveWin(comment, comment.Left - p.X, comment.Top - p.Y);
      comment.PinControl := Self;
      comment.Parent := Page;
      Inc(result);
   end;
end;

procedure TBlock.UnPinComments;
begin
   var p := ClientToParent(TPoint.Zero, Page.Box);
   for var comment in GetPinComments do
   begin
      TInfra.MoveWin(comment, comment.Left + p.X, comment.Top + p.Y);
      comment.Parent := Page.Box;
      comment.Visible := True;
      comment.PinControl := nil;
      comment.BringToFront;
   end;
end;

procedure TGroupBlock.UnPinComments;
begin
   if Expanded then
      inherited;
end;

procedure TBlock.SetVisible(AVisible: boolean; ASetComments: boolean = True);
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
   result := TXMLProcessor.ExportToXMLFile(ExportToXML, AFile);
end;

procedure TGroupBlock.SetVisible(AVisible: boolean; ASetComments: boolean = True);
begin
   inherited SetVisible(AVisible, Expanded);
end;

function TBlock.ProcessComments: boolean;
begin
   result := (FParentBlock = nil) or not FParentBlock.BlockImportMode;
end;

function TGroupBlock.RemoveBranch(AIndex: integer): boolean;
begin
   result := False;
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
         result := True;
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
   tmpWidth, tmpHeight: integer;
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

procedure TGroupBlock.SaveInXML(ANode: IXMLNode);

   function IsAlreadyExported(AComment: TComment; AExportedComments: IXMLNodeList): boolean;
   begin
      result := False;
      AExportedComments.Reset;
      var exportedComment := AExportedComments.NextNode;
      while exportedComment <> nil do
      begin
         var zOrder := GetNodeAttrInt(exportedComment, Z_ORDER_ATTR);
         var pageCaption := GetNodeAttrStr(exportedComment, PAGE_CAPTION_ATTR, '');
         if (zOrder = AComment.GetZOrder) and
            ((pageCaption = AComment.Page.Caption) or (pageCaption.IsEmpty and AComment.Page.IsMain)) then
         begin
            result := True;
            break;
         end;
         exportedComment := AExportedComments.NextNode;
      end;
   end;

begin
   SaveInXML2(ANode);
   if ANode <> nil then
   begin
      var unPin := False;
      if Expanded then
         unPin := PinComments > 0
      else
      begin
         SetNodeAttrInt(ANode, 'h', FFoldParms.Height);
         SetNodeAttrInt(ANode, 'w', FFoldParms.Width);
         SetNodeAttrInt(ANode, 'bh', FFoldParms.BottomHook);
      end;

      try
         SetNodeAttrInt(ANode, 'bry', Branch.Hook.Y);
         SetNodeAttrInt(ANode, 'brx', IfThen(Expanded, Branch.Hook.X, FFoldParms.BranchPoint.X));
         SetNodeAttrInt(ANode, 'fw', IfThen(Expanded, FFoldParms.Width, Width));
         SetNodeAttrInt(ANode, 'fh', IfThen(Expanded, FFoldParms.Height, Height));
         SetNodeAttrBool(ANode, FOLDED_ATTR, not Expanded);

         var txt := GetFoldedText;
         if not txt.IsEmpty then
            SetNodeCData(ANode, FOLD_TEXT_TAG, txt);

         var exportedComments := ANode.OwnerDocument.GetElementsByTagName(COMMENT_TAG);
         for var comment in GetPinComments do
         begin
            if not IsAlreadyExported(comment, exportedComments) then
               comment.ExportToXML2(ANode);
         end;

         for var i := PRIMARY_BRANCH_IDX to FBranchList.Count-1 do
         begin
            var br := FBranchList[i];
            var node := AppendNode(ANode, BRANCH_TAG);
            SetNodeAttrInt(node, ID_ATTR, br.Id);

            if br.Statement <> nil then
               SetNodeAttrInt(node, BRANCH_STMNT_ATTR, br.Statement.Id);

            SetNodeTextInt(node, 'x', br.hook.X);
            SetNodeTextInt(node, 'y', br.hook.Y);

            for var block in br do
               block.SaveInXml(AppendNode(node, BLOCK_TAG));
         end;
      finally
         if unPin then
            UnPinComments;
      end;
   end;
end;

procedure TBlock.LockDrawing;
begin
   TWinControl(Self).LockDrawing;
   for var comment in GetComments(True) do
      comment.LockDrawing;
end;

procedure TBlock.UnlockDrawing;
begin
   TWinControl(Self).UnlockDrawing;
   for var comment in GetComments(True) do
      comment.UnlockDrawing;
   if not IsDrawingLocked then
      GProject.RepaintFlowcharts;
end;

procedure TBlock.SaveInXML(ANode: IXMLNode);
begin
   SaveInXML2(ANode);
   if (ANode <> nil) and (PinComments > 0) then
   begin
      for var comment in GetPinComments do
         comment.ExportToXML2(ANode);
      UnPinComments;
   end;
end;

procedure TBlock.SaveInXML2(ANode: IXMLNode);
begin
   if ANode <> nil then
   begin
      SetNodeAttrStr(ANode, BLOCK_TYPE_ATTR, TRttiEnumerationType.GetName(BType));
      SetNodeAttrBool(ANode, FRAME_ATTR, FFrame);
      SetNodeAttrInt(ANode, 'x', Left);
      SetNodeAttrInt(ANode, 'y', Top);
      SetNodeAttrInt(ANode, 'h', Height);
      SetNodeAttrInt(ANode, 'w', Width);
      SetNodeAttrInt(ANode, 'bh', BottomHook);
      SetNodeAttrInt(ANode, 'brx', BottomPoint.X);
      SetNodeAttrInt(ANode, ID_ATTR, FId);
      SetNodeAttrInt(ANode, FONT_SIZE_ATTR, Font.Size);
      SetNodeAttrInt(ANode, FONT_STYLE_ATTR, TInfra.EncodeFontStyle(Font.Style));
      var memo := GetMemoEx;
      if memo <> nil then
         memo.SaveInXML(ANode);
      var txtControl := GetTextControl;
      if (txtControl <> nil) and (txtControl.Text <> '') then
         SetNodeCData(ANode, TEXT_TAG, ReplaceStr(txtControl.Text, sLineBreak, LB_PHOLDER));
   end;
end;

procedure TBlock.ImportCommentsFromXML(ANode: IXMLNode);
begin
   if ProcessComments then
   begin
      var commentNodes := FilterNodes(ANode, COMMENT_TAG);
      var commentNode := commentNodes.NextNode;
      while commentNode <> nil do
      begin
         var comment := TComment.CreateDefault(Page);
         comment.ImportFromXML(commentNode, Self);
         commentNode := commentNodes.NextNode;
      end;
      UnPinComments;
   end;
end;

function TBlock.GetFromXML(ANode: IXMLNode): TError;
begin
   result := errNone;
   if ANode <> nil then
   begin
      var node := FindNode(ANode, TEXT_TAG);
      var textControl := GetTextControl;
      if (node <> nil) and (textControl <> nil) then
      begin
         FRefreshMode := True;
         textControl.Text := ReplaceStr(node.Text, LB_PHOLDER, sLineBreak);
         FRefreshMode := False;
      end;

      var i := GetNodeAttrInt(ANode, FONT_SIZE_ATTR);
      if i in FLOWCHART_VALID_FONT_SIZES then
         SetFontSize(i);

      i := GetNodeAttrInt(ANode, FONT_STYLE_ATTR);
      SetFontStyle(TInfra.DecodeFontStyle(i));

      Frame := GetNodeAttrBool(ANode, FRAME_ATTR);

      var memo := GetMemoEx;
      if memo <> nil then
         memo.GetFromXML(ANode);

      ImportCommentsFromXML(ANode);
   end;
end;

function TGroupBlock.GetFromXML(ANode: IXMLNode): TError;
begin
   result := inherited GetFromXML(ANode);
   if ANode <> nil then
   begin
      var idx := PRIMARY_BRANCH_IDX;
      var branchNodes := FilterNodes(ANode, BRANCH_TAG);
      var branchNode := branchNodes.NextNode;
      while branchNode <> nil do
      begin
         var hx := 0;
         var hy := 0;
         var node := FindNode(branchNode, 'x');
         if node <> nil then
            hx := StrToIntDef(node.Text, 0);
         node := FindNode(branchNode, 'y');
         if node <> nil then
            hy := StrToIntDef(node.Text, 0);
         var bId := GetNodeAttrInt(branchNode, ID_ATTR);
         var bStmntId := GetNodeAttrInt(branchNode, BRANCH_STMNT_ATTR, ID_INVALID);
         if GetBranch(idx) = nil then
            AddBranch(Point(hx, hy), bId, bStmntId);
         node := FindNode(branchNode, BLOCK_TAG);
         if node <> nil then
         begin
            TXMLProcessor.ImportFlowchartFromXML(node, Self, nil, idx, result);
            if result <> errNone then break;
         end;
         Inc(idx);
         branchNode := branchNodes.NextNode;
      end;
      var tnode := FindNode(ANode, FOLD_TEXT_TAG);
      if tnode <> nil then
         SetFoldedText(tnode.Text);
      FFoldParms.Width := GetNodeAttrInt(ANode, 'fw');
      FFoldParms.Height := GetNodeAttrInt(ANode, 'fh');
      if GetNodeAttrBool(ANode, FOLDED_ATTR) then
         ExpandFold(False);
   end;
end;

procedure TBlock.ExportToXML(ANode: IXMLNode);
begin
   SaveInXml(AppendNode(ANode, BLOCK_TAG));
   var block := Next;
   while (block <> nil) and block.Frame do
   begin
      block.SaveInXml(AppendNode(ANode, BLOCK_TAG));
      block := block.Next;
   end;
end;

function TBlock.ImportFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
begin
   result := errValidate;
   var bt := blUnknown;
   var node := FindNode(ANode, BLOCK_TAG);
   if node <> nil then
      bt := TRttiEnumerationType.GetValue<TBlockType>(GetNodeAttrStr(node, BLOCK_TYPE_ATTR));
   if bt in [blMain, blUnknown, blComment] then
      Gerr_text := i18Manager.GetString('BadImportTag')
   else
   begin
      var lParent: TGroupBlock := nil;
      var block: TBlock := nil;
      if FRedArrow = 0 then
      begin
         lParent := FParentBlock;
         block := Self;
      end
      else if FRedArrow > 0 then
         lParent := TGroupBlock(Self);
      if lParent <> nil then
      begin
         var newBlock: TBlock := nil;
         lParent.BlockImportMode := True;
         try
            newBlock := TXMLProcessor.ImportFlowchartFromXML(node, lParent, block, FRedArrow, result);
         finally
            lParent.BlockImportMode := False;
         end;
         if newBlock <> nil then
         begin
            lParent.ResizeWithDrawLock;
            newBlock.ImportCommentsFromXML(node);
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
   if (AEdit <> nil) and ShouldFocusEditor then
   begin
      var chLine := TInfra.GetChangeLine(Self, AEdit);
      if chLine.Row <> ROW_NOT_FOUND then
      begin
         chLine.Text := ReplaceStr(chLine.Text, PRIMARY_PLACEHOLDER, Trim(AEdit.Text));
         chLine.Text := TInfra.StripInstrEnd(chLine.Text);
         TInfra.GetEditorForm.UpdateEditorForBlock(Self, chLine);
      end;
   end;
end;

function TBlock.ShouldUpdateEditor: boolean;
begin
   var funcHeader := TInfra.GetFunctionHeader(Self);
   var skipUpdateEditor := (funcHeader <> nil) and (TInfra.IsNOkColor(funcHeader.Font.Color) or (funcHeader.chkExternal.Checked and not GInfra.CurrentLang.CodeIncludeExternFunction));
   result := TInfra.ShouldUpdateEditor and not skipUpdateEditor;
end;

function TBlock.ShouldFocusEditor: boolean;
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

function TBlock.FindTemplate(const ALangId, ATemplate: string): string;
begin
   result := '';
   if not ATemplate.IsEmpty then
      result := ATemplate
   else if not GetBlockTemplate(ALangId).IsEmpty then
      result := GetBlockTemplateExpr(ALangId);
end;

function TBlock.FillTemplate(const ALangId, ATemplate: string): string;
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
   DeSelect;
   var bitmap: TBitmap := nil;
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   bitmap.Width := Width + 2;
   bitmap.Height := Height + 2;
   var lPage := Page;
   lPage.DrawI := False;
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
      lPage.DrawI := True;
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
begin
   if ABranch <> nil then
   begin
      var p: TPoint;
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
