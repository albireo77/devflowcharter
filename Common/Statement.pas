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

unit Statement;

interface

uses
   System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, WinApi.Messages,
   Interfaces, Types, YaccLib;

type

  TStatement = class;

  TOnChangeExtend = procedure(AStatement: TStatement) of object;

  TStatement = class(TCustomEdit, IWithId, IWithFocus)
  private
    { Private declarations }
    FExecuteParse: boolean;
    FParserMode: TYYMode;
    FId,
    FLMargin,
    FRMargin: integer;
    function GetId: integer;
    procedure ApplyMargins;
  protected
    procedure WndProc(var msg: TMessage); override;
    procedure CreateHandle; override;
  public
    { Public declarations }
    OnChangeExtend: TOnChangeExtend;
    property Id: integer read GetId;
    constructor Create(AParent: TWinControl; AParserMode: TYYMode; AId: integer = ID_INVALID);
    destructor Destroy; override;
    procedure Change; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    function RetrieveFocus(AInfo: TFocusInfo): boolean;
    function CanBeFocused: boolean;
    function GetFocusColor: TColor;
    function Remove(ANode: TTreeNodeWithFriend = nil): boolean;
    function CanRemove: boolean;
    function IsBoldDesc: boolean;
    function GetTreeNodeText(ANodeOffset: integer = 0): string;
    procedure SetLRMargins(ALMargin, ARMargin: integer);
  published
    //property Anchors;
    //property AutoSelect;
    //property AutoSize;
    //property BevelInner;
    //property BevelOuter;
    //property BiDiMode;
    property BorderStyle;
    //property CharCase;
    property Color;
    //property Constraints;
    //property Ctl3D;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    //property Enabled;
    property Font;
    //property HideSelection;
    //property ImeMode;
    //property ImeName;
    //property MaxLength;
    //property OEMConvert;
    //property ParentBiDiMode;
    //property ParentColor;
    //property ParentCtl3D;
    //property ParentFont;
    //property ParentShowHint;
    //property PasswordChar;
    //property PopupMenu;
    //property ReadOnly;
    //property ShowHint;
    //property TabOrder;
    //property TabStop;
    //property Text;
    //property Visible;
    //property OnChange;
    //property OnClick;
    //property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnEndDock;
    //property OnEndDrag;
    //property OnEnter;
    //property OnExit;
    //property OnKeyDown;
    //property OnKeyPress;
    //property OnKeyUp;
    //property OnMouseDown;
    //property OnMouseMove;
    //property OnMouseUp;
    //property OnStartDock;
    //property OnStartDrag;
  end;

implementation

uses
   WinApi.Windows, System.SysUtils, Vcl.Forms, Infrastructure, Base_Block, Navigator_Form, Constants;

constructor TStatement.Create(AParent: TWinControl; AParserMode: TYYMode; AId: integer = ID_INVALID);
begin
   inherited Create(AParent);
   Parent := AParent;
   Color := TBlock(AParent).Color;
   PopupMenu := TBlock(AParent).Page.Form.pmEdits;
   BorderStyle := bsNone;
   ShowHint := True;
   AutoSelect := False;
   DoubleBuffered := true;
   OnChangeExtend := nil;
   ControlStyle := ControlStyle + [csOpaque];
   FParserMode := AParserMode;
   FId := GProject.Register(Self, AId);
   if CanFocus then
      SetFocus;
end;

destructor TStatement.Destroy;
begin
   GProject.UnRegister(Self);
   inherited Destroy;
end;

procedure TStatement.SetLRMargins(ALMargin, ARMargin: integer);
begin
   if ALMargin >= 0 then
      FLMargin := ALMargin;
   if ARMargin >= 0 then
      FRMargin := ARMargin;
   ApplyMargins;
end;

procedure TStatement.ApplyMargins;
begin
   Perform(EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLong(FLMargin, FRMargin));
end;

procedure TStatement.WndProc(var msg: TMessage);
begin
   inherited;
   if (msg.Msg = CM_FONTCHANGED) and not (csLoading in ComponentState) then
      ApplyMargins;
end;

procedure TStatement.CreateHandle;
begin
   inherited;
   ApplyMargins;
end;

function TStatement.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   AInfo.FocusEdit := Self;
   result := TBlock(Parent).RetrieveFocus(AInfo);
end;

function TStatement.CanBeFocused: boolean;
begin
   result := TBlock(Parent).CanBeFocused;
end;

procedure TStatement.Change;
begin
   inherited Change;
   var txt := Trim(Text);
   Font.Color := GSettings.FontColor;
   GProject.SetChanged;
   Hint := i18Manager.GetFormattedString('ExpOk', [txt, sLineBreak]);
   TBlock(Parent).UpdateEditor(Self);
   if FExecuteParse then
   begin
      if txt.IsEmpty then
      begin
         case FParserMode of
            yymFor:
            begin
               Hint := i18Manager.GetFormattedString('ExpErr', ['', sLineBreak, i18Manager.GetString('IntReq')]);
               Font.Color := NOK_COLOR;
            end;
            yymCondition:
            begin
               Hint := i18Manager.GetFormattedString('NoCExp', [sLineBreak]);
               Font.Color := NOK_COLOR;
            end;
            yymCase:
            begin
               Hint := i18Manager.GetFormattedString('NoCaseExp', [sLineBreak]);
               Font.Color := NOK_COLOR;
            end;
            yymAssign:
            begin
               Hint := i18Manager.GetFormattedString('NoInstr', [sLineBreak]);
               Font.Color := WARN_COLOR;
            end;
            yymFuncCall:
            begin
               Hint := i18Manager.GetFormattedString('NoFCall', [sLineBreak]);
               Font.Color := WARN_COLOR;
            end;
         end;
      end
      else if not TInfra.Parse(Self, FParserMode) then
      begin
         Hint := i18Manager.GetFormattedString('ExpErr', [txt, sLineBreak, TInfra.GetParserErrMsg]);
         Font.Color := NOK_COLOR;
      end;
   end;
   if Assigned(OnChangeExtend) then
      OnChangeExtend(Self);
   NavigatorForm.DoInvalidate;
end;

procedure TStatement.KeyDown(var Key: Word; Shift: TShiftState);
begin
   inherited KeyDown(Key, Shift);
   TInfra.OnKeyDownSelectAll(Self, Key, Shift);
end;

procedure TStatement.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
   inherited MouseDown(Button, Shift, X, Y);
   if (Parent is TBlock) and Assigned(TBlock(Parent).OnMouseDown) then
      TBlock(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

procedure TStatement.DoEnter;
begin
   inherited DoEnter;
   case FParserMode of
      yymInput:     FExecuteParse := GSettings.ParseInput;
      yymOutput:    FExecuteParse := GSettings.ParseOutput;
      yymAssign:    FExecuteParse := GSettings.ParseAssign;
      yymFuncCall:  FExecuteParse := GSettings.ParseRoutineCall;
      yymFor:       FExecuteParse := GSettings.ParseFor;
      yymReturn:    FExecuteParse := GSettings.ParseReturn;
      yymCondition: FExecuteParse := GSettings.ParseCondition;
      yymCase,
      yymCaseValue: FExecuteParse := GSettings.ParseCase;
   else
      FExecuteParse := false;
   end;
   var chon := GProject.ChangingOn;
   GProject.ChangingOn := false;
   try
      Change;
   finally
      GProject.ChangingOn := chon;
   end;
end;

function TStatement.GetId: integer;
begin
   result := FId;
end;

function TStatement.GetFocusColor: TColor;
begin
   if HasParent then
      result := Font.Color
   else
      result := OK_COLOR;
end;

function TStatement.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := CanRemove;
   if result then
      result := TBlock(Parent).Remove(ANode);
end;

function TStatement.CanRemove: boolean;
begin
   result := HasParent and TBlock(Parent).CanRemove;
end;

function TStatement.IsBoldDesc: boolean;
begin
   result := false;
end;

function TStatement.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := '';
   if HasParent then
      result := TBlock(Parent).GetTreeNodeText(ANodeOffset);
end;

end.
