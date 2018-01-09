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
   System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, CommonInterfaces, CommonTypes;

type

  TOnEditChange = procedure(AEdit: TCustomEdit) of object;

  TStatement = class(TCustomEdit, IIdentifiable, IFocusable)
  private
    { Private declarations }
    FExecuteParse: boolean;
    FParserMode: TParserMode;
    FId: integer;
    function GetId: integer;
  public
    { Public declarations }
    OnChangeCallBack: TOnEditChange;
    property ParserMode: TParserMode read FParserMode default prsNone;
    property Id: integer read GetId;
    procedure Change; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AId: integer); overload;
    destructor Destroy; override;
    function RetrieveFocus(AInfo: TFocusInfo): boolean;
    function CanBeFocused: boolean;
    function GetFocusColor: TColor;
    function Remove: boolean;
    function CanBeRemoved: boolean;
    function IsBoldDesc: boolean;
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
   WinApi.Windows, System.SysUtils, Vcl.Forms, ApplicationCommon, Base_Block, Navigator_Form;

constructor TStatement.Create(AOwner: TComponent);
const
   BlockToParserMapping: array[TBlockType] of TParserMode = (prsNone, prsAssign, prsAssign,
                         prsInput, prsOutput, prsFuncCall, prsCondition, prsCondition, prsCondition,
                         prsCondition, prsFor, prsCase, prsNone, prsNone, prsReturn, prsNone, prsNone);
var
   block: TBlock;
begin
   inherited Create(AOwner);
   Parent := TWinControl(AOwner);
   block := TBlock(AOwner);
   Color := block.Color;
   PopupMenu := block.Page.Form.pmEdits;
   FParserMode := BlockToParserMapping[block.BType];
   if Parent.ControlCount > 0 then
   begin
      if Parent.Controls[0] = Self then
      begin
         if Parent.Parent is TBlock then
            block := TBlock(Parent.Parent);
      end
      else if FParserMode = prsCase then
         FParserMode := prsCaseValue;
   end;
   Font.Assign(block.GetFont);
   BorderStyle := bsNone;
   BorderWidth := 0;
   ShowHint := True;
   BevelKind := bkNone;
   AutoSelect := False;
   DoubleBuffered := true;
   FId := GProject.Register(Self);
   OnChangeCallBack := nil;
   Font.Name := GSettings.FlowchartFontName;
   ControlStyle := ControlStyle + [csOpaque];
   if CanFocus then
      SetFocus;
end;

constructor TStatement.Create(AOwner: TComponent; const AId: integer);
begin
   Create(AOwner);
   FId := GProject.Register(Self, AId);
end;

destructor TStatement.Destroy;
begin
   GProject.UnRegister(Self);
   inherited Destroy;
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
var
   txt: string;
begin

   inherited Change;
   
   txt := Trim(Text);
   Font.Color := GSettings.FontColor;
   GProject.SetChanged;
   Hint := i18Manager.GetFormattedString('ExpOk', [txt, sLineBreak]);
   TBlock(Parent).UpdateEditor(Self);

    if FExecuteParse then
   begin
      if txt.IsEmpty then
      begin
         case FParserMode of
            prsFor:
            begin
               Hint := i18Manager.GetFormattedString('ExpErr', ['', sLineBreak, i18Manager.GetString('IntReq')]);
               Font.Color := NOK_COLOR;
            end;
            prsCondition:
            begin
               txt := 'NoCExp';
               Font.Color := NOK_COLOR;
            end;
            prsCase:
            begin
               txt := 'NoCaseExp';
               Font.Color := NOK_COLOR;
            end;
            prsAssign:
            begin
               txt := 'NoInstr';
               Font.Color := WARN_COLOR;
            end;
            prsFuncCall:
            begin
               txt := 'NoFCall';
               Font.Color := WARN_COLOR;
            end;
         end;
         if not txt.IsEmpty then
            Hint := i18Manager.GetFormattedString(txt, [sLineBreak]);
      end
      else if not TInfra.Parse(Self, FParserMode) then
      begin
         Hint := i18Manager.GetFormattedString('ExpErr', [txt, sLineBreak, errString]);
         Font.Color := NOK_COLOR;
      end;
      if Assigned(OnChangeCallBack) then
         OnChangeCallBack(Self);
   end;
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
var
   chon: boolean;
begin
   inherited DoEnter;
   case FParserMode of
      prsInput:     FExecuteParse := GSettings.ParseInput;
      prsOutput:    FExecuteParse := GSettings.ParseOutput;
      prsAssign:    FExecuteParse := GSettings.ParseAssign;
      prsFuncCall:  FExecuteParse := GSettings.ParseRoutineCall;
      prsFor:       FExecuteParse := GSettings.ParseFor;
      prsReturn:    FExecuteParse := GSettings.ParseReturn;
      prsCondition: FExecuteParse := GSettings.ParseCondition;
      prsCase,
      prsCaseValue: FExecuteParse := GSettings.ParseCase;
   else
      FExecuteParse := false;
   end;
   chon := GProject.ChangingOn;
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

function TStatement.Remove: boolean;
begin
   result := CanBeRemoved;
   if result then
      result := TBlock(Parent).Remove;
end;

function TStatement.CanBeRemoved: boolean;
begin
   result := HasParent and TBlock(Parent).CanBeRemoved;
end;

function TStatement.IsBoldDesc: boolean;
begin
   result := false;
end;

end.
