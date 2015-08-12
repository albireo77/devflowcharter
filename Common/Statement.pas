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
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
   CommonInterfaces, ComCtrls, CommonTypes;

type

  TOnChangeComplement = procedure of object;

  TStatement = class(TCustomEdit, IIdentifiable, IFocusable)
  private
    { Private declarations }
    FAlignment: TAlignment;
    FExecuteParse: boolean;
    FParserMode: TParserMode;
    FId: integer;
    function GetId: integer;
  public
    { Public declarations }
    OnChangeComplement: TOnChangeComplement;
    property ParserMode: TParserMode read FParserMode default prNone;
    property Id: integer read GetId;
    class procedure SetFontSize(const AControl: TControl; const ASize: integer);
    class procedure SetFontStyle(const AControl: TControl; const AStyle: TFontStyles);
    procedure Change; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoEnter; override;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AId: integer); overload;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetAlignment(AValue: TAlignment);
    function RetrieveFocus(AInfo: TFocusInfo): boolean;
    function CanBeFocused: boolean;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    function GetFocusColor: TColor;
    procedure Remove;
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
   ApplicationCommon, Base_Block, Navigator_Form;

type
  THackCustomEdit = class(TCustomEdit);
  THackControl = class(TControl);

constructor TStatement.Create(AOwner: TComponent);
const
   BlockToParserMapping: array[TBlockType] of TParserMode = (prNone, prAssign, prAssign,
                         prInput, prOutput, prFuncCall, prCondition, prCondition, prCondition,
                         prCondition, prFor, prCase, prNone, prNone, prReturn, prNone);
var
   lBlock: TBlock;
   lControl: TControl;
begin
   inherited Create(AOwner);
   Parent := TWinControl(AOwner);
   lBlock := TBlock(AOwner);
   Color := lBlock.Color;
   FParserMode := BlockToParserMapping[lBlock.BType];
   if Parent.ControlCount > 0 then
   begin
      if Parent.Controls[0] = Self then
      begin
         if Parent.Parent is TBlock then
            lBlock := TBlock(Parent.Parent);
      end
      else if FParserMode = prCase then
         FParserMode := prCaseValue;
   end;
   lControl := lBlock.GetTextControl;
   if lControl <> nil then
      Font.Assign(THackControl(lControl).Font);
   BorderStyle := bsNone;
   DoubleBuffered := True;
   ShowHint := True;
   BevelKind := bkNone;
   AutoSelect := False;
   FId := GProject.Register(Self);
   OnChangeComplement := nil;
   Font.Name := GSettings.FlowchartFontName;
   ControlStyle := ControlStyle + [csOpaque];
   if CanFocus then SetFocus;
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

procedure TStatement.CreateParams(var Params: TCreateParams);
const
   Alignments: array[Boolean, TAlignment] of Longint =
    ((ES_LEFT, ES_RIGHT, ES_CENTER),(ES_RIGHT, ES_LEFT, ES_CENTER));
begin
   inherited CreateParams(Params);
   Params.Style := Params.Style {or ES_MULTILINE} or Alignments[UseRightToLeftAlignment, FAlignment];
end;

function TStatement.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   AInfo.FocusEdit := Self;
   result := TBlock(Parent).RetrieveFocus(AInfo);
end;

function TStatement.CanBeFocused: boolean;
begin
   result := CanFocus;
end;

procedure TStatement.SetAlignment(AValue: TAlignment);
begin
   if FAlignment <> AValue then
   begin
      FAlignment := AValue;
      RecreateWnd;
   end;
end;

procedure TStatement.Change;
var
   lText: string;
begin

   inherited Change;
   
   lText := Trim(Text);
   Font.Color := GSettings.FontColor;
   GChange := 1;
   Hint := i18Manager.GetFormattedString('ExpOk', [lText, CRLF]);
   if GSettings.UpdateCodeEditor and not TBlock(Parent).SkipUpdateCodeEditor then
      TBlock(Parent).UpdateEditor(Self);

   if FExecuteParse then
   begin
      if lText = '' then
      begin
         case FParserMode of
            prFor:
            begin
               Hint := i18Manager.GetFormattedString('ExpErr', ['', CRLF, i18Manager.GetString('IntReq')]);
               Font.Color := NOK_COLOR;
            end;
            prCondition:
            begin
               lText := 'NoCExp';
               Font.Color := NOK_COLOR;
            end;
            prCase:
            begin
               lText := 'NoCaseExp';
               Font.Color := NOK_COLOR;
            end;
            prAssign:
            begin
               lText := 'NoInstr';
               Font.Color := WARN_COLOR;
            end;
            prFuncCall:
            begin
               lText := 'NoFCall';
               Font.Color := WARN_COLOR;
            end;
         end;
         if lText <> '' then
            Hint := i18Manager.GetFormattedString(lText, [CRLF]);
      end
      else if not TInfra.Parse(Self, FParserMode) then
      begin
         Hint := i18Manager.GetFormattedString('ExpErr', [lText, CRLF, errString]);
         Font.Color := NOK_COLOR;
      end;
      if Assigned(OnChangeComplement) then
         OnChangeComplement;
   end;
   NavigatorForm.ExecuteRepaint
end;

procedure TStatement.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
   inherited MouseDown(Button, Shift, X, Y);
   if (Parent is TBlock) and Assigned(TBlock(Parent).OnMouseDown) then
      TBlock(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

procedure TStatement.DoEnter;
var
   lChange: byte;
begin
   inherited DoEnter;
   case FParserMode of
      prInput:     FExecuteParse := GSettings.ParseInput;
      prOutput:    FExecuteParse := GSettings.ParseOutput;
      prAssign:    FExecuteParse := GSettings.ParseAssign;
      prFuncCall:  FExecuteParse := GSettings.ParseRoutineCall;
      prFor:       FExecuteParse := GSettings.ParseFor;
      prReturn:    FExecuteParse := GSettings.ParseReturn;
      prCondition: FExecuteParse := GSettings.ParseCondition;
      prCase,
      prCaseValue: FExecuteParse := GSettings.ParseCase;
   else
      FExecuteParse := false;
   end;
   lChange := GChange;
   Change;
   if lChange = 0 then
      GChange := 0;
end;

class procedure TStatement.SetFontSize(const AControl: TControl; const ASize: integer);
var
   lFlag: boolean;
begin
   lFlag := (AControl is TCustomEdit) and (THackCustomEdit(AControl).BorderStyle = bsNone);
   if lFlag then THackCustomEdit(AControl).BorderStyle := bsSingle;
   THackControl(AControl).Font.Size := ASize;
   if lFlag then THackCustomEdit(AControl).BorderStyle := bsNone;
end;

class procedure TStatement.SetFontStyle(const AControl: TControl; const AStyle: TFontStyles);
begin
   THackControl(AControl).Font.Style := AStyle;
end;

function TStatement.GetId: integer;
begin
   result := FId;
end;

function TStatement.GetFocusColor: TColor;
begin
   result := Font.Color;
end;

procedure TStatement.Remove;
begin
   if CanBeRemoved then
      TBlock(Parent).Remove;
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
