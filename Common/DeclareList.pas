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
  
unit DeclareList;

interface

uses
   Controls, Types, OmniXML, StdCtrls, Grids, Classes, Windows, SizeEdit, CommonInterfaces,
   Base_Form, Messages, Graphics;

type

    TStringGridEx = class(TStringGrid)
       private
          FColWidthsChanged: TNotifyEvent;
          FTopLeftChanged: TNotifyEvent;
       protected
          procedure ColWidthsChanged; override;
          procedure TopLeftChanged; override;
       published
          property OnColWidthsChanged: TNotifyEvent read FColWidthsChanged write FColWidthsChanged;
          property OnTopLeftChanged: TNotifyEvent read FTopLeftChanged write FTopLeftChanged;
    end;

   TDeclareList = class(TGroupBox, IFocusable, IIdentifiable)
      protected
         FModifying: boolean;
         FParentForm: TBaseForm;
         FId,
         FDragRow: integer;
         function GetId: integer;
         function IsDeclared(const AName: string; const AssociatedListCheck: boolean): boolean;
         function AddUpdateRow: integer; virtual;
         function IsRowVisible(const ARow: integer): boolean;
         function FindValidRowByPoint(const APoint: TPoint): integer;
         procedure OnRowMovedList(Sender: TObject; FromIndex, ToIndex: Longint); virtual;
         procedure OnClickAdd(Sender: TObject); virtual; abstract;
         procedure OnClickRemove(Sender: TObject); virtual;
         procedure OnClickChange(Sender: TObject); virtual;
         procedure OnDblClickList(Sender: TObject);
         procedure OnSelectCellList(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
         procedure OnMouseDownList(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
         procedure OnDragDropList(Sender, Source: TObject; X, Y: Integer);
         procedure OnDragOverList(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
         procedure OnKeyDownCommon(Sender: TObject; var Key: Word; Shift: TShiftState);
         procedure UpdateCodeEditor;
      public
         sgList: TStringGridEx;
         btnRemove,
         btnChange,
         btnAdd: TButton;
         gbBox: TGroupBox;
         lblName: TLabel;
         edtName: TEdit;
         AssociatedList: TDeclareList;
         property Id: integer read GetId;
         property ParentForm: TBaseForm read FParentForm;
         constructor Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         destructor Destroy; override;
         procedure ImportFromXMLTag(const rootTag: IXMLElement); virtual;
         procedure ExportToXMLTag(const rootTag: IXMLElement); virtual;
         function RetrieveFocus(AInfo: TFocusInfo): boolean;
         function CanBeFocused: boolean;
         function GetFocusColor: TColor;
         function Remove: boolean;
         function CanBeRemoved: boolean;
         function IsBoldDesc: boolean;
         procedure SetDefaultFocus;
   end;

   TVarDeclareList = class(TDeclareList)
      protected
         function AddUpdateRow: integer; override;
         procedure OnClickAdd(Sender: TObject); override;
         procedure OnClickRemove(Sender: TObject); override;
         procedure OnClickChange(Sender: TObject); override;
      public
         edtSize: TSizeEdit;
         cbType: TComboBox;
         edtInit: TEdit;
         lblType,
         lblSize,
         lblInit: TLabel;
         constructor Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         procedure ImportFromXMLTag(const rootTag: IXMLElement); override;
         procedure ExportToXMLTag(const rootTag: IXMLElement); override;
         procedure FillForList(const AList: TStrings);
         function IsValidLoopVar(const AName: string): boolean;
         function GetDimensionCount(const AVarName: string; const AIncludeTypeDimens: boolean = false): integer;
         function GetDimension(const AVarName: string; const ADimensIndex: integer): string;
   end;
   
   TConstDeclareList = class(TDeclareList)
      protected
         function AddUpdateRow: integer; override;
         function CreateCheckBox(const ACol, ARow: integer): TCheckBox;
         procedure OnColWidthsChanged(Sender: TObject);
         procedure OnTopLeftChanged(Sender: TObject);
         procedure RefreshChBoxes;
         function GetCheckBoxPoint(const ACol, ARow: integer): TPoint;
         procedure OnClickChBox(Sender: TObject);
         procedure OnClickAdd(Sender: TObject); override;
         procedure OnClickChange(Sender: TObject); override;
         procedure OnClickRemove(Sender: TObject); override;
         function GetRightMargin(const AControl: TControl = nil): integer;
         procedure OnRowMovedList(Sender: TObject; FromIndex, ToIndex: Longint); override;
         procedure WMSize(var Msg: TMessage); message WM_SIZE;
      public
         edtValue: TEdit;
         lblValue: TLabel;
         constructor Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         procedure ImportFromXMLTag(const rootTag: IXMLElement); override;
         procedure ExportToXMLTag(const rootTag: IXMLElement); override;
         function GetValue(const AIdent: string): string;
         function IsExternal(const ARow: integer): boolean;
   end;

const
   NAME_COL = 0;

   VAR_NAME_COL = NAME_COL;
   VAR_TYPE_COL = 1;
   VAR_SIZE_COL = 2;
   VAR_INIT_COL = 3;

   CONST_NAME_COL  = NAME_COL;
   CONST_VALUE_COL = 1;
   CONST_CHBOX_COL = 2;

   DEF_VARLIST_WIDTH = 289;
   DEF_CONSTLIST_WIDTH = 200;

implementation

uses
   ApplicationCommon, SysUtils, XMLProcessor, Dialogs, Project, StrUtils, CommonTypes, UserDataType,
   LangDefinition, ParserHelper;

constructor TDeclareList.Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
var
   i, colWidth: integer;
begin

   inherited Create(AParent);
   Parent := AParent;
   FParentForm := TInfra.FindParentForm(Self);
   ParentFont := false;
   Font.Style := [fsBold];
   Font.Color := clBlack;
   FModifying := false;
   AssociatedList := nil;
   DoubleBuffered := true;
   FDragRow := -1;
   FId := TProject.GetInstance.Register(Self);

   sgList := TStringGridEx.Create(Self);
   sgList.Parent := Self;
   sgList.DefaultRowHeight := 16;
   sgList.SetBounds(5, 16, AWidth-9, (ADispRowCount+1)*(sgList.DefaultRowHeight+2));
   sgList.ColCount := AColCount;
   colWidth := (sgList.Width div AColCount) - 2;
   for i := 0 to AColCount-1 do
      sgList.ColWidths[i] := colWidth;
   sgList.FixedRows := 1;
   sgList.FixedCols := 0;
   sgList.RowCount := 2;
   sgList.FixedColor := clMoneyGreen;
   sgList.Options := sgList.Options + [goRowSelect, goColSizing, goThumbTracking, goRowMoving] - [goRangeSelect];
   sgList.ScrollBars := ssVertical;
   sgList.ParentFont := false;
   sgList.Font.Style := [];
   sgList.DoubleBuffered := true;
   sgList.OnDblClick := OnDblClickList;
   sgList.OnSelectCell := OnSelectCellList;
   sgList.OnMouseDown := OnMouseDownList;
   sgList.OnDragDrop := OnDragDropList;
   sgList.OnDragOver := OnDragOverList;
   sgList.OnRowMoved := OnRowMovedList;

   SetBounds(ALeft, ATop, AWidth, (ADispRowCount+1)*(sgList.DefaultRowHeight+2)+157);
   sgList.Anchors := [akTop, akBottom, akLeft, akRight];

   btnRemove := TButton.Create(Self);
   btnRemove.Parent := Self;
   btnRemove.SetBounds(5, sgList.Top+sgList.Height+8, (Width div 2)-5, 25);
   btnRemove.OnClick := OnClickRemove;
   btnRemove.Caption := i18Manager.GetString('btnRemove');
   btnRemove.ParentFont := false;
   btnRemove.Font.Style := [];
   btnRemove.Enabled := false;
   btnRemove.Anchors := [akBottom, akLeft];

   btnChange := TButton.Create(Self);
   btnChange.Parent := Self;
   btnChange.SetBounds(Width div 2, btnRemove.Top, (Width div 2)-5, 25);
   btnChange.OnClick := OnClickChange;
   btnChange.Caption := i18Manager.GetString('btnChange');
   btnChange.ParentFont := false;
   btnChange.Font.Style := [];
   btnChange.Enabled := false;
   btnChange.Anchors := [akBottom, akLeft];

   gbBox := TGroupBox.Create(Self);
   gbBox.Parent := Self;
   gbBox.SetBounds(5, btnChange.Top+btnChange.Height+4, AGBoxWidth, 72);
   gbBox.ParentFont := false;
   gbBox.Font.Style := [];
   gbBox.Anchors := [akBottom, akLeft];

   lblName := TLabel.Create(gbBox);
   lblName.Parent := gbBox;
   lblName.Top := 22;
   lblName.Left := 5;
   lblName.Caption := i18Manager.GetString('sgVarListCol0');

   edtName := TEdit.Create(gbBox);
   edtName.Parent := gbBox;
   edtName.OnKeyDown := OnKeyDownCommon;
   edtName.Anchors := edtName.Anchors + [akRight];

   btnAdd := TButton.Create(Self);
   btnAdd.Parent := Self;
   btnAdd.SetBounds(5, gbBox.Top+gbBox.Height+3, Width-9, 25);
   btnAdd.OnClick := OnClickAdd;
   btnAdd.ParentFont := false;
   btnAdd.Font.Style := [];
   btnAdd.Anchors := [akLeft, akBottom, akRight];

end;

destructor TDeclareList.Destroy;
begin
   TProject.GetInstance.UnRegister(Self);
   inherited Destroy;
end;

constructor TConstDeclareList.Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
var
   i: integer;
begin

   inherited Create(AParent, ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth);

   for i := 0 to AColCount-1 do
      sgList.Cells[i, 0] := i18Manager.GetString('sgConstListCol'+IntToStr(i));

   edtName.SetBounds(lblName.Width+10, 17, gbBox.Width-lblName.Width-18, 21);
   
   lblValue := TLabel.Create(gbBox);
   lblValue.Parent := gbBox;
   lblValue.Top := 47;
   lblValue.Caption := i18Manager.GetString('sgConstListCol1');
   lblValue.Left := 5;

   sgList.OnColWidthsChanged := OnColWidthsChanged;
   sgList.OnTopLeftChanged := OnTopLeftChanged;

   if GInfra.CurrentLang.UpperCaseConstId then
      edtName.CharCase := ecUpperCase;

   edtValue := TEdit.Create(gbBox);
   edtValue.Parent := gbBox;
   edtValue.SetBounds(lblValue.Width+10, 42, gbBox.Width-lblValue.Width-18, 21);
   edtValue.Anchors := edtValue.Anchors + [akRight];
   edtValue.OnKeyDown := OnKeyDownCommon;

   gbBox.Caption := i18Manager.GetString('gbConstant');
   btnAdd.Caption := i18Manager.GetString('btnAddConst');

   Anchors := Anchors + [akBottom];

end;

constructor TVarDeclareList.Create(const AParent: TWinControl; const ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
var
   i: integer;
begin

   inherited Create(AParent, ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth);

   for i := 0 to AColCount-1 do
      sgList.Cells[i, 0] := i18Manager.GetString('sgVarListCol'+IntToStr(i));

   edtName.SetBounds(lblName.Width+10, 17, btnRemove.Left+btnRemove.Width-lblName.Width-12, 21);
   
   lblType := TLabel.Create(gbBox);
   lblType.Parent := gbBox;
   lblType.Left := lblName.Width + edtName.Width + 20;
   lblType.Top := 22;
   lblType.Caption := i18Manager.GetString('sgVarListCol1');

   lblSize := TLabel.Create(gbBox);
   lblSize.Parent := gbBox;
   lblSize.Top := 47;
   lblSize.Caption := i18Manager.GetString('sgVarListCol2');
   lblSize.Left := 5;

   edtSize := TSizeEdit.Create(gbBox);
   edtSize.Parent := gbBox;
   edtSize.SetBounds(lblSize.Width+10, 42, edtName.Left+edtName.Width-lblSize.Width-9, 21);
   edtSize.OnKeyDown := OnKeyDownCommon;

   cbType := TComboBox.Create(gbBox);
   cbType.Parent := gbBox;
   cbType.Style := csDropDownList;
   cbType.SetBounds(lblName.Width+edtName.Width+lblType.Width+25, 17, 73, 21);
   cbType.Constraints.MaxWidth := gbBox.Width - cbType.Left - 7;
   cbType.OnKeyDown := OnKeyDownCommon;
   TInfra.PopulateDataTypeCombo(cbType);

   lblInit := TLabel.Create(gbBox);
   lblInit.Parent := gbBox;
   lblInit.Left := edtSize.Left + edtSize.Width + 10;
   lblInit.Top := 47;
   lblInit.Caption := i18Manager.GetString('sgVarListCol3');

   edtInit := TEdit.Create(gbBox);
   edtInit.Parent := gbBox;
   edtInit.SetBounds(lblInit.Left+lblInit.Width+5, 42, gbBox.Width-lblInit.Width-lblInit.Left-13, 21);
   edtInit.OnKeyDown := OnKeyDownCommon;

   gbBox.Caption := i18Manager.GetString('gbVariable');
   btnAdd.Caption := i18Manager.GetString('btnAddVar');

   Anchors := Anchors + [akBottom];
   
end;

function TDeclareList.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   i: integer;
   lName: string;
   lList: TDeclareList;
   lForm: TBaseForm;
   lType: TUserDataType;
begin
   i := 0;
   lList := nil;
   lName := Trim(AInfo.SelText);
   if lName <> '' then
   begin
      i := sgList.Cols[NAME_COL].IndexOf(lName);
      if i > 0 then
         lList := Self
      else if AssociatedList <> nil then
      begin
         i := AssociatedList.sgList.Cols[NAME_COL].IndexOf(lName);
         if i > 0 then
            lList := AssociatedList;
      end;
      if lList = nil then
      begin
         lType := GProject.GetUserDataType(lName);
         if lType <> nil then
         begin
            result := lType.RetrieveFocus(AInfo);
            if result then exit;
         end;
      end;
   end;
   if lList <> nil then
   begin
      lForm := lList.ParentForm;
      lList.sgList.Row := i;
      lList.Show;
   end
   else
      lForm := FParentForm;
   if lForm <> nil then
      lForm.Show;
   result := i > 0;
end;

function TDeclareList.CanBeFocused: boolean;
begin
   result := true;
end;

function TDeclareList.GetId: integer;
begin
   result := FId;
end;

procedure TConstDeclareList.WMSize(var Msg: TMessage);
begin
   inherited;
   RefreshChBoxes;
end;

procedure TDeclareList.SetDefaultFocus;
begin
   if edtName.CanFocus and not (FParentForm.ActiveControl is TCustomEdit) then
      edtName.SetFocus;
end;

function TVarDeclareList.GetDimensionCount(const AVarName: string; const AIncludeTypeDimens: boolean = false): integer;
var
   i, a: integer;
   lDataType: TUserDataType;
   lSize: string;
begin
   result := 0;
   if GProject <> nil then
   begin
      i := sgList.Cols[VAR_NAME_COL].IndexOf(AVarName);
      if i > 0 then
      begin
         if AIncludeTypeDimens then
         begin
            lDataType := GProject.GetUserDataType(sgList.Cells[VAR_TYPE_COL, i]);
            if lDataType <> nil then
               result := lDataType.GetDimensionCount;
         end;
         lSize := sgList.Cells[VAR_SIZE_COL, i];
         for a := 1 to Length(lSize) do
         begin
            if lSize[a] = ',' then
               Inc(result);
         end;
         if lSize <> '1' then
            Inc(result);
      end;
   end;
end;

function TVarDeclareList.GetDimension(const AVarName: string; const ADimensIndex: integer): string;
var
   i, a, cnt: integer;
   lString, lDims: string;
   lDataType: TUserDataType;
begin
   result := '';
   i := sgList.Cols[VAR_NAME_COL].IndexOf(AVarName);
   if i > 0 then
   begin
      cnt := 1;
      lString := sgList.Cells[VAR_SIZE_COL, i];
      if GProject <> nil then
      begin
         lDataType := GProject.GetUserDataType(sgList.Cells[VAR_TYPE_COL, i]);
         if lDataType <> nil then
         begin
            lDims := lDataType.GetDimensions;
            if lDims <> '' then
               lString := lString + ',' + lDims;
         end;
      end;
      for a := 1 to Length(lString) do
      begin
         if lString[a] <> ',' then
            result := result + lString[a]
         else
         begin
            Inc(cnt);
            if cnt > ADimensIndex then
               break
            else
               result := '';
         end;
      end;
   end;
end;

procedure TDeclareList.OnDblClickList(Sender: TObject);
var
   lPoint: TPoint;
begin
   lPoint := sgList.ScreenToClient(Mouse.CursorPos);
   if FindValidRowByPoint(lPoint) <> -1 then
      btnChange.Click;
end;

procedure TDeclareList.OnSelectCellList(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
   if ARow = sgList.RowCount-1 then
      CanSelect := False
   else
   begin
      btnChange.Enabled := True;
      btnRemove.Enabled := True;
   end;
end;

procedure TDeclareList.OnRowMovedList(Sender: TObject; FromIndex, ToIndex: Longint);
begin
   UpdateCodeEditor;
end;

procedure TConstDeclareList.OnRowMovedList(Sender: TObject; FromIndex, ToIndex: Longint);
var
   lControl: TControl;
   lPoint: TPoint;
begin
   inherited OnRowMovedList(Sender, FromIndex, ToIndex);
   if sgList.Objects[CONST_CHBOX_COL, FromIndex] is TControl then
   begin
      lControl := TControl(sgList.Objects[CONST_CHBOX_COL, FromIndex]);
      lPoint := GetCheckBoxPoint(CONST_CHBOX_COL, ToIndex);
      lControl.SetBounds(lPoint.X, lPoint.Y, lControl.Width, lControl.Height);
   end;
   if sgList.Objects[CONST_CHBOX_COL, ToIndex] is TControl then
   begin
      lControl := TControl(sgList.Objects[CONST_CHBOX_COL, ToIndex]);
      lPoint := GetCheckBoxPoint(CONST_CHBOX_COL, FromIndex);
      lControl.SetBounds(lPoint.X, lPoint.Y, lControl.Width, lControl.Height);
   end;
   OnTopLeftChanged(sgList);
end;

function TDeclareList.FindValidRowByPoint(const APoint: TPoint): integer;
var
   lCol, lRow: integer;
begin
   sgList.MouseToCell(APoint.X, APoint.Y, lCol, lRow);
   if (lRow > 0) and (lRow < sgList.RowCount-1) then
      result := lRow
   else
      result := -1;
end;

procedure TDeclareList.OnDragOverList(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   Accept := (Source = sgList) and (FindValidRowByPoint(Point(X, Y)) <> -1);
end;

procedure TDeclareList.OnDragDropList(Sender, Source: TObject; X, Y: Integer);
var
   lRowTo: integer;
begin
   lRowTo := FindValidRowByPoint(Point(X, Y));
   if (Source = sgList) and (lRowTo <> -1) and (FDragRow <> -1) then
      sgList.MoveRow(FDragRow, lRowTo);
   FDragRow := -1;
end;

procedure TDeclareList.OnMouseDownList(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   lRow: integer;
begin
   edtName.SetFocus;
   lRow := FindValidRowByPoint(Point(X, Y));
   if (Button = mbLeft) and (ssShift in Shift) and (lRow <> -1) then
   begin
      FDragRow := lRow;
      sgList.BeginDrag(true);
   end;
end;

procedure TVarDeclareList.OnClickAdd(Sender: TObject);
var
   status, lType: integer;
   lInfo, lInitString: string;
   lEdit: TWinControl;
   lDataType: TUserDataType;
begin
   status := ValidateId(edtName.Text);
   lType := TParserHelper.GetType(cbType.Text);
   if status <> VALID_IDENT then
   else if IsDeclared(edtName.Text, true) then
      status := DUPLICATED_IDENT
   else if GSettings.ValidateDeclaration then
   begin
      if not edtSize.ParseSize then
         status := INCORRECT_SIZE
      else if (edtSize.Text = '1') and not TParserHelper.IsStructType(lType) and Assigned(GInfra.CurrentLang.GetLiteralType) then
      begin
         lInitString := Trim(edtInit.Text);
         if lInitString <> '' then
         begin
            if TParserHelper.IsEnumType(lType) then
            begin
               lDataType := GProject.GetUserDataType(cbType.Text);
               if (lDataType <> nil) and not lDataType.IsValidEnumValue(lInitString) then
                  status := INVALID_INIT_VAL;
            end
            else if not TParserHelper.AreTypesCompatible(lType, GInfra.CurrentLang.GetLiteralType(lInitString)) and
                    not TParserHelper.AreTypesCompatible(lType, TParserHelper.GetConstType(lInitString)) then status := INVALID_INIT_VAL;
         end;
      end;
   end;

   if status <> VALID_IDENT then
   begin
      case status of
         INCORRECT_SIZE:
         begin
            lEdit := edtSize;
            lInfo := 'BadSize';
         end;
         INVALID_INIT_VAL:
         begin
            lEdit := edtInit;
            lInfo := 'BadInitVal';
         end;
      else
         lEdit := edtName;
         case status of
            INCORRECT_IDENT:  lInfo := 'BadId';
            DUPLICATED_IDENT: lInfo := 'DupId';
            RESERVED_IDENT:   lInfo := i18Manager.GetFormattedString('IncorrectIdKeyword', [edtName.Text, GInfra.CurrentLang.Name]);
         end;
      end;
      TInfra.ShowErrorBox(i18Manager.GetString(lInfo), errDeclare);
      lEdit.SetFocus;
   end
   else
      AddUpdateRow;
end;

procedure TDeclareList.UpdateCodeEditor;
begin
   if GSettings.UpdateEditor then
      TInfra.GetEditorForm.RefreshEditorForObject(nil);
end;

function TVarDeclareList.AddUpdateRow: integer;
begin
   result := inherited AddUpdateRow;
   sgList.Cells[VAR_TYPE_COL, result] := cbType.Text;
   sgList.Cells[VAR_SIZE_COL, result] := Trim(edtSize.Text);
   sgList.Cells[VAR_INIT_COL, result] := Trim(edtInit.Text);
   edtSize.Text := '1';
   edtInit.Clear;
   GProject.PopulateDataTypeCombos;
   GProject.RefreshStatements;
   UpdateCodeEditor;
end;

procedure TConstDeclareList.OnClickAdd(Sender: TObject);
var
   status, const_type: integer;
   lInfo: string;
   lEdit: TWinControl;
begin
   status := VALID_IDENT;
   const_type := GENERIC_INT_TYPE;
   if Assigned(GInfra.CurrentLang.ValidateConstId) then
      status := GInfra.CurrentLang.ValidateConstId(edtName.Text);
   if status <> VALID_IDENT then
   else if IsDeclared(edtName.Text, true) then
      status := DUPLICATED_IDENT
   else
   begin
      if GSettings.ValidateDeclaration and Assigned(GInfra.CurrentLang.GetLiteralType) then
         const_type := GInfra.CurrentLang.GetLiteralType(edtValue.Text);
      if const_type = UNKNOWN_TYPE then
         status := UNKNOWN_TYPE;
   end;

   if status <> VALID_IDENT then
   begin
      if status = UNKNOWN_TYPE then
      begin
         lInfo := 'BadCVal';
         lEdit := edtValue;

      end
      else
      begin
         case status of
            INCORRECT_IDENT:  lInfo := 'BadId';
            DUPLICATED_IDENT: lInfo := 'DupId';
            RESERVED_IDENT:   lInfo := i18Manager.GetFormattedString('IncorrectIdKeyword', [edtName.Text, GInfra.CurrentLang.Name]);
         end;
         lEdit := edtName;
      end;
      TInfra.ShowErrorBox(i18Manager.GetString(lInfo), errDeclare);
      lEdit.SetFocus;
   end
   else
      AddUpdateRow;
end;

function TConstDeclareList.AddUpdateRow: integer;
var
   lModifying: boolean;
begin
   lModifying := FModifying;
   result := inherited AddUpdateRow;
   sgList.Cells[CONST_VALUE_COL, result] := edtValue.Text;
   if not lModifying then
   begin
      sgList.Objects[CONST_CHBOX_COL, result] := CreateCheckBox(CONST_CHBOX_COL, result);
      RefreshChBoxes;
   end;
   edtValue.Clear;
   GProject.RefreshStatements;
   GProject.RefreshSizeEdits;
   UpdateCodeEditor;
end;

function TDeclareList.AddUpdateRow: integer;
begin
   if FModifying then
   begin
      result := sgList.Row;
      sgList.Enabled := true;
      btnChange.Enabled := true;
      btnRemove.Enabled := true;
      FModifying := false;
   end
   else
   begin
      sgList.RowCount := sgList.RowCount + 1;
      result := sgList.RowCount - 2;
   end;
   sgList.Cells[NAME_COL, result] := edtName.Text;
   edtName.Clear;
   edtName.SetFocus;
   GChange := 1;
end;

procedure TDeclareList.OnClickRemove(Sender: TObject);
var
   i: integer;
begin
   with sgList do
   begin
      for i := Row to RowCount-2 do
         Rows[i].Assign(Rows[i+1]);
      RowCount := RowCount - 1;
      if (Row = RowCount-1) and (Row <> 1) then
         Row := Row - 1;
      if RowCount = 2 then
      begin
         btnChange.Enabled := False;
         btnRemove.Enabled := False;
      end;
   end;
   GChange := 1;
   edtName.SetFocus;
   GProject.RefreshStatements;
   UpdateCodeEditor; 
end;

procedure TVarDeclareList.OnClickRemove(Sender: TObject);
begin
   inherited OnClickRemove(Sender);
   GProject.PopulateDataTypeCombos;
end;

procedure TConstDeclareList.OnClickRemove(Sender: TObject);
begin
   sgList.Objects[CONST_CHBOX_COL, sgList.Row].Free;
   inherited OnClickRemove(Sender);
   OnTopLeftChanged(sgList);
   GProject.RefreshSizeEdits;
end;

procedure TDeclareList.OnClickChange(Sender: TObject);
begin
   sgList.Enabled := False;
   edtName.Text := sgList.Cells[NAME_COL, sgList.Row];
   btnChange.Enabled := False;
   btnRemove.Enabled := False;
   edtName.SetFocus;
   FModifying := true;
end;

procedure TVarDeclareList.OnClickChange(Sender: TObject);
begin
   inherited OnClickChange(Sender);
   cbType.ItemIndex := TParserHelper.GetType(sgList.Cells[VAR_TYPE_COL, sgList.Row]);
   edtSize.Text := sgList.Cells[VAR_SIZE_COL, sgList.Row];
   edtInit.Text := sgList.Cells[VAR_INIT_COL, sgList.Row];
end;

procedure TConstDeclareList.OnClickChange(Sender: TObject);
begin
   inherited OnClickChange(Sender);
   edtValue.Text := sgList.Cells[CONST_VALUE_COL, sgList.Row];
end;

function TConstDeclareList.GetValue(const AIdent: string): string;
var
   i: integer;
begin
   result := '';
   i := sgList.Cols[CONST_NAME_COL].IndexOf(AIdent);
   if i > 0 then
      result := sgList.Cells[CONST_VALUE_COL, i];
end;

function TDeclareList.IsDeclared(const AName: string; const AssociatedListCheck: boolean): boolean;
var
   i: integer;
begin
   i := sgList.Cols[NAME_COL].IndexOf(AName);
   result := (i > 0) and not (FModifying and (i = sgList.Row));
   if not result and (AssociatedList <> nil) and AssociatedListCheck then
      result := AssociatedList.IsDeclared(AName, false);
end;

procedure TDeclareList.OnKeyDownCommon(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Key = VK_RETURN then
      btnAdd.Click;
end;

procedure TVarDeclareList.FillForList(const AList: TStrings);
var
   i, lType: integer;
begin
   AList.BeginUpdate;
   for i := 1 to sgList.RowCount-2 do
   begin
      lType := TParserHelper.GetType(sgList.Cells[VAR_TYPE_COL, i]);
      if (TParserHelper.IsIntegerType(lType) or (TParserHelper.IsEnumType(lType) and  GInfra.CurrentLang.AllowEnumsInForLoop)) and (sgList.Cells[VAR_SIZE_COL, i] = '1') then
         AList.Add(sgList.Cells[VAR_NAME_COL, i]);
   end;
   AList.EndUpdate;
end;

function TVarDeclareList.IsValidLoopVar(const AName: string): boolean;
var
   i, lType: integer;
begin
   result := false;
   i := sgList.Cols[VAR_NAME_COL].IndexOf(AName);
   if i > 0 then
   begin
      lType := TParserHelper.GetType(sgList.Cells[VAR_TYPE_COL, i]);
      result := (TParserHelper.IsIntegerType(lType) or (TParserHelper.IsEnumType(lType) and  GInfra.CurrentLang.AllowEnumsInForLoop)) and (sgList.Cells[VAR_SIZE_COL, i] = '1');
   end;
end;

procedure TVarDeclareList.ExportToXMLTag(const rootTag: IXMLElement);
var
   tag: IXMLElement;
   i: integer;
begin
   for i := 1 to sgList.RowCount-2 do
   begin
      tag := rootTag.OwnerDocument.CreateElement('var');
      tag.SetAttribute('name', sgList.Cells[VAR_NAME_COL, i]);
      tag.SetAttribute('type', sgList.Cells[VAR_TYPE_COL, i]);
      tag.SetAttribute('size', sgList.Cells[VAR_SIZE_COL, i]);
      tag.SetAttribute('init', sgList.Cells[VAR_INIT_COL, i]);
      rootTag.AppendChild(tag);
   end;
   inherited ExportToXMLTag(rootTag);
end;

procedure TVarDeclareList.ImportFromXMLTag(const rootTag: IXMLElement);
var
   tag: IXMLElement;
   lType: string;
begin
   TInfra.PopulateDataTypeCombo(cbType);
   tag := TXMLProcessor.FindChildTag(rootTag, 'var');
   while tag <> nil do
   begin
      sgList.Cells[VAR_NAME_COL, sgList.RowCount-1] := tag.GetAttribute('name');
      lType := tag.GetAttribute('type');
      if cbType.Items.IndexOf(lType) = -1 then
         lType := i18Manager.GetString('Unknown');
      sgList.Cells[VAR_TYPE_COL, sgList.RowCount-1] := lType;
      sgList.Cells[VAR_SIZE_COL, sgList.RowCount-1] := tag.GetAttribute('size');
      sgList.Cells[VAR_INIT_COL, sgList.RowCount-1] := tag.GetAttribute('init');
      sgList.RowCount := sgList.RowCount + 1;
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   inherited ImportFromXMLTag(rootTag);
end;

procedure TConstDeclareList.ExportToXMLTag(const rootTag: IXMLElement);
var
   tag: IXMLElement;
   i: integer;
begin
   for i := 1 to sgList.RowCount-2 do
   begin
      tag := rootTag.OwnerDocument.CreateElement('const');
      tag.SetAttribute('name', sgList.Cells[CONST_NAME_COL, i]);
      tag.SetAttribute('value', sgList.Cells[CONST_VALUE_COL, i]);
      tag.SetAttribute('extern', BoolToStr(IsExternal(i), true));
      rootTag.AppendChild(tag);
   end;
   inherited ExportToXMLTag(rootTag);
end;

procedure TConstDeclareList.ImportFromXMLTag(const rootTag: IXMLElement);
var
   tag: IXMLElement;
   lRowIndex: integer;
   lchkExtern: TCheckBox;
begin
   tag := TXMLProcessor.FindChildTag(rootTag, 'const');
   while tag <> nil do
   begin
      lRowIndex := sgList.RowCount - 1;
      sgList.Cells[CONST_NAME_COL, lRowIndex] := tag.GetAttribute('name');
      sgList.Cells[CONST_VALUE_COL, lRowIndex] := tag.GetAttribute('value');
      lchkExtern := CreateCheckBox(CONST_CHBOX_COL, lRowIndex);
      lchkExtern.Checked := tag.GetAttribute('extern') = 'True';
      sgList.Objects[CONST_CHBOX_COL, lRowIndex] := lchkExtern;
      sgList.RowCount := lRowIndex + 2;
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   RefreshChBoxes;
   inherited ImportFromXMLTag(rootTag);
end;

procedure TDeclareList.ImportFromXMLTag(const rootTag: IXMLElement);
begin
   FId := GProject.Register(Self, StrToIntDef(rootTag.GetAttribute(ID_ATTR), ID_INVALID));
end;

procedure TDeclareList.ExportToXMLTag(const rootTag: IXMLElement);
begin
   rootTag.SetAttribute(ID_ATTR, IntToStr(FId));
end;

function TConstDeclareList.IsExternal(const ARow: integer): boolean;
begin
   result := false;
   if (ARow > 0) and (ARow < sgList.RowCount-1) then
      result := (sgList.Objects[CONST_CHBOX_COL, ARow] is TCheckBox) and TCheckBox(sgList.Objects[CONST_CHBOX_COL, ARow]).Checked;
end;

procedure TConstDeclareList.OnClickChBox(Sender: TObject);
begin
   UpdateCodeEditor;
end;

function TConstDeclareList.GetCheckBoxPoint(const ACol, ARow: integer): TPoint;
begin
   result := sgList.CellRect(ACol, ARow).TopLeft;
   if result.X = 0 then
      result.X := sgList.Width;
   if result.Y = 0 then
      result.Y := ARow * sgList.DefaultRowHeight + ARow;
   result.X := result.X + sgList.Left + (sgList.ColWidths[ACol] div 2) - 4;
   result.Y := result.Y + sgList.Top + 5;
end;

function TConstDeclareList.CreateCheckBox(const ACol, ARow: integer): TCheckBox;
var
   lPoint: TPoint;
begin
   lPoint := GetCheckBoxPoint(ACol, ARow);
   result := TCheckBox.Create(Self);
   result.Parent := Self;
   result.Visible := IsRowVisible(ARow) and (lPoint.X <= GetRightMargin);
   result.SetBounds(lPoint.X, lPoint.Y, 10, 10);
   result.OnClick := OnClickChBox;
   Repaint;
end;

procedure TStringGridEx.ColWidthsChanged;
begin
   inherited;
   if Assigned(OnColWidthsChanged) then
      OnColWidthsChanged(Self);
end;

procedure TStringGridEx.TopLeftChanged;
begin
   inherited;
   if Assigned(OnTopLeftChanged) then
      OnTopLeftChanged(Self);
end;

procedure TConstDeclareList.OnColWidthsChanged(Sender: TObject);
var
   i, xPos: integer;
   lControl: TControl;
begin
   xPos := GetCheckBoxPoint(CONST_CHBOX_COL, 0).X;
   for i := 1 to sgList.RowCount-2 do
   begin
      if sgList.Objects[CONST_CHBOX_COL, i] is TControl then
      begin
         lControl := TControl(sgList.Objects[CONST_CHBOX_COL, i]);
         lControl.Left := xPos;
         lControl.Visible := IsRowVisible(i) and (lControl.Left <= GetRightMargin(lControl));
      end;
   end;
end;

procedure TConstDeclareList.OnTopLeftChanged(Sender: TObject);
var
   i: integer;
   lControl: TControl;
   lPoint: TPoint;
begin
   for i := 1 to sgList.RowCount-2 do
   begin
      if sgList.Objects[CONST_CHBOX_COL, i] is TControl then
      begin
         lPoint := GetCheckBoxPoint(CONST_CHBOX_COL, i);
         lControl := TControl(sgList.Objects[CONST_CHBOX_COL, i]);
         lControl.SetBounds(lPoint.X, lPoint.Y, lControl.Width, lControl.Height);
      end;
   end;
   RefreshChBoxes;
end;

procedure TConstDeclareList.RefreshChBoxes;
var
   i: integer;
   lControl: TControl;
begin
   for i := 1 to sgList.RowCount-2 do
   begin
      if sgList.Objects[CONST_CHBOX_COL, i] is TControl then
      begin
         lControl := TControl(sgList.Objects[CONST_CHBOX_COL, i]);
         lControl.Visible := IsRowVisible(i) and (lControl.Left <= GetRightMargin(lControl));
      end;
   end;
end;

function TConstDeclareList.GetRightMargin(const AControl: TControl = nil): integer;
begin
   result := Width - 6;
   if AControl <> nil then
      result := result - AControl.Width
   else
      result := result - 10;
   if (GetWindowlong(sgList.Handle, GWL_STYLE) and WS_VSCROLL) <> 0 then
      result := result - 17;
end;

function TDeclareList.IsRowVisible(const ARow: integer): boolean;
begin
   result := (ARow >= sgList.TopRow) and (ARow < sgList.TopRow+sgList.VisibleRowCount);
end;

function TDeclareList.GetFocusColor: TColor;
begin
   result := OK_COLOR;
end;

function TDeclareList.Remove: boolean;
begin
   result := CanBeRemoved;
end;

function TDeclareLIst.CanBeRemoved: boolean;
begin
   result := false;
end;

function TDeclareList.IsBoldDesc: boolean;
begin
   result := true;
end;

end.
