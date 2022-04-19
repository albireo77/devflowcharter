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
   Vcl.Controls, OmniXML, Vcl.StdCtrls, Vcl.Grids, System.Classes, System.Types,
   Vcl.Graphics, Vcl.Forms, Vcl.ExtCtrls, SizeEdit, Interfaces, Types;

type

    TStringGridEx = class(TStringGrid)
       private
          FColWidthsChanged: TNotifyEvent;
          FTopLeftChanged: TNotifyEvent;
       protected
          procedure TopLeftChanged; override;
       public
          function GetMinWidth: integer;
          procedure ColWidthsChanged; override;
       published
          property OnColWidthsChanged: TNotifyEvent read FColWidthsChanged write FColWidthsChanged;
          property OnTopLeftChanged: TNotifyEvent read FTopLeftChanged write FTopLeftChanged;
    end;

   TDeclareList = class(TGroupBox, IWithFocus, IWithId)
      protected
         FModifying: boolean;
         FId,
         FDragRow,
         FExternalCol: integer;
         FKind: string;
         FSplitter: TSplitter;
         function GetId: integer;
         function IsDeclared(const AName: string; AssociatedListCheck: boolean): boolean;
         function AddUpdateRow: integer; virtual;
         function IsRowVisible(ARow: integer): boolean;
         function IsControlTooRight(AControl: TWinControl): boolean;
         function FindRow(x, y: integer): integer;
         procedure OnRowMovedList(Sender: TObject; FromIndex, ToIndex: Longint);
         procedure OnClickAdd(Sender: TObject); virtual; abstract;
         procedure OnClickImport(Sender: TObject);
         procedure OnClickExport(Sender: TObject);
         procedure OnClickRemove(Sender: TObject); virtual;
         procedure OnClickChange(Sender: TObject); virtual;
         procedure OnDblClickList(Sender: TObject);
         procedure OnSelectCellList(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
         procedure OnMouseDownList(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
         procedure OnDragDropList(Sender, Source: TObject; X, Y: Integer);
         procedure OnDragOverList(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
         procedure OnKeyDownCommon(Sender: TObject; var Key: Word; Shift: TShiftState);
         function GetExternalCheckBoxPoint(ARow: integer): TPoint;
         procedure OnTopLeftChanged(Sender: TObject);
         function CreateExternalCheckBox(ARow: integer): TCheckBox;
         procedure OnClickChBox(Sender: TObject);
         procedure RefreshCheckBoxes;
         procedure OnColWidthsChanged(Sender: TObject);
         procedure Resize; override;
         procedure OnCanResizeSplitter(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
         procedure SetColumnLabel(ACol: integer; const AColLabel: string = '');
      public
         sgList: TStringGridEx;
         btnRemove,
         btnChange,
         btnAdd,
         btnImport,
         btnExport: TButton;
         gbBox: TGroupBox;
         lblName: TLabel;
         edtName: TEdit;
         AssociatedList: TDeclareList;
         property Id: integer read GetId;
         constructor Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         destructor Destroy; override;
         function ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
         function ImportItemFromXMLTag(ATag: IXMLElement): TError; virtual;
         procedure ExportItemToXMLTag(ATag: IXMLElement; idx: integer); virtual;
         procedure ExportToXMLTag(ATag: IXMLElement);
         function GetImportTag(ATag: IXMLElement): IXMLElement; virtual;
         function RetrieveFocus(AInfo: TFocusInfo): boolean;
         function GetTreeNodeText(ANodeOffset: integer = 0): string;
         function CanBeFocused: boolean;
         function GetFocusColor: TColor;
         function Remove(ANode: TTreeNodeWithFriend = nil): boolean;
         function CanRemove: boolean;
         function IsBoldDesc: boolean;
         function IsGlobal: boolean; virtual; abstract;
         procedure SetSplitter(ASplitter: TSplitter);
         procedure SetDefaultFocus;
         function GetExternalState(ARow: integer): TCheckBoxState;
         procedure SetExternalColumn(AExternalCol: integer);
         function GetExternModifier(idx: integer): string; virtual; abstract;
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
         constructor Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         function ImportItemFromXMLTag(ATag: IXMLElement): TError; override;
         procedure ExportItemToXMLTag(ATag: IXMLElement; idx: integer); override;
         function GetImportTag(ATag: IXMLElement): IXMLElement; override;
         procedure FillForList(AList: TStrings);
         function IsValidLoopVar(const AName: string): boolean;
         function IsGlobal: boolean; override;
         function GetDimensionCount(const AVarName: string; AIncludeType: boolean = false): integer;
         function GetDimensions(const AVarName: string; AIncludeType: boolean = false): TArray<string>;
         function GetExternModifier(idx: integer): string; override;
   end;

   TConstDeclareList = class(TDeclareList)
      protected
         function AddUpdateRow: integer; override;
         procedure OnClickAdd(Sender: TObject); override;
         procedure OnClickChange(Sender: TObject); override;
         procedure OnClickRemove(Sender: TObject); override;
      public
         edtValue: TEdit;
         lblValue: TLabel;
         constructor Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
         function ImportItemFromXMLTag(ATag: IXMLElement): TError; override;
         procedure ExportItemToXMLTag(ATag: IXMLElement; idx: integer); override;
         function GetImportTag(ATag: IXMLElement): IXMLElement; override;
         function GetValue(const AIdent: string): string;
         function IsGlobal: boolean; override;
         function GetExternModifier(idx: integer): string; override;
   end;

const
   NAME_COL = 0;

   VAR_NAME_COL = NAME_COL;
   VAR_TYPE_COL = 1;
   VAR_SIZE_COL = 2;
   VAR_INIT_COL = 3;

   INVALID_ROW = -1;
   INVALID_COL = -1;

   CONST_NAME_COL  = NAME_COL;
   CONST_VALUE_COL = 1;

   DEF_VARLIST_WIDTH = 358;
   DEF_CONSTLIST_WIDTH = 235;

implementation

uses
   System.SysUtils, System.StrUtils, System.UITypes, System.Rtti, Infrastructure,
   XMLProcessor, Project, UserDataType, LangDefinition, ParserHelper, Constants;

constructor TDeclareList.Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
var
   i: integer;
begin
   FExternalCol := INVALID_COL;
   inherited Create(AParent);
   Parent := AParent;
   ParentFont := false;
   ParentBackground := false;
   Font.Style := [fsBold];
   Font.Color := clBlack;
   FModifying := false;
   AssociatedList := nil;
   DoubleBuffered := true;
   FDragRow := INVALID_ROW;
   FId := TProject.GetInstance.Register(Self);

   sgList := TStringGridEx.Create(Self);
   sgList.Parent := Self;
   sgList.DefaultRowHeight := 16;
   sgList.SetBounds(5, 16, AWidth-9, (ADispRowCount+1)*(sgList.DefaultRowHeight+2));
   sgList.RowCount := 2;
   sgList.ColCount := AColCount;
   sgList.FixedRows := 1;
   sgList.FixedCols := 0;
   sgList.DefaultColWidth := sgList.Width div AColCount;
   for i := 0 to AColCount-1 do
      SetColumnLabel(i);
   sgList.DrawingStyle := gdsClassic;
   sgList.Ctl3D := false;
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
   sgList.OnColWidthsChanged := OnColWidthsChanged;
   sgList.OnTopLeftChanged := OnTopLeftChanged;

   SetBounds(ALeft, ATop, AWidth, (ADispRowCount+1)*(sgList.DefaultRowHeight+2)+157);
   sgList.Anchors := [akTop, akBottom, akLeft, akRight];

   btnRemove := TButton.Create(Self);
   btnRemove.Parent := Self;
   btnRemove.SetBounds(4, sgList.BoundsRect.Bottom+8, (Width div 2)-5, 25);
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
   gbBox.SetBounds(5, btnChange.BoundsRect.Bottom+4, AGBoxWidth, 72);
   gbBox.ParentFont := false;
   gbBox.ParentBackground := false;
   gbBox.Font.Style := [];
   gbBox.Anchors := [akBottom, akLeft, akRight];

   lblName := TLabel.Create(gbBox);
   lblName.Parent := gbBox;
   lblName.Top := 22;
   lblName.Left := 5;
   lblName.Caption := i18Manager.GetString('sgVarListCol0');

   edtName := TEdit.Create(gbBox);
   edtName.Parent := gbBox;
   edtName.OnKeyDown := OnKeyDownCommon;

   btnAdd := TButton.Create(Self);
   btnAdd.Parent := Self;
   btnAdd.SetBounds(4, gbBox.BoundsRect.Bottom+3, (Width-11) div 3, 25);
   btnAdd.OnClick := OnClickAdd;
   btnAdd.ParentFont := false;
   btnAdd.Font.Style := [];
   btnAdd.Anchors := [akLeft, akBottom, akRight];
   btnAdd.Caption := i18Manager.GetString('btnAdd');

   btnImport := TButton.Create(Self);
   btnImport.Parent := Self;
   btnImport.SetBounds(btnAdd.BoundsRect.Right+1, btnAdd.Top, btnAdd.Width, 25);
   btnImport.OnClick := OnClickImport;
   btnImport.ParentFont := false;
   btnImport.Font.Style := [];
   btnImport.Anchors := [akLeft, akBottom, akRight];
   btnImport.Caption := i18Manager.GetString('btnImport');

   btnExport := TButton.Create(Self);
   btnExport.Parent := Self;
   btnExport.SetBounds(btnImport.BoundsRect.Right+1, btnAdd.Top, btnAdd.Width, 25);
   btnExport.OnClick := OnClickExport;
   btnExport.ParentFont := false;
   btnExport.Font.Style := [];
   btnExport.Anchors := [akLeft, akBottom, akRight];
   btnExport.Caption := i18Manager.GetString('btnExport');

end;

destructor TDeclareList.Destroy;
begin
   TProject.GetInstance.UnRegister(Self);
   FSplitter.Free;
   inherited Destroy;
end;

constructor TConstDeclareList.Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
begin

   FKind := 'Const';

   inherited Create(AParent, ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth);

   edtName.SetBounds(lblName.Width+10, 17, gbBox.Width-lblName.Width-18, 21);
   edtName.Anchors := edtName.Anchors + [akRight];
   
   lblValue := TLabel.Create(gbBox);
   lblValue.Parent := gbBox;
   lblValue.Top := 47;
   lblValue.Caption := i18Manager.GetString('sgConstListCol1');
   lblValue.Left := 5;

   if GInfra.CurrentLang.UpperCaseConstId then
      edtName.CharCase := ecUpperCase;

   edtValue := TEdit.Create(gbBox);
   edtValue.Parent := gbBox;
   edtValue.SetBounds(lblValue.Width+10, 42, gbBox.Width-lblValue.Width-18, 21);
   edtValue.Anchors := edtValue.Anchors + [akRight];
   edtValue.ShowHint := true;
   edtValue.Hint := i18Manager.GetString('DisableFieldValid');
   edtValue.OnKeyDown := OnKeyDownCommon;

   gbBox.Caption := i18Manager.GetString('gbConstant');
   Anchors := Anchors + [akBottom];
end;

constructor TVarDeclareList.Create(AParent: TWinControl; ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth: integer);
begin

   FKind := 'Var';

   inherited Create(AParent, ALeft, ATop, AWidth, ADispRowCount, AColCount, AGBoxWidth);

   edtName.SetBounds(lblName.Width+10, 17, 100, 21);
   
   lblType := TLabel.Create(gbBox);
   lblType.Parent := gbBox;
   lblType.Left := 5;
   lblType.Top := 47;
   lblType.Caption := i18Manager.GetString('sgVarListCol1');

   lblSize := TLabel.Create(gbBox);
   lblSize.Parent := gbBox;
   lblSize.Top := 22;
   lblSize.Caption := i18Manager.GetString('sgVarListCol2');
   lblSize.Left := lblName.Width + edtName.Width + 20;

   edtSize := TSizeEdit.Create(gbBox);
   edtSize.Parent := gbBox;
   edtSize.SetBounds(lblSize.BoundsRect.Right+5, 17, gbBox.Width - lblSize.BoundsRect.Right-13, 21);
   edtSize.Anchors := edtSize.Anchors + [akRight];
   edtSize.OnKeyDown := OnKeyDownCommon;

   cbType := TComboBox.Create(gbBox);
   cbType.Parent := gbBox;
   cbType.Style := csDropDownList;
   cbType.SetBounds(lblType.BoundsRect.Right + 5, 42, 0, 21);
   cbType.Constraints.MaxWidth := edtName.BoundsRect.Right - lblType.BoundsRect.Right - 5;
   cbType.Constraints.MinWidth := cbType.Constraints.MaxWidth;
   cbType.OnKeyDown := OnKeyDownCommon;
   TInfra.PopulateDataTypeCombo(cbType);

   lblInit := TLabel.Create(gbBox);
   lblInit.Parent := gbBox;
   lblInit.Left := cbType.BoundsRect.Right + 10;
   lblInit.Top := 47;
   lblInit.Caption := i18Manager.GetString('sgVarListCol3');

   edtInit := TEdit.Create(gbBox);
   edtInit.Parent := gbBox;
   edtInit.SetBounds(lblInit.BoundsRect.Right+5, 42, gbBox.Width-lblInit.BoundsRect.Right-13, 21);
   edtInit.Anchors := edtInit.Anchors + [akRight];
   edtInit.ShowHint := true;
   edtInit.Hint := i18Manager.GetString('DisableFieldValid');
   edtInit.OnKeyDown := OnKeyDownCommon;

   gbBox.Caption := i18Manager.GetString('gbVariable');
   Anchors := Anchors + [akBottom];
end;

procedure TDeclareList.SetColumnLabel(ACol: integer; const AColLabel: string = '');
var
   s: string;
begin
   if AColLabel.IsEmpty then
      s := i18Manager.GetString('sg' + FKind + 'ListCol' + ACol.ToString)
   else
      s := AColLabel;
   sgList.Cells[ACol, 0] := s;
end;

procedure TDeclareList.SetExternalColumn(AExternalCol: integer);
begin
   if (AExternalCol >= 0) and (AExternalCol < sgList.ColCount) then
   begin
      FExternalCol := AExternalCol;
      SetColumnLabel(FExternalCol, GInfra.CurrentLang.ExternalLabel);
   end;
end;

procedure TDeclareList.SetSplitter(ASplitter: TSplitter);
begin
   FSplitter := ASplitter;
   FSplitter.OnCanResize := OnCanResizeSplitter;
end;

procedure TDeclareList.OnCanResizeSplitter(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
   if (NewSize < sgList.BoundsRect.Right + 4) and (NewSize < sgList.GetMinWidth) then
      Accept := false;
end;

function TDeclareList.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   i: integer;
   typeName: string;
   list: TDeclareList;
   dataType: TUserDataType;
begin
   i := 0;
   list := nil;
   typeName := AInfo.SelText.Trim;
   if not typeName.IsEmpty then
   begin
      i := sgList.Cols[NAME_COL].IndexOf(typeName);
      if i > 0 then
         list := Self
      else if AssociatedList <> nil then
      begin
         i := AssociatedList.sgList.Cols[NAME_COL].IndexOf(typeName);
         if i > 0 then
            list := AssociatedList;
      end;
      if list = nil then
      begin
         dataType := GProject.GetUserDataType(typeName);
         if dataType <> nil then
         begin
            result := dataType.RetrieveFocus(AInfo);
            if result then Exit;
         end;
      end;
   end;
   if list <> nil then
      list.sgList.Row := i
   else
      list := Self;
   list.Show;
   GetParentForm(list, False).Show;
   result := i > 0;
end;

function TDeclareList.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := Caption;
end;

function TDeclareList.CanBeFocused: boolean;
begin
   result := true;
end;

function TDeclareList.GetId: integer;
begin
   result := FId;
end;

procedure TDeclareList.SetDefaultFocus;
begin
   if edtName.CanFocus and not (GetParentForm(Self, False).ActiveControl is TCustomEdit) then
      edtName.SetFocus;
end;

function TVarDeclareList.GetDimensionCount(const AVarName: string; AIncludeType: boolean = false): integer;
begin
   var i := sgList.Cols[VAR_NAME_COL].IndexOf(AVarName);
   if i < 1 then
      Exit(-1);
   result := TInfra.GetDimensionCount(sgList.Cells[VAR_SIZE_COL, i]);
   if AIncludeType and (result <> -1) then
   begin
      var dataType := GProject.GetUserDataType(sgList.Cells[VAR_TYPE_COL, i]);
      if dataType <> nil then
         result := dataType.GetDimensionCount + result;
   end;
end;

function TVarDeclareList.GetDimensions(const AVarName: string; AIncludeType: boolean = false): TArray<string>;
begin
   var i := sgList.Cols[VAR_NAME_COL].IndexOf(AVarName);
   if i < 1 then
      Exit(nil);
   var size := sgList.Cells[VAR_SIZE_COL, i];
   if AIncludeType then
   begin
      var dataType := GProject.GetUserDataType(sgList.Cells[VAR_TYPE_COL, i]);
      if dataType <> nil then
         size := size + dataType.GetDimensions;
   end;
   result := TInfra.GetDimensions(size);
end;

procedure TDeclareList.OnDblClickList(Sender: TObject);
begin
   var p := sgList.ScreenToClient(Mouse.CursorPos);
   if FindRow(p.X, p.Y) <> INVALID_ROW then
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
   TInfra.UpdateCodeEditor;
   RefreshCheckBoxes;
end;

function TDeclareList.FindRow(x, y: integer): integer;
var
   lCol, lRow: integer;
begin
   sgList.MouseToCell(x, y, lCol, lRow);
   if (lRow > 0) and (lRow < sgList.RowCount-1) then
      result := lRow
   else
      result := INVALID_ROW;
end;

procedure TDeclareList.OnDragOverList(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   Accept := (Source = sgList) and (FindRow(X, Y) <> INVALID_ROW);
end;

procedure TDeclareList.OnDragDropList(Sender, Source: TObject; X, Y: Integer);
begin
   if (Source = sgList) and (FDragRow <> INVALID_ROW) then
   begin
      var lRow := FindRow(X, Y);
      if lRow <> INVALID_ROW then
         sgList.MoveRow(FDragRow, lRow);
   end;
   FDragRow := INVALID_ROW;
end;

procedure TDeclareList.OnMouseDownList(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   edtName.SetFocus;
   if (Button = mbLeft) and (ssShift in Shift) then
   begin
      var lRow := FindRow(X, Y);
      if lRow <> INVALID_ROW then
      begin
         FDragRow := lRow;
         sgList.BeginDrag(true);
      end;
   end;
end;

procedure TVarDeclareList.OnClickAdd(Sender: TObject);
var
   status, lType: integer;
   info, initVal, secType: string;
   edit: TCustomEdit;
   dataType: TUserDataType;
begin
   status := GInfra.ValidateId(edtName.Text);
   lType := TParserHelper.GetType(cbType.Text);
   if status <> VALID_IDENT then
   else if IsDeclared(edtName.Text, true) then
      status := DUPLICATED_IDENT
   else if not edtSize.ParseSize then
      status := INCORRECT_SIZE
   else if GSettings.ValidateDeclaration and (edtSize.Text = '1') and not TParserHelper.IsRecordType(lType) and Assigned(GInfra.CurrentLang.GetConstantType) then
   begin
      initVal := Trim(edtInit.Text);
      if not initVal.isEmpty then
      begin
         if TParserHelper.IsEnumType(lType) then
         begin
            dataType := GProject.GetUserDataType(cbType.Text);
            if (dataType <> nil) and not dataType.IsValidEnumValue(initVal) then
               status := INVALID_INIT_VAL;
         end
         else if not TParserHelper.AreTypesCompatible(lType, GInfra.CurrentLang.GetConstantType(initVal, secType)) and
            not TParserHelper.AreTypesCompatible(lType, TParserHelper.GetConstType(initVal)) then status := INVALID_INIT_VAL;
      end;
   end;

   if status <> VALID_IDENT then
   begin
      edit := edtName;
      case status of
         INCORRECT_SIZE:
         begin
            edit := edtSize;
            info := 'BadSize';
         end;
         INVALID_INIT_VAL:
         begin
            edit := edtInit;
            info := 'BadInitVal';
         end;
         INCORRECT_IDENT:  info := 'BadId';
         DUPLICATED_IDENT: info := 'DupId';
         RESERVED_IDENT:   info := 'IncorrectIdKeyword';
      end;
      TInfra.ShowErrorBox(i18Manager.GetFormattedString(info, [edit.Text, GInfra.CurrentLang.Name]), errDeclare);
      edit.SetFocus;
   end
   else
      AddUpdateRow;
end;

procedure TDeclareList.OnClickImport(Sender: TObject);
begin
   if not TXMLProcessor.ImportFromXMLFile(ImportFromXMLTag, impAll).IsEmpty then
      TInfra.UpdateCodeEditor;
end;

procedure TDeclareList.OnClickExport(Sender: TObject);
begin
   if sgList.RowCount > 2 then
   begin
      var fileName := GProject.Name + '_' + i18Manager.GetString(FKind + 's');
      TXMLProcessor.ExportToXMLFile(ExportToXMLTag, fileName);
   end;
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
   TInfra.UpdateCodeEditor;
end;

procedure TConstDeclareList.OnClickAdd(Sender: TObject);
var
   status, constType: integer;
   info, secType: string;
   edit: TCustomEdit;
begin
   constType := GENERIC_INT_TYPE;
   status := GInfra.ValidateConstId(edtName.Text);
   if status <> VALID_IDENT then
   else if IsDeclared(edtName.Text, true) then
      status := DUPLICATED_IDENT
   else
   begin
      if GSettings.ValidateDeclaration and Assigned(GInfra.CurrentLang.GetConstantType) then
         constType := GInfra.CurrentLang.GetConstantType(edtValue.Text, secType);
      if constType = UNKNOWN_TYPE then
         status := UNKNOWN_TYPE;
   end;

   if status <> VALID_IDENT then
   begin
      edit := edtName;
      case status of
         INCORRECT_IDENT:  info := 'BadId';
         DUPLICATED_IDENT: info := 'DupId';
         RESERVED_IDENT:   info := 'IncorrectIdKeyword';
         UNKNOWN_TYPE:
         begin
            info := 'BadCVal';
            edit := edtValue;
         end;
      end;
      TInfra.ShowErrorBox(i18Manager.GetFormattedString(info, [edit.Text, GInfra.CurrentLang.Name]), errDeclare);
      edit.SetFocus;
   end
   else
      AddUpdateRow;
end;

function TConstDeclareList.AddUpdateRow: integer;
begin
   result := inherited AddUpdateRow;
   sgList.Cells[CONST_VALUE_COL, result] := edtValue.Text;
   edtValue.Clear;
   GProject.RefreshStatements;
   GProject.RefreshSizeEdits;
   TInfra.UpdateCodeEditor;
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
      CreateExternalCheckBox(result);
      RefreshCheckBoxes;
   end;
   sgList.Cells[NAME_COL, result] := edtName.Text;
   edtName.Clear;
   edtName.SetFocus;
   GProject.SetChanged;
end;

procedure TDeclareList.OnClickRemove(Sender: TObject);
begin
   with sgList do
   begin
      if FExternalCol <> INVALID_COL then
         Objects[FExternalCol, Row].Free;
      for var i := Row to RowCount-2 do
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
   OnTopLeftChanged(sgList);
   GProject.SetChanged;
   edtName.SetFocus;
   GProject.RefreshStatements;
   TInfra.UpdateCodeEditor;
end;

procedure TVarDeclareList.OnClickRemove(Sender: TObject);
begin
   inherited OnClickRemove(Sender);
   GProject.PopulateDataTypeCombos;
end;

procedure TConstDeclareList.OnClickRemove(Sender: TObject);
begin
   inherited OnClickRemove(Sender);
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
begin
   result := '';
   var i := sgList.Cols[CONST_NAME_COL].IndexOf(AIdent);
   if i > 0 then
      result := sgList.Cells[CONST_VALUE_COL, i];
end;

function TDeclareList.IsDeclared(const AName: string; AssociatedListCheck: boolean): boolean;
begin
   var i := sgList.Cols[NAME_COL].IndexOf(AName);
   result := (i > 0) and not (FModifying and (i = sgList.Row));
   if (AssociatedList <> nil) and AssociatedListCheck and not result then
      result := AssociatedList.IsDeclared(AName, false);
end;

procedure TDeclareList.OnKeyDownCommon(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Key = vkReturn then
      btnAdd.Click;
end;

procedure TDeclareList.Resize;
begin
   inherited;
   if btnAdd <> nil then
   begin
      RefreshCheckBoxes;
      btnAdd.Width := gbBox.Width div 3;
      btnImport.SetBounds(btnAdd.BoundsRect.Right+1, btnImport.Top, btnAdd.Width, btnImport.Height);
      btnExport.SetBounds(btnImport.BoundsRect.Right+1, btnExport.Top, btnAdd.Width, btnExport.Height);
      btnRemove.Width := sgList.Width div 2;
      btnChange.SetBounds(btnRemove.BoundsRect.Right+1, btnChange.Top, btnRemove.Width, btnChange.Height);
   end;
end;

procedure TVarDeclareList.FillForList(AList: TStrings);
begin
   AList.BeginUpdate;
   for var i := 1 to sgList.RowCount-2 do
   begin
      var lType := TParserHelper.GetType(sgList.Cells[VAR_TYPE_COL, i]);
      if (TParserHelper.IsIntegerType(lType) or (TParserHelper.IsEnumType(lType) and  GInfra.CurrentLang.AllowEnumsInForLoop)) and (sgList.Cells[VAR_SIZE_COL, i] = '1') then
         AList.Add(sgList.Cells[VAR_NAME_COL, i]);
   end;
   AList.EndUpdate;
end;

function TVarDeclareList.IsValidLoopVar(const AName: string): boolean;
begin
   result := false;
   var i := sgList.Cols[VAR_NAME_COL].IndexOf(AName);
   if i > 0 then
   begin
      var lType := TParserHelper.GetType(sgList.Cells[VAR_TYPE_COL, i]);
      result := (TParserHelper.IsIntegerType(lType) or (TParserHelper.IsEnumType(lType) and  GInfra.CurrentLang.AllowEnumsInForLoop)) and (sgList.Cells[VAR_SIZE_COL, i] = '1');
   end;
end;

function TDeclareList.GetImportTag(ATag: IXMLElement): IXMLElement;
begin
   result := ATag;
end;

function TVarDeclareList.GetImportTag(ATag: IXMLElement): IXMLElement;
begin
   result := TXMLProcessor.FindChildTag(ATag, VAR_TAG);
   if result = nil then
   begin
      result := TXMLProcessor.FindChildTag(ATag, FUNCTION_TAG);
      if result <> nil then
      begin
         result := TXMLProcessor.FindChildTag(result, HEADER_TAG);
         if result <> nil then
            result := TXMLProcessor.FindChildTag(result, VAR_TAG);
      end;
   end;
   if result <> nil then
      TInfra.PopulateDataTypeCombo(cbType);
end;

function TConstDeclareList.GetImportTag(ATag: IXMLElement): IXMLElement;
begin
   result := TXMLProcessor.FindChildTag(ATag, CONST_TAG);
end;

function TDeclareList.ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   tag: IXMLElement;
   i: integer;
begin
   if ATag <> nil then
      FId := GProject.Register(Self, StrToIntDef(ATag.GetAttribute(ID_ATTR), ID_INVALID));
   if goColSizing in sgList.Options then
   begin
      i := 0;
      tag := TXMLProcessor.FindChildTag(ATag, FKind + 'colwidth');
      while (tag <> nil) and (i < sgList.ColCount) do
      begin
         sgList.ColWidths[i] := StrToIntDef(tag.Text, sgList.ColWidths[i]);
         tag := TXMLProcessor.FindNextTag(tag);
         i := i + 1;
      end;
   end;
   tag := TXMLProcessor.FindChildTag(ATag, FKind + 'width');
   if tag <> nil then
      Width := StrToIntDef(tag.Text, Width);
   tag := GetImportTag(ATag);
   while tag <> nil do
   begin
      ImportItemFromXMLTag(tag);
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   RefreshCheckBoxes;
   result := errNone;
end;

function TDeclareList.ImportItemFromXMLTag(ATag: IXMLElement): TError;
begin
   result := errValidate;
   var lName := ATag.GetAttribute(NAME_ATTR).Trim;
   if (not lName.IsEmpty) and (sgList.Cols[NAME_COL].IndexOf(lName) < 1) then
   begin
      var idx := sgList.RowCount - 1;
      sgList.Cells[NAME_COL, idx] := lName;
      var box := CreateExternalCheckBox(idx);
      if box <> nil then
         box.State := TInfra.DecodeCheckBoxState(ATag.GetAttribute(EXTERN_ATTR));
      result := errNone;
   end;
end;

function TConstDeclareList.ImportItemFromXMLTag(ATag: IXMLElement): TError;
begin
   result := inherited ImportItemFromXMLTag(ATag);
   if result = errNone then
   begin
      var idx := sgList.RowCount - 1;
      sgList.Cells[CONST_VALUE_COL, idx] := ATag.GetAttribute(VALUE_ATTR);
      sgList.RowCount := idx + 2;
   end;
end;

function TVarDeclareList.ImportItemFromXMLTag(ATag: IXMLElement): TError;
begin
   result := inherited ImportItemFromXMLTag(ATag);
   if result = errNone then
   begin
      var idx := sgList.RowCount - 1;
      var lType := ATag.GetAttribute(TYPE_ATTR);
      if cbType.Items.IndexOf(lType) = -1 then
         lType := i18Manager.GetString('Unknown');
      sgList.Cells[VAR_TYPE_COL, idx] := lType;
      sgList.Cells[VAR_SIZE_COL, idx] := ATag.GetAttribute(SIZE_ATTR);
      sgList.Cells[VAR_INIT_COL, idx] := ATag.GetAttribute(INIT_ATTR);
      sgList.RowCount := idx + 2;
   end;
end;

procedure TDeclareList.ExportToXMLTag(ATag: IXMLElement);
var
   i: integer;
   tag: IXMLElement;
begin
   for i := 1 to sgList.RowCount-2 do
      ExportItemToXMLTag(ATag, i);
   if goColSizing in sgList.Options then
   begin
      for i := 0 to sgList.ColCount-1 do
      begin
         tag := ATag.OwnerDocument.CreateElement(FKind + 'colwidth');
         tag.Text := sgList.ColWidths[i].ToString;
         ATag.AppendChild(tag);
      end;
   end;
   tag := ATag.OwnerDocument.CreateElement(FKind + 'width');
   tag.Text := Width.ToString;
   ATag.AppendChild(tag);
end;

procedure TDeclareList.ExportItemToXMLTag(ATag: IXMLElement; idx: integer);
begin
   ATag.SetAttribute(NAME_ATTR, sgList.Cells[NAME_COL, idx]);
   ATag.SetAttribute(EXTERN_ATTR, TRttiEnumerationType.GetName(GetExternalState(idx)));
end;

procedure TVarDeclareList.ExportItemToXMLTag(ATag: IXMLElement; idx: integer);
begin
   var tag := ATag.OwnerDocument.CreateElement(VAR_TAG);
   ATag.AppendChild(tag);
   tag.SetAttribute(TYPE_ATTR, sgList.Cells[VAR_TYPE_COL, idx]);
   tag.SetAttribute(SIZE_ATTR, sgList.Cells[VAR_SIZE_COL, idx]);
   tag.SetAttribute(INIT_ATTR, sgList.Cells[VAR_INIT_COL, idx]);
   inherited ExportItemToXMLTag(tag, idx);
end;

procedure TConstDeclareList.ExportItemToXMLTag(ATag: IXMLElement; idx: integer);
begin
   var tag := ATag.OwnerDocument.CreateElement(CONST_TAG);
   ATag.AppendChild(tag);
   tag.SetAttribute(VALUE_ATTR, sgList.Cells[CONST_VALUE_COL, idx]);
   inherited ExportItemToXMLTag(tag, idx);
end;

function TVarDeclareList.IsGlobal: boolean;
begin
   result := (GProject <> nil) and (GProject.GlobalVars = Self);
end;

function TConstDeclareList.IsGlobal: boolean;
begin
   result := (GProject <> nil) and (GProject.GlobalConsts = Self);
end;

function TDeclareList.GetExternalState(ARow: integer): TCheckBoxState;
begin
   result := cbUnchecked;
   if (ARow > 0) and (ARow < sgList.RowCount-1) and (FExternalCol <> INVALID_COL) and (sgList.Objects[FExternalCol, ARow] is TCheckBox) then
      result := TCheckBox(sgList.Objects[FExternalCol, ARow]).State;
end;

function TVarDeclareList.GetExternModifier(idx: integer): string;
begin
   var lang := GInfra.CurrentLang;
   case GetExternalState(idx) of
      cbChecked:   result := lang.VarExtern;
      cbUnchecked: result := lang.VarNotExtern;
      cbGrayed:    result := lang.VarTransExtern;
   end;
end;

function TConstDeclareList.GetExternModifier(idx: integer): string;
begin
   var lang := GInfra.CurrentLang;
   case GetExternalState(idx) of
      cbChecked:   result := lang.ConstExtern;
      cbUnchecked: result := lang.ConstNotExtern;
      cbGrayed:    result := lang.ConstTransExtern;
   end;
end;

procedure TDeclareList.OnClickChBox(Sender: TObject);
begin
   TInfra.UpdateCodeEditor;
end;

function TDeclareList.GetExternalCheckBoxPoint(ARow: integer): TPoint;
begin
   result := sgList.CellRect(FExternalCol, ARow).TopLeft;
   if result.X = 0 then
      result.X := sgList.ClientWidth;
   if result.Y = 0 then
      result.Y := ARow * (sgList.DefaultRowHeight + sgList.GridLineWidth);
   var s5 := TInfra.Scaled(5);
   result.X := result.X + sgList.Left + (sgList.ColWidths[FExternalCol] div 2) - s5;
   result.Y := result.Y + sgList.Top + (sgList.DefaultRowHeight div 2) - s5;
end;

function TDeclareList.CreateExternalCheckBox(ARow: integer): TCheckBox;
begin
   result := nil;
   if FExternalCol <> INVALID_COL then
   begin
      var s12 := TInfra.Scaled(12);
      var pnt := GetExternalCheckBoxPoint(ARow);
      result := TCheckBox.Create(sgList.Parent);
      result.Parent := sgList.Parent;
      sgList.Objects[FExternalCol, ARow] := result;
      result.AllowGrayed := GInfra.CurrentLang.AllowTransExternVarConst;
      result.SetBounds(pnt.X, pnt.Y, s12, s12);
      result.Visible := IsRowVisible(ARow) and not IsControlTooRight(result);
      result.OnClick := OnClickChBox;
      result.Repaint;
   end;
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

function TStringGridEx.GetMinWidth: integer;
begin
   result := Left + 3;
   for var i := 0 to ColCount-1 do
      result := result + ColWidths[i] + GridLineWidth;
end;

procedure TDeclareList.RefreshCheckBoxes;
begin
   if FExternalCol <> INVALID_COL then
   begin
      for var i := 1 to sgList.RowCount-2 do
      begin
         var obj := sgList.Objects[FExternalCol, i];
         if obj is TWinControl then
         begin
            var winControl := TWinControl(obj);
            TInfra.MoveWin(winControl, GetExternalCheckBoxPoint(i));
            winControl.Visible := IsRowVisible(i) and not IsControlTooRight(winControl);
         end;
      end;
   end;
end;

procedure TDeclareList.OnColWidthsChanged(Sender: TObject);
begin
   RefreshCheckBoxes;
end;

procedure TDeclareList.OnTopLeftChanged(Sender: TObject);
begin
   RefreshCheckBoxes;
end;

function TDeclareList.IsRowVisible(ARow: integer): boolean;
begin
   result := (ARow >= sgList.TopRow) and (ARow < sgList.TopRow + sgList.VisibleRowCount);
end;

function TDeclareList.IsControlTooRight(AControl: TWinControl): boolean;
begin
   result := AControl.BoundsRect.Right > sgList.ClientWidth + sgList.Left + 1;
end;

function TDeclareList.GetFocusColor: TColor;
begin
   result := OK_COLOR;
end;

function TDeclareList.Remove(ANode: TTreeNodeWithFriend = nil): boolean;
begin
   result := CanRemove;
end;

function TDeclareLIst.CanRemove: boolean;
begin
   result := false;
end;

function TDeclareList.IsBoldDesc: boolean;
begin
   result := true;
end;

end.
