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



unit UserDataType;

interface

uses
   Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, System.Classes, Vcl.ExtCtrls, OmniXML,
   SizeEdit, TabComponent, Element, DataTypes_Form, Interfaces, Types;

type

   TUserDataType = class;

   TField = class(TElement)
      constructor Create(AParentTab: TUserDataType);
   protected
      procedure OnChangeSize(Sender: TObject);
      procedure OnChangeName(Sender: TObject); override;
   public
      edtSize: TSizeEdit;
      function ExportToXML(ANode: IXMLNode): IXMLNode; override;
      procedure ImportFromXML(ANode: IXMLNode); override;
      function IsValid: boolean; override;
   end;

   TUserDataType = class(TTabComponent)
   protected
      procedure OnChangeName(Sender: TObject); override;
      procedure OnClickType(Sender: TObject);
      procedure SetActive(AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure AddElement(Sender: TObject); override;
      procedure Resize; override;
   public
      chkAddPtrType: TCheckBox;
      rgTypeBox: TRadioGroup;
      lblName2,
      lblType,
      lblSize: TLabel;
      property FieldCount: integer read GetElementCount default 0;
      constructor Create(AParentForm: TDataTypesForm);
      procedure ExportToXML(ANode: IXMLNode); override;
      procedure ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
      procedure RefreshSizeEdits; override;
      procedure ExportCode(ALines: TStringList); override;
      function IsValidEnumValue(const AValue: string): boolean;
      function GetDimensionCount: integer;
      function GetDimensions: string;
      function GetOriginalType: integer;
      procedure GenerateTree(ANode: TTreeNode);
      function Kind: TUserDataTypeKind;
      function GetFields: IEnumerable<TField>;
      function GetFirstField: TField;
      function GetExternModifier: string; override;
      function GetTreeNodeText(ANodeOffset: integer = 0): string; override;
   end;

implementation

uses
   Vcl.Forms, Vcl.Graphics, System.SysUtils, System.StrUtils, System.Rtti, System.Math,
   Generics.Defaults, Infrastructure, ParserHelper, OmniXMLUtils, Constants;

var
   ByTopFieldComparer: IComparer<TField>;

constructor TUserDataType.Create(AParentForm: TDataTypesForm);
begin

   FElementTypeId := 'field';
   FCodeIncludeExtern := GInfra.CurrentLang.CodeIncludeExternDataType;

   inherited Create(AParentForm);

   CreateNameControls(Self, 9, 10);

   lblName2 := TLabel.Create(Self);
   lblName2.Parent := Self;
   lblName2.ParentFont := False;
   lblName2.Font.Style := [fsBold];
   lblName2.Font.Color := clWindowText;
   lblName2.SetBounds(5, 131, 0, 13);
   lblName2.Caption := i18Manager.GetString('lblField');

   lblSize := TLabel.Create(Self);
   lblSize.Parent := Self;
   lblSize.ParentFont := False;
   lblSize.Font.Style := [fsBold];
   lblSize.Font.Color := clWindowText;
   lblSize.SetBounds(TInfra.Scaled(Self, 171), 131, 0, 13);
   lblSize.Caption := i18Manager.GetString('lblSize');

   lblType := TLabel.Create(Self);
   lblType.Parent := Self;
   lblType.ParentFont := False;
   lblType.Font.Style := [fsBold];
   lblType.Font.Color := clWindowText;
   lblType.SetBounds(TInfra.Scaled(Self, 87), 131, 0, 13);
   lblType.Caption := i18Manager.GetString('lblType');

   sbxElements := TScrollBox.Create(Self);
   sbxElements.Parent := Self;
   sbxElements.StyleElements := sbxElements.StyleElements - [seClient];
   sbxElements.Ctl3D := False;
   sbxElements.BorderStyle := bsNone;
   sbxElements.SetBounds(0, 149, TInfra.Scaled(Self, 302), 0);
   sbxElements.Constraints.MaxHeight := Height - 148;
   sbxElements.Constraints.MinWidth := sbxElements.Width;
   sbxElements.VertScrollBar.Tracking := True;
   sbxElements.UseWheelForScrolling := True;
   sbxElements.Anchors := [akTop, akBottom, akLeft, akRight];

   btnAddElement := TButton.Create(Self);
   btnAddElement.Parent := Self;
   btnAddElement.ParentFont := False;
   btnAddElement.Font.Style := [];
   btnAddElement.Caption := i18Manager.GetString('btnAddField');
   btnAddElement.ShowHint := True;
   btnAddElement.SetBounds(1, 102, TInfra.Scaled(Self, 306), 25);
   btnAddElement.OnClick := AddElement;

   CreateLibControls(Self, edtName.Left+edtName.Width+7, 10);

   rgTypeBox := TRadioGroup.Create(Self);
   rgTypeBox.Parent := Self;
   rgTypeBox.ParentFont := False;
   rgTypeBox.ParentBackground := False;
   rgTypeBox.Font.Style := [];
   rgTypeBox.Font.Color := clWindowText;
   rgTypeBox.Columns := 2;
   rgTypeBox.Caption := i18Manager.GetString('rgTypeBox');
   for var dt := Low(TUserDataTypeKind) to High(TUserDataTypeKind) do
   begin
      var s := TRttiEnumerationType.GetName(dt);
      rgTypeBox.Items.Add(i18Manager.GetString(s));
   end;
   rgTypeBox.Buttons[Ord(dtInt)].Enabled := GInfra.CurrentLang.EnabledUserDataTypeInt;
   rgTypeBox.Buttons[Ord(dtReal)].Enabled := GInfra.CurrentLang.EnabledUserDataTypeReal;
   rgTypeBox.Buttons[Ord(dtOther)].Enabled := GInfra.CurrentLang.EnabledUserDataTypeOther;
   rgTypeBox.Buttons[Ord(dtArray)].Enabled := GInfra.CurrentLang.EnabledUserDataTypeArray;
   rgTypeBox.Buttons[Ord(dtEnum)].Enabled := GInfra.CurrentLang.EnabledUserDataTypeEnum;
   if not GInfra.CurrentLang.RecordLabel.IsEmpty then
      rgTypeBox.Items[Ord(dtRecord)] := GInfra.CurrentLang.RecordLabel;
   rgTypeBox.ItemIndex := Ord(dtRecord);
   rgTypeBox.OnClick := OnClickType;
   rgTypeBox.SetBounds(1, 28, rgTypeBox.Buttons[Ord(dtEnum)].BoundsRect.Right-9, 73);

   chkAddPtrType := TCheckBox.Create(Self);
   chkAddPtrType.Parent := Self;
   chkAddPtrType.Caption := i18Manager.GetString('chkAddPtrType');
   chkAddPtrType.ParentFont := False;
   chkAddPtrType.Font.Style := [];
   chkAddPtrType.Font.Color := clWindowText;
   chkAddPtrType.SetBounds(rgTypeBox.BoundsRect.Right+6, 42, TInfra.GetAutoWidth(chkAddPtrType), 17);
   chkAddPtrType.Enabled := GInfra.CurrentLang.EnabledPointers;
   chkAddPtrType.OnClick := OnClickCh;

   CreateExtDeclareChBox(Self, chkAddPtrType.Left, 60);
   chkExternal.AllowGrayed := GInfra.CurrentLang.AllowTransExternDataType;

   GProject.AddComponent(Self);
end;

function TUserDataType.Kind: TUserDataTypeKind;
begin
   result := TUserDataTypeKind(rgTypeBox.ItemIndex);
end;

procedure TUserDataType.SetActive(AValue: boolean);
begin
   if AValue <> FActive then
   begin
      inherited SetActive(AValue);
      ParentForm.FormDeactivate(ParentForm);
      ParentForm.RefreshTabs;
   end;
end;

procedure TUserDataType.AddElement(Sender: TObject);
begin
   if (Kind in [dtOther, dtArray]) and (GetFirstField = nil) then
      btnAddElement.Enabled := False;
   inherited AddElement(Sender);
end;

procedure TUserDataType.RefreshSizeEdits;
begin
   ParentForm.UpdateCodeEditor := False;
   for var field in GetFields do
   begin
      if field.edtSize.Text <> '1' then
         field.edtSize.Change;
   end;
   ParentForm.UpdateCodeEditor := True;
end;

function TUserDataType.GetExternModifier: string;
begin
   var lang := GInfra.CurrentLang;
   case chkExternal.State of
      cbChecked:   result := lang.DataTypeExternal;
      cbUnchecked: result := lang.DataTypeNotExternal;
      cbGrayed:    result := lang.DataTypeTransExternal;
   end;
end;

procedure TUserDataType.ExportCode(ALines: TStringList);
begin
   if Assigned(GInfra.CurrentLang.UserDataTypeGenerator) then
      GInfra.CurrentLang.UserDataTypeGenerator(ALines, Self)
   else
      GInfra.TemplateLang.UserDataTypeGenerator(ALines, Self);
end;

procedure TUserDataType.Resize;
begin
   inherited;
   if sbxElements <> nil then
   begin
      sbxElements.Constraints.MaxHeight := Height - 148;
      sbxElements.Height := sbxElements.Constraints.MaxHeight;
   end;
end;

procedure TUserDataType.OnClickType(Sender: TObject);
begin
   var t := Kind;
   var b := t in [dtRecord, dtEnum, dtOther, dtArray];
   sbxElements.Enabled := b;
   lblName2.Enabled := b and (t <> dtArray);
   lblSize.Enabled := t in [dtRecord, dtArray];
   lblType.Enabled := lblSize.Enabled;
   if b then
   begin
      var str := IfThen(t = dtRecord, 'Field', 'Value');
      btnAddElement.Caption := i18Manager.GetString('btnAdd' + str);
      lblName2.Caption := i18Manager.GetString('lbl' + str);
   end;
   var i := 0;
   for var field in GetFields do
   begin
      field.edtName.Enabled := b;
      field.cbType.Enabled := t = dtRecord;
      field.btnRemove.Enabled := b;
      field.edtSize.Enabled := field.cbType.Enabled;
      if i = 0 then
      begin
         if t = dtOther then
            b := False
         else if t = dtArray then
         begin
            b := False;
            field.edtName.Enabled := False;
            field.edtSize.Enabled := True;
            field.cbType.Enabled := True;
         end;
         i := 1;
      end;
   end;
   btnAddElement.Enabled := b;
   if GInfra.CurrentLang.EnabledPointers then
   begin
      if t = dtEnum then
         chkAddPtrType.Checked := False;
      chkAddPtrType.Enabled := t <> dtEnum;
   end;
   RefreshElements;
   if GProject <> nil then
      GProject.RefreshStatements;
   PageControl.Refresh;
   UpdateCodeEditor;
end;


constructor TField.Create(AParentTab: TUserDataType);
begin

   inherited Create(AParentTab.sbxElements, AParentTab.FElementTypeId);

   FParentTab := AParentTab;
   FParentForm := AParentTab.ParentForm;
   Constraints.MaxWidth := AParentTab.sbxElements.Width - 6;
   SetBounds(0, Parent.Height, Constraints.MaxWidth, TInfra.Scaled(Self, 22));
   Align := alTop;

   TInfra.PopulateDataTypeCombo(cbType, ParentTab.PageIndex);

   edtSize := TSizeEdit.Create(Self);
   edtSize.SetBounds(TInfra.Scaled(Self, 171), 2, edtSize.Parent.Width-btnRemove.Width-TInfra.Scaled(Self, 188), 17);
   edtSize.BorderStyle := bsNone;
   edtSize.OnChange := OnChangeSize;
end;

function TUserDataType.CreateElement: TElement;
begin
   result := TField.Create(Self);
   result.cbType.Enabled := Kind in [dtRecord, dtArray];
   TField(result).edtSize.Enabled := result.cbType.Enabled;
   result.edtName.Enabled := Kind <> dtArray;
end;

procedure TUserDataType.OnChangeName(Sender: TObject);
begin
   var lColor := NOK_COLOR;
   var info := 'OkIdD';
   var typeName := Trim(edtName.Text);
   var dataType := GInfra.GetNativeDataType(typeName);
   if typeName.IsEmpty then
      info := 'BadIdD'
   else if IsDuplicated(edtName) then
      info := 'DupType'
   else if dataType <> nil then
      info := 'DefNtvType'
   else
      lColor := OK_COLOR;
   edtName.Font.Color := lColor;
   edtName.Hint := i18Manager.GetFormattedString(info, [typeName]);
   inherited OnChangeName(Sender);
end;

procedure TUserDataType.ExportToXML(ANode: IXMLNode);
begin
   var node := AppendNode(ANode, DATATYPE_TAG);
   inherited ExportToXML(node);
   if chkAddPtrType.Enabled and chkAddPtrType.Checked then
      SetNodeAttrBool(node, POINTER_ATTR, True);
   SetNodeAttrStr(node, KIND_ATTR, TRttiEnumerationType.GetName(Kind));
end;

procedure TUserDataType.ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
begin
   inherited ImportFromXML(ANode, APinControl);
   if chkAddPtrType.Enabled then
      chkAddPtrType.Checked := GetNodeAttrBool(ANode, POINTER_ATTR, False);
   rgTypeBox.ItemIndex := Ord(TRttiEnumerationType.GetValue<TUserDataTypeKind>(GetNodeAttrStr(ANode, KIND_ATTR)));
end;

function TUserDataType.GetDimensionCount: integer;
begin
   result := 0;
   var field := GetFirstField;
   if (Kind = dtArray) and (field <> nil) and (field.edtSize.Font.Color <> NOK_COLOR) then
      result := field.edtSize.DimensionCount;
end;

function TUserDataType.GetDimensions: string;
begin
   result := '';
   var field := GetFirstField;
   if (Kind = dtArray) and (field <> nil) and (field.edtSize.Font.Color <> NOK_COLOR) then
      result := Trim(field.edtSize.Text);
end;

function TUserDataType.GetOriginalType: integer;
begin
   result := TParserHelper.GetType(Trim(edtName.Text));
   var field := GetFirstField;
   if (Kind = dtArray) and (field <> nil) then
      result := TParserHelper.GetType(field.cbType.Text);
end;

function TUserDataType.IsValidEnumValue(const AValue: string): boolean;
begin
   result := False;
   if Kind = dtEnum then
   begin
      for var field in GetFields do
      begin
         if Trim(field.edtName.Text) = AValue then
         begin
            result := True;
            break;
         end;
      end;
   end;
end;

function TUserDataType.GetFields: IEnumerable<TField>;
begin
   result := GetElements<TField>(ByTopFieldComparer);
end;

function TUserDataType.GetFirstField: TField;
begin
   result := nil;
   var fields := GetFields;
   if fields.GetEnumerator.MoveNext then
      result := fields.GetEnumerator.Current;
end;

procedure TField.OnChangeName(Sender: TObject);
begin
   var dataType := TUserDataType(ParentTab);
   if dataType.Kind in [dtOther, dtArray] then
   begin
      var lColor := OK_COLOR;
      var lHint := 'OkIdD';
      if (edtName.Text = '') and not edtName.Focused then
      begin
         lColor := NOK_COLOR;
         lHint := 'BadIdD';
      end;
      edtName.Font.Color := lColor;
      edtName.Hint := i18Manager.GetString(lHint);
      ParentTab.PageControl.Refresh;
      GProject.SetChanged;
   end
   else
      inherited OnChangeName(Sender);
end;

function TField.ExportToXML(ANode: IXMLNode): IXMLNode;
begin
   result := inherited ExportToXML(ANode);
   SetNodeAttrStr(result, SIZE_ATTR, edtSize.Text);
end;

procedure TField.ImportFromXML(ANode: IXMLNode);
begin
   inherited ImportFromXML(ANode);
   var size := GetNodeAttrStr(ANode, SIZE_ATTR);
   if size.IsEmpty then
      size := '1';
   edtSize.Text := size;
end;

procedure TField.OnChangeSize(Sender: TObject);
begin
   var sizeEdit := TSizeEdit(Sender);
   sizeEdit.Font.Color := IfThen(sizeEdit.ParseSize, BLACK_COLOR, NOK_COLOR);
   ParentTab.PageControl.Refresh;
   GProject.SetChanged;
   if ParentForm.UpdateCodeEditor then
      TTabComponent(ParentTab).UpdateCodeEditor;
end;

function TField.IsValid: boolean;
begin
   result := inherited IsValid;
   if result and edtSize.Enabled then
      result := edtSize.Font.Color = BLACK_COLOR;
end;

procedure TUserDataType.GenerateTree(ANode: TTreeNode);
begin
   ANode.Owner.AddChildObject(ANode, GetTreeNodeText, Self);
   if TInfra.IsNOkColor(Font.Color) then
   begin
      ANode.MakeVisible;
      ANode.Expand(False);
   end;
end;

function TUserDataType.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   var lang := GInfra.CurrentLang;
   if not Assigned(lang.GetUserTypeDesc) then
      lang := GInfra.TemplateLang;
   result := lang.GetUserTypeDesc(Self).Trim;
end;

initialization

   ByTopFieldComparer := TDelegatedComparer<TField>.Create(
      function(const L, R: TField): integer
      begin
         result := L.Top - R.Top;
      end
   );

end.

