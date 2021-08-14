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
      function ExportToXMLTag(ATag: IXMLElement): IXMLElement; override;
      procedure ImportFromXMLTag(ATag: IXMLElement); override;
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
      procedure ExportToXMLTag(ATag: IXMLElement); override;
      procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
      procedure RefreshSizeEdits; override;
      function IsValidEnumValue(const AValue: string): boolean;
      function GetDimensionCount: integer;
      function GetDimensions: string;
      function GetOriginalType: integer;
      procedure GenerateTree(ANode: TTreeNode);
      function Kind: TUserDataTypeKind;
      function GetFields: IEnumerable<TField>;
      function GetExternModifier: string; override;
      function GetTreeNodeText(ANodeOffset: integer = 0): string; override;
   end;

implementation

uses
   Vcl.Forms, Vcl.Graphics, System.SysUtils, System.StrUtils, System.Rtti,
   Generics.Defaults, Infrastructure, LangDefinition, ParserHelper, XMLProcessor, Constants;

var
   ByTopFieldComparer: IComparer<TField>;

constructor TUserDataType.Create(AParentForm: TDataTypesForm);
var
   dt: TUserDataTypeKind;
   s: string;
begin

   FElementTypeID := 'field';
   FCodeIncludeExtern := GInfra.CurrentLang.CodeIncludeExternDataType;

   inherited Create(AParentForm);

   CreateNameControls(Self, 9, 10);

   lblName2 := TLabel.Create(Self);
   lblName2.Parent := Self;
   lblName2.ParentFont := false;
   lblName2.Font.Style := [fsBold];
   lblName2.Font.Color := clWindowText;
   lblName2.SetBounds(5, 131, 0, 13);
   lblName2.Caption := i18Manager.GetString('lblField');

   lblSize := TLabel.Create(Self);
   lblSize.Parent := Self;
   lblSize.ParentFont := false;
   lblSize.Font.Style := [fsBold];
   lblSize.Font.Color := clWindowText;
   lblSize.SetBounds(TInfra.Scaled(176), 131, 0, 13);
   lblSize.Caption := i18Manager.GetString('lblSize');

   lblType := TLabel.Create(Self);
   lblType.Parent := Self;
   lblType.ParentFont := false;
   lblType.Font.Style := [fsBold];
   lblType.Font.Color := clWindowText;
   lblType.SetBounds(TInfra.Scaled(87), 131, 0, 13);
   lblType.Caption := i18Manager.GetString('lblType');

   sbxElements := TScrollBox.Create(Self);
   sbxElements.Parent := Self;
   sbxElements.Ctl3D := false;
   sbxElements.BorderStyle := bsNone;
   sbxElements.Constraints.MaxHeight := AParentForm.Height - 233;
   sbxElements.Constraints.MinWidth := 302;
   sbxElements.SetBounds(0, 149, TInfra.Scaled(308), 0);
   sbxElements.VertScrollBar.Tracking := true;
   sbxElements.DoubleBuffered := true;
   sbxElements.Anchors := [akTop, akBottom, akLeft, akRight];

   btnAddElement := TButton.Create(Self);
   btnAddElement.Parent := Self;
   btnAddElement.ParentFont := false;
   btnAddElement.Font.Style := [];
   btnAddElement.Caption := i18Manager.GetString('btnAddField');
   btnAddElement.ShowHint := true;
   btnAddElement.DoubleBuffered := true;
   btnAddElement.SetBounds(1, 102, TInfra.Scaled(306), 25);
   btnAddElement.OnClick := AddElement;

   CreateLibControls(Self, edtName.Left+edtName.Width+7, 10);

   rgTypeBox := TRadioGroup.Create(Self);
   rgTypeBox.Parent := Self;
   rgTypeBox.ParentFont := false;
   rgTypeBox.ParentBackground := false;
   rgTypeBox.Font.Style := [];
   rgTypeBox.Font.Color := clWindowText;
   rgTypeBox.DoubleBuffered := true;
   rgTypeBox.Columns := 2;
   rgTypeBox.Caption := i18Manager.GetString('rgTypeBox');
   for dt := Low(TUserDataTypeKind) to High(TUserDataTypeKind) do
   begin
      s := TRttiEnumerationType.GetName(dt);
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
   chkAddPtrType.ParentFont := false;
   chkAddPtrType.Font.Style := [];
   chkAddPtrType.Font.Color := clWindowText;
   chkAddPtrType.SetBounds(rgTypeBox.BoundsRect.Right+6, 42, TInfra.GetAutoWidth(chkAddPtrType), 17);
   chkAddPtrType.DoubleBuffered := true;
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
   if (Kind in [dtOther, dtArray]) and (sbxElements.ControlCount = 0) then
      btnAddElement.Enabled := false;
   inherited AddElement(Sender);
end;

procedure TUserDataType.RefreshSizeEdits;
var
   i: integer;
   field: TField;
begin
   ParentForm.UpdateCodeEditor := false;
   for i := 0 to sbxElements.ControlCount-1 do
   begin
      field := TField(sbxElements.Controls[i]);
      if (field.edtSize.Text <> '1') and Assigned(field.edtSize.OnChange) then
         field.edtSize.OnChange(field.edtSize);
   end;
   ParentForm.UpdateCodeEditor := true;
end;

function TUserDataType.GetExternModifier: string;
var
   lang: TLangDefinition;
begin
   lang := GInfra.CurrentLang;
   case chkExternal.State of
      cbChecked:   result := lang.DataTypeExternal;
      cbUnchecked: result := lang.DataTypeNotExternal;
      cbGrayed:    result := lang.DataTypeTransExternal;
   end;
end;

procedure TUserDataType.Resize;
begin
   inherited;
   if sbxElements <> nil then
   begin
      sbxElements.Constraints.MaxHeight := ParentForm.Height - 233;
      sbxElements.Height := sbxElements.Constraints.MaxHeight;
   end;
end;

procedure TUserDataType.OnClickType(Sender: TObject);
var
   b: boolean;
   field: TField;
   i: integer;
   t: TUserDataTypeKind;
   str: string;
begin
   t := Kind;
   b := t in [dtRecord, dtEnum, dtOther, dtArray];
   sbxElements.Enabled := b;
   lblName2.Enabled := b and (t <> dtArray);
   lblSize.Enabled := t in [dtRecord, dtArray];
   lblType.Enabled := lblSize.Enabled;
   if b then
   begin
      str := IfThen(t = dtRecord, 'Field', 'Value');
      btnAddElement.Caption := i18Manager.GetString('btnAdd' + str);
      lblName2.Caption := i18Manager.GetString('lbl' + str);
   end;
   for i := 0 to sbxElements.ControlCount-1 do
   begin
      field := TField(sbxElements.Controls[i]);
      with field do
      begin
         edtName.Enabled := b;
         cbType.Enabled := t = dtRecord;
         btnRemove.Enabled := b;
         edtSize.Enabled := cbType.Enabled;
         if i = 0 then
         begin
            if t = dtOther then
               b := false
            else if t = dtArray then
            begin
               b := false;
               edtName.Enabled := false;
               edtSize.Enabled := true;
               cbType.Enabled := true;
            end;
         end;
      end;
   end;
   btnAddElement.Enabled := b;
   if GInfra.CurrentLang.EnabledPointers then
   begin
      if t = dtEnum then
         chkAddPtrType.Checked := false;
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

   inherited Create(AParentTab.sbxElements);

   FElementTypeID := AParentTab.FElementTypeID;
   Constraints.MaxWidth := AParentTab.sbxElements.Width - 6;
   SetBounds(0, Parent.Height, Constraints.MaxWidth, TInfra.Scaled(22));
   Align := alTop;

   TInfra.PopulateDataTypeCombo(cbType, ParentTab.PageIndex);

   btnRemove.SetBounds(TInfra.Scaled(237), 0, TInfra.GetAutoWidth(btnRemove)+14, TInfra.Scaled(20));

   edtSize := TSizeEdit.Create(Self);
   edtSize.SetBounds(TInfra.Scaled(176), 2, 54, 17);
   edtSize.BorderStyle := bsNone;
   edtSize.OnChange := OnChangeSize;
end;

function TUserDataType.CreateElement: TElement;
var
   field: TField;
   t: TUserDataTypeKind;
begin
   t := Kind;
   field := TField.Create(Self);
   field.cbType.Enabled := t in [dtRecord, dtArray];
   field.edtSize.Enabled := field.cbType.Enabled;
   field.edtName.Enabled := t <> dtArray;
   result := field;
end;

procedure TUserDataType.OnChangeName(Sender: TObject);
var
   info, typeName: string;
   dataType: PNativeDataType;
begin
   edtName.Font.Color := NOK_COLOR;
   typeName := Trim(edtName.Text);
   dataType := GInfra.GetNativeDataType(typeName);
   if typeName.IsEmpty then
      info := 'BadIdD'
   else if IsDuplicated(edtName) then
      info := 'DupType'
   else if dataType <> nil then
      info := 'DefNtvType'
   else
   begin
      edtName.Font.Color := OK_COLOR;
      info := 'OkIdD';
   end;
   edtName.Hint := i18Manager.GetFormattedString(info, [typeName]);
   inherited OnChangeName(Sender);
end;

procedure TUserDataType.ExportToXMLTag(ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   tag := ATag.OwnerDocument.CreateElement(DATATYPE_TAG);
   ATag.AppendChild(tag);
   inherited ExportToXMLTag(tag);
   if chkAddPtrType.Enabled and chkAddPtrType.Checked then
      tag.SetAttribute(POINTER_ATTR, 'true');
   tag.SetAttribute(KIND_ATTR, TRttiEnumerationType.GetName(Kind));
end;

procedure TUserDataType.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
begin
   inherited ImportFromXMLTag(ATag, APinControl);
   if chkAddPtrType.Enabled then
      chkAddPtrType.Checked := TXMLProcessor.GetBool(ATag, POINTER_ATTR);
   rgTypeBox.ItemIndex := Ord(TRttiEnumerationType.GetValue<TUserDataTypeKind>(ATag.GetAttribute(KIND_ATTR)));
end;

function TUserDataType.GetDimensionCount: integer;
var
   field: TField;
begin
   result := 0;
   if (Kind = dtArray) and (sbxElements.ControlCount > 0) then
   begin
      field := TField(sbxElements.Controls[0]);
      if field.edtSize.Font.Color <> NOK_COLOR then
         result := field.edtSize.DimensionCount;
   end;
end;

function TUserDataType.GetDimensions: string;
var
   field: TField;
begin
   result := '';
   if (Kind = dtArray) and (sbxElements.ControlCount > 0) then
   begin
      field := TField(sbxElements.Controls[0]);
      if field.edtSize.Font.Color <> NOK_COLOR then
         result := Trim(field.edtSize.Text);
   end;
end;

function TUserDataType.GetOriginalType: integer;
var
   field: TField;
begin
   result := TParserHelper.GetType(Trim(edtName.Text));
   if (Kind = dtArray) and (sbxElements.ControlCount > 0) then
   begin
      field := TField(sbxElements.Controls[0]);
      result := TParserHelper.GetType(field.cbType.Text);
   end;
end;

function TUserDataType.IsValidEnumValue(const AValue: string): boolean;
var
   field: TField;
   i: integer;
begin
   result := false;
   if Kind = dtEnum then
   begin
      for i := 0 to sbxElements.ControlCount-1 do
      begin
         field := TField(sbxElements.Controls[i]);
         if Trim(field.edtName.Text) = AValue then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

function TUserDataType.GetFields: IEnumerable<TField>;
begin
   result := GetElements<TField>(ByTopFieldComparer);
end;

procedure TField.OnChangeName(Sender: TObject);
var
   lColor: TColor;
   lHint: string;
   dataType: TUserDataType;
begin
   dataType := TUserDataType(ParentTab);
   if dataType.Kind in [dtOther, dtArray] then
   begin
      lColor := OK_COLOR;
      lHint := 'OkIdD';
      if (edtName.Text = '') and not edtName.Focused then
      begin
         lColor := NOK_COLOR;
         lHint := 'BadIdD';
      end;
      edtName.Font.Color := lColor;
      edtName.Hint := i18Manager.GetString(lHint);
      UpdateMe;
   end
   else
      inherited OnChangeName(Sender);
end;

function TField.ExportToXMLTag(ATag: IXMLElement): IXMLElement;
begin
   inherited ExportToXMLTag(ATag).SetAttribute(SIZE_ATTR, edtSize.Text);
end;

procedure TField.ImportFromXMLTag(ATag: IXMLElement);
var
   size: string;
begin
   inherited ImportFromXMLTag(ATag);
   size := ATag.GetAttribute(SIZE_ATTR);
   if size.IsEmpty then
      size := '1';
   edtSize.Text := size;
end;

procedure TField.OnChangeSize(Sender: TObject);
begin
   edtSize.OnChangeSize(edtSize);
   UpdateMe;
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
      ANode.Expand(false);
   end;
end;

function TUserDataType.GetTreeNodeText(ANodeOffset: integer = 0): string;
var
   lang: TLangDefinition;
begin
   lang := nil;
   if Assigned(GInfra.CurrentLang.GetUserTypeDesc) then
      lang := GInfra.CurrentLang
   else if Assigned(GInfra.TemplateLang.GetUserTypeDesc) then
      lang := GInfra.TemplateLang;
   if lang <> nil then
      result := lang.GetUserTypeDesc(Self).Trim
   else
      result := inherited GetTreeNodeText(ANodeOffset);
end;

initialization

   ByTopFieldComparer := TDelegatedComparer<TField>.Create(
      function(const L, R: TField): integer
      begin
         result := L.Top - R.Top;
      end
   );

end.

