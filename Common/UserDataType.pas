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
   Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, System.Classes, WinApi.Messages, OmniXML,
   SizeEdit, TabComponent, Element, DataTypes_Form, CommonInterfaces;

type

   TUserDataType = class;

   TField = class(TElement)
      constructor Create(const AParentTab: TUserDataType);
   protected
      procedure OnChangeSize(Sender: TObject);
      procedure OnChangeName(Sender: TObject); override;
   public
      edtSize: TSizeEdit;
      function ExportToXMLTag(const ATag: IXMLElement): IXMLElement; override;
      procedure ImportFromXMLTag(const ATag: IXMLElement); override;
      function IsValid: boolean; override;
   end;

   TUserDataType = class(TTabComponent)
   protected
      procedure OnChangeName(Sender: TObject); override;
      procedure OnClickType(Sender: TObject);
      procedure SetActive(const AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure AddElement(Sender: TObject); override;
      procedure ExtDeclareOnClick(Sender: TObject);
      procedure WMSize(var Msg: TMessage); message WM_SIZE;
   public
      chkAddPtrType: TCheckBox;
      gbTypeBox: TGroupBox;
      rbReal,
      rbInt,
      rbStruct,
      rbEnum,
      rbOther,
      rbArray: TRadioButton;
      lblName2,
      lblType,
      lblSize: TLabel;
      property FieldCount: integer read GetElementCount default 0;
      constructor Create(const AParentForm: TDataTypesForm);
      procedure ExportToXMLTag(ATag: IXMLElement); override;
      procedure ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
      function GetFieldIterator: IIterator;
      procedure Localize(const AList: TStringList); override;
      procedure RefreshSizeEdits; override;
      function IsValidEnumValue(const AValue: string): boolean;
      function GetDimensionCount: integer;
      function GetDimensions: string;
      function GetOriginalType: integer;
      procedure GenerateTree(const ANode: TTreeNode);
   end;

implementation

uses
   Vcl.Forms, Vcl.Graphics, System.SysUtils, System.StrUtils, ApplicationCommon,
   CommonTypes, LangDefinition, ParserHelper, XMLProcessor;

constructor TUserDataType.Create(const AParentForm: TDataTypesForm);
begin

   inherited Create(AParentForm);

   FElementMode := FIELD_IDENT;

   lblName := TLabel.Create(Self);
   lblName.Parent := Self;
   lblName.SetBounds(9, 10, 0, 13);
   lblName.Caption := i18Manager.GetString('lblName');
   lblName.ParentFont := false;
   lblName.Font.Style := [];
   lblName.Font.Color := clWindowText;

   edtName := TEdit.Create(Self);
   edtName.Parent := Self;
   edtName.SetBounds(lblName.Width+13, 5, 162-lblName.Width, 21);
   edtName.ParentFont := false;
   edtName.Font.Style := [];
   edtName.ShowHint := true;
   edtName.Hint := i18Manager.GetString('BadIdD');
   edtName.DoubleBuffered := true;
   edtName.OnChange := OnChangeName;

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
   lblSize.SetBounds(195, 131, 0, 13);
   lblSize.Caption := i18Manager.GetString('lblSize');

   lblType := TLabel.Create(Self);
   lblType.Parent := Self;
   lblType.ParentFont := false;
   lblType.Font.Style := [fsBold];
   lblType.Font.Color := clWindowText;
   lblType.SetBounds(97, 131, 0, 13);
   lblType.Caption := i18Manager.GetString('lblType');

   sbxElements := TScrollBox.Create(Self);
   sbxElements.Parent := Self;
   sbxElements.Ctl3D := false;
   sbxElements.BorderStyle := bsNone;
   sbxElements.Constraints.MaxHeight := AParentForm.Height - 233;
   sbxElements.Constraints.MinWidth := 302;
   sbxElements.SetBounds(0, 149, 308, 0);
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
   btnAddElement.SetBounds(1, 102, 306, 25);
   btnAddElement.OnClick := AddElement;

   lblLibrary := TLabel.Create(Self);
   lblLibrary.Parent := Self;
   lblLibrary.SetBounds(edtName.Left+edtName.Width+7, 10, 0, 13);
   lblLibrary.Caption := i18Manager.GetString('lblLibrary');
   lblLibrary.ParentFont := false;
   lblLibrary.Font.Style := [];
   lblLibrary.Font.Color := clWindowText;

   edtLibrary := TEdit.Create(Self);
   edtLibrary.Parent := Self;
   edtLibrary.SetBounds(lblLibrary.Left+lblLibrary.Width+5, 5, 119-lblLibrary.Width, 21);
   edtLibrary.ParentFont := false;
   edtLibrary.Font.Style := [];
   edtLibrary.Font.Color := clGreen;
   edtLibrary.ShowHint := true;
   edtLibrary.DoubleBuffered := true;
   edtLibrary.OnChange := OnChangeLib;
   edtLibrary.Hint := ReplaceStr(i18Manager.GetFormattedString('edtLibHintType', [GInfra.CurrentLang.LibraryExt]), '##', sLineBreak);

   gbTypeBox := TGroupBox.Create(Self);
   gbTypeBox.Parent := Self;
   gbTypeBox.SetBounds(1, 28, 305, 73);
   gbTypeBox.ParentFont := false;
   gbTypeBox.ParentBackground := false;
   gbTypeBox.Font.Style := [];
   gbTypeBox.Font.Color := clWindowText;
   gbTypeBox.DoubleBuffered := true;
   gbTypeBox.Caption := i18Manager.GetString('rgTypeBox');

   chkAddPtrType := TCheckBox.Create(gbTypeBox);
   chkAddPtrType.Parent := gbTypeBox;
   chkAddPtrType.SetBounds(170, 18, 134, 17);
   chkAddPtrType.ParentFont := false;
   chkAddPtrType.Font.Style := [];
   chkAddPtrType.Font.Color := clWindowText;
   chkAddPtrType.DoubleBuffered := true;
   chkAddPtrType.Caption := i18Manager.GetString('chkAddPtrType');
   chkAddPtrType.Enabled := GInfra.CurrentLang.EnabledPointers;

   chkExtDeclare := TCheckBox.Create(gbTypeBox);
   chkExtDeclare.Parent := gbTypeBox;
   chkExtDeclare.SetBounds(170, 35, 128, 17);
   chkExtDeclare.ParentFont := false;
   chkExtDeclare.Font.Style := [];
   chkExtDeclare.Font.Color := clWindowText;
   chkExtDeclare.DoubleBuffered := true;
   chkExtDeclare.OnClick := ExtDeclareOnClick;
   chkExtDeclare.Caption := i18Manager.GetString('chkExtDeclare');
   chkExtDeclare.ShowHint := true;
   chkExtDeclare.Hint := i18Manager.GetString('chkExtDeclare.Hint');

   rbInt := TRadioButton.Create(gbTypeBox);
   rbInt.Parent := gbTypeBox;
   rbInt.SetBounds(8, 18, 78, 17);
   rbInt.ParentFont := false;
   rbInt.Font.Style := [];
   rbInt.Font.Color := clWindowText;
   rbInt.Caption := i18Manager.GetString('rbInt');
   rbInt.DoubleBuffered := true;
   rbInt.OnClick := OnClickType;

   rbReal := TRadioButton.Create(gbTypeBox);
   rbReal.Parent := gbTypeBox;
   rbReal.SetBounds(86, 18, 82, 17);
   rbReal.ParentFont := false;
   rbReal.Font.Style := [];
   rbReal.Font.Color := clWindowText;
   rbReal.Caption := i18Manager.GetString('rbReal');
   rbReal.DoubleBuffered := true;
   rbReal.OnClick := OnClickType;

   rbStruct := TRadioButton.Create(gbTypeBox);
   rbStruct.Parent := gbTypeBox;
   rbStruct.SetBounds(8, 35, 78, 17);
   rbStruct.ParentFont := false;
   rbStruct.Font.Style := [];
   rbStruct.Font.Color := clWindowText;
   rbStruct.Caption := i18Manager.GetString('rbStruct');
   rbStruct.Checked := true;
   rbStruct.DoubleBuffered := true;
   rbStruct.OnClick := OnClickType;

   rbEnum := TRadioButton.Create(gbTypeBox);
   rbEnum.Parent := gbTypeBox;
   rbEnum.SetBounds(86, 52, 84, 17);
   rbEnum.ParentFont := false;
   rbEnum.Font.Style := [];
   rbEnum.Font.Color := clWindowText;
   rbEnum.Caption := i18Manager.GetString('rbEnum');
   rbEnum.DoubleBuffered := true;
   rbEnum.OnClick := OnClickType;

   rbArray := TRadioButton.Create(gbTypeBox);
   rbArray.Parent := gbTypeBox;
   rbArray.SetBounds(8, 52, 78, 17);
   rbArray.ParentFont := false;
   rbArray.Font.Style := [];
   rbArray.Font.Color := clWindowText;
   rbArray.Caption := i18Manager.GetString('rbArray');
   rbArray.DoubleBuffered := true;
   rbArray.OnClick := OnClickType;

   rbOther := TRadioButton.Create(gbTypeBox);
   rbOther.Parent := gbTypeBox;
   rbOther.SetBounds(86, 35, 78, 17);
   rbOther.ParentFont := false;
   rbOther.Font.Style := [];
   rbOther.Font.Color := clWindowText;
   rbOther.Caption := i18Manager.GetString('rbOther');
   rbOther.DoubleBuffered := true;
   rbOther.OnClick := OnClickType;

   GProject.AddComponent(Self);
end;

procedure TUserDataType.SetActive(const AValue: boolean);
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
   if (rbOther.Checked or rbArray.Checked) and (sbxElements.ControlCount = 0) then
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
      if field.edtSize.Text <> '1' then
         field.edtSize.OnChange(field.edtSize);
   end;
   ParentForm.UpdateCodeEditor := true;
end;

procedure TUserDataType.ExtDeclareOnClick(Sender: TObject);
begin
   if Font.Color <> NOK_COLOR then
      TInfra.UpdateCodeEditor(Self);
end;

procedure TUserDataType.WMSize(var Msg: TMessage);
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
   str: string;
begin
   b := rbStruct.Checked or rbEnum.Checked or rbOther.Checked or rbArray.Checked;
   sbxElements.Enabled := b;
   lblName2.Enabled := b and not rbArray.Checked;
   lblSize.Enabled := rbStruct.Checked or rbArray.Checked;
   lblType.Enabled := lblSize.Enabled;
   if b then
   begin
      str := IfThen(rbStruct.Checked, 'field', 'value');
      btnAddElement.Caption := i18Manager.GetString('btnAdd' + str);
      lblName2.Caption := i18Manager.GetString('lbl' + str);
   end;
   for i := 0 to sbxElements.ControlCount-1 do
   begin
      field := TField(sbxElements.Controls[i]);
      with field do
      begin
         edtName.Enabled := b;
         cbType.Enabled := rbStruct.Checked;
         btnRemove.Enabled := b;
         edtSize.Enabled := rbStruct.Checked;
         if i = 0 then
         begin
            if rbOther.Checked then
               b := false;
            if rbArray.Checked then
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
      if rbEnum.Checked then
         chkAddPtrType.Checked := false;
      chkAddPtrType.Enabled := not rbEnum.Checked;
   end;
   RefreshElements;
   if GProject <> nil then
      GProject.RefreshStatements;
   PageControl.Refresh;
   UpdateCodeEditor;
end;


constructor TField.Create(const AParentTab: TUserDataType);
begin

   inherited Create(AParentTab.sbxElements);
   
   FElem_Id := FIELD_IDENT;
   Constraints.MaxWidth := 302;
   SetBounds(0, Parent.Height, 302, 22);
   Align := alTop;

   cbType.SetBounds(97, 0, 80, 21);
   cbType.Constraints.MaxWidth := 90;
   TInfra.PopulateDataTypeCombo(cbType, ParentTab.PageIndex);

   btnRemove.SetBounds(239, 0, 52, 20);

   edtSize := TSizeEdit.Create(Self);
   edtSize.SetBounds(195, 2, 43, 17);
   edtSize.BorderStyle := bsNone;
   edtSize.OnChange := OnChangeSize;
end;

function TUserDataType.CreateElement: TElement;
var
   field: TField;
begin
   field := TField.Create(Self);
   field.cbType.Enabled := rbStruct.Checked or rbArray.Checked;
   field.edtSize.Enabled := field.cbType.Enabled;
   field.edtName.Enabled := not rbArray.Checked;
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
   if typeName = '' then
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
   typeId: string;
begin
   tag := ATag.OwnerDocument.CreateElement(DATATYPE_TAG);
   ATag.AppendChild(tag);
   inherited ExportToXMLTag(tag);
   if chkAddPtrType.Enabled and chkAddPtrType.Checked then
      tag.SetAttribute('pointer', 'true');
   if rbStruct.Checked then
      typeId := 'struct_type'
   else if rbInt.Checked then
      typeId := 'int_type'
   else if rbReal.Checked then
      typeId := 'real_type'
   else if rbEnum.Checked then
      typeId := 'enum_type'
   else if rbArray.Checked then
      typeId := 'array_type'
   else
      typeId := 'other_type';
   tag.SetAttribute(typeId, 'true');
end;

procedure TUserDataType.Localize(const AList: TStringList);
begin
   lblName2.Caption := AList.Values['lblName'];
   btnAddElement.Caption := AList.Values['btnAddField'];
   btnAddElement.Hint := AList.Values['btnAddFieldHint'];
   chkAddPtrType.Caption := AList.Values['chkAddPtrType'];
   rbStruct.Caption := AList.Values['rbStruct'];
   rbEnum.Caption := AList.Values['rbEnum'];
   rbOther.Caption := AList.Values['rbOther'];
   rbArray.Caption := AList.Values['rbArray'];
   rbInt.Caption := AList.Values['rbInt'];
   rbReal.Caption := AList.Values['rbReal'];
   gbTypeBox.Caption := AList.Values['rgTypeBox'];
   edtLibrary.Hint := Format(AList.Values['edtLibHintType'], [GInfra.CurrentLang.LibraryExt]);
   inherited Localize(AList);
end;

procedure TUserDataType.ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
begin
   inherited ImportFromXMLTag(ATag, APinControl);
   if chkAddPtrType.Enabled then
      chkAddPtrType.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'pointer');
   if TXMLProcessor.GetBoolFromAttr(ATag, 'struct_type') then
      rbStruct.Checked := true
   else if TXMLProcessor.GetBoolFromAttr(ATag, 'int_type') then
      rbInt.Checked := true
   else if TXMLProcessor.GetBoolFromAttr(ATag, 'real_type') then
      rbReal.Checked := true
   else if TXMLProcessor.GetBoolFromAttr(ATag, 'enum_type') then
      rbEnum.Checked := true
   else if TXMLProcessor.GetBoolFromAttr(ATag, 'array_type') then
      rbArray.Checked := true
   else
      rbOther.Checked := true;
end;

function TUserDataType.GetDimensionCount: integer;
var
   field: TField;
begin
   result := 0;
   if rbArray.Checked and (sbxElements.ControlCount > 0) then
   begin
      field := TField(sbxElements.Controls[0]);
      result := field.edtSize.DimensionCount;
   end;
end;

function TUserDataType.GetDimensions: string;
var
   field: TField;
begin
   result := '';
   if rbArray.Checked and (sbxElements.ControlCount > 0) then
   begin
      field := TField(sbxElements.Controls[0]);
      result := Trim(field.edtSize.Text);
   end;
end;

function TUserDataType.GetOriginalType: integer;
var
   field: TField;
begin
   result := TParserHelper.GetType(Trim(edtName.Text));
   if rbArray.Checked and (sbxElements.ControlCount > 0) then
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
   if rbEnum.Checked then
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

procedure TField.OnChangeName(Sender: TObject);
var
   lColor: TColor;
   lHint: string;
   dataType: TUserDataType;
begin
   dataType := TUserDataType(ParentTab);
   if dataType.rbOther.Checked or dataType.rbArray.Checked then
   begin
      if Trim(edtName.Text) = '' then
      begin
         lColor := NOK_COLOR;
         lHint := 'BadIdD';
      end
      else
      begin
         lColor := OK_COLOR;
         lHint := 'OkIdD';
      end;
      edtName.Font.Color := lColor;
      edtName.Hint := i18Manager.GetString(lHint);
      UpdateMe;
   end
   else
      inherited OnChangeName(Sender);
end;

function TField.ExportToXMLTag(const ATag: IXMLElement): IXMLElement;
begin
   inherited ExportToXMLTag(ATag).SetAttribute(SIZE_ATTR, edtSize.Text);
end;

procedure TField.ImportFromXMLTag(const ATag: IXMLElement);
var
   size: string;
begin
   inherited ImportFromXMLTag(ATag);
   if TXMLProcessor.GetBoolFromAttr(ATag, 'table') then  // for backward compatibility
      edtSize.Text := '100'
   else
   begin
      size := ATag.GetAttribute(SIZE_ATTR);
      if size.IsEmpty then
         size := '1';
      edtSize.Text := size;
   end;
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

function TUserDataType.GetFieldIterator: IIterator;
begin
   result := GetElementIterator;
end;

procedure TUserDataType.GenerateTree(const ANode: TTreeNode);
var
   desc: string;
   lang: TLangDefinition;
begin
   desc := '';
   lang := nil;
   if Assigned(GInfra.CurrentLang.GetUserTypeDesc) then
      lang := GInfra.CurrentLang
   else if Assigned(GInfra.DummyLang.GetUserTypeDesc) then
      lang := GInfra.DummyLang;
   if lang <> nil then
      desc := lang.GetUserTypeDesc(Self).Trim;
   ANode.Owner.AddChildObject(ANode, desc, Self);
   if TInfra.IsNOkColor(Font.Color) then
   begin
      ANode.MakeVisible;
      ANode.Expand(false);
   end;
end;

end.