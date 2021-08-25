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



unit Element;

interface

uses
   Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Generics.Defaults,
   OmniXml, PageControl_Form, Types;

type

   TElement = class;

   TElementComparer = class(TComparer<TElement>)
      FCompareType: integer;
      constructor Create(ACompareType: integer);
      function Compare(const L, R: TElement): integer; override;
   end;

   TElement = class(TPanel)
      private
         FParentTab: TTabSheet;
         FParentForm: TPageControlForm;
         function GetParentTab: TTabSheet;
      protected
         FElementTypeID: string;
         constructor Create(AParent: TScrollBox);
         procedure OnClickRemove(Sender: TObject);
         procedure OnChangeType(Sender: TObject); virtual;
         procedure OnChangeName(Sender: TObject); virtual;
         procedure UpdateMe;
         procedure OnDragOverElement(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
         procedure OnDragDropElement(Sender, Source: TObject; X, Y: Integer);
      public
         edtName: TNameEdit;
         cbType: TComboBox;
         btnRemove: TButton;
         property ParentTab: TTabSheet read FParentTab;
         property ParentForm: TPageControlForm read FParentForm;
         function ExportToXMLTag(ATag: IXMLElement): IXMLElement; virtual;
         procedure ImportFromXMLTag(ATag: IXMLElement); virtual;
         function IsValid: boolean; virtual;
   end;

implementation

uses
   Vcl.Graphics, System.SysUtils, Interfaces, TabComponent, Infrastructure, Constants;

constructor TElement.Create(AParent: TScrollBox);
var
   w: integer;
begin

   inherited Create(AParent);
   Parent := AParent;
   
   Ctl3D := false;
   BevelOuter := bvNone;
   FParentTab := GetParentTab;
   FParentForm := TTabComponent(FParentTab).ParentForm;
   DoubleBuffered := true;
   DragMode := dmAutomatic;

   edtName := TNameEdit.Create(Self);
   edtName.Parent := Self;
   edtName.SetBounds(3, 0, TInfra.Scaled(70), 21);
   edtName.ParentFont := false;
   edtName.Font.Style := [];
   edtName.ParentCtl3D := false;
   edtName.Ctl3D := true;
   edtName.ShowHint := true;
   edtName.Hint := i18Manager.GetString('BadIdD');
   edtName.Font.Color := NOK_COLOR;
   edtName.DoubleBuffered := true;
   edtName.OnChange := OnChangeName;

   cbType := TComboBox.Create(Self);
   cbType.Parent := Self;
   cbType.SetBounds(TInfra.Scaled(87), 0, TInfra.Scaled(70), 21);
   cbType.Constraints.MaxWidth := TInfra.Scaled(74);
   cbType.Style := csDropDownList;
   cbType.ParentFont := false;
   cbType.Font.Style := [];
   cbType.Font.Color := clWindowText;
   cbType.OnChange := OnChangeType;

   btnRemove := TButton.Create(Self);
   btnRemove.Parent := Self;
   btnRemove.ParentFont := false;
   btnRemove.Font.Style := [];
   btnRemove.DoubleBuffered := true;
   btnRemove.Caption := i18Manager.GetString('btnRemove');
   btnRemove.OnClick := OnClickRemove;
   w := TInfra.GetAutoWidth(btnRemove);
   btnRemove.SetBounds(Parent.Width-w-TInfra.Scaled(32), 0, w+14, TInfra.Scaled(20));

   OnDragOver := OnDragOverElement;
   OnDragDrop := OnDragDropElement;
end;

function TElement.GetParentTab: TTabSheet;
var
   winControl: TWinControl;
begin
   result := nil;
   winControl := Parent;
   while not (winControl is TForm) do
   begin
      if winControl is TTabComponent then
      begin
         result := TTabComponent(winControl);
         break;
      end
      else
         winControl := winControl.Parent;
   end;
end;

procedure TElement.OnClickRemove(Sender: TObject);
begin
   Hide;
   if Parent.Height < Parent.Constraints.MaxHeight then
      Parent.Height := Parent.Height - 22;
   Parent := Parent.Parent;
   TTabComponent(FParentTab).RefreshElements;
   UpdateMe;
   TTabComponent(FParentTab).UpdateCodeEditor;
end;

procedure TElement.OnChangeType(Sender: TObject);
begin
   if FParentForm.Visible and FParentForm.Enabled then  // replace with CanFocus once fixed by Embarcadero (RSP-34465)
      FParentForm.SetFocus;
   cbType.Hint := cbType.Text;
   if FParentTab.Font.Color <> NOK_COLOR then
      TTabComponent(FParentTab).UpdateCodeEditor;
end;

function TElement.IsValid: boolean;
begin
   result := true;
   if edtName.Enabled and ((edtName.Font.Color = NOK_COLOR) or ((Trim(edtName.Text) = '') and not edtName.Focused)) then
      result := false;
end;

procedure TElement.OnChangeName(Sender: TObject);
var
   info: string;
   lColor: TColor;
begin
   lColor := NOK_COLOR;
   info := '';
   if edtName.Text = '' then
   begin
      if not edtName.Focused then
         info := 'BadIdD';
   end
   else if GInfra.ValidateId(edtName.Text) <> VALID_IDENT then
      info := 'BadIdD'
   else if TTabComponent(FParentTab).IsDuplicatedElement(Self) then
      info := 'DupIdD';
   if info.IsEmpty then
   begin
      info := 'OkIdD';
      lColor := OK_COLOR
   end;
   edtName.Font.Color := lColor;
   edtName.Hint := i18Manager.GetString(info);
   UpdateMe;
   if FParentForm.UpdateCodeEditor then
      TTabComponent(FParentTab).UpdateCodeEditor;
end;

procedure TElement.UpdateMe;
begin
   FParentTab.PageControl.Refresh;
   GProject.SetChanged;
end;

procedure TElement.ImportFromXMLTag(ATag: IXMLElement);
var
   idx: integer;
begin
   edtName.Text := ATag.GetAttribute(NAME_ATTR);
   idx := cbType.Items.IndexOf(ATag.GetAttribute(TYPE_ATTR));
   if idx <> -1 then
      cbType.ItemIndex := idx
   else if cbType.Items.Count > 0 then 
      cbType.ItemIndex := 0;
   cbType.Hint := cbType.Text;
end;

function TElement.ExportToXMLTag(ATag: IXMLElement): IXMLElement;
begin
   result := ATag.OwnerDocument.CreateElement(FElementTypeID);
   ATag.AppendChild(result);
   result.SetAttribute(NAME_ATTR, Trim(edtName.Text));
   result.SetAttribute(TYPE_ATTR, cbType.Text);
end;

procedure TElement.OnDragOverElement(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if (Source = Sender) or (not (Source is TElement)) or (TElement(Source).Parent <> Parent) then
      Accept := false;
end;

procedure TElement.OnDragDropElement(Sender, Source: TObject; X, Y: Integer);
var
   sourceElement: TElement;
   t: integer;
begin
   if Source is TElement then
   begin
      sourceElement := TElement(Source);
      t := sourceElement.Top;
      sourceElement.Top := Top;
      Top := t;
      GProject.SetChanged;
      TTabComponent(FParentTab).UpdateCodeEditor;
   end;
end;

constructor TElementComparer.Create(ACompareType: integer);
begin
   inherited Create;
   FCompareType := ACompareType;
end;

function TElementComparer.Compare(const L, R: TElement): integer;
begin
   if (L = nil) and (R = nil) then
      result := 0
   else if L = nil then
      result := 241
   else if R = nil then
      result := -241
   else if FCompareType = TOP_COMPARE then
      result := L.Top - R.Top
   else if FCompareType = NAME_COMPARE then
      result := TComparer<string>.Default.Compare(Trim(L.edtName.Text), Trim(R.edtName.Text))
   else if FCompareType = COMPONENT_INDEX_COMPARE then
      result := L.ComponentIndex - R.ComponentIndex
   else
      result := -170;
end;

end.
