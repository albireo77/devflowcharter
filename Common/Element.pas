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
         procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
         procedure DragDrop(Source: TObject; X, Y: Integer); override;
      public
         edtName: TNameEdit;
         cbType: TComboBox;
         btnRemove: TButton;
         property ParentTab: TTabSheet read FParentTab;
         property ParentForm: TPageControlForm read FParentForm;
         function ExportToXML(ANode: IXMLNode): IXMLNode; virtual;
         procedure ImportFromXML(ANode: IXMLNode); virtual;
         function IsValid: boolean; virtual;
   end;

implementation

uses
   Vcl.Graphics, System.SysUtils, System.Classes, Interfaces, TabComponent, Infrastructure,
   Constants, OmniXMLUtils;

constructor TElement.Create(AParent: TScrollBox);
begin

   inherited Create(AParent);
   Parent := AParent;
   
   Ctl3D := False;
   BevelOuter := bvNone;
   FParentTab := GetParentTab;
   FParentForm := TTabComponent(FParentTab).ParentForm;
   DoubleBuffered := True;
   DragMode := dmAutomatic;

   edtName := TNameEdit.Create(Self);
   edtName.Parent := Self;
   edtName.SetBounds(3, 0, TInfra.Scaled(Self, 70), 21);
   edtName.ParentFont := False;
   edtName.Font.Style := [];
   edtName.ParentCtl3D := False;
   edtName.Ctl3D := True;
   edtName.ShowHint := True;
   edtName.Hint := i18Manager.GetString('BadIdD');
   edtName.Font.Color := NOK_COLOR;
   edtName.DoubleBuffered := True;
   edtName.OnChange := OnChangeName;

   cbType := TComboBox.Create(Self);
   cbType.Parent := Self;
   cbType.SetBounds(TInfra.Scaled(Self, 87), 0, TInfra.Scaled(Self, 70), 21);
   cbType.Constraints.MaxWidth := TInfra.Scaled(Self, 74);
   cbType.Style := csDropDownList;
   cbType.ParentFont := False;
   cbType.Font.Style := [];
   cbType.Font.Color := clWindowText;
   cbType.OnChange := OnChangeType;

   btnRemove := TButton.Create(Self);
   btnRemove.Parent := Self;
   btnRemove.ParentFont := False;
   btnRemove.Font.Style := [];
   btnRemove.DoubleBuffered := True;
   btnRemove.Caption := i18Manager.GetString('btnRemove');
   btnRemove.OnClick := OnClickRemove;
   var w := TInfra.GetAutoWidth(btnRemove);
   btnRemove.SetBounds(Parent.Width-w-TInfra.Scaled(Self, 32), 0, w+14, TInfra.Scaled(Self, 20));
end;

function TElement.GetParentTab: TTabSheet;
begin
   result := nil;
   var winControl := Parent;
   while not (winControl is TForm) do
   begin
      if winControl is TTabComponent then
      begin
         result := TTabComponent(winControl);
         break;
      end;
      winControl := winControl.Parent;
   end;
end;

procedure TElement.OnClickRemove(Sender: TObject);
begin
   Hide;
   if Parent.Height < Parent.Constraints.MaxHeight then
      Parent.Height := Parent.Height - Height;
   TTabComponent(FParentTab).RefreshElements;
   FParentTab.PageControl.Refresh;
   GProject.SetChanged;
   TTabComponent(FParentTab).UpdateCodeEditor;
   TThread.ForceQueue(nil, Free);
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
   result := True;
   if edtName.Enabled and ((edtName.Font.Color = NOK_COLOR) or ((Trim(edtName.Text) = '') and not edtName.Focused)) then
      result := False;
end;

procedure TElement.OnChangeName(Sender: TObject);
begin
   var lColor := NOK_COLOR;
   var info := '';
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
   FParentTab.PageControl.Refresh;
   GProject.SetChanged;
   if FParentForm.UpdateCodeEditor then
      TTabComponent(FParentTab).UpdateCodeEditor;
end;

procedure TElement.ImportFromXML(ANode: IXMLNode);
begin
   edtName.Text :=  GetNodeAttrStr(ANode, NAME_ATTR);
   var idx := cbType.Items.IndexOf(GetNodeAttrStr(ANode, TYPE_ATTR));
   if idx <> -1 then
      cbType.ItemIndex := idx
   else if cbType.Items.Count > 0 then
      cbType.ItemIndex := 0;
   cbType.Hint := cbType.Text;
end;

function TElement.ExportToXML(ANode: IXMLNode): IXMLNode;
begin
   result := AppendNode(ANode, FElementTypeID);
   SetNodeAttrStr(result, NAME_ATTR, Trim(edtName.Text));
   SetNodeAttrStr(result, TYPE_ATTR, cbType.Text);
end;

procedure TElement.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if (Source = Self) or (not (Source is TElement)) or (TElement(Source).Parent <> Parent) then
      Accept := False;
end;

procedure TElement.DragDrop(Source: TObject; X, Y: Integer);
begin
   if Source is TElement then
   begin
      var sourceElement := TElement(Source);
      var t := sourceElement.Top;
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
