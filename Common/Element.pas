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
   Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, OmniXml, PageControl_Form;

type
   
   TElement = class(TPanel)
      private
         FParentTab: TTabSheet;
         FParentForm: TPageControlForm;
         function GetParentTab: TTabSheet;
      protected
         FElementTypeID: string;
         constructor Create(AParent: TScrollBox);
         procedure OnClickRemove(Sender: TObject);
         procedure OnChangeType(Sender: TObject);
         procedure OnChangeName(Sender: TObject); virtual;
         procedure UpdateMe;
      public
         edtName: TEdit;
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
   Vcl.Controls, Vcl.Graphics, System.SysUtils, ApplicationCommon, TabComponent;

constructor TElement.Create(AParent: TScrollBox);
begin

   inherited Create(AParent);
   Parent := AParent;
   
   Ctl3D := false;
   BevelOuter := bvNone;
   FParentTab := GetParentTab;
   FParentForm := TTabComponent(FParentTab).ParentForm;
   DoubleBuffered := true;

   edtName := TEdit.Create(Self);
   edtName.Parent := Self;
   edtName.SetBounds(3, 0, 70, 21);
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
   cbType.SetBounds(87, 0, 70, 21);
   cbType.Constraints.MaxWidth := 74;
   cbType.Parent := Self;
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
   FParentForm.UpdateCodeEditor := false;
   TTabComponent(FParentTab).RefreshElements;
   FParentForm.UpdateCodeEditor := true;
   UpdateMe;
   TTabComponent(FParentTab).UpdateCodeEditor;
end;

procedure TElement.OnChangeType(Sender: TObject);
begin
   cbType.Hint := cbType.Text;
   if FParentTab.Font.Color <> NOK_COLOR then
      TTabComponent(FParentTab).UpdateCodeEditor;
end;

function TElement.IsValid: boolean;
begin
   result := true;
   if edtName.Enabled and ((edtName.Font.Color = NOK_COLOR) or Trim(edtName.Text).IsEmpty) then
      result := false;
end;

procedure TElement.OnChangeName(Sender: TObject);
var
   info: string;
   lColor: TColor;
begin
   lColor := OK_COLOR;
   info := 'OkIdD';
   if GInfra.ValidateId(edtName.Text) <> VALID_IDENT then
   begin
      lColor := NOK_COLOR;
      info := 'BadIdD';
   end
   else if TTabComponent(FParentTab).IsDuplicatedElement(Self) then
   begin
      lColor := NOK_COLOR;
      info := 'DupIdD';
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

end.
