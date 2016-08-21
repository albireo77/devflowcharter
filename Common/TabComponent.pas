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



unit TabComponent;

interface

uses
   Classes, ComCtrls, Forms, CommonInterfaces, OmniXML, StdCtrls, Element, ExtCtrls,
   Controls, PageControl_Form, Graphics, Menus;

type

   TElementIteratorFriend = class(TElementIterator)
   end;

   TTabComponent = class(TTabSheet, IXMLable, IIdentifiable, ITabbable, ISizeEditable, ISortable, IFocusable)
      private
         FParentForm: TPageControlForm;
         FId: integer;
      protected
         FOverlayObject: TComponent;
         FActive: boolean;
         FElementMode: string;
         sbxElements: TScrollBox;
         procedure SetActive(const AValue: boolean); virtual;
         function GetActive: boolean; virtual;
         procedure AddElement(Sender: TObject); virtual;
         function GetId: integer;
         function CreateElement: TElement; virtual; abstract;
         function GetElementCount: integer;
         function GetScrollPos: integer;
         procedure SetScrollPos(AValue: integer);
         procedure OnChangeLib(Sender: TObject);
         procedure OnChangeName(Sender: TObject); virtual;
         function GetElementIterator: IIterator;
      public
         edtName: TEdit;
         chkExtDeclare: TCheckBox;
         edtLibrary: TEdit;
         lblName: TLabel;
         lblLibrary: TLabel;
         btnAddElement: TButton;
         property Active: boolean read FActive write SetActive;
         property OverlayObject: TComponent read FOverlayObject write FOverlayObject;
         property Id: integer read GetId;
         property ElementMode: string read FElementMode;
         property ParentForm: TPageControlForm read FParentForm;
         constructor Create(const AParentForm: TPageControlForm);
         destructor Destroy; override;
         procedure ExportToXMLTag(const ATag: IXMLElement); virtual;
         function IsDuplicated(ANameEdit: TEdit): boolean;
         procedure Localize(const AList: TStringList); virtual;
         procedure ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil); virtual;
         function GetLibName: string;
         procedure ScrollElements(const AValue: integer);
         property ScrollPos: integer read GetScrollPos write SetScrollPos;
         function GetName: string;
         procedure RefreshSizeEdits; virtual; abstract;
         function GetSortValue(const ASortType: integer): integer; virtual;
         function RetrieveFocus(AInfo: TFocusInfo): boolean;
         function CanBeFocused: boolean;
         function IsDuplicatedElement(const AElement: TElement): boolean;
         procedure RefreshElements;
         function HasInvalidElement: boolean;
         function HasFocusedComboBox: boolean;
         function GetFocusColor: TColor;
         function Remove: boolean;
         function CanBeRemoved: boolean;
         function IsBoldDesc: boolean;
         procedure RefreshTab;
         procedure UpdateCodeEditor;
   end;

implementation

uses
   ApplicationCommon, SysUtils, XMLProcessor, Contnrs, SortListDecorator, Windows,
   Messages;

constructor TTabComponent.Create(const AParentForm: TPageControlForm);
begin
   inherited Create(AParentForm.pgcTabs);
   PageControl := AParentForm.pgcTabs;
   ParentFont := false;
   Align := alClient;
   Font.Color := NOK_COLOR;
   Font.Style := [fsBold];
   DoubleBuffered := true;
   FOverlayObject := Self;
   FActive := true;
   FId := GProject.Register(Self);
   FParentForm := AParentForm;
end;

destructor TTabComponent.Destroy;
begin
   GProject.UnRegister(Self);
   inherited Destroy;
end;

procedure TTabComponent.UpdateCodeEditor;
begin
   if not chkExtDeclare.Checked then
      TInfra.UpdateCodeEditor(Self);
end;

function TTabComponent.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   iter: IIterator;
   lElem: TElement;
begin
   if FActive then
   begin
      FParentForm.Show;
      Show;
      if AInfo.SelText <> '' then
      begin
         iter := GetElementIterator;
         while iter.HasNext do
         begin
            lElem := TElement(iter.Next);
            if AnsiSameText(Trim(lElem.edtName.Text), AInfo.SelText) then
            begin
               if lElem.edtName.CanFocus and (AInfo.ActiveControl = nil) then
                  lElem.edtName.SetFocus;
               break;
            end;
         end;
      end;
      if (AInfo.ActiveControl <> nil) and AInfo.ActiveControl.CanFocus then
         AInfo.ActiveControl.SetFocus;
   end;
   result := FActive;
end;

function TTabComponent.CanBeFocused: boolean;
begin
   result := FActive;
end;

procedure TTabComponent.OnChangeLib(Sender: TObject);
begin
   if (Font.Color <> NOK_COLOR) and chkExtDeclare.Checked then
      TInfra.UpdateCodeEditor(Self);
end;

procedure TTabComponent.SetActive(const AValue: boolean);
var
   i: integer;
   lTab: TTabComponent;
begin
   if AValue <> FActive then
   begin
      FActive := AValue;
      TabVisible := FActive;
      GChange := 1;
      FParentForm.UpdateCodeEditor := false;
      for i := 0 to PageControl.PageCount-1 do
      begin
         lTab := TTabComponent(PageControl.Pages[i]);
         if lTab.TabVisible then
            lTab.edtName.OnChange(lTab.edtName);
      end;
      FParentForm.UpdateCodeEditor := true;
   end;
end;

function TTabComponent.GetLibName: string;
begin
   result := '';
   if FActive and chkExtDeclare.Checked and (Font.Color <> NOK_COLOR) then
      result := Trim(edtLibrary.Text);
end;

function TTabComponent.GetActive: boolean;
begin
   result := FActive;
end;

function TTabComponent.GetElementCount: integer;
begin
   result := sbxElements.ControlCount;
end;

function TTabComponent.GetSortValue(const ASortType: integer): integer;
begin
   result := -1;
   if ASortType = PAGE_INDEX_SORT then
      result := PageIndex;
end;

function TTabComponent.IsDuplicated(ANameEdit: TEdit): boolean;
var
   lTab: TTabComponent;
   i: integer;
begin
   result := false;
   if ANameEdit <> nil then
   begin
      for i := 0 to PageControl.PageCount-1 do
      begin
         lTab := TTabComponent(PageControl.Pages[i]);
         if lTab.TabVisible and (lTab.edtName <> ANameEdit) and TInfra.SameStrings(Trim(lTab.edtName.Text), Trim(ANameEdit.Text)) then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

procedure TTabComponent.RefreshTab;
var
   lColor: TColor;
begin
   lColor := edtName.Font.Color;
   if HasInvalidElement then
      lColor := NOK_COLOR;
   Font.Color := lColor;
end;

function TTabComponent.GetElementIterator: IIterator;
var
   i: integer;
   lList: TObjectList;
   lListSortWrap: TSortListDecorator;
begin
   lList := TObjectList.Create(false);
   if lList.Capacity < sbxElements.ControlCount then
      lList.Capacity := sbxElements.ControlCount;
   for i := 0 to sbxElements.ControlCount-1 do
      lList.Add(sbxElements.Controls[i]);
   if lList.Count > 1 then
   begin
      lListSortWrap := TSortListDecorator.Create(lList, 0);
      lListSortWrap.Sort;
      lListSortWrap.Free;
   end;
   result := TElementIteratorFriend.Create(lList);
end;

function TTabComponent.GetScrollPos: integer;
begin
   result := sbxElements.VertScrollBar.Position;
end;

procedure TTabComponent.SetScrollPos(AValue: integer);
begin
   sbxElements.VertScrollBar.Position := AValue;
end;

procedure TTabComponent.AddElement(Sender: TObject);
var
   lElem: TElement;
begin
   SendMessage(sbxElements.Handle, WM_SETREDRAW, WPARAM(False), 0);
   try
      lElem := CreateElement;
      sbxElements.Height := sbxElements.Height + 22;
   finally
      SendMessage(sbxElements.Handle, WM_SETREDRAW, WPARAM(True), 0);
      RedrawWindow(sbxElements.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE or RDW_ALLCHILDREN);
   end;
   if lElem.edtName.CanFocus then
      lElem.edtName.SetFocus;
   PageControl.Refresh;
   UpdateCodeEditor;
end;

function TTabComponent.GetName: string;
begin
   result := '';
   if FActive and (Font.Color <> NOK_COLOR) then
      result := Trim(edtName.Text);
end;


function TTabComponent.GetId: integer;
begin
   result := FId;
end;

procedure TTabComponent.OnChangeName(Sender: TObject);
begin
   Caption := Trim(edtName.Text);
   PageControl.Refresh;
   if FParentForm.UpdateCodeEditor then
      UpdateCodeEditor;
   GChange := 1;
end;

function TTabComponent.HasInvalidElement: boolean;
var
   iter: IIterator;
begin
   result := false;
   iter := GetElementIterator;
   while iter.HasNext do
   begin
      if not TElement(iter.Next).IsValid then
      begin
         result := true;
         break;
      end;
   end;
end;

function TTabComponent.HasFocusedComboBox: boolean;
var
   lHandle: THandle;
   iter: IIterator;
begin
   result := false;
   lHandle := GetFocus();
   iter := GetElementIterator;
   while iter.HasNext do
   begin
      if TElement(iter.Next).cbType.Handle = lHandle then
      begin
         result := true;
         break;
      end;
   end;
end;

function TTabComponent.IsDuplicatedElement(const AElement: TElement): boolean;
var
   lElement: TElement;
   iter: IIterator;
begin
   result := false;
   if (AElement <> nil) and (AElement.ParentTab = Self) then
   begin
      iter := GetElementIterator;
      while iter.HasNext do
      begin
         lElement := TElement(iter.Next);
         if (lElement <> AElement) and TInfra.SameStrings(Trim(AElement.edtName.Text), Trim(lElement.edtName.Text)) then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

procedure TTabComponent.RefreshElements;
var
   iter: IIterator;
   lElement: TElement;
begin
   FParentForm.UpdateCodeEditor := false;
   iter := GetElementIterator;
   while iter.HasNext do
   begin
      lElement := TElement(iter.Next);
      lElement.edtName.OnChange(lElement.edtName);
   end;
   FParentForm.UpdateCodeEditor := true;
end;

procedure TTabComponent.ExportToXMLTag(const ATag: IXMLElement);
var
   iter: IIterator;
begin
   ATag.SetAttribute(NAME_ATTR, Trim(edtName.Text));
   ATag.SetAttribute(ID_ATTR, IntToStr(FId));
   ATag.SetAttribute('ext_decl', BoolToStr(chkExtDeclare.Checked, true));
   ATag.SetAttribute('library', Trim(edtLibrary.Text));
   iter := GetElementIterator;
   while iter.HasNext do
      TElement(iter.Next).ExportToXMLTag(ATag);
end;

procedure TTabComponent.ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
var
   lElem: TElement;
   tag: IXMLElement;
begin
   edtName.Text := ATag.GetAttribute(NAME_ATTR);
   edtName.OnChange(edtName);
   chkExtDeclare.Checked := ATag.GetAttribute('ext_decl') = 'True';
   edtLibrary.Text := ATag.GetAttribute('library');
   tag := TXMLProcessor.FindChildTag(ATag, FElementMode);
   while tag <> nil do
   begin
      lElem := CreateElement;
      sbxElements.Constraints.MaxHeight := sbxElements.Constraints.MaxHeight + 22;
      sbxElements.Height := sbxElements.Height + 22;
      lElem.ImportFromXMLTag(tag);
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   FId := GProject.Register(Self, StrToIntDef(ATag.GetAttribute(ID_ATTR), ID_INVALID));
end;

procedure TTabComponent.Localize(const AList: TStringList);
var
   a: integer;
   lElem: TElement;
begin
   lblName.Caption := AList.Values['lblName'];
   chkExtDeclare.Caption := AList.Values['chkExtDeclare'];
   lblLibrary.Caption := AList.Values['lblLibrary'];
   edtName.OnChange(edtName);
   for a := 0 to sbxElements.ControlCount-1 do
   begin
      lElem := TElement(sbxElements.Controls[a]);
      lElem.btnRemove.Caption := AList.Values['btnRemove'];
      lElem.edtName.OnChange(lElem.edtName);
   end;
end;

procedure TTabComponent.ScrollElements(const AValue: integer);
begin
   sbxElements.VertScrollBar.Position := sbxElements.VertScrollBar.Position + AValue;
end;

function TTabComponent.GetFocusColor: TColor;
begin
   result := Font.Color;
end;

function TTabComponent.Remove: boolean;
begin
   result := CanBeRemoved;
   if result then
   begin
      FParentForm.pgcTabs.ActivePage := Self;
      FParentForm.miRemove.Click;
   end;
end;

function TTabComponent.CanBeRemoved: boolean;
begin
   result := FActive;
end;

function TTabComponent.IsBoldDesc: boolean;
begin
   result := false;
end;

end.
