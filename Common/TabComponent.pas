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
   System.Classes, Vcl.ComCtrls, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, WinApi.Windows,
   Vcl.Graphics, WinApi.Messages, CommonInterfaces, OmniXML, Element, PageControl_Form,
   CommonTypes;

type

   TTabComponent = class(TTabSheet, IXMLable, IIdentifiable, ITabbable, ISizeEditable, ISortable, IFocusable, IExportable)
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
         procedure OnClickCh(Sender: TObject);
         procedure OnChangeName(Sender: TObject); virtual;
         procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
         procedure CreateExtDeclareChBox(AParent: TWinControl; x, y: integer);
         procedure CreateNameControls(AParent: TWinControl; x, y: integer);
         procedure CreateLibControls(AParent: TWinControl; x, y: integer);
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
         function GetElements<T: class>: IEnumerable<T>;
         procedure ExportToXMLTag(ATag: IXMLElement); virtual;
         function ExportToXMLFile(const AFile: string): TErrorType;
         procedure ExportToGraphic(const AGraphic: TGraphic);
         function GetExportFileName: string;
         function IsDuplicated(ANameEdit: TEdit): boolean;
         procedure Localize(const AList: TStringList); virtual;
         procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil); virtual;
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
   System.SysUtils, Generics.Collections, System.StrUtils, ApplicationCommon, XMLProcessor,
   SortListDecorator, BaseEnumerator;

constructor TTabComponent.Create(const AParentForm: TPageControlForm);
begin
   inherited Create(AParentForm.pgcTabs);
   PageControl := AParentForm.pgcTabs;
   ParentFont := false;
   ParentBackground := false;
   Brush.Color := AParentForm.Color;
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

procedure TTabComponent.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
   FillRect(Msg.DC, ClientRect, Brush.Handle);
   Msg.Result := 1;
end;

procedure TTabComponent.UpdateCodeEditor;
begin
   if not chkExtDeclare.Checked then
      TInfra.UpdateCodeEditor(Self);
end;

function TTabComponent.RetrieveFocus(AInfo: TFocusInfo): boolean;
var
   elem: TElement;
begin
   if FActive then
   begin
      FParentForm.Show;
      Show;
      if not AInfo.SelText.IsEmpty then
      begin
         for elem in GetElements<TElement> do
         begin
            if SameText(Trim(elem.edtName.Text), AInfo.SelText) then
            begin
               if elem.edtName.CanFocus and (AInfo.ActiveControl = nil) then
                  elem.edtName.SetFocus;
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
   GProject.SetChanged;
   if (Font.Color <> NOK_COLOR) and chkExtDeclare.Checked then
      TInfra.UpdateCodeEditor(Self);
end;

function TTabComponent.ExportToXMLFile(const AFile: string): TErrorType;
begin
   result := TXMLProcessor.ExportToXMLFile(ExportToXMLTag, AFile);
end;

function TTabComponent.GetExportFileName: string;
begin
   result := edtName.Text;
end;

procedure TTabComponent.ExportToGraphic(const AGraphic: TGraphic);
var
   bitmap: TBitmap;
begin
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   bitmap.Width := Width;
   bitmap.Height := Height;
   bitmap.Canvas.Lock;
   PaintTo(bitmap.Canvas, 0, 0);
   bitmap.Canvas.Unlock;
   if AGraphic <> bitmap then
   begin
      AGraphic.Assign(bitmap);
      bitmap.Free;
   end;
end;

procedure TTabComponent.CreateExtDeclareChBox(AParent: TWinControl; x, y: integer);
begin
   chkExtDeclare := TCheckBox.Create(AParent);
   chkExtDeclare.Parent := AParent;
   chkExtDeclare.Caption := i18Manager.GetString('chkExtDeclare');
   chkExtDeclare.SetBounds(x, y, PageControl.Canvas.TextWidth(chkExtDeclare.Caption) + 25, 17);
   chkExtDeclare.ParentFont := false;
   chkExtDeclare.Font.Style := [];
   chkExtDeclare.Font.Color := clWindowText;
   chkExtDeclare.DoubleBuffered := true;
   chkExtDeclare.OnClick := OnClickCh;
   chkExtDeclare.ShowHint := true;
   chkExtDeclare.Hint := i18Manager.GetString('chkExtDeclare.Hint');
end;

procedure TTabComponent.CreateNameControls(AParent: TWinControl; x, y: integer);
begin
   lblName := TLabel.Create(AParent);
   lblName.Parent := AParent;
   lblName.SetBounds(x, y, 0, 13);
   lblName.Caption := i18Manager.GetString('lblName');
   lblName.ParentFont := false;
   lblName.Font.Style := [];
   lblName.Font.Color := clWindowText;

   edtName := TEdit.Create(AParent);
   edtName.Parent := AParent;
   edtName.SetBounds(lblName.BoundsRect.Right+5, y-6, 84, 21);
   edtName.ParentFont := false;
   edtName.Font.Style := [];
   edtName.ShowHint := true;
   edtName.Hint := i18Manager.GetString('BadIdD');
   edtName.DoubleBuffered := true;
   edtName.OnChange := OnChangeName;
end;

procedure TTabComponent.CreateLibControls(AParent: TWinControl; x, y: integer);
begin
   lblLibrary := TLabel.Create(AParent);
   lblLibrary.Parent := AParent;
   lblLibrary.SetBounds(x, y, 0, 13);
   lblLibrary.Caption := i18Manager.GetString('lblLibrary');
   lblLibrary.ParentFont := false;
   lblLibrary.Font.Style := [];
   lblLibrary.Font.Color := clWindowText;

   edtLibrary := TEdit.Create(AParent);
   edtLibrary.Parent := AParent;
   edtLibrary.SetBounds(lblLibrary.BoundsRect.Right+5, y-6, 115-lblLibrary.Width, 21);
   edtLibrary.ParentFont := false;
   edtLibrary.Font.Style := [];
   edtLibrary.Font.Color := clGreen;
   edtLibrary.ShowHint := true;
   edtLibrary.DoubleBuffered := true;
   edtLibrary.OnChange := OnChangeLib;
   edtLibrary.Hint := ReplaceStr(i18Manager.GetFormattedString('edtLibHintType', [GInfra.CurrentLang.LibraryExt]), LB_PHOLDER2, sLineBreak);
end;

procedure TTabComponent.OnClickCh(Sender: TObject);
begin
   GProject.SetChanged;
   if Font.Color <> NOK_COLOR then
      TInfra.UpdateCodeEditor(Self);
end;

procedure TTabComponent.SetActive(const AValue: boolean);
var
   i: integer;
   tab: TTabComponent;
begin
   if AValue <> FActive then
   begin
      FActive := AValue;
      TabVisible := FActive;
      GProject.SetChanged;
      FParentForm.UpdateCodeEditor := false;
      for i := 0 to PageControl.PageCount-1 do
      begin
         tab := TTabComponent(PageControl.Pages[i]);
         if tab.TabVisible then
            tab.edtName.OnChange(tab.edtName);
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
   tab: TTabComponent;
   i: integer;
begin
   result := false;
   if ANameEdit <> nil then
   begin
      for i := 0 to PageControl.PageCount-1 do
      begin
         tab := TTabComponent(PageControl.Pages[i]);
         if tab.TabVisible and (tab.edtName <> ANameEdit) and TInfra.SameStrings(Trim(tab.edtName.Text), Trim(ANameEdit.Text)) then
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

function TTabComponent.GetElements<T>: IEnumerable<T>;
var
   i: integer;
   list: TList<T>;
   decorList: TSortListDecorator<T>;
begin
   list := TList<T>.Create;
   if list.Capacity < sbxElements.ControlCount then
      list.Capacity := sbxElements.ControlCount;
   for i := 0 to sbxElements.ControlCount-1 do
      list.Add(sbxElements.Controls[i]);
   if list.Count > 1 then
   begin
      decorList := TSortListDecorator<T>.Create(list, 0);
      decorList.Sort;
      decorList.Free;
   end;
   result := TEnumeratorFactory<T>.Create(list);
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
   elem: TElement;
begin
   SendMessage(sbxElements.Handle, WM_SETREDRAW, WPARAM(False), 0);
   try
      elem := CreateElement;
      sbxElements.Height := sbxElements.Height + 22;
   finally
      SendMessage(sbxElements.Handle, WM_SETREDRAW, WPARAM(True), 0);
      RedrawWindow(sbxElements.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE or RDW_ALLCHILDREN);
   end;
   if elem.edtName.CanFocus then
      elem.edtName.SetFocus;
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
   GProject.SetChanged;
end;

function TTabComponent.HasInvalidElement: boolean;
var
   elem: TElement;
begin
   result := false;
   for elem in GetElements<TElement> do
   begin
      if not elem.IsValid then
      begin
         result := true;
         break;
      end;
   end;
end;

function TTabComponent.HasFocusedComboBox: boolean;
var
   hnd: THandle;
   elem: TElement;
begin
   result := false;
   hnd := GetFocus();
   for elem in GetElements<TElement> do
   begin
      if elem.cbType.Handle = hnd then
      begin
         result := true;
         break;
      end;
   end;
end;

function TTabComponent.IsDuplicatedElement(const AElement: TElement): boolean;
var
   elem: TElement;
begin
   result := false;
   if (AElement <> nil) and (AElement.ParentTab = Self) then
   begin
      for elem in GetElements<TElement> do
      begin
         if (elem <> AElement) and TInfra.SameStrings(Trim(AElement.edtName.Text), Trim(elem.edtName.Text)) then
         begin
            result := true;
            break;
         end;
      end;
   end;
end;

procedure TTabComponent.RefreshElements;
var
   elem: TElement;
begin
   FParentForm.UpdateCodeEditor := false;
   for elem in GetElements<TElement> do
      elem.edtName.OnChange(elem.edtName);
   FParentForm.UpdateCodeEditor := true;
end;

procedure TTabComponent.ExportToXMLTag(ATag: IXMLElement);
var
   elem: TElement;
begin
   ATag.SetAttribute(NAME_ATTR, Trim(edtName.Text));
   ATag.SetAttribute(ID_ATTR, FId.ToString);
   ATag.SetAttribute('ext_decl', chkExtDeclare.Checked.ToString);
   ATag.SetAttribute('library', Trim(edtLibrary.Text));
   for elem in GetElements<TElement> do
      elem.ExportToXMLTag(ATag);
end;

procedure TTabComponent.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
var
   elem: TElement;
   tag: IXMLElement;
begin
   edtName.Text := ATag.GetAttribute(NAME_ATTR);
   edtName.OnChange(edtName);
   chkExtDeclare.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'ext_decl');
   edtLibrary.Text := ATag.GetAttribute('library');
   tag := TXMLProcessor.FindChildTag(ATag, FElementMode);
   while tag <> nil do
   begin
      elem := CreateElement;
      sbxElements.Constraints.MaxHeight := sbxElements.Constraints.MaxHeight + 22;
      sbxElements.Height := sbxElements.Height + 22;
      elem.ImportFromXMLTag(tag);
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   FId := GProject.Register(Self, StrToIntDef(ATag.GetAttribute(ID_ATTR), ID_INVALID));
end;

procedure TTabComponent.Localize(const AList: TStringList);
var
   a: integer;
   elem: TElement;
begin
   lblName.Caption := AList.Values['lblName'];
   chkExtDeclare.Caption := AList.Values['chkExtDeclare'];
   lblLibrary.Caption := AList.Values['lblLibrary'];
   edtName.OnChange(edtName);
   for a := 0 to sbxElements.ControlCount-1 do
   begin
      elem := TElement(sbxElements.Controls[a]);
      elem.btnRemove.Caption := AList.Values['btnRemove'];
      elem.edtName.OnChange(elem.edtName);
   end;
end;

procedure TTabComponent.ScrollElements(const AValue: integer);
begin
   sbxElements.VertScrollBar.Position := sbxElements.VertScrollBar.Position + AValue;
end;

function TTabComponent.GetFocusColor: TColor;
begin
   if HasParent then
      result := Font.Color
   else
      result := OK_COLOR;
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
