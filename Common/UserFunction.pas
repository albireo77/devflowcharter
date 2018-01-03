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



unit UserFunction;

interface

uses
   Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, System.Classes, System.Types,
   Generics.Defaults, OmniXML, Main_Block, DeclareList, CommonInterfaces, TabComponent,
   Element, Functions_Form;

type

   TUserFunction = class;
   TUserFunctionHeader = class;

   TUserFunctionComparer = class(TComparer<TUserFunction>)
      FCompareType: integer;
      constructor Create(ACompareType: integer);
      function Compare(const L, R: TUserFunction): integer; override;
   end;

   TParameter = class(TElement)
      constructor Create(AParentTab: TUserFunctionHeader);
   public
      chkTable: TCheckBox;
      chkReference: TCheckBox;
      edtDefault: TEdit;
      function ExportToXMLTag(ATag: IXMLElement): IXMLElement; override;
      procedure ImportFromXMLTag(ATag: IXMLElement); override;
   end;

   TUserFunctionHeader = class(TTabComponent)
   private
      FUserFunction: TUserFunction;
      FLocalVars: TVarDeclareList;
   protected
      procedure OnChangeName(Sender: TObject); override;
      procedure OnChangeDesc(Sender: TObject);
      procedure OnClickInclDescFlow(Sender: TObject);
      procedure OnClickInclDescCode(Sender: TObject);
      procedure OnClickBodyVisible(Sender: TObject);
      procedure OnChangeType(Sender: TObject);
      procedure OnMovedParams(Sender: TObject);
      procedure SetActive(AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure OnChangeBodyPage(Sender: TObject);
      procedure OnDropDownBodyPage(Sender: TObject);
      procedure DrawBodyLabel;
   public
      cbType,
      cbBodyPage: TComboBox;
      lblType,
      lblBodyPage,
      lblParams: TLabel;
      gbHeader,
      gbBody,
      gbParams,
      gbDesc: TGroupBox;
      memDesc: TMemo;
      chkInclDescCode,
      chkInclDescFlow,
      chkBodyVisible: TCheckBox;
      splDesc,
      splParams: TSplitter;
      property UserFunction: TUserFunction read FUserFunction;
      property LocalVars: TVarDeclareList read FLocalVars;
      property ParameterCount: integer read GetElementCount;
      constructor Create(AParentForm: TFunctionsForm);
      destructor Destroy; override;
      procedure ExportToXMLTag(ATag: IXMLElement); override;
      procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
      procedure Localize(AList: TStringList); override;
      procedure RefreshSizeEdits; override;
      procedure GenerateDescription(ALines: TStrings);
      procedure SetPageCombo(const ACaption: TCaption = '');
      function GetParameters: IEnumerable<TParameter>;
   end;

   TUserFunction = class(TComponent, IXMLable, ITabbable, IIdentifiable, ISizeEditable, IWinControl, IGenericComparable)
   private
      FHeader: TUserFunctionHeader;
      FBody: TMainBlock;
      FActive: boolean;
      procedure SetActive(AValue: boolean);
      function GetActive: boolean;
   public
      property Header: TUserFunctionHeader read FHeader;
      property Body: TMainBlock read FBody;
      property Active: boolean read GetActive write SetActive;
      constructor Create(AFunctionHeader: TUserFunctionHeader; AFunctionBody: TMainBlock);
      destructor Destroy; override;
      procedure ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
      procedure ExportToXMLTag(ATag: IXMLElement);
      procedure GenerateTree(ANode: TTreeNode);
      function GetId: integer;
      function GetLibName: string;
      procedure RefreshSizeEdits;
      function GetName: string;
      function GetHandle: THandle;
      procedure BringAllToFront;
      procedure SetZOrder(AValue: integer);
      function GetZOrder: integer;
      function IsMain: boolean;
      function GetCompareValue(ACompareType: integer): integer;
   end;

implementation

uses
   Vcl.Forms, Vcl.Graphics, System.SysUtils, System.StrUtils, Vcl.Grids, ApplicationCommon,
   Main_Form, XMLProcessor, LangDefinition, Navigator_Form, BlockTabSheet;

constructor TUserFunction.Create(AFunctionHeader: TUserFunctionHeader; AFunctionBody: TMainBlock);
begin
   inherited Create(Application);
   GProject.AddComponent(Self);
   FBody := AFunctionBody;
   FHeader := AFunctionHeader;
   FActive := true;
   GProject.LastUserFunction := Self;
   if FHeader <> nil then
   begin
      FHeader.FUserFunction := Self;
      FHeader.FOverlayObject := Self;
      if FBody <> nil then
      begin
         FHeader.SetPageCombo(FBody.Page.Caption);
         FHeader.chkBodyVisible.OnClick(FHeader.chkBodyVisible);
      end;
   end;
   if FBody <> nil then
   begin
      FBody.UserFunction := Self;
      FBody.SetWidth(FBody.Width);
      FBody.Page.Box.SetScrollBars;
   end;
end;

destructor TUserFunction.Destroy;
begin
   if GProject.LastUserFunction = Self then
      GProject.LastUserFunction := nil;
   FBody.Free;
   FBody := nil;
   FHeader.Free;
   FHeader := nil;
   inherited Destroy;
end;

procedure TUserFunction.RefreshSizeEdits;
begin
   if FHeader <> nil then
      FHeader.RefreshSizeEdits;
end;

procedure TUserFunctionHeader.RefreshSizeEdits;
begin
   if FLocalVars.edtSize.Text <> '1' then
      FLocalVars.edtSize.OnChange(FLocalVars.edtSize);
end;

function TUserFunction.GetId: integer;
begin
   result := ID_INVALID;
   if FHeader <> nil then
      result := FHeader.Id;
end;

procedure TUserFunction.BringAllToFront;
begin
   if FBody <> nil then
      FBody.BringAllToFront;
end;

procedure TUserFunction.SetZOrder(AValue: integer);
begin
   if FBody <> nil then
      FBody.SetZOrder(AValue);
end;

function TUserFunction.GetZOrder: integer;
begin
   result := -1;
   if FBody <> nil then
      result := FBody.GetZOrder;
end;

function TUserFunction.GetLibName: string;
begin
   result := '';
   if FHeader <> nil then
      result := FHeader.GetLibName;
end;

destructor TUserFunctionHeader.Destroy;
begin
   FLocalVars.Free;
   inherited Destroy;
end;

procedure TUserFunctionHeader.SetActive(AValue: boolean);
begin
   if AValue <> FActive then
   begin
      inherited SetActive(AValue);
      if (FUserFunction <> nil) and (FUserFunction.Active <> AValue) then
         FUserFunction.Active := AValue;
   end;
end;

procedure TUserFunctionHeader.GenerateDescription(ALines: TStrings);
var
   i: integer;
begin
   if chkInclDescCode.Checked and (memDesc.Text <> '') then
   begin
      for i := 0 to memDesc.Lines.Count-1 do
         ALines.Add(memDesc.Lines[i]);
      if EndsText(sLineBreak, memDesc.Text) then
         ALines.Add('');
   end;
end;

procedure TUserFunction.SetActive(AValue: boolean);
var
   vis: boolean;
begin
   if FActive <> AValue then
   begin
      FActive := AValue;
      if FBody <> nil then
      begin
         vis := AValue;
         if FHeader <> nil then
            vis := vis and FHeader.chkBodyVisible.Checked;
         FBody.SetVisible(vis);
         if FBody.Visible then
            FBody.RefreshStatements;
         FBody.Page.Box.SetScrollBars;
      end;
      if (FHeader <> nil) and (AValue <> FHeader.Active) then
         FHeader.Active := AValue;
   end;
end;

function TUserFunction.GetName: string;
begin
   result := '';
   if FHeader <> nil then
      result := FHeader.GetName;
end;

function TUserFunction.GetActive: boolean;
begin
   result := FActive;
end;

procedure TUserFunction.ExportToXMLTag(ATag: IXMLElement);
var
   tag: IXMLElement;
begin
   if FHeader <> nil then
      FHeader.ExportToXMLTag(ATag)
   else if FBody <> nil then
   begin
      tag := ATag.OwnerDocument.CreateElement(FUNCTION_TAG);
      ATag.AppendChild(tag);
      FBody.ExportToXMLTag(tag);
   end;
end;

procedure TUserFunction.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
begin
{}
end;

constructor TUserFunctionHeader.Create(AParentForm: TFunctionsForm);
var
   l: integer;
begin

   inherited Create(AParentForm);

   FElementMode := PARAMETER_IDENT;

   FLocalVars := TVarDeclareList.Create(Self, 0, 350, 389, 3, 4, 380);
   FLocalVars.Caption := i18Manager.GetString('LocalDeclare');
   FLocalVars.sgList.Options := FLocalVars.sgList.Options - [goColSizing];
   FLocalVars.gbBox.DoubleBuffered := true;

   gbDesc := TGroupBox.Create(Self);
   gbDesc.Parent := Self;
   gbDesc.SetBounds(0, 0, 400, 77);
   gbDesc.ParentFont := false;
   gbDesc.ParentBackground := false;
   gbDesc.Font.Color := clBlack;
   gbDesc.Align := alTop;
   gbDesc.Caption := i18Manager.GetString('gbDesc');
   gbDesc.DoubleBuffered := true;
   gbDesc.Constraints.MinHeight := gbDesc.Height;

   splDesc := TSplitter.Create(Self);
   splDesc.Parent := Self;
   splDesc.Align := gbDesc.Align;

   memDesc := TMemo.Create(gbDesc);
   memDesc.Parent := gbDesc;
   memDesc.SetBounds(5, 17, 378, 35);
   memDesc.ParentFont := false;
   memDesc.Font.Style := [];
   memDesc.Font.Color := clGreen;
   memDesc.WordWrap := false;
   memDesc.DoubleBuffered := true;
   memDesc.ScrollBars := ssVertical;
   memDesc.Anchors := [akTop, akBottom, akLeft, akRight];
   memDesc.OnChange := OnChangeDesc;

   chkInclDescCode := TCheckBox.Create(gbDesc);
   chkInclDescCode.Parent := gbDesc;
   chkInclDescCode.ParentFont := false;
   chkInclDescCode.Font.Style := [];
   chkInclDescCode.Font.Color := clWindowText;
   chkInclDescCode.SetBounds(5, 55, 150, 17);
   chkInclDescCode.Caption := i18Manager.GetString('chkInclDescCode');
   chkInclDescCode.DoubleBuffered := true;
   chkInclDescCode.Anchors := [akBottom, akLeft];
   chkInclDescCode.OnClick := OnClickInclDescCode;

   chkInclDescFlow := TCheckBox.Create(gbDesc);
   chkInclDescFlow.Parent := gbDesc;
   chkInclDescFlow.ParentFont := false;
   chkInclDescFlow.Font.Style := [];
   chkInclDescFlow.Font.Color := clWindowText;
   chkInclDescFlow.SetBounds(180, 55, 150, 17);
   chkInclDescFlow.Caption := i18Manager.GetString('chkInclDescFlow');
   chkInclDescFlow.DoubleBuffered := true;
   chkInclDescFlow.Anchors := [akBottom, akLeft];
   chkInclDescFlow.OnClick := OnClickInclDescFlow;

   gbBody := TGroupBox.Create(Self);
   gbBody.Parent := Self;
   gbBody.SetBounds(0, 80, 400, 50);
   gbBody.ParentFont := false;
   gbBody.ParentBackground := false;
   gbBody.Font.Color := clBlack;
   gbBody.Caption := i18Manager.GetString('Body');
   gbBody.DoubleBuffered := true;
   gbBody.Align := alTop;

   lblBodyPage := TLabel.Create(gbBody);
   lblBodyPage.Parent := gbBody;
   lblBodyPage.SetBounds(8, 25, 0, 13);
   lblBodyPage.ParentFont := false;
   lblBodyPage.Caption := i18Manager.GetString('lblBodyPage');
   lblBodyPage.Font.Style := [];
   lblBodyPage.Font.Color := clWindowText;

   cbBodyPage := TComboBox.Create(gbBody);
   cbBodyPage.Parent := gbBody;
   cbBodyPage.SetBounds(lblBodyPage.BoundsRect.Right + 6, 20, 85, 21);
   cbBodyPage.Style := csDropDownList;
   cbBodyPage.ParentFont := false;
   cbBodyPage.Font.Style := [];
   cbBodyPage.Font.Color := clWindowText;
   cbBodyPage.DropDownCount := 9;
   cbBodyPage.OnDropDown := OnDropDownBodyPage;
   cbBodyPage.OnChange := OnChangeBodyPage;
   SetPageCombo;

   if cbBodyPage.BoundsRect.Right > 170 then
      l := cbBodyPage.BoundsRect.Right + 10
   else
      l := 180;
   chkBodyVisible := TCheckBox.Create(gbBody);
   chkBodyVisible.Parent := gbBody;
   chkBodyVisible.ParentFont := false;
   chkBodyVisible.Font.Style := [];
   chkBodyVisible.Font.Color := clWindowText;
   chkBodyVisible.SetBounds(l, 20, 150, 17);
   chkBodyVisible.Caption := i18Manager.GetString('Visible');
   chkBodyVisible.DoubleBuffered := true;
   chkBodyVisible.Anchors := [akBottom, akLeft];
   chkBodyVisible.OnClick := OnClickBodyVisible;
   chkBodyVisible.Checked := true;

   gbHeader := TGroupBox.Create(Self);
   gbHeader.Parent := Self;
   gbHeader.SetBounds(0, 130, 400, 83);
   gbHeader.ParentFont := false;
   gbHeader.ParentBackground := false;
   gbHeader.Font.Color := clBlack;
   gbHeader.Caption := i18Manager.GetString('Header');
   gbHeader.DoubleBuffered := true;
   gbHeader.Align := alTop;

   CreateNameControls(gbHeader, 8, 25);

   lblType := TLabel.Create(gbHeader);
   lblTYpe.Parent := gbHeader;
   lblType.SetBounds(145, 25, 0, 13);
   lblType.ParentFont := false;
   lblType.Caption := i18Manager.GetString('lblRetType');
   lblType.Font.Style := [];
   lblType.Font.Color := clWindowText;

   cbType := TComboBox.Create(gbHeader);
   cbType.Parent := gbHeader;
   cbType.SetBounds(lblType.BoundsRect.Right + 6, 20, 85, 21);
   cbType.Style := csDropDownList;
   cbType.ParentFont := false;
   cbType.Font.Style := [];
   cbType.Font.Color := clWindowText;
   cbType.DropDownCount := 9;
   cbType.Tag := FUNCTION_TYPE_IND;
   TInfra.PopulateDataTypeCombo(cbType);
   cbType.OnChange := OnChangeType;

   CreateExtDeclareChBox(gbHeader, 143, 52);
   CreateLibControls(gbHeader, 8, 52);

   gbParams := TGroupBox.Create(Self);
   gbParams.Parent := Self;
   gbParams.SetBounds(0, 215, 400, 110);
   gbParams.ParentFont := false;
   gbParams.ParentBackground := false;
   gbParams.Font.Color := clBlack;
   gbParams.Caption := i18Manager.GetString('Params');
   gbParams.Constraints.MinHeight := gbParams.Height;
   gbParams.DoubleBuffered := true;
   gbParams.Align := alTop;

   splParams := TSplitter.Create(Self);
   splParams.Parent := Self;
   splParams.Align := gbParams.Align;
   splParams.OnMoved := OnMovedParams;

   lblParams := TLabel.Create(gbParams);
   lblParams.Parent := gbParams;
   lblParams.Font.Style := [];
   lblParams.ParentFont := false;
   lblParams.Font.Color := clWindowText;
   lblParams.SetBounds(8, 18, 0, 13);
   lblParams.Caption := i18Manager.GetString('lblParameters');

   sbxElements := TScrollBox.Create(gbParams);
   sbxElements.Parent := gbParams;
   sbxElements.Ctl3D := false;
   sbxElements.BorderStyle := bsNone;
   sbxElements.Constraints.MaxHeight := 44;
   sbxElements.Constraints.MinWidth := 379;
   sbxElements.SetBounds(6, 36, 379, 0);
   sbxElements.VertScrollBar.Tracking := true;
   sbxElements.Anchors := [akTop, akBottom, akLeft];

   btnAddElement := TButton.Create(gbParams);
   btnAddElement.Parent := gbParams;
   btnAddElement.ParentFont := false;
   btnAddElement.Font.Style := [];
   btnAddElement.DoubleBuffered := true;
   btnAddElement.Caption := i18Manager.GetString('btnAddParm');
   btnAddElement.SetBounds(8, 81, 377, 25);
   btnAddElement.Anchors := [akBottom];
   btnAddElement.OnClick := AddElement;
   btnAddElement.Anchors := [akLeft, akRight, akBottom];

   FLocalVars.Align := alClient;
end;

procedure TUserFunctionHeader.OnMovedParams(Sender: TObject);
begin
   sbxElements.Constraints.MaxHeight := gbParams.Height - 66;
end;

function TUserFunctionHeader.CreateElement: TElement;
begin
   result := TParameter.Create(Self);
end;

procedure TUserFunctionHeader.Localize(AList: TStringList);
begin
   lblType.Caption := AList.Values['lblType'];
   lblParams.Caption := AList.Values['lblParameters'];
   btnAddElement.Caption := AList.Values['btnAddParm'];
   edtLibrary.Hint := Format(AList.Values['edtLibraryHint'], [GInfra.CurrentLang.LibraryExt]);
   if cbType.Items.Count > 0 then
   begin
      cbType.Items[0] := AList.Values['NoType'];
      if cbType.ItemIndex = -1 then
         cbType.ItemIndex := 0;
   end;
   inherited Localize(AList);
end;

constructor TParameter.Create(AParentTab: TUserFunctionHeader);
begin

   inherited Create(AParentTab.sbxElements);

   FElem_Id := PARAMETER_IDENT;

   Constraints.MaxWidth := 362;
   SetBounds(0, Parent.Height, 362, 22);
   Align := alTop;
   TInfra.PopulateDataTypeCombo(cbType);

   edtDefault := TEdit.Create(Self);
   edtDefault.Parent := Self;
   edtDefault.SetBounds(174, 0, 50, 21);
   edtDefault.ParentFont := false;
   edtDefault.Font.Style := [];
   edtDefault.Font.Color := clGreen;
   edtDefault.ParentCtl3D := false;
   edtDefault.Ctl3D := true;
   edtDefault.OnChange := OnChangeType;

   btnRemove.SetBounds(307, 0, 54, 20);

   chkTable := TCheckBox.Create(Self);
   chkTable.Parent := Self;
   chkTable.SetBounds(240, 1, 20, 17);
   chkTable.DoubleBuffered := true;
   chkTable.OnClick := OnChangeType;

   chkReference := TCheckBox.Create(Self);
   chkReference.Parent := Self;
   chkReference.SetBounds(278, 1, 20, 17);
   chkReference.DoubleBuffered := true;
   chkReference.OnClick := OnChangeType;

end;

function TUserFunction.GetHandle: THandle;
begin
   result := 0;
   if FBody <> nil then
      result := FBody.GetHandle;
end;

procedure TUserFunctionHeader.OnChangeName(Sender: TObject);
var
   info, funcName: string;
begin
   info := '';
   edtName.Font.Color := NOK_COLOR;
   funcName := Trim(edtName.Text);
   if GInfra.ValidateId(funcName) <> VALID_IDENT then
      info := 'BadIdD'
   else if not GInfra.CurrentLang.AllowUserFunctionOverload then
   begin
      if GInfra.CurrentLang.NativeFunctions.IndexOf(funcName) <> -1 then
         info := 'DefNtvFunc'
      else if IsDuplicated(edtName) then
         info := 'DupIdD';
   end;
   if info.IsEmpty then
   begin
      edtName.Font.Color := OK_COLOR;
      info := 'OkIdD';
   end;
   edtName.Hint := i18Manager.GetFormattedString(info, [funcName]);
   DrawBodyLabel;
   inherited OnChangeName(Sender);
end;

procedure TUserFunctionHeader.OnChangeDesc(Sender: TObject);
begin
   GProject.SetChanged;
   if GSettings.ShowFuncLabels and chkInclDescFlow.Checked then
      DrawBodyLabel;
   if (Font.Color <> NOK_COLOR) and chkInclDescCode.Checked then
      UpdateCodeEditor;
end;

procedure TUserFunctionHeader.OnDropDownBodyPage(Sender: TObject);
begin
   SetPageCombo(cbBodyPage.Text);
end;

procedure TUserFunctionHeader.SetPageCombo(const ACaption: TCaption = '');
var
   i: integer;
   pageControl: TPageControl;
   lCaption: TCaption;
begin
   lCaption := cbBodyPage.Text;
   cbBodyPage.Items.Clear;
   cbBodyPage.Items.BeginUpdate;
   pageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to pageControl.PageCount-1 do
      cbBodyPage.Items.Add(pageControl.Pages[i].Caption);
   cbBodyPage.Items.EndUpdate;
   if ACaption = '' then
   begin
      if lCaption = '' then
         lCaption := pageControl.ActivePage.Caption;
   end
   else
      lCaption := ACaption;
   i := cbBodyPage.Items.IndexOf(lCaption);
   if i <> -1 then
      cbBodyPage.ItemIndex := i;
   cbBodyPage.DropDownCount := cbBodyPage.Items.Count;
   cbBodyPage.Width := TInfra.GetComboMaxWidth(cbBodyPage);
end;

procedure TUserFunctionHeader.OnChangeBodyPage(Sender: TObject);
var
   page: TBlockTabSheet;
begin
   page := GProject.GetPage(cbBodyPage.Text);
   if (page <> nil) and (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
   begin
      FUserFunction.Body.Page := page;
      page.PageControl.ActivePage := page;
      page.Box.ScrollInView(FUserFunction.Body);
      NavigatorForm.Invalidate;
   end;
end;

function TUserFunctionHeader.GetParameters: IEnumerable<TParameter>;
begin
   result := GetElements<TParameter>;
end;

procedure TUserFunctionHeader.OnChangeType(Sender: TObject);
begin
   if Font.Color <> NOK_COLOR then
      UpdateCodeEditor;
   GProject.SetChanged;
   DrawBodyLabel;
end;

procedure TUserFunction.GenerateTree(ANode: TTreeNode);
var
   node: TTreeNode;
   desc: string;
   lang: TLangDefinition;
   obj: TObject;
begin
   desc := '';
   if IsMain then
   begin
      obj := FBody;
      if GInfra.CurrentLang.EnabledUserFunctionHeader then
      begin
         lang := nil;
         if Assigned(GInfra.CurrentLang.GetMainProgramDesc) then
            lang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.GetMainProgramDesc) then
            lang := GInfra.DummyLang;
         if lang <> nil then
            desc := lang.GetMainProgramDesc;
      end
      else
         desc := i18Manager.GetString('Flowchart');
   end
   else
   begin
      obj := FHeader;
      lang := nil;
      if Assigned(GInfra.CurrentLang.GetUserFuncDesc) then
         lang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.GetUserFuncDesc) then
         lang := GInfra.DummyLang;
      if lang <> nil then
         desc := lang.GetUserFuncDesc(FHeader, false).Trim;
   end;
   node := ANode.Owner.AddChildObject(ANode, desc, obj);
   if FBody <> nil then
      FBody.GenerateTree(node);
   if (FHeader <> nil) and TInfra.IsNOkColor(FHeader.Font.Color) then
   begin
      ANode.MakeVisible;
      ANode.Expand(false);
   end;
end;

function TUserFunction.IsMain: boolean;
begin
   result := FHeader = nil;
end;

procedure TUserFunctionHeader.OnClickInclDescFlow(Sender: TObject);
begin
   if GSettings.ShowFuncLabels then
   begin
      DrawBodyLabel;
      NavigatorForm.Invalidate;
   end;
   GProject.SetChanged;
end;

procedure TUserFunctionHeader.OnClickBodyVisible(Sender: TObject);
begin
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
   begin
      FUserFunction.Body.SetVisible(chkBodyVisible.Checked);
      FUserFunction.Body.Page.Box.SetScrollBars;
   end;
   cbBodyPage.Enabled := chkBodyVisible.Checked;
   GProject.SetChanged;
end;

procedure TUserFunctionHeader.OnClickInclDescCode(Sender: TObject);
begin
   if Font.Color <> NOK_COLOR then
      UpdateCodeEditor;
end;

procedure TUserFunctionHeader.DrawBodyLabel;
begin
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
   begin
      FUserFunction.Body.SetWidth(0);
      FUserFunction.Body.DrawLabel;
   end;
end;

procedure TUserFunctionHeader.ExportToXMLTag(ATag: IXMLElement);
var
   tag, tag2, tag3: IXMLElement;
begin
   tag := ATag.OwnerDocument.CreateElement(FUNCTION_TAG);
   ATag.AppendChild(tag);
   tag2 := ATag.OwnerDocument.CreateElement(HEADER_TAG);
   tag.AppendChild(tag2);
   inherited ExportToXMLTag(tag2);
   tag2.SetAttribute(TYPE_ATTR, IfThen(cbType.ItemIndex = 0, 'none', cbType.Text));
   if memDesc.Text <> '' then
   begin
      tag3 := ATag.OwnerDocument.CreateElement('desc');
      TXMLProcessor.AddCDATA(tag3, ReplaceStr(memDesc.Text, sLineBreak, LB_PHOLDER));
      tag2.AppendChild(tag3);
   end;
   tag2.SetAttribute('show_body', chkBodyVisible.Checked.ToString);
   tag2.SetAttribute('desc_incl', chkInclDescCode.Checked.ToString);
   tag2.SetAttribute('desc_incl_flow', chkInclDescFlow.Checked.ToString);
   FLocalVars.ExportToXMLTag(tag2);
   tag2.SetAttribute('descrh', gbDesc.Height.ToString);
   tag2.SetAttribute('headerh', gbHeader.Height.ToString);
   tag2.SetAttribute('parmsh', gbParams.Height.ToString);
   tag2.SetAttribute('lvarsh', FLocalVars.Height.ToString);
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
      FUserFunction.Body.ExportToXMLTag(tag);
end;

procedure TUserFunctionHeader.ImportFromXMLTag(ATag: IXMLElement; APinControl: TControl = nil);
var
   idx: integer;
   tag2: IXMLElement;
begin
   inherited ImportFromXMLTag(ATag, APinControl);
   idx := cbType.Items.IndexOf(ATag.GetAttribute(TYPE_ATTR));
   if idx <> -1 then
      cbType.ItemIndex := idx
   else if cbType.Items.Count > 0 then
      cbType.ItemIndex := 0;
   cbType.OnChange(cbType);
   tag2 := TXMLProcessor.FindChildTag(ATag, 'desc');
   if tag2 <> nil then
      memDesc.Text := ReplaceStr(tag2.Text, LB_PHOLDER, sLineBreak);
   chkBodyVisible.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'show_body');
   chkInclDescCode.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'desc_incl');
   chkInclDescFlow.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'desc_incl_flow');
   FLocalVars.ImportFromXMLTag(ATag);
   idx := StrToIntDef(ATag.GetAttribute('descrh'), -1);
   if idx > -1 then
      gbDesc.Height := idx;
   idx := StrToIntDef(ATag.GetAttribute('headerh'), -1);
   if idx > -1 then
      gbHeader.Height := idx;
   idx := StrToIntDef(ATag.GetAttribute('parmsh'), -1);
   if idx > -1 then
   begin
      gbParams.Height := idx;
      sbxElements.Constraints.MaxHeight := gbParams.Height - 66;
   end;
   idx := StrToIntDef(ATag.GetAttribute('lvarsh'), -1);
   if idx > -1 then
      FLocalVars.Height := idx;
end;

procedure TParameter.ImportFromXMLTag(ATag: IXMLElement);
begin
   inherited ImportFromXMLTag(ATag);
   chkTable.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'table');
   chkReference.Checked := TXMLProcessor.GetBoolFromAttr(ATag, 'reference');
   edtDefault.Text := ATag.GetAttribute('default');
end;

function TParameter.ExportToXMLTag(ATag: IXMLElement): IXMLElement;
var
   tag: IXMLElement;
begin
   tag := inherited ExportToXMLTag(ATag);
   tag.SetAttribute('table', chkTable.Checked.ToString);
   tag.SetAttribute('reference', chkReference.Checked.ToString);
   if edtDefault.Text <> '' then
      tag.SetAttribute('default', edtDefault.Text);
end;

function TUserFunction.GetCompareValue(ACompareType: integer): integer;
begin
   result := -1;
   if ACompareType = Z_ORDER_COMPARE then
   begin
      if FBody <> nil then
         result := FBody.GetZOrder;
   end
   else if FHeader <> nil then
      result := FHeader.GetCompareValue(ACompareType);
end;

constructor TUserFunctionComparer.Create(ACompareType: integer);
begin
   inherited Create;
   FCompareType := ACompareType;
end;

function TUserFunctionComparer.Compare(const L, R: TUserFunction): integer;
begin
   result := -1;
   if FCompareType = Z_ORDER_COMPARE then
   begin
      if (L.Body = nil) and (R.Body = nil) then
         result := 0
      else if L.Body = nil then
         result := 1
      else if R.Body = nil then
         result := -1
      else
         result := L.Body.GetZOrder - R.Body.GetZOrder;
   end
   else if FCompareType = PAGE_INDEX_COMPARE then
   begin
      if (L.Header = nil) and (R.Header = nil) then
         result := 0
      else if L.Header = nil then
         result := 1
      else if R.Header = nil then
         result := -1
      else
         result := L.Header.PageIndex - R.Header.PageIndex;
   end;
end;

end.
