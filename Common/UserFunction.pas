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
   Controls, Forms, StdCtrls, ComCtrls, Graphics, ExtCtrls, Classes, OmniXML, Types,
   Main_Block, DeclareList, CommonInterfaces, TabComponent, Element, Functions_Form;

type

   TUserFunction = class;
   TUserFunctionHeader = class;

   TParameter = class(TElement)
      constructor Create(const AParentTab: TUserFunctionHeader);
   public
      chkTable: TCheckBox;
      chkReference: TCheckBox;
      function ExportToXMLTag(const ATag: IXMLElement): IXMLElement; override;
      procedure ImportFromXMLTag(const ATag: IXMLElement); override;
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
      procedure OnClickExtDecl(Sender: TObject);
      procedure SetActive(const AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure OnChangeBodyPage(Sender: TObject);
      procedure OnDropDownBodyPage(Sender: TObject);
      procedure DrawBodyLabel;
   public
      cbType,
      cbBodyPage: TComboBox;
      lblType,
      lblBodyPage,
      lblParameters: TLabel;
      gbHeader,
      gbBody,
      gbParameters,
      gbDescription: TGroupBox;
      memDescription: TMemo;
      chkInclDescCode,
      chkInclDescFlow,
      chkBodyVisible: TCheckBox;
      splDescription,
      splParameters: TSplitter;
      property UserFunction: TUserFunction read FUserFunction;
      property LocalVars: TVarDeclareList read FLocalVars;
      property ParameterCount: integer read GetElementCount;
      constructor Create(const AParentForm: TFunctionsForm);
      destructor Destroy; override;
      procedure ExportToXMLTag(const ATag: IXMLElement); override;
      procedure ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
      function GetParameterIterator: IIterator;
      procedure Localize(const AList: TStringList); override;
      procedure RefreshSizeEdits; override;
      procedure GenerateDescription(const ALines: TStrings);
      procedure SetPageCombo(const ACaption: TCaption = '');
   end;

   TUserFunction = class(TComponent, IXMLable, ITabbable, IIdentifiable, ISizeEditable, IWinControl, IMaxBoundable, ISortable)
   private
      FHeader: TUserFunctionHeader;
      FBody: TMainBlock;
      FActive: boolean;
      procedure SetActive(const AValue: boolean);
      function GetActive: boolean;
   public
      property Header: TUserFunctionHeader read FHeader;
      property Body: TMainBlock read FBody;
      property Active: boolean read GetActive write SetActive;
      constructor Create(const AFunctionHeader: TUserFunctionHeader; const AFunctionBody: TMainBlock);
      destructor Destroy; override;
      procedure ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
      procedure ExportToXMLTag(const ATag: IXMLElement);
      procedure GenerateTree(const ANode: TTreeNode);
      function GetId: integer;
      function GetLibName: string;
      procedure RefreshSizeEdits;
      procedure PaintToCanvas(const ACanvas: TCanvas);
      function GetMaxBounds: TPoint;
      function GetName: string;
      function GetSortValue(const ASortType: integer): integer;
      function GetHandle: THandle;
      procedure BringAllToFront;
      procedure SetZOrder(const AValue: integer);
      function GetZOrder: integer;
      function IsMain: boolean;
   end;

implementation

uses
   SysUtils, ApplicationCommon, Main_Form, XMLProcessor, Windows, Grids, StrUtils,
   LangDefinition, Navigator_Form, BlockTabSheet;

constructor TUserFunction.Create(const AFunctionHeader: TUserFunctionHeader; const AFunctionBody: TMainBlock);
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
      FBody.Page.Form.SetScrollBars;
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

function TUserFunction.GetMaxBounds: TPoint;
begin
   result := Point(0, 0);
   if FBody <> nil then
      result := FBody.GetMaxBounds;
end;

function TUserFunction.GetSortValue(const ASortType: integer): integer;
begin
   result := -1;
   if ASortType = Z_ORDER_SORT then
   begin
      if FBody <> nil then
         result := FBody.GetZOrder;
   end
   else if FHeader <> nil then
      result := FHeader.GetSortValue(ASortType);
end;

procedure TUserFunction.BringAllToFront;
begin
   if FBody <> nil then
      FBody.BringAllToFront;
end;

procedure TUserFunction.SetZOrder(const AValue: integer);
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

procedure TUserFunctionHeader.SetActive(const AValue: boolean);
begin
   if AValue <> FActive then
   begin
      inherited SetActive(AValue);
      if (FUserFunction <> nil) and (FUserFunction.Active <> AValue) then
         FUserFunction.Active := AValue;
   end;
end;

procedure TUserFunctionHeader.GenerateDescription(const ALines: TStrings);
var
   i: integer;
begin
   if chkInclDescCode.Checked and (memDescription.Text <> '') then
   begin
      for i := 0 to memDescription.Lines.Count-1 do
         ALines.Add(memDescription.Lines[i]);
      if AnsiEndsText(CRLF, memDescription.Text) then
         ALines.Add('');
   end;
end;

procedure TUserFunction.SetActive(const AValue: boolean);
var
   lVisible: boolean;
begin
   if FActive <> AValue then
   begin
      FActive := AValue;
      if FBody <> nil then
      begin
         lVisible := AValue;
         if FHeader <> nil then
            lVisible := lVisible and FHeader.chkBodyVisible.Checked;
         FBody.SetVisible(lVisible);
         if FBody.Visible then
            FBody.RefreshStatements;
         FBody.Page.Form.SetScrollBars;
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

procedure TUserFunction.ExportToXMLTag(const ATag: IXMLElement);
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

procedure TUserFunction.ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
begin
{}
end;

constructor TUserFunctionHeader.Create(const AParentForm: TFunctionsForm);
var
   lLeft: integer;
begin

   inherited Create(AParentForm);

   FElementMode := PARAMETER_IDENT;

   FLocalVars := TVarDeclareList.Create(Self, 0, 350, 389, 3, 4, 380);
   FLocalVars.Caption := i18Manager.GetString('LocalDeclare');
   FLocalVars.sgList.Options := FLocalVars.sgList.Options - [goColSizing];
   FLocalVars.gbBox.DoubleBuffered := true;

   gbDescription := TGroupBox.Create(Self);
   gbDescription.Parent := Self;
   gbDescription.SetBounds(0, 0, 400, 77);
   gbDescription.ParentFont := false;
   gbDescription.Font.Color := clBlack;
   gbDescription.Align := alTop;
   gbDescription.Caption := i18Manager.GetString('gbDesc');
   gbDescription.DoubleBuffered := true;
   gbDescription.Constraints.MinHeight := gbDescription.Height;

   splDescription := TSplitter.Create(Self);
   splDescription.Parent := Self;
   splDescription.Align := gbDescription.Align;

   memDescription := TMemo.Create(gbDescription);
   memDescription.Parent := gbDescription;
   memDescription.SetBounds(5, 17, 378, 35);
   memDescription.ParentFont := false;
   memDescription.Font.Style := [];
   memDescription.Font.Color := clGreen;
   memDescription.WordWrap := false;
   memDescription.DoubleBuffered := true;
   memDescription.ScrollBars := ssVertical;
   memDescription.Anchors := [akTop, akBottom, akLeft, akRight];
   memDescription.OnChange := OnChangeDesc;

   chkInclDescCode := TCheckBox.Create(gbDescription);
   chkInclDescCode.Parent := gbDescription;
   chkInclDescCode.ParentFont := false;
   chkInclDescCode.Font.Style := [];
   chkInclDescCode.Font.Color := clWindowText;
   chkInclDescCode.SetBounds(5, 55, 150, 17);
   chkInclDescCode.Caption := i18Manager.GetString('chkInclDescCode');
   chkInclDescCode.DoubleBuffered := true;
   chkInclDescCode.Anchors := [akBottom, akLeft];
   chkInclDescCode.OnClick := OnClickInclDescCode;

   chkInclDescFlow := TCheckBox.Create(gbDescription);
   chkInclDescFlow.Parent := gbDescription;
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
      lLeft := cbBodyPage.BoundsRect.Right + 10
   else
      lLeft := 180;
   chkBodyVisible := TCheckBox.Create(gbBody);
   chkBodyVisible.Parent := gbBody;
   chkBodyVisible.ParentFont := false;
   chkBodyVisible.Font.Style := [];
   chkBodyVisible.Font.Color := clWindowText;
   chkBodyVisible.SetBounds(lLeft, 20, 150, 17);
   chkBodyVisible.Caption := i18Manager.GetString('Visible');
   chkBodyVisible.DoubleBuffered := true;
   chkBodyVisible.Anchors := [akBottom, akLeft];
   chkBodyVisible.OnClick := OnClickBodyVisible;
   chkBodyVisible.Checked := true;

   gbHeader := TGroupBox.Create(Self);
   gbHeader.Parent := Self;
   gbHeader.SetBounds(0, 130, 400, 83);
   gbHeader.ParentFont := false;
   gbHeader.Font.Color := clBlack;
   gbHeader.Caption := i18Manager.GetString('Header');
   gbHeader.DoubleBuffered := true;
   gbHeader.Align := alTop;

   lblName := TLabel.Create(gbHeader);
   lblName.Parent := gbHeader;
   lblName.SetBounds(8, 25, 0, 13);
   lblName.ParentFont := false;
   lblName.Caption := i18Manager.GetString('lblName');
   lblName.Font.Style := [];
   lblName.Font.Color := clWindowText;

   edtName := TEdit.Create(gbHeader);
   edtName.Parent := gbHeader;
   edtName.SetBounds(lblName.Width+14, 20, 119-lblName.Width, 21);
   edtName.ParentFont := false;
   edtName.Font.Style := [];
   edtName.ShowHint := true;
   edtName.Hint := i18Manager.GetString('BadIdD');
   edtName.DoubleBuffered := true;
   edtName.OnChange := OnChangeName;

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

   chkExtDeclare := TCheckBox.Create(gbHeader);
   chkExtDeclare.Parent := gbHeader;
   chkExtDeclare.Caption := i18Manager.GetString('chkExtDeclare');
   chkExtDeclare.SetBounds(143, 52, PageControl.Canvas.TextWidth(chkExtDeclare.Caption) + 25, 17);
   chkExtDeclare.ParentFont := false;
   chkExtDeclare.Font.Style := [];
   chkExtDeclare.Font.Color := clWindowText;
   chkExtDeclare.Alignment := taLeftJustify;
   chkExtDeclare.DoubleBuffered := true;
   chkExtDeclare.ShowHint := true;
   chkExtDeclare.Hint := i18Manager.GetString('chkExtDeclare.Hint');
   chkExtDeclare.OnClick := OnClickExtDecl;

   lblLibrary := TLabel.Create(gbHeader);
   lblLibrary.Parent := gbHeader;
   lblLibrary.SetBounds(8, 52, 0, 13);
   lblLibrary.Caption := i18Manager.GetString('lblLibrary');
   lblLibrary.ParentFont := false;
   lblLibrary.Font.Style := [];
   lblLibrary.Font.Color := clWindowText;

   edtLibrary := TEdit.Create(gbHeader);
   edtLibrary.Parent := gbHeader;
   edtLibrary.SetBounds(lblLibrary.Width+14, 48, 119-lblLibrary.Width, 21);
   edtLibrary.ParentFont := false;
   edtLibrary.Font.Style := [];
   edtLibrary.Font.Color := clGreen;
   edtLibrary.ShowHint := true;
   edtLibrary.DoubleBuffered := true;
   edtLibrary.OnChange := OnChangeLib;
   edtLibrary.Hint := AnsiReplaceStr(i18Manager.GetFormattedString('edtLibraryHint', [GInfra.CurrentLang.LibraryExt]), '##', CRLF);

   gbParameters := TGroupBox.Create(Self);
   gbParameters.Parent := Self;
   gbParameters.SetBounds(0, 215, 400, 110);
   gbParameters.ParentFont := false;
   gbParameters.Font.Color := clBlack;
   gbParameters.Caption := i18Manager.GetString('Params');
   gbParameters.Constraints.MinHeight := gbParameters.Height;
   gbParameters.DoubleBuffered := true;
   gbParameters.Align := alTop;

   splParameters := TSplitter.Create(Self);
   splParameters.Parent := Self;
   splParameters.Align := gbParameters.Align;
   splParameters.OnMoved := OnMovedParams;

   lblParameters := TLabel.Create(gbParameters);
   lblParameters.Parent := gbParameters;
   lblParameters.Font.Style := [];
   lblParameters.ParentFont := false;
   lblParameters.Font.Color := clWindowText;
   lblParameters.SetBounds(8, 18, 0, 13);
   lblParameters.Caption := i18Manager.GetString('lblParameters');

   sbxElements := TScrollBox.Create(gbParameters);
   sbxElements.Parent := gbParameters;
   sbxElements.Ctl3D := false;
   sbxElements.BorderStyle := bsNone;
   sbxElements.Constraints.MaxHeight := 44;
   sbxElements.Constraints.MinWidth := 379;
   sbxElements.SetBounds(6, 36, 379, 0);
   sbxElements.VertScrollBar.Tracking := true;
   sbxElements.Anchors := [akTop, akBottom, akLeft];

   btnAddElement := TButton.Create(gbParameters);
   btnAddElement.Parent := gbParameters;
   btnAddElement.ParentFont := false;
   btnAddElement.Font.Style := [];
   btnAddElement.DoubleBuffered := true;
   btnAddElement.Caption := i18Manager.GetString('btnAddParm');
   btnAddElement.SetBounds(4, 81, 381, 25);
   btnAddElement.Anchors := [akBottom];
   btnAddElement.OnClick := AddElement;
   btnAddElement.Anchors := [akLeft, akRight, akBottom];

   FLocalVars.Align := alClient;
end;

procedure TUserFunctionHeader.OnMovedParams(Sender: TObject);
begin
   sbxElements.Constraints.MaxHeight := gbParameters.Height - 66;
end;

function TUserFunctionHeader.CreateElement: TElement;
begin
   result := TParameter.Create(Self);
end;

procedure TUserFunctionHeader.Localize(const AList: TStringList);
begin
   lblType.Caption := AList.Values['lblType'];
   lblParameters.Caption := AList.Values['lblParameters'];
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

constructor TParameter.Create(const AParentTab: TUserFunctionHeader);
begin

   inherited Create(AParentTab.sbxElements);

   FElem_Id := PARAMETER_IDENT;

   Constraints.MaxWidth := 362;
   SetBounds(0, Parent.Height, 362, 22);
   Align := alTop;
   cbType.SetBounds(97, 0, 75, 21);
   cbType.Constraints.MaxWidth := 95;
   TInfra.PopulateDataTypeCombo(cbType);

   btnRemove.SetBounds(307, 0, 54, 20);

   chkTable := TCheckBox.Create(Self);
   chkTable.Parent := Self;
   chkTable.SetBounds(203, 2, 20, 17);
   chkTable.DoubleBuffered := true;
   chkTable.OnClick := OnChangeType;

   chkReference := TCheckBox.Create(Self);
   chkReference.Parent := Self;
   chkReference.SetBounds(268, 2, 20, 17);
   chkReference.DoubleBuffered := true;
   chkReference.OnClick := OnChangeType;

end;

procedure TUserFunction.PaintToCanvas(const ACanvas: TCanvas);
begin
   if FBody <> nil then
      FBody.PaintToCanvas(ACanvas);
end;

function TUserFunction.GetHandle: THandle;
begin
   result := 0;
   if FBody <> nil then
      result := FBody.GetHandle;
end;

procedure TUserFunctionHeader.OnChangeName(Sender: TObject);
var
   lInfo, lFuncName: string;
begin
   lInfo := '';
   edtName.Font.Color := NOK_COLOR;
   lFuncName := Trim(edtName.Text);
   if GInfra.ValidateId(lFuncName) <> VALID_IDENT then
      lInfo := 'BadIdD'
   else if not GInfra.CurrentLang.AllowUserFunctionOverload then
   begin
      if GInfra.CurrentLang.NativeFunctions.IndexOf(lFuncName) <> -1 then
         lInfo := 'DefNtvFunc'
      else if IsDuplicated(edtName) then
         lInfo := 'DupIdD';
   end;
   if lInfo = '' then
   begin
      edtName.Font.Color := OK_COLOR;
      lInfo := 'OkIdD';
   end;
   edtName.Hint := i18Manager.GetFormattedString(lInfo, [lFuncName]);
   DrawBodyLabel;
   inherited OnChangeName(Sender);
end;

procedure TUserFunctionHeader.OnChangeDesc(Sender: TObject);
begin
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
   lPageControl: TPageControl;
   lCaption: TCaption;
begin
   lCaption := cbBodyPage.Text;
   cbBodyPage.Items.Clear;
   cbBodyPage.Items.BeginUpdate;
   lPageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to lPageControl.PageCount-1 do
      cbBodyPage.Items.Add(lPageControl.Pages[i].Caption);
   cbBodyPage.Items.EndUpdate;
   if ACaption = '' then
   begin
      if lCaption = '' then
         lCaption := lPageControl.ActivePage.Caption;
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
   lPage: TBlockTabSheet;
begin
   lPage := GProject.GetPage(cbBodyPage.Text);
   if (lPage <> nil) and (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
   begin
      FUserFunction.Body.Page := lPage;
      lPage.PageControl.ActivePage := lPage;
      lPage.Form.ScrollInView(FUserFunction.Body);
      NavigatorForm.Invalidate;
   end;
end;

procedure TUserFunctionHeader.OnChangeType(Sender: TObject);
begin
   if Font.Color <> NOK_COLOR then
      UpdateCodeEditor;
   GChange := 1;
   DrawBodyLabel;
end;

procedure TUserFunction.GenerateTree(const ANode: TTreeNode);
var
   lNode: TTreeNode;
   lDesc: string;
   lLang: TLangDefinition;
   lObject: TObject;
begin
   lDesc := '';
   if IsMain then
   begin
      lObject := FBody;
      if GInfra.CurrentLang.EnabledUserFunctionHeader then
      begin
         lLang := nil;
         if Assigned(GInfra.CurrentLang.GetMainProgramDesc) then
            lLang := GInfra.CurrentLang
         else if Assigned(GInfra.DummyLang.GetMainProgramDesc) then
            lLang := GInfra.DummyLang;
         if lLang <> nil then
            lDesc := lLang.GetMainProgramDesc;
      end
      else
         lDesc := i18Manager.GetString('Flowchart');
   end
   else
   begin
      lObject := FHeader;
      lLang := nil;
      if Assigned(GInfra.CurrentLang.GetUserFuncDesc) then
         lLang := GInfra.CurrentLang
      else if Assigned(GInfra.DummyLang.GetUserFuncDesc) then
         lLang := GInfra.DummyLang;
      if lLang <> nil then
         lDesc := lLang.GetUserFuncDesc(FHeader);
   end;
   lNode := ANode.Owner.AddChildObject(ANode, lDesc, lObject);
   if FBody <> nil then
      FBody.GenerateTree(lNode);
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
   GChange := 1;
end;

procedure TUserFunctionHeader.OnClickExtDecl(Sender: TObject);
begin
   if Font.Color <> NOK_COLOR then
      TInfra.UpdateCodeEditor(Self);
end;

procedure TUserFunctionHeader.OnClickBodyVisible(Sender: TObject);
begin
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
   begin
      FUserFunction.Body.SetVisible(chkBodyVisible.Checked);
      FUserFunction.Body.Page.Form.SetScrollBars;
   end;
   cbBodyPage.Enabled := chkBodyVisible.Checked;
   GChange := 1;
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

procedure TUserFunctionHeader.ExportToXMLTag(const ATag: IXMLElement);
var
   tag, tag2, tag3: IXMLElement;
   lType: string;
begin
   tag := ATag.OwnerDocument.CreateElement(FUNCTION_TAG);
   ATag.AppendChild(tag);
   tag2 := ATag.OwnerDocument.CreateElement(HEADER_TAG);
   tag.AppendChild(tag2);
   inherited ExportToXMLTag(tag2);
   if cbType.ItemIndex = 0 then
      lType := 'none'
   else
      lType := cbType.Text;
   tag2.SetAttribute(TYPE_ATTR, lType);
   if memDescription.Text <> '' then
   begin
      tag3 := ATag.OwnerDocument.CreateElement('desc');
      TXMLProcessor.AddCDATA(tag3, AnsiReplaceStr(memDescription.Text, CRLF, CRLF_PLACEHOLDER));
      tag2.AppendChild(tag3);
   end;
   tag2.SetAttribute('show_body', BoolToStr(chkBodyVisible.Checked, true));
   tag2.SetAttribute('desc_incl', BoolToStr(chkInclDescCode.Checked, true));
   tag2.SetAttribute('desc_incl_flow', BoolToStr(chkInclDescFlow.Checked, true));
   FLocalVars.ExportToXMLTag(tag2);
   tag2.SetAttribute('descrh', IntToStr(gbDescription.Height));
   tag2.SetAttribute('headerh', IntToStr(gbHeader.Height));
   tag2.SetAttribute('parmsh', IntToStr(gbParameters.Height));
   tag2.SetAttribute('lvarsh', IntToStr(FLocalVars.Height));
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
      FUserFunction.Body.ExportToXMLTag(tag);
end;

procedure TUserFunctionHeader.ImportFromXMLTag(const ATag: IXMLElement; const APinControl: TControl = nil);
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
      memDescription.Text := AnsiReplaceStr(tag2.Text, CRLF_PLACEHOLDER, CRLF);
   if ATag.GetAttribute('show_body') = 'False' then
      chkBodyVisible.Checked := false;
   chkInclDescCode.Checked := ATag.GetAttribute('desc_incl') = 'True';
   chkInclDescFlow.Checked := ATag.GetAttribute('desc_incl_flow') = 'True';
   FLocalVars.ImportFromXMLTag(ATag);
   idx := StrToIntDef(ATag.GetAttribute('descrh'), -1);
   if idx > -1 then
      gbDescription.Height := idx;
   idx := StrToIntDef(ATag.GetAttribute('headerh'), -1);
   if idx > -1 then
      gbHeader.Height := idx;
   idx := StrToIntDef(ATag.GetAttribute('parmsh'), -1);
   if idx > -1 then
   begin
      gbParameters.Height := idx;
      sbxElements.Constraints.MaxHeight := gbParameters.Height - 66;
   end;
   idx := StrToIntDef(ATag.GetAttribute('lvarsh'), -1);
   if idx > -1 then
      FLocalVars.Height := idx;
end;

procedure TParameter.ImportFromXMLTag(const ATag: IXMLElement);
begin
   inherited ImportFromXMLTag(ATag);
   chkTable.Checked := CompareText(ATag.GetAttribute('table'), 'true') = 0;
   chkReference.Checked := CompareText(ATag.GetAttribute('reference'), 'true') = 0;
end;

function TParameter.ExportToXMLTag(const ATag: IXMLElement): IXMLElement;
var
   tag: IXMLElement;
begin
   tag := inherited ExportToXMLTag(ATag);
   tag.SetAttribute('table', BoolToStr(chkTable.Checked, true));
   tag.SetAttribute('reference', BoolToStr(chkReference.Checked, true));
end;

function TUserFunctionHeader.GetParameterIterator: IIterator;
begin
   result := GetElementIterator;
end;

end.
