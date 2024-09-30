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
   Generics.Defaults, OmniXML, Main_Block, DeclareList, Interfaces, TabComponent,
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
   protected
      procedure OnChangeType(Sender: TObject); override;
      procedure OnChangeName(Sender: TObject); override;
   public
      chkTable: TCheckBox;
      chkReference: TCheckBox;
      edtDefault: TEdit;
      function ExportToXML(ANode: IXMLNode): IXMLNode; override;
      procedure ImportFromXML(ANode: IXMLNode); override;
   end;

   TUserFunctionHeader = class(TTabComponent)
   private
      FUserFunction: TUserFunction;
      FLocalVars: TVarDeclareList;
   protected
      procedure OnChangeName(Sender: TObject); override;
      procedure OnChangeDesc(Sender: TObject);
      procedure OnClickInclDescFlow(Sender: TObject);
      procedure OnClickGenDesc(Sender: TObject);
      procedure OnClickInclDescCode(Sender: TObject);
      procedure OnClickBodyVisible(Sender: TObject);
      procedure OnChangeType(Sender: TObject);
      procedure OnMovedParams(Sender: TObject);
      procedure SetActive(AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure OnChangeBodyPage(Sender: TObject);
      procedure OnDropDownBodyPage(Sender: TObject);
      procedure DrawBodyLabel;
      procedure OnClickCh(Sender: TObject); override;
      procedure AddElement(Sender: TObject); override;
   public
      cbType,
      cbBodyPage: TComboBox;
      lblType,
      lblBodyPage,
      lblParam,
      lblParamType,
      lblParamDefault,
      lblParamArray,
      lblParamRef: TLabel;
      gbHeader,
      gbBody,
      gbParams,
      gbDesc: TGroupBox;
      memDesc: TMemo;
      chkInclDescCode,
      chkInclDescFlow,
      chkBodyVisible,
      chkArrayType,
      chkStatic,
      chkConstructor: TCheckBox;
      splDesc,
      splParams: TSplitter;
      btnGenDesc: TButton;
      property UserFunction: TUserFunction read FUserFunction;
      property LocalVars: TVarDeclareList read FLocalVars;
      property ParameterCount: integer read GetElementCount;
      constructor Create(AParentForm: TFunctionsForm);
      destructor Destroy; override;
      procedure ExportCode(ALines: TStringList); override;
      procedure ExportToXML(ANode: IXMLNode); override;
      procedure ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
      procedure RefreshSizeEdits; override;
      procedure GenerateDescription(ALines: TStrings);
      procedure SetPageCombo(const ACaption: TCaption = '');
      function GetParameters: IEnumerable<TParameter>;
      procedure RefreshElements; override;
      function GetExternModifier: string; override;
      function GetTreeNodeText(ANodeOffset: integer = 0): string; override;
   end;

   TUserFunction = class(TComponent, IXMLable, IWithTab, IWithName, IWithId, IWithSizeEdits, IWinControl, IGenericComparable)
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
      procedure ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
      procedure ExportToXML(ANode: IXMLNode);
      procedure GenerateTree(ANode: TTreeNode);
      function GetId: integer;
      function GetLibrary: string;
      procedure RefreshSizeEdits;
      function GetName: string;
      function GetTab: TTabSheet;
      function GetHandle: THandle;
      procedure BringAllToFront;
      procedure SetZOrder(AValue: integer);
      function GetZOrder: integer;
      function IsMain: boolean;
      function GetCompareValue(ACompareType: integer): integer;
      function GetTreeNodeText(ANodeOffset: integer = 0): string;
   end;

implementation

uses
   Vcl.Forms, Vcl.Graphics, System.SysUtils, System.StrUtils, Infrastructure, Constants,
   Navigator_Form, Types, OmniXMLUtils;

var
   ByTopParameterComparer: IComparer<TParameter>;

constructor TUserFunction.Create(AFunctionHeader: TUserFunctionHeader; AFunctionBody: TMainBlock);
begin
   inherited Create(Application);
   GProject.AddComponent(Self);
   FBody := AFunctionBody;
   FHeader := AFunctionHeader;
   FActive := True;
   if FHeader <> nil then
   begin
      FHeader.FUserFunction := Self;
      FHeader.FParentObject := Self;
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
      FLocalVars.edtSize.Change;
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

function TUserFunction.GetLibrary: string;
begin
   result := '';
   if FHeader <> nil then
      result := FHeader.GetLibrary;
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
begin
   if chkInclDescCode.Checked and (memDesc.Text <> '') then
   begin
      for var i := 0 to memDesc.Lines.Count-1 do
         ALines.Add(memDesc.Lines[i]);
      if EndsText(sLineBreak, memDesc.Text) then
         ALines.Add('');
   end;
end;

procedure TUserFunction.SetActive(AValue: boolean);
begin
   if FActive <> AValue then
   begin
      FActive := AValue;
      if FBody <> nil then
      begin
         var vis := AValue;
         if FHeader <> nil then
            vis := vis and FHeader.chkBodyVisible.Checked;
         FBody.SetVisible(vis);
         if FBody.Visible then
            FBody.PerformRefreshStatements;
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

function TUserFunction.GetTab: TTabSheet;
begin
   result := FHeader;
end;

function TUserFunction.GetActive: boolean;
begin
   result := FActive;
end;

procedure TUserFunction.ExportToXML(ANode: IXMLNode);
begin
   if FHeader <> nil then
      FHeader.ExportToXML(ANode)
   else if FBody <> nil then
      FBody.ExportToXML(AppendNode(ANode, FUNCTION_TAG));
end;

procedure TUserFunction.ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
begin
{}
end;

procedure TUserFunctionHeader.OnClickCh(Sender: TObject);
begin
   inherited;
   DrawBodyLabel;
end;

constructor TUserFunctionHeader.Create(AParentForm: TFunctionsForm);
var
   x: integer;
   ctrl: TControl;
begin

   FElementTypeId := 'arg';
   FCodeIncludeExtern := GInfra.CurrentLang.CodeIncludeExternFunction;

   inherited Create(AParentForm);

   FLocalVars := TVarDeclareList.Create(Self, 0, 350, 389, 3, 4, 380);
   FLocalVars.Caption := i18Manager.GetString('LocalDeclare');
   FLocalVars.gbBox.DoubleBuffered := True;

   gbDesc := TGroupBox.Create(Self);
   gbDesc.Parent := Self;
   gbDesc.SetBounds(0, 0, 400, 77);
   gbDesc.ParentFont := False;
   gbDesc.ParentBackground := False;
   gbDesc.Font.Color := clBlack;
   gbDesc.Align := alTop;
   gbDesc.Caption := i18Manager.GetString('gbDesc');
   gbDesc.DoubleBuffered := True;
   gbDesc.Constraints.MinHeight := gbDesc.Height;

   splDesc := TSplitter.Create(Self);
   splDesc.Parent := Self;
   splDesc.Align := gbDesc.Align;
   splDesc.Height := 3;

   var s22 := TInfra.Scaled(Self, 22);
   var s16 := TInfra.Scaled(Self, 16);

   chkInclDescCode := TCheckBox.Create(gbDesc);
   chkInclDescCode.Parent := gbDesc;
   chkInclDescCode.ParentFont := False;
   chkInclDescCode.Font.Style := [];
   chkInclDescCode.Font.Color := clWindowText;
   chkInclDescCode.Caption := i18Manager.GetString('chkInclDescCode');
   chkInclDescCode.SetBounds(5, gbDesc.Height-s22, TInfra.GetAutoWidth(chkInclDescCode), s16);
   chkInclDescCode.DoubleBuffered := True;
   chkInclDescCode.Anchors := [akBottom, akLeft];
   chkInclDescCode.OnClick := OnClickInclDescCode;

   chkInclDescFlow := TCheckBox.Create(gbDesc);
   chkInclDescFlow.Parent := gbDesc;
   chkInclDescFlow.ParentFont := False;
   chkInclDescFlow.Font.Style := [];
   chkInclDescFlow.Font.Color := clWindowText;
   chkInclDescFlow.Caption := i18Manager.GetString('chkInclDescFlow');
   chkInclDescFlow.SetBounds(chkInclDescCode.BoundsRect.Right+20, gbDesc.Height-s22, TInfra.GetAutoWidth(chkInclDescFlow), s16);
   chkInclDescFlow.DoubleBuffered := True;
   chkInclDescFlow.Anchors := [akBottom, akLeft];
   chkInclDescFlow.OnClick := OnClickInclDescFlow;

   memDesc := TMemo.Create(gbDesc);
   memDesc.Parent := gbDesc;
   memDesc.SetBounds(5, 17, gbDesc.Width-12, chkInclDescCode.Top-22);
   memDesc.ParentFont := False;
   memDesc.Font.Style := [];
   memDesc.Font.Color := clGreen;
   memDesc.WordWrap := False;
   memDesc.DoubleBuffered := True;
   memDesc.ScrollBars := ssVertical;
   memDesc.Anchors := [akTop, akBottom, akLeft, akRight];
   memDesc.OnChange := OnChangeDesc;

   btnGenDesc := TButton.Create(gbDesc);
   btnGenDesc.Parent := gbDesc;
   btnGenDesc.ParentFont := False;
   btnGenDesc.Font.Style := [];
   btnGenDesc.DoubleBuffered := True;
   btnGenDesc.Caption := i18Manager.GetString('btnGenDesc');
   btnGenDesc.SetBounds(gbDesc.Width-TInfra.Scaled(Self, 85), chkInclDescCode.Top-3, TInfra.Scaled(Self, 80), TInfra.Scaled(Self, 20));
   btnGenDesc.Anchors := [akBottom];
   btnGenDesc.OnClick := OnClickGenDesc;
   btnGenDesc.Anchors := [akLeft, akRight, akBottom];

   gbBody := TGroupBox.Create(Self);
   gbBody.Parent := Self;
   gbBody.SetBounds(0, 80, 400, 50);
   gbBody.ParentFont := False;
   gbBody.ParentBackground := False;
   gbBody.Font.Color := clBlack;
   gbBody.Caption := i18Manager.GetString('Body');
   gbBody.DoubleBuffered := True;
   gbBody.Align := alTop;

   lblBodyPage := TLabel.Create(gbBody);
   lblBodyPage.Parent := gbBody;
   lblBodyPage.SetBounds(8, 25, 0, 13);
   lblBodyPage.ParentFont := False;
   lblBodyPage.Caption := i18Manager.GetString('lblBodyPage');
   lblBodyPage.Font.Style := [];
   lblBodyPage.Font.Color := clWindowText;

   cbBodyPage := TComboBox.Create(gbBody);
   cbBodyPage.Parent := gbBody;
   cbBodyPage.Constraints.MinWidth := TInfra.Scaled(Self, 75);
   cbBodyPage.SetBounds(lblBodyPage.BoundsRect.Right+6, 20, cbBodyPage.Constraints.MinWidth, 21);
   cbBodyPage.Style := csDropDownList;
   cbBodyPage.ParentFont := False;
   cbBodyPage.Font.Style := [];
   cbBodyPage.Font.Color := clWindowText;
   cbBodyPage.DropDownCount := 9;
   cbBodyPage.OnDropDown := OnDropDownBodyPage;
   cbBodyPage.OnChange := OnChangeBodyPage;
   SetPageCombo;

   chkBodyVisible := TCheckBox.Create(gbBody);
   chkBodyVisible.Parent := gbBody;
   chkBodyVisible.ParentFont := False;
   chkBodyVisible.Font.Style := [];
   chkBodyVisible.Font.Color := clWindowText;
   chkBodyVisible.Caption := i18Manager.GetString('Visible');
   chkBodyVisible.SetBounds(cbBodyPage.BoundsRect.Right+20, 22, TInfra.GetAutoWidth(chkBodyVisible), TInfra.Scaled(Self, 17));
   chkBodyVisible.DoubleBuffered := True;
   chkBodyVisible.Anchors := [akBottom, akLeft];
   chkBodyVisible.OnClick := OnClickBodyVisible;
   chkBodyVisible.Checked := True;

   gbHeader := TGroupBox.Create(Self);
   gbHeader.Parent := Self;
   gbHeader.SetBounds(0, 130, 400, 83);
   gbHeader.ParentFont := False;
   gbHeader.ParentBackground := False;
   gbHeader.Font.Color := clBlack;
   gbHeader.Caption := i18Manager.GetString('Header');
   gbHeader.DoubleBuffered := True;
   gbHeader.Align := alTop;

   CreateNameControls(gbHeader, 8, 25);

   lblType := TLabel.Create(gbHeader);
   lblTYpe.Parent := gbHeader;
   lblType.SetBounds(165, 25, 0, 13);
   lblType.ParentFont := False;
   lblType.Caption := i18Manager.GetString('lblRetType');
   lblType.Font.Style := [];
   lblType.Font.Color := clWindowText;

   cbType := TComboBox.Create(gbHeader);
   cbType.Parent := gbHeader;
   cbType.SetBounds(lblType.BoundsRect.Right + 6, 20, 85, 21);
   cbType.Style := csDropDownList;
   cbType.ParentFont := False;
   cbType.Font.Style := [];
   cbType.Font.Color := clWindowText;
   cbType.DropDownCount := 9;
   cbType.Tag := FUNCTION_TYPE_IND;
   cbType.Constraints.MaxWidth := 97;
   TInfra.PopulateDataTypeCombo(cbType);
   cbType.OnChange := OnChangeType;

   chkArrayType := TCheckBox.Create(gbHeader);
   chkArrayType.Parent := gbHeader;
   chkArrayType.ParentFont := False;
   chkArrayType.Font.Style := [];
   chkArrayType.Font.Color := clWindowText;
   x := cbType.BoundsRect.Right + 10;
   chkArrayType.Caption := i18Manager.GetString('chkArrayType');
   chkArrayType.SetBounds(x, 23, gbHeader.Width-x-3, 17);
   chkArrayType.DoubleBuffered := True;
   chkArrayType.Anchors := [akBottom, akLeft];
   chkArrayType.Enabled := False;
   chkArrayType.OnClick := OnChangeType;

   CreateExtDeclareChBox(gbHeader, 165, 52, taLeftJustify);
   chkExternal.AllowGrayed := GInfra.CurrentLang.AllowTransExternFunction;

   chkStatic := TCheckBox.Create(gbHeader);
   chkStatic.Parent := gbHeader;
   chkStatic.ParentFont := False;
   chkStatic.Font.Style := [];
   chkStatic.Font.Color := clWindowText;
   chkStatic.Alignment := taLeftJustify;
   chkStatic.Visible := not GInfra.CurrentLang.StaticLabel.IsEmpty;
   if chkStatic.Visible then
   begin
      chkStatic.Caption := GInfra.CurrentLang.StaticLabel;
      chkStatic.SetBounds(chkExternal.BoundsRect.Right + 15, 52, TInfra.GetAutoWidth(chkStatic), 17);
   end;
   chkStatic.DoubleBuffered := True;
   chkStatic.Anchors := [akBottom, akLeft];
   chkStatic.OnClick := OnClickCh;

   chkConstructor := TCheckBox.Create(gbHeader);
   chkConstructor.Parent := gbHeader;
   chkConstructor.ParentFont := False;
   chkConstructor.Font.Style := [];
   chkConstructor.Font.Color := clWindowText;
   chkConstructor.Alignment := taLeftJustify;
   chkConstructor.Caption := i18Manager.GetString('constructor');
   if chkStatic.Visible then
      ctrl := chkStatic
   else
      ctrl := chkExternal;
   chkConstructor.SetBounds(ctrl.BoundsRect.Right + 15, 52, TInfra.GetAutoWidth(chkConstructor), 17);
   chkConstructor.DoubleBuffered := True;
   chkConstructor.Anchors := [akBottom, akLeft];
   chkConstructor.OnClick := OnClickCh;

   CreateLibControls(gbHeader, 8, 52);
   x := lblLibrary.BoundsRect.Right + 5;
   edtLibrary.SetBounds(x, edtLibrary.Top, edtName.BoundsRect.Right-x, edtLibrary.Height);

   gbParams := TGroupBox.Create(Self);
   gbParams.Parent := Self;
   gbParams.SetBounds(0, 215, 400, 110);
   gbParams.ParentFont := False;
   gbParams.ParentBackground := False;
   gbParams.Font.Color := clBlack;
   gbParams.Caption := i18Manager.GetString('Params');
   gbParams.Constraints.MinHeight := gbParams.Height;
   gbParams.DoubleBuffered := True;
   gbParams.Align := alTop;

   splParams := TSplitter.Create(Self);
   splParams.Parent := Self;
   splParams.Align := gbParams.Align;
   splParams.Height := 3;
   splParams.OnMoved := OnMovedParams;

   lblParam := TLabel.Create(gbParams);
   lblParam.Parent := gbParams;
   lblParam.Font.Style := [];
   lblParam.ParentFont := False;
   lblParam.Font.Color := clWindowText;
   lblParam.SetBounds(8, 18, 0, 13);
   lblParam.Caption := i18Manager.GetString('lblParam');

   lblParamType := TLabel.Create(gbParams);
   lblParamType.Parent := gbParams;
   lblParamType.Font.Style := [];
   lblParamType.ParentFont := False;
   lblParamType.Font.Color := clWindowText;
   lblParamType.SetBounds(TInfra.Scaled(Self, 92), 18, 0, 13);
   lblParamType.Caption := i18Manager.GetString('lblParamType');

   lblParamDefault := TLabel.Create(gbParams);
   lblParamDefault.Parent := gbParams;
   lblParamDefault.Font.Style := [];
   lblParamDefault.ParentFont := False;
   lblParamDefault.Font.Color := clWindowText;
   lblParamDefault.SetBounds(TInfra.Scaled(Self, 180), 18, 0, 13);
   lblParamDefault.Caption := i18Manager.GetString('lblParamDefault');

   lblParamArray := TLabel.Create(gbParams);
   lblParamArray.Parent := gbParams;
   lblParamArray.Font.Style := [];
   lblParamArray.ParentFont := False;
   lblParamArray.Font.Color := clWindowText;
   lblParamArray.SetBounds(TInfra.Scaled(Self, 239), 18, 0, 13);
   lblParamArray.Caption := i18Manager.GetString('lblParamArray');

   lblParamRef := TLabel.Create(gbParams);
   lblParamRef.Parent := gbParams;
   lblParamRef.Font.Style := [];
   lblParamRef.ParentFont := False;
   lblParamRef.Font.Color := clWindowText;
   lblParamRef.SetBounds(TInfra.Scaled(Self, 281), 18, 0, 13);
   lblParamRef.Caption := i18Manager.GetString('lblParamRef');

   sbxElements := TScrollBox.Create(gbParams);
   sbxElements.Parent := gbParams;
   sbxElements.StyleElements := sbxElements.StyleElements - [seClient];
   sbxElements.SetBounds(6, 36, gbParams.Width-10, 0);
   sbxElements.Ctl3D := False;
   sbxElements.BorderStyle := bsNone;
   sbxElements.Constraints.MaxHeight := 44;
   sbxElements.Constraints.MinWidth := sbxElements.Width;
   sbxElements.VertScrollBar.Tracking := True;
   sbxElements.UseWheelForScrolling := True;
   sbxElements.Anchors := [akTop, akBottom, akLeft];

   btnAddElement := TButton.Create(gbParams);
   btnAddElement.Parent := gbParams;
   btnAddElement.ParentFont := False;
   btnAddElement.Font.Style := [];
   btnAddElement.DoubleBuffered := True;
   btnAddElement.Caption := i18Manager.GetString('btnAddParm');
   btnAddElement.SetBounds(8, 81, gbParams.Width-12, 25);
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

constructor TParameter.Create(AParentTab: TUserFunctionHeader);
begin

   inherited Create(AParentTab.sbxElements, AParentTab.FElementTypeId);

   FParentTab := AParentTab;
   FParentForm := AParentTab.ParentForm;
   Constraints.MaxWidth := AParentTab.sbxElements.Width - 17;
   SetBounds(0, Parent.Height, Constraints.MaxWidth, TInfra.Scaled(Self, 22));
   Align := alTop;
   TInfra.PopulateDataTypeCombo(cbType);
   var w17 := TInfra.Scaled(Self, 17);

   edtDefault := TEdit.Create(Self);
   edtDefault.Parent := Self;
   edtDefault.SetBounds(TInfra.Scaled(Self, 174), 0, TInfra.Scaled(Self, 50), 21);
   edtDefault.ParentFont := False;
   edtDefault.Font.Style := [];
   edtDefault.Font.Color := clGreen;
   edtDefault.ParentCtl3D := False;
   edtDefault.Ctl3D := True;
   edtDefault.OnChange := OnChangeType;

   chkTable := TCheckBox.Create(Self);
   chkTable.Parent := Self;
   chkTable.SetBounds(TInfra.Scaled(Self, 240), 1, w17, w17);
   chkTable.DoubleBuffered := True;
   chkTable.OnClick := OnChangeType;

   chkReference := TCheckBox.Create(Self);
   chkReference.Parent := Self;
   chkReference.SetBounds(TInfra.Scaled(Self, 278), 1, w17, w17);
   chkReference.DoubleBuffered := True;
   chkReference.OnClick := OnChangeType;

end;

function TUserFunction.GetHandle: THandle;
begin
   result := 0;
   if FBody <> nil then
      result := FBody.GetHandle;
end;

procedure TUserFunctionHeader.OnChangeName(Sender: TObject);
begin
   var info := '';
   var lColor := NOK_COLOR;
   var funcName := Trim(edtName.Text);
   if GInfra.ValidateId(funcName) <> VALID_IDENT then
      info := 'BadIdD'
   else if not GInfra.CurrentLang.AllowUserFunctionOverload then
   begin
      if GInfra.GetNativeFunction(funcName) <> nil then
         info := 'DefNtvFunc'
      else if IsDuplicated(edtName) then
         info := 'DupIdD';
   end;
   if info.IsEmpty then
   begin
      lColor := OK_COLOR;
      info := 'OkIdD';
   end;
   edtName.Font.Color := lColor;
   edtName.Hint := i18Manager.GetFormattedString(info, [funcName]);
   DrawBodyLabel;
   inherited OnChangeName(Sender);
end;

procedure TUserFunctionHeader.ExportCode(ALines: TStringList);
begin
   if Assigned(GInfra.CurrentLang.UserFunctionGenerator) then
      GInfra.CurrentLang.UserFunctionGenerator(ALines, FUserFunction, False)
   else
      GInfra.TemplateLang.UserFunctionGenerator(ALines, FUserFunction, False);
end;

procedure TUserFunctionHeader.AddElement(Sender: TObject);
begin
   inherited AddElement(Sender);
   OnClickInclDescFlow(chkInclDescFlow);
end;

procedure TUserFunctionHeader.RefreshElements;
begin
   inherited RefreshElements;
   OnClickInclDescFlow(chkInclDescFlow);
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
begin
   var lCaption := cbBodyPage.Text;
   cbBodyPage.Items.Clear;
   cbBodyPage.Items.BeginUpdate;
   var pageControl := TInfra.GetMainForm.pgcPages;
   for var i := 0 to pageControl.PageCount-1 do
      cbBodyPage.Items.Add(pageControl.Pages[i].Caption);
   cbBodyPage.Items.EndUpdate;
   if ACaption = '' then
   begin
      if lCaption = '' then
         lCaption := pageControl.ActivePage.Caption;
   end
   else
      lCaption := ACaption;
   var i := cbBodyPage.Items.IndexOf(lCaption);
   if i <> -1 then
      cbBodyPage.ItemIndex := i;
   cbBodyPage.DropDownCount := cbBodyPage.Items.Count;
   cbBodyPage.Width := TInfra.GetComboMaxWidth(cbBodyPage);
end;

procedure TUserFunctionHeader.OnChangeBodyPage(Sender: TObject);
begin
   var page := GProject.GetPage(cbBodyPage.Text);
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
   result := GetElements<TParameter>(ByTopParameterComparer);
end;

function TUserFunctionHeader.GetExternModifier: string;
begin
   var lang := GInfra.CurrentLang;
   case chkExternal.State of
      cbChecked:   result := lang.FunctionHeaderExternal;
      cbUnchecked: result := lang.FunctionHeaderNotExternal;
      cbGrayed:    result := lang.FunctionHeaderTransExternal;
   end;
end;

procedure TUserFunctionHeader.OnChangeType(Sender: TObject);
begin
   if Sender = cbType then
   begin
      if ParentForm.Visible and ParentForm.Enabled then // replace with CanFocus once fixed by Embarcadero (RSP-34465)
         ParentForm.SetFocus;
      chkArrayType.Enabled := cbType.ItemIndex > 0;
      cbType.Hint := cbType.Text;
   end;
   if Font.Color <> NOK_COLOR then
      UpdateCodeEditor;
   GProject.SetChanged;
   DrawBodyLabel;
end;

procedure TUserFunction.GenerateTree(ANode: TTreeNode);
begin
   var obj: TObject := FHeader;
   if IsMain then
      obj := FBody;
   var node := ANode.Owner.AddChildObject(ANode, GetTreeNodeText, obj);
   if FBody <> nil then
      FBody.GenerateTree(node);
   if (FHeader <> nil) and TInfra.IsNOkColor(FHeader.Font.Color) then
   begin
      ANode.MakeVisible;
      ANode.Expand(False);
   end;
end;

function TUserFunction.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := '';
   var lang := GInfra.CurrentLang;
   if IsMain then
   begin
      if lang.EnabledUserFunctionHeader then
      begin
         if not Assigned(lang.GetMainProgramDesc) then
            lang := GInfra.TemplateLang;
         result := lang.GetMainProgramDesc;
      end
      else
         result := i18Manager.GetString('Flowchart');
   end
   else
   begin
      if not Assigned(lang.GetUserFuncDesc) then
         lang := GInfra.TemplateLang;
      result := lang.GetUserFuncDesc(FHeader, False, False).Trim;
   end;
end;

function TUserFunctionHeader.GetTreeNodeText(ANodeOffset: integer = 0): string;
begin
   result := FUserFunction.GetTreeNodeText(ANodeOffset);
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

procedure TUserFunctionHeader.OnClickGenDesc(Sender: TObject);
begin
   var lang := GInfra.CurrentLang;
   if not Assigned(lang.GetUserFuncHeaderDesc) then
      lang := GInfra.TemplateLang;
   var description := lang.GetUserFuncHeaderDesc(Self).TrimRight;
   if not description.IsEmpty then
   begin
      memDesc.Text := description;
      GProject.SetChanged;
   end;
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

procedure TUserFunctionHeader.ExportToXML(ANode: IXMLNode);
begin
   var functionNode := AppendNode(ANode, FUNCTION_TAG);
   var headerNode := AppendNode(functionNode, HEADER_TAG);
   inherited ExportToXML(headerNode);
   SetNodeAttrStr(headerNode, TYPE_ATTR, IfThen(cbType.ItemIndex = 0, 'none', cbType.Text));
   if memDesc.Text <> '' then
      SetNodeCData(headerNode, 'desc', ReplaceStr(memDesc.Text, sLineBreak, LB_PHOLDER));
   SetNodeAttrBool(headerNode, 'show_body', chkBodyVisible.Checked);
   SetNodeAttrBool(headerNode, 'desc_incl', chkInclDescCode.Checked);
   SetNodeAttrBool(headerNode, 'desc_incl_flow', chkInclDescFlow.Checked);
   FLocalVars.ExportToXML(headerNode);
   SetNodeAttrInt(headerNode, 'descrh', gbDesc.Height);
   SetNodeAttrInt(headerNode, 'headerh', gbHeader.Height);
   SetNodeAttrInt(headerNode, 'parmsh', gbParams.Height);
   SetNodeAttrInt(headerNode, 'lvarsh', FLocalVars.Height);
   SetNodeAttrBool(headerNode, 'arrayType', chkArrayType.Checked);
   SetNodeAttrBool(headerNode, 'constructor', chkConstructor.Checked);
   if chkStatic.Visible then
      SetNodeAttrBool(headerNode, 'static', chkStatic.Checked);
   if (FUserFunction <> nil) and (FUserFunction.Body <> nil) then
      FUserFunction.Body.ExportToXML(functionNode);
end;

procedure TUserFunctionHeader.ImportFromXML(ANode: IXMLNode; APinControl: TControl = nil);
begin
   inherited ImportFromXML(ANode, APinControl);
   var i := cbType.Items.IndexOf(GetNodeAttrStr(ANode, TYPE_ATTR));
   if i <> -1 then
      cbType.ItemIndex := i
   else if cbType.Items.Count > 0 then
      cbType.ItemIndex := 0;
   if Assigned(cbType.OnChange) then
      cbType.OnChange(cbType);
   var node := FindNode(ANode, 'desc');
   if node <> nil then
      memDesc.Text := ReplaceStr(node.Text, LB_PHOLDER, sLineBreak);
   chkBodyVisible.Checked := GetNodeAttrBool(ANode, 'show_body');
   chkInclDescCode.Checked := GetNodeAttrBool(ANode, 'desc_incl');
   chkInclDescFlow.Checked := GetNodeAttrBool(ANode, 'desc_incl_flow');
   chkArrayType.Checked := GetNodeAttrBool(ANode, 'arrayType', False);
   chkConstructor.Checked := GetNodeAttrBool(ANode, 'constructor', False);
   if chkStatic.Visible then
      chkStatic.Checked := GetNodeAttrBool(ANode, 'static', False);
   FLocalVars.ImportFromXML(ANode, impAll);
   gbDesc.Height := GetNodeAttrInt(ANode, 'descrh');
   gbHeader.Height := GetNodeAttrInt(ANode, 'headerh');
   gbParams.Height := GetNodeAttrInt(ANode, 'parmsh');
   sbxElements.Constraints.MaxHeight := gbParams.Height - 66;
   FLocalVars.Height := GetNodeAttrInt(ANode, 'lvarsh');
end;

procedure TParameter.OnChangeType(Sender: TObject);
begin
   inherited OnChangeType(Sender);
   TUserFunctionHeader(ParentTab).DrawBodyLabel;
end;

procedure TParameter.OnChangeName(Sender: TObject);
begin
   inherited OnChangeName(Sender);
   TUserFunctionHeader(ParentTab).DrawBodyLabel;
end;

procedure TParameter.ImportFromXML(ANode: IXMLNode);
begin
   inherited ImportFromXML(ANode);
   chkTable.Checked := GetNodeAttrBool(ANode, 'table');
   chkReference.Checked := GetNodeAttrBool(ANode, 'reference');
   edtDefault.Text := GetNodeAttrStr(ANode, 'default', '');
end;

function TParameter.ExportToXML(ANode: IXMLNode): IXMLNode;
begin
   result := inherited ExportToXML(ANode);
   SetNodeAttrBool(result, 'table', chkTable.Checked);
   SetNodeAttrBool(result, 'reference', chkReference.Checked);
   if edtDefault.Text <> '' then
      SetNodeAttrStr(result, 'default', edtDefault.Text);
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
   if (L = nil) and (R = nil) then
      result := 0
   else if L = nil then
      result := 241
   else if R = nil then
      result := -241
   else if FCompareType = Z_ORDER_COMPARE then
   begin
      if (L.Body = nil) and (R.Body = nil) then
         result := 0
      else if L.Body = nil then
         result := 163
      else if R.Body = nil then
         result := -163
      else
         result := L.Body.GetZOrder - R.Body.GetZOrder;
   end
   else if FCompareType = PAGE_INDEX_COMPARE then
   begin
      if (L.Header = nil) and (R.Header = nil) then
         result := 0
      else if L.Header = nil then
         result := 111
      else if R.Header = nil then
         result := -111
      else
         result := L.Header.PageIndex - R.Header.PageIndex;
   end
   else if FCompareType = NAME_COMPARE then
      result := TComparer<string>.Default.Compare(L.GetName, R.GetName)
   else
      result := -170;
end;

initialization

   ByTopParameterComparer := TDelegatedComparer<TParameter>.Create(
      function(const L, R: TParameter): integer
      begin
         result := L.Top - R.Top;
      end
   );

end.

