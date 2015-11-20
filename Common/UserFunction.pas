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
      function ExportToXMLTag(const root: IXMLElement): IXMLElement; override;
      procedure ImportFromXMLTag(const root: IXMLElement); override;
   end;

   TUserFunctionHeader = class(TTabComponent)
   private
      FOwnerFunction: TUserFunction;
      FLocalVars: TVarDeclareList;
   protected
      procedure OnChangeName(Sender: TObject); override;
      procedure OnChangeDesc(Sender: TObject);
      procedure OnClickInclDescFlow(Sender: TObject);
      procedure OnClickInclDescCode(Sender: TObject);
      procedure OnChangeType(Sender: TObject);
      procedure OnMovedParams(Sender: TObject);
      procedure OnClickExtDecl(Sender: TObject);
      procedure SetActive(const AValue: boolean); override;
      function CreateElement: TElement; override;
      procedure RedrawBody;
   public
      cbType: TComboBox;
      lblType,
      lblParameters: TLabel;
      gbHeader,
      gbParameters,
      gbDescription: TGroupBox;
      memDescription: TMemo;
      chkInclDescCode,
      chkInclDescFlow: TCheckBox;
      splDescription,
      splParameters: TSplitter;
      property OwnerFunction: TUserFunction read FOwnerFunction default nil;
      property LocalVars: TVarDeclareList read FLocalVars default nil;
      property ParameterCount: integer read GetElementCount default 0;
      constructor Create(const AParentForm: TFunctionsForm);
      destructor Destroy; override;
      procedure ExportToXMLTag(const rootTag: IXMLElement); override;
      procedure ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl = nil);
      function GetParameterIterator: IIterator;
      procedure Localize(const list: TStringList); override;
      procedure RefreshSizeEdits; override;
      procedure GenerateDescription(const ALines: TStrings);
   end;

   TUserFunction = class(TComponent, IXMLable, ITabbable, IIdentifiable, ISizeEditable, IWinControl, IMaxBoundable, ISortable)
   private
      FHeader: TUserFunctionHeader;
      FBody: TMainBlock;
      FActive: boolean;
      procedure SetActive(const AValue: boolean);
      function GetActive: boolean;
      procedure ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl = nil); virtual; abstract;
   public
      property Header: TUserFunctionHeader read FHeader default nil;
      property Body: TMainBlock read FBody default nil;
      property Active: boolean read GetActive write SetActive;
      constructor Create(const AFunctionHeader: TUserFunctionHeader; const AFunctionBody: TMainBlock);
      destructor Destroy; override;
      procedure ExportToXMLTag(const rootTag: IXMLElement);
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
      procedure SetZOrderValue(const AValue: integer);
      function GetZOrderValue: integer;
      function IsMain: boolean;
   end;

implementation

uses
   SysUtils, ApplicationCommon, Main_Form, XMLProcessor, Windows, Grids, StrUtils,
   LangDefinition, Navigator_Form;

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
      FHeader.FOwnerFunction := Self;
      FHeader.FOverlayObject := Self;
      if (FBody <> nil) and FHeader.chkExtDeclare.Checked then
         FBody.Visible := false;
   end;
   if FBody <> nil then
   begin
      FBody.OwnerUserFunction := Self;
      FBody.SetWidth(FBody.Width);
      FBody.ParentForm.SetScrollBars;
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
         result := FBody.GetZOrderValue;
   end
   else if FHeader <> nil then
      result := FHeader.GetSortValue(ASortType);
end;

procedure TUserFunction.BringAllToFront;
begin
   if FBody <> nil then
      FBody.BringAllToFront;
end;

procedure TUserFunction.SetZOrderValue(const AValue: integer);
begin
   if FBody <> nil then
      FBody.SetZOrderValue(AValue);
end;

function TUserFunction.GetZOrderValue: integer;
begin
   result := -1;
   if FBody <> nil then
      result := FBody.GetZOrderValue;
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
      if (OwnerFunction <> nil) and (OwnerFunction.Active <> AValue) then
         OwnerFunction.Active := AValue;
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
begin
   if AValue <> FActive then
   begin
      FActive := AValue;
      if FBody <> nil then
      begin
         FBody.SetVisible(AValue and not ((FHeader <> nil) and FHeader.chkExtDeclare.Checked));
         if FBody.Visible then
            FBody.RefreshStatements;
         FBody.ParentForm.SetScrollBars;
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

procedure TUserFunction.ExportToXMLTag(const rootTag: IXMLElement);
var
   tag: IXMLElement;
begin
   if FHeader <> nil then
      FHeader.ExportToXMLTag(rootTag)
   else if FBody <> nil then
   begin
      tag := rootTag.OwnerDocument.CreateElement('routine');
      rootTag.AppendChild(tag);
      FBody.ExportToXMLTag(tag);
   end;
end;

constructor TUserFunctionHeader.Create(const AParentForm: TFunctionsForm);
begin

   inherited Create(AParentForm);

   FElementMode := PARAMETER_IDENT;

   FLocalVars := TVarDeclareList.Create(Self, 0, 300, 389, 3, 4, 380);
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

   gbHeader := TGroupBox.Create(Self);
   gbHeader.Parent := Self;
   gbHeader.SetBounds(0, 80, 400, 83);
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
   cbType.SetBounds(lblType.Left + lblType.Width + 6, 20, 85, 21);
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
   gbParameters.SetBounds(0, 165, 400, 110);
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

procedure TUserFunctionHeader.Localize(const list: TStringList);
begin
   lblType.Caption := list.Values['lblType'];
   lblParameters.Caption := list.Values['lblParameters'];
   btnAddElement.Caption := list.Values['btnAddParm'];
   edtLibrary.Hint := Format(list.Values['edtLibraryHint'], [GInfra.CurrentLang.LibraryExt]);
   if cbType.Items.Count > 0 then
   begin
      cbType.Items[0] := list.Values['NoType'];
      if cbType.ItemIndex = -1 then
         cbType.ItemIndex := 0;
   end;
   inherited Localize(list);
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
   if ValidateId(lFuncName) <> VALID_IDENT then
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
   RedrawBody;
   inherited OnChangeName(Sender);
end;

procedure TUserFunctionHeader.OnChangeDesc(Sender: TObject);
begin
   if GSettings.ShowFlowchartLabels and chkInclDescFlow.Checked then
      RedrawBody;
   if GSettings.UpdateEditor and not chkExtDeclare.Checked and (Font.Color <> NOK_COLOR) and chkInclDescCode.Checked then
      TInfra.GetEditorForm.RefreshEditorForObject(Self);
   GChange := 1;
end;

procedure TUserFunctionHeader.OnChangeType(Sender: TObject);
begin
   if GSettings.UpdateEditor and (Font.Color <> NOK_COLOR) and not chkExtDeclare.Checked then
      TInfra.GetEditorForm.RefreshEditorForObject(Self);
   GChange := 1;
   RedrawBody;
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
   if (FBody <> nil) and FBody.Visible then
      FBody.GenerateTree(lNode);
   if (FHeader <> nil) and TInfra.IsRestricted(FHeader.Font.Color) then
   begin
      ANode.MakeVisible;
      ANode.Expand(false);
   end;
end;


function TUserFunction.IsMain: boolean;
begin
   result := FHeader = nil;
end;

procedure TUserFunctionHeader.OnClickExtDecl(Sender: TObject);
begin
   if (FOwnerFunction <> nil) and (FOwnerFunction.Body <> nil) then
   begin
      FOwnerFunction.Body.SetVisible(not chkExtDeclare.Checked);
      FOwnerFunction.Body.ParentForm.SetScrollBars;
      if GSettings.UpdateEditor and (Font.Color <> NOK_COLOR) then
         TInfra.GetEditorForm.RefreshEditorForObject(Self);
   end;
   GChange := 1;
end;

procedure TUserFunctionHeader.OnClickInclDescFlow(Sender: TObject);
begin
   if GSettings.ShowFlowchartLabels then
   begin
      RedrawBody;
      NavigatorForm.Repaint;
   end;
   GChange := 1;
end;

procedure TUserFunctionHeader.OnClickInclDescCode(Sender: TObject);
begin
   if GSettings.UpdateEditor and (Font.Color <> NOK_COLOR) and not chkExtDeclare.Checked then
      TInfra.GetEditorForm.RefreshEditorForObject(Self);
end;

procedure TUserFunctionHeader.RedrawBody;
begin
   if (FOwnerFunction <> nil) and (FOwnerFunction.Body <> nil) then
   begin
      FOwnerFunction.Body.SetWidth(0);
      FOwnerFunction.Body.RepaintAll;
   end;
end;

procedure TUserFunctionHeader.ExportToXMLTag(const rootTag: IXMLElement);
var
   tag, tag2, tag3: IXMLElement;
   lType: string;
begin
   tag := rootTag.OwnerDocument.CreateElement('routine');
   rootTag.AppendChild(tag);
   tag2 := rootTag.OwnerDocument.CreateElement('header');
   tag.AppendChild(tag2);
   inherited ExportToXMLTag(tag2);
   if cbType.ItemIndex = 0 then
      lType := 'none'
   else
      lType := cbType.Text;
   tag2.SetAttribute('type', lType);
   if memDescription.Text <> '' then
   begin
      tag3 := rootTag.OwnerDocument.CreateElement('desc');
      TXMLProcessor.AddCDATA(tag3, AnsiReplaceStr(memDescription.Text, CRLF, '#!'));
      tag2.AppendChild(tag3);
   end;
   tag2.SetAttribute('desc_incl', BoolToStr(chkInclDescCode.Checked, true));
   tag2.SetAttribute('desc_incl_flow', BoolToStr(chkInclDescFlow.Checked, true));
   FLocalVars.ExportToXMLTag(tag2);
   tag2.SetAttribute('descrh', IntToStr(gbDescription.Height));
   tag2.SetAttribute('headerh', IntToStr(gbHeader.Height));
   tag2.SetAttribute('parmsh', IntToStr(gbParameters.Height));
   tag2.SetAttribute('lvarsh', IntToStr(FLocalVars.Height));
   if (OwnerFunction <> nil) and (OwnerFunction.Body <> nil) then
      OwnerFunction.Body.ExportToXMLTag(tag);
end;

procedure TUserFunctionHeader.ImportFromXMLTag(const rootTag: IXMLElement; const APinControl: TControl = nil);
var
   idx: integer;
   tag2: IXMLElement;
begin
   inherited ImportFromXMLTag(rootTag, APinControl);
   idx := cbType.Items.IndexOf(rootTag.GetAttribute('type'));
   if idx <> -1 then
      cbType.ItemIndex := idx
   else if cbType.Items.Count > 0 then
      cbType.ItemIndex := 0;
   cbType.OnChange(cbType);
   tag2 := TXMLProcessor.FindChildTag(rootTag, 'desc');
   if tag2 <> nil then
      memDescription.Text := AnsiReplaceStr(tag2.Text, '#!', CRLF);
   chkInclDescCode.Checked := rootTag.GetAttribute('desc_incl') = 'True';
   chkInclDescFlow.Checked := rootTag.GetAttribute('desc_incl_flow') = 'True';
   FLocalVars.ImportFromXMLTag(rootTag);
   idx := StrToIntDef(rootTag.GetAttribute('descrh'), -1);
   if idx > -1 then
      gbDescription.Height := idx;
   idx := StrToIntDef(rootTag.GetAttribute('headerh'), -1);
   if idx > -1 then
      gbHeader.Height := idx;
   idx := StrToIntDef(rootTag.GetAttribute('parmsh'), -1);
   if idx > -1 then
   begin
      gbParameters.Height := idx;
      sbxElements.Constraints.MaxHeight := gbParameters.Height - 66;
   end;
   idx := StrToIntDef(rootTag.GetAttribute('lvarsh'), -1);
   if idx > -1 then
      FLocalVars.Height := idx;
end;

procedure TParameter.ImportFromXMLTag(const root: IXMLElement);
begin
   inherited ImportFromXMLTag(root);
   chkTable.Checked := CompareText(root.GetAttribute('table'), 'true') = 0;
   chkReference.Checked := CompareText(root.GetAttribute('reference'), 'true') = 0;
end;

function TParameter.ExportToXMLTag(const root: IXMLElement): IXMLElement;
var
   tag: IXMLElement;
begin
   tag := inherited ExportToXMLTag(root);
   tag.SetAttribute('table', BoolToStr(chkTable.Checked, true));
   tag.SetAttribute('reference', BoolToStr(chkReference.Checked, true));
end;

function TUserFunctionHeader.GetParameterIterator: IIterator;
begin
   result := GetElementIterator;
end;

end.