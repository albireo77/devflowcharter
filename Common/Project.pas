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



unit Project;

interface

uses
   WinApi.Windows, Vcl.Graphics, System.Classes, Vcl.ComCtrls, Vcl.Controls, System.Contnrs,
   Generics.Defaults, UserFunction, OmniXML, UserDataType, Main_Block, DeclareList,
   Types, Interfaces, BlockTabSheet, Comment, Declarations_Form, Base_Block;

type

   TProject = class(TInterfacedPersistent, IExportable)
   private
      FGlobalVars: TVarDeclareList;
      FGlobalConsts: TConstDeclareList;
      FIntegerTypesSet,
      FRealTypesSet,
      FBoolTypesSet,
      FOtherTypesSet,
      FPointerTypesSet,
      FRecordTypesSet,
      FEnumTypesSet,
      FArrayTypesSet,
      FStringTypesSet: TTypesSet;
      FComponentList: TComponentList;
      FObjectIds: TStringList;
      FNextObjectId: integer;
      FMainPage: TBlockTabSheet;
      FChanged: boolean;
      class var FInstance: TProject;
      constructor Create;
      procedure SetGlobalDeclarations(AForm: TDeclarationsForm);
      function GetIWinControlComponent(AHandle: THandle): IWinControl;
      procedure RefreshZOrder;
      procedure ExportPagesToXML(ANode: IXMLNode);
      function GetSelectList(ANode: IXMLNode; const ALabel, ANodeName: string; const ANodeName2: string = ''): TStringList;
      function GetComponents<T: class>(AComparer: IComparer<T> = nil): IEnumerable<T>;
      function GetIComponents<I: IInterface>(AComparer: IComparer<TComponent> = nil): IEnumerable<I>; overload;
      function GetIComponents<T: class; I: IInterface>(AComparer: IComparer<T> = nil): IEnumerable<I>; overload;
      function GetComponent<T: class>(const AName: string): T;
   public
      Name: string;
      ChangingOn: boolean;
      HeaderComment: TComment;
      LibSectionOffset: integer;
      property IntegerTypesSet: TTypesSet read FIntegerTypesSet;
      property BoolTypesSet: TTypesSet read FBoolTypesSet;
      property OtherTypesSet: TTypesSet read FOtherTypesSet;
      property RealTypesSet: TTypesSet read FRealTypesSet;
      property PointerTypesSet: TTypesSet read FPointerTypesSet;
      property RecordTypesSet: TTypesSet read FRecordTypesSet;
      property EnumTypesSet: TTypesSet read FEnumTypesSet;
      property ArrayTypesSet: TTypesSet read FArrayTypesSet;
      property StringTypesSet: TTypesSet read FStringTypesSet;
      property GlobalVars: TVarDeclareList read FGlobalVars default nil;
      property GlobalConsts: TConstDeclareList read FGlobalConsts default nil;
      class function GetInstance: TProject;
      destructor Destroy; override;
      procedure AddComponent(AComponent: TComponent);
      function GetComments: IEnumerable<TComment>;
      function GetUserFunctions: IEnumerable<TUserFunction>;
      function GetUserDataTypes: IEnumerable<TUserDataType>;
      function GetUserFunction(const AName: string): TUserFunction;
      function GetUserDataType(const AName: string): TUserDataType;
      procedure ExportCode(ALines: TStringList);
      procedure ExportToGraphic(AGraphic: TGraphic);
      procedure ExportToXML(ANode: IXMLNode);
      function ExportToXMLFile(const AFile: string): TError;
      function ImportFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
      function ImportUserFunctionsFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
      function ImportUserDataTypesFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
      function ImportCommentsFromXML(ANode: IXMLNode): integer;
      procedure ImportPagesFromXML(ANode: IXMLNode);
      function GetMain: TMainBlock;
      procedure PopulateDataTypeCombos;
      procedure RefreshStatements;
      procedure ChangeDesktopColor(AColor: TColor);
      function CountErrWarn: TErrWarnCount;
      procedure GenerateTree(ANode: TTreeNode);
      procedure RepaintFlowcharts;
      function GetLibraryList: TStringList;
      function FindObject(AId: integer): TObject;
      procedure RefreshSizeEdits;
      procedure RefreshVarTypes;
      procedure PopulateDataTypeSets;
      procedure UpdateZOrder(AParent: TWinControl);
      function Register(AObject: TObject; AId: integer = ID_UNDEFINED): integer;
      procedure UnRegister(AObject: TObject);
      function GetPage(const ACaption: string; ACreate: boolean = True): TBlockTabSheet;
      function MainPage: TBlockTabSheet;
      function ActivePage: TBlockTabSheet;
      procedure UpdateHeadersBody(APage: TTabSheet);
      function FindMainBlockForControl(AControl: TControl): TMainBlock;
      function GetProgramHeader: string;
      function GetExportFileName: string;
      procedure SetChanged;
      function IsChanged: boolean;
      function IsNew: boolean;
      procedure SetNotChanged;
      function FindSelectedBlock: TBlock;
      function FindRedArrowBlock: TBlock;
      function FindUserFunction(ABody: TGroupBlock): TUserFunction;
      function FindFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
   end;

implementation

uses
   System.SysUtils, Vcl.Menus, Vcl.Forms, System.StrUtils, System.Types, System.UITypes,
   Generics.Collections, Infrastructure, Constants, XMLProcessor, Base_Form, Navigator_Form,
   ParserHelper, SelectImport_Form, BaseEnumerator, WinApi.Messages, Vcl.ExtCtrls,
   Rtti, OmniXMLUtils;

var
   ByPageIndexUserDataTypeComparer: IComparer<TUserDataType>;
   ByPageIndexUserFunctionComparer: IComparer<TUserFunction>;
   ByPageIndexComponentComparer: IComparer<TComponent>;
   ByZOrderComponentComparer: IComparer<TComponent>;

constructor TProject.Create;
begin
   inherited Create;
   FObjectIds := TStringList.Create;
   FComponentList := TComponentList.Create;
   LibSectionOffset := -1;
end;

destructor TProject.Destroy;
begin
   while not FComponentList.IsEmpty do             // automatic disposing objects that are stored in list by calling list's destructor
      FComponentList[0].Free;                    // will generate EListError exception for pinned comments
   FComponentList.Free;                          // so to destroy FComponentList, objects must be freed in while loop first
   FGlobalVars.Free;
   FGlobalConsts.Free;
   FObjectIds.Free;
   FInstance := nil;
   inherited Destroy;
end;

class function TProject.GetInstance: TProject;
begin
   if FInstance = nil then
   begin
      FInstance := TProject.Create;
      FInstance.SetGlobalDeclarations(TInfra.GetDeclarationsForm);
      FInstance.PopulateDataTypeSets;
   end;
   result := FInstance;
end;

function TProject.GetPage(const ACaption: string; ACreate: boolean = True): TBlockTabSheet;
begin
   result := nil;
   var caption := ACaption.Trim;
   if not caption.IsEmpty then
   begin
      var pageControl := TInfra.GetMainForm.pgcPages;
      for var i := 0 to pageControl.PageCount-1 do
      begin
         if SameCaption(pageControl.Pages[i].Caption, caption) then
         begin
            result := TBlockTabSheet(pageControl.Pages[i]);
            break;
         end;
      end;
      if result = nil then
      begin
         if SameCaption(caption, MAIN_PAGE_MARKER) then
            result := MainPage
         else if ACreate then
         begin
            result := TBlockTabSheet.Create(TInfra.GetMainForm);
            result.Caption := caption;
         end;
      end;
   end;
end;

function TProject.MainPage: TBlockTabSheet;
begin
   if FMainPage = nil then
      FMainPage := GetPage(trnsManager.GetString(DEF_PAGE_CAPTION_KEY));
   result := FMainPage;
end;

function TProject.ActivePage: TBlockTabSheet;
begin
   result := TBlockTabSheet(TInfra.GetMainForm.pgcPages.ActivePage);
end;

function TProject.GetExportFileName: string;
begin
   result := Name;
end;

procedure TProject.PopulateDataTypeSets;
begin
   FIntegerTypesSet := [];
   FRealTypesSet    := [];
   FBoolTypesSet    := [];
   FOtherTypesSet   := [];
   FRecordTypesSet  := [];
   FEnumTypesSet    := [];
   FArrayTypesSet   := [];
   FStringTypesSet  := [];
   FPointerTypesSet := [GENERIC_PTR_TYPE];

   if FGlobalVars <> nil then
   begin
      for var i := 0 to FGlobalVars.cbType.Items.Count-1 do
      begin
         var typeName := FGlobalVars.cbType.Items[i];
         var userType := GetComponent<TUserDataType>(typeName);
         var nativeType := GInfra.GetNativeDataType(typeName);
         var typesSet: PTypesSet := @FOtherTypesSet;
         if nativeType <> nil then
         begin
            case nativeType.Kind of
               tpInt:    typesSet := @FIntegerTypesSet;
               tpReal:   typesSet := @FRealTypesSet;
               tpString: typesSet := @FStringTypesSet;
               tpBool:   typesSet := @FBoolTypesSet;
               tpPtr:    typesSet := @FPointerTypesSet;
            end;
         end
         else if userType <> nil then
         begin
            case userType.Kind of
               dtInt:    typesSet := @FIntegerTypesSet;
               dtReal:   typesSet := @FRealTypesSet;
               dtRecord: typesSet := @FRecordTypesSet;
               dtEnum:   typesSet := @FEnumTypesSet;
               dtArray:  typesSet := @FArrayTypesSet;
            end;
         end
         else if Assigned(GInfra.CurrentLang.IsPointerType) and GInfra.CurrentLang.IsPointerType(typeName) then
            typesSet := @FPointerTypesSet;
         Include(typesSet^, i);
      end;
   end;
end;

procedure TProject.AddComponent(AComponent: TComponent);
begin
   FComponentList.Add(AComponent);
end;

function TProject.GetComments: IEnumerable<TComment>;
begin
   result := GetComponents<TComment>;
end;

function TProject.GetUserFunctions: IEnumerable<TUserFunction>;
begin
   result := GetComponents<TUserFunction>(ByPageIndexUserFunctionComparer);
end;

function TProject.GetUserDataTypes: IEnumerable<TUserDataType>;
begin
   result := GetComponents<TUserDataType>(ByPageIndexUserDataTypeComparer);
end;

function TProject.GetIComponents<I>(AComparer: IComparer<TComponent> = nil): IEnumerable<I>;
begin
   result := GetIComponents<TComponent, I>(AComparer);
end;

function TProject.GetComponents<T>(AComparer: IComparer<T> = nil): IEnumerable<T>;
begin
   var list := TList<T>.Create;
   for var i := 0 to FComponentList.Count-1 do
   begin
      var comp := FComponentList[i];
      if comp is T then
         list.Add(comp);
   end;
   if AComparer <> nil then
      list.Sort(AComparer);
   result := TEnumeratorFactory<T>.Create(list);
end;

function TProject.GetIComponents<T, I>(AComparer: IComparer<T> = nil): IEnumerable<I>;
begin
   var list := TList<I>.Create;
   var rType := TRttiContext.Create.GetType(TypeInfo(I));
   var intf: I := nil;
   if rType is TRttiInterfaceType then
   begin
      var iType := rType as TRttiInterfaceType;
      for var comp in GetComponents<T>(AComparer) do
      begin
         if Supports(comp, iType.GUID, intf) then
            list.Add(intf);
      end;
   end;
   result := TEnumeratorFactory<I>.Create(list);
end;

function TProject.Register(AObject: TObject; AId: integer = ID_UNDEFINED): integer;
begin
   var id := AId.ToString;
   var accepted := (AId <> ID_UNDEFINED) and not FObjectIds.Contains(id);
   var idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
   begin
      result := FObjectIds[idx].ToInteger;
      if accepted then
         FObjectIds[idx] := id;
   end
   else if accepted then
      FObjectIds.AddObject(id, AObject)
   else
   begin
      FObjectIds.AddObject(FNextObjectId.ToString, AObject);
      result := FNextObjectId;
      Inc(FNextObjectId);
   end;
   if accepted then
   begin
      if FNextObjectId <= AId then
         FNextObjectId := AId + 1;
      result := AId;
   end;
end;

procedure TProject.UnRegister(AObject: TObject);
begin
   var idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
      FObjectIds.Delete(idx);
end;

function TProject.FindObject(AId: integer): TObject;
begin
   result := nil;
   var idx := FObjectIds.IndexOf(AId.ToString);
   if idx <> -1 then
      result := FObjectIds.Objects[idx];
end;

procedure TProject.ExportPagesToXML(ANode: IXMLNode);
begin
   var pageControl := TInfra.GetMainForm.pgcPages;
   var pagesNode := AppendNode(ANode, 'pages');
   for var i := 0 to pageControl.PageCount-1 do
      TBlockTabSheet(pageControl.Pages[i]).ExportToXML(pagesNode);
end;

procedure TProject.SetChanged;
begin
   if ChangingOn and not FChanged then
   begin
      FChanged := True;
      TInfra.GetMainForm.SetChanged;
   end;
end;

procedure TProject.SetNotChanged;
begin
   if ChangingOn then
      FChanged := False;
end;

function TProject.IsChanged: boolean;
begin
   result := FChanged;
end;

function TProject.IsNew: boolean;
begin
   result := MatchText(TInfra.GetMainForm.Caption, [NEW_PROJECT_CAPTION, NEW_PROJECT_CAPTION + '*']);
end;

function TProject.ExportToXMLFile(const AFile: string): TError;
begin
   ChangingOn := False;
   result := TXMLProcessor.ExportToXMLFile(ExportToXML, AFile);
   ChangingOn := True;
   if result = errNone then
   begin
      FChanged := False;
      TInfra.GetMainForm.AcceptFile(AFile);
   end;
end;

procedure TProject.ExportToXML(ANode: IXMLNode);
begin

   SetNodeAttrStr(ANode, LANG_ATTR, GInfra.CurrentLang.Name);
   SetNodeAttrStr(ANode, APP_VERSION_ATTR, TInfra.AppVersion);

   ExportPagesToXML(ANode);

   if MainPage <> ActivePage then
      SetNodeAttrStr(ANode, PAGE_FRONT_ATTR, ActivePage.Caption);

   if FGlobalVars <> nil then
      FGlobalVars.ExportToXML(ANode);
   if FGlobalConsts <> nil then
      FGlobalConsts.ExportToXML(ANode);

   var pageControl := TInfra.GetMainForm.pgcPages;
   for var i := 0 to pageControl.PageCount-1 do
      UpdateZOrder(TBlockTabSheet(pageControl.Pages[i]).Box);

   for var xmlable in GetIComponents<IXMLable>(ByPageIndexComponentComparer) do
   begin
      if xmlable.Active then
         xmlable.ExportToXML(ANode);
   end;

   for var baseForm in TInfra.GetBaseForms do
      baseForm.ExportToXML(ANode);
end;

procedure TProject.ImportPagesFromXML(ANode: IXMLNode);

begin
   if ANode <> nil then
   begin
      var activePage: TBlockTabSheet := nil;
      var pageFront := GetNodeAttrStr(ANode, PAGE_FRONT_ATTR, '');
      var pageNodes := FilterNodes(FindNode(ANode, 'pages'), 'page');
      var pageNode := pageNodes.NextNode;
      while pageNode <> nil do
      begin
         var page := GetPage(GetNodeAttrStr(pageNode, 'name', ''));
         if page <> nil then
         begin
            page.ImportFromXML(pageNode);
            if (activePage = nil) and SameCaption(page.Caption, pageFront) then
               activePage := page;
         end;
         pageNode := pageNodes.NextNode;
      end;
      if activePage = nil then
         activePage := MainPage;
      activePage.SetAsActivePage;
   end;
end;

function TProject.ImportFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
begin

   result := errValidate;

   var langName := GetNodeAttrStr(ANode, LANG_ATTR);
   if GInfra.GetLangDefinition(langName) = nil then
   begin
      Gerr_text := trnsManager.GetFormattedString('LngNoSprt', [langName]);
      Exit;
   end;

   var ver := GetNodeAttrStr(ANode, APP_VERSION_ATTR, '');
   if TInfra.CompareWithAppVersion(ver) > 0 then
      TInfra.ShowWarningBox('OldVerMsg', [ver]);

   var s := IfThen(SameText(langName, GInfra.TemplateLang.Name), 'ChangeLngNone', 'ChangeLngAsk');

   if (not SameText(GInfra.CurrentLang.Name, langName)) and
      (TInfra.ShowQuestionBox(s, [langName.Trim, sLineBreak], MB_YESNO+MB_ICONQUESTION) = mrYes) then
   begin
      GSettings.CurrentLangName := langName;
{$IFDEF USE_CODEFOLDING}
      TInfra.GetEditorForm.ReloadFoldRegions;
{$ENDIF}
      TInfra.GetEditorForm.SetFormAttributes;
      SetGlobalDeclarations(TInfra.GetDeclarationsForm);
      TInfra.GetMainForm.SetProjectMenu(True);
   end;

   if FGlobalConsts <> nil then
      FGlobalConsts.ImportFromXML(ANode, impAll);

   ImportUserDataTypesFromXML(ANode, impAll);
   ImportPagesFromXML(ANode);

   result := ImportUserFunctionsFromXML(ANode, impAll);
   if result = errNone then
   begin
      if FGlobalVars <> nil then
         FGlobalVars.ImportFromXML(ANode, impAll);
      PopulateDataTypeCombos;
      RefreshSizeEdits;
      RefreshStatements;
      ImportCommentsFromXML(ANode);
      RefreshZOrder;
      for var baseForm in TInfra.GetBaseForms do
         baseForm.ImportFromXML(ANode);
   end;
end;

procedure TProject.SetGlobalDeclarations(AForm: TDeclarationsForm);
begin
   FreeAndNil(FGlobalVars);
   FreeAndNil(FGlobalConsts);
   if GInfra.CurrentLang.EnabledVars then
      FGlobalVars := TVarDeclareList.Create(AForm, 2, 1, 358, 6, 5);
   if GInfra.CurrentLang.EnabledConsts then
   begin
      var splitter: TSplitter := nil;
      var x := 2;
      if FGlobalVars <> nil then
      begin
         FGlobalVars.Align := alLeft;
         x := FGlobalVars.BoundsRect.Right + 5;
         splitter := TSplitter.Create(AForm);
         splitter.Parent := AForm;
         splitter.Left := x - 4;
         splitter.Align := FGlobalVars.Align;
         FGlobalVars.SetSplitter(splitter);
      end;
      FGlobalConsts := TConstDeclareList.Create(AForm, x, 1, 235, 6, 3);
      if splitter <> nil then
         FGlobalConsts.Align := alClient;
   end;
   if FGlobalVars <> nil then
   begin
      FGlobalVars.Caption := trnsManager.GetString(GInfra.CurrentLang.GlobalVarsLabelKey);
      FGlobalVars.SetExternalColumn(4);
      FGlobalVars.Associate := FGlobalConsts;
      AForm.Width := FGlobalVars.BoundsRect.Right + DECLARATIONS_FORM_RIGHT_MARGIN;
      if FGlobalConsts = nil then
         FGlobalVars.Align := alClient;
   end;
   if FGlobalConsts <> nil then
   begin
      FGlobalConsts.Caption := trnsManager.GetString(GInfra.CurrentLang.GlobalConstsLabelKey);
      FGlobalConsts.SetExternalColumn(2);
      FGlobalConsts.Associate := FGlobalVars;
      AForm.Width := FGlobalConsts.BoundsRect.Right + DECLARATIONS_FORM_RIGHT_MARGIN;
   end;
end;

procedure TProject.ExportCode(ALines: TStringList);
begin
   var lines := GInfra.GenerateProgram;
   try
      ALines.AddStrings(lines);
   finally
      lines.Free;
   end;
end;

function TProject.GetSelectList(ANode: IXMLNode; const ALabel, ANodeName: string; const ANodeName2: string = ''): TStringList;
begin
   result := TStringList.Create;
   var nodes := FilterNodes(ANode, ANodeName);
   var node := nodes.NextNode;
   while node <> nil do
   begin
      if not ANodeName2.IsEmpty then
         node := FindNode(node, ANodeName2);
      if node <> nil then
         result.Add(GetNodeAttrStr(node, NAME_ATTR, ''));
      node := nodes.NextNode;
   end;
   if result.Count = 1 then
      FreeAndNil(result)
   else if result.Count > 1 then
   begin
      SelectImportForm.SetSelectList(result);
      SelectImportForm.Caption := trnsManager.GetString(ALabel);
      if IsAbortResult(SelectImportForm.ShowModal) then
         result.Clear;
   end;
end;

function TProject.ImportUserFunctionsFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
var
   header, lastHeader: TUserFunctionHeader;
   body, lastBody: TMainBlock;
   page: TTabSheet;
   selectList: TStringList;
begin
   result := errNone;
   selectList := nil;
   try
      if AImportMode <> impAll then
      begin
         selectList := GetSelectList(ANode, 'ImportFunc', FUNCTION_TAG, HEADER_TAG);
         if (selectList <> nil) and (selectList.Count = 0) then
            Exit;
      end;
      var funcNodes := FilterNodes(ANode, FUNCTION_TAG);
      var funcNode := funcNodes.NextNode;
      lastBody := nil;
      lastHeader := nil;
      while (funcNode <> nil) and (result = errNone) do
      begin
         header := nil;
         body := nil;
         var headerNode := FindNode(funcNode, HEADER_TAG);
         if headerNode <> nil then
         begin
            if (selectList <> nil) and not selectList.Contains(GetNodeAttrStr(headerNode, NAME_ATTR, '')) then
            begin
               funcNode := funcNodes.NextNode;
               continue;
            end;
            if GInfra.CurrentLang.EnabledUserFunctionHeader then
            begin
               header := TUserFunctionHeader.Create(TInfra.GetFunctionsForm);
               header.ImportFromXML(headerNode);
               header.RefreshFontColor;
            end;
         end
         else if AImportMode <> impAll then
         begin
            funcNode := funcNodes.NextNode;
            continue;
         end;
         var blockNode := FindNode(funcNode, BLOCK_TAG);
         if (blockNode <> nil) and GInfra.CurrentLang.EnabledUserFunctionBody then
         begin
            if AImportMode = impAll then
            begin
               page := GetPage(GetNodeAttrStr(blockNode, PAGE_CAPTION_ATTR, ''));
               if page = nil then
                  page := MainPage;
            end
            else
               page := ActivePage;
            var tmpBlock := TXMLProcessor.ImportFlowchartFromXML(blockNode, page, nil, PRIMARY_BRANCH_IDX, result);
            if tmpBlock is TMainBlock then
               body := TMainBlock(tmpBlock);
         end;
         if body <> nil then
         begin
            TUserFunction.Create(header, body);
            lastBody := body;
            lastHeader := header;
         end
         else
            header.Free;
         funcNode := funcNodes.NextNode;
      end;
      if lastBody <> nil then
      begin
         var box := lastBody.Page.Box;
         if AImportMode = impSelectPopup then
         begin
            var p := box.PopupMenu.PopupPoint;
            if not InvalidPoint(p) then
               TInfra.MoveWin(lastBody, box.ScreenToClient(p));
            if lastBody.Visible then
               box.SetScrollBars;
         end
         else if (AImportMode = impSelectTab) and lastBody.Visible then
            box.ScrollInView(lastBody);
      end;
      if lastHeader <> nil then
         lastHeader.SetAsActivePage;
   finally
      selectList.Free;
   end;
end;

function TProject.ImportUserDataTypesFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError;
begin
   result := errNone;
   var selectList: TStringList := nil;
   var dataType: TUserDataType := nil;
   if GInfra.CurrentLang.EnabledUserDataTypes then
   try
      if AImportMode <> impAll then
      begin
         selectList := GetSelectList(ANode, 'ImportType', DATATYPE_TAG);
         if (selectList <> nil) and (selectList.Count = 0) then
            Exit;
      end;
      var dataTypeNodes := FilterNodes(ANode, DATATYPE_TAG);
      var dataTypeNode := dataTypeNodes.NextNode;
      while dataTypeNode <> nil do
      begin
         var dataTypeName := GetNodeAttrStr(dataTypeNode, NAME_ATTR, '');
         if (selectList <> nil) and not selectList.Contains(dataTypeName) then
         begin
            dataTypeNode := dataTypeNodes.NextNode;
            continue;
         end;
         dataType := TUserDataType.Create(TInfra.GetDataTypesForm);
         dataType.ImportFromXML(dataTypeNode);
         dataType.RefreshFontColor;
         dataTypeNode := dataTypeNodes.NextNode;
      end;
      if dataType <> nil then
         dataType.SetAsActivePage;
   finally
      selectList.Free;
   end;

   if FGlobalVars <> nil then
      TInfra.PopulateDataTypeCombo(FGlobalVars.cbType);

   PopulateDataTypeSets;
   for dataType in GetUserDataTypes do
   begin
      dataType.RefreshSizeEdits;
      dataType.RefreshFontColor;
   end;

end;

function TProject.ImportCommentsFromXML(ANode: IXMLNode): integer;
begin
   result := NO_ERROR;
   var commentNodes := FilterNodes(ANode, COMMENT_TAG);
   var commentNode := commentNodes.NextNode;
   while commentNode <> nil do
   begin
      var page := GetPage(GetNodeAttrStr(commentNode, PAGE_CAPTION_ATTR, ''));
      if page = nil then
         page := MainPage;
      var comment := TComment.CreateDefault(page);
      comment.ImportFromXML(commentNode, nil);
      commentNode := commentNodes.NextNode;
   end;
end;

function TProject.GetProgramHeader: string;
begin
   result := '';
   if HeaderComment <> nil then
   begin
      result := HeaderComment.Text;
      if EndsText(HeaderComment.Lines.LineBreak, HeaderComment.Text) then
         result := result + sLineBreak;
   end;
end;

function TProject.GetIWinControlComponent(AHandle: THandle): IWinControl;
begin
   result := nil;
   for var winControl in GetIComponents<IWinControl> do
   begin
      if winControl.GetHandle = AHandle then
      begin
         result := winControl;
         break;
      end;
   end;
end;

procedure TProject.UpdateZOrder(AParent: TWinControl);
begin
   var i := 0;
   if AParent <> nil then
   begin
      var hnd := GetWindow(GetTopWindow(AParent.Handle), GW_HWNDLAST);
      while hnd <> 0 do
      begin
         var winControl := GetIWinControlComponent(hnd);
         if winControl <> nil then
         begin
            winControl.SetZOrder(i);
            i := i + 1;
         end;
         hnd := GetNextWindow(hnd, GW_HWNDPREV);
      end;
   end;
end;

procedure TProject.RefreshZOrder;
begin
   for var winControl in GetIComponents<IWinControl>(ByZOrderComponentComparer) do
      winControl.BringAllToFront;
end;

procedure TProject.ExportToGraphic(AGraphic: TGraphic);
begin
   var lBitmap: TBitmap := nil;
   if AGraphic is TBitmap then
      lBitmap := TBitmap(AGraphic)
   else
      lBitmap := TBitmap.Create;
   var page := ActivePage;
   var pnt := page.Box.GetBottomRight;
   lBitmap.Width := pnt.X;
   lBitmap.Height := pnt.Y;
   page.DrawI := False;
   page.Box.PaintToCanvas(lBitmap.Canvas);
   page.DrawI := True;
   if AGraphic <> lBitmap then
   begin
      AGraphic.Assign(lBitmap);
      lBitmap.Free;
   end;
end;

function TProject.GetMain: TMainBlock;
begin
   result := nil;
   for var func in GetUserFunctions do
   begin
      if func.IsMain and func.Active then
      begin
         result := func.Body;
         break;
      end;
   end;
end;

function TProject.FindUserFunction(ABody: TGroupBlock): TUserFunction;
begin
   result := nil;
   for var func in GetUserFunctions do
   begin
      if func.Body = ABody then
      begin
         result := func;
         break;
      end;
   end;
end;

function TProject.FindFunctionHeader(ABlock: TBlock): TUserFunctionHeader;
begin
   result := nil;
   if ABlock <> nil then
   begin
      var userFunction := FindUserFunction(ABlock.TopParentBlock);
      if userFunction <> nil then
         result := userFunction.Header;
   end;
end;

procedure TProject.PopulateDataTypeCombos;
begin
   for var func in GetUserFunctions do
   begin
      if func.Body <> nil then
         func.Body.PopulateComboBoxes;
   end;
end;

procedure TProject.ChangeDesktopColor(AColor: TColor);
begin
   TInfra.GetMainForm.UpdateTabsColor(AColor);
   for var func in GetUserFunctions do
   begin
      if func.Body <> nil then
         func.Body.ChangeColor(AColor);
   end;
   for var comment in GetComments do
      comment.Color := AColor;
end;

function TProject.CountErrWarn: TErrWarnCount;
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   for var func in GetUserFunctions do
   begin
      if func.Active then
      begin
         if func.Header <> nil then
         begin
            case func.Header.GetFocusColor of
               NOK_COLOR:  Inc(result.ErrorCount);
               WARN_COLOR: Inc(result.WarningCount);
            end;
         end;
         if func.Body <> nil then
         begin
            var errWarnCount := func.Body.CountErrWarn;
            Inc(result.ErrorCount, errWarnCount.ErrorCount);
            Inc(result.WarningCount, errWarnCount.WarningCount);
         end;
      end;
   end;
   for var dataType in GetUserDataTypes do
   begin
      if dataType.Active then
      begin
         case dataType.GetFocusColor of
            NOK_COLOR:  Inc(result.ErrorCount);
            WARN_COLOR: Inc(result.WarningCount);
         end;
      end;
   end;
end;

procedure TProject.GenerateTree(ANode: TTreeNode);
begin

   var mainFunc: TUserFunction := nil;
   var mForm: TBaseForm := nil;
   var node: TTreeNode := nil;

   if GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      mForm := TInfra.GetDataTypesForm;
      node := ANode.Owner.AddChildObject(ANode, mForm.GetTreeNodeText, mForm);
      for var dataType in GetUserDataTypes do
      begin
         if dataType.Active then
            dataType.GenerateTree(node);
      end;
   end;

   if GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts then
   begin
      mForm := TInfra.GetDeclarationsForm;
      ANode.Owner.AddChildObject(ANode, mForm.GetTreeNodeText, mForm);
   end;

   if GInfra.CurrentLang.EnabledUserFunctionHeader then
   begin
      mForm := TInfra.GetFunctionsForm;
      node := ANode.Owner.AddChildObject(ANode, mForm.GetTreeNodeText, mForm);
   end
   else
      node := ANode;

   for var func in GetUserFunctions do
   begin
      if func.Active then
      begin
         if func.IsMain and (mainFunc = nil) then
            mainFunc := func
         else
            func.GenerateTree(node);
      end;
   end;

   if mainFunc <> nil then
      mainFunc.GenerateTree(ANode);

end;

procedure TProject.UpdateHeadersBody(APage: TTabSheet);
begin
   for var func in GetUserFunctions do
   begin
      if (func.Header <> nil ) and (func.Body <> nil) and (func.Body.Page = APage) then
         func.Header.SetPageBox(APage.Caption);
   end;
end;

procedure TProject.RefreshStatements;
begin
   var chon := ChangingOn;
   ChangingOn := False;
   try
      for var func in GetUserFunctions do
      begin
         if func.Active and (func.Body <> nil) then
            func.Body.PerformRefreshStatements;
      end;
   finally
      ChangingOn := chon;
   end;
   NavigatorForm.Invalidate;
end;

procedure TProject.RefreshVarTypes;
begin
   if FGlobalVars <> nil then
      FGlobalVars.RefreshTypes;
   for var func in GetUserFunctions do
   begin
      if func.Header <> nil then
         func.Header.LocalVars.RefreshTypes;
   end;
end;

function TProject.FindMainBlockForControl(AControl: TControl): TMainBlock;
begin
   result := nil;
   for var func in GetUserFunctions do
   begin
      if func.Active and (func.Body <> nil) and (func.Body.Parent = AControl.Parent) and func.Body.BoundsRect.Contains(AControl.BoundsRect.TopLeft) then
      begin
         result := func.Body;
         break;
      end;
   end;
end;

procedure TProject.RepaintFlowcharts;
begin
   for var func in GetUserFunctions do
   begin
      if (func.Body <> nil) and func.Body.Visible then
         func.Body.RepaintAll;
   end;
end;

function TProject.FindSelectedBlock: TBlock;
begin
   result := nil;
   for var func in GetUserFunctions do
   begin
      if func.Body <> nil then
      begin
         result := func.Body.FindSelectedBlock;
         if result <> nil then
            break;
      end;
   end;
end;

function TProject.FindRedArrowBlock: TBlock;
begin
   result := nil;
   for var func in GetUserFunctions do
   begin
      if func.Body <> nil then
      begin
         result := func.Body.FindRedArrowBlock;
         if result <> nil then
            break;
      end;
   end;
end;

function TProject.GetLibraryList: TStringList;
begin
   result := TStringList.Create;
   result.CaseSensitive := GInfra.CurrentLang.CaseSensitiveSyntax;
   for var tab in GetIComponents<IWithTab>(ByPageIndexComponentComparer) do
   begin
      var lib := tab.GetLibrary;
      if (not lib.IsEmpty) and (GInfra.CurrentLang.AllowDuplicatedLibs or not result.Contains(lib)) then
         result.AddObject(lib, tab.GetTab);
   end;
end;

procedure TProject.RefreshSizeEdits;
begin
   if (GlobalVars <> nil) and (GlobalVars.edtSize.Text <> '1') then
      GlobalVars.edtSize.Change;
   for var withSizeEdits in GetIComponents<IWithSizeEdits> do
      withSizeEdits.RefreshSizeEdits;
end;

function TProject.GetUserFunction(const AName: string): TUserFunction;
begin
   result := GetComponent<TUserFunction>(AName);
end;

function TProject.GetUserDataType(const AName: string): TUserDataType;
begin
   result := GetComponent<TUserDataType>(AName);
end;

function TProject.GetComponent<T>(const AName: string): T;
begin
   result := nil;
   if not AName.Trim.IsEmpty then
   begin
      for var withName in GetIComponents<T, IWithName> do
      begin
         if TInfra.SameStrings(withName.GetName, AName) then
         begin
            result := T(withName);
            break;
         end;
      end;
   end;
end;

initialization

   ByPageIndexUserDataTypeComparer := TDelegatedComparer<TUserDataType>.Create(
      function(const L, R: TUserDataType): integer
      begin
         result := L.PageIndex - R.PageIndex;
      end
   );

   ByPageIndexUserFunctionComparer := TUserFunctionComparer.Create(PAGE_INDEX_COMPARE);
   ByPageIndexComponentComparer := TComponentComparer.Create(PAGE_INDEX_COMPARE);
   ByZOrderComponentComparer := TComponentComparer.Create(Z_ORDER_COMPARE);

end.

