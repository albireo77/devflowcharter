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
   BaseEnumerator, Types, Interfaces, BlockTabSheet, Comment, Declarations_Form;

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
      FObjectIdSeed,
      FLibSectionOffset: integer;
      FMainPage: TBlockTabSheet;
      FChanged: boolean;
      class var FInstance: TProject;
      constructor Create;
      procedure SetGlobalDeclarations(AForm: TDeclarationsForm);
      function GetIWinControlComponent(AHandle: THandle): IWinControl;
      procedure RefreshZOrder;
      procedure ExportPagesToXMLTag(ATag: IXMLElement);
      function GetSelectList(ATag: IXMLElement; const ALabel: string; const ATagName: string; const ATagName2: string = ''): TStringList;
   public
      Name: string;
      ChangingOn: boolean;
      HeaderComment: TComment;
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
      function GetComponent<T: class>(const AName: string): T;
      function GetComponents<T: class>(AComparer: IComparer<T> = nil): IEnumerable<T>;
      procedure ExportToGraphic(AGraphic: TGraphic);
      procedure ExportToXMLTag(ATag: IXMLElement);
      function ExportToXMLFile(const AFile: string): TError;
      function ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
      function ImportUserFunctionsFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
      function ImportUserDataTypesFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
      function ImportCommentsFromXML(ATag: IXMLElement): integer;
      procedure ImportPagesFromXML(ATag: IXMLElement);
      function GetMainBlock: TMainBlock;
      procedure PopulateDataTypeCombos;
      procedure RefreshStatements;
      procedure ChangeDesktopColor(const AColor: TColor);
      function CountErrWarn: TErrWarnCount;
      procedure GenerateTree(ANode: TTreeNode);
      procedure RepaintFlowcharts;
      procedure RepaintComments;
      function GetLibraryList: TStringList;
      function FindObject(AId: integer): TObject;
      procedure RefreshSizeEdits;
      procedure PopulateDataTypeSets;
      procedure UpdateZOrder(AParent: TWinControl);
      function Register(AObject: TObject; AId: integer = ID_INVALID): integer;
      procedure UnRegister(AObject: TObject);
      function GetPage(const ACaption: string; ACreate: boolean = true): TBlockTabSheet;
      function GetMainPage: TBlockTabSheet;
      function GetActivePage: TBlockTabSheet;
      procedure UpdateHeadersBody(APage: TTabSheet);
      function FindMainBlockForControl(const AControl: TControl): TMainBlock;
      function GetProgramHeader: string;
      function GetExportFileName: string;
      procedure SetChanged;
      function IsChanged: boolean;
      function IsNew: boolean;
      procedure SetNotChanged;
      procedure SetLibSectionOffset(ALibSectionOffset: integer);
      function GetLibSectionOffset: integer;
   end;

implementation

uses
   System.SysUtils, Vcl.Menus, Vcl.Forms, System.StrUtils, System.Types, System.UITypes,
   Generics.Collections, Infrastructure, Constants, XMLProcessor, Base_Form, LangDefinition,
   Navigator_Form, Base_Block, TabComponent, ParserHelper, SelectImport_Form,
   WinApi.Messages, Vcl.ExtCtrls;

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
   FLibSectionOffset := -1;
   TInfra.GetMainForm.SetProjectMenu(true);
end;

destructor TProject.Destroy;
begin
   while FComponentList.Count > 0 do             // automatic disposing objects that are stored in list by calling list's destructor
      FComponentList[0].Free;                    // will generate EListError exception for pinned comments
   FComponentList.Free;                          // so to destroy FComponentList, objects must be freed in while loop first
   FGlobalVars.Free;
   FGlobalConsts.Free;
   FObjectIds.Free;
   FInstance := nil;
   inherited Destroy;
end;

procedure TProject.SetLibSectionOffset(ALibSectionOffset: integer);
begin
   FLibSectionOffset := ALibSectionOffset;
end;

function TProject.GetLibSectionOffset: integer;
begin
   result := FLibSectionOffset;
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

function TProject.GetPage(const ACaption: string; ACreate: boolean = true): TBlockTabSheet;
var
   i: integer;
   caption: string;
   pageControl: TPageControl;
begin
   result := nil;
   caption := ACaption.Trim;
   if not caption.IsEmpty then
   begin
      pageControl := TInfra.GetMainForm.pgcPages;
      for i := 0 to pageControl.PageCount-1 do
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
            result := GetMainPage
         else if ACreate then
         begin
            result := TBlockTabSheet.Create(TInfra.GetMainForm);
            result.Caption := caption;
         end;
      end;
   end;
end;

function TProject.GetMainPage: TBlockTabSheet;
begin
   if FMainPage = nil then
      FMainPage := GetPage(i18Manager.GetString(DEF_PAGE_CAPTION_KEY));
   result := FMainPage;
end;

function TProject.GetActivePage: TBlockTabSheet;
begin
   result := TBlockTabSheet(TInfra.GetMainForm.pgcPages.ActivePage);
end;

function TProject.GetExportFileName: string;
begin
   result := Name;
end;

procedure TProject.PopulateDataTypeSets;
var
   userType: TUserDataType;
   i: integer;
   name: string;
   nativeType: PNativeDataType;
   typesSet: PTypesSet;
begin
   FIntegerTypesSet := [];
   FRealTypesSet := [];
   FBoolTypesSet := [];
   FOtherTypesSet := [];
   FPointerTypesSet := [];
   FRecordTypesSet := [];
   FEnumTypesSet := [];
   FArrayTypesSet := [];
   FStringTypesSet := [];
   Include(FPointerTypesSet, GENERIC_PTR_TYPE);

   if FGlobalVars <> nil then
   begin
      for i := 0 to FGlobalVars.cbType.Items.Count-1 do
      begin
         name := FGlobalVars.cbType.Items[i];
         userType := GetComponent<TUserDataType>(name);
         nativeType := GInfra.GetNativeDataType(name);
         if nativeType <> nil then
         begin
            case nativeType.Kind of
               tpInt:    typesSet := @FIntegerTypesSet;
               tpReal:   typesSet := @FRealTypesSet;
               tpString: typesSet := @FStringTypesSet;
               tpBool:   typesSet := @FBoolTypesSet;
               tpPtr:    typesSet := @FPointerTypesSet;
            else
               typesSet := @FOtherTypesSet;
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
            else
               typesSet := @FOtherTypesSet;
            end;
         end
         else if Assigned(GInfra.CurrentLang.IsPointerType) and GInfra.CurrentLang.IsPointerType(name) then
            typesSet := @FPointerTypesSet
         else
            typesSet := @FOtherTypesSet;
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

function TProject.GetComponents<T>(AComparer: IComparer<T> = nil): IEnumerable<T>;
var
   i: integer;
   list: TList<T>;
begin
   list := TList<T>.Create;
   if list.Capacity < FComponentList.Count then
      list.Capacity := FComponentList.Count;
   for i := 0 to FComponentList.Count-1 do
   begin
       if T <> TComponent then
       begin
          if FComponentList[i].ClassType = T then
             list.Add(FComponentList[i]);
       end
       else
          list.Add(FComponentList[i]);
   end;
   if AComparer <> nil then
      list.Sort(AComparer);
   result := TEnumeratorFactory<T>.Create(list);
end;

function TProject.Register(AObject: TObject; AId: integer = ID_INVALID): integer;
var
   idx: integer;
   id: string;
   accepted: boolean;
begin
   id := AId.ToString;
   accepted := (AId <> ID_INVALID) and (FObjectIds.IndexOf(id) = -1);
   idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
   begin
      result := FObjectIds[idx].ToInteger;
      if accepted then
         FObjectIds[idx] := id;
   end
   else
   begin
      if accepted then
         FObjectIds.AddObject(id, AObject)
      else
      begin
         FObjectIds.AddObject(FObjectIdSeed.ToString, AObject);
         result := FObjectIdSeed;
         FObjectIdSeed := FObjectIdSeed + 1;
      end;
   end;
   if accepted then
   begin
      if FObjectIdSeed <= AId then
         FObjectIdSeed := AId + 1;
      result := AId;
   end;
end;

procedure TProject.UnRegister(AObject: TObject);
var
   idx: integer;
begin
   idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
      FObjectIds.Delete(idx);
end;

function TProject.FindObject(AId: integer): TObject;
var
   idx: integer;
begin
   result := nil;
   idx := FObjectIds.IndexOf(AId.ToString);
   if idx <> -1 then
      result := FObjectIds.Objects[idx];
end;

procedure TProject.ExportPagesToXMLTag(ATag: IXMLElement);
var
   i: integer;
   pageControl: TPageControl;
   tag: IXMLElement;
begin
   tag := ATag.OwnerDocument.CreateElement('pages');
   ATag.AppendChild(tag);
   pageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to pageControl.PageCount-1 do
      TBlockTabSheet(pageControl.Pages[i]).ExportToXMLTag(tag);
end;

procedure TProject.SetChanged;
begin
   if ChangingOn and not FChanged then
   begin
      FChanged := true;
      TInfra.GetMainForm.SetChanged;
   end;
end;

procedure TProject.SetNotChanged;
begin
   if ChangingOn then
      FChanged := false;
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
   ChangingOn := false;
   result := TXMLProcessor.ExportToXMLFile(ExportToXMLTag, AFile);
   ChangingOn := true;
   if result = errNone then
   begin
      FChanged := false;
      TInfra.GetMainForm.AcceptFile(AFile);
   end;
end;

procedure TProject.ExportToXMLTag(ATag: IXMLElement);
var
   comp: TComponent;
   components: IEnumerable<TComponent>;
   xmlObj: IXMLable;
   i: integer;
   pageControl: TPageControl;
   baseForm: TBaseForm;
begin

   ATag.SetAttribute(LANG_ATTR, GInfra.CurrentLang.Name);
   ATag.SetAttribute(APP_VERSION_ATTR, TInfra.GetAboutForm.GetProgramVersion);

   ExportPagesToXMLTag(ATag);

   if GetMainPage <> GetActivePage then
      ATag.SetAttribute(PAGE_FRONT_ATTR, GetActivePage.Caption);

   if FGlobalVars <> nil then
      FGlobalVars.ExportToXMLTag(ATag);
   if FGlobalConsts <> nil then
      FGlobalConsts.ExportToXMLTag(ATag);

   pageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to pageControl.PageCount-1 do
      UpdateZOrder(TBlockTabSheet(pageControl.Pages[i]).Box);

   components := GetComponents<TComponent>(ByPageIndexComponentComparer);
   for comp in components do
   begin
      if Supports(comp, IXMLable, xmlObj) and xmlObj.Active then
         xmlObj.ExportToXMLTag(ATag);
   end;

   for baseForm in TInfra.GetBaseForms do
      baseForm.ExportSettingsToXMLTag(ATag);
end;

procedure TProject.ImportPagesFromXML(ATag: IXMLElement);
var
   pageFront: string;
   page, activePage: TBlockTabSheet;
   tag: IXMLElement;
begin
   if ATag <> nil then
   begin
      activePage := nil;
      pageFront := ATag.GetAttribute(PAGE_FRONT_ATTR);
      tag := TXMLProcessor.FindChildTag(ATag, 'pages');
      tag := TXMLProcessor.FindChildTag(tag, 'page');
      while tag <> nil do
      begin
         page := GetPage(tag.GetAttribute('name'));
         if page <> nil then
         begin
            page.ImportFromXMLTag(tag);
            if (activePage = nil) and SameCaption(page.Caption, pageFront) then
               activePage := page;
         end;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      if activePage = nil then
         activePage := GetMainPage;
      activePage.PageControl.ActivePage := activePage;
   end;
end;

function TProject.ImportFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   s, langName, ver: string;
   baseForm: TBaseForm;
begin

   result := errValidate;

   langName := ATag.GetAttribute(LANG_ATTR);
   if GInfra.GetLangDefinition(langName) = nil then
   begin
      Gerr_text := i18Manager.GetFormattedString('LngNoSprt', [langName]);
      exit;
   end;

   ver := ATag.GetAttribute(APP_VERSION_ATTR);
   if TInfra.CompareProgramVersion(ver) > 0 then
      TInfra.ShowFormattedWarningBox('OldVerMsg', [ver]);

   s := IfThen(SameText(langName, GInfra.TemplateLang.Name), 'ChangeLngNone', 'ChangeLngAsk');

   if (not SameText(GInfra.CurrentLang.Name, langName)) and
      (TInfra.ShowFormattedQuestionBox(s, [langName.Trim, sLineBreak], MB_YESNO+MB_ICONQUESTION) = mrYes) then
   begin
      GSettings.CurrentLangName := langName;
{$IFDEF USE_CODEFOLDING}
      TInfra.GetEditorForm.ReloadFoldRegions;
{$ENDIF}
      TInfra.GetEditorForm.SetFormAttributes;
      SetGlobalDeclarations(TInfra.GetDeclarationsForm);
      TInfra.GetMainForm.SetProjectMenu(true);
   end;

   if FGlobalConsts <> nil then
      FGlobalConsts.ImportFromXMLTag(ATag, impAll);

   ImportUserDataTypesFromXML(ATag, impAll);
   ImportPagesFromXML(ATag);

   result := ImportUserFunctionsFromXML(ATag, impAll);
   if result = errNone then
   begin
      if FGlobalVars <> nil then
         FGlobalVars.ImportFromXMLTag(ATag, impAll);
      PopulateDataTypeCombos;
      RefreshSizeEdits;
      RefreshStatements;
      ImportCommentsFromXML(ATag);
      RefreshZOrder;
      for baseForm in TInfra.GetBaseForms do
         baseForm.ImportSettingsFromXMLTag(ATag);
   end;
end;

procedure TProject.SetGlobalDeclarations(AForm: TDeclarationsForm);
var
   x: integer;
   splitter: TSplitter;
begin
   FGlobalVars.Free;
   FGlobalVars := nil;
   FGlobalConsts.Free;
   FGlobalConsts := nil;
   if GInfra.CurrentLang.EnabledVars then
      FGlobalVars := TVarDeclareList.Create(AForm, 2, 1, DEF_VARLIST_WIDTH, 6, 5, DEF_VARLIST_WIDTH-10);
   if GInfra.CurrentLang.EnabledConsts then
   begin
      splitter := nil;
      if FGlobalVars <> nil then
      begin
         FGlobalVars.Align := alLeft;
         x := FGlobalVars.BoundsRect.Right + 5;
         splitter := TSplitter.Create(AForm);
         splitter.Parent := AForm;
         splitter.Left := x - 4;
         splitter.Align := FGlobalVars.Align;
         FGlobalVars.SetSplitter(splitter);
      end
      else
         x := 2;
      FGlobalConsts := TConstDeclareList.Create(AForm, x, 1, DEF_CONSTLIST_WIDTH-5, 6, 3, DEF_CONSTLIST_WIDTH-15);
      if splitter <> nil then
         FGlobalConsts.Align := alClient;
   end;
   if FGlobalVars <> nil then
   begin
      FGlobalVars.Caption := i18Manager.GetString(GInfra.CurrentLang.GlobalVarsLabelKey);
      FGlobalVars.SetExternalColumn(4);
      FGlobalVars.AssociatedList := FGlobalConsts;
      AForm.Width := FGlobalVars.BoundsRect.Right + DECLARATIONS_FORM_RIGHT_MARGIN;
      if FGlobalConsts = nil then
         FGlobalVars.Align := alClient;
   end;
   if FGlobalConsts <> nil then
   begin
      FGlobalConsts.Caption := i18Manager.GetString(GInfra.CurrentLang.GlobalConstsLabelKey);
      FGlobalConsts.SetExternalColumn(2);
      FGlobalConsts.AssociatedList := FGlobalVars;
      AForm.Width := FGlobalConsts.BoundsRect.Right + DECLARATIONS_FORM_RIGHT_MARGIN;
   end;
end;

function TProject.GetSelectList(ATag: IXMLElement; const ALabel: string; const ATagName: string; const ATagName2: string = ''): TStringList;
var
   tag, tag1: IXMLElement;
   isTag2Empty: boolean;
begin
   isTag2Empty := ATagName2.IsEmpty;
   result := TStringList.Create;
   tag := TXMLProcessor.FindChildTag(ATag, ATagName);
   while tag <> nil do
   begin
      if not isTag2Empty then
         tag1 := TXMLProcessor.FindChildTag(tag, ATagName2)
      else
         tag1 := tag;
      if tag1 <> nil then
         result.Add(tag1.GetAttribute(NAME_ATTR));
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   if result.Count = 1 then
      FreeAndNil(result)
   else if result.Count > 1 then
   begin
      SelectImportForm.SetSelectList(result);
      SelectImportForm.Caption := i18Manager.GetString(ALabel);
      if IsAbortResult(SelectImportForm.ShowModal) then
         result.Clear;
   end;
end;

function TProject.ImportUserFunctionsFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   tag, tag1: IXMLElement;
   header, lastHeader: TUserFunctionHeader;
   body, lastBody: TMainBlock;
   tmpBlock: TBlock;
   page: TTabSheet;
   selectList: TStringList;
   box: TScrollBoxEx;
   p: TPoint;
begin
   result := errNone;
   selectList := nil;
   try
      if AImportMode <> impAll then
      begin
         selectList := GetSelectList(ATag, 'ImportFunc', FUNCTION_TAG, HEADER_TAG);
         if (selectList <> nil) and (selectList.Count = 0) then
            Exit;
      end;
      tag := TXMLProcessor.FindChildTag(ATag, FUNCTION_TAG);
      lastBody := nil;
      lastHeader := nil;
      while (tag <> nil) and (result = errNone) do
      begin
         header := nil;
         body := nil;
         tag1 := TXMLProcessor.FindChildTag(tag, HEADER_TAG);
         if tag1 <> nil then
         begin
            if (selectList <> nil) and (selectList.IndexOf(tag1.GetAttribute(NAME_ATTR)) = -1) then
            begin
               tag := TXMLProcessor.FindNextTag(tag);
               continue;
            end;
            if GInfra.CurrentLang.EnabledUserFunctionHeader then
            begin
               header := TUserFunctionHeader.Create(TInfra.GetFunctionsForm);
               header.ImportFromXMLTag(tag1);
               header.RefreshFontColor;
            end;
         end
         else if AImportMode <> impAll then
         begin
            tag := TXMLProcessor.FindNextTag(tag);
            continue;
         end;
         tag1 := TXMLProcessor.FindChildTag(tag, BLOCK_TAG);
         if (tag1 <> nil) and GInfra.CurrentLang.EnabledUserFunctionBody then
         begin
            if AImportMode = impAll then
            begin
               page := GetPage(tag1.GetAttribute(PAGE_CAPTION_ATTR));
               if page = nil then
                  page := GetMainPage;
            end
            else
               page := GetActivePage;
            tmpBlock := TXMLProcessor.ImportFlowchartFromXMLTag(tag1, page, nil, result);
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
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      if lastBody <> nil then
      begin
         box := lastBody.Page.Box;
         if AImportMode = impSelectPopup then
         begin
            p := box.PopupMenu.PopupPoint;
            if not InvalidPoint(p) then
               TInfra.MoveWin(lastBody, box.ScreenToClient(p));
            if lastBody.Visible then
               box.SetScrollBars;
         end
         else if (AImportMode = impSelectTab) and lastBody.Visible then
            box.ScrollInView(lastBody);
      end;
      if lastHeader <> nil then
         lastHeader.PageControl.ActivePage := lastHeader;
   finally
      selectList.Free;
   end;
end;

function TProject.ImportUserDataTypesFromXML(ATag: IXMLElement; AImportMode: TImportMode): TError;
var
   dataType: TUserDataType;
   tag: IXMLElement;
   selectList: TStringList;
begin
   result := errNone;
   selectList := nil;
   dataType := nil;
   if GInfra.CurrentLang.EnabledUserDataTypes then
   try
      if AImportMode <> impAll then
      begin
         selectList := GetSelectList(ATag, 'ImportType', DATATYPE_TAG);
         if (selectList <> nil) and (selectList.Count = 0) then
            Exit;
      end;
      tag := TXMLProcessor.FindChildTag(ATag, DATATYPE_TAG);
      while tag <> nil do
      begin
         if (selectList <> nil) and (selectList.IndexOf(tag.GetAttribute(NAME_ATTR)) = -1) then
         begin
            tag := TXMLProcessor.FindNextTag(tag);
            continue;
         end;
         dataType := TUserDataType.Create(TInfra.GetDataTypesForm);
         dataType.ImportFromXMLTag(tag);
         dataType.RefreshFontColor;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
      if dataType <> nil then
         dataType.PageControl.ActivePage := dataType;
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

function TProject.ImportCommentsFromXML(ATag: IXMLElement): integer;
var
   comment: TComment;
   tag: IXMLElement;
   page: TBlockTabSheet;
begin
   result := NO_ERROR;
   tag := TXMLProcessor.FindChildTag(ATag, COMMENT_ATTR);
   while tag <> nil do
   begin
      page := GetPage(tag.GetAttribute(PAGE_CAPTION_ATTR));
      if page = nil then
         page := GetMainPage;
      comment := TComment.CreateDefault(page);
      comment.ImportFromXMLTag(tag, nil);
      tag := TXMLProcessor.FindNextTag(tag);
   end;
end;

function TProject.GetProgramHeader: string;
begin
   result := '';
   if HeaderComment <> nil then
   begin
      result := HeaderComment.Text;
      if EndsText(sLineBreak, HeaderComment.Text) then
         result := result + sLineBreak;
   end;
end;

function TProject.GetIWinControlComponent(AHandle: THandle): IWinControl;
var
   i: integer;
   winControl: IWinControl;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], IWinControl, winControl) and (winControl.GetHandle = AHandle) then
      begin
         result := winControl;
         break;
      end;
   end;
end;

procedure TProject.UpdateZOrder(AParent: TWinControl);
var
   winControl: IWinControl;
   hnd: THandle;
   i: integer;
begin
   i := 0;
   if AParent <> nil then
   begin
      hnd := GetWindow(GetTopWindow(AParent.Handle), GW_HWNDLAST);
      while hnd <> 0 do
      begin
         winControl := GetIWinControlComponent(hnd);
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
var
   comp: TComponent;
   winControl: IWinControl;
   components: IEnumerable<TComponent>;
begin
   components := GetComponents<TComponent>(ByZOrderComponentComparer);
   for comp in components do
   begin
      if Supports(comp, IWinControl, winControl) then
         winControl.BringAllToFront;
   end;
end;

procedure TProject.ExportToGraphic(AGraphic: TGraphic);
var
   lBitmap: TBitmap;
   page: TBlockTabSheet;
   pnt: TPoint;
begin
   if AGraphic is TBitmap then
      lBitmap := TBitmap(AGraphic)
   else
      lBitmap := TBitmap.Create;
   page := GetActivePage;
   pnt := page.Box.GetBottomRight;
   lBitmap.Width := pnt.X;
   lBitmap.Height := pnt.Y;
   page.DrawI := false;
   page.Box.PaintToCanvas(lBitmap.Canvas);
   page.DrawI := true;
   if AGraphic <> lBitmap then
   begin
      AGraphic.Assign(lBitmap);
      lBitmap.Free;
   end;
end;

function TProject.GetMainBlock: TMainBlock;
var
   i: integer;
   func: TUserFunction;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
         if (func.Header = nil) and func.Active then
         begin
            result := func.Body;
            break;
         end;
      end;
   end;
end;

procedure TProject.PopulateDataTypeCombos;
var
   i: integer;
   func: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
         if func.Body <> nil then
            func.Body.PopulateComboBoxes;
      end;
   end;
end;

procedure TProject.ChangeDesktopColor(const AColor: TColor);
var
   i: integer;
   comp: TComponent;
   pgcPages: TPageControl;
begin
   pgcPages := TInfra.GetMainForm.pgcPages;
   for i := 0 to pgcPages.PageCount-1 do
      TBlockTabSheet(pgcPages.Pages[i]).Box.Color := AColor;
   for i := 0 to FComponentList.Count-1 do
   begin
      comp := FComponentList[i];
      if comp is TUserFunction then
      begin
         if TUserFunction(comp).Body <> nil then
            TUserFunction(comp).Body.ChangeColor(AColor);
      end
      else if comp is TComment then
         TComment(comp).Color := AColor;
   end;
end;

function TProject.CountErrWarn: TErrWarnCount;
var
   i: integer;
   func: TUserFunction;
   dataType: TUserDataType;
   errWarnCount: TErrWarnCount;
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
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
               errWarnCount := func.Body.CountErrWarn;
               Inc(result.ErrorCount, errWarnCount.ErrorCount);
               Inc(result.WarningCount, errWarnCount.WarningCount);
            end;
         end;
      end
      else if FComponentList[i] is TUserDataType then
      begin
         dataType := TUserDataType(FComponentList[i]);
         if dataType.Active then
         begin
            case dataType.GetFocusColor of
               NOK_COLOR:  Inc(result.ErrorCount);
               WARN_COLOR: Inc(result.WarningCount);
            end;
         end;
      end;
   end;
end;

procedure TProject.GenerateTree(ANode: TTreeNode);
var
   dataType: TUserDataType;
   mainFunc, func: TUserFunction;
   node: TTreeNode;
   mForm: TBaseForm;
begin

   mainFunc := nil;

   if GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      mForm := TInfra.GetDataTypesForm;
      node := ANode.Owner.AddChildObject(ANode, mForm.GetTreeNodeText, mForm);
      for dataType in GetUserDataTypes do
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

   for func in GetUserFunctions do
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
var
   i: integer;
   func: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
         if (func.Header <> nil ) and (func.Body <> nil) and (func.Body.Page = APage) then
            func.Header.SetPageCombo(APage.Caption);
      end;
   end;
end;

procedure TProject.RefreshStatements;
var
   i: integer;
   func: TUserFunction;
   chon: boolean;
begin
   chon := ChangingOn;
   ChangingOn := false;
   try
      for i := 0 to FComponentList.Count-1 do
      begin
         if FComponentList[i] is TUserFunction then
         begin
            func := TUserFunction(FComponentList[i]);
            if func.Active and (func.Body <> nil) then
               func.Body.RefreshStatements;
         end;
      end;
   finally
      ChangingOn := chon;
   end;
   NavigatorForm.Invalidate;
end;

function TProject.FindMainBlockForControl(const AControl: TControl): TMainBlock;
var
   i: integer;
   func: TUserFunction;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
         if func.Active and (func.Body <> nil) and (func.Body.Parent = AControl.Parent) and func.Body.BoundsRect.Contains(AControl.BoundsRect.TopLeft) then
         begin
            result := func.Body;
            break;
         end;
      end;
   end;
end;

procedure TProject.RepaintComments;
var
   i: integer;
   comment: TComment;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TComment then
      begin
         comment := TComment(FComponentList[i]);
         if comment.Visible then
            comment.Repaint;
      end;
   end;
end;

procedure TProject.RepaintFlowcharts;
var
   i: integer;
   func: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         func := TUserFunction(FComponentList[i]);
         if (func.Body <> nil) and func.Body.Visible then
            func.Body.RepaintAll;
      end;
   end;
end;

function TProject.GetLibraryList: TStringList;
var
   libName: string;
   tab: ITabable;
   comp: TComponent;
   components: IEnumerable<TComponent>;
begin
   result := TStringList.Create;
   result.CaseSensitive := GInfra.CurrentLang.CaseSensitiveSyntax;
   components := GetComponents<TComponent>(ByPageIndexComponentComparer);
   for comp in components do
   begin
      if Supports(comp, ITabable, tab) then
      begin
         libName := tab.GetLibName;
         if (not libName.IsEmpty) and (GInfra.CurrentLang.AllowDuplicatedLibs or (result.IndexOf(libName) = -1)) then
            result.AddObject(libName, tab.GetTab);
      end;
   end;
end;

procedure TProject.RefreshSizeEdits;
var
   i: integer;
   sizeEdit: ISizeEditable;
begin
   if (GlobalVars <> nil) and (GlobalVars.edtSize.Text <> '1') then
      GlobalVars.edtSize.OnChange(GlobalVars.edtSize);
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], ISizeEditable, sizeEdit) then
         sizeEdit.RefreshSizeEdits;
   end;
end;

function TProject.GetComponent<T>(const AName: string): T;
var
   i: integer;
   named: INameable;
   comp: TComponent;
begin
   result := nil;
   if not AName.Trim.IsEmpty then
   begin
      for i := 0 to FComponentList.Count-1 do
      begin
         comp := FComponentList[i];
         if (comp.ClassType = T) and Supports(comp, INameable, named) then
         begin
            if TInfra.SameStrings(named.GetName, AName) then
            begin
               result := T(comp);
               break;
            end;
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

