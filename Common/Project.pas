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
   UserFunction, OmniXML, UserDataType, Main_Block, DeclareList, BaseIterator, CommonTypes,
   CommonInterfaces, BlockTabSheet;

type

   TBaseIteratorFriend = class(TBaseIterator)
   end;

   TProject = class(TInterfacedPersistent, IExportable)
   private
      FGlobalVars: TVarDeclareList;
      FGlobalConsts: TConstDeclareList;
      FIntegerTypesSet: TIntegerTypesSet;
      FRealTypesSet: TRealTypesSet;
      FBoolTypesSet: TBoolTypesSet;
      FOtherTypesSet: TOtherTypesSet;
      FPointerTypesSet: TPointerTypesSet;
      FStructTypesSet: TStructTypesSet;
      FEnumTypesSet: TEnumTypesSet;
      FArrayTypesSet: TArrayTypesSet;
      FStringTypesSet: TStringTypesSet;
      FComponentList: TComponentList;
      FObjectIds: TStringList;
      FObjectIdSeed: integer;
      FMainPage: TBlockTabSheet;
      FChanged: boolean;
      class var FInstance: TProject;
      procedure SetGlobals;
      function GetComponents(ASortType: integer = NO_SORT; AClass: TClass = nil): IIterator;
      function GetComponentByName(AClass: TClass; const AName: string): TComponent;
      function GetIWinControlComponent(const AHandle: THandle): IWinControl;
      procedure RefreshZOrder;
      constructor Create;
   public
      Name: string;
      LastUserFunction: TUserFunction;
      ChangingOn: boolean;
      property IntegerTypesSet: TIntegerTypesSet read FIntegerTypesSet;
      property BoolTypesSet: TBoolTypesSet read FBoolTypesSet;
      property OtherTypesSet: TOtherTypesSet read FOtherTypesSet;
      property RealTypesSet: TRealTypesSet read FRealTypesSet;
      property PointerTypesSet: TPointerTypesSet read FPointerTypesSet;
      property StructTypesSet: TStructTypesSet read FStructTypesSet;
      property EnumTypesSet: TEnumTypesSet read FEnumTypesSet;
      property ArrayTypesSet: TArrayTypesSet read FArrayTypesSet;
      property StringTypesSet: TStringTypesSet read FStringTypesSet;
      property GlobalVars: TVarDeclareList read FGlobalVars default nil;
      property GlobalConsts: TConstDeclareList read FGlobalConsts default nil;
      class function GetInstance: TProject;
      destructor Destroy; override;
      procedure AddComponent(const AComponent: TComponent);
      function GetComments: IIterator;
      function GetUserFunctions(const ASortType: integer = PAGE_INDEX_SORT): IIterator;
      function GetUserDataTypes: IIterator;
      function GetUserDataType(const ATypeName: string): TUserDataType;
      function GetUserFunction(const AFunctionName: string): TUserFunction;
      procedure ExportToGraphic(const AGraphic: TGraphic);
      procedure ExportToXMLTag(ATag: IXMLElement);
      function ExportToXMLFile(const AFile: string): TErrorType;
      function ImportFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
      function ImportUserFunctionsFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
      function ImportUserDataTypesFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
      function ImportCommentsFromXML(const ATag: IXMLElement): integer;
      procedure ImportPagesFromXML(const ATag: IXMLElement);
      function GetMainBlock: TMainBlock;
      function GetBottomRight: TPoint;
      procedure PopulateDataTypeCombos;
      procedure RefreshStatements;
      procedure ChangeDesktopColor(const AColor: TColor);
      function CountErrWarn: TErrWarnCount;
      procedure GenerateTree(const ANode: TTreeNode);
      procedure RepaintFlowcharts;
      procedure RepaintComments;
      function GetLibraryList: TStringList;
      function FindObject(const AId: integer): TObject;
      procedure RefreshSizeEdits;
      procedure PopulateDataTypes;
      procedure UpdateZOrder(const AParent: TWinControl);
      function Register(const AObject: TObject; const AId: integer = ID_INVALID): integer;
      procedure UnRegister(const AObject: TObject);
      function GetPage(const ACaption: string; const ACreate: boolean = true): TBlockTabSheet;
      function GetMainPage: TBlockTabSheet;
      function GetActivePage: TBlockTabSheet;
      procedure UpdateHeadersBody(const APage: TTabSheet);
      function GetPageOrder: string;
      function FindMainBlockForControl(const AControl: TControl): TMainBlock;
      function GetProgramHeader: string;
      function GetExportFileName: string;
      procedure SetChanged;
      function IsChanged: boolean;
      function IsNew: boolean;
      procedure SetNotChanged;
   end;

implementation

uses
   System.SysUtils, Vcl.Menus, Vcl.Forms, System.StrUtils, System.Types, System.UITypes, ApplicationCommon,
   XMLProcessor, Base_Form, LangDefinition, Navigator_Form, SortListDecorator, Base_Block,
   Comment, TabComponent, ParserHelper, SelectImport_Form;

constructor TProject.Create;
begin
   inherited Create;
   FObjectIds := TStringList.Create;
   FComponentList := TComponentList.Create;
end;

destructor TProject.Destroy;
begin
   if GSettings <> nil then
   begin
      if FGlobalVars <> nil then
      begin
         GSettings.ColumnV1Width := FGlobalVars.sgList.ColWidths[0];
         GSettings.ColumnV2Width := FGlobalVars.sgList.ColWidths[1];
         GSettings.ColumnV3Width := FGlobalVars.sgList.ColWidths[2];
         GSettings.ColumnV4Width := FGlobalVars.sgList.ColWidths[3];
         GSettings.ColumnV5Width := FGlobalVars.sgList.ColWidths[4];
      end;
      if FGlobalConsts <> nil then
      begin
         GSettings.ColumnC1Width := FGlobalConsts.sgList.ColWidths[0];
         GSettings.ColumnC2Width := FGlobalConsts.sgList.ColWidths[1];
         GSettings.ColumnC3Width := FGlobalConsts.sgList.ColWidths[2];
      end;
   end;
   while FComponentList.Count > 0 do             // automatic disposing objects that are stored in list by calling list's destructor
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
      FInstance.SetGlobals;
      FInstance.PopulateDataTypes;
   end;
   result := FInstance;
end;

function TProject.GetPage(const ACaption: string; const ACreate: boolean = true): TBlockTabSheet;
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

procedure TProject.PopulateDataTypes;
var
   userType: TUserDataType;
   i: integer;
   name: string;
   nativeType: PNativeDataType;
begin
   FIntegerTypesSet := [];
   FRealTypesSet := [];
   FBoolTypesSet := [];
   FOtherTypesSet := [];
   FPointerTypesSet := [];
   FStructTypesSet := [];
   FEnumTypesSet := [];
   FArrayTypesSet := [];
   FStringTypesSet := [];
   Include(FPointerTypesSet, GENERIC_PTR_TYPE);

   if FGlobalVars <> nil then
   begin
      for i := 0 to FGlobalVars.cbType.Items.Count-1 do
      begin
         name := FGlobalVars.cbType.Items[i];
         userType := GetUserDataType(name);
         nativeType := GInfra.GetNativeDataType(name);
         if nativeType <> nil then
         begin
            case nativeType.Kind of
               tpInt:    Include(FIntegerTypesSet, i);
               tpReal:   Include(FRealTypesSet, i);
               tpString: Include(FStringTypesSet, i);
               tpBool:   Include(FBoolTypesSet, i);
               tpPtr:    Include(FPointerTypesSet, i);
            else
               Include(FOtherTypesSet, i);
            end;
         end
         else if userType <> nil then
         begin
            case userType.rgTypeBox.ItemIndex of
               INT_TYPE:    Include(FIntegerTypesSet, i);
               REAL_TYPE:   Include(FRealTypesSet, i);
               STRUCT_TYPE: Include(FStructTypesSet, i);
               ENUM_TYPE:   Include(FEnumTypesSet, i);
               ARRAY_TYPE:  Include(FArrayTypesSet, i);
            else
               Include(FOtherTypesSet, i);
            end;
         end
         else if Assigned(GInfra.CurrentLang.IsPointerType) and GInfra.CurrentLang.IsPointerType(name) then
            Include(FPointerTypesSet, i)
         else
            Include(FOtherTypesSet, i);
      end;
   end;
end;

procedure TProject.AddComponent(const AComponent: TComponent);
begin
   FComponentList.Add(AComponent);
end;

function TProject.GetComments: IIterator;
begin
   result := GetComponents(NO_SORT, TComment);
end;

function TProject.GetUserFunctions(const ASortType: integer = PAGE_INDEX_SORT): IIterator;
begin
   result := GetComponents(ASortType, TUserFunction);
end;

function TProject.GetUserDataTypes: IIterator;
begin
   result := GetComponents(PAGE_INDEX_SORT, TUserDataType);
end;

function TProject.GetComponents(ASortType: integer = NO_SORT; AClass: TClass = nil): IIterator;
var
   i: integer;
   list: TComponentList;
   listDecor: TSortListDecorator;
begin
   list := TComponentList.Create(false);
   if list.Capacity < FComponentList.Count then
      list.Capacity := FComponentList.Count;
   for i := 0 to FComponentList.Count-1 do
   begin
       if AClass <> nil then
       begin
          if FComponentList[i].ClassType = AClass then
             list.Add(FComponentList[i]);
       end
       else
          list.Add(FComponentList[i]);
   end;
   if (ASortType <> NO_SORT) and (list.Count > 1) then
   begin
      listDecor := TSortListDecorator.Create(list, ASortType);
      listDecor.Sort;
      listDecor.Free;
   end;
   result := TBaseIteratorFriend.Create(list);
end;

function TProject.Register(const AObject: TObject; const AId: integer = ID_INVALID): integer;
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

procedure TProject.UnRegister(const AObject: TObject);
var
   idx: integer;
begin
   idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
      FObjectIds.Delete(idx);
end;

function TProject.FindObject(const AId: integer): TObject;
var
   idx: integer;
begin
   result := nil;
   idx := FObjectIds.IndexOf(AId.ToString);
   if idx <> -1 then
      result := FObjectIds.Objects[idx];
end;

function TProject.GetPageOrder: string;
var
   i: integer;
   pageControl: TPageControl;
begin
   result := '';
   pageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to pageControl.PageCount-1 do
   begin
      if i <> 0 then
         result := result + PAGE_LIST_DELIM;
      if GetMainPage = pageControl.Pages[i] then
         result := result + MAIN_PAGE_MARKER
      else
         result := result + pageControl.Pages[i].Caption;
   end;
end;

procedure TProject.SetChanged;
begin
   if ChangingOn then
   begin
      if not FChanged then
      begin
         FChanged := true;
         TInfra.GetMainForm.Caption := TInfra.GetMainForm.Caption + '*';
      end;
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
var
   mcap: string;
begin
   mcap := TInfra.GetMainForm.Caption;
   result := (PROGRAM_NAME = mcap) or (PROGRAM_NAME + '*' = mcap);
end;

function TProject.ExportToXMLFile(const AFile: string): TErrorType;
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
   itr, iter: IIterator;
   xmlObj: IXMLable;
   i: integer;
   pageControl: TPageControl;
begin

   ATag.SetAttribute(LANG_ATTR, GInfra.CurrentLang.Name);
   ATag.SetAttribute(PAGE_ORDER_ATTR, GetPageOrder);
   ATag.SetAttribute(APP_VERSION_ATTR, TInfra.GetAboutForm.GetProgramVersion);
   if GetMainPage <> GetActivePage then
      ATag.SetAttribute(PAGE_FRONT_ATTR, GetActivePage.Caption);

   if FGlobalVars <> nil then
      FGlobalVars.ExportToXMLTag(ATag);
   if FGlobalConsts <> nil then
      FGlobalConsts.ExportToXMLTag(ATag);

   pageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to pageControl.PageCount-1 do
      UpdateZOrder(pageControl.Pages[i]);

   iter := GetComponents(PAGE_INDEX_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, IXMLable, xmlObj) and xmlObj.Active then
         xmlObj.ExportToXMLTag(ATag);
   end;

   itr := TBaseFormIterator.Create;
   while itr.HasNext do
      TBaseForm(itr.Next).ExportSettingsToXMLTag(ATag);
   
end;

procedure TProject.ImportPagesFromXML(const ATag: IXMLElement);
var
   pageList, pageName, pageFront: string;
   i, len: integer;
   page, activePage: TTabSheet;
begin
   if ATag <> nil then
   begin
      activePage := nil;
      pageName := '';
      pageFront := ATag.GetAttribute(PAGE_FRONT_ATTR);
      if pageFront.IsEmpty then
         activePage := GetMainPage;
      pageList := ATag.GetAttribute(PAGE_ORDER_ATTR);
      len := pageList.Length;
      for i := 1 to len do
      begin
         page := nil;
         if pageList[i] = PAGE_LIST_DELIM then
         begin
            page := GetPage(pageName);
            pageName := '';
         end
         else
         begin
            pageName := pageName + pageList[i];
            if i = len then
               page := GetPage(pageName);
         end;
         if (page <> nil) and (activePage = nil) and SameCaption(page.Caption, pageFront) then
            activePage := page;
      end;
      if activePage <> nil then
         activePage.PageControl.ActivePage := activePage;
   end;
end;

function TProject.ImportFromXMLTag(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
var
   itr: IIterator;
   s, langName, ver: string;
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

   s := IfThen(SameText(langName, GInfra.DummyLang.Name), 'ChangeLngNone', 'ChangeLngAsk');

   if (not SameText(GInfra.CurrentLang.Name, langName)) and
      (TInfra.ShowFormattedQuestionBox(s, [langName.Trim, sLineBreak], MB_YESNO+MB_ICONQUESTION) = IDYES) then
   begin
      GInfra.SetCurrentLang(langName);
{$IFDEF USE_CODEFOLDING}
      TInfra.GetEditorForm.ReloadFoldRegions;
{$ENDIF}
      TInfra.GetEditorForm.SetFormAttributes;
      SetGlobals;
   end;

   if FGlobalConsts <> nil then
      FGlobalConsts.ImportFromXMLTag(ATag);

   ImportUserDataTypesFromXML(ATag);
   ImportPagesFromXML(ATag);

   result := ImportUserFunctionsFromXML(ATag);
   if result = errNone then
   begin
      if FGlobalVars <> nil then
         FGlobalVars.ImportFromXMLTag(ATag);
      PopulateDataTypeCombos;
      RefreshSizeEdits;
      RefreshStatements;
      ImportCommentsFromXML(ATag);
      RefreshZOrder;
      itr := TBaseFormIterator.Create;
      while itr.HasNext do
         TBaseForm(itr.Next).ImportSettingsFromXMLTag(ATag);
   end;
end;

procedure TProject.SetGlobals;
var
   l, w: integer;
begin
   w := 0;
   if GInfra.CurrentLang.EnabledVars then
   begin
      if FGlobalVars = nil then
      begin
         FGlobalVars := TVarDeclareList.Create(TInfra.GetDeclarationsForm, 2, 1, DEF_VARLIST_WIDTH, 6, 5, DEF_VARLIST_WIDTH-10);
         FGlobalVars.Caption := i18Manager.GetString('GlobalVars');
         FGlobalVars.SetCheckBoxCol(4);
      end;
   end
   else
   begin
      FGlobalVars.Free;
      FGlobalVars := nil;
   end;
   if GInfra.CurrentLang.EnabledConsts then
   begin
      if FGlobalConsts = nil then
      begin
         if FGlobalVars <> nil then
            l := FGlobalVars.BoundsRect.Right
         else
            l := 2;
         FGlobalConsts := TConstDeclareList.Create(TInfra.GetDeclarationsForm, l, 1, DEF_CONSTLIST_WIDTH, 6, 3, DEF_CONSTLIST_WIDTH-10);
         FGlobalConsts.Caption := i18Manager.GetString('GlobalConsts');
         FGlobalConsts.SetCheckBoxCol(2);
      end;
   end
   else
   begin
      FGlobalConsts.Free;
      FGlobalConsts := nil;
   end;
   if FGlobalVars <> nil then
   begin
      FGlobalVars.AssociatedList := FGlobalConsts;
      w := FGlobalVars.BoundsRect.Right + 16;
      if GSettings <> nil then
      begin
         FGlobalVars.sgList.ColWidths[0] := GSettings.ColumnV1Width;
         FGlobalVars.sgList.ColWidths[1] := GSettings.ColumnV2Width;
         FGlobalVars.sgList.ColWidths[2] := GSettings.ColumnV3Width;
         FGlobalVars.sgList.ColWidths[3] := GSettings.ColumnV4Width;
         FGlobalVars.sgList.ColWidths[4] := GSettings.ColumnV5Width;
      end;
   end;
   if FGlobalConsts <> nil then
   begin
      FGlobalConsts.AssociatedList := FGlobalVars;
      w := FGlobalConsts.BoundsRect.Right + 16;
      if GSettings <> nil then
      begin
         FGlobalConsts.sgList.ColWidths[0] := GSettings.ColumnC1Width;
         FGlobalConsts.sgList.ColWidths[1] := GSettings.ColumnC2Width;
         FGlobalConsts.sgList.ColWidths[2] := GSettings.ColumnC3Width;
      end;
   end;
   if w > 0 then
   begin
      TInfra.GetDeclarationsForm.Constraints.MaxWidth := w;
      TInfra.GetDeclarationsForm.Constraints.MinWidth := w;
   end;
   TInfra.GetMainForm.SetMenu(true);
end;

function TProject.ImportUserFunctionsFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
var
   tag, tag1: IXMLElement;
   header: TUserFunctionHeader;
   body: TMainBlock;
   tmpBlock: TBlock;
   page: TTabSheet;
   selectList: TStringList;
begin
   result := errNone;
   selectList := nil;
   try
      if ASelect then
      begin
         selectList := TStringList.Create;
         tag := TXMLProcessor.FindChildTag(ATag, FUNCTION_TAG);
         while tag <> nil do
         begin
            tag1 := TXMLProcessor.FindChildTag(tag, HEADER_TAG);
            if tag1 <> nil then
               selectList.Add(tag1.GetAttribute(NAME_ATTR));
            tag := TXMLProcessor.FindNextTag(tag);
         end;
         if selectList.Count = 0 then
            exit
         else if selectList.Count = 1 then
         begin
            selectList.Free;
            selectList := nil;
         end
         else
         begin
            SelectImportForm.SetSelectList(selectList);
            SelectImportForm.Caption := i18Manager.GetString('ImportFunc');
            if IsAbortResult(SelectImportForm.ShowModal) or (selectList.Count = 0) then
               exit;
         end;
      end;
      tag := TXMLProcessor.FindChildTag(ATag, FUNCTION_TAG);
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
               header.RefreshTab;
            end;
         end
         else if ASelect then
         begin
            tag := TXMLProcessor.FindNextTag(tag);
            continue;
         end;
         tag1 := TXMLProcessor.FindChildTag(tag, BLOCK_TAG);
         if (tag1 <> nil) and GInfra.CurrentLang.EnabledUserFunctionBody then
         begin
            page := GetPage(tag1.GetAttribute(PAGE_CAPTION_ATTR));
            if page = nil then
               page := GetMainPage;
            tmpBlock := TXMLProcessor.ImportFlowchartFromXMLTag(tag1, page, nil, result);
            if tmpBlock is TMainBlock then
               body := TMainBlock(tmpBlock);
         end;
         if result = errNone then
            TUserFunction.Create(header, body)
         else
            header.Free;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
   finally
      selectList.Free;
   end;
end;

function TProject.ImportUserDataTypesFromXML(ATag: IXMLElement; ASelect: boolean = false): TErrorType;
var
   dataType: TUserDataType;
   tag: IXMLElement;
   iter: IIterator;
   selectList: TStringList;
begin
   result := errNone;
   dataType := nil;
   selectList := nil;

   if GInfra.CurrentLang.EnabledUserDataTypes then
   try
      if ASelect then
      begin
         selectList := TStringList.Create;
         tag := TXMLProcessor.FindChildTag(ATag, DATATYPE_TAG);
         while tag <> nil do
         begin
            selectList.Add(tag.GetAttribute(NAME_ATTR));
            tag := TXMLProcessor.FindNextTag(tag);
         end;
         if selectList.Count = 0 then
            exit
         else if selectList.Count = 1 then
         begin
            selectList.Free;
            selectList := nil;
         end
         else
         begin
            SelectImportForm.SetSelectList(selectList);
            SelectImportForm.Caption := i18Manager.GetString('ImportType');
            if IsAbortResult(SelectImportForm.ShowModal) or (selectList.Count = 0) then
               exit;
         end;
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
         dataType.RefreshTab;
         tag := TXMLProcessor.FindNextTag(tag);
      end;
   finally
      selectList.Free;
   end;

   if FGlobalVars <> nil then
      TInfra.PopulateDataTypeCombo(FGlobalVars.cbType);

   if dataType <> nil then
   begin
      PopulateDataTypes;
      iter := GetUserDataTypes;
      while iter.HasNext do
      begin
         dataType := TUserDataType(iter.Next);
         dataType.RefreshSizeEdits;
         dataType.RefreshTab;
      end;
   end;

end;

function TProject.ImportCommentsFromXML(const ATag: IXMLElement): integer;
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
var
   i: integer;
   comment: TComment;
begin
   result := '';
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TComment then
      begin
         comment := TComment(FComponentList[i]);
         if comment.IsHeader then
         begin
            result := comment.Text;
            if EndsText(sLineBreak, comment.Text) then
               result := result + sLineBreak;
            break;
         end;
      end;
   end;
end;

function TProject.GetBottomRight: TPoint;
var
   pnt: TPoint;
   i: integer;
   maxBounds: IMaxBoundable;
begin
   result := TPoint.Zero;
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], IMaxBoundable, maxBounds) then
      begin
         pnt := maxBounds.GetMaxBounds;
         if pnt.X > result.X then
            result.X := pnt.X;
         if pnt.Y > result.Y then
            result.Y := pnt.Y;
      end;
   end;
end;

function TProject.GetIWinControlComponent(const AHandle: THandle): IWinControl;
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

procedure TProject.UpdateZOrder(const AParent: TWinControl);
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
   iter: IIterator;
   winControl: IWinControl;
begin
   iter := GetComponents(Z_ORDER_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, IWinControl, winControl) then
         winControl.BringAllToFront;
   end;
end;

procedure TProject.ExportToGraphic(const AGraphic: TGraphic);
var
   pnt: TPoint;
   bitmap: TBitmap;
   page: TBlockTabSheet;
   i: integer;
   winControl: TWinControl;
begin
   if AGraphic is TBitmap then
      bitmap := TBitmap(AGraphic)
   else
      bitmap := TBitmap.Create;
   pnt := GetBottomRight;
   bitmap.Width := pnt.X;
   bitmap.Height := pnt.Y;
   page := GetActivePage;
   page.DrawI := false;
   with bitmap.Canvas do
   begin
      Brush.Style := bsSolid;
      Brush.Color := page.Brush.Color;
      PatBlt(Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom, PATCOPY);
      Lock;
   end;
   try
      for i := 0 to page.ControlCount-1 do
      begin
         if page.Controls[i] is TWinControl then
         begin
            winControl := TWinControl(page.Controls[i]);
            winControl.PaintTo(bitmap.Canvas, winControl.Left, winControl.Top);       // single call to page.PaintTo will not work for
         end;                                                                         // currently invisible page child controls
      end;
   finally
      page.DrawI := true;
      bitmap.Canvas.Unlock;
   end;
   if AGraphic <> bitmap then
   begin
      AGraphic.Assign(bitmap);
      bitmap.Free;
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
   begin
      pgcPages.Pages[i].Brush.Color := AColor;
      pgcPages.Pages[i].Repaint;
   end;
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

procedure TProject.GenerateTree(const ANode: TTreeNode);
var
   it, iter: IIterator;
   dataType: TUserDataType;
   mainFunc, func: TUserFunction;
   node: TTreeNode;
begin

   mainFunc := nil;

   if GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      node := ANode.Owner.AddChildObject(ANode, i18Manager.GetString('Structures'), TInfra.GetDataTypesForm);
      it := GetUserDataTypes;
      while it.HasNext do
      begin
         dataType := TUserDataType(it.Next);
         if dataType.Active then
            dataType.GenerateTree(node);
      end;
   end;

   if GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts then
      ANode.Owner.AddChildObject(ANode, i18Manager.GetString('GlobalDeclares'), TInfra.GetDeclarationsForm);

   if GInfra.CurrentLang.EnabledUserFunctionHeader then
      node := ANode.Owner.AddChildObject(ANode, i18Manager.GetString('Functions'), TInfra.GetFunctionsForm)
   else
      node := ANode;

   iter := GetUserFunctions;
   while iter.HasNext do
   begin
      func := TUserFunction(iter.Next);
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

procedure TProject.UpdateHeadersBody(const APage: TTabSheet);
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
   tab: ITabbable;
   iter: IIterator;
begin
   result := TStringList.Create;
   result.CaseSensitive := GInfra.CurrentLang.CaseSensitiveSyntax;
   iter := GetComponents(PAGE_INDEX_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, ITabbable, tab) then
      begin
         libName := tab.GetLibName;
         if (result.IndexOf(libName) = -1) and not libName.IsEmpty then
            result.Add(libName);
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

function TProject.GetUserDataType(const ATypeName: string): TUserDataType;
begin
   result := TUserDataType(GetComponentByName(TUserDataType, ATypeName));
end;

function TProject.GetUserFunction(const AFunctionName: string): TUserFunction;
begin
   result := TUserFunction(GetComponentByName(TUserFunction, AFunctionName));
end;

function TProject.GetComponentByName(AClass: TClass; const AName: string): TComponent;
var
   i: integer;
   tab: ITabbable;
begin
   result := nil;
   if not AName.Trim.IsEmpty then
   begin
      for i := 0 to FComponentList.Count-1 do
      begin
         if (FComponentList[i].ClassType = AClass) and Supports(FComponentList[i], ITabbable, tab) then
         begin
            if TInfra.SameStrings(tab.GetName, AName) then
            begin
               result := FComponentList[i];
               break;
            end;
         end;
      end;
   end;
end;

end.

