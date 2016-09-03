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
   Windows, Graphics, UserFunction, OmniXML, UserDataType, Classes, Main_Block,
   ComCtrls, DeclareList, Controls, Forms, Contnrs, BaseIterator, CommonTypes,
   StdCtrls, CommonInterfaces, BlockTabSheet;

type

   TBaseIteratorFriend = class(TBaseIterator)
   end;

   TProject = class(TObject)
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
      procedure SetGlobals;
      function GetComponents(const ASortType: integer = NO_SORT; const AClassName: string = ''): IIterator;
      function GetComponentByName(const AClassName: string; const AName: string): TComponent;
      function GetIWinControlComponent(const AHandle: THandle): IWinControl;
      procedure RefreshZOrder;
      constructor Create;
   public
      Name: string;
      LastUserFunction: TUserFunction;
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
      procedure SetCommentsColor(const AColor: TColor);
      function GetComments: IIterator;
      function GetUserFunctions(const ASortType: integer = PAGE_INDEX_SORT): IIterator;
      function GetUserDataTypes: IIterator;
      function GetUserDataType(const ATypeName: string): TUserDataType;
      function GetUserFunction(const AFunctionName: string): TUserFunction;
      procedure ExportToGraphic(const AImage: TGraphic);
      procedure ExportToXMLTag(const ATag: IXMLElement);
      function ExportToXMLFile(const AFile: string): TErrorType;
      procedure PaintToCanvas(const ACanvas: TCanvas);
      function ImportFromXMLTag(const ATag: IXMLElement): TErrorType;
      function ImportUserFunctionsFromXML(const ATag: IXMLElement): TErrorType;
      function ImportUserDataTypesFromXML(const ATag: IXMLElement): TErrorType;
      function ImportCommentsFromXML(const ATag: IXMLElement): integer;
      procedure ImportPagesFromXML(const ATag: IXMLElement);
      function GetMainBlock: TMainBlock;
      function GetBottomRight: TPoint;
      procedure PopulateDataTypeCombos;
      procedure RefreshStatements;
      procedure ChangeFlowchartsColor(const AColor: TColor);
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
   end;

implementation

uses
   SysUtils, ApplicationCommon, XMLProcessor, Base_Form, LangDefinition, Messages, Navigator_Form,
   SortListDecorator, Base_Block, Comment, TabComponent, ParserHelper, Menus, StrUtils;

var
   Instance: TProject;

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
   Instance := nil;
   inherited Destroy;
end;

class function TProject.GetInstance: TProject;
begin
   if Instance = nil then
   begin
      Instance := TProject.Create;
      Instance.SetGlobals;
      Instance.PopulateDataTypes;
   end;
   result := Instance;
end;

function TProject.GetPage(const ACaption: string; const ACreate: boolean = true): TBlockTabSheet;
var
   i: integer;
   lCaption: string;
   lPageControl: TPageControl;
begin
   result := nil;
   lCaption := Trim(ACaption);
   if lCaption <> '' then
   begin
      lPageControl := TInfra.GetMainForm.pgcPages;
      for i := 0 to lPageControl.PageCount-1 do
      begin
         if AnsiSameCaption(lPageControl.Pages[i].Caption, lCaption) then
         begin
            result := TBlockTabSheet(lPageControl.Pages[i]);
            break;
         end;
      end;
      if result = nil then
      begin
         if AnsiSameCaption(lCaption, MAIN_PAGE_MARKER) then
            result := GetMainPage
         else if ACreate then
         begin
            result := TBlockTabSheet.Create(TInfra.GetMainForm);
            result.Caption := lCaption;
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

procedure TProject.PopulateDataTypes;
var
   lUserDataType: TUserDataType;
   i: integer;
   lName: string;
   lNativeDataType: PNativeDataType;
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
         lName := FGlobalVars.cbType.Items[i];
         lUserDataType := GetUserDataType(lName);
         lNativeDataType := GInfra.GetNativeDataType(lName);
         if lNativeDataType <> nil then
         begin
            case lNativeDataType.Kind of
               tpInt:    Include(FIntegerTypesSet, i);
               tpReal:   Include(FRealTypesSet, i);
               tpString: Include(FStringTypesSet, i);
               tpBool:   Include(FBoolTypesSet, i);
               tpPtr:    Include(FPointerTypesSet, i);
            else
               Include(FOtherTypesSet, i);
            end;
         end
         else if lUserDataType <> nil then
         begin
            if lUserDataType.rbInt.Checked then
               Include(FIntegerTypesSet, i)
            else if lUserDataType.rbReal.Checked then
               Include(FRealTypesSet, i)
            else if lUserDataType.rbStruct.Checked then
               Include(FStructTypesSet, i)
            else if lUserDataType.rbEnum.Checked then
               Include(FEnumTypesSet, i)
            else if lUserDataType.rbArray.Checked then
               Include(FArrayTypesSet, i)
            else
               Include(FOtherTypesSet, i);
         end
         else if Assigned(GInfra.CurrentLang.IsPointerType) and GInfra.CurrentLang.IsPointerType(lName) then
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
   result := GetComponents(NO_SORT, TComment.ClassName);
end;

function TProject.GetUserFunctions(const ASortType: integer = PAGE_INDEX_SORT): IIterator;
begin
   result := GetComponents(ASortType, TUserFunction.ClassName);
end;

function TProject.GetUserDataTypes: IIterator;
begin
   result := GetComponents(PAGE_INDEX_SORT, TUserDataType.ClassName);
end;

function TProject.GetComponents(const ASortType: integer = NO_SORT; const AClassName: string = ''): IIterator;
var
   i: integer;
   lList: TComponentList;
   lListSortWrap: TSortListDecorator;
begin
   lList := TComponentList.Create(false);
   if lList.Capacity < FComponentList.Count then
      lList.Capacity := FComponentList.Count;
   for i := 0 to FComponentList.Count-1 do
   begin
       if AClassName <> '' then
       begin
          if FComponentList[i].ClassNameIs(AClassName) then
             lList.Add(FComponentList[i]);
       end
       else
          lList.Add(FComponentList[i]);
   end;
   if (ASortType <> NO_SORT) and (lList.Count > 1) then
   begin
      lListSortWrap := TSortListDecorator.Create(lList, ASortType);
      lListSortWrap.Sort;
      lListSortWrap.Free;
   end;
   result := TBaseIteratorFriend.Create(lList);
end;

function TProject.Register(const AObject: TObject; const AId: integer = ID_INVALID): integer;
var
   idx: integer;
   lStrId: string;
   lIdAccepted: boolean;
begin
   lStrId := IntToStr(AId);
   lIdAccepted := (AId <> ID_INVALID) and (FObjectIds.IndexOf(lStrId) = -1);
   idx := FObjectIds.IndexOfObject(AObject);
   if idx <> -1 then
   begin
      result := StrToInt(FObjectIds.Strings[idx]);
      if lIdAccepted then
         FObjectIds.Strings[idx] := lStrId;
   end
   else
   begin
      if lIdAccepted then
         FObjectIds.AddObject(lStrId, AObject)
      else
      begin
         FObjectIds.AddObject(IntToStr(FObjectIdSeed), AObject);
         result := FObjectIdSeed;
         FObjectIdSeed := FObjectIdSeed + 1;
      end;
   end;
   if lIdAccepted then
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
   idx := FObjectIds.IndexOf(IntToStr(AId));
   if idx <> -1 then
      result := FObjectIds.Objects[idx];
end;

function TProject.GetPageOrder: string;
var
   i: integer;
   lPageControl: TPageControl;
begin
   result := '';
   lPageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to lPageControl.PageCount-1 do
   begin
      if i <> 0 then
         result := result + PAGE_LIST_DELIM;
      if GetMainPage = lPageControl.Pages[i] then
         result := result + MAIN_PAGE_MARKER
      else
         result := result + lPageControl.Pages[i].Caption;
   end;
end;

function TProject.ExportToXMLFile(const AFile: string): TErrorType;
begin
   result := TXMLProcessor.ExportToXMLFile(ExportToXMLTag, AFile);
end;

procedure TProject.ExportToXMLTag(const ATag: IXMLElement);
var
   itr, iter: IIterator;
   lXmlObj: IXMLable;
   i: integer;
   lPageControl: TPageControl;
begin

   ATag.SetAttribute(LANG_ATTR, GInfra.CurrentLang.Name);
   ATag.SetAttribute(PAGE_ORDER_ATTR, GetPageOrder);
   if GetMainPage <> GetActivePage then
      ATag.SetAttribute(PAGE_FRONT_ATTR, GetActivePage.Caption);

   if FGlobalVars <> nil then
      FGlobalVars.ExportToXMLTag(ATag);
   if FGlobalConsts <> nil then
      FGlobalConsts.ExportToXMLTag(ATag);

   lPageControl := TInfra.GetMainForm.pgcPages;
   for i := 0 to lPageControl.PageCount-1 do
      UpdateZOrder(lPageControl.Pages[i]);

   iter := GetComponents(PAGE_INDEX_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, IXMLable, lXmlObj) and lXmlObj.Active then
         lXmlObj.ExportToXMLTag(ATag);
   end;

   itr := TBaseFormIterator.Create;
   while itr.HasNext do
      TBaseForm(itr.Next).ExportSettingsToXMLTag(ATag);
   
end;

procedure TProject.ImportPagesFromXML(const ATag: IXMLElement);
var
   lPageList, lPageName, lPageFront: string;
   i, len: integer;
   lPage, lActivePage: TTabSheet;
begin
   if ATag <> nil then
   begin
      lActivePage := nil;
      lPageName := '';
      lPageFront := ATag.GetAttribute(PAGE_FRONT_ATTR);
      if lPageFront = '' then
         lActivePage := GetMainPage;
      lPageList := ATag.GetAttribute(PAGE_ORDER_ATTR);
      len := Length(lPageList);
      for i := 1 to len do
      begin
         lPage := nil;
         if lPageList[i] = PAGE_LIST_DELIM then
         begin
            lPage := GetPage(lPageName);
            lPageName := '';
         end
         else
         begin
            lPageName := lPageName + lPageList[i];
            if i = len then
               lPage := GetPage(lPageName);
         end;
         if (lPage <> nil) and (lActivePage = nil) and AnsiSameCaption(lPage.Caption, lPageFront) then
            lActivePage := lPage;
      end;
      if lActivePage <> nil then
         lActivePage.PageControl.ActivePage := lActivePage;
   end;
end;

function TProject.ImportFromXMLTag(const ATag: IXMLElement): TErrorType;
var
   itr: IIterator;
   lString, lang: string;
   lLangDef: TLangDefinition;
begin
   result := errValidate;
   lang := ATag.GetAttribute(LANG_ATTR);
   lLangDef := GInfra.GetLangDefinition(lang);
   if lLangDef = nil then
      Gerr_text := i18Manager.GetFormattedString('LngNoSprt', [lang])
   else
   begin
      if AnsiSameText(lang, GInfra.DummyLang.Name) then
         lString := 'ChangeLngNone'
      else
         lString := 'ChangeLngAsk';
      if (not AnsiSameText(GInfra.CurrentLang.Name, lang)) and
         (TInfra.ShowFormattedQuestionBox(lString, [Trim(lang), CRLF], MB_YESNO+MB_ICONQUESTION) = IDYES) then
      begin
         GInfra.SetCurrentLang(lang);
{$IFDEF USE_CODEFOLDING}
         TInfra.GetEditorForm.ReloadFoldRegions;
{$ENDIF}
         TInfra.GetEditorForm.SetFormAttributes;
         SetGlobals;
      end;

      if FGlobalConsts <> nil then
         FGlobalConsts.ImportFromXMLTag(ATag);
         
      if GInfra.CurrentLang.EnabledUserDataTypes then
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
end;

procedure TProject.SetGlobals;
var
   lLeft, lWidth: integer;
begin
   lWidth := 0;
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
            lLeft := FGlobalVars.BoundsRect.Right
         else
            lLeft := 2;
         FGlobalConsts := TConstDeclareList.Create(TInfra.GetDeclarationsForm, lLeft, 1, DEF_CONSTLIST_WIDTH, 6, 3, DEF_CONSTLIST_WIDTH-10);
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
      lWidth := FGlobalVars.BoundsRect.Right + 16;
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
      lWidth := FGlobalConsts.BoundsRect.Right + 16;
      if GSettings <> nil then
      begin
         FGlobalConsts.sgList.ColWidths[0] := GSettings.ColumnC1Width;
         FGlobalConsts.sgList.ColWidths[1] := GSettings.ColumnC2Width;
         FGlobalConsts.sgList.ColWidths[2] := GSettings.ColumnC3Width;
      end;
   end;
   if lWidth > 0 then
   begin
      TInfra.GetDeclarationsForm.Constraints.MaxWidth := lWidth;
      TInfra.GetDeclarationsForm.Constraints.MinWidth := lWidth;
   end;
   TInfra.GetMainForm.SetMenu(true);
end;

function TProject.ImportUserFunctionsFromXML(const ATag: IXMLElement): TErrorType;
var
   tag, tag1: IXMLElement;
   lHeader: TUserFunctionHeader;
   lBody: TMainBlock;
   lTmpBlock: TBlock;
   lPage: TTabSheet;
begin
   result := errNone;
   tag := TXMLProcessor.FindChildTag(ATag, FUNCTION_TAG);
   while (tag <> nil) and (result = errNone) do
   begin
      lHeader := nil;
      lBody := nil;
      tag1 := TXMLProcessor.FindChildTag(tag, HEADER_TAG);
      if (tag1 <> nil) and GInfra.CurrentLang.EnabledUserFunctionHeader then
      begin
         lHeader := TUserFunctionHeader.Create(TInfra.GetFunctionsForm);
         lHeader.ImportFromXMLTag(tag1);
         lHeader.RefreshTab;
      end;
      tag1 := TXMLProcessor.FindChildTag(tag, BLOCK_TAG);
      if (tag1 <> nil) and GInfra.CurrentLang.EnabledUserFunctionBody then
      begin
         lPage := GetPage(tag1.GetAttribute(PAGE_CAPTION_ATTR));
         if lPage = nil then
            lPage := GetMainPage;
         lTmpBlock := TXMLProcessor.ImportFlowchartFromXMLTag(tag1, lPage, nil, result);
         if lTmpBlock is TMainBlock then
            lBody := TMainBlock(lTmpBlock);
      end;
      if result = errNone then
         TUserFunction.Create(lHeader, lBody)
      else
         lHeader.Free;
      tag := TXMLProcessor.FindNextTag(tag);
   end;
end;

function TProject.ImportUserDataTypesFromXML(const ATag: IXMLElement): TErrorType;
var
   lDataType, lType: TUserDataType;
   tag: IXMLElement;
   iter: IIterator;
begin
   result := errNone;
   lDataType := nil;
   tag := TXMLProcessor.FindChildTag(ATag, DATATYPE_TAG);
   while tag <> nil do
   begin
      lDataType := TUserDataType.Create(TInfra.GetDataTypesForm);
      lDataType.ImportFromXMLTag(tag);
      lDataType.RefreshTab;
      tag := TXMLProcessor.FindNextTag(tag);
   end;
   if FGlobalVars <> nil then
      TInfra.PopulateDataTypeCombo(FGlobalVars.cbType);
   if lDataType <> nil then
   begin
      PopulateDataTypes;
      iter := GetUserDataTypes;
      while iter.HasNext do
      begin
         lType := TUserDataType(iter.Next);
         lType.RefreshSizeEdits;
         lType.RefreshTab;
      end;
   end;
end;

function TProject.ImportCommentsFromXML(const ATag: IXMLElement): integer;
var
   lComment: TComment;
   tag: IXMLElement;
   lPage: TBlockTabSheet;
begin
   result := NO_ERROR;
   tag := TXMLProcessor.FindChildTag(ATag, COMMENT_ATTR);
   while tag <> nil do
   begin
      lPage := GetPage(tag.GetAttribute(PAGE_CAPTION_ATTR));
      if lPage = nil then
         lPage := GetMainPage;
      lComment := TComment.CreateDefault(lPage);
      lComment.ImportFromXMLTag(tag, nil);
      tag := TXMLProcessor.FindNextTag(tag);
   end;
end;

function TProject.GetProgramHeader: string;
var
   i: integer;
   lComment: TComment;
begin
   result := '';
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TComment then
      begin
         lComment := TComment(FComponentList[i]);
         if lComment.IsHeader then
         begin
            result := lComment.Text;
            if AnsiEndsText(CRLF, lComment.Text) then
               result := result + CRLF;
            break;
         end;
      end;
   end;
end;

function TProject.GetBottomRight: TPoint;
var
   lPoint: TPoint;
   i: integer;
   lBoundsObj: IMaxBoundable;
begin
   result := Point(0, 0);
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], IMaxBoundable, lBoundsObj) then
      begin
         lPoint := lBoundsObj.GetMaxBounds;
         if lPoint.X > result.X then
            result.X := lPoint.X;
         if lPoint.Y > result.Y then
            result.Y := lPoint.Y;
      end;
   end;
end;

function TProject.GetIWinControlComponent(const AHandle: THandle): IWinControl;
var
   i: integer;
   lWinControl: IWinControl;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], IWinControl, lWinControl) and (lWinControl.GetHandle = AHandle) then
      begin
         result := lWinControl;
         break;
      end;
   end;
end;

procedure TProject.UpdateZOrder(const AParent: TWinControl);
var
   lWinControl: IWinControl;
   lWnd: THandle;
   i: integer;
begin
   i := 0;
   if AParent <> nil then
   begin
      lWnd := GetWindow(GetTopWindow(AParent.Handle), GW_HWNDLAST);
      while lWnd <> 0 do
      begin
         lWinControl := GetIWinControlComponent(lWnd);
         if lWinControl <> nil then
         begin
            lWinControl.SetZOrder(i);
            i := i + 1;
         end;
         lWnd := GetNextWindow(lWnd, GW_HWNDPREV);
      end;
   end;
end;

procedure TProject.RefreshZOrder;
var
   iter: IIterator;
   lWinControl: IWinControl;
begin
   iter := GetComponents(Z_ORDER_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, IWinControl, lWinControl) then
         lWinControl.BringAllToFront;
   end;
end;

procedure TProject.PaintToCanvas(const ACanvas: TCanvas);
var
   lWinControl: IWinControl;
   lWnd: THandle;
begin
   with ACanvas do
   begin
      Brush.Style := bsSolid;
      Brush.Color := GetActivePage.Brush.Color;
      PatBlt(Handle, ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom, PATCOPY);
   end;
   lWnd := GetWindow(GetTopWindow(GetActivePage.Handle), GW_HWNDLAST);
   while lWnd <> 0 do
   begin
      lWinControl := GetIWinControlComponent(lWnd);
      if lWinControl <> nil then
         lWinControl.PaintToCanvas(ACanvas);
      lWnd := GetNextWindow(lWnd, GW_HWNDPREV);
   end;
end;

procedure TProject.ExportToGraphic(const AImage: TGraphic);
var
   lPoint: TPoint;
   lBitmap: TBitmap;
begin
   if AImage is TBitmap then
      lBitmap := TBitmap(AImage)
   else
      lBitmap := TBitmap.Create;
   lPoint := GetBottomRight;
   lBitmap.Width := lPoint.X;
   lBitmap.Height := lPoint.Y;
   PaintToCanvas(lBitmap.Canvas);
   if AImage <> lBitmap then
   begin
      AImage.Assign(lBitmap);
      lBitmap.Free;
   end;
end;

procedure TProject.SetCommentsColor(const AColor: TColor);
var
   i: integer;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TComment then
         TComment(FComponentList[i]).Color := AColor;
   end;
end;

function TProject.GetMainBlock: TMainBlock;
var
   i: integer;
   lFunction: TUserFunction;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if (lFunction.Header = nil) and lFunction.Active then
         begin
            result := lFunction.Body;
            break;
         end;
      end;
   end;
end;

procedure TProject.PopulateDataTypeCombos;
var
   i: integer;
   lFunction: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if lFunction.Body <> nil then
            lFunction.Body.PopulateComboBoxes;
      end;
   end;
end;

procedure TProject.ChangeFlowchartsColor(const AColor: TColor);
var
   i: integer;
   lFunction: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if lFunction.Body <> nil then
            lFunction.Body.ChangeColor(AColor);
      end;
   end;
end;

function TProject.CountErrWarn: TErrWarnCount;
var
   i: integer;
   lFunction: TUserFunction;
   lDataType: TUserDataType;
   lErrWarnCount: TErrWarnCount; 
begin
   result.ErrorCount := 0;
   result.WarningCount := 0;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if lFunction.Active then
         begin
            if lFunction.Header <> nil then
            begin
               case lFunction.Header.GetFocusColor of
                  NOK_COLOR:  Inc(result.ErrorCount);
                  WARN_COLOR: Inc(result.WarningCount);
               end;
            end;
            if lFunction.Body <> nil then
            begin
               lErrWarnCount := lFunction.Body.CountErrWarn;
               Inc(result.ErrorCount, lErrWarnCount.ErrorCount);
               Inc(result.WarningCount, lErrWarnCount.WarningCount);
            end;
         end;
      end
      else if FComponentList[i] is TUserDataType then
      begin
         lDataType := TUserDataType(FComponentList[i]);
         if lDataType.Active then
         begin
            case lDataType.GetFocusColor of
               NOK_COLOR:  Inc(result.ErrorCount);
               WARN_COLOR: Inc(result.WarningCount);
            end;
         end;
      end;
   end;
end;

procedure TProject.GenerateTree(const ANode: TTreeNode);
var
   lDataTypes, lFunctions: IIterator;
   lDataType: TUserDataType;
   lMainFunction, lFunction: TUserFunction;
   lNode: TTreeNode;
begin

   lMainFunction := nil;

   if GInfra.CurrentLang.EnabledUserDataTypes then
   begin
      lNode := ANode.Owner.AddChildObject(ANode, i18Manager.GetString('Structures'), TInfra.GetDataTypesForm);
      lDataTypes := GetUserDataTypes;
      while lDataTypes.HasNext do
      begin
         lDataType := TUserDataType(lDataTypes.Next);
         if lDataType.Active then
            lDataType.GenerateTree(lNode);
      end;
   end;

   if GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts then
      ANode.Owner.AddChildObject(ANode, i18Manager.GetString('GlobalDeclares'), TInfra.GetDeclarationsForm);

   if GInfra.CurrentLang.EnabledUserFunctionHeader then
      lNode := ANode.Owner.AddChildObject(ANode, i18Manager.GetString('Functions'), TInfra.GetFunctionsForm)
   else
      lNode := ANode;

   lFunctions := GetUserFunctions;
   while lFunctions.HasNext do
   begin
      lFunction := TUserFunction(lFunctions.Next);
      if lFunction.Active then
      begin
         if lFunction.IsMain and (lMainFunction = nil) then
            lMainFunction := lFunction
         else
            lFunction.GenerateTree(lNode);
      end;
   end;

   if lMainFunction <> nil then
      lMainFunction.GenerateTree(ANode);

end;

procedure TProject.UpdateHeadersBody(const APage: TTabSheet);
var
   i: integer;
   lFunction: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if (lFunction.Header <> nil ) and (lFunction.Body <> nil) and (lFunction.Body.Page = APage) then
            lFunction.Header.SetPageCombo(APage.Caption);
      end;
   end;
end;

procedure TProject.RefreshStatements;
var
   i: integer;
   lChange: byte;
   lFunction: TUserFunction;
begin
   lChange := GChange;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if lFunction.Active and (lFunction.Body <> nil) then
            lFunction.Body.RefreshStatements;
      end;
   end;
   NavigatorForm.Invalidate;
   if lChange = 0 then
      GChange := 0;
end;

function TProject.FindMainBlockForControl(const AControl: TControl): TMainBlock;
var
   i: integer;
   lFunction: TUserFunction;
begin
   result := nil;
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if lFunction.Active and (lFunction.Body <> nil) and (lFunction.Body.Parent = AControl.Parent) and PtInRect(lFunction.Body.BoundsRect, AControl.BoundsRect.TopLeft) then
         begin
            result := lFunction.Body;
            break;
         end;
      end;
   end;
end;

procedure TProject.RepaintComments;
var
   i: integer;
   lComment: TComment;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TComment then
      begin
         lComment := TComment(FComponentList[i]);
         if lComment.Visible then
            lComment.Repaint;
      end;
   end;
end;

procedure TProject.RepaintFlowcharts;
var
   i: integer;
   lFunction: TUserFunction;
begin
   for i := 0 to FComponentList.Count-1 do
   begin
      if FComponentList[i] is TUserFunction then
      begin
         lFunction := TUserFunction(FComponentList[i]);
         if (lFunction.Body <> nil) and lFunction.Body.Visible then
            lFunction.Body.RepaintAll;
      end;
   end;
end;

function TProject.GetLibraryList: TStringList;
var
   lLibName: string;
   lTabObj: ITabbable;
   iter: IIterator;
begin
   result := TStringList.Create;
   result.CaseSensitive := GInfra.CurrentLang.CaseSensitiveSyntax;
   iter := GetComponents(PAGE_INDEX_SORT);
   while iter.HasNext do
   begin
      if Supports(iter.Next, ITabbable, lTabObj) then
      begin
         lLibName := lTabObj.GetLibName;
         if (lLibName <> '') and (result.IndexOf(lLibName) = -1) then
            result.Add(lLibName);
      end;
   end;
end;

procedure TProject.RefreshSizeEdits;
var
   i: integer;
   lSizeEdit: ISizeEditable;
begin
   if (GlobalVars <> nil) and (GlobalVars.edtSize.Text <> '1') then
      GlobalVars.edtSize.OnChange(GlobalVars.edtSize);
   for i := 0 to FComponentList.Count-1 do
   begin
      if Supports(FComponentList[i], ISizeEditable, lSizeEdit) then
         lSizeEdit.RefreshSizeEdits;
   end;
end;

function TProject.GetUserDataType(const ATypeName: string): TUserDataType;
begin
   result := TUserDataType(GetComponentByName(TUserDataType.ClassName, ATypeName));
end;

function TProject.GetUserFunction(const AFunctionName: string): TUserFunction;
begin
   result := TUserFunction(GetComponentByName(TUserFunction.ClassName, AFunctionName));
end;

function TProject.GetComponentByName(const AClassName: string; const AName: string): TComponent;
var
   i: integer;
   lTabObj: ITabbable;
begin
   result := nil;
   if Trim(AName) <> '' then
   begin
      for i := 0 to FComponentList.Count-1 do
      begin
         if FComponentList[i].ClassNameIs(AClassName) and Supports(FComponentList[i], ITabbable, lTabObj) then
         begin
            if TInfra.SameStrings(lTabObj.GetName, AName) then
            begin
               result := FComponentList[i];
               break;
            end;
         end;
      end;
   end;
end;

initialization

   Instance := nil;

end.

