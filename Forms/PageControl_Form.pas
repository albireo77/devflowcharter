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



unit PageControl_Form;

interface

uses
   Vcl.Menus, Vcl.ComCtrls, Vcl.Controls, System.Classes, System.Types, OmniXML, Base_Form, Types;

type

  TPageControlForm = class(TBaseForm)
    MainMenu1: TMainMenu;
    miAction: TMenuItem;
    miAdd: TMenuItem;
    miRemove: TMenuItem;
    N1: TMenuItem;
    miRemoveAll: TMenuItem;
    miImport: TMenuItem;
    miExport: TMenuItem;
    miExportAll: TMenuItem;
    pgcTabs: TPageControl;
    procedure miAddClick(Sender: TObject); virtual; abstract;
    procedure miRemoveClick(Sender: TObject);
    procedure miActionClick(Sender: TObject);
    procedure pgcTabsDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure miExportClick(Sender: TObject);
    procedure miImportClick(Sender: TObject);
    procedure miRemoveAllClick(Sender: TObject);
    procedure pgcTabsChange(Sender: TObject); virtual;
    procedure miExportAllClick(Sender: TObject);
    procedure ExportTabsToXML(ANode: IXMLNode);
    function IsEnabled: boolean; virtual; abstract;
    function ImportTabsFromXML(ANode: IXMLNode; AImportMode: TImportMode): TError; virtual; abstract;
    procedure FormDeactivate(Sender: TObject); virtual;
    procedure RefreshTabs; virtual;
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
    procedure pgcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pgcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pgcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pgcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pgcTabsMouseLeave(Sender: TObject);
    procedure ResetForm; override;
  protected
    FPrefix: string;
  public
    UpdateCodeEditor: boolean;
    { Public declarations }
    function GetVisiblePageCount: integer;
  private
    FLastHintPageIndex: integer;
  end;

implementation

{$R *.dfm}

uses
   System.SysUtils, System.StrUtils, Vcl.Forms, Infrastructure, XMLProcessor, OmniXMLUtils,
   TabComponent, Interfaces;

procedure TPageControlForm.miRemoveClick(Sender: TObject);
begin
   if pgcTabs.ActivePage <> nil then
   begin
      var tab := TTabComponent(pgcTabs.ActivePage);
      tab.Active := False;
      GClpbrd.UndoObject.Free;
      GClpbrd.UndoObject := tab.ParentObject;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.ResetForm;
begin
   UpdateCodeEditor := True;
   FLastHintPageIndex := -1;
   inherited ResetForm;
end;

procedure TPageControlForm.miActionClick(Sender: TObject);
begin
   miRemove.Enabled := pgcTabs.ActivePage <> nil;
   miRemoveAll.Enabled := GetVisiblePageCount > 0;
   miExport.Enabled := miRemove.Enabled;
   miExportAll.Enabled := miRemoveAll.Enabled;
end;

procedure TPageControlForm.RefreshTabs;
begin
{}
end;

procedure TPageControlForm.pgcTabsDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
   var page := TInfra.GetPageFromTabIndex(pgcTabs, TabIndex);
   if page <> nil then
   begin
      var lRect := Rect;
      lRect.Right := lRect.Right-3;
      TTabComponent(page).RefreshFontColor;
      Control.Canvas.Font.Color := page.Font.Color;
      Control.Canvas.TextRect(lRect, lRect.Left+5, lRect.Top+3, page.Caption);
   end;
end;

procedure TPageControlForm.ExportTabsToXML(ANode: IXMLNode);
var
   xmlable: IXMLable;
begin
   for var i := 0 to pgcTabs.PageCount-1 do
   begin
      if pgcTabs.Pages[i].TabVisible and Supports(pgcTabs.Pages[i], IXMLable, xmlable) then
         xmlable.ExportToXML(ANode);
   end;
end;

procedure TPageControlForm.miExportClick(Sender: TObject);
var
   exportable: IExportable;
begin
   if Supports(pgcTabs.ActivePage, IExportable, exportable) then
      TInfra.ExportToFile(exportable);
end;

procedure TPageControlForm.miExportAllClick(Sender: TObject);
begin
   var fileName := ReplaceStr(GProject.Name + ' ' + Caption, ' ', '_');
   TXMLProcessor.ExportToXMLFile(ExportTabsToXML, fileName);
end;

function TPageControlForm.GetVisiblePageCount: integer;
begin
   result := 0;
   for var i := 0 to pgcTabs.PageCount-1 do
   begin
      if pgcTabs.Pages[i].TabVisible then
         Inc(result);
   end;
end;

procedure TPageControlForm.miImportClick(Sender: TObject);
begin
   if not TXMLProcessor.ImportFromXMLFile(ImportTabsFromXML, impSelectTab).IsEmpty then
      TInfra.UpdateCodeEditor;
end;

procedure TPageControlForm.miRemoveAllClick(Sender: TObject);
begin
   var res := mrYes;
   if GSettings.ConfirmRemove then
      res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
   if res = mrYes then
   begin
      while GetVisiblePageCount > 0 do
      begin
         for var i := 0 to pgcTabs.PageCount-1 do
         begin
            if pgcTabs.Pages[i].TabVisible then
            begin
               TTabComponent(pgcTabs.Pages[i]).ParentObject.Free;
               break;
            end;
         end;
      end;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.pgcTabsChange(Sender: TObject);
begin
   TInfra.GetEditorForm.SelectCodeRange(pgcTabs.ActivePage);
end;

procedure TPageControlForm.FormDeactivate(Sender: TObject);
begin
   if GProject <> nil then
      GProject.RefreshStatements;
end;

procedure TPageControlForm.pgcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
   var page := TInfra.GetPageFromXY(pgcTabs, X, Y);
   if page <> nil then
   begin
      var idx := page.PageIndex;
      page.PageIndex := TTabSheet(Source).PageIndex;
      TTabSheet(Source).PageIndex := idx;
      RefreshTabs;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.pgcTabsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if not (Source is TTabComponent) then
      Accept := False;
end;

procedure TPageControlForm.pgcTabsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft then
   begin
      var page := TInfra.GetPageFromXY(pgcTabs, X, Y);
      if page <> nil then
         page.BeginDrag(False, 3);
   end;
end;

procedure TPageControlForm.pgcTabsMouseLeave(Sender: TObject);
begin
   pgcTabs.Hint := '';
   FLastHintPageIndex := -1;
end;

procedure TPageControlForm.pgcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
   var page := TInfra.GetPageFromXY(pgcTabs, X, Y);
   if (page <> nil) and (page.PageIndex <> FLastHintPageIndex) then
   begin
      Application.CancelHint;
      pgcTabs.Hint := page.Caption;
      FLastHintPageIndex := page.PageIndex;
   end;
end;

procedure TPageControlForm.ExportToXML(ANode: IXMLNode);
begin
   RefreshTabs;
   SetNodeAttrInt(ANode, FPrefix + 'win_h', Height);
   if Visible then
   begin
      SetNodeAttrBool(ANode, FPrefix + 'win_show', True);
      SetNodeAttrInt(ANode, FPrefix + 'win_x', Left);
      SetNodeAttrInt(ANode, FPrefix + 'win_y', Top);
      var i := pgcTabs.ActivePageIndex;
      if i <> -1 then
      begin
         SetNodeAttrInt(ANode, FPrefix + 'idx', i);
         var a := TTabComponent(pgcTabs.ActivePage).ScrollPos;
         if a > 0 then
            SetNodeAttrInt(ANode, FPrefix + 'scroll_v', a);
      end;
      if WindowState = wsMinimized then
         SetNodeAttrBool(ANode, FPrefix + 'win_min', True);
   end;
end;

procedure TPageControlForm.ImportFromXML(ANode: IXMLNode);
begin
   Height := GetNodeAttrInt(ANode, FPrefix + 'win_h', Height);
   if IsEnabled and GetNodeAttrBool(ANode, FPrefix + 'win_show', False) then
   begin
      Position := poDesigned;
      if GetNodeAttrBool(ANode, FPrefix + 'win_min', False) then
         WindowState := wsMinimized;
      Left := GetNodeAttrInt(ANode, FPrefix + 'win_x');
      Top := GetNodeAttrInt(ANode, FPrefix + 'win_y');
      var i := GetNodeAttrInt(ANode, FPrefix + 'idx', -2);
      if (i >= 0) and (i < pgcTabs.PageCount) then
      begin
         pgcTabs.ActivePageIndex := i;
         i := GetNodeAttrInt(ANode, FPrefix + 'scroll_v', 0);
         if i > 0 then
            TTabComponent(pgcTabs.ActivePage).ScrollPos := i;
      end;
      Show;
   end;
end;

end.
