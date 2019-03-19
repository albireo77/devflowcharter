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
   Vcl.Controls, Vcl.Menus, Vcl.ComCtrls, System.Classes, System.Types, OmniXML,
   Base_Form, CommonTypes;

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
    procedure ExportTabsToXMLTag(ATag: IXMLElement);
    function ImportTabsFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TErrorType; virtual; abstract;
    procedure FormDeactivate(Sender: TObject); virtual;
    procedure RefreshTabs; virtual;
    procedure pgcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pgcTabsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pgcTabsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pgcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pgcTabsMouseLeave(Sender: TObject);
    procedure ResetForm; override;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    UpdateCodeEditor: boolean;
    { Public declarations }
    function GetVisiblePageCount: integer;
  private
    FLastHintTabIndex: integer;
  end;

implementation

{$R *.dfm}

uses
   System.SysUtils, System.StrUtils, Vcl.Forms, ApplicationCommon, XMLProcessor,
   TabComponent, CommonInterfaces;

procedure TPageControlForm.miRemoveClick(Sender: TObject);
var
   tab: TTabComponent;
begin
   if pgcTabs.ActivePage <> nil then
   begin
      tab := TTabComponent(pgcTabs.ActivePage);
      pgcTabs.OwnerDraw := false;
      tab.Active := false;
      GClpbrd.UndoObject.Free;
      pgcTabs.OwnerDraw := true;
      GClpbrd.UndoObject := tab.OverlayObject;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.ResetForm;
begin
   UpdateCodeEditor := true;
   FLastHintTabIndex := -1;
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
var
   lRect: TRect;
   tab: TTabComponent;
begin
   TabIndex := TInfra.GetPageIndex(TPageControl(Control), Rect.Left+5, Rect.Top+5);
   if TabIndex <> -1 then
   begin
      lRect := Rect;
      lRect.Right := lRect.Right-3;
      tab := TTabComponent(TPageControl(Control).Pages[TabIndex]);
      tab.RefreshTab;
      Control.Canvas.Font.Color := tab.Font.Color;
      Control.Canvas.TextRect(lRect, lRect.Left+5, lRect.Top+3, tab.Caption);
   end;
end;

procedure TPageControlForm.ExportTabsToXMLTag(ATag: IXMLElement);
var
   i: integer;
   xmlable: IXMLable;
begin
   for i:= 0 to pgcTabs.PageCount-1 do
   begin
      if pgcTabs.Pages[i].TabVisible and Supports(pgcTabs.Pages[i], IXMLable, xmlable) then
         xmlable.ExportToXMLTag(ATag);
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
var
   fileName: string;
begin
   fileName := ReplaceStr(GProject.Name + ' ' + Caption, ' ', '_');
   TXMLProcessor.ExportToXMLFile(ExportTabsToXMLTag, fileName);
end;

function TPageControlForm.GetVisiblePageCount: integer;
var
   i: integer;
begin
   result := 0;
   for i:= 0 to pgcTabs.PageCount-1 do
   begin
      if pgcTabs.Pages[i].TabVisible then
         Inc(result);
   end;
end;

procedure TPageControlForm.miImportClick(Sender: TObject);
begin
   if not TXMLProcessor.ImportFromXMLFile(ImportTabsFromXMLTag, impSelectTab).IsEmpty then
      TInfra.UpdateCodeEditor;
end;

procedure TPageControlForm.miRemoveAllClick(Sender: TObject);
var
   i, res: integer;
begin
   res := mrYes;
   if GSettings.ConfirmRemove then
      res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
   if res = mrYes then
   begin
      while GetVisiblePageCount > 0 do
      begin
         for i := 0 to pgcTabs.PageCount-1 do
         begin
            if pgcTabs.Pages[i].TabVisible then
            begin
               TTabComponent(pgcTabs.Pages[i]).OverlayObject.Free;
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

procedure TPageControlForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
   WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
   tab: TTabComponent;
begin
   if pgcTabs.ActivePage is TTabComponent then
   begin
      tab := TTabComponent(pgcTabs.ActivePage);
      if not tab.HasFocusedComboBox then
         tab.ScrollElements(-WheelDelta div 10);
   end;
end;

procedure TPageControlForm.pgcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   idx: integer;
begin
   idx := TInfra.GetPageIndex(pgcTabs, X, Y);
   if idx <> -1 then
   begin
      pgcTabs.Pages[idx].PageIndex := TTabSheet(Source).PageIndex;
      TTabSheet(Source).PageIndex := idx;
      RefreshTabs;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.pgcTabsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
   if not (Source is TTabComponent) then
      Accept := false;
end;

procedure TPageControlForm.pgcTabsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   idx: integer;
begin
   if Button = mbLeft then
   begin
      idx := TInfra.GetPageIndex(pgcTabs, X, Y);
      if idx <> -1 then
         pgcTabs.Pages[idx].BeginDrag(false, 3);
   end;
end;

procedure TPageControlForm.pgcTabsMouseLeave(Sender: TObject);
begin
   pgcTabs.Hint := '';
   FLastHintTabIndex := -1;
end;

procedure TPageControlForm.pgcTabsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
   idx: integer;
begin
   idx := TInfra.GetPageIndex(pgcTabs, X, Y);
   if (idx <> -1) and (idx <> FLastHintTabIndex) then
   begin
      Application.CancelHint;
      pgcTabs.Hint := pgcTabs.Pages[idx].Caption;
      FLastHintTabIndex := idx;
   end;
end;

end.
