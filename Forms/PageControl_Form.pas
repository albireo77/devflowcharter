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
   Forms, StdCtrls, ExtCtrls, Graphics, Controls, Menus, ComCtrls, SysUtils,
   Classes, Types, Windows, OmniXML, Base_Form, CommonTypes;

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
    procedure pgcTabsDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure miExportClick(Sender: TObject);
    procedure miImportClick(Sender: TObject);
    procedure miRemoveAllClick(Sender: TObject);
    procedure pgcTabsChange(Sender: TObject); virtual;
    procedure miExportAllClick(Sender: TObject);
    procedure ExportTabsToXMLTag(const ATag: IXMLElement);
    function ImportTabsFromXMLTag(const ATag: IXMLElement): TErrorType; virtual; abstract;
    procedure FormDeactivate(Sender: TObject); virtual;
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RefreshTabs; virtual;
    procedure Localize(const list: TStringList); override;
    procedure pgcTabsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pgcTabsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pgcTabsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResetForm; override;
  private
    procedure ScrollElements(const AValue: integer);
  public
    UpdateCodeEditor: boolean;
    { Public declarations }
    function GetVisiblePageCount: integer;
  end;

implementation

{$R *.dfm}

uses
   ApplicationCommon, XMLProcessor, TabComponent, StrUtils;

procedure TPageControlForm.miRemoveClick(Sender: TObject);
var
   lTabComp: TTabComponent;
begin
   if pgcTabs.ActivePage <> nil then
   begin
      lTabComp := TTabComponent(pgcTabs.ActivePage);
      pgcTabs.OwnerDraw := false;
      lTabComp.Active := false;
      GClpbrd.UndoObject.Free;
      pgcTabs.OwnerDraw := true;
      GClpbrd.UndoObject := lTabComp.OverlayObject;
      TInfra.UpdateCodeEditor;
   end;
end;

procedure TPageControlForm.ResetForm;
begin
   UpdateCodeEditor := true;
   inherited ResetForm;
end;

procedure TPageControlForm.miActionClick(Sender: TObject);
begin
   miRemove.Enabled := pgcTabs.ActivePage <> nil;
   miRemoveAll.Enabled := GetVisiblePageCount > 0;
   miExport.Enabled := miRemove.Enabled;
   miExportAll.Enabled := miRemoveAll.Enabled;
end;

procedure TPageControlForm.Localize(const list: TStringList);
var
   i: integer;
begin
   inherited Localize(list);
   for i := 0 to pgcTabs.PageCount-1 do
      TTabComponent(pgcTabs.Pages[i]).Localize(list);
end;

procedure TPageControlForm.RefreshTabs;
begin
{}
end;

procedure TPageControlForm.pgcTabsDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
   ARect: TRect;
   lTab: TTabComponent;
begin
   TabIndex := TInfra.GetPageIndex(TPageControl(Control), Rect.Left+5, Rect.Top+5);
   if TabIndex <> -1 then
   begin
      ARect := Rect;
      ARect.Right := ARect.Right-3;
      lTab := TTabComponent(TPageControl(Control).Pages[TabIndex]);
      lTab.RefreshTab;
      Control.Canvas.Font.Color := lTab.Font.Color;
      Control.Canvas.TextRect(ARect, ARect.Left+5, ARect.Top+3, lTab.Caption);
   end;
end;

procedure TPageControlForm.ExportTabsToXMLTag(const ATag: IXMLElement);
var
   i: integer;
begin
   for i:= 0 to pgcTabs.PageCount-1 do
   begin
      if pgcTabs.Pages[i].TabVisible then
         TTabComponent(pgcTabs.Pages[i]).ExportToXMLTag(ATag);
   end;
end;

procedure TPageControlForm.miExportClick(Sender: TObject);
var
   lTab: TTabComponent;
begin
   if pgcTabs.ActivePage <> nil then
   begin
      lTab := TTabComponent(pgcTabs.ActivePage);
      TXMLProcessor.ExportToXMLFile(lTab.ExportToXMLTag, lTab.edtName.Text);
   end;
end;

procedure TPageControlForm.miExportAllClick(Sender: TObject);
var
   lFileName: string;
begin
   lFileName := AnsiReplaceStr(GProject.Name + ' ' + Caption, ' ', '_');
   TXMLProcessor.ExportToXMLFile(ExportTabsToXMLTag, lFileName);
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
   if TXMLProcessor.ImportFromXMLFile(ImportTabsFromXMLTag) <> '' then
      TInfra.UpdateCodeEditor;
end;

procedure TPageControlForm.miRemoveAllClick(Sender: TObject);
var
   i, res: integer;
begin
   res := IDYES;
   if GSettings.ConfirmRemove then
      res := TInfra.ShowQuestionBox(i18Manager.GetString('ConfirmRemove'));
   if res = IDYES then
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

procedure TPageControlForm.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
   ScrollElements(-2);
end;

procedure TPageControlForm.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
   ScrollElements(2);
end;

procedure TPageControlForm.ScrollElements(const AValue: integer);
begin
   if (pgcTabs.ActivePage <> nil) and not TTabComponent(pgcTabs.ActivePage).HasFocusedComboBox then
      TTabComponent(pgcTabs.ActivePage).ScrollElements(AValue);
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

end.
