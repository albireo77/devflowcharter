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



unit Functions_Form;

interface

uses
   System.Types, OmniXML, PageControl_Form, Types;

type

  TFunctionsForm = class(TPageControlForm)
    procedure miAddClick(Sender: TObject); override;
    procedure pgcTabsChange(Sender: TObject); override;
    function IsEnabled: boolean; override;
  public
    { Public declarations }
    function ImportTabsFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError; override;
    procedure RefreshTabs; override;
    procedure ResetForm; override;
    procedure AddUserFunction(const ABodyTopLeft: TPoint);
  end;

var
  FunctionsForm: TFunctionsForm;

implementation

{$R *.dfm}

uses
   System.SysUtils, Infrastructure, Base_Block, Main_Block, Navigator_Form,
   UserFunction, Interfaces, XMLProcessor, BlockTabSheet, Constants;

procedure TFunctionsForm.miAddClick(Sender: TObject);
begin
   AddUserFunction(TInfra.GetMainForm.GetMainBlockNextTopLeft);
end;

procedure TFunctionsForm.AddUserFunction(const ABodyTopLeft: TPoint);
var
   header: TUserFunctionHeader;
   body: TMainBlock;
begin
   Show;
   body := TMainBlock.Create(GProject.ActivePage, ABodyTopLeft);
   header := TUserFunctionHeader.Create(Self);
   TUserFunction.Create(header, body);
   if Visible and Enabled then  // replace with CanFocus once fixed by Embarcadero (RSP-34465)
      SetFocus;
   pgcTabs.ActivePage := header;
   if header.edtName.CanFocus then
      header.edtName.SetFocus;
   if Assigned(header.edtName.OnChange) then
      header.edtName.OnChange(header.edtName);
   if header.Font.Color <> NOK_COLOR then
      TInfra.UpdateCodeEditor(header);
   GProject.SetChanged;
end;

procedure TFunctionsForm.pgcTabsChange(Sender: TObject);
var
   body: TMainBlock;
   bpage: TBlockTabSheet;
begin
   inherited pgcTabsChange(Sender);
   body := TUserFunctionHeader(pgcTabs.ActivePage).UserFunction.Body;
   if (body <> nil) and body.Visible then
   begin
      bpage := body.Page;
      bpage.PageControl.ActivePage := bpage;
      bpage.Box.ScrollInView(body);
      body.BringAllToFront;
      NavigatorForm.Invalidate;
   end;
end;

function TFunctionsForm.ImportTabsFromXMLTag(ATag: IXMLElement; AImportMode: TImportMode): TError;
begin
   result := GProject.ImportUserFunctionsFromXML(ATag, AImportMode);
end;

procedure TFunctionsForm.RefreshTabs;
var
   i: integer;
   header: TUserFunctionHeader;
   param: TParameter;
begin
   inherited;
   for i := 0 to pgcTabs.PageCount-1 do
   begin
      header := TUserFunctionHeader(pgcTabs.Pages[i]);
      TInfra.PopulateDataTypeCombo(header.LocalVars.cbType);
      TInfra.PopulateDataTypeCombo(header.cbType);
      for param in header.GetParameters do
         TInfra.PopulateDataTypeCombo(param.cbType);
   end;
end;

procedure TFunctionsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 625;
   FPrefix := 'func_';
end;

function TFunctionsForm.IsEnabled: boolean;
begin
   result := GInfra.CurrentLang.EnabledUserFunctionHeader;
end;

end.
