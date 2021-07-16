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



unit Declarations_Form;

interface

uses
   OmniXML, Base_Form;

type

  TDeclarationsForm = class(TBaseForm)
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExportSettingsToXMLTag(ATag: IXMLElement); override;
    procedure ImportSettingsFromXMLTag(ATag: IXMLElement); override;
    procedure ResetForm; override;
  end;

var
  DeclarationsForm: TDeclarationsForm;

implementation

uses
   Vcl.Forms, System.SysUtils, XMLProcessor, DeclareList, Infrastructure, Constants;

{$R *.dfm}

procedure TDeclarationsForm.ExportSettingsToXMLTag(ATag: IXMLElement);
begin
   ATag.SetAttribute('var_win_h', Height.ToString);
   ATag.SetAttribute('var_win_w', Width.ToString);
   if Visible then
   begin
      ATag.SetAttribute('var_win_show', 'true');
      ATag.SetAttribute('var_win_x', Left.ToString);
      ATag.SetAttribute('var_win_y', Top.ToString);
      if WindowState = wsMinimized then
         ATag.SetAttribute('var_win_min', 'true');
   end;
end;

procedure TDeclarationsForm.ImportSettingsFromXMLTag(ATag: IXMLElement);
begin
   Height := TXMLProcessor.GetInt(ATag, 'var_win_h', Height);
   Width := TXMLProcessor.GetInt(ATag, 'var_win_w', Width);
   if TXMLProcessor.GetBool(ATag, 'var_win_show') and (GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts) then
   begin
      Position := poDesigned;
      if TXMLProcessor.GetBool(ATag, 'var_win_min') then
         WindowState := wsMinimized;
      Left := TXMLProcessor.GetInt(ATag, 'var_win_x', Left);
      Top := TXMLProcessor.GetInt(ATag, 'var_win_y', Top);
      Show;
   end;
end;

procedure TDeclarationsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
   Width := 610;
end;

procedure TDeclarationsForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
   i: integer;
   declareList: TDeclareList;
begin
   if NewWidth < Width then
   begin
      i := ControlCount;
      if (i > 0) and  (Controls[i-1] is TDeclareList) then
      begin
         declareList := TDeclareList(Controls[i-1]);
         if NewWidth < (declareList.sgList.GetMinWidth + declareList.Left + DECLARATIONS_FORM_RIGHT_MARGIN) then
            Resize := false;
      end;
   end;
end;

procedure TDeclarationsForm.FormShow(Sender: TObject);
var
   i: integer;
   f: boolean;
   declareList: TDeclareList;
begin
   f := true;
   for i := 0 to ControlCount-1 do
   begin
      if Controls[i] is TDeclareList then
      begin
         declareList := TDeclareList(Controls[i]);
         if f then
         begin
             f := false;
             declareList.SetDefaultFocus;
         end;
         declareList.sgList.ColWidthsChanged;
      end;
   end;
end;

end.
