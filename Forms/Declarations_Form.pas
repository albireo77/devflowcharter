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
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExportToXML(ANode: IXMLNode); override;
    procedure ImportFromXML(ANode: IXMLNode); override;
    procedure ResetForm; override;
  end;

var
  DeclarationsForm: TDeclarationsForm;

implementation

uses
   Vcl.Forms, System.SysUtils, OmniXMLUtils, DeclareList, Infrastructure, Constants;

{$R *.dfm}

procedure TDeclarationsForm.ExportToXML(ANode: IXMLNode);
begin
   SetNodeAttrInt(ANode, 'var_win_h', Height);
   SetNodeAttrInt(ANode, 'var_win_w', Width);
   if Visible then
   begin
      SetNodeAttrBool(ANode, 'var_win_show', True);
      SetNodeAttrInt(ANode, 'var_win_x', Left);
      SetNodeAttrInt(ANode, 'var_win_y', Top);
      if WindowState = wsMinimized then
         SetNodeAttrBool(ANode, 'var_win_min', True);
   end;
end;

procedure TDeclarationsForm.ImportFromXML(ANode: IXMLNode);
begin
   Height := GetNodeAttrInt(ANode, 'var_win_h');
   Width := GetNodeAttrInt(ANode, 'var_win_w');
   if GetNodeAttrBool(ANode, 'var_win_show', False) and (GInfra.CurrentLang.EnabledVars or GInfra.CurrentLang.EnabledConsts) then
   begin
      Position := poDesigned;
      if GetNodeAttrBool(ANode, 'var_win_min', False) then
         WindowState := wsMinimized;
      Left := GetNodeAttrInt(ANode, 'var_win_x');
      Top := GetNodeAttrInt(ANode, 'var_win_y');
      Show;
   end;
end;

procedure TDeclarationsForm.ResetForm;
begin
   inherited ResetForm;
   Height := 323;
   Width := 610;
end;

procedure TDeclarationsForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin
   if NewWidth < Width then
   begin
      var i := ControlCount;
      if (i > 0) and  (Controls[i-1] is TDeclareList) then
      begin
         var declareList := TDeclareList(Controls[i-1]);
         if NewWidth < (declareList.sgList.GetMinWidth + declareList.Left + DECLARATIONS_FORM_RIGHT_MARGIN) then
            Resize := False;
      end;
   end;
end;

procedure TDeclarationsForm.FormShow(Sender: TObject);
begin
   var f := True;
   for var control in GetControls do
   begin
      if control is TDeclareList then
      begin
         var declareList := TDeclareList(control);
         if f then
         begin
             f := False;
             declareList.SetDefaultFocus;
         end;
         declareList.sgList.ColWidthsChanged;
      end;
   end;
end;

end.
