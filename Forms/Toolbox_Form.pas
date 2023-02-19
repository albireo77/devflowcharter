{
   Copyright (C) 2006 The devFlowcharter project
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



unit Toolbox_Form;

interface

uses
   Vcl.Forms, Vcl.Buttons, System.Classes, Base_Form, Vcl.Controls;

type

  TToolboxForm = class(TBaseForm)
    sbNormal: TSpeedButton;
    sbRepeat: TSpeedButton;
    sbWhile: TSpeedButton;
    sbIfElse: TSpeedButton;
    sbFor: TSpeedButton;
    sbInstr: TSpeedButton;
    sbMultiInstr: TSpeedButton;
    sbInput: TSpeedButton;
    sbOutput: TSpeedButton;
    sbFuncCall: TSpeedButton;
    sbIf: TSpeedButton;
    sbCase: TSpeedButton;
    sbReturn: TSpeedButton;
    sbText: TSpeedButton;
    sbFolder: TSpeedButton;
    procedure sbNormalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ResetForm; override;
  end;

var
  ToolboxForm: TToolboxForm;

implementation

{$R *.dfm}

uses
   Infrastructure, Types;

procedure TToolboxForm.sbNormalClick(Sender: TObject);
begin
    (Sender as TSpeedButton).Down := True;
    if Sender = sbNormal then
       GCustomCursor := crNormal
    else if Sender = sbRepeat then
       GCustomCursor := crRepeat
    else if Sender = sbWhile then
       GCustomCursor := crWhile
    else if Sender = sbIfElse then
       GCustomCursor := crIfElse
    else if Sender = sbIf then
       GCustomCursor := crIf
    else if Sender = sbFor then
       GCustomCursor := crFor
    else if Sender = sbInstr then
       GCustomCursor := crInstr
    else if Sender = sbMultiInstr then
       GCustomCursor := crMultiInstr
    else if Sender = sbInput then
       GCustomCursor := crInput
    else if Sender = sbOutput then
       GCustomCursor := crOutput
    else if Sender = sbFuncCall then
       GCustomCursor := crFuncCall
    else if Sender = sbCase then
       GCustomCursor := crCase
    else if Sender = sbReturn then
       GCustomCursor := crReturn
    else if Sender = sbText then
       GCustomCursor := crText
    else if Sender = sbFolder then
       GCustomCursor := crFolder;
end;

procedure TToolboxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   GCustomCursor := crNormal;
   sbNormal.Down := True;
end;

procedure TToolboxForm.FormCreate(Sender: TObject);
begin
   Left := Screen.Width - Width;
end;

procedure TToolboxForm.ResetForm;
begin
   sbNormal.Down := True;
   Close;
end;

end.

