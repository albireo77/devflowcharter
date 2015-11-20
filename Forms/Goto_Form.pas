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



unit Goto_Form;

interface

uses
   Controls, Forms, StdCtrls, ExtCtrls, SysUtils, Classes, Base_Form;

type

  TGotoForm = class(TBaseForm)
    pnlOptions: TPanel;
    rbLine: TRadioButton;
    rbNextBookmark: TRadioButton;
    edtNumber: TEdit;
    btnGoto: TButton;
    rbPrevBookmark: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure btnGotoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GotoForm: TGotoForm;

implementation

{$R *.dfm}

uses
   SynEdit, ApplicationCommon;

procedure TGotoForm.FormShow(Sender: TObject);
begin
   edtNumber.SetFocus;
   edtNumber.Text := '1';
end;

procedure TGotoForm.btnGotoClick(Sender: TObject);
var
   i, line: integer;
   goto_flag: boolean;
begin

   with TInfra.GetEditorForm.memCodeEditor do
   begin
      goto_flag := false;
      line := 0;
      if rbLine.Checked then
      begin
         line := StrToIntDef(edtNumber.Text, 0);
         if line > 0 then
            goto_flag := true;
         SetFocus;
         Self.Close;
      end
      else if rbNextBookmark.Checked then
      begin
         line := Lines.Count;
         for i := 0 to Marks.Count-1 do
         begin
            if (Marks[i].Line > CaretY) and (Marks[i].Line <= line) then
            begin
               line := Marks[i].Line;
               goto_flag := true;
            end;
         end;
      end
      else if rbPrevBookmark.Checked then
      begin
         line := 1;
         for i := 0 to Marks.Count-1 do
         begin
            if (Marks[i].Line < CaretY) and (Marks[i].Line >= line) then
            begin
               line := Marks[i].Line;
               goto_flag := true;
            end;
         end;
      end;
      if goto_flag then
         GotoLineAndCenter(line);
   end;

end;

end.
