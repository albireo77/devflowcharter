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
   Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, System.Classes, Base_Form;

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
   System.SysUtils, SynEdit, Infrastructure;

procedure TGotoForm.FormShow(Sender: TObject);
begin
   edtNumber.SetFocus;
   edtNumber.Text := '1';
end;

procedure TGotoForm.btnGotoClick(Sender: TObject);
begin

   var codeEditor := TInfra.GetEditorForm.memCodeEditor;
   var gotoFlag := False;
   var line := 0;
   var i := 0;

   if rbLine.Checked then
   begin
      line := StrToIntDef(edtNumber.Text, 0);
      if line > 0 then
         gotoFlag := True;
      codeEditor.SetFocus;
      Close;
   end
   else if rbNextBookmark.Checked then
   begin
      line := codeEditor.Lines.Count;
      for i := 0 to codeEditor.Marks.Count-1 do
      begin
         var cLine := codeEditor.Marks[i].Line;
         if (cLine > codeEditor.CaretY) and (cLine <= line) then
         begin
            line := cLine;
            gotoFlag := True;
         end;
      end;
   end
   else if rbPrevBookmark.Checked then
   begin
      line := 1;
      for i := 0 to codeEditor.Marks.Count-1 do
      begin
         var cLine := codeEditor.Marks[i].Line;
         if (cLine < codeEditor.CaretY) and (cLine >= line) then
         begin
            line := cLine;
            gotoFlag := True;
         end;
      end;
   end;
   if gotoFlag then
      codeEditor.GotoLineAndCenter(line);

end;

end.
