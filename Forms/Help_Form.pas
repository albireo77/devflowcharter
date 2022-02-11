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



unit Help_Form;

interface

uses
  System.Classes, Vcl.StdCtrls, Vcl.Controls, Base_Form;

type

  THelpForm = class(TBaseForm)
    memHelp: TMemo;
    procedure Localize(AList: TStringList); override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HelpForm: THelpForm;

implementation

uses
   System.SysUtils, System.StrUtils, Constants;

{$R *.dfm}

procedure THelpForm.Localize(AList: TStringList);
begin
   var txt := '';
   for var i := 1 to 28 do
      txt := txt + ' ' + AList.Values['EditorHelp' + i.ToString] + sLineBreak;
   memHelp.Text := ReplaceText(txt, ' ' + LB_PHOLDER2, StringOfChar('-', 55));
   inherited Localize(AList);
end;

end.
