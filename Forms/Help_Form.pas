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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Base_Form;

type

  THelpForm = class(TBaseForm)
    memHelp: TMemo;
    procedure Localize(const AList: TStringList); override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HelpForm: THelpForm;

implementation

uses
   ApplicationCommon, StrUtils;

{$R *.dfm}

procedure THelpForm.Localize(const AList: TStringList);
var
   i: integer;
   lText: string;
begin
   lText := '';
   for i := 1 to 28 do
      lText := lText + ' ' + AList.Values['EditorHelp' + IntToStr(i)] + CRLF;
   memHelp.Text := AnsiReplaceText(lText, ' ##', StringOfChar('-', 55));
   inherited Localize(AList);
end;

end.
