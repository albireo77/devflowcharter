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



unit About_Form;

interface

uses
   Vcl.StdCtrls, Vcl.Imaging.jpeg, System.Classes, Vcl.ExtCtrls, Base_Form, Vcl.Controls;

type

  TAboutForm = class(TBaseForm)
    btnOK: TButton;
    pnlInfo: TPanel;
    imDelphi,
    imSynEdit: TImage;
    lblXML,
    lblProjectLink,
    lblInfo1,
    lblInfo2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imDelphiClick(Sender: TObject);
  end;

var
  AboutForm: TAboutForm;

implementation

uses
   WinApi.Windows, System.SysUtils, Constants, ShellAPI, Infrastructure;

{$R *.dfm}

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
   Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
const
   LABEL_1 = '''
                      %s
   The easiest way from flowchart to program!
                Version: %s (win%d)
   ''';
   LABEL_2 = '''
    This program is freeware and released under the
                   GNU General Public License.

          The %s project (2006-2024)
   ''';
   WIN_PLATFORM = {$IFDEF WIN32}32{$ELSE}64{$ENDIF};
begin
   lblInfo1.Caption := Format(LABEL_1, [PROGRAM_NAME, TInfra.AppVersion, WIN_PLATFORM]);
   lblInfo2.Caption := Format(LABEL_2, [PROGRAM_NAME]);
end;

procedure TAboutForm.imDelphiClick(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('http://' + TControl(Sender).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
