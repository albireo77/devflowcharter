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
  private
    FVersion: string;
    function ExtractProgramVersion: string;
  public
    function GetProgramVersion: string;
  end;

var
  AboutForm: TAboutForm;

implementation

uses
   WinApi.Windows, System.SysUtils, Vcl.Forms, Constants, ShellAPI;

{$R *.dfm}

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
   Close;
end;

function TAboutForm.ExtractProgramVersion: string;
var
   s: string;
   n, hnd: DWORD;
   buf: TBytes;
   value: PVSFixedFileInfo;
begin
   result := UNKNOWN_VERSION;
   s := Application.ExeName;
   n := GetFileVersionInfoSize(PChar(s), hnd);
   if n > 0 then
   begin
      SetLength(buf, n);
      if GetFileVersionInfo(PWideChar(s), 0, n, buf) and VerQueryValue(buf, '\', Pointer(value), n) then
         result := Format('%d%s%d%s%d%s%d', [LongRec(value.dwFileVersionMS).Hi, VERSION_NUMBER_SEP,
                                             LongRec(value.dwFileVersionMS).Lo, VERSION_NUMBER_SEP,
                                             LongRec(value.dwFileVersionLS).Hi, VERSION_NUMBER_SEP,
                                             LongRec(value.dwFileVersionLS).Lo]);
      buf := nil;
   end;
end;

function TAboutForm.GetProgramVersion: string;
begin
   result := FVersion;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
const
   LABEL_1 = '                   %s%sThe easiest way from flowchart to program!%s             Version: %s (x%d)';
   LABEL_2 = ' This program is freeware and released under the%s                GNU General Public License.%s%s       The %s project (2006-2022)';
{$IFDEF WIN32}
   winXX = 32;
{$ENDIF}
{$IFDEF WIN64}
   winXX = 64;
{$ENDIF}
begin
   FVersion := ExtractProgramVersion;
   lblInfo1.Caption := Format(LABEL_1, [PROGRAM_NAME, sLineBreak, sLineBreak, FVersion, winXX]);
   lblInfo2.Caption := Format(LABEL_2, [sLineBreak, sLineBreak, sLineBreak, PROGRAM_NAME]);
end;

procedure TAboutForm.imDelphiClick(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('http://' + TControl(Sender).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
