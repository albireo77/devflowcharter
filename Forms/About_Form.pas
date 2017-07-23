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
   Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Imaging.jpeg, System.Classes, Vcl.ExtCtrls,
   Base_Form;

type

  TAboutForm = class(TBaseForm)
    btnOK: TButton;
    imDelphi: TImage;
    imSynEdit: TImage;
    lblXML: TLabel;
    lblProjectLink: TLabel;
    lblInfo: TLabel;
    pnlInfo: TPanel;
    lblInfo1: TLabel;
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
   WinApi.Windows, System.SysUtils, ApplicationCommon, ShellAPI;

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
   result := 'unknown';
   s := Application.ExeName;
   n := GetFileVersionInfoSize(PChar(s), hnd);
   if n > 0 then
   begin
      SetLength(buf, n);
      if GetFileVersionInfo(PWideChar(s), 0, n, buf) and VerQueryValue(buf, '\', Pointer(value), n) then
         result := Format('%d%s%d%s%d%s%d', [LongRec(value.dwFileVersionMS).Hi, VER_NUMBER_DELIM,
                                             LongRec(value.dwFileVersionMS).Lo, VER_NUMBER_DELIM,
                                             LongRec(value.dwFileVersionLS).Hi, VER_NUMBER_DELIM,
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
   lb = sLineBreak;
begin
   FVersion := ExtractProgramVersion;
   lblInfo.Caption := Format(' This program is freeware and released under the%s                GNU General Public License.%s%s    Copyright(C) 2006-2017 The %s%s                             project', [lb, lb, lb, PROGRAM_NAME, lb]);
   lblInfo1.Caption := Format('                   %s%sThe easiest way from flowchart to program!%s                Version: %s', [PROGRAM_NAME, lb, lb, FVersion]);
end;

procedure TAboutForm.imDelphiClick(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('http://' + TControl(Sender).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
