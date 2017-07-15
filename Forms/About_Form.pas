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
    function ExtractVersion: string;
  public
    function GetVersion: string;
  end;

var
  AboutForm: TAboutForm;

implementation

uses
   WinApi.Windows, System.SysUtils, ApplicationCommon, ShellAPI;

const
   DELPHI_LINK  = 'www.embarcadero.com/products/delphi';
   SYNEDIT_LINK = 'synedit.sourceforge.net';
   IcXML_LINK   = 'www.omnixml.com';
   PROJECT_LINK = 'github.com/albireo77/devflowcharter';

{$R *.dfm}

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
   Close;
end;

function TAboutForm.ExtractVersion: string;
var
   s: string;
   vMajor, vMinor, vRelease, vBuild: integer;
   n, hnd: DWORD;
   buf: TBytes;
   value: PVSFixedFileInfo;
begin
   result := '';
   s := Application.ExeName;
   n := GetFileVersionInfoSize(PChar(s), hnd);
   if n > 0 then
   begin
      SetLength(buf, n);
      if GetFileVersionInfo(PWideChar(s), 0, n, buf) and VerQueryValue(buf, '\', Pointer(value), n) then
      begin
         vMajor := LongRec(value.dwFileVersionMS).Hi;
         vMinor := LongRec(value.dwFileVersionMS).Lo;
         vRelease := LongRec(value.dwFileVersionLS).Hi;
         vBuild := LongRec(value.dwFileVersionLS).Lo;
         result := Format('%d%s%d%s%d%s%d', [vMajor,
                                             VER_NUMBER_DELIM,
                                             vMinor,
                                             VER_NUMBER_DELIM,
                                             vRelease,
                                             VER_NUMBER_DELIM,
                                             vBuild]);
      end;
      buf := nil;
   end;
end;

function TAboutForm.GetVersion: string;
begin
   result := FVersion;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
   FVersion := ExtractVersion;
   imDelphi.Hint := DELPHI_LINK;
   imSynEdit.Hint := SYNEDIT_LINK;
   lblXML.Hint := IcXML_LINK;
   lblProjectLink.Caption := PROJECT_LINK;
   lblProjectLink.Hint := PROJECT_LINK;
   lblInfo.Caption := ' This program is freeware and released under the'#13#10'                GNU General Public License.'#13#10#13#10'    Copyright(C) 2006-2017 The devFlowcharter'#13#10'                             project';
   lblInfo1.Caption := '                   ' + PROGRAM_NAME + CRLF +
                       'The easiest way from flowchart to program!' + CRLF +
                       '                Version: ' + FVersion;
end;

procedure TAboutForm.imDelphiClick(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('http:\\' + TControl(Sender).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
