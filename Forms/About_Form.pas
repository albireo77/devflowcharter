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
   Controls, Forms, StdCtrls, ExtCtrls, Graphics, Classes, Base_Form, jpeg;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure imDelphiClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
   Windows, SysUtils, ApplicationCommon, ShellAPI;

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

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i, cavb: 0..255;
begin
  if not TInfra.IsWin9x then
  begin
     cavb := AlphaBlendValue;
     for i := cavb downto 0 do
     begin
        AlphaBlendValue := i;
        Application.ProcessMessages;
     end;
     AlphaBlendValue := 230;
  end;
end;

procedure TAboutForm.FormCreate(Sender: TObject);

   function GetVersionInfo: string;
   var
      S: string;
      VMajor, VMinor, VRelease, VBuild: integer;
      n, Len: DWORD;
      Buf: PChar;
      Value: PVSFixedFileInfo;
      VerH, VerL: integer;
   begin
      S := Application.ExeName;
      n := GetFileVersionInfoSize(PChar(S), n);
      if n > 0 then
      begin
         Buf := AllocMem(n);
         try
            if GetFileVersionInfo(PChar(S), 0, n, Buf) and VerQueryValue(Buf, '\', Pointer(Value), Len) then
            begin
               VerH := Value.dwFileVersionMS;
               VerL := Value.dwFileVersionLS;
      	       VMajor := ($FFFF0000 and VerH) shr 16;
      	       VMinor := $0000FFFF and VerH;
      	       VRelease := ($FFFF0000 and VerL) shr 16;
      	       VBuild := $0000FFFF and VerL;
      	       result := Format('Version: %d.%d.%d (Build %d)', [VMajor, VMinor, VRelease, VBuild]);
            end;
         finally
            FreeMem(Buf, n);
         end;
      end;
   end;

begin
   imDelphi.Hint := DELPHI_LINK;
   imSynEdit.Hint := SYNEDIT_LINK;
   lblXML.Hint := IcXML_LINK;
   lblProjectLink.Caption := PROJECT_LINK;
   lblProjectLink.Hint := PROJECT_LINK;
   lblInfo.Caption := ' This program is freeware and released under the'#13#10'                GNU General Public License.'#13#10#13#10'    Copyright(C) 2006-2016 The devFlowcharter'#13#10'                             project';
   lblInfo1.Caption := '                   ' + PROGRAM_NAME + CRLF +
                       'The easiest way from flowchart to program!' + CRLF +
                       '             ' + GetVersionInfo;
end;

procedure TAboutForm.imDelphiClick(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('http:\\' + TControl(Sender).Hint), nil, nil, SW_SHOWNORMAL);
end;

end.
