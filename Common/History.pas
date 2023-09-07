{
   Copyright (C) 2006 The devFlowcharter project.
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

{ This unit contains the implementation of history for recently opened files }

unit History;

interface

uses
   Vcl.Menus, System.Classes;

const
   HISTORY_SIZE = 10;
   HISTORY_SECTION = 'History';

type
   THistoryMenu = class(TObject)
      private
         FParentMenu: TMenuItem;
         FOnClick: TNotifyEvent;
      public
         constructor Create(AParentMenu: TMenuItem; AOnClick: TNotifyEvent);
         procedure AddFile(const AFilePath: string);
         procedure Save;
         procedure Load;
   end;

implementation

uses
   System.IniFiles, System.SysUtils, Infrastructure;

constructor THistoryMenu.Create(AParentMenu: TMenuItem; AOnClick: TNotifyEvent);
begin
   inherited Create;
   FParentMenu := AParentMenu;
   FOnClick := AOnClick;
end;

procedure THistoryMenu.AddFile(const AFilePath: string);
begin
   if FileExists(AFilePath) then
   begin
      var menuItem := FParentMenu.Find(AFilePath);
      if menuItem <> nil then
      begin
         if menuItem.MenuIndex = 0 then
            Exit;
         FParentMenu.Remove(menuItem);
      end
      else
      begin
         menuItem := TMenuItem.Create(FParentMenu);
         menuItem.OnClick := FOnClick;
         menuItem.Caption := AFilePath;
      end;
      FParentMenu.Insert(0, menuItem);
      if FParentMenu.Count > HISTORY_SIZE then
         FParentMenu[FParentMenu.Count-1].Free;
   end;
end;

procedure THistoryMenu.Save;
begin
   for var i := 0 to FParentMenu.Count-1 do
       GSettings.SettingsFile.WriteString(HISTORY_SECTION, i.ToString, FParentMenu[i].Caption);
end;

procedure THistoryMenu.Load;
begin
   for var i := HISTORY_SIZE-1 downto 0 do
       AddFile(GSettings.SettingsFile.ReadString(HISTORY_SECTION, i.ToString, ''));
end;

end.