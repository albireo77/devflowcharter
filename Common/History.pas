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
   KEY_HISTORY = 'HistoryEntry';

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
   System.Win.Registry, System.SysUtils, ApplicationCommon;

constructor THistoryMenu.Create(AParentMenu: TMenuItem; AOnClick: TNotifyEvent);
begin
   inherited Create;
   FParentMenu := AParentMenu;
   FOnClick := AOnClick;
end;

procedure THistoryMenu.Load;
var
   reg: TRegistry;
   i: integer;
   fileKey: string;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
         for i := HISTORY_SIZE-1 downto 0 do
         begin
            fileKey := KEY_HISTORY + IntToStr(i);
            if reg.ValueExists(fileKey) then
               AddFile(reg.ReadString(fileKey));
         end;
      end;
   finally
      reg.Free;
   end;
end;

procedure THistoryMenu.AddFile(const AFilePath: string);
var
   menuItem: TMenuItem;
begin
   if FileExists(AFilePath) then
   begin
      menuItem := FParentMenu.Find(AFilePath);
      if menuItem <> nil then
         FParentMenu.Remove(menuItem)
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
var
   reg: TRegistry;
   i: integer;
begin
   reg := TRegistry.Create;
   try
      if reg.OpenKey(REGISTRY_KEY, true) then
      begin
         for i := 0 to FParentMenu.Count-1 do
            reg.WriteString(KEY_HISTORY + IntToStr(i), FParentMenu[i].Caption);
      end;
   finally
      reg.Free;
   end;
end;

end.