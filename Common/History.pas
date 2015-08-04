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
   Menus, Classes;

const
   HISTORY_SIZE = 10;
   KEY_HISTORY = 'HistoryEntry';

type

   THistoryMenu = class(TObject)
      private
         FList: array [1..HISTORY_SIZE] of string;
         FMenuList: array [1..HISTORY_SIZE] of TMenuItem;
         procedure ResetList;
         procedure ResetMenuList;
      public
         constructor Create(AParentItem: TMenuItem; AOnClick: TNotifyEvent);
         procedure Add(const AEntry: string);
         procedure Save;
         procedure Load;
   end;

implementation

uses
   Registry, SysUtils, ApplicationCommon;

constructor THistoryMenu.Create(AParentItem: TMenuItem; AOnClick: TNotifyEvent);
var
   i: integer;
begin
   inherited Create;
   for i := 1 to Length(FMenuList) do
   begin
      FMenuList[i] := TMenuItem.Create(AParentItem);
      FMenuList[i].OnClick := AOnClick;
   end;
   AParentItem.Add(FMenuList);
   ResetList;
   ResetMenuList;
end;

procedure THistoryMenu.Load;
var
   lReg: TRegistry;
   i: integer;
begin
   lReg := TRegistry.Create;
   try
      if lReg.OpenKeyReadOnly(REGISTRY_KEY) then
      begin
         for i := Length(FList) downto 1 do
         begin
            if lReg.ValueExists(KEY_HISTORY + IntToStr(i)) then
               Add(lReg.ReadString(KEY_HISTORY + IntToStr(i)));
         end;
      end;
   finally
      lReg.Free;
   end;
end;

procedure THistoryMenu.ResetList;
var
   i: integer;
begin
   for i := 1 to Length(FList) do
      FList[i] := '';
end;

procedure THistoryMenu.ResetMenuList;
var
   i: integer;
begin
   for i := 1 to Length(FMenuList) do
   begin
      FMenuList[i].Caption := '';
      FMenuList[i].Visible := false;
   end;
end;

procedure THistoryMenu.Add(const AEntry: string);
var
   i, a, len: integer;
begin
   if FileExists(AEntry) then
   begin
      len := Length(FList);
      for i := 1 to len do
      begin
         if AnsiSameText(FList[i], AEntry) then
         begin
            for a := i to len-1 do
               FList[a] := FList[a+1];
         end;
      end;
      for i := len downto 2 do
         FList[i] := FList[i-1];
      FList[1] := AEntry;
      ResetMenuList;
      a := 1;
      for i := 1 to len do
      begin
         if FileExists(FList[i]) then
         begin
            FMenuList[a].Caption := FList[i];
            FMenuList[a].Visible := true;
            a := a + 1;
         end;
      end;
   end;
end;

procedure THistoryMenu.Save;
var
   lReg: TRegistry;
   i, a: integer;
begin
   a := 1;
   lReg := TRegistry.Create;
   try
      if lReg.OpenKey(REGISTRY_KEY, true) then
      begin
         for i := 1 to Length(FList) do
         begin
            if FileExists(FList[i]) then
            begin
               lReg.WriteString(KEY_HISTORY + IntToStr(a), FList[i]);
               a := a + 1;
            end;
         end;
      end;
   finally
      lReg.Free;
   end;
end;

end.