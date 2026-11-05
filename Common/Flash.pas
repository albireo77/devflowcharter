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

{ This unit highlights TCustomEdits (swaps their font and background colors for a moment) }
{ while switching nodes on TreeView_Form.tvFlowChart tree and then focuses them. }
{ It runs on a timer, so the application is not blocked while edit is highlighted. }

unit Flash;

interface

uses
   Vcl.ExtCtrls, Vcl.Graphics, System.Classes, Types;

type

   TFlash = class(TComponent)
   private
      FFocusInfo: TFocusInfo;
      FHasActiveControl: boolean;
      FColor,
      FFontColor: TColor;
      FTimer: TTimer;
      procedure OnTimer(Sender: TObject);
      procedure Finish(AFocus: boolean);
      procedure Focus;
   protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public
      constructor Create(const AFocusInfo: TFocusInfo); reintroduce;
      class procedure Start(const AFocusInfo: TFocusInfo);
   end;


implementation

uses
   Vcl.StdCtrls;

const
   FLASH_TIME = 400;

type
   TCustomEditHack = class(TCustomEdit);

var
   GFlash: TFlash;   // highlight in progress; only one at a time, so that edit colors are always restored

class procedure TFlash.Start(const AFocusInfo: TFocusInfo);
begin
   if GFlash <> nil then
   begin
      var flash := GFlash;
      flash.Finish(False);
      flash.Free;
   end;
   if AFocusInfo.FocusEdit <> nil then
      GFlash := TFlash.Create(AFocusInfo);
end;

constructor TFlash.Create(const AFocusInfo: TFocusInfo);
begin
   inherited Create(nil);
   FFocusInfo := AFocusInfo;
   FHasActiveControl := FFocusInfo.ActiveControl <> nil;
   // controls may be destroyed before highlight is over (e.g. block is removed)
   FFocusInfo.FocusEdit.FreeNotification(Self);
   if FFocusInfo.FocusEditForm <> nil then
      FFocusInfo.FocusEditForm.FreeNotification(Self);
   if FFocusInfo.ActiveControl <> nil then
      FFocusInfo.ActiveControl.FreeNotification(Self);
   var edit := TCustomEditHack(FFocusInfo.FocusEdit);
   FColor := edit.Color;
   FFontColor := edit.Font.Color;
   edit.Font.Color := FColor;
   edit.Color := FFontColor;
   edit.Repaint;
   FTimer := TTimer.Create(Self);
   FTimer.Interval := FLASH_TIME;
   FTimer.OnTimer := OnTimer;
end;

procedure TFlash.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited Notification(AComponent, Operation);
   if Operation = opRemove then
   begin
      if AComponent = FFocusInfo.FocusEdit then
         FFocusInfo.FocusEdit := nil;
      if AComponent = FFocusInfo.FocusEditForm then
         FFocusInfo.FocusEditForm := nil;
      if AComponent = FFocusInfo.ActiveControl then
         FFocusInfo.ActiveControl := nil;
   end;
end;

procedure TFlash.OnTimer(Sender: TObject);
begin
   Finish(True);
   // timer must not be freed inside its own event
   TThread.ForceQueue(nil, procedure
                           begin
                              Free;
                           end);
end;

procedure TFlash.Finish(AFocus: boolean);
begin
   FTimer.Enabled := False;
   if GFlash = Self then
      GFlash := nil;
   var edit := TCustomEditHack(FFocusInfo.FocusEdit);
   if edit <> nil then
   begin
      edit.Color := FColor;
      edit.Font.Color := FFontColor;
      edit.Repaint;
      if AFocus then
         Focus;
   end;
end;

procedure TFlash.Focus;
begin
   var edit := TCustomEditHack(FFocusInfo.FocusEdit);
   if not FHasActiveControl then
   begin
      if FFocusInfo.SelStart >= 0 then
         edit.SelStart := FFocusInfo.SelStart;
      edit.SelLength := Length(FFocusInfo.SelText);
      if edit.CanFocus then
         edit.SetFocus;
   end
   else if (FFocusInfo.ActiveControl <> nil) and FFocusInfo.ActiveControl.CanFocus then
   begin
      FFocusInfo.ActiveControl.SetFocus;
      if FFocusInfo.FocusEditForm <> nil then
      begin
         FFocusInfo.FocusEditForm.ActiveControl := FFocusInfo.FocusEdit;
         if Assigned(FFocusInfo.FocusEditCallBack) then
            FFocusInfo.FocusEditCallBack(FFocusInfo.FocusEdit);
      end;
   end;
end;

initialization

finalization
   GFlash.Free;

end.
