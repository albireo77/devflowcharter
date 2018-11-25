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

unit StatementMemo;

interface

uses
  System.Classes, Vcl.StdCtrls, Vcl.Graphics, Vcl.Controls, Vcl.ComCtrls, CommonInterfaces,
  MemoEx, CommonTypes;

type

  TStatementMemo = class(TMemoEx, IFocusable)
     public
     { Public declarations }
        constructor Create(AOwner: TComponent); override;
        function RetrieveFocus(AInfo: TFocusInfo): boolean;
        function CanBeFocused: boolean;
        function GetFocusColor: TColor;
        function Remove(ANode: TTreeNode): boolean;
        function CanRemove: boolean;
        function IsBoldDesc: boolean;
  end;

implementation

uses
   Vcl.Forms, System.SysUtils, ApplicationCommon, Base_Block;

constructor TStatementMemo.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Color := GSettings.GetShapeColor(shpRectangle);
   Font.Color := GSettings.FontColor;
   Font.Name := GSettings.FlowchartFontName;
   DoubleBuffered := true;
   Anchors := [akRight, akLeft, akBottom, akTop];
end;

function TStatementMemo.RetrieveFocus(AInfo: TFocusInfo): boolean;
begin
   AInfo.FocusEdit := Self;
   if Parent is TBlock then
      result := TBlock(Parent).RetrieveFocus(AInfo)
   else
   begin
      if CanFocus then
         SetFocus;
      result := true;
   end;
end;

function TStatementMemo.CanBeFocused: boolean;
begin
   if Parent is TBlock then
      result := TBlock(Parent).CanBeFocused
   else
      result := CanFocus;
end;

function TStatementMemo.GetFocusColor: TColor;
begin
   if HasParent then
      result := Font.Color
   else
      result := OK_COLOR;
end;

function TStatementMemo.Remove(ANode: TTreeNode): boolean;
begin
   result := CanRemove;
   if result then
   begin
      if (ANode <> nil) and (ANode.Index < Lines.Count) and ANode.Text.StartsWith(Lines[ANode.Index]) then
         Lines.Delete(ANode.Index)
      else
         result := false;
      if (Lines.Count = 0) and (Parent is TBlock) then
         result := TBlock(Parent).Remove(ANode);
   end;
end;

function TStatementMemo.CanRemove: boolean;
begin
   result := HasParent;
end;

function TStatementMemo.IsBoldDesc: boolean;
begin
   result := false;
end;

end.
