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

unit XMLUtils;

interface

uses
   MSXML2_TLB, System.Classes;

function AppendCDATA(ATag: IXMLDOMElement; const AValue: string): IXMLDOMCDATASection;
function AppendCDATAChild(ATag: IXMLDOMElement; const AChildName, AValue: string): IXMLDOMCDATASection;
function AppendTag(ATag: IXMLDOMElement; AName: string): IXMLDOMElement;
procedure GetNodesText(ANode: IXMLDOMNode; const AName: string; AList: TStringList);
function GetNodeAttrStr(ANode: IXMLDOMNode; const AName, ADefault: string): string;
function GetNodeAttrBool(ANode: IXMLDOMNode; const AName: string; ADefault: boolean): boolean;
function GetNodeAttrInt(ANode: IXMLDOMNode; const AName: string; ADefault: integer): integer;
function FindChildTag(ANode: IXMLDOMNode; const AName: string): IXMLDOMElement;
function GetChildTextBool(ATag: IXMLDOMElement; const AChildName: string; ADefault: boolean): boolean;
function GetChildTextInt(ATag: IXMLDOMElement; const AChildName: string; ADefault: integer): integer;
function GetChildTextStr(ATag: IXMLDOMElement; const AChildName, ADefault: string): string; overload;
function GetChildTextStr(ATag: IXMLDOMElement; const AChildName: string): string; overload;


implementation

uses
   System.SysUtils;

function GetChildTextStr(ATag: IXMLDOMElement; const AChildName, ADefault: string): string;
begin
   result := ADefault;
   if (ATag <> nil) and not AChildName.IsEmpty then
   begin
      var node := ATag.SelectSingleNode(AChildName);
      if node <> nil then
         result := node.Text;
   end;
end;

function GetChildTextStr(ATag: IXMLDOMElement; const AChildName: string): string;
begin
   result := GetChildTextStr(ATag, AChildName, '');
end;

function GetChildTextInt(ATag: IXMLDOMElement; const AChildName: string; ADefault: integer): integer;
begin
   var s := GetChildTextStr(ATag, AChildName);
   if s.isEmpty then
      result := ADefault
   else
      result := StrToInt(s);
end;

function GetChildTextBool(ATag: IXMLDOMElement; const AChildName: string; ADefault: boolean): boolean;
begin
   var s := GetChildTextStr(ATag, AChildName);
   if s.isEmpty then
      result := ADefault
   else
      result := StrToBool(s);
end;

function GetNodeAttrStr(ANode: IXMLDOMNode; const AName, ADefault: string): string;
begin
   result := ADefault;
   if (ANode <> nil) and (ANode.Attributes <> nil) and not AName.IsEmpty then
   begin
      var node := ANode.Attributes.GetNamedItem(AName);
      if node <> nil then
         result := node.Text;
   end;
end;

function GetNodeAttrBool(ANode: IXMLDOMNode; const AName: string; ADefault: boolean): boolean;
begin
   result := ADefault;
   var s := GetNodeAttrStr(ANode, AName, '');
   if not s.IsEmpty then
      result := StrToBool(s);
end;

function GetNodeAttrInt(ANode: IXMLDOMNode; const AName: string; ADefault: integer): integer;
begin
   result := ADefault;
   var s := GetNodeAttrStr(ANode, AName, '');
   if not s.IsEmpty then
      result := StrToInt(s);
end;

procedure GetNodesText(ANode: IXMLDOMNode; const AName: string; AList: TStringList);
begin
   AList.Clear;
   if (ANode <> nil) and not AName.IsEmpty then
   begin
      var nodes := ANode.SelectNodes(AName);
      for var i := 0 to nodes.Length-1 do
         AList.Add(Trim(nodes.Item[i].Text));
   end;
end;

function FindTag(ANode: IXMLDOMNode; const ANodeName: string): IXMLDOMElement;
begin
   result := nil;
   while ANode <> nil do
   begin
      if ANode.NodeName = ANodeName then
      begin
         result := ANode as IXMLDOMElement;
         break;
      end;
      ANode := ANode.NextSibling;
   end;
end;

function FindChildTag(ANode: IXMLDOMNode; const AName: string): IXMLDOMElement;
begin
   result := nil;
   if ANode <> nil then
      result := FindTag(ANode.FirstChild, AName);
end;

function AppendTag(ATag: IXMLDOMElement; AName: string): IXMLDOMElement;
begin
   result := nil;
   if (ATag <> nil) and not AName.IsEmpty  then
   begin
      result := ATag.OwnerDocument.CreateElement(AName);
      ATag.AppendChild(result);
   end;
end;

function AppendCDATAChild(ATag: IXMLDOMElement; const AChildName, AValue: string): IXMLDOMCDATASection;
begin
   result := nil;
   var tag := AppendTag(ATag, AChildName);
   if tag <> nil then
   begin
      result := tag.OwnerDocument.createCDATASection(AValue);
      tag.AppendChild(result);
   end;
end;

function AppendCDATA(ATag: IXMLDOMElement; const AValue: string): IXMLDOMCDATASection;
begin
   result := nil;
   if ATag <> nil then
   begin
      result := ATag.OwnerDocument.CreateCDATASection(AValue);
      ATag.AppendChild(result);
   end;

end;


end.





