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



unit BaseIterator;

interface

uses
   Classes, CommonInterfaces, Contnrs;

type

   TBaseIterator = class(TInterfacedObject, IIterator)
      private
         idx: integer;
         FList: TObjectList;
      protected
         constructor Create(AList: TObjectList);
         function GetCount: integer;
      public
         property Count: integer read GetCount;
         destructor Destroy; override;
         function HasNext: boolean;
         function Next: TObject;
         function GetObjectIndex(const AObject: TObject): integer;
         procedure Reverse;
   end;

implementation

constructor TBaseIterator.Create(AList: TObjectList);
begin
   inherited Create;
   FList := AList;
   idx := -1;
end;

destructor TBaseIterator.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

function TBaseIterator.HasNext: boolean;
begin
   result := (idx+1 >= 0) and (FList <> nil) and (idx+1 < FList.Count);
end;

function TBaseIterator.Next: TObject;
begin
   result := nil;
   if HasNext then
   begin
      Inc(idx);
      result := FList[idx];
   end;
end;

function TBaseIterator.GetCount: integer;
begin
   result := 0;
   if FList <> nil then
      result := FList.Count;
end;

function TBaseIterator.GetObjectIndex(const AObject: TObject): integer;
begin
   result := -1;
   if FList <> nil then
      result := FList.IndexOf(AObject);
end;

procedure TBaseIterator.Reverse;
var
   lObject: TObject;
   i: integer;
begin
   if (FList <> nil) and (FList.Count > 1) then
   begin
      for i := 0 to (FList.Count div 2)-1 do
      begin
         lObject := FList[i];
         FList[i] := FList[FList.Count-i-1];
         FList[FList.Count-i-1] := lObject;
      end;
   end;
end;

end.
