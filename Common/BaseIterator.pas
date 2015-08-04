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
   Classes, CommonInterfaces;

type

   TBaseIterator = class(TInterfacedObject, IIterator)
      private
         idx: integer;
      protected
         FArray: array of TObject;
         constructor Create;
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

constructor TBaseIterator.Create;
begin
   inherited Create;
   idx := -1;
end;

destructor TBaseIterator.Destroy;
begin
   FArray := nil;
   inherited Destroy;
end;

function TBaseIterator.HasNext: boolean;
begin
   result := (idx+1 >= 0) and (idx+1 < Count);
end;

function TBaseIterator.Next: TObject;
begin
   result := nil;
   if HasNext then
   begin
      Inc(idx);
      result := FArray[idx];
   end;
end;

function TBaseIterator.GetCount: integer;
begin
   result := Length(FArray);
end;

function TBaseIterator.GetObjectIndex(const AObject: TObject): integer;
var
   i: integer;
begin
   result := -1;
   for i := 0 to Count-1 do
   begin
      if FArray[i] = AObject then
      begin
         result := i;
         break;
      end;
   end;
end;

procedure TBaseIterator.Reverse;
var
   lBuf: TObject;
   i, lLength: integer;
begin
   lLength := Count;
   for i := 0 to (lLength div 2)-1 do
   begin
      lBuf := FArray[i];
      FArray[i] := FArray[lLength-i-1];
      FArray[lLength-i-1] := lBuf;
   end;
end;

end.
