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



unit BaseEnumerator;

interface

uses
   Generics.Collections;

type

   TCustomEnumerator<T: class> = class(TInterfacedObject, IEnumerator<T>, IEnumerator)
      private
         FList: TList<T>;
         FIndex: integer;
         function InListRange: boolean;
      protected
         constructor Create(AList: TList<T>); overload;
         destructor Destroy; override;
         function MoveNext: Boolean;
         procedure Reset;
         function GenericGetCurrent: T;
         function GetCurrent: TObject;
         function IEnumerator<T>.GetCurrent = GenericGetCurrent;
   end;

   TEnumeratorFactory<T: class> = class(TInterfacedObject, IEnumerable<T>, IEnumerable)
     private
        FList: TList<T>;
        FInstance: IEnumerator<T>;
     public
        constructor Create(AList: TList<T>); overload;
        function GetEnumerator: IEnumerator;
        function GenericGetEnumerator: IEnumerator<T>;
        function IEnumerable<T>.GetEnumerator = GenericGetEnumerator;
   end;

implementation

uses
   System.Classes;

constructor TCustomEnumerator<T>.Create(AList: TList<T>);
begin
   inherited Create;
   FList := AList;
   FIndex := -1;
end;

destructor TCustomEnumerator<T>.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

function TCustomEnumerator<T>.InListRange: boolean;
begin
   result := (FIndex >= 0) and (FList <> nil) and (FIndex < FList.Count);
end;

function TCustomEnumerator<T>.MoveNext: boolean;
begin
   Inc(FIndex);
   result := InListRange;
end;

function TCustomEnumerator<T>.GetCurrent: TObject;
begin
   result := nil;
   if InListRange then
      result := FList[FIndex];
end;

procedure TCustomEnumerator<T>.Reset;
begin
   FIndex := -1;
end;

function TCustomEnumerator<T>.GenericGetCurrent: T;
begin
   result := T(GetCurrent);
end;

constructor TEnumeratorFactory<T>.Create(AList: TList<T>);
begin
   inherited Create;
   FList := AList;
end;

function TEnumeratorFactory<T>.GetEnumerator: IEnumerator;
begin
   result := GenericGetEnumerator;
end;

function TEnumeratorFactory<T>.GenericGetEnumerator: IEnumerator<T>;
begin
   if FInstance = nil then
      FInstance := TCustomEnumerator<T>.Create(FList);
   result := FInstance;
end;

end.
