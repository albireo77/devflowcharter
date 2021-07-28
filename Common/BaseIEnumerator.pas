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



unit BaseIEnumerator;

interface

uses
   Generics.Collections;

type

   TBaseIEnumerator<I: IInterface> = class(TInterfacedObject, IEnumerator<I>, IEnumerator)
      private
         FList: TList<I>;
         FIndex: integer;
         function InListRange: boolean;
      protected
         constructor Create(AList: TList<I>); overload;
         function MoveNext: Boolean;
         procedure Reset;
         function GenericGetCurrent: I;
         function GetCurrent: TObject;
         function IEnumerator<I>.GetCurrent = GenericGetCurrent;
      public
         destructor Destroy; override;
   end;

   TIEnumeratorFactory<I: IInterface> = class(TInterfacedObject, IEnumerable<I>, IEnumerable)
     private
        FInstance: IEnumerator<I>;
     public
        constructor Create(AList: TList<I>); overload;
        function GetEnumerator: IEnumerator;
        function GenericGetEnumerator: IEnumerator<I>;
        function IEnumerable<I>.GetEnumerator = GenericGetEnumerator;
   end;

implementation

uses
   System.Classes;

constructor TBaseIEnumerator<I>.Create(AList: TList<I>);
begin
   inherited Create;
   FList := AList;
   FIndex := -1;
end;

destructor TBaseIEnumerator<I>.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

function TBaseIEnumerator<I>.InListRange: boolean;
begin
   result := (FIndex >= 0) and (FList <> nil) and (FIndex < FList.Count);
end;

function TBaseIEnumerator<I>.MoveNext: boolean;
begin
   Inc(FIndex);
   result := InListRange;
end;

function TBaseIEnumerator<I>.GetCurrent: TObject;
begin
   result := nil;
end;

procedure TBaseIEnumerator<I>.Reset;
begin
   FIndex := -1;
end;

function TBaseIEnumerator<I>.GenericGetCurrent: I;
begin
   result := nil;
   if InListRange then
      result := FList[FIndex];
end;

constructor TIEnumeratorFactory<I>.Create(AList: TList<I>);
begin
   inherited Create;
   FInstance := TBaseIEnumerator<I>.Create(AList);
end;

function TIEnumeratorFactory<I>.GetEnumerator: IEnumerator;
begin
   result := GenericGetEnumerator;
end;

function TIEnumeratorFactory<I>.GenericGetEnumerator: IEnumerator<I>;
begin
   result := FInstance;
end;

end.
