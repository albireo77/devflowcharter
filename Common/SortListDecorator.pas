unit SortListDecorator;

interface

uses
   Generics.Collections;

type

   { TSortListDecorator is a decorator class for TObjectList and overrides its Sort method.
     It allows comparing function to be an object's method and pass additional argument of integer type. }

   TSortListDecorator<T: class> = class
   private
      FWrapList: TList<T>;
      FSortArg: integer;
      procedure QuickSort(L, R: integer);
      function FCompare(T1, T2: T; ASortType: integer): integer;
   public
      constructor Create(AWrapList: TList<T>; ASortArg: integer);
      procedure Sort;
   end;

implementation

uses
   System.SysUtils, System.Classes, CommonInterfaces;

constructor TSortListDecorator<T>.Create(AWrapList: TList<T>; ASortArg: integer);
begin
  FWrapList := AWrapList;
  FSortArg := ASortArg;
end;

function TSortListDecorator<T>.FCompare(T1, T2: T; ASortType: integer): integer;
var
   sortObj: ISortable;
begin
   result := 1;
   if Supports(T1, ISortable, sortObj) then
      result := sortObj.GetSortValue(ASortType);
   if Supports(T2, ISortable, sortObj) then
      result := result - sortObj.GetSortValue(ASortType);
end;

procedure TSortListDecorator<T>.QuickSort(L, R: integer);
var
  I, J: integer;
  K, U: T;
begin
  repeat
    I := L;
    J := R;
    K := FWrapList.Items[(L+R) shr 1];
    repeat
      while FCompare(FWrapList.Items[I], K, FSortArg) < 0 do
        Inc(I);
      while FCompare(FWrapList.Items[J], K, FSortArg) > 0 do
        Dec(J);
      if I <= J then
      begin
        U := FWrapList.Items[I];
        FWrapList.Items[I] := FWrapList.Items[J];
        FWrapList.Items[J] := U;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TSortListDecorator<T>.Sort;
begin
  if (FWrapList <> nil) and (FWrapList.Count > 1) then
    QuickSort(0, FWrapList.Count-1);
end;

end.