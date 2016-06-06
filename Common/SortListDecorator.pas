unit SortListDecorator;

interface

uses
   Classes, Contnrs;

type

   TSortCompareMethod = function (Ptr, Ptr2: TObject; SortArg: integer): integer of object;

   { TSortListDecorator is a decorator class for TObjectList that can override its Sort method.
     It allows comparing function to be an object's method and pass additional argument of integer type. }

   TSortListDecorator = class
   private
      FWrappedList: TObjectList;
      FSortArg: integer;
      procedure QuickSort(L, R: integer; SCompare: TSortCompareMethod);
      function FDefaultCompareMethod(AItem1, AItem2: TObject; ASortType: integer): integer;
   public
      constructor Create(AListToWrap: TObjectList; ASortArg: integer);
      procedure Sort(ACompareMethod: TSortCompareMethod = nil);
      property WrappedList: TObjectList read FWrappedList;
   end;

implementation

uses
   CommonInterfaces, SysUtils;

constructor TSortListDecorator.Create(AListToWrap: TObjectList; ASortArg: integer);
begin
  FWrappedList := AListToWrap;
  FSortArg := ASortArg;
end;

function TSortListDecorator.FDefaultCompareMethod(AItem1, AItem2: TObject; ASortType: integer): integer;
var
   lSortObj: ISortable;
begin
   result := 1;
   if Supports(AItem1, ISortable, lSortObj) then
      result := lSortObj.GetSortValue(ASortType);
   if Supports(AItem2, ISortable, lSortObj) then
      result := result - lSortObj.GetSortValue(ASortType);
end;

procedure TSortListDecorator.QuickSort(L, R: integer; SCompare: TSortCompareMethod);
var
  I, J: integer;
  P, T: TObject;
begin
  repeat
    I := L;
    J := R;
    P := FWrappedList.Items[(L+R) shr 1];
    repeat
      while SCompare(FWrappedList.Items[I], P, FSortArg) < 0 do
        Inc(I);
      while SCompare(FWrappedList.Items[J], P, FSortArg) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := FWrappedList.Items[I];
        FWrappedList.Items[I] := FWrappedList.Items[J];
        FWrappedList.Items[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TSortListDecorator.Sort(ACompareMethod: TSortCompareMethod = nil);
begin
  if not Assigned(ACompareMethod) then
     ACompareMethod := FDefaultCompareMethod;
  if (FWrappedList <> nil) and (FWrappedList.Count > 1) then
    QuickSort(0, FWrappedList.Count-1, ACompareMethod);
end;

end.