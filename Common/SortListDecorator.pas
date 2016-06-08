unit SortListDecorator;

interface

uses
   Classes, Contnrs;

type

   TCompareMethod = function(AObject1, AObject2: TObject; ASortType: integer): integer of object;

   { TSortListDecorator is a decorator class for TObjectList and overrides its Sort method.
     It allows comparing function to be an object's method and pass additional argument of integer type. }

   TSortListDecorator = class
   private
      FWrapList: TObjectList;
      FSortArg: integer;
      procedure QuickSort(L, R: integer; ACompare: TCompareMethod);
      function FDefaultCompareMethod(AObject1, AObject2: TObject; ASortType: integer): integer;
   public
      constructor Create(AWrapList: TObjectList; ASortArg: integer);
      procedure Sort(ACompareMethod: TCompareMethod = nil);
      property WrapList: TObjectList read FWrapList;
   end;

implementation

uses
   CommonInterfaces, SysUtils;

constructor TSortListDecorator.Create(AWrapList: TObjectList; ASortArg: integer);
begin
  FWrapList := AWrapList;
  FSortArg := ASortArg;
end;

function TSortListDecorator.FDefaultCompareMethod(AObject1, AObject2: TObject; ASortType: integer): integer;
var
   lSortObj: ISortable;
begin
   result := 1;
   if Supports(AObject1, ISortable, lSortObj) then
      result := lSortObj.GetSortValue(ASortType);
   if Supports(AObject2, ISortable, lSortObj) then
      result := result - lSortObj.GetSortValue(ASortType);
end;

procedure TSortListDecorator.QuickSort(L, R: integer; ACompare: TCompareMethod);
var
  I, J: integer;
  P, T: TObject;
begin
  repeat
    I := L;
    J := R;
    P := FWrapList.Items[(L+R) shr 1];
    repeat
      while ACompare(FWrapList.Items[I], P, FSortArg) < 0 do
        Inc(I);
      while ACompare(FWrapList.Items[J], P, FSortArg) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := FWrapList.Items[I];
        FWrapList.Items[I] := FWrapList.Items[J];
        FWrapList.Items[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, ACompare);
    L := I;
  until I >= R;
end;

procedure TSortListDecorator.Sort(ACompareMethod: TCompareMethod = nil);
begin
  if not Assigned(ACompareMethod) then
     ACompareMethod := FDefaultCompareMethod;
  if (FWrapList <> nil) and (FWrapList.Count > 1) then
    QuickSort(0, FWrapList.Count-1, ACompareMethod);
end;

end.