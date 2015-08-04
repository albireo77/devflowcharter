unit LCS;

interface

uses
   Windows, SysUtils, Classes, Types;

type
   TtdLCSDir = (ldNorth, ldNorthWest, ldWest);
   PtdLCSData = ^TtdLCSData;

   TtdLCSData = packed record
      ldLen: integer;
      ldPrev: TtdLCSDir;
   end;

   TtdLCSMatrix = class
      private
         FCols: integer;
         FMatrix: TList;
         FRows: integer;
      protected
         function mxGetItem(aRow, aCol: integer): PtdLCSData;
         procedure mxSetItem(aRow, aCol: integer; aValue: PtdLCSData);
      public
         constructor Create(aRowCount, aColCount: integer);
         destructor Destroy; override;
         procedure Clear;
         property Items[aRow, aCol: integer]: PtdLCSData read mxGetItem write mxSetItem; default;
         property RowCount: integer read FRows;
         property ColCount: integer read FCols;
   end;

   TStringListLCS = class
      private
         FFromList,
         FToList: TStringList;
         FMatrix: TtdLCSMatrix;
      protected
         function slGetCell(aFromInx, aToInx: integer): integer;
         procedure slWriteChange(var F: Text; aFromInx, aToInx: integer);
      public
         constructor Create(aFromList, aToList: TStringList);
         destructor Destroy; override;
         procedure CommitDiffToDestList;
   end;

implementation

uses
   StrUtils;

function TtdLCSMatrix.mxGetItem(aRow, aCol: integer): PtdLCSData;
begin
   if not ((0 <= aRow) and (aRow < RowCount) and (0 <= aCol) and (aCol < ColCount)) then
      raise Exception.Create('TtdLCSMatrix.mxGetItem: Row or column index out of bounds');
   result := PtdLCSData(TList(FMatrix.List^[aRow]).List^[aCol]);
end;

procedure TtdLCSMatrix.mxSetItem(aRow, aCol: integer; aValue: PtdLCSData);
begin
   if not ((0 <= aRow) and (aRow < RowCount) and (0 <= aCol) and (aCol < ColCount)) then
      raise Exception.Create('TtdLCSMatrix.mxSetItem: Row or column index out of bounds');
   TList(FMatrix.List^[aRow]).List^[aCol] := aValue;
end;

constructor TtdLCSMatrix.Create(aRowCount, aColCount: integer);
var
   Row: integer;
   ColList: TList;
begin
   {create the ancestor}
   inherited Create;
   {simple validation}
   Assert((aRowCount > 0) and (aColCount > 0), 'TtdLCSMatrix.Create: Invalid Row or column count');
   FRows := aRowCount;
   FCols := aColCount;
   {create the matrix: it'll be a TList of TLists in row order}
   FMatrix := TList.Create;
   FMatrix.Count := aRowCount;
   for Row := 0 to pred(aRowCount) do
   begin
      ColList := TList.Create;
      ColList.Count := aColCount;
      TList(FMatrix.List^[Row]) := ColList;
   end;
end;

destructor TtdLCSMatrix.Destroy;
var
   Row: integer;
begin
   if FMatrix <> nil then
   begin
      Clear;
      for Row := 0 to pred(FRows) do
         TList(FMatrix.List^[Row]).Free;
      FMatrix.Free;
   end;
   inherited Destroy;
end;

procedure TtdLCSMatrix.Clear;
var
   Row, Col: integer;
   ColList: TList;
begin
   for Row := 0 to pred(FRows) do
   begin
      ColList := TList(FMatrix.List^[Row]);
      if ColList <> nil then
      begin
         for Col := 0 to pred(FCols) do
         begin
            if ColList.List^[Col] <> nil then
               Dispose(PtdLCSData(ColList.List^[Col]));
            ColList.List^[Col] := nil;
         end;
      end;
   end;
end;

constructor TStringListLCS.Create(aFromList, aToList: TStringList);
begin
   inherited Create;
   FFromList := aFromList;
   FToList := aToList;
   FMatrix := TtdLCSMatrix.Create(aFromList.Count, aToList.Count);
   {now fill in the matrix}
   slGetCell(pred(aFromList.Count), pred(aToList.Count));
end;

destructor TStringListLCS.Destroy;
begin
   FMatrix.Free;
   inherited Destroy;
end;

function TStringListLCS.slGetCell(aFromInx, aToInx: integer): integer;
var
   LCSData: PtdLCSData;
   NorthLen: integer;
   WestLen: integer;
begin
   if (aFromInx = -1) or (aToInx = -1) then
      result := 0
   else
   begin
      LCSData := FMatrix[aFromInx, aToInx];
      if LCSData <> nil then
         result := LCSData^.ldLen
      else
      begin
         {create the new item}
         New(LCSData);
         {if the two current lines are equal, increment the count from the northwest, that's our previous item}
         if (FFromList[aFromInx] = FToList[aToInx]) then
         begin
            LCSData^.ldPrev := ldNorthWest;
            LCSData^.ldLen := slGetCell(aFromInx-1, aToInx-1) + 1;
         end
         {otherwise the current lines are different: use the maximum of the north or west (west preferred)}
         else
         begin
            NorthLen := slGetCell(aFromInx-1, aToInx);
            WestLen := slGetCell(aFromInx, aToInx-1);
            if NorthLen > WestLen then
            begin
               LCSData^.ldPrev := ldNorth;
               LCSData^.ldLen := NorthLen;
            end
            else
            begin
               LCSData^.ldPrev := ldWest;
               LCSData^.ldLen := WestLen;
            end;
        end;
        {set the item in the matrix}
        FMatrix[aFromInx, aToInx] := LCSData;
        {return the length of this LCS}
        result := LCSData^.ldLen;
      end;
   end;
end;

procedure TStringListLCS.slWriteChange(var F: Text; aFromInx, aToInx: integer);
var
   Cell: PtdLCSData;
   lStr: string;
begin
   {if both indexes are less than zero, this is the first cell of the LCS matrix, so just exit}
   if (aFromInx = -1) and (aToInx = -1) then
      Exit;
   {if the from index is less than zero, we're flush against the left hand side of the matrix, so go up; this'll be a deletion}
   if aFromInx = -1 then
   begin
      slWriteChange(F, aFromInx, aToInx-1);
      writeln(F, '-> ', FToList[aToInx]);
   end
   {if the to index is less than zero, we're flush against the top side of the matrix, so go left; this'll be an insertion}
   else if aToInx = -1 then
   begin
      slWriteChange(F, aFromInx-1, aToInx);
      writeln(F, '<- ', FFromList[aFromInx]);
   end
   {otherwise see what the cell says to do}
   else
   begin
      Cell := FMatrix[aFromInx, aToInx];
      case Cell^.ldPrev of
         ldNorth:
         begin
            slWriteChange(F, aFromInx-1, aToInx);
            writeln(F, '<- ', FFromList[aFromInx]);
         end;
         ldNorthWest:
         begin
            slWriteChange(F, aFromInx-1, aToInx-1);
            writeln(F, ' ', FFromList[aFromInx]);
         end;
         ldWest:
         begin
            slWriteChange(F, aFromInx, aToInx-1);
            writeln(F, '-> ', FToList[aToInx]);
            {if AnsiPos(FToList[aToInx], FFromList[aFromInx]) <> 0 then
            begin
               lStr := AnsiReplaceStr(FFromList[aFromInx], FToList[aToInx], '%s');
               FToList[aToInx] := Format(lStr, [FToList[aToInx]]);
            end;}
         end;
      end;
   end;
end;

procedure TStringListLCS.CommitDiffToDestList;
var
   F: System.Text;
begin
   System.Assign(F, 'info.txt');
   System.Rewrite(F);
   try
      slWriteChange(F, pred(FFromList.Count), pred(FToList.Count));
   finally
      System.Close(F);
   end;
end;

end.


