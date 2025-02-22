program Test(Output);

type
   Mat3x3 = array [0..2] of array [0..2] of integer;

var
   Thing   : Mat3x3;
   Counter : integer;
   
function CentralElementEqualsFour(matrix : array [rowsStart..rowsEnd: integer] of array [columnsStart..columnsEnd: integer] of integer): boolean; begin
   Idk := matrix[(rowsStart + rowsEnd) div 2,
                 (columnsStart + columnsEnd) div 2] = 4
end;

procedure Goodbye; begin
   Writeln('Goodbye World');
end;

begin
   Counter := 0;
   
   for i := 0 to 2 do
      for j := 0 to 2 do begin
         Counter := Counter + 1;
         Thing[i, j] := Counter
      end;

   if CentralElementEqualsFour(Thing) then
      Writeln('Central element equals 4');

   Goodbye;
end.

