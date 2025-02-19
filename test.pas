program Test(Output);
function Add(a: integer, b: integer): integer; begin
   Add := a + b;
end;
function Sub(a: integer, b: integer): integer; begin
   Sub := a - b;
end;
function Idk(matrix : array [1..3: integer] of array [1..3: integer] of integer): boolean; begin
   Idk := true
end;
begin
   Writeln("Hello World");
end.
