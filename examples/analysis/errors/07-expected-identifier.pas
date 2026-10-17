program Test7;
var
  Test: integer;
procedure Apply(var x: integer; function f(x: integer): integer);
begin
  x := f(x)
end;
function Increment(x: integer): integer;
begin
  Increment := x + 1;
end;
begin
  Test := 0;
  Apply(Test, Increment);
end.
