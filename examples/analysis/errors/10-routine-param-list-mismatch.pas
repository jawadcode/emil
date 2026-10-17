program Test10;
var
  Test: integer;

function HOF(function f(a: integer; b: integer): integer; a: integer; b: integer): integer;
begin
  HOF := f(a, b);
end;

function Add(a, b: integer): integer;
begin;
  Add := a + b;
end;

begin
  (* I hate that there's a difference betweeen `a: integer; b: integer`
     and `a, b: integer` *)
  Test := HOF(Add, 1, 2);
end.
