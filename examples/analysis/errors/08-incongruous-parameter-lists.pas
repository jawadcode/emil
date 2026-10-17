program Test8;
var
  Test: integer;

function HOF(function f(a: integer; b: real): integer; a: integer; b: real): integer;
begin
  HOF := f(a, b);
end;

function Add(a: integer; b: integer): integer;
begin;
  Add := a + b;
end;

begin
  Test := HOF(Add, 1, 2.0);
end.
