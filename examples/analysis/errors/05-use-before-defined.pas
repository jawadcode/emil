(* TODO: This should throw an error, because `x` is used in the definition of `y`, before its defining point and in the same region, which is a violation of 6.2.2.9 *)
program Test5;
const
  x = 5;
procedure p;
const
  y = x;
var
  x = 1;
begin
end;
begin
end.
