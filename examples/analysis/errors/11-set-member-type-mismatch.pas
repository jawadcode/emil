program Test11;
type
  Enum = (One, Two, Three);
var
  Numbers: set of integer;
procedure DoThing;
begin
end;
begin
  Numbers := [1, 2..5, 7];
  if One in Numbers then
    DoThing;
end.
