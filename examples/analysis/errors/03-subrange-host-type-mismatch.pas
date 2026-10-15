program Test3(Output);
type
  ExampleEnum1 = (One, Two, Three, Four);
  ExampleEnum2 = (A, B, C, D);
var
  ExampleVariable1: One..Four;
  ExampleVariable2: B..C;
begin
  ExampleVariable2 := B;
  ExampleVariable1 := ExampleVariable2;
end.
