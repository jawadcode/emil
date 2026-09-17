program Test(Output);

const
  ExampleInteger = 0;
type
  ExampleType = Integer;
var
  ExampleVariable: ExampleType;

procedure Printeger(int: Integer); forward;

function Identity(elem: Integer): Integer;
const
  IdentityElement = 0;
begin
  Identity := elem + IdentityElement
end;

procedure Printeger;
begin
  (* Pretend that this actually does something *)
end;

begin
  ExampleVariable := ExampleInteger + Identity(123);
  Printeger(ExampleVariable);
end.
