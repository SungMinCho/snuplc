module test;

function f(): integer;
var b : boolean; i:integer;
begin
  b := !b;
  b := !!b;
  b := !!!b;
  b := !!!!b;
  return i
end f;

begin
f()
end test.
