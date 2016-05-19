module test;

function min(a, b:integer):integer;
var c : boolean;
begin
c := a = b;
if (a<b) then return a else return b end
end min;

begin
  min(3, 4)
end test.
