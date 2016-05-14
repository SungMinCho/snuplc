module neg_bool;

var x : integer;

function f() : boolean;
begin
  return (- true) && false
end f;

begin
end neg_bool.