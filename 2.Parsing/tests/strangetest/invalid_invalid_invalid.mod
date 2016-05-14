module invalid_invalid_invalid;

var x : integer;

function f() : boolean;
begin
  return ((x + 'a') = ('a' + x))
end f;

begin
end invalid_invalid_invalid.