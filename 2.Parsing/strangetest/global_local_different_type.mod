module global_local_different_type;

var x : integer;

procedure f(c : char);
var x : char;
begin
  x := c;
  WriteChar(x)
end f;

begin
  f('c')
end global_local_different_type.