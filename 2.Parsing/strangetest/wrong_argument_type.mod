module wrong_argument_type;

var a, b: integer;
    c, d : char;

function add(A, B: integer) : integer;
begin
  return A + B
end add;

begin
  add(a, c)
end wrong_argument_type.