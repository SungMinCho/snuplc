module call_latter_defined_function;

var sum: integer[5][5];

procedure call_latter(a: integer[][]);
var i, j : integer;
begin
  i := latter_proc(i+j, a);
  latter_func(i-j, a)
end call_latter;

function latter_func(a: integer, intarr: integer[][]) : integer;
begin
  return intarr[a][2]
end latter_func;

procedure latter_proc(a: integer, intarr: integer[][]);
begin
  a := intarr[a][a]
end latter_proc;

begin
  call_latter(sum);
end call_latter_defined_function.