module more_arg;

var a,b,c,d: integer[5][5];

procedure add(A,B,C: integer[5][5]);
var i,j: integer;
begin
  i := 0;
  while (i < 5) do
    j := 0;
    while (j < 5) do
      C[i][j] := A[i][j] + B[i][j]
    end
  end
end add;

begin
  add(a,b,c,d)
end more_arg.
