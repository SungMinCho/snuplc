module arraylength_notnumber;

var X, Y: integer;
a,b,c,d: integer[X][Y];

procedure add(A,B,C: integer[X][Y]);
var i,j: integer;
begin
  i := 0;
  while (i < X) do
    j := 0;
    while (j < Y) do
      C[i][j] := A[i][j] + B[i][j]
    end
  end
end add;

begin
  add(a,b)
end arraylength_notnumber.
