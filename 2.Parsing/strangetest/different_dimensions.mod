module different_dimensions;

var a,b,c,d: integer[3][4];

procedure add(A,B,C: integer[3][4]);
var i,j: integer;
begin
  i := 0;
  while (i < 3) do
    j := 0;
    while (j < 4) do
      C[i][j] := A[i][j] + B[i][j]
    end
  end
end add;

begin
  add(a,b)
end different_dimensions.
