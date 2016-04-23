module multiple_type_formalparam;

var X, Y: integer;
a,b,c,d: integer[10][11];

procedure add(A,B,C: integer[][]; D : integer);
var i,j: integer;
begin
  i := 0;
  while (i < D) do
    j := 0;
    while (j < D) do
      C[i][j] := A[i][j] + B[i][j]
    end
  end
end add;

begin
  add(a,b)
end multiple_type_formalparam.
