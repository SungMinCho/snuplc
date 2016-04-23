module array_out_of_bounds;

var X, Y : integer[3][4];

procedure f(A, B : integer[4]);
begin
	A[200] := B[200]
end f;

begin
	f(X[100], Y[100])
end array_out_of_bounds.