module passing_dereferenced_array;

var X, Y : integer[3][4];

procedure f(A, B : integer[4]);
begin
	A[0] := B[0];
	A[1] := B[1];
	A[2] := B[2];
	A[3] := B[3]
end f;

begin
	f(X[1], Y[1])
end passing_dereferenced_array.