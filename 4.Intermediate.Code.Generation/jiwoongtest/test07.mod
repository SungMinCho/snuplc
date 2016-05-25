module test07;

var i0, i1, i2 : integer;
		b0, b1, b2 : boolean;
		c0, c1, c2 : char;
		IntArr0 : integer[5];
		IntArr1 : integer[5][5];
		IntArr2 : integer[5][5][5];
		MatA : integer[5][5];
		MatB : integer[5][5];
		MatRes : integer[5][5];

function MatAdd(A, B: integer[5][5]) : boolean;
var rowcnt, colcnt: integer;
begin
	rowcnt := 0;
	colcnt := 0;

	if (DIM(A, 1) # DIM(B, 1)) then
		WriteStr("Row count does not match!\n");
		return false
	end;

	if (DIM(A, 2) # DIM(B, 2)) then
		WriteStr("Column count does not match!\n");
		return false
	end;

	if (DIM(A, 0) # 2) then
		WriteStr("Parameter A is not matrix!\n");
		return false
	end;

	if (DIM(B, 0) # 2) then
		WriteStr("Parameter B is not matrix!\n");
		return false
	end;

	while (rowcnt < DIM(A, 1)) do
		while (colcnt < DIM(A, 2)) do
			MatRes[rowcnt][colcnt] := A[rowcnt][colcnt] + B[rowcnt][colcnt];
			colcnt := colcnt + 1
		end;
		rowcnt := rowcnt + 1
	end;

	return true
end MatAdd;

begin
	i0 := 0;
	i1 := 0;
	
	while (i0 < 5) do
		while (i1 < 5) do
			MatA[i0][i1] := i0 * 2 + i1;
			MatB[i0][i1] := MatA[i0][i1] * 2 + i0 * 3 + i1;
			i1 := i1 + 1
		end;
		i0 := i0 + 1
	end;
	
	b0 := MatAdd(MatA, MatB);
	if (b0) then
		WriteStr("Matrix addition successful!\n")
	else
		WriteStr("Matrix addition failed!\n")
	end;
	
	WriteStr("End of matrix test.\n")
end test07.
