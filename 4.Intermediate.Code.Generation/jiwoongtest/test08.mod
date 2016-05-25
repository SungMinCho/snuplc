//
// File Name : test08.mod
// Description : This test intensively tests for boolean related things.
//

module test08;

var i0, i1, i2 : integer;
		b0, b1, b2 : boolean;
		c0, c1, c2 : char;
		BoolArr0 : boolean[5];
		BoolArr1 : boolean[5][5];
		BoolArr2 : boolean[5][5][5];
		BoolArr3 : boolean[5][5][5];
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

function MatSub(A, B: integer[5][5]) : boolean;
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
			MatRes[rowcnt][colcnt] := A[rowcnt][colcnt] - B[rowcnt][colcnt];
			colcnt := colcnt + 1
		end;
		rowcnt := rowcnt + 1
	end;

	return true
end MatSub;

function MatMult(A, B: integer[5][5]) : boolean;
var rowcnt, colcnt, tempcnt, tempval: integer;
begin
	rowcnt := 0;
	colcnt := 0;
	tempcnt := 0;
	tempval := 0;
	
	if (DIM(A, 2) # DIM(B, 1)) then
		WriteStr("Column count of Matrix A does not match with Row count of Matrix B!\n");
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
		while (colcnt < DIM(B, 2)) do
			while (tempcnt < DIM(A, 2)) do
				tempval := tempval + A[rowcnt][tempcnt] * B[tempcnt][colcnt];
				tempcnt := tempcnt + 1
			end;
			colcnt := colcnt + 1
		end;
		rowcnt := rowcnt + 1
	end;
	
	return true
end MatMult;

procedure booltest1 (p1, p2 : boolean);
begin
	p1 := true;
	p1 := p2;
	p1 := b0;
	p1 := p2 && (i0 < i2 * i1) || !p1 && (BoolArr1[2][1] || BoolArr3[2][3][1]);
	p1 := p2 || p1;
	p2 := (BoolArr2[2][3][1] && !p2) && (BoolArr1[2][3] && BoolArr0[2] || BoolArr2[2][3][1]);
	
	if (true) then
		if (false) then
			if (p2) then
				if (b0) then
					if (p2 && (i0 < i2 * i1) || !p1 && (BoolArr1[2][1] || BoolArr3[2][3][1])) then
						b0 := b1 && b2
					else
						if (p2 || p1) then
							b1 := b0 || b1
						end
					end
				end
			else
				if ((BoolArr2[2][3][1] && !p2) && (BoolArr1[2][3] && BoolArr0[2] || BoolArr2[2][3][1])) then
					WriteStr("Hello, World!\n")
				end
			end
		end
	end;
	
	while (true) do
		while (false) do
			while (p2) do
				while (b0) do
					while (p2 && (i0 < i2 * i1) || !p1 && (BoolArr1[2][1] || BoolArr3[2][3][1])) do
						b0 := b1 && b2;
						while (p2 || p1) do
							p2 := b0 || b1
						end
					end
				end
			end;
			while ((BoolArr2[2][3][1] && !p2) && (BoolArr1[2][3] && BoolArr0[2] || BoolArr2[2][3][1])) do
				BoolArr2[2][3][1] := !p2 && BoolArr0[2];
				BoolArr1[2][3] := BoolArr0[2]
			end
		end
	end;
	
	p1 := !true;
	p2 := !false;
	p1 := !b0;
	p2 := !p2;
	p2 := (!(!(((!p2 && !(BoolArr2[3][2][1] && BoolArr0[1]))))));
	return
end booltest1;

function booltest2 (p1, p2 : boolean; p3, p4 : boolean[5]) : boolean;
begin
	if (true) then
		if (false) then
			if (p3[2] && p4[1] && !p1) then
				WriteStr("Hello, World!\n");
				if (p3[0]) then
					return true
				else
					return false
				end
			else
				return p3[2] && BoolArr1[1][2] || !BoolArr2[1][4][3] || (!p3[1] && (!(!(!p4[1]))))
			end
		else
			return p1
		end
	else
		return p2
	end;
	return false
end booltest2;

procedure booltest3 (p1 : boolean[5][5]);
begin
	p1[i0][i1 * i2] := BoolArr0[1];
	return
end booltest3;

begin
	booltest1(BoolArr0[1], BoolArr0[2]);
	booltest1(BoolArr2[1][4][3], BoolArr1[2][3]);
	booltest1(BoolArr3[1][3][2], BoolArr0[1]);
	booltest2(true, true, BoolArr0, BoolArr0);
	booltest3(BoolArr1);
	booltest3(BoolArr2[1]);
	
	b0 := booltest2(BoolArr0[2], BoolArr1[1][3], BoolArr1[2], BoolArr2[1][4]);
	b1 := !booltest2(BoolArr1[2][3], BoolArr2[1][2][3], BoolArr3[1][2], BoolArr2[0][0]);
	b0 := !(!booltest2(true, false, BoolArr3[1][2], BoolArr2[0][1]));
	
	if (booltest2(false, true, BoolArr0, BoolArr0) && booltest2(BoolArr0[1], BoolArr0[0], BoolArr0, BoolArr0)) then
		WriteStr("Hello, World!\n")
	end;
	WriteStr("End of boolean tests.\n")
end test08.