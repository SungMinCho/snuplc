module test10;

var b0, b1, b2, b3, b4, b5, b6, b7 : boolean;
		boolArr0 : boolean[5];
		boolArr1 : boolean[5][5];
		boolArr2 : boolean[5][5][5];
		boolArr3 : boolean[5][5][5][5];
		boolArr4 : boolean[5][5][5][5][5];

function foo (p1 : boolean; p2 : boolean[5]; p3 : boolean[5][5]) : boolean;
begin
	return p3[1][2]
end foo;

begin
	b0 := foo(b0, boolArr0, boolArr1);
	b1 := foo(boolArr0[1], boolArr1[1], boolArr2[1]);
	b2 := foo(boolArr0[2], boolArr4[1][2][3][4], boolArr3[1][1]);
	b3 := foo(boolArr3[1][2][3][4], boolArr2[1][2], boolArr4[1][2][3]);
	b4 := foo(boolArr2[1][3][2], boolArr4[1][2][3][4], boolArr1);
	b5 := foo(boolArr1[1][2], boolArr1[1], boolArr1);
	b6 := foo(boolArr2[1][2][3], boolArr2[1][2], boolArr2[1]);
	b7 := foo(boolArr4[1][2][3][4][5], boolArr4[1][2][3][4], boolArr4[1][2][3])
end test10.
