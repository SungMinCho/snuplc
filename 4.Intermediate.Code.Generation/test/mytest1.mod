module test;

var B : boolean[1][2][3];
    A : integer[1][2][3];
    x : integer;

function f(b : boolean[][][]; a:integer[][][]) : integer;
var i, j, k : integer;
begin
i := 0;
j := 0;
k := 0;
while (b[i][j][k] && b[k][j][i])
  do
  i := i + 1;
  j := j + 1;
  k := k + 1;
  if(true && b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(false && b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] && true) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] && false) then b[i][j][k] := false else b[i][j][k] := true end;
  if(true || b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(false || b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] || true) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] || false) then b[i][j][k] := false else b[i][j][k] := true end;
  if(true) then b[i][j][k] := a[i][j][k] < a[k][j][i] end
  end;
if ( (i < j) && (j < k) && false) then return 1 else return 0 end
end f;

function g() : integer;
var b : boolean[5][6][7];
    a : integer[5][6][7];
    i, j, k : integer;
begin
i := 0;
j := 0;
k := 0;
while (b[i][j][k] && b[k][j][i])
  do
  i := i + 1;
  j := j + 1;
  k := k + 1;
  if(true && b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(false && b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] && true) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] && false) then b[i][j][k] := false else b[i][j][k] := true end;
  if(true || b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(false || b[i][j][k]) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] || true) then b[i][j][k] := false else b[i][j][k] := true end;
  if(b[i][j][k] || false) then b[i][j][k] := false else b[i][j][k] := true end;
  if(true) then b[i][j][k] := a[i][j][k] < a[k][j][i] end
  end;
if ( (i < j) && (j < k) && false) then return 1 else return 0 end
end g;

begin
x := f(B, A) + g();
if(x = 0) then else end
end test.
