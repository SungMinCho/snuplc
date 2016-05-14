module recursive_array;

var a: integer[5][6][7];
b: boolean[1][2][3];
p, q, r: integer;
s: char;

procedure rec_arr(a: integer[][][]; b: boolean[][][])
var i, j, k: integer;
begin
  a[a[a[2][3][5]]] := a[j][a[2][k][j]][i];
  b[a[2][a[4][1][2]][i+k]] := false || false || b[a[2][a[i*k][2][3]][i/k]]
end rec_arr;

function add(a, b, c: integer): integer;
var i, j, k: integer;
begin
  i := a*b;
  j := b*c*a;
  k := i+j;
  if (k >= i) then
    return (a+b)
  else
    return (i+c+k)
  end
end add;

begin
  rec_arr(a, b);
  a[add(add(p, q, q), a[add(p, q, r)][r][q+r/p], add(p, p, q))][add(p, r, q)][a[p][p+q][p+q+r]] := p*q*r
end recursive_array.