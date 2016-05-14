module procedure_return;

var sum: integer[5][5];

procedure proc_return1(a : integer[][]);
begin
  return sum[1][2]
end proc_return1;

procedure proc_return(a: integer[][]);
begin
  sum[2][3] := sum[2][4]
end proc_return;

begin
  proc_return1(sum);
  proc_return(sum)
end procedure_return.