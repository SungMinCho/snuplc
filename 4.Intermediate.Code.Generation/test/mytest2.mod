module test;

var B : boolean[1][2][3];
    A : integer[1][2][3];
    x : integer;

function f(a:integer[]):integer;
begin
return a[5]
end f;

function g(a:integer[][]):integer;
begin
return f(a[3])
end g;

begin
g(A[0])
end test.
