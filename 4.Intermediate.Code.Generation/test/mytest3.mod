module test;

var s:char[10];

function f(s:char[]):char;
begin
return s[3]
end f;

begin
f("hello");
f(s)
end test.
