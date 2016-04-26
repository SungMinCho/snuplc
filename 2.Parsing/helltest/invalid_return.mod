module invalid_return;

var proc, proc1: integer[5][5];
b0, b1, b2: boolean;
i0, i1, i2: integer;
ch0, ch1, ch2: char;

procedure return_recursive_procedure(proc, proc1: integer[][]);
begin
  proc[1][2] := proc1[2][3];
  return return_recursive_procedure(proc, proc1)
end return_recursive_procedure;

function foo(b: boolean; i0, i1: integer) : integer;
begin
  if (b) then
    return i0+i1
  else
    return i0-i1
  end
end foo;

function return_function_named_variable() : integer;
var foo: integer;
begin
  return foo
end return_function_named_variable;

function return_function() : integer;
var foox: integer;
begin
  return foo
end return_function;

procedure return_procedure();
begin
  return return_recursive_procedure
end return_procedure;

begin
  return_recursive_procedure(proc, proc1);
  return_function_named_variable();
  return_function();
  return_procedure();
end invalid_return.