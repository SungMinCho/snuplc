module procedure_variable_samename;

var f : integer;

procedure f(X : integer);
begin
	WriteInt(X)
end f;

begin
	f(f)
end procedure_variable_samename.
