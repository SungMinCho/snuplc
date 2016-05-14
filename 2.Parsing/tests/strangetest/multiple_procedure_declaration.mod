module multiple_procedure_declaration;

var X, Y : integer;

procedure print(X : integer; Y : integer);
begin
	WriteInt(X)
end print;

procedure print(X : integer; Y : integer);
begin
	WriteInt(Y)
end print;

begin
	print(X, Y)
end multiple_procedure_declaration.
