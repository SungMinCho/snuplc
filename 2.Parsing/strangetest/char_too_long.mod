module char_too_long;

procedure f(c : char);
begin
	WriteChar(c)
end f;

begin
	f('ab')
end char_too_long.