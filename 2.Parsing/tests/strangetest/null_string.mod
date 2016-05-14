module null_string;

var a,b,c,d: integer[5][5];

procedure write;
begin
  WriteStr("\0");
  WriteStr("\t");
  WriteStr("\n");
  WriteChar('\0');
  WriteChar('\t');
  WriteChar('\n');
  WriteLn()
end write;

begin
  write()
end null_string.
