parsing 'test/parser/test05.mod'...
successfully parsed.
  AST:
    CAstScope: 'test05'
      symbol table:
        [[
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @max      <int>           ]
          [ @min      <int>           ]
        ]]
      statement list:
        := <int>
          [ @min      <int> ] <int>
          -2147483648 <int>
        := <int>
          [ @max      <int> ] <int>
          2147483647 <int>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/test05.mod.ast.pdf test/parser/test05.mod.ast.dot


Done.
