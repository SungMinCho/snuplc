parsing 'test/parser/array02.mod'...
successfully parsed.
  AST:
    CAstScope: 'array02'
      symbol table:
        [[
          [ @A        <array 5  of <array 5  of <int>>>           ]
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
        ]]
      statement list:
        := <int>
          [ @A        <array 5  of <array 5  of <int>>> ] <int>
            2 <int>
            3 <int>
          call [ *ReadInt() --> <int> ] <int>
        := <int>
          [ @A        <array 5  of <array 5  of <int>>> ] <int>
            0 <int>
            1 <int>
          [ @A        <array 5  of <array 5  of <int>>> ] <int>
            2 <int>
            3 <int>
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          [ @A        <array 5  of <array 5  of <int>>> ] <int>
            0 <int>
            1 <int>
        call [ *WriteLn() --> <NULL> ] <NULL>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/array02.mod.ast.pdf test/parser/array02.mod.ast.dot


Done.
