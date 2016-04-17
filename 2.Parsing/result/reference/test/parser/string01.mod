parsing 'test/parser/string01.mod'...
successfully parsed.
  AST:
    CAstScope: 'string01'
      symbol table:
        [[
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @_str_1   <array 14  of <char>>           ]
            [ data: 'Hello, world\n' ]
          [ @_str_2   <array 23  of <char>>           ]
            [ data: 'This is pretty nice!\n\n' ]
        ]]
      statement list:
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 14  of <char>>>
            "Hello, world\n" <array 14  of <char>>
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 23  of <char>>>
            "This is pretty nice!\n\n" <array 23  of <char>>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/string01.mod.ast.pdf test/parser/string01.mod.ast.dot


Done.
