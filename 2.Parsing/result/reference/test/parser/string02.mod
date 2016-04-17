parsing 'test/parser/string02.mod'...
successfully parsed.
  AST:
    CAstScope: 'string02'
      symbol table:
        [[
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @_str_1   <array 17  of <char>>           ]
            [ data: 'Enter your age: ' ]
          [ @_str_2   <array 17  of <char>>           ]
            [ data: 'Enter the year: ' ]
          [ @_str_3   <array 39  of <char>>           ]
            [ data: 'You will be 100 years old in the year ' ]
          [ @_str_4   <array 3  of <char>>           ]
            [ data: '.\n' ]
          [ @age      <int>           ]
          [ @year     <int>           ]
        ]]
      statement list:
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 17  of <char>>>
            "Enter your age: " <array 17  of <char>>
        := <int>
          [ @age      <int> ] <int>
          call [ *ReadInt() --> <int> ] <int>
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 17  of <char>>>
            "Enter the year: " <array 17  of <char>>
        := <int>
          [ @year     <int> ] <int>
          call [ *ReadInt() --> <int> ] <int>
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 39  of <char>>>
            "You will be 100 years old in the year " <array 39  of <char>>
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          sub <int>
            add <int>
              [ @year     <int> ] <int>
              100 <int>
            [ @age      <int> ] <int>
        call [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 3  of <char>>>
            ".\n" <array 3  of <char>>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/string02.mod.ast.pdf test/parser/string02.mod.ast.dot


Done.
