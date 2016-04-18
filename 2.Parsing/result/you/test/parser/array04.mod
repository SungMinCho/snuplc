parsing 'test/parser/array04.mod'...
successfully parsed.
  AST:
    CAstScope: 'array04'
      symbol table:
        [[
          [ @A        <array 3  of <array 3  of <int>>>           ]
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *Set(<ptr(4) to <array  of <array  of <int>>>>,<int>) --> <NULL>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @i        <int>           ]
          [ *print() --> <NULL>           ]
        ]]
      statement list:
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          11111111 <int>
        call [ *WriteLn() --> <NULL> ] <NULL>
        call [ *print() --> <NULL> ] <NULL>
        := <int>
          [ @A        <array 3  of <array 3  of <int>>> ] <int>
            0 <int>
            0 <int>
          2 <int>
        := <int>
          [ @i        <int> ] <int>
          [ @A        <array 3  of <array 3  of <int>>> ] <int>
            0 <int>
            0 <int>
        call [ *Set(<ptr(4) to <array  of <array  of <int>>>>,<int>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 3  of <array 3  of <int>>>>
            [ @A        <array 3  of <array 3  of <int>>> ] <array 3  of <array 3  of <int>>>
          [ @i        <int> ] <int>
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          22222222 <int>
        call [ *WriteLn() --> <NULL> ] <NULL>
        call [ *print() --> <NULL> ] <NULL>
      nested scopes:
        CAstScope: 'Set'
          symbol table:
            [[
              [ %P        <ptr(4) to <array  of <array  of <int>>>>               ]
              [ %v        <int>               ]
            ]]
          statement list:
            := <int>
              [ %P        <ptr(4) to <array  of <array  of <int>>>> ] <int>
                [ %v        <int> ] <int>
                [ %v        <int> ] <int>
              1 <int>
          nested scopes:
            empty.
        
        CAstScope: 'print'
          symbol table:
            [[
              [ $i        <int>               ]
              [ $j        <int>               ]
            ]]
          statement list:
            := <int>
              [ $i        <int> ] <int>
              0 <int>
            while cond
              < <bool>
                [ $i        <int> ] <int>
                3 <int>
            while-body
              := <int>
                [ $j        <int> ] <int>
                0 <int>
              while cond
                < <bool>
                  [ $j        <int> ] <int>
                  3 <int>
              while-body
                call [ *WriteInt(<int>) --> <NULL> ] <NULL>
                  [ @A        <array 3  of <array 3  of <int>>> ] <int>
                    [ $i        <int> ] <int>
                    [ $j        <int> ] <int>
                := <int>
                  [ $j        <int> ] <int>
                  add <int>
                    [ $j        <int> ] <int>
                    1 <int>
              := <int>
                [ $i        <int> ] <int>
                add <int>
                  [ $i        <int> ] <int>
                  1 <int>
            call [ *WriteLn() --> <NULL> ] <NULL>
          nested scopes:
            empty.
        
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/array04.mod.ast.pdf test/parser/array04.mod.ast.dot


Done.
