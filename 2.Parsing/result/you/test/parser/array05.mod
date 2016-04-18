parsing 'test/parser/array05.mod'...
successfully parsed.
  AST:
    CAstScope: 'array05'
      symbol table:
        [[
          [ @A        <array 3  of <array 3  of <int>>>           ]
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *Init(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL>           ]
          [ *Print(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL>           ]
          [ *ReadInt() --> <int>           ]
          [ *Set(<ptr(4) to <array  of <array  of <int>>>>,<int>) --> <NULL>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @i        <int>           ]
        ]]
      statement list:
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          11111111 <int>
        call [ *WriteLn() --> <NULL> ] <NULL>
        call [ *Print(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 3  of <array 3  of <int>>>>
            [ @A        <array 3  of <array 3  of <int>>> ] <array 3  of <array 3  of <int>>>
        call [ *WriteInt(<int>) --> <NULL> ] <NULL>
          22222222 <int>
        call [ *WriteLn() --> <NULL> ] <NULL>
        call [ *Init(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 3  of <array 3  of <int>>>>
            [ @A        <array 3  of <array 3  of <int>>> ] <array 3  of <array 3  of <int>>>
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
        
        CAstScope: 'Print'
          symbol table:
            [[
              [ %A        <ptr(4) to <array  of <array  of <int>>>>               ]
              [ $M        <int>               ]
              [ $N        <int>               ]
              [ $i        <int>               ]
              [ $j        <int>               ]
            ]]
          statement list:
            := <int>
              [ $N        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %A        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
                1 <int>
            := <int>
              [ $M        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %A        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
                2 <int>
            := <int>
              [ $i        <int> ] <int>
              0 <int>
            while cond
              < <bool>
                [ $i        <int> ] <int>
                [ $N        <int> ] <int>
            while-body
              := <int>
                [ $j        <int> ] <int>
                0 <int>
              while cond
                < <bool>
                  [ $j        <int> ] <int>
                  [ $M        <int> ] <int>
              while-body
                call [ *WriteInt(<int>) --> <NULL> ] <NULL>
                  [ %A        <ptr(4) to <array  of <array  of <int>>>> ] <int>
                    [ $i        <int> ] <int>
                    [ $j        <int> ] <int>
                call [ *WriteLn() --> <NULL> ] <NULL>
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
          nested scopes:
            empty.
        
        CAstScope: 'Init'
          symbol table:
            [[
              [ $M        <int>               ]
              [ $N        <int>               ]
              [ %a        <ptr(4) to <array  of <array  of <int>>>>               ]
              [ $c        <int>               ]
              [ $i        <int>               ]
              [ $j        <int>               ]
            ]]
          statement list:
            := <int>
              [ $N        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %a        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
                1 <int>
            := <int>
              [ $M        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %a        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
                2 <int>
            := <int>
              [ $c        <int> ] <int>
              0 <int>
            := <int>
              [ $i        <int> ] <int>
              0 <int>
            while cond
              < <bool>
                [ $i        <int> ] <int>
                [ $N        <int> ] <int>
            while-body
              := <int>
                [ $j        <int> ] <int>
                0 <int>
              while cond
                < <bool>
                  [ $j        <int> ] <int>
                  [ $M        <int> ] <int>
              while-body
                := <int>
                  [ %a        <ptr(4) to <array  of <array  of <int>>>> ] <int>
                    [ $i        <int> ] <int>
                    [ $j        <int> ] <int>
                  [ $c        <int> ] <int>
                := <int>
                  [ $c        <int> ] <int>
                  add <int>
                    [ $c        <int> ] <int>
                    1 <int>
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
            call [ *Print(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
              [ %a        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
          nested scopes:
            empty.
        
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/array05.mod.ast.pdf test/parser/array05.mod.ast.dot


Done.
