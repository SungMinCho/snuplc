parsing 'test/parser/array07.mod'...
successfully parsed.
  AST:
    CAstScope: 'array07'
      symbol table:
        [[
          [ *Add(<ptr(4) to <array  of <array  of <int>>>>,<ptr(4) to <array  of <array  of <int>>>>,<ptr(4) to <array  of <array  of <int>>>>) --> <NULL>           ]
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *Init(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL>           ]
          [ *Print(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL>           ]
          [ *ReadInt() --> <int>           ]
          [ *Test() --> <NULL>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @sum      <array 5  of <array 5  of <int>>>           ]
        ]]
      statement list:
        call [ *Test() --> <NULL> ] <NULL>
        call [ *Print(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
          &() <ptr(4) to <array 5  of <array 5  of <int>>>>
            [ @sum      <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
      nested scopes:
        CAstScope: 'Print'
          symbol table:
            [[
              [ $M        <int>               ]
              [ $N        <int>               ]
              [ %a        <ptr(4) to <array  of <array  of <int>>>>               ]
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
                  [ %a        <ptr(4) to <array  of <array  of <int>>>> ] <int>
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
          nested scopes:
            empty.
        
        CAstScope: 'Add'
          symbol table:
            [[
              [ $M        <int>               ]
              [ $N        <int>               ]
              [ $c        <int>               ]
              [ %d        <ptr(4) to <array  of <array  of <int>>>>               ]
              [ $i        <int>               ]
              [ $j        <int>               ]
              [ %s1       <ptr(4) to <array  of <array  of <int>>>>               ]
              [ %s2       <ptr(4) to <array  of <array  of <int>>>>               ]
            ]]
          statement list:
            := <int>
              [ $N        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %d        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
                1 <int>
            := <int>
              [ $M        <int> ] <int>
              call [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int> ] <int>
                [ %d        <ptr(4) to <array  of <array  of <int>>>> ] <ptr(4) to <array  of <array  of <int>>>>
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
                  [ %d        <ptr(4) to <array  of <array  of <int>>>> ] <int>
                    [ $i        <int> ] <int>
                    [ $j        <int> ] <int>
                  add <int>
                    [ %s1       <ptr(4) to <array  of <array  of <int>>>> ] <int>
                      [ $i        <int> ] <int>
                      [ $j        <int> ] <int>
                    [ %s2       <ptr(4) to <array  of <array  of <int>>>> ] <int>
                      [ $i        <int> ] <int>
                      [ $j        <int> ] <int>
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
          nested scopes:
            empty.
        
        CAstScope: 'Test'
          symbol table:
            [[
              [ $a        <array 5  of <array 5  of <int>>>               ]
              [ $b        <array 5  of <array 5  of <int>>>               ]
              [ $c        <array 5  of <int>>               ]
            ]]
          statement list:
            call [ *Init(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
              &() <ptr(4) to <array 5  of <array 5  of <int>>>>
                [ $a        <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
            call [ *Init(<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
              &() <ptr(4) to <array 5  of <array 5  of <int>>>>
                [ $b        <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
            call [ *Add(<ptr(4) to <array  of <array  of <int>>>>,<ptr(4) to <array  of <array  of <int>>>>,<ptr(4) to <array  of <array  of <int>>>>) --> <NULL> ] <NULL>
              &() <ptr(4) to <array 5  of <array 5  of <int>>>>
                [ $a        <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
              &() <ptr(4) to <array 5  of <array 5  of <int>>>>
                [ $b        <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
              &() <ptr(4) to <array 5  of <array 5  of <int>>>>
                [ @sum      <array 5  of <array 5  of <int>>> ] <array 5  of <array 5  of <int>>>
          nested scopes:
            empty.
        
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/array07.mod.ast.pdf test/parser/array07.mod.ast.dot


Done.
