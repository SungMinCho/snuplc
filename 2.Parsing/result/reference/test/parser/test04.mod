parsing 'test/parser/test04.mod'...
successfully parsed.
  AST:
    CAstScope: 'test04'
      symbol table:
        [[
          [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>           ]
          [ *DOFS(<ptr(4) to <NULL>>) --> <int>           ]
          [ *ReadInt() --> <int>           ]
          [ *WriteChar(<char>) --> <NULL>           ]
          [ *WriteInt(<int>) --> <NULL>           ]
          [ *WriteLn() --> <NULL>           ]
          [ *WriteStr(<ptr(4) to <array  of <char>>>) --> <NULL>           ]
          [ @a        <int>           ]
          [ @b        <int>           ]
          [ @c        <int>           ]
        ]]
      statement list:
        := <int>
          [ @a        <int> ] <int>
          1 <int>
        := <int>
          [ @b        <int> ] <int>
          2 <int>
        := <int>
          [ @c        <int> ] <int>
          3 <int>
        := <int>
          [ @a        <int> ] <int>
          add <int>
            add <int>
              [ @a        <int> ] <int>
              [ @b        <int> ] <int>
            [ @c        <int> ] <int>
        := <int>
          [ @b        <int> ] <int>
          sub <int>
            sub <int>
              [ @a        <int> ] <int>
              [ @b        <int> ] <int>
            [ @c        <int> ] <int>
        := <int>
          [ @c        <int> ] <int>
          add <int>
            [ @a        <int> ] <int>
            mul <int>
              [ @b        <int> ] <int>
              [ @c        <int> ] <int>
        := <int>
          [ @a        <int> ] <int>
          neg <int>
            [ @a        <int> ] <int>
        := <int>
          [ @b        <int> ] <int>
          div <int>
            add <int>
              [ @a        <int> ] <int>
              [ @b        <int> ] <int>
            [ @c        <int> ] <int>
        := <int>
          [ @c        <int> ] <int>
          neg <int>
            add <int>
              -1 <int>
              [ @a        <int> ] <int>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/test04.mod.ast.pdf test/parser/test04.mod.ast.dot


Done.
