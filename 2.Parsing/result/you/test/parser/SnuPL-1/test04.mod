parsing 'test/parser/SnuPL-1/test04.mod'...
successfully parsed.
  AST:
    CAstScope: 'placeholder'
      symbol table:
        [[
        ]]
      statement list:
        := <int>
          1 <int>
          = <int>
            5 <int>
            4 <int>
        := <int>
          2 <int>
          = <int>
            1 <int>
            # <int>
              2 <int>
              = <int>
                3 <int>
                4 <int>
        := <int>
          3 <int>
          add <int>
            add <int>
              1 <int>
              div <int>
                # <int>
                  1 <int>
                  1 <int>
                4 <int>
            4 <int>
        := <int>
          1 <int>
          1 <int>
        := <int>
          2 <int>
          2 <int>
        := <int>
          3 <int>
          3 <int>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/SnuPL-1/test04.mod.ast.pdf test/parser/SnuPL-1/test04.mod.ast.dot


Done.
