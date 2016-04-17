parsing 'test/scanner/SnuPL-1/test03.mod'...
successfully parsed.
  AST:
    CAstScope: 'placeholder'
      symbol table:
        [[
        ]]
      statement list:
        := <int>
          1 <int>
          5 <int>
        := <int>
          1 <int>
          add <int>
            2 <int>
            3 <int>
        := <int>
          5 <int>
          add <int>
            2 <int>
            mul <int>
              3 <int>
              add <int>
                sub <int>
                  sub <int>
                    1 <int>
                    2 <int>
                  3 <int>
                1 <int>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/scanner/SnuPL-1/test03.mod.ast.pdf test/scanner/SnuPL-1/test03.mod.ast.dot


Done.
