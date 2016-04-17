parsing 'test/parser/SnuPL-1/test03.mod'...
successfully parsed.
  AST:
    CAstScope: 'placeholder'
      symbol table:
        [[
        ]]
      statement list:
        := <int>
          1 <int>
          add <int>
            2 <int>
            3 <int>
        := <int>
          4 <int>
          mul <int>
            5 <int>
            6 <int>
      nested scopes:
        empty.
    


run the following command to convert the .dot file into a PDF:
  dot -Tpdf -otest/parser/SnuPL-1/test03.mod.ast.pdf test/parser/SnuPL-1/test03.mod.ast.dot


Done.
