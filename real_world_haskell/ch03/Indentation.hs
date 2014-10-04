-- more on indentation
-- this time, on Let and Where

foo = let firstDefinition = 1
          -- a comment-only line is treated as empty
          -- notice how the "+ 2" still works, continuing the previous line
                              + 2
          secondDefinition = 3
      in
        firstDefinition + secondDefinition
                
