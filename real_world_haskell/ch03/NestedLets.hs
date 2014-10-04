-- Nested Lets: let inside a let
-- Outer variables are accessible in the inner lets

foo = let
        a = 1
      in
        let
          b = 1
        in
          a + b

bar = let
        x = 1
      in
        ((let x = "foo" in x), x)

-- the outer x does not clash with the inner x
-- just proving it is possible, not a good practice since it can be confusing


quux a = let
          a = "foo"
         in
          a ++ "eek!"
-- The a on the left side is never used on the right hand side
