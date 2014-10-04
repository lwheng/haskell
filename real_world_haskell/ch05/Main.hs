module Main (main) where
-- to create an executable, ghc expects a module named Main that contains a main function
--
-- to generate an executable:
--    ghc -o simple Main.hs SimpleJSON.o

import SimpleJSON
-- import statement, import wherever is exported in SimpleJSON

-- main, the function that is run when we execute a standalone program
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
