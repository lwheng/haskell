import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
-- take a function, a filepath, returns a list of matches

simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)
-- simpleFind can only find matches based on the names
-- cannot find based on properties like file size, whether it is a file or directory
-- most importantly, simpleFind is strict, because it consists of a series of actions executed in the IO monad
-- this means if we have a million files, things slows down, and then we receive one huge result
-- -> this is not lazy
