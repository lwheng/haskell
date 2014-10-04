-- ELF is a kind of Unix file

import qualified Data.ByteString.Lazy as L
-- qualified: just means we can do the "import ... as ..."
-- so we can refer to this module with a name defined by us
-- qualified import also makes it easier to switch the module without changing the main bulk of the code
-- of course, we can use the fullname: Data.ByteString.Lazy.take
--
-- Just accept that bytestring is more efficient than String

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
                  content <- L.readFile path
                  return (hasElfMagic content)
