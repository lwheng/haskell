import Control.Monad

{-
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
-}

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
-- Why (>>) works?
-- See, when the modulo is 0, the output is : return ()
-- return () >> return x
-- Recall for Maybe (for example):
-- Just _ >> k = k
-- So "return ()" is "Just _"
--
-- When modulo is not 0, the output is : mzero
-- For Maybe (for example):
-- Nothing >> _ = Nothing
