import Control.Parallel (par, pseq)

parallelMap
  :: (a -> b)
  -> [a]
  -> [b]
parallelMap _ []     = []
parallelMap f (x:xs) = let
                         r = f x
                       in
                         r `par` r : parallelMap f xs

forceList
  :: [a]
  -> ()
forceList []     = ()
forceList (x:xs) = x `pseq` forceList xs

stricterMap
  :: (a -> b)
  -> [a]
  -> [b]
stricterMap f xs = forceList xs `seq` map f xs

forceListAndElts
  :: (a -> ())
  -> [a]
  -> ()
forceListAndElts _        []     = ()
forceListAndElts forceElt (x:xs) = forceElt x `seq` forceListAndElts forceElt xs
