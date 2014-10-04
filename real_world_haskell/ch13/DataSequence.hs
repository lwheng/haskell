import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable

{-
 - Seq.empty ----> fromList []
 - Seq.singleton 1 ----> fromList [1]
 - Seq.singleton 1 |> 2 ----> fromList [1,2]
 -
 - Foldable.toList (Seq.fromList [1,2,3]) ----> [1,2,3]
 - Foldable.foldl' (+) 0 (Seq.fromList [1,2,3])


