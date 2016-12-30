import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

main = do
  presents <- read <$> getContents
  let solution = head $ dropWhile ((< presents) . (* 10) . sum . factors) [1..]
  print solution

factors :: Int -> Set Int
factors n =
  Set.unions
    $ map (\r -> Set.fromList [r, n `div` r])
    $ filter (\r -> n `mod` r == 0)
    $ takeWhile (\r -> r * r <= n)
    $ [1..]
