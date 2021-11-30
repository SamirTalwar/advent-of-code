import Data.Sequence as Seq

main = do
  count <- read <$> getContents
  print $ winner count

winner :: Int -> Int
winner count = winner' $ Seq.fromList [1 .. count]
  where
    winner' elves
      | Seq.length elves == 1 = elves `index` 0
      | otherwise = winner' (Seq.drop 2 elves |> elves `index` 0)
