import qualified Data.List as List
import qualified Data.Ord as Ord

data Character = Character { charHitPoints :: Int, charDamage :: Int, charArmor :: Int }
  deriving (Eq, Show)

data Item = Item { name :: String, itemCost :: Int, itemDamage :: Int, itemArmor :: Int }
  deriving (Eq, Show)

meHitPoints = 100

shopWeapons =
  [
    Item "Dagger" 8 4 0,
    Item "Shortsword" 10 5 0,
    Item "Warhammer" 25 6 0,
    Item "Longsword" 40 7 0,
    Item "Greataxe" 74 8 0
  ]

shopArmor =
  [
    Item "Leather" 13 0 1,
    Item "Chainmail" 31 0 2,
    Item "Splintmail" 53 0 3,
    Item "Bandedmail" 75 0 4,
    Item "Platemail" 102 0 5
  ]

shopRings =
  [
    Item "Damage +1" 25 1 0,
    Item "Damage +2" 50 2 0,
    Item "Damage +3" 100 3 0,
    Item "Defense +1" 20 0 1,
    Item "Defense +2" 40 0 2,
    Item "Defense +3" 80 0 3
  ]

main = do
  [bossHitPoints, bossDamage, bossArmor] <- map (read . dropLabel) <$> lines <$> getContents
  let boss = Character bossHitPoints bossDamage bossArmor
  let me = Character meHitPoints 0 0
  let itemCombinations = chooseItems
  let validItemCombinations = filter (\items -> boss `beats` (me `with` items)) itemCombinations
  let worst = head $ List.sortBy (flip $ Ord.comparing (sum . map itemCost)) validItemCombinations
  print worst
  print $ sum $ map itemCost worst

dropLabel :: String -> String
dropLabel = tail . tail . dropWhile (/= ':')

chooseItems :: [[Item]]
chooseItems =
  [w ++ a ++ r | w <- weapons, a <- armor, r <- rings]
  where
  weapons = combinations 1 shopWeapons
  armor = concatMap (\k -> combinations k shopArmor) [0..1]
  rings = concatMap (\k -> combinations k shopRings) [0..2]

with :: Character -> [Item] -> Character
me `with` items =
  me {
    charDamage = (sum $ map itemDamage items),
    charArmor = (sum $ map itemArmor items)
  }

beats :: Character -> Character -> Bool
Character winnerHitPoints winnerDamage winnerArmor `beats` Character loserHitPoints loserDamage loserArmor =
  winnerIterations <= loserIterations
  where
  winnerActualDamage = max 1 (winnerDamage - loserArmor)
  loserActualDamage  = max 1 (loserDamage - winnerArmor)
  winnerIterations = loserHitPoints `divUp` winnerActualDamage
  loserIterations  = winnerHitPoints `divUp` loserActualDamage

divUp :: Integral a => a -> a -> a
n `divUp` m
  | r == 0 = q
  | otherwise = q + 1
  where
  (q, r) = n `divMod` m

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [x : xs | x : ts <- List.tails list, xs <- combinations (n - 1) ts]
