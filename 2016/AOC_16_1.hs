import Data.Bits
import qualified Data.List as List

newtype Bs = Bs [Bool]
  deriving (Eq)

instance Show Bs where
  show (Bs []) = ""
  show (Bs (False : xs)) = '0' : show (Bs xs)
  show (Bs (True : xs)) = '1' : show (Bs xs)

desiredLength = 272

main = do
  input <- parseInput <$> getContents
  let (Just generatedData) = capped desiredLength <$> List.find (\(Bs bits) -> length bits >= desiredLength) (iterate step input)
  let checksum = calculateChecksum generatedData
  print checksum

parseInput :: String -> Bs
parseInput = Bs . map (== '1') . filter (`elem` ['0', '1'])

step :: Bs -> Bs
step (Bs a) = Bs $ a ++ [False] ++ b
  where
    b = map not $ reverse a

capped :: Int -> Bs -> Bs
capped size (Bs bits) = Bs (take size bits)

calculateChecksum :: Bs -> Bs
calculateChecksum (Bs bits) =
  if odd (length bits)
    then Bs bits
    else calculateChecksum $ Bs $ calculateChecksum' (pairs bits)
  where
    calculateChecksum' [] = []
    calculateChecksum' ((a, b) : rest) = not (a `xor` b) : calculateChecksum' rest

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs
