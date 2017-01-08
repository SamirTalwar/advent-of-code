{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Function (on)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import           Data.Set (Set)
import qualified Data.Set as Set

me = Wizard {
    _wizardHitPoints = 50,
    _wizardManaPoints = 500,
    _wizardArmor = 0
  }

data Wizard = Wizard {
    _wizardHitPoints :: Int,
    _wizardManaPoints :: Int,
    _wizardArmor :: Int
  } deriving (Eq, Show)
makeLenses ''Wizard

data Enemy = Enemy {
    _enemyHitPoints :: Int,
    _enemyDamage :: Int
  } deriving (Eq, Show)
makeLenses ''Enemy

data Spell = Spell {
    _spellName :: String,
    _spellManaCost :: Int,
    _spellEffectLength :: Int,
    _spellSelfOneTimeEffect :: Wizard -> Wizard,
    _spellSelfEffect :: Wizard -> Wizard,
    _spellSelfUneffect :: Wizard -> Wizard,
    _spellEnemyEffect :: Enemy -> Enemy
  }
makeLenses ''Spell

data Battle = Battle {
    _battleWizard :: Wizard,
    _battleEnemy :: Enemy,
    _battleEffects :: Set Spell,
    _battleManaSpent :: Int,
    _battleHistory :: [Spell]
  }
makeLenses ''Battle

instance Eq Spell where
  a == b = a^.spellName == b^.spellName && a^.spellEffectLength == b^.spellEffectLength

instance Ord Spell where
  compare a b = Ord.comparing _spellName a b `thenCompare` Ord.comparing _spellEffectLength a b

instance Show Spell where
  show = show . _spellName

instance Eq Battle where
  a == b = a^.battleWizard == b^.battleWizard
        && a^.battleEnemy == b^.battleEnemy
        && a^.battleEffects == b^.battleEffects
        && a^.battleManaSpent == b^.battleManaSpent

instance Ord Battle where
  compare a b = Ord.comparing (^.battleManaSpent) a b
                  `thenCompare` Ord.comparing (^.battleWizard.wizardHitPoints) a b
                  `thenCompare` Ord.comparing (^.battleEnemy.enemyHitPoints) a b

instance Show Battle where
  show battle =
    concat [
      "Wizard: ",
      show (battle^.battleWizard),
      "\n",
      "Enemy: ",
      show (battle^.battleEnemy),
      "\n",
      "Mana Spent: ",
      show (battle^.battleManaSpent),
      "\n",
      "Effects: ",
      show (battle^.battleEffects),
      "\n",
      "History: ",
      show (reverse $ battle^.battleHistory),
      "\n"
    ]

noSpell =
  Spell {
    _spellName = "Nope",
    _spellManaCost = 0,
    _spellEffectLength = 1,
    _spellSelfOneTimeEffect = id,
    _spellSelfEffect = id,
    _spellSelfUneffect = id,
    _spellEnemyEffect = id
  }

spells = [magicMissile, drain, shield, poison, recharge]

magicMissile =
  Spell {
    _spellName = "Magic Missile",
    _spellManaCost = 53,
    _spellEffectLength = 1,
    _spellSelfOneTimeEffect = id,
    _spellSelfEffect = id,
    _spellSelfUneffect = id,
    _spellEnemyEffect = enemyHitPoints -~ 4
  }

drain =
  Spell {
    _spellName = "Drain",
    _spellManaCost = 73,
    _spellEffectLength = 1,
    _spellSelfOneTimeEffect = id,
    _spellSelfEffect = wizardHitPoints +~ 2,
    _spellSelfUneffect = id,
    _spellEnemyEffect = enemyHitPoints -~ 2
  }

shield =
  Spell {
    _spellName = "Shield",
    _spellManaCost = 113,
    _spellEffectLength = 6,
    _spellSelfOneTimeEffect = wizardArmor +~ 7,
    _spellSelfEffect = id,
    _spellSelfUneffect = wizardArmor -~ 7,
    _spellEnemyEffect = id
  }

poison =
  Spell {
    _spellName = "Poison",
    _spellManaCost = 173,
    _spellEffectLength = 6,
    _spellSelfOneTimeEffect = id,
    _spellSelfEffect = id,
    _spellSelfUneffect = id,
    _spellEnemyEffect = enemyHitPoints -~ 3
  }

recharge =
  Spell {
    _spellName = "Recharge",
    _spellManaCost = 229,
    _spellEffectLength = 5,
    _spellSelfOneTimeEffect = id,
    _spellSelfEffect = wizardManaPoints +~ 101,
    _spellSelfUneffect = id,
    _spellEnemyEffect = id
  }

main = do
  [bossHitPoints, bossDamage] <- map (read . dropLabel) <$> lines <$> getContents
  let boss = Enemy bossHitPoints bossDamage
  let start = Battle me boss Set.empty 0 []
  let wins = allWins [start]
  print $ head wins

dropLabel :: String -> String
dropLabel = tail . tail . dropWhile (/= ':')

allWins :: [Battle] -> [Battle]
allWins [] = []
allWins (battle : battles)
  | won battle = battle : allWins battles
  | lost battle = allWins battles
  | otherwise =
    allWins $ removeWorse $ List.nub $ mergeSorted [play spell battle | spell <- allowedSpells battle] battles

won :: Battle -> Bool
won battle = battle^.battleEnemy.enemyHitPoints <= 0

lost :: Battle -> Bool
lost battle = battle^.battleWizard.wizardHitPoints <= 0

removeWorse :: [Battle] -> [Battle]
removeWorse [] = []
removeWorse battles@[battle] = battles
removeWorse (a : b : battles)
  | a `betterThan` b = removeWorse (a : battles)
  | b `betterThan` a = removeWorse (b : battles)
  | otherwise = a : removeWorse (b : battles)
  where
  a `betterThan` b =
    a^.battleManaSpent == b^.battleManaSpent
      && ((a^.battleWizard.wizardHitPoints >= b^.battleWizard.wizardHitPoints
            && a^.battleEnemy.enemyHitPoints < b^.battleEnemy.enemyHitPoints)
          || (a^.battleWizard.wizardHitPoints > b^.battleWizard.wizardHitPoints
                && a^.battleEnemy.enemyHitPoints <= b^.battleEnemy.enemyHitPoints))

allowedSpells :: Battle -> [Spell]
allowedSpells battle =
  filter (\spell -> (spell^.spellName) `Set.notMember` spellsInEffect)
    $ filter (\spell -> spell^.spellManaCost <= battle^.battleWizard.wizardManaPoints)
    $ spells
  where
  spellsInEffect =
    Set.map (^.spellName)
      $ Set.filter (\spell -> spell^.spellEffectLength > 1)
      $ battle^.battleEffects

play :: Spell -> Battle -> Battle
play spell = spendMana spell . thump . applyEffects noSpell . applyEffects spell . punish

applyEffects :: Spell -> Battle -> Battle
applyEffects newSpell (Battle wizard enemy effects manaSpent history) =
  let
    oneTimeAffectedWizard = newSpell^.spellSelfOneTimeEffect $ wizard
    affectedWizard = Set.fold _spellSelfEffect oneTimeAffectedWizard effects
    (lastingEffects, removedEffects) =
      Set.partition (\spell -> spell^.spellEffectLength > 0)
        $ Set.map (over spellEffectLength (\x -> x - 1)) effects
    nextWizard = Set.fold _spellSelfUneffect affectedWizard removedEffects
    nextEnemy = Set.fold _spellEnemyEffect enemy effects
    nextEffects = Set.insert newSpell lastingEffects
  in Battle nextWizard nextEnemy nextEffects manaSpent history

punish :: Battle -> Battle
punish = battleWizard.wizardHitPoints -~ 1

thump :: Battle -> Battle
thump battle = battleWizard .~ enemy `hits` wizard $ battle
  where
  enemy = battle^.battleEnemy
  wizard = battle^.battleWizard

hits :: Enemy -> Wizard -> Wizard
enemy `hits` wizard
  | enemy^.enemyHitPoints > 0 = wizardHitPoints -~ damage $ wizard
  | otherwise = wizard
  where
  damage = max 1 $ enemy^.enemyDamage - wizard^.wizardArmor

spendMana :: Spell -> Battle -> Battle
spendMana spell =
    (battleWizard.wizardManaPoints -~ spell^.spellManaCost)
  . (battleManaSpent +~ spell^.spellManaCost)
  . (over battleHistory (spell :))

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted xs [] = xs
mergeSorted [] ys = ys
mergeSorted xx@(x : xs) yy@(y : ys)
  | x <= y    = x : mergeSorted xs yy
  | otherwise = y : mergeSorted xx ys

thenCompare :: Ordering -> Ordering -> Ordering
thenCompare EQ ordering = ordering
thenCompare ordering _  = ordering
