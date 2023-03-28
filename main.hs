import Data.List

data Effect = Dazed | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Zap | Squirt | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag { gagTrack :: GagTrack,
                 damage :: Integer,
                 prestige :: Bool }
         deriving (Eq, Ord, Show)

gagDamage = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
             [5, 10, 15, 30, 55, 45, 100, 75], -- lure
             [14, 28, 45, 75, 115, 160, 220, 280], -- trap
             [8, 13, 20, 35, 56, 90, 130, 170], -- throw
             [12, 20, 36, 60, 90, 140, 180, 240], -- zap
             [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
             [5, 10, 16, 23, 30, 50, 70, 90], -- sound
             [8, 12, 35, 56, 90, 140, 200, 240]] -- drop
gags = [Gag (toEnum i :: GagTrack) damage  prestige
         | i <- [0 .. 7], damage <- gagDamage !! i, prestige <- [False, True]]

-- needed for combos
groupGags :: [Gag] -> [[Gag]]
groupGags = groupBy $ \ x y -> gagTrack x == gagTrack y

-- these are the only effects that can't exist at the start of a round
gagEffect :: GagTrack -> Maybe Effect
gagEffect Trap = Just Dazed
gagEffect Throw = Just Marked
gagEffect _ = Nothing

data Cog = Cog { hp :: Integer, cogEffects :: [Effect] }
         deriving Show
newCog :: Cog
newCog = Cog 0 []

canKnockback :: Gag -> Bool
canKnockback gag = gagTrack gag `elem` [Throw, Squirt]

applyDamage :: Integer -> Cog -> Cog
applyDamage dmg cog = Cog (dmg + hp cog) (cogEffects cog)

-- TODO use set instead of list here
applyEffect :: Effect -> Cog -> Cog
applyEffect effect cog = Cog (hp cog) (nub $ effect:cogEffects cog)

-- TODO take in a list of gags (of the same track) for combos
applyGag :: Gag -> Cog -> Cog
applyGag gag = maybe id applyEffect effect . applyDamage dmg
  where
    dmg = damage gag
    effect = gagEffect $ gagTrack gag

applyGags :: [Gag] -> Cog -> Cog
applyGags = flip (foldr applyGag) . reverse . sort
