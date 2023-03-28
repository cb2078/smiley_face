import Data.List

data Effect = Dazed | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Zap | Squirt | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag { gagTrack :: GagTrack,
                 damage :: Integer,
                 prestige :: Bool,
                 gagEffects :: [Effect] }
         deriving Show

gagDamage = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
             [5, 10, 15, 30, 55, 45, 100, 75], -- lure
             [14, 28, 45, 75, 115, 160, 220, 280], -- trap
             [8, 13, 20, 35, 56, 90, 130, 170], -- throw
             [12, 20, 36, 60, 90, 140, 180, 240], -- zap
             [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
             [5, 10, 16, 23, 30, 50, 70, 90], -- sound
             [8, 12, 35, 56, 90, 140, 200, 240]] -- drop
gags = [Gag (toEnum i :: GagTrack) damage  prestige []
         | i <- [0 .. 7], damage <- gagDamage !! i, prestige <- [False, True]]

data Cog = Cog { hp :: Integer, cogEffects :: [Effect] }
         deriving Show
newCog :: Cog
newCog = Cog 0 []
