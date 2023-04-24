import Data.List
import Control.Monad

data Effect = Dazed | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Zap | Squirt | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag { gagTrack :: GagTrack,
                 damage :: Integer,
                 prestige :: Bool }
         deriving (Eq, Ord)
instance Show Gag where
  show gag = (show $ gagTrack gag) ++ (show $ damage gag) 

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

gagCombo :: Fractional a => GagTrack -> a
gagCombo track | elem track [Throw, Squirt] = 1.2
               | track == Drop = 1.3
               | otherwise = 1

data Cog = Cog { hp :: Integer, cogEffects :: [Effect] }
         deriving Show
newCog :: Cog
newCog = Cog 0 []

canKnockback :: Gag -> Bool
canKnockback gag = gagTrack gag `elem` [Throw, Squirt]

applyDamage :: Integer -> Cog -> Cog
applyDamage dmg cog = Cog (dmg' + hp cog) (cogEffects cog)
  where
    marked = if Marked `elem` cogEffects cog then 1.1 else 1.0
    dmg' = (marked*) -: dmg

-- TODO use set instead of list here
applyEffect :: Effect -> Cog -> Cog
applyEffect effect cog = Cog (hp cog) (nub $ effect:cogEffects cog)

-- how TTCC does decimal calculations
(-:) f x = ceiling . f . fromIntegral $ x

applyGagTracks :: [Gag] -> Cog -> Cog
applyGagTracks gags = maybe id applyEffect effect . applyDamage dmg
  where
    track = gagTrack $ head gags
    dmg = let combo = if length gags > 1 then gagCombo track else 1
          in (combo*) -: (foldr1 (+) . map damage $ gags)
    effect = gagEffect track

applyGag :: Gag -> Cog -> Cog
applyGag gag = applyGagTracks [gag] 

applyGags :: [Gag] -> Cog -> Cog
applyGags = flip (foldr applyGagTracks) . groupGags . reverse . sort

-- testing
fruitPie = (!!8) $ filter ((Throw==) . gagTrack) gags
seltzer = (!!8) $ filter ((Squirt==) . gagTrack) gags
bowlingBall = (!!4) $ filter ((Drop==) . gagTrack) gags

type Test = ([Gag], Integer)
tests :: [Test]
tests = [([seltzer, seltzer], 72),
         ([fruitPie, fruitPie], 135),
         ([bowlingBall, bowlingBall], 91)]
runTests :: [Bool]
runTests = map (uncurry test) tests
  where test = (==) . hp . flip applyGags newCog

findCombos :: [GagTrack] -> Int -> [(Integer, [Gag])]
findCombos tracks players = map f . filter ((<=players) . length) . subsequences $ filter pred gags
  where
    pred gag = elem (gagTrack gag) tracks && (not . prestige) gag
    f gags = (hp $ applyGags gags newCog, gags)

cogLevels = [1..20]

addExes :: [Integer] -> [Integer]
addExes hps = (-:) <$> [id, (1.5*)] <*> hps

cogHPs = addExes $ (\ n -> (n + 1) * (n + 2)) <$> cogLevels
lbHPs = addExes $ [(\ n -> (n * (n + 1) + 1)), (\ n -> (n + 2) * (n + 3) - 2)] <*> cogLevels
mgrHPs = flip elem $ [240, 320, 465, 600]

main :: IO ()
main = do
  guard (all id runTests)
  mapM_ print $ sort . findCombos [Throw,Squirt,Drop] $ 2
