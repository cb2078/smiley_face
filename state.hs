import Control.Monad
import Control.Monad.State
import Data.List

-- TODO
-- prestiges
-- TU
-- squirt splash / zap jump

-- how TTCC does decimal calculations
mul :: (RealFrac a, Integral b) => a -> b -> b
mul x = ceiling . (x*) . fromIntegral

data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)

data Gag = Gag { gagTrack :: GagTrack, gagDamage :: Int, prestige :: Bool}
         deriving Show

gagLevels :: [Int]
gagLevels = [0 .. 7]

gagValues :: [[Int]]
gagValues = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
             [14, 28, 45, 75, 115, 160, 220, 280], -- trap
             [5, 10, 25, 30, 65, 50, 100, 75], -- lure
             [8, 13, 20, 35, 56, 90, 130, 170], -- throw
             [4, 8, 12, 21, 30, 60, 90, 120], -- squirt
             [12, 20, 36, 60, 90, 140, 190, 240], -- zap
             [5, 10, 16, 23, 30, 50, 70, 90], -- sound
             [8, 12, 35, 56, 90, 140, 200, 240]] -- drop

gags :: [Gag]
gags = do
  track <- enumFrom ToonUp
  let i = fromEnum track
  j <- gagLevels
  prestige <- [False, True]
  return $ Gag track (gagValues !! i !! j) prestige
  
seltzer, fruitPie, ball :: Gag
fruitPie = Gag Throw 56 False
seltzer = Gag Squirt 30 False
ball = Gag Drop 35 False

data Toon = Toon { toonHP :: Int } -- TODO effects

data Cog = Cog {
  cogHP :: Int,
  trapped :: Int,
  lured :: Int,
  marked :: Bool,
  soaked :: Bool
} deriving Show -- TODO effects

newCog :: Cog
newCog = Cog 0 0 0 False False

damage :: Int -> State Cog ()
damage value = do
  cog <- get
  let multiplier = if marked cog then 1.1 else 1
  put cog { cogHP = mul multiplier value + cogHP cog }

trap :: Int -> State Cog ()
trap damage = do
  cog <- get
  put cog { trapped = if trapped cog > 0 then 0 else damage }

lure :: Int -> State Cog ()
lure knockback = do
  cog <- get
  if trapped cog > 0
     then do
       damage (trapped cog)
       trap 0
     else put cog { lured = knockback }

unlure :: State Cog ()
unlure = do
  cog <- get
  put cog { lured = 0 }

mark :: State Cog ()
mark = do
  cog <- get
  put cog { marked = True }

soak :: State Cog ()
soak = do
  cog <- get
  put cog { soaked = True }

unsoak :: State Cog ()
unsoak = do
  cog <- get
  put cog { soaked = False }

useGags :: [Gag] -> State Cog ()
useGags gags = do
  cog <- get 
  let addKnockback = map (lured cog +)
  case gagTrack (head gags) of
       Trap -> if (length gags == 1 && trapped cog == 0)
         then trap $ head values
         else trap 0
       Lure -> lure (maximum values)
       Throw -> do
         damage $ combo `mul` sum (addKnockback values)
         mark
         unlure
       Squirt -> do
         damage $ combo `mul` sum (addKnockback values)
         soak
         unlure
       Zap -> do
         if soaked cog
            then do
              damage (sum values)
              unsoak
              unlure
            else unlure
       Sound -> do
         damage (sum values)
         unlure
       Drop -> unless (lured cog > 0)
         (damage (combo `mul` sum values))
  where
    values = gagDamage <$> gags
    track = gagTrack $ head gags
    combo | length gags == 1 = 1
          | track == Throw = 1.2
          | track == Drop = 1.3
          | otherwise = 1

useGag :: Gag -> State Cog ()
useGag = useGags . pure

heal :: [Gag] -> Toon -> Toon
heal = undefined

main :: IO ()
main = do
  let exp = do
        useGag (Gag Lure 55 False)
        useGag fruitPie
        useGag seltzer
        useGag ball
  print $ cogHP $ execState exp newCog
