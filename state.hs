import Control.Monad
import Control.Monad.State
import Data.List
import Data.Function

-- TODO
-- squirt splash / zap jump

-- how TTCC does decimal calculations
mul :: Integral b => Rational -> b -> b
mul x = ceiling . (x*) . fromIntegral

data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)

data Gag = Gag {
  gagTrack :: GagTrack,
  gagLevel :: Int,
  prestiged :: Bool
} deriving (Eq, Ord)

instance Show Gag where
  show gag = concat
    [if prestiged gag then "Pres " else ""
    ,show (gagTrack gag)
    ,show (gagDamage gag)]

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
             [8, 12, 35, 56, 90, 140, 200, 250]] -- drop

gagDamage :: Gag -> Int
gagDamage gag = gagValues !! track !! gagLevel gag
  where track = fromEnum $ gagTrack gag

gags :: [Gag]
gags = do
  track <- enumFrom ToonUp
  level <- gagLevels
  prestige <- [False, True]
  return $ Gag track level prestige 
  
groupGags :: [Gag] -> [[Gag]]
groupGags = groupBy ((==) `on` gagTrack) . sort

data Toon = Toon { toonHP :: Int } deriving Show -- TODO effects

newToon :: Toon
newToon = Toon 0

data Cog = Cog {
  cogHP :: Int,
  trapped :: Int,
  lured :: Int,
  dazed :: Bool,
  marked :: Bool,
  soaked :: Bool
} deriving Show 

newCog :: Cog
newCog = Cog 0 0 0 False False False

countCogEffects :: Cog -> Int
countCogEffects Cog { dazed = dazed, marked = marked, soaked = soaked } =
  sum $ map fromEnum [dazed, marked, soaked]

type CogState = State Cog Bool

damage :: Int -> CogState
damage value = do
  cog <- get
  let multiplier = if marked cog then 1.1 else 1
  put cog { cogHP = mul multiplier value + cogHP cog }
  return True

isTrapped :: Cog -> Bool
isTrapped cog = trapped cog /= 0

trap :: Int -> CogState
trap damage = do
  cog <- get
  let result = not (isTrapped cog)
  put cog { trapped = if result then damage else 0 }
  return result

untrap :: CogState
untrap = do
  cog <- get
  when (trapped cog == 0) undefined
  put cog { trapped = 0 }
  return True

isLured :: Cog -> Bool
isLured cog = lured cog /= 0

lure :: Int -> CogState
lure knockback = do
  cog <- get
  let expr
        | isLured cog = return False
        | isTrapped cog = do
            damage (trapped cog)
            untrap
            daze
        | otherwise = do
            put cog { lured = knockback } 
            return True
  expr

unlure :: CogState
unlure = do
  cog <- get
  when (lured cog == 0) undefined
  put cog { lured = 0 }
  return True

daze :: CogState
daze = do
  cog <- get
  put cog { dazed = True }
  return True

mark :: CogState
mark = do
  cog <- get
  put cog { marked = True }
  return True

soak :: CogState
soak = do
  cog <- get
  put cog { soaked = True }
  return True

unsoak :: CogState
unsoak = do
  cog <- get
  unless (soaked cog) undefined
  put cog { soaked = False }
  return True

useGags :: [Gag] -> CogState
useGags gags = foldr1 (>>) (useGagTracks <$> groupGags gags)
  where
    -- use gags of the same track
    useGagTracks gags = do
      cog <- get 
      let addKnockback = map (lured cog +)
      case gagTrack (head gags) of
           Trap -> if (length gags == 1 && not (isTrapped cog))
             then do
               let gag = head gags
                   value = gagDamage gag
               if prestiged gag
                  then trap (1.2 `mul` value)
                  else trap value
             else do
               untrap
               return False
           Lure -> do
             let gag = maximumBy (compare `on` gagDamage) gags
                 value = gagDamage gag
             result <-
               if prestiged gag
                  then let multiplier = 
                             if gagLevel gag `mod` 2 == 0
                                then 1.15
                                else 1.25
                       in lure (multiplier `mul` value)
                  else lure value
             return (result && length gags == 1)
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
           Drop -> 
             if isLured cog
                then return False
                else
                  case sequence $ map (dropValue cog) gags of
                       Nothing -> return False
                       Just values -> damage (combo `mul` sum values)
      where
        values = gagDamage <$> gags
        track = gagTrack $ head gags
        combo | length gags == 1 = 1
              | track == Throw = 1.2
              | track == Drop = 1.3
              | otherwise = 1
        dropValue :: Cog -> Gag -> Maybe Int
        dropValue cog gag = (`mul` gagDamage gag) <$> multiplier
          where
            multiplier | gagTrack gag /= Drop = undefined
                       | prestiged gag =
                           if count > 0 
                              then Just $ 1.10 + 0.05 * (count - 1)
                              else Nothing
                       | otherwise = Just 1
              where
                count = fromIntegral $ countCogEffects cog

useGag :: Gag -> CogState
useGag = useGags . pure

heal :: Int -> State Toon ()
heal value = do
  toon <- get
  put toon { toonHP = toonHP toon + value }

useHeal :: Gag -> State Toon ()
useHeal gag = do
  toon <- get
  case gagTrack gag of
       ToonUp ->
         let multiplier = if prestiged gag then 0.4 else 0.25
         in heal (multiplier `mul` value) 
       Throw -> when (prestiged gag) (heal (0.25 `mul` value))
  where
    value = gagDamage gag

data Combo = Combo {
  comboDamage :: Int,
  comboGags :: [Gag]
  -- TODO starting gag
} 

instance Show Combo where
  show (Combo damage gags) = intercalate "\t" $ [show damage, show gags]

comboRep :: Int -> [a] -> [[a]]
comboRep n = concat . take n . tail . foldr f ([[]] : repeat [])
  where
    f x = scanl1 $ (++) . map (x :)

cogCombos :: Int -> [Combo]
cogCombos players = concatMap combosN [1 .. players]
  where
    combosN n = do
      gags <- comboRep n attackGags
      let (result, cog) = runState (useGags gags) newCog
          hp = cogHP cog
      guard result
      return (Combo hp gags)
      where
        attackGags = filter ((/= ToonUp) . gagTrack) gags

toonCombos :: [Combo]   
toonCombos = do
  gag <- healGags
  let hp = toonHP $ execState (useHeal gag) newToon
  return (Combo hp [gag])
  where
    healTracks = [ToonUp, Throw]
    healGags = filter ((`elem` healTracks) . gagTrack) gags

main :: IO ()
main = do
  mapM_ print (cogCombos 2)
