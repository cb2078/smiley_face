import Control.Monad
import Control.Monad.State
import Data.Function
import Data.List
import Data.Maybe

-- TODO
-- encore and ious
-- tests
-- double trap duplicate
-- merge cog state and toon state
-- make TU work?

-- how TTCC does decimal calculations
mul :: Integral b => Rational -> b -> b
mul x = ceiling . (x*) . fromIntegral

data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)

data Gag = Gag {
  gagTrack :: GagTrack,
  gagLevel :: Int,
  gagDamage :: Int,
  prestiged :: Bool
} deriving (Eq, Ord)

instance Show Gag where
  show gag = concat
    [if prestiged gag then "Pres " else ""
    ,show track
    ,show baseValue
    ,jump
    ,splash]
    where
      track = gagTrack gag
      value = gagDamage gag
      baseValue = gagValues !! (fromEnum track) !! gagLevel gag
      jump
        | isSplitPool gag = " SplitPool"
        | isPool gag = " Pool"
        | otherwise = ""
      splash = if isSplash gag then " Splash" else ""

isSingleLure, isSplitPool, isPool, isSplash :: Gag -> Bool
isSingleLure gag = gagTrack gag == Lure && 0 == gagLevel gag `mod` 2
isSplitPool gag = gagTrack gag == Zap &&
  fromIntegral (gagDamage gag) < presZapPool / 2
isPool gag = and
  [gagTrack gag == Zap
  ,not $ isSplitPool gag
  ,gagDamage gag /= baseDamage gag]
isSplash gag = gagTrack gag == Squirt && gagDamage gag /= baseDamage gag

zapPool = 0.9 
presZapPool = 1.1
soundWinded = 0.5
soundEncore = 1.06
presSoundEncore = 1.16
presSingleLure = 1.15
presGroupLure = 1.25
presTrap = 1.2
squirtSplash = 0.25
presSquirtSplash = 0.5

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

baseDamage :: Gag -> Int
baseDamage Gag { gagLevel = level, gagTrack = track} =
  gagValues !! fromEnum track !! level

gags :: [Gag]
gags = do
  track <- enumFrom ToonUp
  level <- gagLevels
  prestige <- [False, True]
  return $ pickGag track level prestige

pickGag :: GagTrack -> Int -> Bool -> Gag
pickGag track level prestige = Gag track level value prestige
  where
    value = gagValues !! fromEnum track !! level

addSplash :: Gag -> [Gag]
addSplash gag = do
  guard $ gagTrack gag == Squirt
  let multiplier = if prestiged gag then presSquirtSplash else squirtSplash
  return gag { gagDamage = multiplier `mul` gagDamage gag }

addPool :: Gag -> [Gag]
addPool gag = do
  guard $ gagTrack gag == Zap 
  split <- [0.5, 1]
  let multiplier = if prestiged gag then presZapPool else zapPool
  return gag { gagDamage = (multiplier * split) `mul` gagDamage gag }

addMultiTargetGags :: Gag -> [Gag]
addMultiTargetGags gag = gag : addSplash gag ++ addPool gag

removeRedundantGags :: [Gag] -> [Gag]
removeRedundantGags = filter $ \ gag -> not $ and
  [gagTrack gag `elem` [Squirt, Zap]
  ,prestiged gag
  ,gagDamage gag == baseDamage gag]

otherGag :: Gag -> [Gag]
otherGag gag = do
  case gagTrack gag of
       Lure -> do
         guard $ isSingleLure gag 
         return gag
       Squirt ->
         if isSplash gag
            then return baseGag
            else addSplash gag
       Zap ->
         if isPool gag || isSplitPool gag
            then return baseGag
            else addPool gag
       Sound -> return gag
       _ -> []
  where
    baseGag = gag { gagDamage = baseDamage gag }

groupGags :: [Gag] -> [[Gag]]
groupGags = groupBy ((==) `on` gagTrack) . sort

sortGagGroups :: [[Gag]] -> [[Gag]]
sortGagGroups = sortBy $ on compare (sum . map priority)
  where 
    order = [4, 5, 6, 7, 3, 2, 1, 0]
    priority gag = fromJust . lookup (gagLevel gag) $ zip order [0..]


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

cogLevels = [1..20]

addExes :: [Integer] -> [Integer]
addExes hps = mul <$> [1, 1.5] <*> hps

cogHPs, lbHPs, mgrHPs :: [Integer]
cogHPs = addExes $ (\ n -> (n + 1) * (n + 2)) <$> cogLevels
lbHPs = addExes $ [(\ n -> (n * (n + 1) + 1)), (\ n -> (n + 2) * (n + 3) - 2)] <*> cogLevels
mgrHPs = [240, 320, 465, 600]

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
                  then trap (presTrap `mul` value)
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
                             if isSingleLure gag
                                then presSingleLure
                                else presGroupLure
                       in lure (multiplier `mul` value)
                  else lure value
             return (result && length gags == 1)
           Throw -> do
             damage $ combo `mul` sum (addKnockback values)
             mark
             unlure
             return noPrestiges
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
                else do
                  unlure
                  return False
           Sound -> do
             damage (sum values)
             unlure
             return noPrestiges
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
        noPrestiges = all (not . prestiged) gags

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
} deriving Eq

instance Show Combo where
  show (Combo damage gags) = intercalate "\t" $ [show damage, show gags]

comboRep :: [a] -> [[[a]]]
comboRep = tail . foldr f ([[]] : repeat [])
  where
    f x = scanl1 $ (++) . map (x :)

cogCombos :: [[Combo]]
cogCombos = map combosN (comboRep attackGags)
  where
    combosN gagsN = do
      gags <- sortGagGroups gagsN
      let (result, cog) = runState (useGags gags) newCog
          hp = cogHP cog
      guard result
      return (Combo hp gags)
    attackGags =
      removeRedundantGags $
      addMultiTargetGags =<<
      filter ((/= ToonUp) . gagTrack) gags

otherCombos :: Int -> Combo -> [Combo]
otherCombos players combo = concat . take players $ map (filter pred) cogCombos
  where
    otherGags = replicate (players - length (comboGags combo)) gags
    gagsN = sequence . filter (not . null) $
      otherGags ++ map otherGag (comboGags combo)
    pred combo = comboGags combo `elem` gagsN && comboDamage combo /= 0

toonCombos :: [Combo]   
toonCombos = do
  gag <- healGags
  let hp = toonHP $ execState (useHeal gag) newToon
  return (Combo hp [gag])
  where
    healTracks = [ToonUp, Throw]
    healGags = filter ((`elem` healTracks) . gagTrack) gags

search :: [Int] -> [Combo] -> IO ()
search hps combos = mapM_ print $
  foldMap (\ hp -> filter ((==hp) . comboDamage) combos) $ nub hps

main :: IO ()
main = do
  let players = 2
      combos :: [Combo]
      combos = filter ((== 200) . comboDamage) $ (concat . take players) cogCombos
  mapM_ print combos
  putChar '\n'
  mapM_ print $ otherCombos players =<< combos

