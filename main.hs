import Data.List
import Data.Foldable
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.Function

data Effect = Trapped | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag {
  gagTrack :: GagTrack,
  baseDamage :: Integer,
  iouValue :: Integer,
  prestige :: Bool,
  encore :: Float,
  splash :: Float,
  jump :: Float,
  selfHeal :: Float,
  gagIndex :: Maybe Int -- used for sorting
} deriving Eq
instance Show Gag where
  show gag = foldr1 (++)
    [if iouValue gag > 0 then "IOU" ++ show (iouValue gag) ++ " " else ""
    ,maybe undefined snd $ find ((encore gag ==) . fst)
      [(1, ""), (soundEncore, "Encore "), (presSoundEncore, "PresEncore "), (soundWinded, "Winded ")]
    ,if prestige gag then "Pres" else ""
    ,show track
    ,show (baseDamage gag)
    ,if jump gag <= (presZapPool / 2) then " SplitPool" else if jump gag /= 1 then " Pool" else ""
    ,if splash gag /= 1 then " Splash" else ""]
    where
      track = gagTrack gag
instance Ord Gag where
  g0 `compare` g1 = on compare f g0 g1
    where
      f Gag { gagTrack = track, encore = encore, splash = splash } =
        (negate splash, encore, track)

data Config = Config {
  hasEncore :: Bool,
  hasIOUs :: Bool,
  gagTracks :: [GagTrack],
  startingGagTracks :: [GagTrack],
  players :: Integer
}
defaultConfig = Config False False (enumFrom Lure) [Lure] 2

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

newGag :: GagTrack -> Integer -> Float -> Gag
newGag track damage encore = Gag track damage 0 False encore 1 1 0 Nothing

damage :: Gag -> Integer
damage gag = (splash gag `mul`) . (iouValue gag+) . (jump gag `mul`) . (encore gag `mul`) $ baseDamage gag

index :: Gag -> Int
index Gag { gagIndex = Just i } = i
index Gag { gagIndex = Nothing } = 0

gagValues, iouValues :: [[Integer]]
gagValues = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
             [14, 28, 45, 75, 115, 160, 220, 280], -- trap
             [5, 10, 25, 30, 65, 50, 100, 75], -- lure
             [8, 13, 20, 35, 56, 90, 130, 170], -- throw
             [4, 8, 12, 21, 30, 60, 90, 120], -- squirt
             [12, 20, 36, 60, 90, 140, 190, 240], -- zap
             [5, 10, 16, 23, 30, 50, 70, 90], -- sound
             [8, 12, 35, 56, 90, 140, 200, 240]] -- drop
iouValues = map (15:) -- add rain
  [[25, 35, 60],
   [65, 90, 170],
   [15, 20, 30],
   [30, 40, 70],
   [25, 35, 60],
   [25, 35, 60],
   [15, 20, 35],
   [35, 45, 80]]
genGags ::  (Gag -> Int -> [Gag]) -> Reader Config [Gag]
genGags genGag = do
  Config { hasEncore = hasEncore, hasIOUs = hasIOUs, gagTracks = tracks } <- ask
  return $ do
    track <- tracks
    let i = fromEnum track
    j <- [0..7]
    let value = gagValues !! i !! j
    encore <- 1 : if hasEncore then [soundEncore, presSoundEncore] else mempty
    iou <- 0 : if hasIOUs then iouValues !! i else mempty
    let gag = newGag track value encore 
    genGag gag { gagIndex = Just j, iouValue = iou } j
gags, startingGags, selfHealGags, otherHealGags, healGags :: Reader Config [Gag]
gags = genGags $ \ gag@Gag{ baseDamage = damage } j -> gag :
  case gagTrack gag of
       Trap -> [gag { prestige = True, baseDamage = mul 1.2 damage }]
       Lure -> [gag { prestige = True, baseDamage = mul (if (mod j 2) == 0 then presSingleLure else presGroupLure) damage }]
       Squirt -> [gag { splash = squirtSplash }, gag { prestige = True, splash = presSquirtSplash }] -- squirt splash
       Sound -> [gag { encore = soundWinded }] -- winded
       Zap -> do
         pres <- [True, False]
         split <- [0.5, 1]
         let pool = if pres then presZapPool else zapPool
         return gag { prestige = pres, jump = pool * split }
       _ -> []
startingGags = do
  Config { startingGagTracks = tracks } <- ask
  filter ((`elem` tracks) . gagTrack) <$> gags 
selfHealGags = genGags $ \ gag _ ->
  case gagTrack gag of
       ToonUp -> do
         pres <- [False, True]
         return gag { prestige = pres, selfHeal = if pres then 0.4 else 0.25 }
       Throw -> do
         return gag { prestige = True, selfHeal = 0.2 }
       _ -> mempty
otherHealGags = genGags $ \ gag _ -> return gag { selfHeal = 1 }
healGags = liftM2 (++) selfHealGags otherHealGags

-- needed for combos
groupGags :: [Gag] -> [[Gag]]
-- (hack) do squirt splash separately
groupGags = groupBy $ \ x y ->
  on (==) gagTrack x y &&
  on (==) ((==1) . splash) x y

-- these are the only effects that can't exist at the start of a round
gagEffect :: GagTrack -> Maybe Effect
gagEffect Trap = Just Trapped
gagEffect Throw = Just Marked
gagEffect Lure = Just Lured
gagEffect Squirt = Just Soaked
gagEffect _ = Nothing

gagCombo :: Fractional a => [Gag] -> a
gagCombo gags
  | length gags == 1 = 1
  | elem track [Throw, Squirt] = 1.2
  | track == Drop = 1.3
  | otherwise = 1
  where
    track = gagTrack $ head gags

data Cog = Cog {
  hp :: Integer,
  marked :: Float,
  lured :: Integer,
  trapped :: Integer,
  soaked :: Bool
} deriving (Show, Eq, Ord)
newCog :: Cog
newCog = Cog 0 1 0 0 False

applyDamage :: Integer -> Cog -> Cog
applyDamage dmg cog = cog { hp = marked cog `mul` dmg + hp cog }

-- how TTCC does decimal calculations
mul :: (RealFrac a, Integral b) => a -> b -> b
mul x = ceiling . (x*) . fromIntegral

applyGagTracks :: [Gag] -> Cog -> Maybe Cog
applyGagTracks gags cog 
  | any id [
      (track >= Zap || track == Lure || squirtSplash) && lured cog > 0, -- luring does nothing
      track > Lure && trapped cog > 0, -- trap does nothing
      track == Trap && (length gags > 1 || trapped cog > 0 || lured cog > 0), -- using trap twice or trap on lured cog
      track == Lure && (length gags > 1 || lured cog > 0), -- using lure twice
      track == Zap && not (soaked cog), -- dry zap
      track == Sound && lured cog > 0 -- lure does nothing (again)
  ] = Nothing
  | otherwise = Just $ maybe id applyEffect (gagEffect track) . applyDamage dmg $ cog
  where
    track = gagTrack $ head gags
    squirtSplash = track == Squirt && all ((/=1) . splash) gags
    knockback = if track `elem` [Throw, Squirt] && not squirtSplash then lured cog else 0
    dmg = case track of
               Trap -> 0
               Lure -> trapped cog
               _ -> gagCombo gags `mul` (foldr1 (+) . map ((knockback+) . damage)) gags 
    applyEffect :: Effect -> Cog -> Cog
    applyEffect effect cog =
      case effect of
           Marked -> cog { marked = 1.2, lured = 0 }
           Lured -> if trapped cog > 0
                       then cog { trapped = 0 }
                       else cog { lured = maxDmg }
           Trapped -> cog { trapped = maxDmg, lured = 0 }
           -- TODO squirt splash does not unlure
           Soaked -> cog {
             soaked = True,
             lured = if squirtSplash then lured cog else 0
           }
           _ -> cog { lured = 0 }
      where maxDmg = foldr1 max $ map damage gags

applyGag :: Gag -> Cog -> Maybe Cog
applyGag gag = applyGagTracks [gag] 

applyGags :: [Gag] -> Cog -> Maybe Cog
applyGags = flip (foldrM applyGagTracks) . groupGags . reverse . sort

-- testing
fruitPie = newGag Throw 56 1 
seltzer = newGag Squirt 30 1
bowlingBall = newGag Drop 35 1

type Test = ([Gag], Integer)
tests :: [Test]
tests = [([seltzer, seltzer], 72),
         ([fruitPie, fruitPie], 135),
         ([bowlingBall, bowlingBall], 91)]
runTests :: [Bool]
runTests = map (uncurry test) tests
  where
    test :: [Gag] -> Integer -> Bool
    test gags res = maybe False ((res==) . hp) (applyGags gags newCog)

-- NOTE this may be slow without DP
combR :: Integer -> [a] -> [[a]]
combR 0 _ = [[]]
combR _ [] = []
combR k xxs@(x:xs) = ((x:) <$> combR (k-1) xxs) ++ combR k xs 

data Combo = Combo { comboDamage :: Integer, comboGags :: [Gag], startingGag :: Maybe Gag }
           deriving Eq
instance Show Combo where
  show (Combo dmg gags startingGag) = intercalate " " $
    [show dmg, show gags, maybe "" show startingGag]
instance Ord Combo where
  c0 `compare` c1 = on compare f c0 c1
    where
      f Combo { comboGags = comboGags, startingGag = startingGag } =
        liftM2 div sum length $ map (abs . (subtract 4) . index) $
        comboGags ++ maybe [] (:[]) startingGag

findCombos, findHealCombos :: Reader Config [Combo]
findCombos = do
  config@Config { hasEncore = hasEncore, players = players } <- ask
  gags <- gags
  startingGags <- startingGags
  let findCombosN n = do
        startingGag <- Nothing : map Just startingGags
        gags <- combR n gags
        Just damage <- return $ fmap hp $ applyGags gags =<<
          (case startingGag of 
                Nothing -> Just
                Just gag -> applyGag gag)
          newCog
        guard $ damage > 0 
        return $ Combo damage gags startingGag
  return $ foldMap findCombosN [1..players]
findHealCombos = do
  Config { hasEncore = hasEncore, players = players } <- ask
  selfHealGags <- selfHealGags
  otherHealGags <- otherHealGags
  let findCombosN n = do
        gags <- concat $ liftM2 (:) <$> [selfHealGags, []] <*> pure (combR (n-1) otherHealGags)
        return $ Combo (foldr1 (+) $ map (mul <$> selfHeal <*> damage) gags) gags Nothing
  return $ foldMap findCombosN [1..players]

cogLevels = [1..20]

addExes :: [Integer] -> [Integer]
addExes hps = mul <$> [1, 1.5] <*> hps

cogHPs, lbHPs, mgrHPs :: [Integer]
cogHPs = addExes $ (\ n -> (n + 1) * (n + 2)) <$> cogLevels
lbHPs = addExes $ [(\ n -> (n * (n + 1) + 1)), (\ n -> (n + 2) * (n + 3) - 2)] <*> cogLevels
mgrHPs = [240, 320, 465, 600]

[combos, healCombos] = runReader <$> [findCombos, findHealCombos] <*> pure defaultConfig

search :: [Integer] -> [Combo] -> IO ()
search hps combos = mapM_ print $
  foldMap (\ hp -> filter ((==hp) . comboDamage) combos) $ nub hps

main :: IO ()
main = do
  guard (all id runTests)
  putStrLn "Done"
