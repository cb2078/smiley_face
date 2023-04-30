import Data.List
import Data.Foldable
import Control.Monad
import Data.Char
import Data.Function

data Effect = Trapped | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag {
  gagTrack :: GagTrack,
  baseDamage :: Integer,
  prestige :: Bool,
  encore :: Float,
  splash :: Float,
  jump :: Float,
  selfHeal :: Float
} deriving (Eq, Ord)
instance Show Gag where
  show gag = foldr1 (++)
    [maybe undefined snd $ find ((encore gag ==) . fst)
      [(1, ""), (soundEncore, "Encore "), (presSoundEncore, "PresEncore "), (soundWinded, "Winded ")]
    ,if prestige gag then "Pres" else ""
    ,show track
    ,show (baseDamage gag)
    ,if jump gag <= (presZapPool / 2) then " SplitPool" else if jump gag /= 1 then " Pool" else ""
    ,if splash gag /= 1 then " Splash" else ""]
    where
      track = gagTrack gag

data Config = Config { hasEncore :: Bool, allowedGags :: [GagTrack], startingGags :: [GagTrack], players :: Integer }
defaultConfig = Config False (enumFrom Trap) [Lure] 2

zapPool = 0.9
presZapPool = 1.1
soundWinded = 0.5
soundEncore = 1.05
presSoundEncore = 1.15
presSingleLure = 1.15
presGroupLure = 1.25
presTrap = 1.2
squirtSplash = 0.25
presSquirtSplash = 0.5

newGag :: GagTrack -> Integer -> Float -> Gag
newGag track damage encore = Gag track damage False encore 1 1 1

damage :: Gag -> Integer
damage gag = (jump gag `mul`) . (splash gag `mul`) . (encore gag `mul`) $ baseDamage gag

gagValues :: [[Integer]]
gagValues = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
              [14, 28, 45, 75, 115, 160, 220, 280], -- trap
              [5, 10, 15, 30, 55, 45, 100, 75], -- lure
              [8, 13, 20, 35, 56, 90, 130, 170], -- throw
              [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
              [12, 20, 36, 60, 90, 140, 180, 240], -- zap
              [5, 10, 16, 23, 30, 50, 70, 90], -- sound
              [8, 12, 35, 56, 90, 140, 200, 240]] -- drop

genGags :: [GagTrack] -> (Gag -> Integer -> [Gag]) -> [Gag]
genGags tracks genGag = do
  track <- tracks
  let i = fromEnum track
  j <- [0..7]
  let value = gagValues !! i !! j
  encore <- [1, soundEncore, presSoundEncore]
  let gag = (newGag track value encore)
  genGag gag value
  
gags, healGags :: [Gag]
gags = genGags (enumFrom Trap) $ \ gag@Gag{ baseDamage = damage } j -> gag :
  case gagTrack gag of
       Trap -> [gag { prestige = True, baseDamage = mul 1.2 damage }]
       Lure -> [gag { prestige = True, baseDamage = mul (if mod j 2 == 0 then presSingleLure else presGroupLure) damage }]
       Squirt -> [gag { splash = squirtSplash }, gag { prestige = True, splash = presSquirtSplash }] -- squirt splash
       Sound -> [gag { encore = soundWinded }] -- winded
       Zap -> do
         pres <- [True, False]
         split <- [0.5, 1]
         let pool = if pres then presZapPool else zapPool
         return gag { prestige = pres, jump = pool * split }
       _ -> []
healGags = genGags [ToonUp, Throw] $ \ gag _ ->
  case gagTrack gag of 
       ToonUp -> gag : [gag { prestige = pres, selfHeal = if pres then 0.4 else 0.25 } | pres <- [False, True]] 
       Throw -> return gag { prestige = True, selfHeal = 0.2 }

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
      track == Trap && (length gags > 1 || trapped cog > 0), -- using trap twice
      track == Lure && (length gags > 1 || lured cog > 0), -- using lure twice
      track == Zap && not (soaked cog) -- dry zap
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
           deriving (Eq, Ord)
instance Show Combo where
  show (Combo dmg gags startingGag) = intercalate " " $
    [show dmg, show gags, maybe "" show startingGag]

findCombos, findHealCombos :: Config -> [Combo]
findCombos (Config hasEncore gagTracks startingGagTracks players) =
  foldMap findCombosN [1..players]
  where
    findCombosN n = do
      startingGag <- Nothing : (Just <$> gagFilter startingGagTracks gags)
      gags <- combR n $ gagFilter gagTracks gags
      Just damage <- return $ fmap hp $ applyGags gags =<<
        (case startingGag of 
             Nothing -> Just
             Just gag -> applyGag gag)
        newCog
      guard $ damage > 0 
      return $ Combo damage gags startingGag
      where
        gagFilter :: [GagTrack] -> [Gag] -> [Gag]
        gagFilter tracks = filter $ \ Gag { encore = encore, gagTrack = track } ->
          (hasEncore || encore <= 1) && elem track tracks
findHealCombos Config { hasEncore = hasEncore, players = players }
  = foldMap findCombosN [1..players]
  where
    findCombosN n = do
      gags <- filter (any ((/= 1) . selfHeal)) $ combR n $
        filter ((hasEncore||) . (<=1) . encore) healGags
      return $ Combo (foldr1 (+) $ map (mul <$> selfHeal <*> damage) gags) gags Nothing

cogLevels = [1..20]

addExes :: [Integer] -> [Integer]
addExes hps = mul <$> [1, 1.5] <*> hps

cogHPs, lbHPs, mgrHPs :: [Integer]
cogHPs = addExes $ (\ n -> (n + 1) * (n + 2)) <$> cogLevels
lbHPs = addExes $ [(\ n -> (n * (n + 1) + 1)), (\ n -> (n + 2) * (n + 3) - 2)] <*> cogLevels
mgrHPs = [240, 320, 465, 600]

main :: IO ()
main = do
  guard (all id runTests)
  putStrLn "damage gags startingGag"
  mapM_ print $ filter ((`elem` cogHPs) . comboDamage) . sort $ findCombos defaultConfig
  mapM_ print $ sort $ findHealCombos defaultConfig
