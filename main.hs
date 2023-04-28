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
  jump :: Float
} deriving (Eq, Ord)
instance Show Gag where
  show gag = foldr1 (++)
    [case encore gag of
          1 -> ""
          1.05 -> "Encore "
          1.15 -> "PresEncore "
          0.5 -> "Winded "
    ,if prestige gag then "Pres" else ""
    ,show track
    ,show (baseDamage gag)
    ,if jump gag <= 0.55 then " SplitPool" else if jump gag /= 1 then " Pool" else ""
    ,if splash gag /= 1 then " Splash" else ""]
    where
      track = gagTrack gag

newGag :: GagTrack -> Integer -> Float -> Gag
newGag track damage encore = Gag track damage False encore 1 1

damage :: Gag -> Integer
damage gag = (jump gag `mul`) . (splash gag `mul`) . (encore gag `mul`) $ baseDamage gag

gagDamages :: [[Integer]]
gagDamages = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
              [14, 28, 45, 75, 115, 160, 220, 280], -- trap
              [5, 10, 15, 30, 55, 45, 100, 75], -- lure
              [8, 13, 20, 35, 56, 90, 130, 170], -- throw
              [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
              [12, 20, 36, 60, 90, 140, 180, 240], -- zap
              [5, 10, 16, 23, 30, 50, 70, 90], -- sound
              [8, 12, 35, 56, 90, 140, 200, 240]] -- drop

gags, startingGags :: [Gag]
gags = do
  i <- [0..7]
  let track = toEnum i :: GagTrack
  damage <- gagDamages !! i
  encore <- [1] -- [1, 1.05, 1.15]
  let gag = newGag track damage encore
  -- Track specific stuff
  gag : case track of
             Trap -> [gag { prestige = True, baseDamage = mul 1.2 damage }]
             Lure -> [gag { prestige = True, baseDamage = mul 1.15 damage }]
             Squirt -> [gag { splash = 0.25 }, gag { prestige = True, splash = 0.5 }] -- squirt splash
             Sound -> [gag { encore = 0.5 }] -- winded
             Zap -> do
               pres <- [True, False]
               split <- [0.5, 1]
               let pool = if pres then 1.1 else 0.9
               return gag { prestige = pres, jump = pool * split }
             _ -> []
startingGags = filter ((`elem` [Lure]) . gagTrack) gags

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

type Combo = (Integer, ([Gag], Gag))
findCombos :: [GagTrack] -> Integer -> [Combo]
findCombos tracks players = foldMap findCombosN [1..players]
  where
    findCombosN n = do
      -- TODO don't always have to start with starting gag
      startingGag <- startingGags
      gags <- combR n $ filter ((`elem` tracks) . gagTrack) gags
      Just damage <- return $ fmap hp $ applyGags gags =<< applyGag startingGag newCog
      guard $ damage > 0 
      return $ (damage, (gags, startingGag))

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
  mapM_ pprint $ filter ((`elem` cogHPs) . fst) . sort $ findCombos [Zap,Squirt] 2
  where
    pprint :: Combo -> IO ()
    pprint (dmg, (gags, startingGag)) = putStrLn $ intercalate " "
      [show dmg, show gags, show startingGag]
