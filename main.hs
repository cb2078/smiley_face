import Data.List
import Data.Foldable
import Control.Monad
import Data.Char

data Effect = Dazed | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Zap | Squirt | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag { gagTrack :: GagTrack,
                 damage :: Integer,
                 prestige :: Bool }
         deriving (Eq, Ord)
instance Show Gag where
  show gag =
    (if prestige gag then "Pres" else "") ++
    (show $ gagTrack gag) ++
    (show $ damage gag) 

gagDamage = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
             [14, 28, 45, 75, 115, 160, 220, 280], -- trap
             [5, 10, 15, 30, 55, 45, 100, 75], -- lure
             [8, 13, 20, 35, 56, 90, 130, 170], -- throw
             [12, 20, 36, 60, 90, 140, 180, 240], -- zap
             [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
             [5, 10, 16, 23, 30, 50, 70, 90], -- sound
             [8, 12, 35, 56, 90, 140, 200, 240]] -- drop
gags =
  [Gag (toEnum i :: GagTrack) damage False | i <- [0 .. 7], damage <- gagDamage !! i] ++
  [Gag Lure ((1.15*) -: damage) True | damage <- gagDamage !! 2 ] -- pres lure

-- needed for combos
groupGags :: [Gag] -> [[Gag]]
groupGags = groupBy $ \ x y -> gagTrack x == gagTrack y

-- these are the only effects that can't exist at the start of a round
gagEffect :: GagTrack -> Maybe Effect
gagEffect Trap = Just Dazed
gagEffect Throw = Just Marked
gagEffect Lure = Just Lured
gagEffect _ = Nothing

gagCombo :: Fractional a => [Gag] -> a
gagCombo gags
  | length gags == 1 = 1
  | elem track [Throw, Squirt] = 1.2
  | track == Drop = 1.3
  | otherwise = 1
  where
    track = gagTrack $ head gags

data Cog = Cog { hp :: Integer, marked :: Float, lured :: Integer }
         deriving (Eq, Ord)
instance Show Cog where
  show cog = if lured cog == 0 then "" else "Lure" ++ show (lured cog)
newCog :: Cog
newCog = Cog 0 1 0

applyDamage :: Integer -> Cog -> Cog
applyDamage dmg cog = cog { hp = (marked cog*) -: dmg + hp cog }

applyEffect :: Integer -> Effect -> Cog -> Cog
applyEffect damage effect cog =
  case effect of
       Marked -> cog { marked = 1.2, lured = 0 }
       Lured -> cog { lured = damage }
       _ -> cog

-- how TTCC does decimal calculations
(-:) f x = ceiling . f . fromIntegral $ x

applyGagTracks :: [Gag] -> Cog -> Maybe Cog
applyGagTracks gags cog 
  | ((track >= Zap || track == Lure) && lured cog > 0) = Nothing
  | otherwise = Just $ maybe id (applyEffect $ foldr1 max $ map damage gags) (gagEffect track)  . applyDamage dmg $ cog
  where
    track = gagTrack $ head gags
    knockback = if track `elem` [Throw, Squirt] then lured cog else 0
    dmg = if track == Lure
             then 0
             else (*gagCombo gags) -: (foldr1 (+) . map ((knockback+) . damage)) gags 

applyGag :: Gag -> Cog -> Maybe Cog
applyGag gag = applyGagTracks [gag] 

applyGags :: [Gag] -> Cog -> Maybe Cog
applyGags = flip (foldrM applyGagTracks) . groupGags . reverse . sort

-- testing
fruitPie = (!!4) $ filter ((Throw==) . gagTrack) gags
seltzer = (!!4) $ filter ((Squirt==) . gagTrack) gags
bowlingBall = (!!2) $ filter ((Drop==) . gagTrack) gags

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

type Combo = (Integer, ([Gag], Cog))

findCombos :: [GagTrack] -> Integer -> [Combo]
findCombos tracks players = foldMap findCombosN [1..players]
  where
    cogs = [newCog] ++ do
      knockback <- map damage $ filter ((==Lure) . gagTrack) gags
      return $ applyEffect knockback Lured newCog
    pred gag = elem (gagTrack gag) tracks
    -- TODO remove Nothing results instead of making them zero
    f cog gags = (maybe 0 hp (applyGags gags cog), (gags, cog))
    -- TODO find a better way of doing this
    findCombosN n = foldMap (\ cog -> map (f cog) . combR n $ filter pred gags) cogs

cogLevels = [1..20]

addExes :: [Integer] -> [Integer]
addExes hps = (-:) <$> [id, (1.5*)] <*> hps

cogHPs, lbHPs, mgrHPs :: [Integer]
cogHPs = addExes $ (\ n -> (n + 1) * (n + 2)) <$> cogLevels
lbHPs = addExes $ [(\ n -> (n * (n + 1) + 1)), (\ n -> (n + 2) * (n + 3) - 2)] <*> cogLevels
mgrHPs = [240, 320, 465, 600]

main :: IO ()
main = do
  guard (all id runTests)
  mapM_ pprint $ filter ((`elem` cogHPs) . fst) . filter ((>0) . fst) . sort $ findCombos [Throw,Lure] 2
  where
    pprint :: Combo -> IO ()
    pprint (dmg, (gags, cog)) = putStrLn $ show dmg ++ " " ++ show gags ++ " " ++ show cog
