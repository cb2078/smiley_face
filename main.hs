import Data.List
import Data.Foldable
import Control.Monad
import Data.Char

data Effect = Trapped | Marked | Soaked | Lured | Winded | Encore
              deriving (Eq, Ord, Show)
data GagTrack = ToonUp | Trap | Lure | Throw | Squirt | Zap | Sound | Drop
              deriving (Eq, Ord, Enum, Show)
data Gag = Gag { gagTrack :: GagTrack, baseDamage :: Integer, prestige :: Bool, encore :: Float }
         deriving (Eq, Ord)
instance Show Gag where
  show gag =
    (case encore gag of
          1 -> ""
          1.05 -> "Encore "
          1.15 -> "PresEncore ") ++
    (if prestige gag then "Pres" else "") ++
    (show $ gagTrack gag) ++
    (show $ baseDamage gag) 

damage :: Gag -> Integer
damage gag = (encore gag*) -: baseDamage gag

gagDamages :: [[Integer]]
gagDamages = [[12, 24, 30, 45, 60, 84, 90, 135], -- toon up
              [14, 28, 45, 75, 115, 160, 220, 280], -- trap
              [5, 10, 15, 30, 55, 45, 100, 75], -- lure
              [8, 13, 20, 35, 56, 90, 130, 170], -- throw
              [4, 8, 12, 21, 30, 56, 85, 115], -- squirt
              [12, 20, 36, 60, 90, 140, 180, 240], -- zap
              [5, 10, 16, 23, 30, 50, 70, 90], -- sound
              [8, 12, 35, 56, 90, 140, 200, 240]] -- drop
gags =
  [Gag (toEnum i :: GagTrack) damage False 1 | i <- [0 .. 7], damage <- gagDamages !! i] ++
  [Gag Lure ((1.15*) -: damage) True 1 | damage <- gagDamages !! 2] ++ -- pres lure
  -- TODO trap for exes
  [Gag Trap ((1.2*) -: damage) True 1 | damage <- gagDamages !! 1] -- pres trap

-- needed for combos
groupGags :: [Gag] -> [[Gag]]
groupGags = groupBy $ \ x y -> gagTrack x == gagTrack y

-- these are the only effects that can't exist at the start of a round
gagEffect :: GagTrack -> Maybe Effect
gagEffect Trap = Just Trapped
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

data Cog = Cog { hp :: Integer, marked :: Float, lured :: Integer, trapped :: Integer }
         deriving (Show, Eq, Ord)
newCog :: Cog
newCog = Cog 0 1 0 0

applyDamage :: Integer -> Cog -> Cog
applyDamage dmg cog = cog { hp = (marked cog*) -: dmg + hp cog }

applyEffect :: Integer -> Effect -> Cog -> Cog
applyEffect damage effect cog =
  case effect of
       Marked -> cog { marked = 1.2, lured = 0 }
       Lured -> if trapped cog > 0
                   then cog { trapped = 0 }
                   else cog { lured = damage }
       Trapped -> cog { trapped = damage, lured = 0 }
       _ -> cog { lured = 0 }

-- how TTCC does decimal calculations
(-:) f x = ceiling . f . fromIntegral $ x

-- TODO ignore results where trap doesn't do anything
applyGagTracks :: [Gag] -> Cog -> Maybe Cog
applyGagTracks gags cog 
  | ((track >= Zap || track == Lure) && lured cog > 0 ||
     track > Lure && trapped cog > 0) = Nothing
  | otherwise = Just $ maybe id (applyEffect $ foldr1 max $ map damage gags) (gagEffect track)  . applyDamage dmg $ cog
  where
    track = gagTrack $ head gags
    knockback = if track `elem` [Throw, Squirt] then lured cog else 0
    dmg = case track of
               Trap -> 0
               Lure -> trapped cog
               _ -> (*gagCombo gags) -: (foldr1 (+) . map ((knockback+) . damage)) gags 

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
    cogs = [newCog] ++ foldMap f [(Lure, Lured), (Trap, Trapped)]
      where f (track, effect) = do
              knockback <- map damage $ filter ((==track) . gagTrack) gags
              return $ applyEffect knockback effect newCog
    findCombosN n = do
      cog <- cogs
      let addEncore e gag = gag { encore = e }
      gags <- combR n $
        filter ((`elem` tracks) . gagTrack) $
        addEncore <$> [1, 1.05, 1.15] <*> gags
      Just damage <- return $ hp <$> applyGags gags cog
      guard $ damage > 0 
      return $ (damage, (gags, cog))

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
  mapM_ pprint $ filter ((`elem` cogHPs) . fst) . sort $ findCombos [Throw,Lure] 2
  where
    pprint :: Combo -> IO ()
    pprint (dmg, (gags, cog)) = putStrLn $ intercalate " "
      [show dmg
      ,show gags
      ,if lured cog > 0 then "Lured" ++ show (lured cog) else ""
      ,if trapped cog > 0 then "Trapped" ++ show (trapped cog) else ""]
