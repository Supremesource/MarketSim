{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Statistics where

import           Control.Monad (replicateM)
import           Data.List     (unfoldr)
import           System.Random (Random (randomR), RandomGen (split), StdGen,
                                randomRIO)


import           RunSettings


-- | Modyfiy this function to change the distribution of the volume
distribution :: (Int, Int) -> IO Int
distribution (low, high) = do
  x <- randomRIO (1, 100) :: IO Int
  let percentOfHigh = high `div` 100
  print $ "testin  testin: " ++ show x
  case () of
    _ | percentOfHigh <= 0 -> randomRIO (low, 1)
      | x == 1             -> randomRIO (low,   percentOfHigh)
      | x > 1 && x <= 5    -> randomRIO (low, percentOfHigh  * 5)
      | x > 5 && x <= 13   -> randomRIO (low, percentOfHigh  * 20)
      | x > 13 && x <= 33  -> randomRIO (low, percentOfHigh  * 35)
      | x > 33 && x <= 66  -> randomRIO (low, percentOfHigh  * 60)
      | x > 66 && x <= 86  -> randomRIO (low, percentOfHigh  * 85)
      | x > 86 && x <= 96  -> randomRIO (low, percentOfHigh  * 95)
      | x > 96             -> randomRIO (low, high)
      | otherwise          -> error "Unexpected value for x in distribution function"


-- | Orderbook volume probability distribution
customRandomR :: (RandomGen g) => (Int, Int) -> g -> (Int, g)
customRandomR (low, high) gen = (num, gen3)
  where
    (p, gen1) = randomR (0, 1 :: Double) gen
    (gen2, gen3) = split gen1
    num
      | p <= 0.10 = low + round (fromIntegral (high - low) * 0.05 * r1)
      | p <= 0.30 = low + round (fromIntegral (high - low) * 0.15 * r1)
      | p <= 0.50 = low + round (fromIntegral (high - low) * 0.25 * r1)
      | p <= 0.70 = low + round (fromIntegral (high - low) * 0.35 * r1)
      | p <= 0.80 = low + round (fromIntegral (high - low) * 0.45 * r1)
      | p <= 0.85 = low + round (fromIntegral (high - low) * 0.55 * r1)
      | p <= 0.90 = low + round (fromIntegral (high - low) * 0.65 * r1)
      | p <= 0.95 = low + round (fromIntegral (high - low) * 0.75 * r1)
      | p <= 0.98 = low + round (fromIntegral (high - low) * 0.85 * r1)
      | otherwise = low + round (fromIntegral (high - low) * r1)
    (r1, _) = randomR (0, 1 :: Double) gen2
customRandomRs :: (Int, Int) -> StdGen -> [Int]
customRandomRs bounds gen = nums
  where
    nums = unfoldr (Just . customRandomR bounds) gen


weightedRandom :: [(Int, a)] -> IO a
weightedRandom xs = do
  let totalWeight = sum (map fst xs)
  rnd <- randomRIO (1, totalWeight)
  return $ snd $ head $ dropWhile ((< rnd) . fst) $ scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x)) xs

generateVolumes :: Int -> Int -> IO [Int]
generateVolumes numMakers totalVolume = do
  let maxVol = div totalVolume numMakers
  volumes <- replicateM (numMakers - 1) (randomRIO (1, maxVol))
  let lastVolume = totalVolume - sum volumes
  return (volumes ++ [lastVolume])


-- ! parts that can get adjusted
generateRandomPosition :: IO ([(Int, String)], [(Int, String)])
generateRandomPosition = do

  -- | for longs 1 - 2
  x <- distribution (basecaseValueLongNew, upperBoundLongNew)        -- new longs 1
  print $ "_x: " ++ show x
  f <- distribution (basecaseValueLongClose, upperBoundLongClose)    -- closing longs 2
  print $ "_f: " ++ show f
  -- | for shorts 3 - 4
  y <- distribution (basecaseValueShortNew, upperBoundShortNew)      -- new shorts 3
  print $ "_y: " ++ show y
  z <- distribution (basecaseValueShortClose, upperBoundShortClose)  -- closing shorts 4
  print $ "_z: " ++ show z


  let takerOptions :: [(Int, (Int, String))]
      takerOptions = [(xProbabilityTaker
                      , (x, "x"))
                      , (yProbabilityTaker 
                      , (y, "y"))
                      , (zProbabilityTaker
                      , (z, "z"))
                      , (fProbabilityTaker
                      , (f, "f"))] 

  taker' <- weightedRandom takerOptions
  print $ "taker': " ++ show taker'

  let takerProbab
        | snd taker' == "x" = [(yProbabilityTaker, (x, "x")), (zProbabilityTaker, (z, "z"))] 
        | snd taker' == "y" = [(xProbabilityTaker, (y, "y")), (fProbabilityTaker, (f, "f"))]
        | snd taker' == "z" = [(yProbabilityTaker, (z, "z")), (fProbabilityTaker, (x, "x"))]
        | snd taker' == "f" = [(xProbabilityTaker, (f, "f")), (zProbabilityTaker, (y, "y"))]
        | otherwise = error "taker' is not valid"


  let makerProbab
        | snd taker' == "x" = [(yProbabilityMaker, (x, "y")), (fProbabilityMaker, (x, "f"))] 
        | snd taker' == "y" = [(xProbabilityMaker, (y, "x")), (zProbabilityMaker, (y, "z"))]
        | snd taker' == "z" = [(yProbabilityMaker, (z, "y")), (fProbabilityMaker, (z, "f"))]
        | snd taker' == "f" = [(xProbabilityMaker, (f, "x")), (zProbabilityMaker, (f, "z"))]
        | otherwise = error "maker' is not valid"
  let totalMakerVolume = fst taker'



  numMakers <- randomRIO (1, maxmakers) :: IO Int -- select how many makers, filling, FF*
  makerVolumes <- generateVolumes numMakers totalMakerVolume
  makerInfos <- replicateM numMakers (weightedRandom makerProbab)
  let selectedMakers = zip makerVolumes (map snd makerInfos)
  let makerTuple = selectedMakers
  
  numTakers <- randomRIO (1, maxtakers) :: IO Int -- select how many takers, filling, FF*
  takerVolumes <- generateVolumes numTakers (fst taker')
  takerInfos <- replicateM numTakers (weightedRandom takerProbab)
  let selectedTakers = zip takerVolumes (map snd takerInfos)
  let takerTuple = selectedTakers

  return (takerTuple, makerTuple)
