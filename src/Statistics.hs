{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Statistics where

-- | module where majoritiy of statistical functions are defined
-- | External libraries
import           Control.Monad (replicateM)
import           Data.List     (unfoldr)
import           System.Random (Random (randomR), RandomGen (split), StdGen,
                                randomRIO)
-- | Internal libraries
import           Colours
import           RunSettings


-- important:
-- you might want to change the distribution these functions, to your statistical needs
-- | Modyfiy this function to change the distribution of the volume
distribution :: (Int, Int) -> IO Int
distribution (low, high) = do
  x <- randomRIO (1, 100) :: IO Int
  case () of
    _
      | high <= 0 -> randomRIO (0, 0)
      | x == 1 -> randomRIO (low, low)
      | x > 1 && x <= 5 ->
        randomRIO (low, low + round (fromIntegral high * (0.05 :: Double)))
      | x > 5 && x <= 10 ->
        randomRIO (low, low + round (fromIntegral high * (0.8 :: Double)))
      | x > 10 && x <= 15 ->
        randomRIO (low, low + round (fromIntegral high * (0.10 :: Double)))
      | x > 15 && x <= 20 ->
        randomRIO (low, low + round (fromIntegral high * (0.12 :: Double)))
      | x > 20 && x <= 30 ->
        randomRIO (low, low + round (fromIntegral high * (0.18 :: Double)))
      | x > 30 && x <= 40 ->
        randomRIO (low, low + round (fromIntegral high * (0.22 :: Double)))
      | x > 40 && x <= 50 ->
        randomRIO (low, low + round (fromIntegral high * (0.27 :: Double)))
      | x > 50 && x <= 60 ->
        randomRIO (low, low + round (fromIntegral high * (0.32 :: Double)))
      | x > 60 && x <= 70 ->
        randomRIO (low, low + round (fromIntegral high * (0.37 :: Double)))
      | x > 70 && x <= 75 ->
        randomRIO (low, low + round (fromIntegral high * (0.42 :: Double)))
      | x > 75 && x <= 80 ->
        randomRIO (low, low + round (fromIntegral high * (0.55 :: Double)))
      | x > 80 && x <= 85 ->
        randomRIO (low, low + round (fromIntegral high * (0.65 :: Double)))
      | x > 85 && x <= 91 ->
        randomRIO (low, low + round (fromIntegral high * (0.75 :: Double)))
      | x > 91 && x <= 94 ->
        randomRIO (low, low + round (fromIntegral high * (0.90 :: Double)))
      | x > 94 && x <= 96 -> randomRIO (low, high)
      | x > 96 && x <= 98 -> randomRIO (low, 2 * high)
      | x > 98 -> randomRIO (low, 4 * high)
      | otherwise ->
        error $ red "Unexpected value for x in distribution function"

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

-- | Random generator helper funcitions
customRandomRs :: (Int, Int) -> StdGen -> [Int]
customRandomRs bounds gen = nums
  where
    nums = unfoldr (Just . customRandomR bounds) gen




weightedRandom :: [(Int, a)] -> IO a
weightedRandom xs = do
  let totalWeight = sum (map fst xs)
  rnd <- randomRIO (1, totalWeight)
  return $
    snd $
    head $
    dropWhile ((< rnd) . fst) $ scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x)) xs


generateRandomPosition :: [Int] -> IO ([(Int, String)], [(Int, String)])
generateRandomPosition [xProb, yProb]
  -- | for longs 1 - 2
 = do
  x <- distribution (basecaseValueLongNew, upperBoundLongNew) -- BUY VOL
--  f <- distribution (basecaseValueLongClose, upperBoundLongClose) -- closing longs 2
  -- | for shorts 3 - 4
  y <- distribution (basecaseValueShortNew, upperBoundShortNew) -- SELL VOL
--  z <- distribution (basecaseValueShortClose, upperBoundShortClose) -- closing shorts 4
  let takeROptions =
        [ (xProb, (x, "BUY"))
        , (yProb, (y, "SELL"))
--        , (zProb, (z, "z"))
--        , (fProb, (f, "f"))
        ]

  taker' <- weightedRandom takeROptions
--  let maker' = if snd taker' == "BUY" then (fst taker', "SELL") else (fst taker', "BUY")

  let takerProbab
        | snd taker' == "BUY" =
          [(xProb, (fst taker', "BUY"))]
        | snd taker' == "SELL" =
          [(yProb, (fst taker', "SELL"))]
        | otherwise = error $ red "taker' is not valid"
  
  let oppositeAction "BUY" = "SELL"
      oppositeAction "SELL" = "BUY"
      oppositeAction _ = error $ red "Action is not valid"

  let makerAction 
       | snd taker' == "BUY" || snd taker' == "SELL" =
         oppositeAction (snd taker')
       | otherwise = error $ red "taker' is not valid"

  let transactionElements = 1

-- | initialisation of taker and maker lists
  let takerVolumes = [fst taker']
  takerInfos <- replicateM transactionElements (weightedRandom takerProbab)
  let selectedTakers = zip takerVolumes (map snd takerInfos)
  let takerTuple = selectedTakers

  let totalMakerVolume = fst taker'
  let makerVolumes = [totalMakerVolume]
  let makerInfos = replicate transactionElements makerAction
  let selectedMakers = zip makerVolumes makerInfos
  let makerTuple = selectedMakers

  
  return (takerTuple, makerTuple)
-- | options of takers
-- | Taker and maker probabilitis
-- | local variables
-- | return tuple
generateRandomPosition _ =
  error $
  red
    "Input list in `generateRandomPosition` must contain exactly 2 elements"
