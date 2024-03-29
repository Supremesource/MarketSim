{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
{-
Supreme Source (c) 2023
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the volumeDistribution.

    * Neither the name of Supreme Source nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Statistics where

-- | module where majoritiy of statistical functions are defined
-- | External libraries
import           Control.Monad (replicateM, unless, when)
import           Data.List     (unfoldr)
import           System.Random (Random (randomR), RandomGen (split), StdGen,
                                randomRIO)
import           Debug.Trace
-- | Internal libraries
import           Colours
import           RunSettings
import           DataTypes


-- whole module has tag = -- > RANDOMNESS <

-- ~ /stat1 
------------------------------------------------------------------------------
-- ? run volume volumeDistribution                                               
-- ! important:                                                             
-- | Modyfiy this function to change the volumeDistribution of the volume                                                       
------------------------------------------------------------------------------

markovChainVolume :: (Int,Int) -> VolumeStage -> IO (Int,VolumeStage)
markovChainVolume (low, high) volStage = do
  phaseRandomiser <- randomRIO (1,5) :: IO Int
  switchPhase <- randomRIO (1,50) :: IO Int
  let initialPhase        = case () of _
                                        | phaseRandomiser <= 1 -> LowVol
                                        | phaseRandomiser <= 2 -> MediumVol
                                        | phaseRandomiser <= 3 -> SpikeVol
                                        | phaseRandomiser <= 4 -> DeadVol
                                        | otherwise -> HighVol

  let maybeRandomVolPhase = case () of _
                                        | switchPhase == 50 -> initialPhase
                                        | otherwise -> volStage
  let volumePhase         = case () of _
                                        | volStage == Undefined || volStage == SpikeVol -> initialPhase
                                        | otherwise -> maybeRandomVolPhase

  volumeAmt <- stageDistributionAux volumePhase (low, high)

  -- local volume check
  Control.Monad.when ((volumeAmt > high) && (volumePhase /= SpikeVol)) $ error
    $ red "volumeAmt is greater than high, try to check if you specified correct volum settings"
  Control.Monad.when (volumeAmt < low) $ error
    $ red "volumeAmt is less than low, try to check if you specified correct volum settings"
  return (volumeAmt, volumePhase)

stageDistributionAux :: VolumeStage -> (Int,Int) -> IO Int
stageDistributionAux stage (low,high) = do
  highSLow <- randomRIO (low,high)
  mediumSHigh <- randomRIO (low,high)
  lowSHigh <- randomRIO (low,mediumSHigh)
  deadSHigh <- randomRIO (low,lowSHigh)
  case stage of
    HighVol -> randomRIO (highSLow, high)
    MediumVol -> randomRIO (low, mediumSHigh)
    LowVol -> randomRIO (low, lowSHigh)
    SpikeVol -> randomRIO (low, 5 * high)
    DeadVol -> randomRIO (low, deadSHigh)
    _ -> error $ red "stage is not valid"

-- | the low and high being passed are:
-- low  - base case either for long or short
-- high - upper limit for the volume
-- the low and heigh are specified by the user in settings

------------------------------------------------------------------------------
-- ? Orderbook volume  volumeDistribution  
-- ~ /stat4                                       
------------------------------------------------------------------------------

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

-- ~  /stat3
------------------------------------------------------------------------------
-- leverage volumeDistribution
-- the code implementation below focuses only on the leverage amount e.g 1x, 2x
-- ? new parameter will be passed, called minimum and maximum leverage side
-- | this new parameter will say what the minimum and maximum leverage is for long and short positions
-- pretty much similar to volume volumeDistribution
-- and then a volumeDistribution is created based on that
------------------------------------------------------------------------------

-- data LeverageStage = HighLeverage | MediumLeverage | LowLeverage | UndefinedLeverage deriving (Show, Eq)



takenLeverage :: {-LeverageStage ->-} Int ->  IO Int
takenLeverage {-levgStage-} baseProbability = do
  Control.Monad.when (baseProbability < 1 || baseProbability > 500) $ error "baseProbability must be between 1 and 500"
 -- EXPLINATION:
  -- | levgState = High -> statistically + 10% of higher leverage
  -- | levgState = Medium -> statistically + 5% of higher leverage
  -- | levgState = Low -> statistically + 0% of higher leverage
 -- lvgHigh <- randomRIO (baseProbability + 50, 500) :: IO Int
 -- lvgMedium <-  randomRIO (baseProbability + 25, 100) :: IO Int
  randomLeverage <- randomRIO (baseProbability, 500) :: IO Int

{-
  switchPhase <- randomRIO (1,5) :: IO Int
  stage <- randomRIO (1,3) :: IO Int
  let rotatedLvgState = case stage of
        1 -> HighLeverage
        2 -> MediumLeverage
        3 -> LowLeverage
        _ -> error "something went wrong in takenLeverage"

  let adjustedLeverageState = if levgStage == UndefinedLeverage || switchPhase == 5 then rotatedLvgState else levgStage
  let passLeverage = case adjustedLeverageState of
        HighLeverage -> lvgHigh
        MediumLeverage -> lvgMedium
        LowLeverage -> lvgLow
        _ -> error "something went wrong in takenLeverage"
-}
  paretoLeverageAux randomLeverage

paretoLeverageAux :: Int -> IO Int
paretoLeverageAux l = do
  when (l < 1 || l > 500) $ error "l must be between 1 and 500"

  let leverage =  l
  return $
    case leverage of
      _
        | leverage >= 1  && leverage <= 305 -> 1
        | leverage > 305 && leverage <= 320 -> 2
        | leverage > 320 && leverage <= 335 -> 3
        | leverage > 335 && leverage <= 350 -> 4
        | leverage > 350 && leverage <= 365 -> 5
        | leverage > 365 && leverage <= 380 -> 6
        | leverage > 380 && leverage <= 395 -> 7
        | leverage > 395 && leverage <= 410 -> 8
        | leverage > 410 && leverage <= 425 -> 9
        | leverage > 425 && leverage <= 430 -> 10
        | leverage > 430 && leverage <= 435 -> 20
        | leverage > 435 && leverage <= 440 -> 30
        | leverage > 440 && leverage <= 445 -> 40
        | leverage > 445 && leverage <= 450 -> 50
        | leverage > 450 && leverage <= 455 -> 60
        | leverage > 455 && leverage <= 460 -> 70
        | leverage > 460 && leverage <= 465 -> 80
        | leverage > 465 && leverage <= 470 -> 90
        | leverage > 470 -> 100
        | otherwise -> error "something went wrong in paretoLeverageAux"

  {-
  return $
    case l of
      _
        | l >= 1 &&  l <= 85 -> 1
        | l >= 86 && l <= 90 -> 2
        | l >= 91 && l <= 93 -> 5
        | l >= 94 && l <= 95 -> 10
        | l == 96 -> 15
        | l == 97 -> 20
        | l == 98 -> 25
        | l == 99 -> 50
        | l == 100 -> 100
        | otherwise -> error "something went wrong in takenLeverage"

-}


-- ~  /stat6
------------------------------------------------------------------------------
-- liquidation even volumeDistribution
-- stop or liquidation
-- user passes probability of stop, that is then used as a base case
-- and then a volumeDistribution is created based on that
------------------------------------------------------------------------------
forceEvent :: Bool -> IO String
forceEvent isStop = if isStop then return "stp" else return "liq"


stopCalculation :: Double -> Double -> IO Double
stopCalculation currentP liqP = do
  let priceArea = abs $ currentP - liqP
  let chunk = priceArea / 100
  randomizer <- randomRIO (1, 100) :: IO Int
  case randomizer of
    _ | randomizer >= 1  && randomizer <= 10  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 5)
      | randomizer >= 11 && randomizer <= 20  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 10)
      | randomizer >= 21 && randomizer <= 30  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 15)
      | randomizer >= 31 && randomizer <= 40  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 20)
      | randomizer >= 41 && randomizer <= 50  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 25)
      | randomizer >= 51 && randomizer <= 60  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 30)
      | randomizer >= 61 && randomizer <= 65  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 35)
      | randomizer >= 66 && randomizer <= 70  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 40)
      | randomizer >= 71 && randomizer <= 75  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 45)
      | randomizer >= 76 && randomizer <= 80  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 50)
      | randomizer >= 81 && randomizer <= 85  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 55)
      | randomizer >= 86 && randomizer <= 88  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 60)
      | randomizer >= 89 && randomizer <= 90  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 65)
      | randomizer >= 91 && randomizer <= 92  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 70)
      | randomizer >= 93 && randomizer <= 94  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 75)
      | randomizer >= 95 && randomizer <= 96  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 80)
      | randomizer >= 97 && randomizer <= 98  -> (currentP +) <$> randomRIO (chunk * 2, chunk * 85)
      | randomizer >= 99 && randomizer <= 100 -> (currentP +) <$> randomRIO (chunk * 2, chunk * 90)
      | otherwise -> error "something went wrong in stopCalculation"




-- ~ /stat5
------------------------------------------------------------------------------ 
-- make a custom volumeDistribution of tick sizes (in order book)
-- in settings there is a minimum and maximum up move
-- also min and max down move for bids
-- make a custom volumeDistribution for this
------------------------------------------------------------------------------ 


-- ~ /stat2
------------------------------------------------------------------------------
-- basically no work with stat 2 is needed
-- user passes probability of closing long (rest that adds up to 100% is for closing shorts)
-- ! no custom volumeDistribution needed, random generator is used intstead
------------------------------------------------------------------------------

-- ~ /stat7
------------------------------------------------------------------------------
-- orderbook walls volumeDistribution amount
-- orderbook maximum order amount is used as a base case
-- then from that base case custom volumeDistribution is used, e.g 20% of times 120 % of the base case etc.
------------------------------------------------------------------------------

-- ~ /stat8
------------------------------------------------------------------------------
-- orderbook wall likelyhood volumeDistribution
-- user specifies aproximate number where orderbook wall should take place
-- e.g every 10 ticks there is a wall, then base case is created and custom volumeDistribution is used
------------------------------------------------------------------------------



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
    dropWhile ((< rnd) . fst) $ scanl1 (\(w1, _) (w2, bVolLong) -> (w1 + w2, bVolLong)) xs

-- bug volume can be to large
generateRandomPosition :: (Int,Int) -> VolumeStage -> IO ({-[(Int, String)],-} [(Int, VolumeSide)], VolumeStage)
generateRandomPosition (buyProbLong, sellProbShort) volStage = do
  -- | for longs 1 - 2

  (bVolLong, stageBUY) <- markovChainVolume (minBuyVol, maxBuyVol) volStage  -- BUY VOL
  --trace (red $ "montecarlo buy " ++ show bVolLong) $ return ()
  --trace (red $ "volStage " ++ show stageBUY) $ return ()
--  f <- volumeDistribution (basecaseValueLongClose, upperBoundLongClose) -- closing longs 2
  -- | for shorts 3 - 4
  (sVolShort,stageSELL) <- markovChainVolume (minSellVol, maxSellVol) volStage -- SELL VOL
  --trace (red $ "montecarlo sell " ++ show sVolShort) $ return ()
  --trace (red $ "volStage " ++ show stageSELL) $ return ()

--  z <- volumeDistribution (basecaseValueShortClose, upperBoundShortClose) -- closing shorts 4
  let takeROptions =
        [ (buyProbLong  ,  (bVolLong, Buy) )
        , (sellProbShort,  (sVolShort, Sell)) ]

  taker' <- weightedRandom takeROptions

  let takerProbab
        | snd taker' == Buy =
          [(buyProbLong, (fst taker', Buy))]
        | snd taker' == Sell =
          [(sellProbShort, (fst taker', Sell))]
        | otherwise = error $ red "taker' is not valid"

  let transactionElements = 1

-- | initialisation of taker and maker lists
  let takerVolumes = [fst taker']
  takerInfos <- replicateM transactionElements (weightedRandom takerProbab)
  let selectedTakers = zip takerVolumes (map snd takerInfos)
  let takerLST = selectedTakers

  let currentStage = if snd taker' == Buy then stageBUY else stageSELL


  return (takerLST {-, makerLST-}, currentStage)

-- | options of takers
-- | Taker and maker probabilitis
-- | local variables
-- | return tuple
