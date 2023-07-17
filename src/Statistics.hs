{-# OPTIONS_GHC -Wno-missing-export-lists #-}
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
      with the distribution.

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
import           Control.Monad (replicateM, unless)
import           Data.List     (unfoldr)
import           System.Random (Random (randomR), RandomGen (split), StdGen,
                                randomRIO)
-- | Internal libraries
import           Colours
import           RunSettings

-- whole module has tag = -- > RANDOMNESS <

-- ~ /stat1 
------------------------------------------------------------------------------
-- ? run volume distribution                                               
-- ! important:                                                             
-- | Modyfiy this function to change the distribution of the volume                                                       
------------------------------------------------------------------------------

-- | the low and high being passed are:
-- low  - base case either for long or short
-- high - upper limit for the volume
-- the low and heigh are specified by the user in settings
distribution :: (Int, Int) -> IO Int
distribution (low, high) = do
  bVolLong <- randomRIO (1, 100) :: IO Int
  case () of
    _
      | high <= 0 -> randomRIO (0, 0)
      | bVolLong == 1 -> randomRIO (low, low)
      | bVolLong > 1 && bVolLong <= 5 ->
        randomRIO (low, low + round (fromIntegral high * (0.05 :: Double)))
      | bVolLong > 5 && bVolLong <= 10 ->
        randomRIO (low, low + round (fromIntegral high * (0.8 :: Double)))
      | bVolLong > 10 && bVolLong <= 15 ->
        randomRIO (low, low + round (fromIntegral high * (0.10 :: Double)))
      | bVolLong > 15 && bVolLong <= 20 ->
        randomRIO (low, low + round (fromIntegral high * (0.12 :: Double)))
      | bVolLong > 20 && bVolLong <= 30 ->
        randomRIO (low, low + round (fromIntegral high * (0.18 :: Double)))
      | bVolLong > 30 && bVolLong <= 40 ->
        randomRIO (low, low + round (fromIntegral high * (0.22 :: Double)))
      | bVolLong > 40 && bVolLong <= 50 ->
        randomRIO (low, low + round (fromIntegral high * (0.27 :: Double)))
      | bVolLong > 50 && bVolLong <= 60 ->
        randomRIO (low, low + round (fromIntegral high * (0.32 :: Double)))
      | bVolLong > 60 && bVolLong <= 70 ->
        randomRIO (low, low + round (fromIntegral high * (0.37 :: Double)))
      | bVolLong > 70 && bVolLong <= 75 ->
        randomRIO (low, low + round (fromIntegral high * (0.42 :: Double)))
      | bVolLong > 75 && bVolLong <= 80 ->
        randomRIO (low, low + round (fromIntegral high * (0.55 :: Double)))
      | bVolLong > 80 && bVolLong <= 85 ->
        randomRIO (low, low + round (fromIntegral high * (0.65 :: Double)))
      | bVolLong > 85 && bVolLong <= 91 ->
        randomRIO (low, low + round (fromIntegral high * (0.75 :: Double)))
      | bVolLong > 91 && bVolLong <= 94 ->
        randomRIO (low, low + round (fromIntegral high * (0.90 :: Double)))
      | bVolLong > 94 && bVolLong <= 96 -> randomRIO (low, high)
      | bVolLong > 96 && bVolLong <= 98 -> randomRIO (low, 2 * high)
      | bVolLong > 98 -> randomRIO (low, 4 * high)
      | otherwise ->
        error $ red "Unexpected value for bVolLong in distribution function"

------------------------------------------------------------------------------
-- ? Orderbook volume  distribution  
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
-- leverage distribution
-- the code implementation below focuses only on the leverage amount e.g 1x, 2x
-- ? new parameter will be passed, called minimum and maximum leverage side
-- | this new parameter will say what the minimum and maximum leverage is for long and short positions
-- pretty much similar to volume distribution
-- and then a distribution is created based on that
------------------------------------------------------------------------------

takenLeverage :: IO Int
takenLeverage = do
  l <- randomRIO (1, 100) :: IO Int
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




-- ~  /stat6
------------------------------------------------------------------------------
-- liquidation even distribution
-- stop or liquidation
-- user passes probability of stop, that is then used as a base case
-- and then a distribution is created based on that
------------------------------------------------------------------------------
randomLiquidationEvent :: IO String
randomLiquidationEvent = do
  -- > RANDOMNESS <
  randVal <- randomRIO (1, 10) :: IO Int
  unless (stopProb >= 1 && stopProb <= 10) $
    error ("maxStop is 10 you have: " ++ show stopProb)
  return $
    if randVal < stopProb
      then "stp"
      else "liq"


-- ~ /stat5
------------------------------------------------------------------------------ 
-- make a custom distribution of tick sizes (in order book)
-- in settings there is a minimum and maximum up move
-- also min and max down move for bids
-- make a custom distribution for this
------------------------------------------------------------------------------ 


-- ~ /stat2
------------------------------------------------------------------------------
-- basically no work with stat 2 is needed
-- user passes probability of closing long (rest that adds up to 100% is for closing shorts)
-- ! no custom distribution needed, random generator is used intstead
------------------------------------------------------------------------------

-- ~ /stat7
------------------------------------------------------------------------------
-- orderbook walls distribution amount
-- orderbook maximum order amount is used as a base case
-- then from that base case custom distribution is used, e.g 20% of times 120 % of the base case etc.
------------------------------------------------------------------------------

-- ~ /stat8
------------------------------------------------------------------------------
-- orderbook wall likelyhood distribution
-- user specifies aproximate number where orderbook wall should take place
-- e.g every 10 ticks there is a wall, then base case is created and custom distribution is used
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

generateRandomPosition :: [Int] -> IO ([(Int, String)], [(Int, String)])
generateRandomPosition [buyProbLong, sellProbShort]
  -- | for longs 1 - 2
 = do
  bVolLong <- distribution (basecaseValueLongNew, upperBoundLongNew) -- BUY VOL
--  f <- distribution (basecaseValueLongClose, upperBoundLongClose) -- closing longs 2
  -- | for shorts 3 - 4
  sVolShort <- distribution (basecaseValueShortNew, upperBoundShortNew) -- SELL VOL
--  z <- distribution (basecaseValueShortClose, upperBoundShortClose) -- closing shorts 4
  let takeROptions =
        [ (buyProbLong, (bVolLong, "BUY"))
        , (sellProbShort, (sVolShort, "SELL")) ]

  taker' <- weightedRandom takeROptions

  let takerProbab
        | snd taker' == "BUY" =
          [(buyProbLong, (fst taker', "BUY"))]
        | snd taker' == "SELL" =
          [(sellProbShort, (fst taker', "SELL"))]
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
  let takerLST = selectedTakers

  let totalMakerVolume = fst taker'
  let makerVolumes = [totalMakerVolume]
  let makerInfos = replicate transactionElements makerAction
  let selectedMakers = zip makerVolumes makerInfos
  let makerLST = selectedMakers

  
  return (takerLST, makerLST)
-- | options of takers
-- | Taker and maker probabilitis
-- | local variables
-- | return tuple
generateRandomPosition _ =
  error $
  red
    "Input list in `generateRandomPosition` must contain exactly 2 elements"
