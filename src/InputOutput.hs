{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DerivingVia #-}

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
module InputOutput
{-
-- ! DESCRIPTION
defines the majority of output operations, including filewritig 
  , json convert etc.
-}
where
-- | module where the IO is taking place

-- | external libraries
import           Colours
import           Control.Monad
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Random            (Random (randomRs), mkStdGen, randomIO)
import           Text.Printf              (printf)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable            (toList)

-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings


generateId :: IO String
generateId = do
  currentTime <- getPOSIXTime
  let seed = round $ currentTime * (10 ^ (9 :: Integer)) :: Int
  -- > RANDOMNESS <
  randomSeed <- randomIO :: IO Int
  let gen = mkStdGen (seed + randomSeed)
  let symbols = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "?!@#$&*"
  let randomChars = randomRs (0, length symbols - 1) gen
  return $ map (symbols !!) $ take 10 randomChars

idList :: Int -> IO [String]
idList size = do
  replicateM size generateId

writeBook :: [BookStats] -> [String] -> IO ()
writeBook statsList idList02 = do
  -- | deleting the old json contents, before writing the updated accumulator
  writeFile orderBookDetailsP ""
  bookStats <- zipWithM writeBookStat statsList idList02
  BL.appendFile orderBookDetailsP (encodePretty bookStats)
  where
    writeBookStat stats identifier = do
        let bidAskRatioStr        = printf "%.4f" $ bidAskRatio stats
        let strSpread = printf ("%." ++ show maxDecimal ++ "f") (spread stats) :: String
        let fileWriteBook         = FileWriteBook
              { identifierBook    = identifier
              , priceBook = startingprice stats
              , bidAskRatioBook   = bidAskRatioStr
              , bidsTotalBook     = bidsTotal stats
              , asksTotalBook     = asksTotal stats
              , maxMinLmtBook     = maxMinLimit stats
              , vSideBook         = vSide stats
              , volumeAmountBook  = volumeAmount stats
              , spreadBook        = strSpread
              }
        return fileWriteBook

writePosition :: [Stats] -> [String] -> IO ()
writePosition statList idList02 = do
  -- | deleting the old json contents, before writing the updated accumulator
  writeFile positionInfoP ""
  let positionStats = zipWith writePositionStat statList idList02 
  BL.appendFile positionInfoP (encodePretty positionStats)
  where
    writePositionStat :: Stats -> String  -> FileWritePosition
    writePositionStat stats identifier = FileWritePosition
      { identifierPosition     = identifier
      , totalXPosAmount        = offX        stats
      , totalYPosAmount        = offY        stats
      , totalZPosAmount        = offZ        stats
      , totalFPosAmount        = offF        stats
      , totalXPosCount         = takerXc     stats + makerXc stats
      , totalYPosCount         = takerYc     stats + makerYc stats
      , totalZPosCount         = takerZc     stats + makerZc stats
      , totalFPosCount         = takerFc     stats + makerFc stats
      , takerXPos              = takerX      stats
      , takerYPos              = takerY      stats
      , takerZPos              = takerZ      stats
      , takerFPos              = takerF      stats
      , makerXPos              = makerX      stats
      , makerYPos              = makerY      stats
      , makerZPos              = makerZ      stats
      , makerFPos              = makerF      stats
      , buyVolumePos           = buyVolume   stats
      , sellVolumePos          = sellVolume  stats
      , overalVolumePos        = totalVolume stats
      , overalOpenInterestPos  = overallOI   stats
      , activatedExitPos       = forceCall stats
      , isVolForcedPos         = isVolForced stats
      }


writeLog :: [BookStats] ->  [String] -> IO ()
writeLog statsList idList02 = do
  logStats <- zipWithM writeStat statsList idList02
  BL.appendFile logP (encodePretty logStats)
  where
    writeStat stats identifier = do
        let maxdownStr = printf ("%." ++ show maxDecimal ++ "f") (maxDownMove) :: String
        let maxupStr = printf ("%." ++ show maxDecimal ++ "f") (maxUpMove) :: String
        let minupStr = printf ("%." ++ show maxDecimal ++ "f") (minUpMove) :: String
        let mindownStr = printf ("%." ++ show maxDecimal ++ "f") (minDownMove) :: String
        let spreadStr = printf ("%." ++ show maxDecimal ++ "f") (spread stats) :: String
        let fileWritesLog = FileWritesLog
              { identifierLOG  = identifier
              , startingPointLOG = startingPoint stats
              , takeamountLOG = takeamount
              , maxUpMoveLOG = maxupStr
              , minUpMoveLOG = minupStr
              , maxDownMoveLOG = maxdownStr
              , minDownMoveLOG =  mindownStr
              , minimum'LOG = minimum'
              , maximum'LOG = maximum'
              , maximumActualLOG = fst (maxMinLimit stats)
              , minimumActualLOG = snd (maxMinLimit stats)
              , takeamountBIDLOG = takeamountBID
              , takeamountASKLOG = takeamountASK
              , asksTotalLOG = asksTotal stats
              , bidsTotalLOG = bidsTotal stats
              , orderwalllikelyhoodLOG = orderwalllikelyhood
              , totakefromwallLOG = totakefromwall stats
              , wallminimum'LOG = wallminimum'
              , wallmaximum'LOG = wallmaximum'
              , wallAmplifierLOG = wallAmplifier
              , maxDecimalLOG = maxDecimal
              , lengthchangeBIDLOG = lengthchangeBID stats
              , lengthchangeASKLOG = lengthchangeASK stats
              , listASKLOG = toList $ listASK stats
              , listBIDLOG = toList $ listBID stats
              , vSideLOG = show (vSide stats)
              , volumeAmountLOG = volumeAmount stats
              , spreadLOG = spreadStr
              , startingPriceLOG = startingprice stats
              } -- append mode
        return fileWritesLog



-- | rewriting bid/ask RATIO
-- | rewriting bid TO ask RATIO
-- | printing stats associated with positioning
printPositionStats ::
     (TakerPositions, MakerPositions) -> IO (Int, VolumeSide)
printPositionStats  (taker, makers)  = do

 -- | checking if maker & taker tuple is negative
  let tupleNegativecheck =
        Control.Monad.when (not (nonNegative taker) && not (nonNegative makers)) $
        error $
        red
          "makers transaction is negative, (something possibly wrong with checker "
    
  tupleNegativecheck
  -- | scope bindings
  -- | volumesum
  let volumeSume = foldl (\acc' (x, _) -> acc' + x) 0 taker
  let sideVol
        | snd (head taker) == "BUY" || snd (head taker) == "BUY" = Buy
        | snd (head taker) == "SELL" || snd (head taker) == "SELL" = Sell
        | otherwise                                          = error $ red
          "generating volume failed"

  -- | goes into console  
  return (volumeSume, sideVol)

--writePositionsToFile :: FilePath -> [PositionData] -> IO ()
--writePositionsToFile newLongsPath' positionDataList = do
--    let jsonData = encodePretty positionDataList
--    BL.writeFile newLongsPath' jsonData




-- | final overview
-- ?  REWRTING DATA FILES 3 ? --
-- | asociated with the positioning
-- | positioning information
-- | total X
-- | return
-- | Maker counters
-- | Taker counters
-- | official X Y Z F values

-- | checks the correctness of output
-- | to stop unwanted misinformation
checkers :: Stats -> [(String, String)]
checkers stats =
  [ ( "Checker 1"
    , if (offX stats + offZ stats) - (offY stats + offF stats) /= 0
        then error $ red "fail 1"
        else "check 1 pass")
  , ( "Checker 2"
    , if ((offX stats + offY stats) - (offZ stats + offF stats)) `div` 2 /=
         overallOI stats
        then error $ red "fail 2"
        else "check 2 pass")
  , ( "Checker 3"
    , if ((takerX stats + takerZ stats) - (makerY stats + makerF stats)) /= 0
        then error $ red "fail 3"
        else "check 3 pass")
  , ( "Checker 4"
    , if ((takerY stats + takerF stats) - (makerX stats + makerZ stats)) /= 0
        then error $ red "fail 4"
        else "check 4 pass")
  , ( "Checker 5"
    , if (takerX stats + takerZ stats) /= buyVolume stats
        then error $ red "5 fail"
        else "check 5 pass")
  , ( "Checker 6"
    , if (takerY stats + takerF stats) /= sellVolume stats
        then error $ red "6 fail"
        else "check 6 pass")
  , ( "Checker 7"
    , if ((takerX stats + takerY stats + makerX stats + makerY stats) -
          (takerZ stats + takerF stats + makerZ stats + makerF stats)) `div`
         2 /=
         overallOI stats
        then error $ red "7 fail"
        else "check 7 pass")
  , ( "Checker 8"
    , if (takerX stats + takerZ stats) - (makerY stats + makerF stats) /= 0
        then error $ red "check 8 fail"
        else "check 8 pass")
  , ( "Checker 9"
    , if (takerY stats + takerF stats) - (makerX stats + makerZ stats) /= 0
        then error $ red "check 9 fail"
        else "check 9 pass")

-- | setting checker
  , ( "Checker 10"
    , if minBuyVol >= maxBuyVol
        then error $ red "10 fail"
        else "check 10 pass")
  , ( "Checker 12"
    , if minSellVol >= maxSellVol
        then error $ red "12 fail"
        else "check 12 pass")
 
  ]

nonNegative :: TakerPositions -> Bool
nonNegative []          = True
nonNegative ((x, _):xs) = (x >= 0) && nonNegative xs



