{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           System.Random          (Random (randomRs), mkStdGen, randomIO)
import           Text.Printf            (printf)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)

-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings


generateId :: IO String
generateId = do
  currentTime <- getPOSIXTime
  let seed = round $ currentTime * (10 ^ (9 :: Integer)) :: Int
  randomSeed <- randomIO :: IO Int
  let gen = mkStdGen (seed + randomSeed)
  let symbols = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "?!@#$&*"
  let randomChars = randomRs (0, length symbols - 1) gen
  return $ map (symbols !!) $ take 10 randomChars

idList :: IO [String]
idList = do
  replicateM numPositions generateId
  
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
              , startingPriceBook = startingprice stats
              , bidAskRatioBook   = bidAskRatioStr
              , bidsTotalBook     = bidsTotal stats
              , asksTotalBook     = asksTotal stats
              , maxMinLmtBook     = maxMinLimit stats 
              , vSideBook         = vSide stats
              , volumeAmountBook  = volumeAmount stats
              , spreadBook        = strSpread
              }
        return fileWriteBook

writePosition :: [Stats] -> MarginCall -> [String] -> IO ()
writePosition statList marginCall' idList02 = do
  -- | deleting the old json contents, before writing the updated accumulator
  writeFile positionInfoP ""
  let positionStats = zipWith3 writePositionStat statList idList02 marginCall'
  BL.appendFile positionInfoP (encodePretty positionStats)
  where
    writePositionStat :: Stats -> String -> (Int, String, String) -> FileWritePosition
    writePositionStat stats identifier marginCall = FileWritePosition
      { identifierPosition     = identifier
      , totalXPosAmount        = offX        stats 
      , totalYPosAmount        = offY        stats
      , totalZPosAmount        = offZ        stats
      , totalFPosAmount        = offF        stats 
      , totalXPosCount         = takerXc     stats
      , totalYPosCount         = takerYc     stats
      , totalZPosCount         = takerZc     stats
      , totalFPosCount         = takerFc     stats
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
      , liquidationInfoPos     = marginCall
      }


writeLog :: [BookStats] ->  [String] -> IO ()
writeLog statsList idList02 = do
  logStats <- zipWithM writeStat statsList idList02
  BL.appendFile logP (encodePretty logStats)
  where
    writeStat stats identifier = do 
        let fileWritesLog = FileWritesLog
              { identifierLOG  = identifier
              , startingPointLOG = startingPoint stats
              , takeamountLOG = takeamount
              , maxUpMoveLOG = maxUpMove
              , minUpMoveLOG = minUpMove
              , maxDownMoveLOG = maxDownMove
              , minDownMoveLOG = minDownMove
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
              , spreadLOG = roundTo maxDecimal (spread stats)
              , startingPriceLOG = startingprice stats
              } -- append mode
        return fileWritesLog



-- | rewriting bid/ask RATIO
-- | rewriting bid TO ask RATIO
-- | printing stats associated with positioning
printPositionStats ::
    Int -> (TakerPositions, MakerPositions) -> [PositionData] -> IO (Int, VolumeSide, [PositionData])
printPositionStats i (taker, makers) acc = do

 -- | checking if maker & taker tuple is negative
  let tupleNegativecheck =
        Control.Monad.when (not (nonNegative taker) && not (nonNegative makers)) $
        error $
        red
          "makers transaction is negative, (something possibly wrong with checker \
          \ letting you come to this error),                                \
          \ check /settings and input different values, \n                  \
          \     congratulations on getting the rarest error <(|O|_|O|)> "
  tupleNegativecheck
  -- | scope bindings
  -- | volumesum
  let volumeSume = foldl (\acc (x, _) -> acc + x) 0 taker
  let sideVol
        | snd (head taker) == "BUY" || snd (head taker) == "BUY" = Buy
        | snd (head taker) == "SELL" || snd (head taker) == "SELL" = Sell
        | otherwise                                          = error $ red
          "generating volume failed"
  
-- ! turn back on once implementation back
  let overalOpenInterest =
        interestorPlus taker makers - interestorMinus taker makers
  
  let buyVOLUME =
        if sideVol == Buy
          then volumeSume
          else 0
  let sellVOLUME =
        if sideVol == Sell
          then volumeSume
          else 0
  let overalVOLUME                  = volumeSume
  let fileWritesPosition            = PositionData
        {totalXPosition             = totalX
        ,totalYPosition             = totalY
        ,totalZPosition             = totalZ
        ,totalFPosition             = totalF
        ,buyVolumePosition          = buyVOLUME
        ,sellVolumePosition         = sellVOLUME
        ,overalVOLUMEPosition       = overalVOLUME
        ,overalOpenInterestPosition = overalOpenInterest
        }
  let newAcc = acc ++ [fileWritesPosition]

  -- | goes into console  
  return (volumeSume, sideVol, newAcc)
  where
    totalX                    = elementSize "x" taker + elementSize "x" makers
    totalY                    = elementSize "y" taker + elementSize "y" makers
    totalZ                    = elementSize "z" taker + elementSize "z" makers
    totalF                    = elementSize "f" taker + elementSize "f" makers

--writePositionsToFile :: FilePath -> [PositionData] -> IO ()
--writePositionsToFile newLongsPath' positionDataList = do
--    let jsonData = encodePretty positionDataList
--    BL.writeFile newLongsPath' jsonData
  


{-


  B.hPutStrLn handlePosition (BL.toStrict $ encodePretty fileWritesPosition)
  --B.hPutStrLn handlePosition $ BC.pack (show totalX)
  -- | total Y
  B.hPutStrLn handlePosition2 $ BC.pack (show totalY)
  -- | total Z
  B.hPutStrLn handlePosition3 $ BC.pack (show totalZ)
  -- | total F
  B.hPutStrLn handlePosition4 $ BC.pack (show totalF)
  -- | Buy volume
  B.hPutStrLn handleVol $ BC.pack (show buyVOLUME)
  -- | Sell volume
  B.hPutStrLn handleVol2 $ BC.pack (show sellVOLUME)
  -- | Overal volume
  B.hPutStrLn handleVol3 $ BC.pack (show overalVOLUME)
  -- | Overal open interest
  B.hPutStrLn handleInterest $ BC.pack (show overalOpenInterest)
  
 
-} 


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
    , if basecaseValueLongNew >= upperBoundLongNew
        then error $ red "10 fail"
        else "check 10 pass")
  , ( "Checker 11"
    , if basecaseValueLongClose >= upperBoundLongClose
        then error $ red "11 fail"
        else "check 11 pass")
  , ( "Checker 12"
    , if basecaseValueShortNew >= upperBoundShortNew
        then error $ red "12 fail"
        else "check 12 pass")
  , ( "Checker 13"
    , if basecaseValueShortClose >= upperBoundShortClose
        then error $ red "13 fail"
        else "check 13 pass")
  ]

nonNegative :: TakerPositions -> Bool
nonNegative []          = True
nonNegative ((x, _):xs) = (x >= 0) && nonNegative xs


-- | overal aggregated data associated with positioning
printStats :: Stats -> IO ()
printStats stats = do
  let takerCount =
        [ ( takerXc stats + takerYc stats + takerFc stats + takerZc stats
          , " <- count of takers")
        , (takerXc stats + takerZc stats, " <- buying")
        , (takerYc stats + takerFc stats, " <- selling")
        , ( takerXc stats + takerZc stats - takerYc stats - takerFc stats
          , "delta")
        ]
  let makerCount =
        [ ( makerXc stats + makerYc stats + makerFc stats + makerZc stats
          , " <- count of makers")
        , (makerXc stats + makerZc stats, " <- buying")
        , (makerYc stats + makerFc stats, " <- selling")
        , ( makerXc stats + makerZc stats - makerYc stats - makerFc stats
          , "delta")
        ]
  let overalxCount = takerXc stats + makerXc stats
  let overalyCount = takerYc stats + makerYc stats
  let overalzCount = takerZc stats + makerZc stats
  let overalfCount = takerFc stats + makerFc stats
  let overalLongs = overalxCount - overalfCount
  let overalShorts = overalyCount - overalzCount
  let longShortRatioLONGS =
        (fromIntegral overalLongs / fromIntegral (overalLongs + overalShorts)) *
        100 :: Double
  let longShortRatioSHORTS =
        (fromIntegral overalShorts / fromIntegral (overalLongs + overalShorts)) *
        100 :: Double
  let roundedLongShortRatioL = roundToTwoDecimals longShortRatioLONGS :: Double
  let roundedLongShortRatioS = roundToTwoDecimals longShortRatioSHORTS :: Double
  let checkResult = checkers stats
  putStrLn $ red "----------------------------"
  putStrLn $ red "| Check        | Result    |"
  putStrLn $ red "----------------------------"
  mapM_
    (\(name, result) -> putStrLn $ "| " ++ name ++ " | " ++ result ++ " |")
    checkResult
  putStrLn "----------------------------"
 -- | how many takers and makers are there
-- //  let lsprediction = [ (if (takerXc stats + takerZc stats) > (makerXc stats + makerZc stats) then "C up" else "C down", if buyVolume stats > sellVolume stats then "V up" else "V down", if offX stats > offY stats then "A up" else "A down")]
-- | some scope definitions
-- | checking the correcthnes of output
-- | to stop unvanted missinformation

-- | printing the results formated as a table
-- | final IO ()
-- | this function is called by the main loop if we reached the runPrograms
printFinal :: Stats -> IO ()
printFinal aggregatedStats = do
   print "\n" -- get rid of


{- 
  formatAndPrintInfo :: BookStats -> IO ()
formatAndPrintInfo stats = do
  identifier <- generateId
  

  let formatRow x y z =
        B.pack $ printf (lime "| %-15s | %-15s | %-15s |\n") x y z
  let line = B.pack $ lime (replicate 54 '-' ++ "\n")
 

 
  B.putStr line
  B.putStr $ formatRow "Field" "Value" "Unit"
  B.putStr line
  B.putStr $ formatRow "ID" identifier ""
  B.putStr $
    formatRow
      "Spread"
      (printf ("%." ++ show maxDecimal ++ "f") (spread stats) :: String)
      "$"
  B.putStr $ formatRow "Asks total" (show (asksTotal stats)) "$"
  B.putStr $ formatRow "Bids total" (show (bidsTotal stats)) "$"
  B.putStr $
    formatRow "Bid/Ask ratio" (printf "%.4f" (bidAskRatio stats) :: String) ""
  B.putStr $
    formatRow
      "Starting price"
      (printf "%.4f" (startingprice stats) :: String)
      "$" -- If startingPoint is Double
  B.putStr $ formatRow "Volume side" (show (vSide stats)) "" -- If vSide is show-able
  B.putStr $ formatRow "Volume amount" (show (volumeAmount stats)) "$"
  B.putStr $ formatRow "Taken from ASK" (show (lengthchangeBID stats)) "$"
  B.putStr $ formatRow "Taken from BID" (show (lengthchangeASK stats)) "$"
  B.putStr line

-}

{-
  -- ? REWRTING INTO FILES 2 ? --
  -- | Asociated with the orderbook
  -- | rewriting price changes
    bracket (openFile pricePath AppendMode) hClose $ \handlePrice -> do
      B.hPutStr handlePrice $ BC.pack "\n"
      B.hPutStrLn handlePrice $ BC.pack (show $ startingprice stats)
      hClose handlePrice
    bracket (openFile bidAskRPath AppendMode) hClose $ \handleRatio -> do
      
      hPutStrLn handleRatio (printf "%.4f" $ bidAskRatio stats)
      hClose handleRatio
    bracket (openFile bidToAskRPath AppendMode) hClose $ \handleTORatio -> do
      hPutStrLn
        handleTORatio
       
        (show (bidsTotal stats) ++ " / " ++ show (asksTotal stats))
      hClose handleTORatio
-}

  {-
 
  let statsList =
        [ ("Metric", "Value")
        , ("Taker X", show (takerX stats))
        , ("Taker Y", show (takerY stats))
        , ("Taker Z", show (takerZ stats))
        , ("Taker F", show (takerF stats))
        , ("Maker X", show (makerX stats))
        , ("Maker Y", show (makerY stats))
        , ("Maker Z", show (makerZ stats))
        , ("Maker F", show (makerF stats))
        , ("Overall Open Interest", show (overallOI stats))
        , ("Total Volume", show (totalVolume stats))
        , ("Buy Volume", show (buyVolume stats))
        , ("Sell Volume", show (sellVolume stats))
        , ("Count X", show overalxCount)
        , ("Count Y", show overalyCount)
        , ("Count Z", show overalzCount)
        , ("Count F", show overalfCount)
        , ("Taker Count", show takerCount)
        , ("Maker Count", show makerCount)
        , ( "Long Ratio"
          , show overalLongs ++ ", " ++ show roundedLongShortRatioL ++ "%")
        , ( "Short Ratio"
          , show overalShorts ++ ", " ++ show roundedLongShortRatioS ++ "%")
        , ("Value X", show (offX stats) ++ "$")
        , ("Value Y", show (offY stats) ++ "$")
        , ("Value Z", show (offZ stats) ++ "$")
        , ("Value F", show (offF stats) ++ "$")
        ]
  
 
  
  putStrLn $
    red
      "+------------------------------------------------+---------------------------+"
  putStrLn $
    red
      "|                  Metric                        |               Value       |"
  putStrLn $
    red
      "+------------------------------------------------+---------------------------+"
  mapM_
    (\(metric, value) ->
       Text.Printf.printf "| %-50s | %25s |\n" (purple metric) value)
    statsList
  putStrLn
    "+------------------------------------------------+---------------------------+"
  putStrLn "\n"



 -}

   {-
  putStrLn $ "yn| Position number    | " ++ show i ++ "\n\n"
  putStrLn $ "\n| Taker                | \n\n\n" ++ show taker
  putStrLn $ "\n| Makers               | \n\n\n" ++ show makers
   
  putStrLn $ "| Overal open interest | " ++ show overalOpenInterest
  putStrLn $ "| Volume               | " ++ show overalVOLUME
  putStrLn $ "| Buy volume           | " ++ show buyVOLUME
  putStrLn $ "| Sell volume          | " ++ show sellVOLUME
  putStrLn $ "| Taker X count        | " ++ show takercounter_X
  putStrLn $ "| Taker Y count        | " ++ show takercounter_Y
  putStrLn $ "| Taker Z count        | " ++ show takercounter_Z
  putStrLn $ "| Taker F count        | " ++ show takercounter_F
  putStrLn $ "| Maker X count        | " ++ show makerelement_counter_of_X
  putStrLn $ "| Maker Y count        | " ++ show makerelement_counter_of_Y
  putStrLn $ "| Maker Z count        | " ++ show makerelement_counter_of_Z
  putStrLn $ "| Maker F count        | " ++ show makerelement_counter_of_F
  putStrLn "----------TOTAL---------"
  putStrLn $ purple "| Total USD X | " ++ show totalX
  putStrLn $ purple "| Total USD Y | " ++ show totalY
  putStrLn $ purple "| Total USD Z | " ++ show totalZ
  putStrLn $ purple "| Total USD F | " ++ show totalF
  putStrLn "------------------------\n"
    -}
