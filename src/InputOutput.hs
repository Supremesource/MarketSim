{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module InputOutput where
-- | module where the IO is taking place

-- | external libraries
import           Colours
import           Control.Exception.Base (bracket)
import           Control.Monad          (when)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Char8  as BC
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           System.IO              (IOMode (AppendMode), hClose, hPutStrLn,
                                         openFile)
import           System.Random          (Random (randomRs), mkStdGen)
import           Text.Printf            (printf)
-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings



generateId :: IO String
generateId = do
  currentTime <- getPOSIXTime
  let seed = round $ currentTime * (10^(9 :: Integer)) :: Int
  let gen = mkStdGen seed
  let symbols = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "?!@#$&*"
  let randomChars = randomRs (0, length symbols - 1) gen
  return $ map (symbols !!) $ take 10 randomChars

openRewrites3 :: IO RewriteHandle3
openRewrites3 = do
  handlePosition  <- openFile newLongsPath AppendMode
  handlePosition2 <- openFile newShortsPath AppendMode
  handlePosition3 <- openFile exitShortsPath AppendMode
  handlePosition4 <- openFile exitLongsPath AppendMode
  handleVol       <- openFile buyVolumePath AppendMode
  handleVol2      <- openFile sellVolumePath AppendMode
  handleVol3      <- openFile volumePath AppendMode
  handleInterest  <- openFile openInterestPath AppendMode
  -- TODO: put file handles on record
  return (handlePosition, handlePosition2, handlePosition3, handlePosition4, handleVol, handleVol2, handleVol3, handleInterest)

closeHandles3 :: RewriteHandle3 -> IO ()
closeHandles3 (handlePosition, handlePosition2, handlePosition3, handlePosition4, handleVol, handleVol2, handleVol3, handleInterest) = do
  hClose handlePosition
  hClose handlePosition2
  hClose handlePosition3
  hClose handlePosition4
  hClose handleVol
  hClose handleVol2
  hClose handleVol3
  hClose handleInterest


formatAndPrintInfo :: BookStats -> IO ()
formatAndPrintInfo stats = do
  identifier <- generateId
  let formatRow x y z = B.pack $ printf ( lime "| %-15s | %-15s | %-15s |\n") x y z
  let line = B.pack $ lime (replicate 54 '-' ++ "\n")
  B.putStr line
  B.putStr $ formatRow "Field" "Value" "Unit"
  B.putStr line
  B.putStr $ formatRow "ID" identifier ""
  B.putStr $ formatRow "Spread" (printf ("%." ++ show maxDecimal ++ "f") (spread stats) :: String) "$"
  B.putStr $ formatRow "Asks total" (show (asksTotal stats)) "$"
  B.putStr $ formatRow "Bids total" (show (bidsTotal stats)) "$"
  B.putStr $ formatRow "Bid/Ask ratio" (printf "%.4f" (bidAskRatio stats) :: String) ""
  B.putStr $ formatRow "Starting price" (printf "%.4f" (startingprice stats) :: String ) "$" -- If startingPoint is Double
  B.putStr $ formatRow "Volume side"   (show (vSide stats)) "" -- If vSide is show-able
  B.putStr $ formatRow "Volume amount" (show (volumeAmount stats)) "$"
  B.putStr $ formatRow "Taken from ASK" (show (lengthchangeBID stats)) "$"
  B.putStr $ formatRow "Taken from BID" (show (lengthchangeASK stats)) "$"
  B.putStr line


filewrites1 ::   BookStats -> IO ()
filewrites1   stats  = do
 bracket (openFile logPath AppendMode) hClose $ \handle -> do
  identifier <- generateId

  -- ? WRITING INTO FILES 1 ? --
  -- | (goes into log file)

  let doPrint = B.hPutStrLn handle . BC.pack
  hPutStrLn handle $ printf "%-50s %-20s" "\n\n\nID:" identifier
  doPrint $ printf "%-50s" (allCaps "Code configuration for orderbook:")
  doPrint $ printf "%-50s %-20s" (allCaps "1. Starting price of the whole run:") (show (startingPoint stats) ++ "$")
  doPrint $ printf "%-50s %-20s" (allCaps "2. Order book length (to both sides):") (show takeamount)
  doPrint $ printf "%-50s %-20s" (allCaps "3. Ask max up move:")                  (show maxUpMove)
  doPrint $ printf "%-50s %-20s" (allCaps "4. Ask min up move:")               (show minUpMove)
  doPrint $ printf "%-50s %-20s" (allCaps "5. Bid max down move:")             (show maxDownMove)
  doPrint $ printf "%-50s %-20s" (allCaps "6. Bid down min move:")             (show minDownMove)
  doPrint $ printf "%-50s %-20s" (allCaps "7. Minimum value of limit order was (hardcoded):") (show minimum' ++ " (actual = " ++ show (minimumlimit (maxMinLimit stats)) ++ ")$")
  doPrint $ printf "%-50s %-20s" (allCaps "8. Maximum value of limit order was (hardcoded):") (show maximum' ++ " (actual = " ++ show (maximumlimit (maxMinLimit stats)) ++ ")$")
  doPrint $ printf "%-50s %-20s" (allCaps "9. Bid size of the orderbook:")       (show takeamountBID)
  doPrint $ printf "%-50s %-20s" (allCaps "10. Ask size of the orderbook:")     (show takeamountASK)
  doPrint $ printf "%-50s %-20s" (allCaps "11. ASKS -> BIDS:")                   (show  (asksTotal stats) ++ "$ / " ++ show (bidsTotal stats) ++ "$")
  doPrint $ printf "%-50s %-20s" (allCaps "12. Wall occurrences:")               (show orderwalllikelyhood ++ " (i.e. 10 takeamount -> 2 walls -> to bid, ask)")
  doPrint $ printf "%-50s %-20s" (allCaps "13. Actually taken to walls:")        (show (totakefromwall stats) ++ ", (it is going to get div by 2)")
  doPrint $ printf "%-50s %-20s" (allCaps "14. Wall minimum:")                    (show wallminimum')
  doPrint $ printf "%-50s %-20s" (allCaps "15. Wall maximum:")                    (show wallmaximum')
  doPrint $ printf "%-50s %-20s" (allCaps "16. Wall amplifier:")                  (show wallAmplifier)
  doPrint $ printf "%-50s %-20s" (allCaps "17. Max decimal:")                     (show maxDecimal)
  doPrint $ printf "%-50s %-20s" (allCaps "18. Length change of BID:")            (show $ lengthchangeBID stats)
  doPrint $ printf "%-50s %-20s" (allCaps "19. Length change of ASK:")           (show  $ lengthchangeASK stats )
  doPrint $ printf "%-50s %-20s" (allCaps "20. New Ask List | insertion:")        (show $ listASK stats)
  doPrint $ printf "%-50s %-20s" (allCaps "21. New Bid List | insertion:")        (show $ listBID stats)
  doPrint $ printf "%-50s %-20s" (allCaps "22. Volume side:")                     (show $ vSide stats)
  doPrint $ printf "%-50s %-20s" (allCaps "23. Volume amount:")                  (show $ volumeAmount stats)
  doPrint $ printf "%-50s %-20s" (allCaps "24. Spread: ")                         (show (roundTo maxDecimal $ spread stats)) -- TODO fix rounding here
  doPrint $ printf "%-50s %-20s" (allCaps "25. The starting price:")             (show $ startingprice stats) ++ "\n\n\n"
  -- B.hPutStrLn handle $ B.pack $ printf  "%-50s %-20s" (allCaps "\n26. 'partial' Orderbook ASK: \n\n") (take 750 (unlines (map show bookSpreadFactorAsk)))
  -- B.hPutStrLn handle $ B.pack $ printf  "%-50s %-20s" (allCaps "\n27. 'partial' Orderbook BID: \n\n") (take 750 (unlines (map show bookSpreadFactorBid)))
  hClose handle
  -- ? REWRTING INTO FILES 2 ? --
  -- | Asociated with the orderbook
  -- | rewriting price changes
  bracket (openFile pricePath AppendMode) hClose $ \handlePrice -> do
    B.hPutStr handlePrice $ BC.pack "\n"
    B.hPutStrLn handlePrice $ BC.pack (show $ startingprice stats)
    hClose handlePrice

-- | rewriting bid/ask RATIO
  bracket (openFile bidAskRPath AppendMode) hClose $ \handleRatio -> do
    hPutStrLn handleRatio (printf "%.4f" $ bidAskRatio stats)
    hClose handleRatio

-- | rewriting bid TO ask RATIO
  bracket (openFile bidToAskRPath AppendMode) hClose $ \handleTORatio -> do
    hPutStrLn handleTORatio (show (bidsTotal stats) ++ " / " ++ show (asksTotal stats))
    hClose handleTORatio



-- | printing stats associated with positioning
printPositionStats :: RewriteHandle3 -> Int -> (TakerTuple, MakerTuple) -> IO (Int, VolumeSide)
printPositionStats (handlePosition, handlePosition2,handlePosition3,handlePosition4,handleVol,handleVol2,handleVol3, handleInterest) i (taker, makers) = do
  -- | checking if maker & taker tuple is negative
  let tupleNegativecheck = Control.Monad.when (not (nonNegative taker) && not (nonNegative makers)) $ error $ red "makers tuple is negative, (something possibly wrong with checker letting you come to this error), \
                              \ check /settings and input different values, \n congratulations on getting the rarest error <(|O|_|O|)> "
  tupleNegativecheck
  -- | scope bindings
  -- | volumesum
  let volumeSume = foldl (\acc (x, _) -> acc + x) 0 taker
  let sideVol
        | snd (head taker)         == "x"         || snd (head taker)        == "z"         = Buy
        | snd (head taker)         == "y"         || snd (head taker)        == "f"         = Sell
        | otherwise = error $ red "generating volume failed"
  let overalOpenInterest = interestorPlus taker makers - interestorMinus taker makers
  let buyVOLUME = if sideVol == Buy then volumeSume else 0
  let sellVOLUME = if sideVol == Sell then volumeSume else 0
  let overalVOLUME = volumeSume

  -- | goes into console
  putStrLn   "------------------------------------------"
  putStrLn $ "| Position number    | " ++ show i ++ " 🍻 |"
  putStrLn   "------------------------------------------"
  putStrLn $ "| Taker                | " ++ show taker
  putStrLn $ "| Makers               | " ++ show makers
  putStrLn $ "| Overal open interest | " ++ show overalOpenInterest
  putStrLn $ "| Volume               | " ++ show overalVOLUME
  putStrLn $ "| Buy volume           | " ++ show buyVOLUME
  putStrLn $ "| Sell volume          | " ++ show sellVOLUME
  putStrLn   "------------------------"
  putStrLn $ "| Taker X count        | " ++ show takercounter_X
  putStrLn $ "| Taker Y count        | " ++ show takercounter_Y
  putStrLn $ "| Taker Z count        | " ++ show takercounter_Z
  putStrLn $ "| Taker F count        | " ++ show takercounter_F
  putStrLn $ "| Maker X count        | " ++ show makerelement_counter_of_X
  putStrLn $ "| Maker Y count        | " ++ show makerelement_counter_of_Y
  putStrLn $ "| Maker Z count        | " ++ show makerelement_counter_of_Z
  putStrLn $ "| Maker F count        | " ++ show makerelement_counter_of_F
  putStrLn   "------------------------\n"
-- | final overview
  putStrLn          "----------TOTAL---------"
  putStrLn $ purple "| Total USD X | " ++ show totalX
  putStrLn $ purple "| Total USD Y | " ++ show totalY
  putStrLn $ purple "| Total USD Z | " ++ show totalZ
  putStrLn $ purple "| Total USD F | " ++ show totalF
  putStrLn          "------------------------\n"
-- ?  REWRTING DATA FILES 3 ? --
-- | asociated with the positioning
-- | positioning information
-- | total X
  B.hPutStrLn handlePosition $ BC.pack (show totalX)
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
-- | return
  return (volumeSume, sideVol)

    where
-- | Maker counters
    makerelement_counter_of_X = countElements "x" makers
    makerelement_counter_of_Y = countElements "y" makers
    makerelement_counter_of_Z = countElements "z" makers
    makerelement_counter_of_F = countElements "f" makers
-- | Taker counters
    takercounter_X = countElements "x" taker
    takercounter_Y = countElements "y" taker
    takercounter_Z = countElements "z" taker
    takercounter_F = countElements "f" taker
-- | official X Y Z F values
    totalX = orderSize "x" taker + orderSize "x" makers
    totalY = orderSize "y" taker + orderSize "y" makers
    totalZ = orderSize "z" taker + orderSize "z" makers
    totalF = orderSize "f" taker + orderSize "f" makers



-- | checks the correctness of output
-- | to stop unwanted misinformation
checkers :: Stats -> [(String, String)]
checkers stats =
  [ ("Checker 1", if (offX stats + offZ stats)  - (offY stats + offF stats) /= 0                        then error $ red "fail 1" else "check 1 pass")
  , ("Checker 2", if ((offX stats + offY stats) - (offZ stats + offF stats)) `div` 2 /= overallOI stats then error $ red "fail 2" else "check 2 pass")
  , ("Checker 3", if ((takerX stats + takerZ stats)- (makerY stats + makerF stats)) /= 0                then error $ red  "fail 3" else "check 3 pass")
  , ("Checker 4", if ((takerY stats + takerF stats)- (makerX stats + makerZ stats)) /= 0                then error $ red "fail 4" else "check 4 pass")
         , ("Checker 5", if (takerX stats + takerZ stats) /= buyVolume stats                                   then error $ red "5 fail"               else "check 5 pass")
           , ("Checker 6", if (takerY stats + takerF stats) /= sellVolume stats                                  then error $ red  "6 fail"              else "check 6 pass")
             , ("Checker 7", if ((takerX stats + takerY stats + makerX stats + makerY stats) - (takerZ stats + takerF stats + makerZ stats + makerF stats)) `div` 2 /= overallOI stats then error $ red "7 fail" else "check 7 pass")
               , ("Checker 8", if (takerX stats + takerZ stats) - (makerY stats + makerF stats ) /= 0                then error $ red "check 8 fail" else "check 8 pass")
                 , ("Checker 9", if (takerY stats + takerF stats)- (makerX stats + makerZ  stats ) /= 0                then error $ red "check 9 fail" else "check 9 pass")

  -- | setting checker
                   , ("Checker 10", if basecaseValueLongNew >= upperBoundLongNew then error $ red  "10 fail"       else "check 10 pass")
                    , ("Checker 11", if basecaseValueLongClose >= upperBoundLongClose then error $ red  "11 fail"   else "check 11 pass")
                      , ("Checker 12", if basecaseValueShortNew >= upperBoundShortNew then error $ red "12 fail"     else "check 12 pass")
                        , ("Checker 13", if basecaseValueShortClose >= upperBoundShortClose then  error $ red "13 fail" else "check 13 pass")
  ]

nonNegative :: TakerTuple -> Bool
nonNegative []          = True
nonNegative ((x, _):xs) = (x >= 0) && nonNegative xs

-- | overal aggregated data associated with positioning
printStats :: Stats -> IO ()
printStats stats = do
-- | how many takers and makers are there
  let takerCount = [(takerXc stats + takerYc stats + takerFc stats + takerZc stats, " <- count of takers")
                 ,(takerXc stats + takerZc stats, " <- buying")
                 ,(takerYc stats + takerFc stats, " <- selling")
                 ,(takerXc stats + takerZc stats - takerYc stats - takerFc stats, "delta")]
  let makerCount =[(makerXc stats + makerYc stats + makerFc stats + makerZc stats, " <- count of makers")
                  ,(makerXc stats + makerZc stats, " <- buying")
                  ,(makerYc stats + makerFc stats, " <- selling")
                  ,(makerXc stats + makerZc stats - makerYc stats - makerFc stats, "delta")]

-- //  let lsprediction = [ (if (takerXc stats + takerZc stats) > (makerXc stats + makerZc stats) then "C up" else "C down", if buyVolume stats > sellVolume stats then "V up" else "V down", if offX stats > offY stats then "A up" else "A down")]
-- | some scope definitions
  let overalxCount = takerXc stats + makerXc stats
  let overalyCount = takerYc stats + makerYc stats
  let overalzCount = takerZc stats + makerZc stats
  let overalfCount = takerFc stats + makerFc stats
  let overalLongs = overalxCount - overalfCount
  let overalShorts = overalyCount - overalzCount
  let longShortRatioLONGS = (fromIntegral overalLongs / fromIntegral (overalLongs + overalShorts)) * 100 :: Double
  let longShortRatioSHORTS = (fromIntegral overalShorts / fromIntegral (overalLongs + overalShorts)) * 100 :: Double
  let roundedLongShortRatioL = roundToTwoDecimals longShortRatioLONGS  :: Double
  let roundedLongShortRatioS = roundToTwoDecimals longShortRatioSHORTS :: Double

-- | checking the correcthnes of output
-- | to stop unvanted missinformation
  let checkResult = checkers stats


-- | printing the results formated as a table
  putStrLn $ red "----------------------------"
  putStrLn $ red "| Check        | Result    |"
  putStrLn $ red "----------------------------"
  mapM_ (\(name, result) -> putStrLn $ "| " ++ name ++ " | " ++ result ++ " |") checkResult
  putStrLn "----------------------------"
  let statsList = [("Metric", "Value"),
                  ("Taker X", show (takerX stats)),
                  ("Taker Y", show (takerY stats)),
                  ("Taker Z", show (takerZ stats)),
                  ("Taker F", show (takerF stats)),
                  ("Maker X", show (makerX stats)),
                  ("Maker Y", show (makerY stats)),
                  ("Maker Z", show (makerZ stats)),
                  ("Maker F", show (makerF stats)),
                  ("Overall Open Interest", show (overallOI stats)),
                  ("Total Volume", show (totalVolume stats)),
                  ("Buy Volume", show (buyVolume stats)),
                  ("Sell Volume", show (sellVolume stats)),
                  ("Count X", show overalxCount),
                  ("Count Y", show overalyCount),
                  ("Count Z", show overalzCount),
                  ("Count F", show overalfCount),
                  ("Taker Count", show takerCount),
                  ("Maker Count", show makerCount),
                  ("Long Ratio", show overalLongs ++ ", " ++ show roundedLongShortRatioL ++ "%"),
                  ("Short Ratio", show overalShorts ++ ", " ++ show roundedLongShortRatioS ++ "%"),
                  ("Value X", show (offX stats) ++ "$"),
                  ("Value Y", show (offY stats) ++ "$"),
                  ("Value Z", show (offZ stats) ++ "$"),
                  ("Value F", show (offF stats) ++ "$")
                ]
  putStrLn $ red "+------------------------------------------------+---------------------------+"
  putStrLn $ red "|                  Metric                        |               Value       |"
  putStrLn $ red "+------------------------------------------------+---------------------------+"
  mapM_ (\(metric, value) -> Text.Printf.printf "| %-50s | %25s |\n" (purple metric) value) statsList
  putStrLn       "+------------------------------------------------+---------------------------+"
  putStrLn "\n"

-- | final IO ()
-- | this function is called by the main loop if we reached the runs
printFinal :: Stats -> IO ()
printFinal aggregatedStats = do
  putStrLn $ unlines
    [ " "
     , ""
     , ""
     , ""
     , ""
     , "   ████████╗██╗  ██╗███████╗    ███████╗███╗   ██╗██████╗      "
     , "   ╚══██╔══╝██║  ██║██╔════╝    ██╔════╝████╗  ██║██╔══██╗     "
     , "      ██║   ███████║█████╗      █████╗  ██╔██╗ ██║██║  ██║     "
     , "      ██║   ██╔══██║██╔══╝      ██╔══╝  ██║╚██╗██║██║  ██║     "
     , "      ██║   ██║  ██║███████╗    ███████╗██║ ╚████║██████╔╝     "
     , "      ╚═╝   ╚═╝  ╚═╝╚══════╝    ╚══════╝╚═╝  ╚═══╝╚═════╝      "
     , " "
     , ""
     , ""
     , ""
     , ""
     , ""
     , ""
     , ""
     , "   █████╗  ██████╗  ██████╗ ██████╗ ███████╗ ██████╗  █████╗ ████████╗███████╗██████╗  "
     , "  ██╔══██╗██╔════╝ ██╔════╝ ██╔══██╗██╔════╝██╔════╝ ██╔══██╗╚══██╔══╝██╔════╝██╔══██╗ "
     , "  ███████║██║  ███╗██║  ███╗██████╔╝█████╗  ██║  ███╗███████║   ██║   █████╗  ██║  ██║ "
     , "  ██╔══██║██║   ██║██║   ██║██╔══██╗██╔══╝  ██║   ██║██╔══██║   ██║   ██╔══╝  ██║  ██║ "
     , "  ██║  ██║╚██████╔╝╚██████╔╝██║  ██║███████╗╚██████╔╝██║  ██║   ██║   ███████╗██████╔╝ "
     , "  ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═════╝  "
     , "          ███████╗████████╗ █████╗ ████████╗███████╗                                   "
     , "          ██╔════╝╚══██╔══╝██╔══██╗╚══██╔══╝██╔════╝                                   "
     , "          ███████╗   ██║   ███████║   ██║   ███████╗                                   "
     , "          ╚════██║   ██║   ██╔══██║   ██║   ╚════██║                                   "
     , "          ███████║   ██║   ██║  ██║   ██║   ███████║                                   "
     , "          ╚══════╝   ╚═╝   ╚═╝  ╚═╝   ╚═╝   ╚══════╝                                   "
     , ""
     , ""
    ]
  printStats aggregatedStats
  putStrLn $ unlines
      [ " "
      , ""
      , ""
      , ""
      , ""
      , ""
      , ""
      , "  ██████╗ ██████╗ ██████╗ ███████╗██████╗ ██████╗  ██████╗  ██████╗ ██╗  ██╗ "
      , " ██╔═══██╗██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔══██╗██╔═══██╗██╔═══██╗██║ ██╔╝ "
      , " ██║   ██║██████╔╝██║  ██║█████╗  ██████╔╝██████╔╝██║   ██║██║   ██║█████╔╝  "
      , " ██║   ██║██╔══██╗██║  ██║██╔══╝  ██╔══██╗██╔══██╗██║   ██║██║   ██║██╔═██╗  "
      , " ╚██████╔╝██║  ██║██████╔╝███████╗██║  ██║██████╔╝╚██████╔╝╚██████╔╝██║  ██╗ "
      , "  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝╚═╝  ╚═╝╚═════╝  ╚═════╝  ╚═════╝ ╚═╝  ╚═╝ "
      , ""
      , ""
      , ""
      , ""
      , ""
      , ""
      , ""
      , "                     ██╗███╗   ██╗███████╗ ██████╗     "
      , "                     ██║████╗  ██║██╔════╝██╔═══██╗    "
      , "                     ██║██╔██╗ ██║█████╗  ██║   ██║    "
      , "                     ██║██║╚██╗██║██╔══╝  ██║   ██║    "
      , "                     ██║██║ ╚████║██║     ╚██████╔╝    "
      , "                     ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝     "
      , ""
      , ""
      , ""
      ]
