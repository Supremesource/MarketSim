{-# HLINT ignore "Use withFile" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE BlockArguments #-}
module InputOutput where
-- | module where the IO is taking place

-- | external libraries
import           Control.Exception     (bracket)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as BC
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO             (IOMode (AppendMode, WriteMode), hClose,
                                        hPrint, hPutStrLn, openFile)
import           System.Random         (Random (randomRs), mkStdGen)
import           Text.Printf           (printf)
-- | internal libraries
import           Colours               
import           DataTypes
import           Filepaths             
import           Lib
import           RunSettings


-- | IO orderbook
generateOrderBook :: [(Double, Int)] -> [(Double,Int)]
                  -> Double
                  -> Int
                  -> Int
                  -> Double
                  -> IO ()
                  -> String
                  -> Double
                  -> [[Int]]
                  -> Int
                  -> Int
                  -> Int
           
                  -> [(Double,Int)]
                  -> [(Double,Int)]
                  -> [(Double,Int)]
                  -> [(Double,Int)]
                  -> String
                  -> Int
                  -> Double
                  -> String
                  -> String
                  -> String
                  -> IO ()
generateOrderBook
                  bookSpreadFactorAsk
                  bookSpreadFactorBid
                  spread
                  asksTotal
                  bidsTotal
                  bidAskRatio
                  bidAskBenefit
                  logPath
                  startingPoint
                  maxMinLimit
                  totakefromwall
                  lengthchangeBID
                  lengthchangeASK            
                  bidBook
                  askBook
                  listASK
                  listBID
                  volumeSide
                  volumeAmount
                  startingprice
                  pricePath
                  bidBookPath
                  askBookPath
                  = do

-- | random id generator / every run has its own id, for better debugging + finding bugs in logs
  currentTime <- getPOSIXTime
  let seed = round $ currentTime * (10^9) :: Int
  let gen = mkStdGen seed
  let symbols = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "?!@#$&*"
  let randomChars = randomRs (0, length symbols - 1) gen
  let id = map (symbols !!) $ take 10 randomChars

-- | formatinhg the output as a table
-- | random id generator
  let formatRow x y z = B.pack $ purple $ printf "| %-15s | %-15s | %-15s |\n" x y z
  let line = B.pack $ purple $ replicate 54 '-' ++ "\n"

-- | (goes into console)
  B.putStr line
  B.putStr $  formatRow "Field" "Value" "Unit"
  B.putStr line
  B.putStr $ formatRow "ID" id ""
  B.putStr $ formatRow "Spread" (show (roundTo maxDecimal spread)) "$"
  B.putStr $ formatRow "Asks total" (show asksTotal) "$"
  B.putStr $ formatRow "Bids total" (show bidsTotal) "$"
  B.putStr $ formatRow "Bid/Ask ratio" (printf "%.4f" bidAskRatio :: String) ""
  B.putStr $ formatRow "Starting price" (show startingprice) "$"
  B.putStr $ formatRow "Volume side" volumeSide ""
  B.putStr $ formatRow "Volume amount" (show volumeAmount) "$"
  B.putStr $ formatRow "Taken from ASK" (show lengthchangeBID) "$"
  B.putStr $ formatRow "Taken from BID" (show lengthchangeASK) "$"
  B.putStr line


--filewrites1 :: 
-- ? WRITING INTO FILES 1 ? -- 
-- | (goes into log file)
  bracket (openFile logPath AppendMode) hClose $ \handle -> do
    hPutStrLn handle $ printf "%-50s %-20s" "\n\n\nID:" id
    B.hPutStrLn handle $ BC.pack $ printf "%-50s" (allCaps "Code configuration for orderbook:")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "1. Starting price of the whole run:") (show startingPoint ++ "$")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "2. Order book length (to both sides):") (show takeamount)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "3. Ask max up move:")                  (show maxUpMove)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "4. Ask min up move:")               (show minUpMove)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "5. Bid max down move:")             (show maxDownMove)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "6. Bid down min move:")             (show minDownMove)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "7. Minimum value of limit order was (hardcoded):") (show minimum' ++ " (actual = " ++ show (minimumlimit maxMinLimit) ++ ")$")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "8. Maximum value of limit order was (hardcoded):") (show maximum' ++ " (actual = " ++ show (maximumlimit maxMinLimit) ++ ")$")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "9. Bid size of the orderbook:")       (show takeamountBID)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "10. Ask size of the orderbook:")     (show takeamountASK)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "11. ASKS -> BIDS:")                   (show asksTotal ++ "$ / " ++ show bidsTotal ++ "$")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "12. Wall occurrences:")               (show orderwalllikelyhood ++ " (i.e. 10 takeamount -> 2 walls -> to bid, ask)")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps "13. Actually taken to walls:")        (show totakefromwall ++ ", (it is going to get div by 2)")
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"14. Wall minimum:")                    (show wallminimum')
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"15. Wall maximum:")                    (show wallmaximum')
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"16. Wall amplifier:")                  (show wallAmplifier)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"17. Max decimal:")                     (show maxDecimal)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"18. Length change of BID:")            (show lengthchangeBID)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"19. Length change of ASK:")           (show lengthchangeASK)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"24. New Ask List | insertion:")        (show listASK)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"25. New Bid List | insertion:")        (show listBID)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"28. Volume side:")                     volumeSide
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"29. Volume amount:")                  (show volumeAmount)
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"30. Spread: ")                         (show (roundTo maxDecimal spread))
    B.hPutStrLn handle $ BC.pack $ printf "%-50s %-20s" (allCaps"31. The starting price:")             (show startingprice) ++ "\n\n\n"
    B.hPutStrLn handle $ B.pack $ printf  "%-50s %-20s" (allCaps"\n32. 'partial' Orderbook ASK: \n\n") (take 750 (unlines (map show bookSpreadFactorAsk)))
    B.hPutStrLn handle $ B.pack $ printf  "%-50s %-20s" (allCaps"\n33. 'partial' Orderbook BID: \n\n") (take 750 (unlines (map show bookSpreadFactorBid)))
    hClose handle
    B.putStrLn $ BC.pack $ printf "%-50s" "\n\n + Configuration settings successfully written into an external file"

-- ? REWRTING INTO FILES 2 ? --  
-- | Asociated with the orderbook

-- | rewriting price changes
    bracket (openFile pricePath AppendMode) hClose $ \handlePrice -> do
      B.hPutStr handlePrice $ BC.pack "\n"
      B.hPutStrLn handlePrice $ BC.pack (show startingprice)
      hClose handlePrice
-- | rewriting bidbook
    bracket (openFile bidBookPath WriteMode) hClose $ \handleBID -> do
      hPrint handleBID bookSpreadFactorBid
      hClose handleBID
-- | rewriting askbook
    bracket (openFile askBookPath WriteMode) hClose $ \handleASK -> do
      hPrint handleASK bookSpreadFactorAsk
      hClose handleASK
-- | rewriting bid/ask RATIO
    bracket (openFile bidAskRPath AppendMode) hClose $ \handleRatio -> do
      hPutStrLn handleRatio (printf "%.4f" bidAskRatio)
      hClose handleRatio
-- | rewriting bid TO ask RATIO
    bracket (openFile bidToAskRPath AppendMode) hClose $ \handleTORatio -> do
      hPutStrLn handleTORatio (show bidsTotal ++ " / " ++ show asksTotal)
      hClose handleTORatio

-- | printing stats associated with positioning
printPositionStats :: Int -> (TakerTuple, MakerTuple) -> IO (Int, VolumeSide)
printPositionStats i (taker, makers) = do
-- | scope bindings  
-- | volumesum
  let volumeSume = foldl (\acc (x, _) -> acc + x) 0 taker
  let sideVol
        | snd (head taker)         == "x"         || snd (head taker)        == "z"         = Buy
        | snd (head taker)         == "y"         || snd (head taker)        == "f"         = Sell
        | otherwise = error "generating volume failed"
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
  putStrLn $ purple "| Total USD X | " ++ show offX
  putStrLn $ purple "| Total USD Y | " ++ show offY
  putStrLn $ purple "| Total USD Z | " ++ show offZ
  putStrLn $ purple "| Total USD F | " ++ show offF
  putStrLn          "------------------------\n"

-- ?  REWRTING DATA FILES 3 ? -- 
-- | asociated with the positioning
-- | positioning information

-- | total X 
  bracket (openFile newLongsPath AppendMode) hClose $ \handlePosition -> do
        B.hPutStrLn handlePosition $ BC.pack (show offX)
        hClose handlePosition
-- | total Y  
  bracket (openFile newShortsPath AppendMode) hClose $ \handlePosition2 -> do
        B.hPutStrLn handlePosition2 $ BC.pack (show offY)
        hClose handlePosition2
-- | total Z  
  bracket (openFile exitShortsPath AppendMode) hClose $ \handlePosition3 -> do
        B.hPutStrLn handlePosition3 $ BC.pack (show offZ)
        hClose handlePosition3
-- | total F
  bracket (openFile exitLongsPath AppendMode) hClose $ \handlePosition4 -> do
        B.hPutStrLn handlePosition4 $ BC.pack (show offF)
        hClose handlePosition4
-- | Buy volume
  bracket (openFile buyVolumePath AppendMode) hClose $ \handleVol -> do
        B.hPutStrLn handleVol $ BC.pack (show buyVOLUME)
        hClose handleVol
-- | Sell volume
  bracket (openFile sellVolumePath AppendMode) hClose $ \handleVol2 -> do
      B.hPutStrLn handleVol2 $ BC.pack (show sellVOLUME)
      hClose handleVol2
-- | Overal volume
  bracket (openFile volumePath AppendMode) hClose $ \handleVol3 -> do
      B.hPutStrLn handleVol3 $ BC.pack (show overalVOLUME)
      hClose handleVol3
-- | Overal open interest
  bracket (openFile openInterestPath AppendMode) hClose $ \handleInterest -> do
      B.hPutStrLn handleInterest $ BC.pack (show overalOpenInterest)
      hClose handleInterest
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
    offX = orderSize "x" taker + orderSize "x" makers
    offY = orderSize "y" taker + orderSize "y" makers
    offZ = orderSize "z" taker + orderSize "z" makers
    offF = orderSize "f" taker + orderSize "f" makers


-- | overal aggregated data associated with positioning
printStats :: Stats -> IO ()
printStats stats = do
-- | how many takers and makers are there
  let takerCount = [(takerXc stats + takerYc stats + takerFc stats + takerZc stats, " <- count of takers")
                 ,(takerXc stats + takerZc stats, " <- buying")
                 ,(takerYc stats + takerFc stats, " <- selling")
                 ,(takerXc stats + takerZc stats - takerYc stats - takerFc stats, "delta")]
  let makerCount = [(makerXc stats + makerYc stats + makerFc stats + makerZc stats, " <- count of makers")
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
  let longShortRatioLONGS = (fromIntegral overalLongs / fromIntegral (overalLongs + overalShorts)) * 100
  let longShortRatioSHORTS = (fromIntegral overalShorts / fromIntegral (overalLongs + overalShorts)) * 100
  let roundedLongShortRatioL = roundToTwoDecimals longShortRatioLONGS
  let roundedLongShortRatioS = roundToTwoDecimals longShortRatioSHORTS

-- | checking the correcthnes of output
-- | to stop unvanted missinformation

  let checkers = [ ("Checker 1", if (offX stats + offZ stats)  - (offY stats + offF stats) /= 0                        then error "fail 1" else "check 1 pass")
                 , ("Checker 2", if ((offX stats + offY stats) - (offZ stats + offF stats)) `div` 2 /= overallOI stats then error "fail 2" else "check 2 pass")
                 , ("Checker 3", if ((takerX stats + takerZ stats)- (makerY stats + makerF stats)) /= 0                then error "fail 3" else "check 3 pass")
                 , ("Checker 4", if ((takerY stats + takerF stats)- (makerX stats + makerZ stats)) /= 0                then error "fail 4" else "check 4 pass")
                 , ("Checker 5", if (takerX stats + takerZ stats) /= buyVolume stats then error "5 fail"               else "check 5 pass")
                 , ("Checker 6", if (takerY stats + takerF stats) /= sellVolume stats then error "6 fail"              else "check 6 pass")
                 , ("Checker 7", if ((takerX stats + takerY stats + makerX stats + makerY stats) - (takerZ stats + takerF stats + makerZ stats + makerF stats)) `div` 2 /= overallOI stats then error "7 fail" else "check 7 pass")
                 , ("Checker 8", if (takerX stats + takerZ stats) - (makerY stats + makerF stats ) /= 0                then error "check 8 fail" else "check 8 pass")
                 , ("Checker 9", if (takerY stats + takerF stats)- (makerX stats + makerZ  stats ) /= 0                then error "check 9 fail" else "check 9 pass")
                 -- | setting checker
                 , ("Checker 10", if basecaseValueLongNew >= upperBoundLongNew then error "10 fail"       else "check 10 pass")
                 , ("Checker 11", if basecaseValueLongClose >= upperBoundLongClose then error "11 fail"   else "check 11 pass")
                 , ("Checker 12", if basecaseValueShortNew >= upperBoundShortNew then error "12 fail"     else "check 12 pass")
                 , ("Checker 13", if basecaseValueShortClose >= upperBoundShortClose then error "13 fail" else "check 13 pass")
                
                 ]





-- | printing the results formated as a table
  putStrLn $ red "----------------------------"
  putStrLn $ red "| Check        | Result    |"
  putStrLn $ red "----------------------------"
  mapM_ (\(name, result) -> putStrLn $ "| " ++ name ++ " | " ++ result ++ " |") checkers
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
