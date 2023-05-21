{-# HLINT ignore "Use withFile" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module InputOutput where

import           Control.Exception     (bracket)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as BC
import           System.IO
import           Text.Printf           (printf)


-- | internal libraries
import           DataTypes
import           Lib
import           RunSettings
import           Filepaths



generateOrderBook :: [(Double, Int)] -> [(Double,Int)]
                  -> Double
                  -> Int
                  -> Int
                  -> Double
                  -> IO ()
                  -> String
                  -> Double
                  ->  [[Int]]
                  -> Int
                  -> Int
                  -> Int
                  -> Double
                  -> Double
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
                  stPriceCaseBid
                  stPriceCaseAsk
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

                  =
                  do
--  B.putStrLn $ B.pack $ allCaps "📚Orderbook ↕️ \n\n (PRICE LEVEL , USD VALUE)\n"
--  B.putStrLn $
--          B.pack $
--            allCaps "\n📕 ask orderbook ⏬\n\n"
--              ++ unlines (map show (reverse bookSpreadFactorAsk))
--              ++ allCaps "\nask ⬆️ \n\n\n"
  B.putStrLn $ B.pack $ allCaps "spread: " ++ show (roundTo maxDecimal spread) ++ " $"
--  B.putStrLn $
--          B.pack $
--            allCaps "\n\n\nbid ⬇️ \n\n"
--              ++ unlines (map show bookSpreadFactorBid)
--              ++ allCaps "\n📗 bid orderbook ⏫"
  B.putStrLn $ B.pack "\n------------------------\n"
  B.putStrLn $ B.pack $ allCaps "Overal data ∑ \n"
  B.putStrLn $
          B.pack $
            allCaps
              ( "ASKS -> "
                  ++ show asksTotal
                  ++ "$ / "
                  ++ show bidsTotal
                  ++ "$ <- BIDS" -- !! BID ASKS (MEANING THEIR USD VALUES)
              )
  B.putStrLn $ B.pack $ "THE BID/ASK RATIO IS =  " ++ printf "%.4f" bidAskRatio -- !! the BID & ASK ratio in terms of order count
  bidAskBenefit
  B.putStrLn $
          B.pack $
             allCaps
              ("\nthe spread was = " ++ show (roundTo maxDecimal spread) ++ "$")

  bracket (openFile logPath WriteMode) hClose $ \handle -> do
    B.hPutStr handle $ BC.pack $ allCaps "code configuration, for orderbook: \n"
    B.hPutStrLn handle $
            BC.pack $
              allCaps "\n  general book info:\n 1. starting price: "
                ++ show startingPoint
                ++ "$"
    B.hPutStrLn handle $
          BC.pack $
              allCaps " 2. Order book length "
                ++ "(to both sides): "
                ++ show takeamount
    B.hPutStrLn handle $
      BC.pack $
              allCaps " 3. ask max up move: "
                ++ show maxUpMove
                ++ allCaps "\n 4. ask min up move: "
                ++ show minUpMove
                ++ allCaps "\n 5. bid max down move: "
                ++ show maxDownMove
                ++ allCaps "\n 6. bid down min move: "
                ++ show minDownMove
    B.hPutStrLn handle $
            BC.pack $
              allCaps
                ( " 7. minumum value of limit order was = "
                    ++ show minimum'
                    ++ " (hardcoded)"
                    ++ " actual      = "
                    ++ show (minimumlimit maxMinLimit)
                )
                ++ "$"
    B.hPutStrLn handle $
            BC.pack $
              allCaps
                ( " 8. maximum value of limit order was = "
                    ++ show maximum'
                    ++ " (hardcoded)"
                    ++ " actual  = "
                    ++ show (maximumlimit maxMinLimit)
                )
                ++ "$"
    B.hPutStrLn handle $
          BC.pack $
              allCaps " 9. bid size of the orderbook: " ++ show takeamountBID
    B.hPutStrLn handle $
          BC.pack $
              allCaps " 10.ask size of the orderbook: " ++ show takeamountASK
    B.hPutStrLn handle $
          BC.pack $
              allCaps " 11.ASKS -> "
                ++ show asksTotal
                ++ "$ / "
                ++ show bidsTotal
                ++ "$ <- BIDS"
    B.hPutStrLn handle $
          BC.pack $
              allCaps "\n  wall info:\n 12. Wall occurrences: "
                ++ show orderwalllikelyhood
                ++ "\n                       ^^   (i.e. 10 takeamount -> 2 walls -> to bid, ask)"
    B.hPutStrLn handle $
          BC.pack $
              " 13. (actuall taken to walls: "
                ++ show totakefromwall
                ++ " , (it is going to get div by 2))"
    B.hPutStrLn handle $ BC.pack $ " 14. wall minimum  = " ++ show wallminimum'
    B.hPutStrLn handle $ BC.pack $ " 15. wall maximum  = " ++ show wallmaximum'
    B.hPutStrLn handle $ BC.pack $ " 16. wall aplifier = " ++ show wallAmplifier
    B.hPutStrLn handle $ BC.pack $ "\n (17. max decimal: " ++ show maxDecimal ++ ")\n"
    -- \|regarding price changes
    B.hPutStrLn handle $
      BC.pack $
        "\n18. Length change of BID : " ++ show lengthchangeBID
  --  B.hPutStrLn handle $ BC.pack $ "19. Lenght change of ASK : " ++ show lengthchangeASK
  --  B.hPutStrLn handle $ BC.pack $ "\n20. Bid starting Price: " ++ show stPriceCaseBid
   -- B.hPutStrLn handle $ BC.pack $ "21. Ask starting Price: " ++ show stPriceCaseAsk
   -- B.hPutStrLn handle $ BC.pack $ "\n22. Bid Book: " ++ show bidBook
   -- B.hPutStrLn handle $ BC.pack $ "23. Ask Book: " ++ show askBook
   -- B.hPutStrLn handle $ BC.pack $ "\n24. New Ask List | insertion : " ++ show listASK
   -- B.hPutStrLn handle $ BC.pack $ "25. New Bid List | insertion: " ++ show listBID
   -- B.hPutStrLn handle $ BC.pack $ "\n26. current ask list = " ++ show bookSpreadFactorAsk
   -- B.hPutStrLn handle $ BC.pack $ "27. current bid list = " ++ show bookSpreadFactorBid
   -- B.hPutStrLn handle $ BC.pack $ "28. volume side: " ++ volumeSide
   -- B.hPutStrLn handle $ BC.pack $ "29. volume amount: " ++ show volumeAmount
   -- B.hPutStrLn handle $ BC.pack $ "\n30. the starting price: " ++ show startingprice
   -- B.hPutStrLn handle $ BC.pack $ "\n\nx" ++ show x
   -- B.hPutStrLn handle $ BC.pack $ "y" ++ show y
   -- B.putStrLn $ BC.pack $ allCaps "the price is: " ++ show startingprice
   -- B.putStrLn $ BC.pack $ allCaps "volume side was: " ++ show volumeSide
   -- B.putStrLn $ BC.pack $ allCaps "volume amount was: " ++ show volumeAmount
    hClose handle
    B.putStrLn $
      BC.pack
        "\n\n + configuration settings successfuly written into an external file 🦄"

    -- ! 🔴1  REWRTING DATA FILES | BID ASK BOOKS | PRICE CHANGES | LOGS
    -- //? rewriting price changes
    bracket (openFile pricePath AppendMode) hClose $ \handlePrice -> do
      B.hPutStr handlePrice $ BC.pack "\n"
      B.hPutStrLn handlePrice $ BC.pack (show startingprice)
      hClose handlePrice
      -- //?? rewriting bidbook
  bracket (openFile bidBookPath WriteMode) hClose $ \handleBID -> do
    hPrint handleBID bookSpreadFactorBid
    hClose handleBID

  -- //? rewriting askbook
  bracket (openFile askBookPath WriteMode) hClose $ \handleASK -> do
    hPrint handleASK bookSpreadFactorAsk
    hClose handleASK



color :: Int -> String -> String
color code text = "\x1b[" ++ show code ++ "m" ++ text ++ "\x1b[0m"
red :: String -> String
red = color 31
blue :: String -> String
blue = color 34
green :: String -> String
green = color 32
orange :: String -> String
orange = color 33
purple :: String -> String
purple = color 35

printPositionStats :: Int -> (TakerTuple, MakerTuple) -> IO (Int, VolumeSide)
printPositionStats i (taker, makers) = do
  putStrLn $ "Position number : " ++ show i ++ "🍻"
  putStrLn $ "\n〇Taker: " ++ show taker
  putStrLn $ "〇Makers: " ++ show makers ++ "\n"

  let voL = foldl (\acc (x, _) -> acc + x) 0 taker

  let sideVol
        | snd (head taker)         == "x"         || snd (head taker)        == "z"         = Buy
        | snd (head taker)         == "y"         || snd (head taker)        == "f"         = Sell
        | otherwise = error "generating volume failed"

  putStrLn $ "the volume is: " ++ show voL ++ " , " ++ "and the side of the volume is: " ++ show sideVol ++ "\n"



  -- Additional stats for a single position

  putStrLn $ "〇Taker X_count: " ++ show takercounter_X
  putStrLn $ "〇Taker Y_count: " ++ show takercounter_Y
  putStrLn $ "〇Taker Z_count: " ++ show takercounter_Z
  putStrLn $ "〇Taker F_count: " ++ show takercounter_F ++ "\n"



  putStrLn $ "〇Maker X_count: " ++ show makerelement_counter_of_X
  putStrLn $ "〇Maker Y_count: " ++ show makerelement_counter_of_Y
  putStrLn $ "〇Maker Z_count: " ++ show makerelement_counter_of_Z
  putStrLn $ "〇Maker F_count: " ++ show makerelement_counter_of_F ++ "\n"

  putStrLn $ "〇x " ++ show offX
  putStrLn $ "〇y " ++ show offY
  putStrLn $ "〇z " ++ show offZ
  putStrLn $ "〇f " ++ show offF ++ "\n\n---------\n"

-- ! 🔴2  REWRTING DATA FILES 
-- | positioning information
  bracket (openFile newLongsPath AppendMode) hClose $ \handlePosition -> do
        B.hPutStr handlePosition $ BC.pack "\n"
        B.hPutStrLn handlePosition $ BC.pack (show offX)
        hClose handlePosition

  bracket (openFile newShortsPath AppendMode) hClose $ \handlePosition2 -> do
        B.hPutStr handlePosition2 $ BC.pack "\n"
        B.hPutStrLn handlePosition2 $ BC.pack (show offY)
        hClose handlePosition2

  bracket (openFile exitShortsPath AppendMode) hClose $ \handlePosition3 -> do
        B.hPutStr handlePosition3 $ BC.pack "\n"
        B.hPutStrLn handlePosition3 $ BC.pack (show offZ)
        hClose handlePosition3

  bracket (openFile exitLongsPath AppendMode) hClose $ \handlePosition4 -> do
        B.hPutStr handlePosition4 $ BC.pack "\n"
        B.hPutStrLn handlePosition4 $ BC.pack (show offF)
        hClose handlePosition4


    

  
    
  return (voL, sideVol)

    where
    -- Maker counters
    makerelement_counter_of_X = countElements "x" makers
    makerelement_counter_of_Y = countElements "y" makers
    makerelement_counter_of_Z = countElements "z" makers
    makerelement_counter_of_F = countElements "f" makers
    -- Taker counters
    takercounter_X = countElements "x" taker
    takercounter_Y = countElements "y" taker
    takercounter_Z = countElements "z" taker
    takercounter_F = countElements "f" taker
   -- official X Y Z F values
    offX = orderSize "x" taker + orderSize "x" makers
    offY = orderSize "y" taker + orderSize "y" makers
    offZ = orderSize "z" taker + orderSize "z" makers
    offF = orderSize "f" taker + orderSize "f" makers
-- overal aggregated

printStats :: Stats -> IO ()
printStats stats = do


  let takerCount = [(takerXc stats + takerYc stats + takerFc stats + takerZc stats, " <- count of takers")
                 ,(takerXc stats + takerZc stats, " <- buying")
                 ,(takerYc stats + takerFc stats, " <- selling")
                 ,(takerXc stats + takerZc stats - takerYc stats - takerFc stats, "delta")]
  let makerCount = [(makerXc stats + makerYc stats + makerFc stats + makerZc stats, " <- count of makers")
                 ,(makerXc stats + makerZc stats, " <- buying")
                 ,(makerYc stats + makerFc stats, " <- selling")
                 ,(makerXc stats + makerZc stats - makerYc stats - makerFc stats, "delta")]
  let lsprediction = [ (if (takerXc stats + takerZc stats) > (makerXc stats + makerZc stats) then "C up" else "C down", if buyVolume stats > sellVolume stats then "V up" else "V down", if offX stats > offY stats then "A up" else "A down")]
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


  let checker1 = if (offX stats + offZ stats)  - (offY stats + offF stats) /= 0 then error "fail 1" else "check 1 pass"
  let checker2 = if ((offX stats + offY stats) - (offZ stats + offF stats)) `div` 2 /= overallOI stats then error "fail 2" else "check 2 pass"
  let checker3 = if ((takerX stats + takerZ stats)- (makerY stats + makerF stats)) /= 0 then error "fail 3" else "check 3 pass"
  let checker4 = if ((takerY stats + takerF stats)- (makerX stats + makerZ stats)) /= 0 then error "fail 4" else "check 4 pass"
  let checker5 = if (takerX stats + takerZ stats) /= buyVolume stats then error "5 fail" else "check 5 pass"
  let checker6 = if (takerY stats + takerF stats) /= sellVolume stats then error "6 fail" else "check 6 pass"
  let checker7 = if ((takerX stats + takerY stats + makerX stats + makerY stats) - (takerZ stats + takerF stats + makerZ stats + makerF stats)) `div` 2 /= overallOI stats then error "7 fail" else "check 7 pass"
  let checker8 = if (takerX stats + takerZ stats) - (makerY stats + makerF stats ) /= 0 then error "check 8 fail" else "check 8 pass"
  let checker9 = if (takerY stats + takerF stats)- (makerX stats + makerZ  stats ) /= 0 then error "check 9 fail" else "check 9 pass"
 -- add volume delta

  print checker1
  print checker2
  print checker3
  print checker4
  print checker5
  print checker6
  print checker7
  print checker8
  print checker9
  

  print $ "x " ++ show  (takerX stats)
  print $ "y " ++ show  (takerY stats)
  print $ "z " ++ show  (takerZ stats)
  print $ "f " ++ show  (takerF stats)
  print $ "x' " ++ show (makerX stats)
  print $ "y' " ++ show (makerY stats)
  print $ "z' " ++ show (makerZ stats)
  print $ "f' " ++ show (makerF stats)
--  print takerX
  putStrLn "+--------------------------------+---------------------------+\n|                  Metric        |               Value         |\n+--------------------------------+---------------------------+"
  Text.Printf.printf "| 💹 %s  | %13s |\n" (purple "Overall Open Interest               ") (show  (overallOI stats))
  Text.Printf.printf "| 🔄 %s  | %13s |\n" (blue "Total Volume                        ") (show (totalVolume stats))
  Text.Printf.printf "| 🔼 %s  | %13s |\n" (green "- Buy Volume                        ") (show (buyVolume stats))
  Text.Printf.printf "| 🔽 %s  | %13s |\n" (red "- Sell Volume                       ") (show (sellVolume stats))
  putStrLn "|----------------------------------------------------------"
  Text.Printf.printf "| 🅰️  %s  | %13s |\n" (red "Count X                             ") (show overalxCount)
  Text.Printf.printf "| 🅱️  %s    | %13s |\n" (red "Count Y                           ") (show overalyCount)
  Text.Printf.printf "| 🅾️  %s  | %13s |\n" (red "Count Z                             ") (show overalzCount)
  Text.Printf.printf "| 🆎 %s  | %13s |\n" (red "Count F                             ") (show overalfCount)
  Text.Printf.printf "  👑 %s   %13s \n" (red " taker count ") (show takerCount)
  Text.Printf.printf "  👑 %s   %13s \n" (red " maker count ") (show makerCount)
  Text.Printf.printf "| ∑ %s  | %13s |\n" (red "ratio prediction") (show lsprediction)
  Text.Printf.printf "| ∑ %s  | %13s |\n" (red "long ratio                           ") (show overalLongs ++ ", " ++ show roundedLongShortRatioL ++ "%")
  Text.Printf.printf "| ∑ %s| %13s |\n" (red "short ratio                            ") (show overalShorts ++ "," ++ show roundedLongShortRatioS ++ "%")
  putStrLn "|-------------------------------------------------------"
  Text.Printf.printf "| 💲 %s   | %12s$ |\n" (orange "Value X                            ") (show (offX stats))
  Text.Printf.printf "| 💲 %s  | %12s$ |\n" (orange "Value Y                             ") (show (offY stats))
  Text.Printf.printf "| 💲 %s  | %12s$ |\n" (orange "Value Z                             ") (show (offZ stats))
  Text.Printf.printf "| 💲 %s   | %12s$ |\n" (orange "Value F                            ") (show (offF stats))
  putStrLn "+--------------------end-of-5-minute-time-frame-run------------------------------+\n🍺🍺🍺🍺🍺🍺🍺🍺🍺🍺🍺🍺🍺🍺"
  -- add delta of those other deltas
