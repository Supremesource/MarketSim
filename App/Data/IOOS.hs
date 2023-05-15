{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use withFile" #-}
-- | module name
module Orderbook.App.Data.IOOS where
-- | external modules
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8 qualified as BC
import System.IO
import Control.Exception (bracket, throwIO)
import Text.Printf (printf)
-- | module exports
import Orderbook.App.Functions.AAGenerator
import Orderbook.App.Settings.Arunsettings


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
                  -> Int
                  -> Int  
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
                  x 
                  y 
                  = 
                  do
  B.putStrLn $ B.pack $ allCaps "📚Orderbook ↕️ \n\n (PRICE LEVEL , USD VALUE)\n"
  B.putStrLn $
          B.pack $
            allCaps "\n📕 ask orderbook ⏬\n\n"
              ++ unlines (map show (reverse bookSpreadFactorAsk))
              ++ allCaps "\nask ⬆️ \n\n\n"
  B.putStrLn $ B.pack $ allCaps "spread: " ++ show (roundTo maxDecimal spread) ++ " $"
  B.putStrLn $
          B.pack $
            allCaps "\n\n\nbid ⬇️ \n\n"
              ++ unlines (map show bookSpreadFactorBid)
              ++ allCaps "\n📗 bid orderbook ⏫"
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
    -- !!  REWRTING DATA FILES
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


   





