module Orderbook.Orderbook.App.Source.UpdatedPrice where

-- | importing external libraries
import System.IO ()
import Control.Parallel.Strategies
import System.Random


-- | export modules
import Orderbook.Orderbook.App.Functions.AAGenerator
import Orderbook.Orderbook.App.Settings.Arunsettings
import Orderbook.Orderbook.App.Data.IOOS (generateOrderBook)
import Orderbook.Orderbook.App.Data.Afilepaths
    ( askBookPath, bidBookPath, pricePath, logPath )





recursiveList :: [(Int, VolumeSide)] -- the list of volume amounts and volume sides
              -> [(Double,Int)] -- bidBook
              -> [(Double,Int)] -- askBook
              -> StdGen -- gen1
              -> StdGen -- gen2
              -> [Int] -- fullwallsASK
              -> [Int] -- fullwallsBIDS
              -> Double -- startingPoint
              -> Int -- totakefromwall
              -> IO ([(Double, Int)], [(Double, Int)]) -- IO ([(Double, Int)], [(Double, Int)])

recursiveList [] _ _ _ _ _ _ _ _ = return ([], []) -- | base case

recursiveList (x:xs) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall -- | recursive case
         = 
    orderbookLoop x bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall >>=
    \(newBidBook, newAskBook) -> recursiveList xs newBidBook newAskBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall
  where
    -- insert bidBook askBook volumeSide volumeAmount, gen1, gen2, fullwallsASK, fullwallsBIDS, StartingPoint, Totakefrom wall
    orderbookLoop :: (Int, VolumeSide) -> [(Double, Int)] 
                  -> [(Double, Int)] -> StdGen -> StdGen -> [Int] -> [Int] -> Double -> Int -> IO ([(Double, Int)], [(Double, Int)])
    orderbookLoop (volumeAmount, vSide) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall = do
           
            let volumeBID = if vSide == Sell then volumeAmount else 0
            let volumeASK = if vSide == Buy then volumeAmount else 0
          
            let bidUpdateBook = orderbookChange bidBook volumeBID
            let askUpdateBook = orderbookChange askBook volumeASK

            let lengthchangeASK :: Int = lengthchange bidUpdateBook bidBook
            print $ "bid l ch" ++ show lengthchangeASK

            let lengthchangeBID :: Int = lengthchange askUpdateBook askBook
            print $ "ask l ch" ++ show lengthchangeBID

            let stPriceCaseBid :: Double = if vSide ==  Sell then fst (head bidUpdateBook) else 0

            let stPriceCaseAsk :: Double = if vSide ==  Buy then fst (head askUpdateBook) else 0

            let startingprice :: Double = max stPriceCaseAsk stPriceCaseBid :: Double

            let upMovesInsert :: [Double] = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1

            let downMovesInsert :: [Double] = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2

            let askSetupInsert :: [Double] = take lengthchangeASK (tail (infiniteList' startingprice gen1 upMovesInsert))
            print $ "test of bid setup to insert ask: " ++ show (take 5 askSetupInsert)
            let bidSetupInsert :: [Double] = take lengthchangeBID (tail (infiniteListDown' startingprice gen2 downMovesInsert))
            print $ "test of bid setup to insert: " ++ show (take 5 bidSetupInsert)
            let maxMinLimit :: [[Int]] = [fullwallsASK, fullwallsBIDS]

            pricesASK :: [Int] <- printCustomRandomList lengthchangeASK

            pricesBID :: [Int] <- printRandomList' lengthchangeBID
            
            let usdamountASK' :: [Int] = pricesASK :: [Int]

            let usdamountBID' :: [Int] = pricesBID :: [Int]

            let divNumber :: Double = 1

            let listASK :: [(Double, Int)] = zipToTuples askSetupInsert (map (round . (/ divNumber) . fromIntegral) pricesASK) 

            let listBID :: [(Double, Int)] = zipToTuples bidSetupInsert (map (round . (/ divNumber) . fromIntegral) pricesBID)

            let currentbookASK :: [(Double, Int)] = if vSide ==  Buy then askUpdateBook 
                        else listASK ++ askBook -- ! BUG

            let currentbookBID :: [(Double, Int)] = if vSide ==  Sell then bidUpdateBook else listBID ++ bidBook  

            let bookSpreadFactorAsk :: [(Double, Int)] = if largerSpread then tail currentbookASK else currentbookASK

            let bookSpreadFactorBid :: [(Double, Int)] = if largerSpread then tail currentbookBID else currentbookBID




            -- !! ADDITIONAL INFORMATION
            -- ask in total in terms of count
            let asktotal
                    :: Double
                    = fromIntegral (length bookSpreadFactorAsk)

            -- bids in total in terms of count
            let bidtotal
                    :: Double
                    = fromIntegral (length bookSpreadFactorBid)
            -- Ratio between bids and AKS
            let bidAskRatio
                    :: Double
                    =
                    abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double

            -- Ratio is benefiting :
            let bidAskBenefit
                    :: IO ()
                    =
                    if asktotal > bidtotal
                    then putStr " ->  benefiting ASKS"
                    else putStr " -> benefeting BIDS"

            -- asks in total -> $$
            let asksTotal
                    :: Int
                    =
                    sumInts bookSpreadFactorAsk

            -- bids in total -> $$
            let bidsTotal
                    :: Int
                    =
                    sumInts bookSpreadFactorBid

            -- bid ask spread
            let firstelemASK
                    :: Double
                    =
                    (fst . head) bookSpreadFactorAsk

            let firstelemBID
                    :: Double
                    =
                    (fst . head) bookSpreadFactorBid

            let spread
                    :: Double
                    =
                    spread' firstelemASK firstelemBID

            -- checking if the stats above will go through with the orderbook
            let check
                    :: String
                    | vSide /=  Buy && vSide /=  Sell =
                        error "wrong volume specification !"
                    | vSide ==  Buy && volumeAmount > asksTotal =
                        error "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | vSide ==  Sell && volumeAmount > bidsTotal =
                        error "-THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | otherwise = ""
            putStr check
            
            print $ "l ch BID : " ++ show lengthchangeBID
            print $ "l ch ASK : " ++ show  lengthchangeASK
            let orderbookIO =  generateOrderBook -- function to generate the orderbook
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
                                (show vSide)
                                volumeAmount
                                startingprice
                                pricePath
                                bidBookPath
                                askBookPath
                               

            orderbookIO
            return (currentbookBID,currentbookASK)




  