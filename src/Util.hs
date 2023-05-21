{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Util where


-- | importing external libraries
import           System.Random (Random (randomRs), RandomGen (split), StdGen)

-- | internal libraries
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib           (countElements, infiniteList', infiniteListDown',
                                interestorMinus, interestorPlus, lengthchange,
                                orderSize, orderbookChange,
                                printCustomRandomList, printRandomList',
                                spread', sumInts, zipToTuples)
import           RunSettings   (largerSpread, maxDownMove, maxUpMove,
                                minDownMove, minUpMove, takeamountASK,
                                takeamountBID)


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

-- | base case
recursiveList [] _ _ _ _ _ _ _ _ = return ([], [])

recursiveList (x:xs) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall -- | recursive case
         =
    orderbookLoop x bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall >>=
    \(newBidBook, newAskBook) -> do

        let (newGen1, newGen2) = (fst (split gen1), fst (split gen2)) -- create two new generators
        recursiveList xs newBidBook newAskBook newGen1 newGen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall

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
            let lengthchangeBID :: Int = lengthchange askUpdateBook askBook

            print $ "l ch ask: " ++ show lengthchangeBID
            print $ "l ch bid: " ++ show lengthchangeASK

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
                        else listASK ++ askBook

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

            let orderbookIO =   generateOrderBook -- function to generate the orderbook
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



initStats :: Stats
initStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

aggregateStats :: (TakerTuple, MakerTuple) -> Stats -> Stats
aggregateStats (taker, makers) stats  =
  Stats
    { overallOI = overallOI stats + sum (interestorPlus taker makers) - sum (interestorMinus taker makers),
      totalVolume = totalVolume stats + foldl (\acc (x, _) -> acc + x) 0 taker,
      buyVolume =
        buyVolume stats + foldl (\acc (x, y) -> if y == "x" || y == "z" then acc + x else acc) 0 taker,
        
      sellVolume =
        sellVolume stats + foldl (\acc (x, y) -> if y == "y" || y == "f" then acc + x else acc) 0 taker,
      takerXc =
        takerXc stats + countElements "x" taker,
      takerYc =
        takerYc stats + countElements "y" taker,
      takerZc =
        takerZc stats + countElements "z" taker,
      takerFc =
        takerFc stats + countElements "f" taker,
      makerXc =
        makerXc stats
          + countElements "x" makers,
      
      makerYc = makerYc stats + countElements "y" makers ,
     
      makerZc =
        makerZc stats + countElements "z" makers,
      makerFc =
        makerFc stats
          +  countElements "f" makers,
    
      offX = offX stats + orderSize "x" taker + orderSize "x" makers,
      offY = offY stats + orderSize "y" taker + orderSize "y" makers,
      offZ = offZ stats + orderSize "z" taker + orderSize "z" makers,
      offF = offF stats + orderSize "f" taker + orderSize "f" makers,
    
      takerX =
        takerX stats + orderSize "x" taker,
      
      takerY =
        takerY stats + orderSize "y" taker,
    
      takerZ =
        takerZ stats + orderSize "z" taker,
         
      takerF =
        takerF stats + orderSize "f" taker,
        
      makerX =
        makerX stats + orderSize "x" makers,
        
      makerY =
        makerY stats + orderSize "y" makers,
      
      makerZ =
        makerZ stats + orderSize "z" makers,
       
      makerF =
        makerF stats + orderSize "f" makers
       
    }
