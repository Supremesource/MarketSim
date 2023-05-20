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
                                makerSize, orderbookChange,
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

aggregateStats :: ((Int, String), MakerTuple) -> Stats -> Int -> Stats
aggregateStats (taker, makers) stats randominmain =
  Stats
    { overallOI = overallOI stats + sum (interestorPlus taker makers) - sum (interestorMinus taker makers),
      totalVolume = totalVolume stats + fst taker,
      buyVolume =
        buyVolume stats
          + ( if randominmain
                == 1
                && (snd taker == "x" || snd taker == "z")
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                && (snd taker == "y" || snd taker == "f")
                then fst taker
                else 0
            ),
      sellVolume =
        sellVolume stats
          + ( if randominmain
                == 1
                && (snd taker == "y" || snd taker == "f")
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                && (snd taker == "x" || snd taker == "z")
                then fst taker
                else 0
            ),
      takerXc =
        takerXc stats
          + ( if randominmain
                == 1
                && snd taker == "x"
                then 1
                else 0
            )
          + ( if randominmain
                == 0
                then countElements "x" makers
                else 0
            ),
      takerYc =
        takerYc stats
          + ( if randominmain
                == 1
                && snd taker == "y"
                then 1
                else 0
            )
          + ( if randominmain
                == 0
                then countElements "y" makers
                else 0
            ),
      takerZc =
        takerZc stats
          + ( if randominmain
                == 1
                && snd taker == "z"
                then 1
                else 0
            )
          + ( if randominmain
                == 0
                then countElements "z" makers
                else 0
            ),
      takerFc =
        takerFc stats
          + ( if randominmain
                == 1
                && snd taker == "f"
                then 1
                else 0
            )
          + ( if randominmain
                == 0
                then countElements "f" makers
                else 0
            ),
      makerXc =
        makerXc stats
          + if randominmain
            == 1
            then countElements "x" makers
            else
              if randominmain
                == 0
                && snd taker == "x"
                then 1
                else 0,
      makerYc =
        makerYc stats
          + if randominmain
            == 1
            then countElements "y" makers
            else
              if randominmain
                == 0
                && snd taker == "y"
                then 1
                else 0,
      makerZc =
        makerZc stats
          + if randominmain
            == 1
            then countElements "z" makers
            else
              if randominmain
                == 0
                && snd taker == "z"
                then 1
                else 0,
      makerFc =
        makerFc stats
          + if randominmain
            == 1
            then countElements "f" makers
            else
              if randominmain
                == 0
                && snd taker == "f"
                then 1
                else 0,
      offX = offX stats + (if snd taker == "x" then fst taker else 0) + makerSize "x" makers,
      offY = offY stats + (if snd taker == "y" then fst taker else 0) + makerSize "y" makers,
      offZ = offZ stats + (if snd taker == "z" then fst taker else 0) + makerSize "z" makers,
      offF = offF stats + (if snd taker == "f" then fst taker else 0) + makerSize "f" makers,
      takerX =
        takerX stats
          + ( if randominmain
                == 1
                && snd taker == "x"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                then makerSize "x" makers
                else 0
            ),
      takerY =
        takerY stats
          + ( if randominmain
                == 1
                && snd taker == "y"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                then makerSize "y" makers
                else 0
            ),
      takerZ =
        takerZ stats
          + ( if randominmain
                == 1
                && snd taker == "z"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                then makerSize "z" makers
                else 0
            ),
      takerF =
        takerF stats
          + ( if randominmain
                == 1
                && snd taker == "f"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 0
                then makerSize "f" makers
                else 0
            ),
      makerX =
        makerX stats
          + ( if randominmain
                == 0
                && snd taker == "x"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 1
                then makerSize "x" makers
                else 0
            ),
      makerY =
        makerY stats
          + ( if randominmain
                == 0
                && snd taker == "y"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 1
                then makerSize "y" makers
                else 0
            ),
      makerZ =
        makerZ stats
          + ( if randominmain
                == 0
                && snd taker == "z"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 1
                then makerSize "z" makers
                else 0
            ),
      makerF =
        makerF stats
          + ( if randominmain
                == 0
                && snd taker == "f"
                then fst taker
                else 0
            )
          + ( if randominmain
                == 1
                then makerSize "f" makers
                else 0
            )
    }
