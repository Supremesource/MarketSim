{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Util where
-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import System.Random
    ( Random(randomRs), RandomGen(split), randomRIO )
import           Data.Aeson (encode)

-- | internal libraries
import           Colours
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           RunSettings



-- | Store the volume result , THIS IS ACCUMULATOR
initialBookDetails :: BookStats
initialBookDetails = BookStats
  { startingPoint    = 0.0
  , maxMinLimit     = replicate 2 []
  , asksTotal       = 0
  , bidsTotal       = 0
  , totakefromwall  = 0
  , lengthchangeBID = 0
  , lengthchangeASK = 0
  , listASK         = []
  , listBID         = []
  , vSide           = Buy
  , volumeAmount    = 0
  , spread          = 0.0
  , startingprice   = 0.0
  , bidAskRatio     = 0.0
  }

setupBookDetails :: InitBookStats -> BookStats
setupBookDetails (startingP', maxMinL' ,asksTot', bidsTot', takewall', lengchngBid'
  , lengchngAsk' ,listASK', listBID', vSide', volumeA', sprd' ,sprice' ,bidAskR') =
        BookStats {     startingPoint     = startingP'
                                            , maxMinLimit       = maxMinL'
                                            , asksTotal         = asksTot'
                                            , bidsTotal         = bidsTot'
                                            , totakefromwall    = takewall'
                                            , lengthchangeBID   = lengchngBid'
                                            , lengthchangeASK    = lengchngAsk'
                                            , listASK           = listASK'
                                            , listBID           = listBID'
                                            , vSide              = vSide'
                                            , volumeAmount      = volumeA'
                                            , spread            = sprd'
                                            , startingprice     = sprice'
                                            , bidAskRatio       = bidAskR'
                                            }


settingcheck :: VolumeSide -> Int -> Int -> Int -> IO ()
settingcheck vSide' volumeA' asksTot' bidsTot' = do
            let check
                    :: String
                    | vSide' /=  Buy && vSide' /=  Sell =
                        error $ red  "wrong volume specification !"
                    | vSide' ==  Buy && volumeA' > asksTot' =
                        error $ red  "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | vSide' ==  Sell && volumeA' > bidsTot' =
                        error $ red  "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | otherwise = ""
            putStrLn check

calculateVolumes :: VolumeSide -> Int -> (Int, Int)
calculateVolumes vSide' volumeA' =
    (if vSide' == Sell then volumeA' else 0,
     if vSide' == Buy then volumeA'  else 0)

calculateBooks :: Int -> Int -> OrderBook -> OrderBook -> (OrderBook, OrderBook)
calculateBooks volumeBID volumeASK bidBook askBook =
    let bidUpdateBook = orderbookChange bidBook volumeBID
        askUpdateBook = orderbookChange askBook volumeASK
    in (bidUpdateBook, askUpdateBook)

calculateFinalBooks :: VolumeSide -> OrderBook -> [(Double,Int)] -> OrderBook -> OrderBook -> [(Double,Int)] -> OrderBook ->(OrderBook, OrderBook)
calculateFinalBooks vSide' askUpdateBook listASK' askBook bidUpdateBook listBID' bidBook =
    let currentbookASK = if vSide' == Buy  then askUpdateBook else listASK' ++ askBook
        currentbookBID = if vSide' == Sell then bidUpdateBook else listBID' ++ bidBook
    in (currentbookASK, currentbookBID)

lengthChanges :: OrderBook -> OrderBook -> OrderBook -> OrderBook -> (Int, Int)
lengthChanges bidUpdateBook bidBook askUpdateBook askBook =
    (lengthchange bidUpdateBook bidBook, lengthchange askUpdateBook askBook)

startingPrices :: VolumeSide -> OrderBook -> OrderBook -> Double
startingPrices vSide' bidUpdateBook askUpdateBook =
    max (if vSide' == Sell then fst (head bidUpdateBook) else 0)
        (if vSide' == Buy then fst (head askUpdateBook)  else 0)

divisionValue :: Double
divisionValue = 1.10

calculateListTuples :: [Double] -> [Double] -> [Int] -> [Int] -> ([(Double,Int)], [(Double,Int)])
calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID  =
    let listASK' = zipToTuples askSetupInsert (map (round . ((/ divisionValue) :: Double -> Double) . fromIntegral) pricesASK)
        listBID' = zipToTuples bidSetupInsert (map (round . ((/ divisionValue) :: Double -> Double) . fromIntegral) pricesBID)
    in (listASK', listBID')

calculateFirstElements :: OrderBook -> OrderBook -> (Double, Double)
calculateFirstElements finalBookAsk finalBookBid =
    let firstelemASK = (fst . head) finalBookAsk
        firstelemBID = (fst . head) finalBookBid
    in (firstelemASK, firstelemBID)

calculateTotals :: OrderBook -> OrderBook -> (Int, Int)
calculateTotals finalBookAsk finalBookBid =
    let asksTot' = sumInts finalBookAsk
        bidsTot' = sumInts finalBookBid
    in (asksTot', bidsTot')

calculateSetupInserts :: Int -> Int -> Double -> Generator -> Generator -> ([Double], [Double])
calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2 =
    let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
        downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
        askSetupInsert = take lengchngAsk' (tail (infiniteList' sPrice gen1 upMovesInsert))
        bidSetupInsert = take lengchngBid' (tail (infiniteListDown' sPrice gen2 downMovesInsert))
    in (askSetupInsert, bidSetupInsert)

calculateTotalsCount :: OrderBook -> OrderBook -> (Double, Double)
calculateTotalsCount finalBookAsk finalBookBid =
    let asktotal = fromIntegral (length finalBookAsk)
        bidtotal = fromIntegral (length finalBookBid)
    in (asktotal, bidtotal)


recursiveList :: RecursionPass -> IO (OrderBook, OrderBook, [BookStats])
recursiveList ([], bidBook, askBook, _, _, _, _, _, _, bookDetails) = do
    filewrites1 $ tail (reverse bookDetails)

    let writeBidBook = Book { book = bidBook }
    let writeAskBook = Book { book = askBook }
    BL.writeFile bidBookP (encode writeBidBook)
    BL.writeFile askBookP (encode writeAskBook)

    return (bidBook, askBook, bookDetails)
recursiveList (x:xs, bidBook, askBook, gen1, gen2, fullwallsASK, fullwallsBIDS, sPoint, takeWall, bookDetails) =
    orderbookLoop (x, bidBook, askBook, gen1, gen2, fullwallsASK, fullwallsBIDS, sPoint, takeWall) >>=
    \(newBidBook, newAskBook, newBookDetails) -> do
        let (newGen1, newGen2) = (fst (split gen1), fst (split gen2))
        recursiveList (xs, newBidBook, newAskBook, newGen1, newGen2, fullwallsASK
            , fullwallsBIDS, sPoint, takeWall, newBookDetails:bookDetails)




-- ? POSITION FUTURE

type FutureInfo = [(Double, Int, String)]

-- PACKAGE CLOSING CONVERSION X & Y TO F & Z
closingConversion :: (TakerTuple,MakerTuple) -> (TakerTuple, MakerTuple)
closingConversion (takers,makers )| hasBothXY takers = error "Unsupported tuple format in takers"
                                | hasBothXY makers = error "Unsupported tuple format in makers"
                                | otherwise =
  (closingconvert filterTakers takers, closingconvert filterMakers makers)
  where
  isValidTuple (_, side) = side == "x" || side == "y"
  filterTakers = filter isValidTuple
  filterMakers = filter isValidTuple
  closingconvert f t = map (\(x,y) -> (x, if y == "x" then "f" else "z")) (f t)
  hasBothXY xs = "x" `elem` map snd xs && "y" `elem` map snd xs


-- TODO move this into the stats and also make it customizable or at least point 
-- to it in the settings
takenLeverage :: IO Int
takenLeverage = do
  x <- randomRIO (1,100) :: IO Int
  return $ case x of
     _ | x >= 1  && x <= 85 -> 1
       | x >= 86 && x <= 90 -> 2
       | x >= 91 && x <= 93 -> 5
       | x >= 94 && x <= 95 -> 10
       | x == 96            -> 15
       | x == 97            -> 20
       | x == 98            -> 25
       | x == 99            -> 50
       | x == 100           -> 100
       | otherwise          -> error "something went wrong in takenLeverage"


                                                  -- (liq price, liq amount, liq side)
positionFuture :: Double -> (TakerTuple, MakerTuple) -> IO FutureInfo
positionFuture price (taker, maker)  = do
  let (takerConver, makerConvert) = closingConversion (taker, maker)
  let concatTakerMaker = takerConver ++ makerConvert
  mapM calcPosition concatTakerMaker
  where
    calcPosition (amt, side) = do
      leverage <- takenLeverage
      print leverage
      let liquidationPrice                
            | leverage /= 1 && side == "z" = (price / fromIntegral leverage) + price
            | leverage /= 1 && side == "f" = price - (price / fromIntegral leverage)
            | leverage == 1 && side == "z" = 2 * price
            | otherwise = 0
      return (liquidationPrice, amt, side)




-- | check if the future file is empty
isFutureEmpty :: IO Bool
isFutureEmpty = isFileEmpty posFutureP


-- // end of position future


orderbookLoop :: ListPass -> IO (OrderBook, OrderBook, BookStats)
orderbookLoop ((vAmount, vSide'), bidBook, askBook, gen1, gen2 ,fullwallsASK ,fullwallsBIDS, sPoint, takeWall)   = do

                          -- | local variables      
                          let (volumeBID, volumeASK) =
                                calculateVolumes vSide' vAmount
                          let (bidUpdateBook, askUpdateBook) =
                                calculateBooks volumeBID volumeASK bidBook askBook
                          -- | how much volume took from certain order books
                          let (lengchngAsk', lengchngBid') =
                                lengthChanges bidUpdateBook bidBook askUpdateBook askBook
                          let sPrice  =
                               startingPrices vSide' bidUpdateBook askUpdateBook
                          let (askSetupInsert, bidSetupInsert) =
                                calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2
                          let maxMinLmt            :: [[Int]]    =
                                [fullwallsASK, fullwallsBIDS]
                          pricesASK  <- printCustomRandomList lengchngAsk'
                          pricesBID   <- printRandomList' lengchngBid'
                          -- | the / number is how smaller the insertion will be
                          let (listASK', listBID') =
                                calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID
-- //TODO, possible microoptimization with the stuff below :
                          -- | let insertInAsk = if vSide == Buy then [] else listASK
                          --  / let insertInBid = if vSide == Sell then [] else listBID    // --
                          let (finalBookAsk, finalBookBid) =
                                calculateFinalBooks vSide' askUpdateBook listASK' askBook bidUpdateBook listBID' bidBook
                          -- | ask in total in terms of count
                          -- | bids in total in terms of count
                          let (asktotal, bidtotal) =
                               calculateTotalsCount finalBookAsk finalBookBid
                          -- | Ratio between bids and AKS
                          let bidAskR' =
                               abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
                          -- | Ratio is benefiting :
                          -- | asks in total -> $$
                          -- | bids in total -> $$
                          let (asksTot', bidsTot') = calculateTotals finalBookAsk finalBookBid
                          -- | bid ask spread
                          let (firstelemASK, firstelemBID) = calculateFirstElements finalBookAsk finalBookBid
                          let sprd'  = spread' firstelemASK firstelemBID
                          -- | checking if the stats above will go through with the orderbook
                          -- | local check
                          let check= settingcheck vSide' vAmount asksTot' bidsTot'
                          check
                          -- | how accumulator stores the values
                          let newbookDetails = setupBookDetails (sPoint, maxMinLmt
                                , asksTot', bidsTot', takeWall, lengchngBid', lengchngAsk' ,listASK'
                                , listBID', vSide', vAmount, sprd' ,sPrice ,bidAskR')
                          let insertinInfo = formatAndPrintInfo newbookDetails
                          insertinInfo -- THIS IS ONLY CONSOLE


-- // DANGER EXPERIMENTAL CODE BELOW
                          putStrLn "\n\n\n"
                          print vAmount >> print vSide'

                          putStrLn "\n\n\n"
                          putStrLn "taker"
                          numTakers <- randomRIO (1, maxTakers) :: IO Int -- select how many makers
                          volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
                          if any (< 0) volumeSplitT  then error "volume split consists of a negative element" else print volumeSplitT

                          putStrLn "maker"
                          numMakers <- randomRIO (1, maxMakers) :: IO Int -- select how many makers
                          volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
                          if any (< 0) volumeSplitM  then error "volume split consists of a negative element" else print volumeSplitM

                          -- | returning the orderbook
                          return (finalBookBid, finalBookAsk, newbookDetails)


-- | helper funciton for the funciton below (where is everything starting at)
initStats :: Stats
initStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-- | aggregating  stats together
aggregateStats :: (TakerTuple, MakerTuple) -> Stats -> Stats
aggregateStats (taker, makers) stats  =
  Stats
          { overallOI = overallOI stats + interestorPlus taker makers - interestorMinus taker makers,
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

