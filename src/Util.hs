{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Util where
-- | module of utility funcitons

-- | importing external libraries
import           System.Random (Random (randomRs), RandomGen (split))



-- | internal libraries
import           DataTypes     
import           InputOutput   
import           Lib           
import           RunSettings  
import           Filepaths
-- TODO implement types instead of long def



settingcheck :: VolumeSide -> Int -> Int -> Int -> IO ()
settingcheck vSide volumeAmount asksTotal bidsTotal = do
            let check
                    :: String
                    | vSide /=  Buy && vSide /=  Sell =
                        error "wrong volume specification !"
                    | vSide ==  Buy && volumeAmount > asksTotal =
                        error "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | vSide ==  Sell && volumeAmount > bidsTotal =
                        error "-THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | otherwise = ""
            putStrLn check

calculateVolumes :: VolumeSide -> Int -> (Int, Int)
calculateVolumes vSide volumeAmount = 
    (if vSide == Sell then volumeAmount else 0,
     if vSide == Buy then volumeAmount else 0)

lengthChanges :: OrderBook -> OrderBook -> OrderBook -> OrderBook -> (Int, Int)
lengthChanges bidUpdateBook bidBook askUpdateBook askBook =
    (lengthchange bidUpdateBook bidBook, lengthchange askUpdateBook askBook)

startingPrices :: VolumeSide -> OrderBook -> OrderBook -> Double
startingPrices vSide bidUpdateBook askUpdateBook =
    max (if vSide == Sell then fst (head bidUpdateBook) else 0)
        (if vSide == Buy then fst (head askUpdateBook) else 0)


-- | processing the orderbook with a volume
recursiveList :: VolumeList -> OrderBook -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall  -> IO (OrderBook, OrderBook) 
-- | base case
recursiveList [] bidBook askBook _ _ _ _ _ _  = do 
    writeFile bidBookPath $ show bidBook 
    writeFile askBookPath $ show askBook
    return (bidBook, askBook)

recursiveList (x:xs) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall =
    orderbookLoop x bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall  >>=
    \(newBidBook, newAskBook) -> do
        let (newGen1, newGen2) = (fst (split gen1), fst (split gen2)) -- create two new generators
        recursiveList xs newBidBook newAskBook newGen1 newGen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall

  where
    orderbookLoop :: Volume -> OrderBook
                  -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall  -> IO (OrderBook, OrderBook) 
    orderbookLoop (volumeAmount, vSide) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall  = do
            -- | local variables
            let (volumeBID, volumeASK) = calculateVolumes vSide volumeAmount
            let bidUpdateBook = orderbookChange bidBook volumeBID
            let askUpdateBook = orderbookChange askBook volumeASK
-- | how much volume took from certain order books
            let (lengthchangeASK, lengthchangeBID) = lengthChanges bidUpdateBook bidBook askUpdateBook askBook
            let startingprice = startingPrices vSide bidUpdateBook askUpdateBook
            let upMovesInsert   ::   [Double]   = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
            let downMovesInsert ::   [Double]   = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
            let askSetupInsert  ::   [Double]   = take lengthchangeASK (tail (infiniteList' startingprice gen1 upMovesInsert))
            let bidSetupInsert  ::   [Double]   = take lengthchangeBID (tail (infiniteListDown' startingprice gen2 downMovesInsert))
            let maxMinLimit :: [[Int]] = [fullwallsASK, fullwallsBIDS]
            pricesASK :: [Int] <- printCustomRandomList lengthchangeASK
            pricesBID :: [Int] <- printRandomList' lengthchangeBID
            -- | the / number is how smaller the insertion will be
            let listASK :: [(Double, Int)] = zipToTuples askSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesASK)
            let listBID :: [(Double, Int)] = zipToTuples bidSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesBID)
        --  let insertInAsk = if vSide == Buy then [] else listASK 
        --  let insertInBid = if vSide == Sell then [] else listBID           
            let currentbookASK :: [(Double, Int)] = if vSide ==  Buy     then askUpdateBook  else  listASK ++ askBook
            let currentbookBID :: [(Double, Int)] = if vSide ==  Sell    then bidUpdateBook  else  listBID ++ bidBook
            let finalBookAsk :: [(Double, Int)] = if largerSpread        then tail currentbookASK else currentbookASK
            let finalBookBid :: [(Double, Int)] = if largerSpread        then tail currentbookBID else currentbookBID
-- | ask in total in terms of count
            let asktotal = fromIntegral (length finalBookAsk)
-- | bids in total in terms of count
            let bidtotal = fromIntegral (length finalBookBid)
-- | Ratio between bids and AKS
            let bidAskRatio = abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
-- | Ratio is benefiting :
-- | asks in total -> $$
            let asksTotal = sumInts finalBookAsk
 -- | bids in total -> $$
            let bidsTotal = sumInts finalBookBid
-- | bid ask spread
            let firstelemASK = (fst . head) finalBookAsk
            let firstelemBID = (fst . head) finalBookBid
            let spread = spread' firstelemASK firstelemBID
-- | checking if the stats above will go through with the orderbook
-- | local check
            let check = settingcheck vSide volumeAmount asksTotal bidsTotal
            check
    -- TODO make this shit more simple
           
            let bookDetails = 
                    BookStats {startingPoint    = startingPoint
                              , maxMinLimit     = maxMinLimit
                              , asksTotal       = asksTotal
                              , bidsTotal       = bidsTotal
                              , totakefromwall  = totakefromwall
                              , lengthchangeBID = lengthchangeBID
                              , lengthchangeASK = lengthchangeASK
                              , listASK         = listASK
                              , listBID         = listBID
                              , vSide           = vSide
                              , volumeAmount    = volumeAmount
                              , spread          = spread
                              , startingprice    = startingprice
                              , bidAskRatio     = bidAskRatio
                              }



            -- exclude this in data types, this can be printed all the time
            let insertinInfo = formatAndPrintInfo bookDetails
            insertinInfo -- THIS IS ONLY CONSOLE

            let writeInfo = filewrites1 bookDetails
            writeInfo -- THIS IS ONLY FILE
-- | returning the orderbook
            return (finalBookBid, finalBookAsk)


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

