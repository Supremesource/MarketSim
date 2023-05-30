{-# OPTIONS_GHC -Wno-unused-local-binds #-}
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
recursiveList :: VolumeList -> OrderBook -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall -> FileWrtiter -> IO (OrderBook, OrderBook) 
-- | base case
recursiveList [] _ _ _ _ _ _ _ _ _ = return ([], [])
recursiveList (x:xs) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall handles1 -- | recursive case
         =
    orderbookLoop x bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall handles1 >>=
    \(newBidBook, newAskBook) -> do
        let (newGen1, newGen2) = (fst (split gen1), fst (split gen2)) -- create two new generators
        recursiveList xs newBidBook newAskBook newGen1 newGen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall handles1
  where
    orderbookLoop :: Volume -> OrderBook
                  -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall -> FileWrtiter  -> IO (OrderBook, OrderBook) 
    orderbookLoop (volumeAmount, vSide) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall handles1 = do
            -- | local variables
            let (volumeBID, volumeASK) = calculateVolumes vSide volumeAmount
            let bidUpdateBook = orderbookChange bidBook volumeBID
            let askUpdateBook = orderbookChange askBook volumeASK
-- | how much volume took from certain order books
            let (lengthchangeASK, lengthchangeBID) = lengthChanges bidUpdateBook bidBook askUpdateBook askBook
            let startingprice = startingPrices vSide bidUpdateBook askUpdateBook
            let upMovesInsert :: [Double] = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
            let downMovesInsert :: [Double] = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
            let askSetupInsert :: [Double] = take lengthchangeASK (tail (infiniteList' startingprice gen1 upMovesInsert))
            let bidSetupInsert :: [Double] = take lengthchangeBID (tail (infiniteListDown' startingprice gen2 downMovesInsert))
            let maxMinLimit :: [[Int]] = [fullwallsASK, fullwallsBIDS]
            pricesASK :: [Int] <- printCustomRandomList lengthchangeASK
            pricesBID :: [Int] <- printRandomList' lengthchangeBID
            -- | the / number is how smaller the insertion will be
            let listASK :: [(Double, Int)] = zipToTuples askSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesASK)
            let listBID :: [(Double, Int)] = zipToTuples bidSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesBID)
            let currentbookASK :: [(Double, Int)] = if vSide ==  Buy     then askUpdateBook  else listASK 
            let currentbookBID :: [(Double, Int)] = if vSide ==  Sell    then bidUpdateBook else listBID 
            let finalBookAsk :: [(Double, Int)] = if largerSpread then tail currentbookASK else currentbookASK
            let finalBookBid :: [(Double, Int)] = if largerSpread then tail currentbookBID else currentbookBID
-- | ask in total in terms of count
            let asktotal = fromIntegral (length finalBookAsk)
-- | bids in total in terms of count
            let bidtotal = fromIntegral (length finalBookBid)
-- | Ratio between bids and AKS
            let bidAskRatio = abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
-- | Ratio is benefiting :
            let bidAskBenefit = if asktotal > bidtotal then putStr " ->  benefiting ASKS" else putStr " -> benefeting BIDS"
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
    
    -- TODO make this shit more simple
            let insertinInfo = formatAndPrintInfo spread asksTotal bidsTotal bidAskRatio startingprice vSide volumeAmount lengthchangeBID lengthchangeASK
            insertinInfo
       
            let writeInfo = filewrites1 handles1 startingPoint maxMinLimit asksTotal bidsTotal totakefromwall lengthchangeBID lengthchangeASK listASK listBID vSide volumeAmount spread startingprice  finalBookAsk finalBookBid bidAskRatio
            writeInfo


-- | returning the orderbook
            return (currentbookBID, currentbookASK)


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

